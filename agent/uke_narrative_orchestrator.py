"""UKE Pipeline — Gemini + Claude orchestrator with air-gap enforcement.

Two modes:
  - narrative: Stage 0 (Gemini) → Stages 1-4 (Claude, generation) →
               Constraint engine (optional) → Stages 5-10 (Claude, editorial pipeline)
    Story translation preserving constraint topology.
    Optional: Constraint engine between Stages 4 and 5 (evaluates generated narrative).
    Editorial pipeline: Discovery → Strategy → Structure/Rewrite →
                       Pacing/Subtraction → Review → Validation
    Review (Stage 9) routes to Strategy (Stage 6) or Validation (Stage 10).
  - artifact: Stage 0 (Gemini) → Stages 1-6 (Claude)
    Software artifact generation from constraint topology.

Usage:
    # Full pipeline (default)
    python3 uke_narrative_orchestrator.py originals/eighty_yard_run.md

    # Workshop mode — editorial pass on existing story
    python3 uke_narrative_orchestrator.py --from-stage stage_5 stories/my_story.md

    # Resume from run directory
    python3 uke_narrative_orchestrator.py --resume outputs/run/ --from-stage stage_5

    # Artifact mode
    python3 uke_narrative_orchestrator.py --mode artifact originals/eighty_yard_run.md

    # Skip constraint engine
    python3 uke_narrative_orchestrator.py --skip-engine originals/story.md
"""

import argparse
import json
import logging
import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass, field
from functools import lru_cache
from pathlib import Path
from typing import Any, Callable, Protocol

logging.basicConfig(level=logging.INFO, format="%(asctime)s [%(name)s] %(message)s")
_log = logging.getLogger("uke_pipeline")


# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

NARRATIVE_TRANSFORM_DIR = Path(__file__).resolve().parent / "narrative_transform"
ORIGINALS_DIR = NARRATIVE_TRANSFORM_DIR / "originals"
STORIES_DIR = NARRATIVE_TRANSFORM_DIR / "stories"
ARTIFACTS_DIR = NARRATIVE_TRANSFORM_DIR / "artifacts"
LOGIC_NARRATIVE_PATH = NARRATIVE_TRANSFORM_DIR / "logic_narrative_v4.1.md"
LOGIC_SYMBOLIC_PATH = NARRATIVE_TRANSFORM_DIR / "logic_symbolic.md"
LOGIC_NARRATIVE_TRANSLATION_PATH = NARRATIVE_TRANSFORM_DIR / "logic_narrative_translation.md"
UKE_OUTPUT_DIR = NARRATIVE_TRANSFORM_DIR / "uke"


# ---------------------------------------------------------------------------
# DR engine integration (optional — degrades gracefully if unavailable)
# ---------------------------------------------------------------------------

_DR_ENGINE_AVAILABLE = False
_REPO_ROOT = None

def _init_dr_engine():
    """Attempt to import the DR constraint story infrastructure.

    Returns True if the Prolog engine pipeline is available.
    """
    global _DR_ENGINE_AVAILABLE, _REPO_ROOT

    # The agent/ directory sits one level below the DR repo root
    candidate_root = Path(__file__).resolve().parent.parent
    pipeline_script = candidate_root / "python" / "run_pipeline.py"
    report_script = candidate_root / "python" / "enhanced_report.py"

    if pipeline_script.exists() and report_script.exists():
        if str(candidate_root) not in sys.path:
            sys.path.insert(0, str(candidate_root))
        try:
            from agent.story_generator_base import (
                process_response,
                save_story,
                strip_json_fences,
                build_prompt,
                _SYSTEM_INSTRUCTION,
                REPO_ROOT,
            )
            _REPO_ROOT = REPO_ROOT
            _DR_ENGINE_AVAILABLE = True
            _log.info("DR engine available at %s", candidate_root)
            return True
        except ImportError as e:
            _log.warning("DR engine import failed: %s", e)
    else:
        _log.info("DR engine not found (no run_pipeline.py at %s)", candidate_root)

    return False

# Lazy init on first use
_dr_engine_initialized = False

def _ensure_dr_engine():
    global _dr_engine_initialized
    if not _dr_engine_initialized:
        _init_dr_engine()
        _dr_engine_initialized = True
    return _DR_ENGINE_AVAILABLE


# ---------------------------------------------------------------------------
# Pipeline mode configuration
# ---------------------------------------------------------------------------

PIPELINE_MODES = {
    "narrative": {
        "stages": ["stage_0", "stage_1", "stage_2", "stage_3", "stage_4",
                    "stage_5", "stage_6", "stage_7", "stage_8", "stage_9", "stage_10"],
        "file_prefix": "stage",              # stage0.md .. stage10.md
        "output_dir": STORIES_DIR,
        "validation_gates": set(),
        "air_gap_stage": "stage_4",          # narrative air gap at stage 4
        "review_blind_stage": "stage_9",     # review reads blind — only stage_8
    },
    "artifact": {
        "stages": ["stage_0", "stage_1", "stage_2", "stage_3", "stage_4", "stage_5", "stage_6"],
        "file_prefix": "artifact_stage",     # artifact_stage0.md .. artifact_stage6.md
        "output_dir": ARTIFACTS_DIR,
        "validation_gates": {"stage_2", "stage_6"},
        "air_gap_stage": "stage_5",          # artifact air gap at stage 5
    },
}

# Stage data flow: which prior outputs feed each stage.
# "source" = original text, "dr_logic" = logic reference, "stage_N" = output of stage N.
# "constraint_reports" = Prolog engine reports (NEW — air-gap safe, no source material)
STAGE_INPUTS = {
    "narrative": {
        "stage_0": ["source", "dr_logic_symbolic"],
        "stage_1": ["stage_0", "dr_logic_symbolic"],
        "stage_2": ["stage_1_anon", "dr_logic_narrative", "break_contract"],  # break_contract: documented flow; _run_stage_2 (the actual runner) mirrors it
        "stage_3": ["stage_1_anon", "stage_2"],                               # NO logic ref
        "stage_4": ["stage_2", "stage_3"],                                     # AIR GAP: no source, no stage_0, no stage_1, no logic ref
        "stage_5": ["stage_4", "constraint_reports"],                          # Discovery: story + engine reports (if available)
        "stage_6": ["stage_4", "stage_5"],                                     # Strategy: story + discovery report
        "stage_7": ["stage_4", "stage_6"],                                     # Structure/rewrite: story + strategy brief
        "stage_8": ["stage_7", "stage_6"],                                     # Pacing/subtraction: revised story + strategy brief
        "stage_9": ["stage_8", "invariant_contract", "break_contract"],       # Review: BLIND — story + the two contracts ONLY (both are surface-free, no strategy/source info)
        "stage_10": ["stage_8", "stage_1_anon", "stage_6", "invariant_contract", "break_contract"],  # Validation: story + spec (optional) + strategy + contracts (D9/D10)
    },
    "artifact": {
        "stage_0": ["source", "dr_logic"],
        "stage_1": ["stage_0", "dr_logic"],
        "stage_2": ["stage_0", "stage_1"],                   # validation gate
        "stage_3": ["stage_1", "dr_logic"],                  # path/modality selection
        "stage_4": ["stage_1", "stage_3"],                   # interaction design
        "stage_5": ["stage_1", "stage_3", "stage_4"],        # AIR GAP: no source, no stage_0
        "stage_6": ["stage_1", "stage_5"],                   # validation gate
    },
}

# All possible stages across both modes (for CLI --from-stage choices)
ALL_POSSIBLE_STAGES = ["stage_0", "stage_1", "stage_2", "stage_3",
                       "stage_4", "stage_5", "stage_6", "stage_7",
                       "stage_8", "stage_9", "stage_10"]


# ---------------------------------------------------------------------------
# File loading
# ---------------------------------------------------------------------------

@lru_cache(maxsize=None)
def _load_context_file(path: str) -> str:
    """Read and cache a context file."""
    return Path(path).read_text(encoding="utf-8")


# ---------------------------------------------------------------------------
# Title extraction helpers
# ---------------------------------------------------------------------------

def _extract_title(text: str) -> str:
    """Extract a title from the first markdown heading or first non-empty line."""
    for line in text.strip().splitlines():
        line = line.strip()
        if not line:
            continue
        m = re.match(r'^#{1,2}\s+(.+)$', line)
        if m:
            return m.group(1).strip()
        return line.strip()
    return "untitled"


def _title_to_filename(title: str) -> str:
    """Convert a title string to a safe filename slug."""
    slug = re.sub(r'[^a-z0-9]+', '_', title.lower()).strip('_')
    return slug[:80] if slug else "untitled"


# ---------------------------------------------------------------------------
# Editorial-output helpers (manifest split, word counts)
# ---------------------------------------------------------------------------

# Stage 7/8 outputs append an EDIT MANIFEST section (header decoration
# varies across runs: "EDIT MANIFEST", "## EDIT MANIFEST", "**EDIT MANIFEST**").
_EDIT_MANIFEST_HEADER_RE = re.compile(
    r'^\s{0,3}(?:#{1,6}\s+)?\*{0,2}EDIT MANIFEST\*{0,2}\s*:?\s*$',
    flags=re.MULTILINE,
)


def _split_edit_manifest(text: str) -> tuple[str, str]:
    """Split a stage-7/8 output into (story, manifest+omega log).

    The final published story must not ship with editorial apparatus;
    the manifest is kept as a run-dir sidecar. Returns manifest '' when
    no marker is present (e.g. stage_4 fallback output).
    """
    m = _EDIT_MANIFEST_HEADER_RE.search(text)
    if not m:
        return text, ""
    cut = m.start()
    # Pull a horizontal rule immediately above the header into the manifest.
    before = text[:cut]
    rule = re.search(r'(?:^|\n)(-{3,}\s*\n\s*)\Z', before)
    if rule:
        cut = rule.start(1)
    return text[:cut].rstrip() + "\n", text[cut:]


def _word_count(text: str) -> int:
    """wc -w equivalent: whitespace-separated tokens."""
    return len(text.split())


# ---------------------------------------------------------------------------
# Numeric inventory (R6) — mechanical counting meter
#
# The counting ban failed exhortatively at three sites (stage 4 system
# prompt, stage 7's embedded copy, stage 8's scan): the model quoted the
# ban back and waived it. The fix is structural (OQ-101 remedy class):
# the orchestrator extracts every numeric-register item deterministically
# and injects the complete list into the editorial prompts. The model's
# only job is per-instance adjudication; neither side can waive wholesale.
# This is the register-level analogue of stage 4's framework-terminology
# grep — the invisibility check extended to the leak the grep can't see.
# ---------------------------------------------------------------------------

_SPELLED_UNITS = (
    "one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|"
    "thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen"
)
_SPELLED_TENS = "twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety"
_SPELLED_BIG = "hundred|thousand|million|billion"
_NUMBER_WORD_RE = re.compile(
    rf'\b(?:(?:{_SPELLED_TENS})(?:-(?:{_SPELLED_UNITS}))?'
    rf'|{_SPELLED_UNITS}|{_SPELLED_BIG})\b',
    flags=re.IGNORECASE,
)
_NUMERAL_RE = re.compile(r'\b\d+(?:[.,:]\d+)*\s?%?')
_COUNT_VERB_RE = re.compile(
    r'\b(?:count|counts|counted|counting|recount|recounts|recounted|'
    r'tally|tallies|tallied|tallying)\b',
    flags=re.IGNORECASE,
)
_MATH_PHRASE_RE = re.compile(r'\bthe (?:math|arithmetic)\b', flags=re.IGNORECASE)

# Density threshold (numerals + number-words per 1,000 story words) above
# which the post-stage-8 gate fires.
#
# RECALIBRATED from the OQ-215 arm-3 variance runs (operator ruling
# 2026-07-11: "set the threshold from them, not from the two-point
# control"). Witnessed stage-8 densities of improved generated output:
# 0.0, 0.12, 0.0, 0.47, 0.0 (five arm-3 runs) + 0.48 (arm 1) — the
# improved ceiling is ~0.5/1000. The anchored defect sits at 37.6-47.6.
# 10.0 is 20x the improved ceiling and far below the defect band; a
# hypothetical legitimate earned-instrument story tripping it costs one
# revision call + an OPEN read (the gate escalates, never auto-rejects),
# which is the designed behavior for the rift3-class false positive.
#
# Original calibration record (2026-07-11, amended 2026-07-12):
#   positive controls (flag): uke/the_empty_pan_1783821245
#     stage_4_output.md = 37.6, stage_8_output.md = 47.6 (powerless POV
#     tallying ambiently — the knowledge-boundary violation)
#   negative controls (pass): human originals — classic prose 2.3-16.3;
#     inherent-instrument source the-empty-pan.md = 18.8
#   measurement artifact (excluded): the_waste_land.md = 33.1 (verse
#     line-number transcription, a scanner false positive)
#   KNOWN FALSE-POSITIVE CLASS (witnessed, NOT excluded): rift3.md = 46.0
#     — a gauge-owning institutional POV where every reading is taken and
#     acted on in-scene (the log-vs-reading discrepancy IS the story), plus
#     numeric proper nouns ("Vent Fourteen"). Density alone cannot see
#     positional access; the R6 per-instance PRECISION/TEXTURE adjudication
#     and the OPEN-flag escalation are the layers that catch this class —
#     the gate escalates to the operator, it never auto-rejects.
# The meter is a proxy for numeric REGISTER DENSITY, which is narrower
# than the defect (UNEARNED counting). The operator read is the verdict.
NUMERIC_DENSITY_THRESHOLD = 10.0

# Standing caveat, rendered wherever a density figure is read (sidecar
# JSON, inventory prompt block, gate summary). OQ-215 close, 2026-07-12:
# a green density is not evidence the invariant survived.
DENSITY_CAVEAT = (
    "Density measures counting only; invariant survival is adjudicated by "
    "blind stage-9 + operator read. 0.0 is not evidence the invariant held."
)

# No silent caps: if an inventory listing is truncated for prompt size,
# the omitted count is stated in the listing itself.
_MAX_INVENTORY_LISTING = 300


def _numeric_inventory(text: str) -> dict:
    """Deterministic numeric-register extraction. Pure function.

    Returns entries (line, kind, token, context), per-kind counts,
    monotone numeric sequences (3+ strictly monotone numerals inside one
    paragraph — the descending-scores/countdown shape), word count, and
    density per 1,000 words (numerals + number-words).
    """
    lines = text.splitlines()
    entries: list[dict] = []
    for i, line in enumerate(lines, 1):
        for kind, rx in (
            ("numeral", _NUMERAL_RE),
            ("number_word", _NUMBER_WORD_RE),
            ("count_verb", _COUNT_VERB_RE),
            ("math_phrase", _MATH_PHRASE_RE),
        ):
            for m in rx.finditer(line):
                entries.append({
                    "line": i,
                    "kind": kind,
                    "token": m.group(0).strip(),
                    "context": line.strip()[:160],
                })

    # Monotone numeric sequences within a paragraph (blank-line separated)
    monotone: list[dict] = []
    para_start = 1
    para_lines: list[str] = []
    def _flush(start: int, plines: list[str]):
        values = []
        for ln in plines:
            for m in _NUMERAL_RE.finditer(ln):
                tok = m.group(0).strip().rstrip('%').replace(',', '')
                try:
                    values.append(float(tok.replace(':', '.')))
                except ValueError:
                    pass
        if len(values) >= 3:
            inc = all(a < b for a, b in zip(values, values[1:]))
            dec = all(a > b for a, b in zip(values, values[1:]))
            if inc or dec:
                monotone.append({
                    "start_line": start,
                    "direction": "increasing" if inc else "decreasing",
                    "values": values,
                })
    for i, line in enumerate(lines, 1):
        if line.strip():
            if not para_lines:
                para_start = i
            para_lines.append(line)
        elif para_lines:
            _flush(para_start, para_lines)
            para_lines = []
    if para_lines:
        _flush(para_start, para_lines)

    counts = {k: 0 for k in ("numeral", "number_word", "count_verb", "math_phrase")}
    for e in entries:
        counts[e["kind"]] += 1
    words = _word_count(text)
    density = 1000.0 * (counts["numeral"] + counts["number_word"]) / max(words, 1)
    return {
        "word_count": words,
        "counts": counts,
        "density_per_1000": round(density, 2),
        "threshold": NUMERIC_DENSITY_THRESHOLD,
        "caveat": DENSITY_CAVEAT,
        "monotone_sequences": monotone,
        "entries": entries,
    }


def _format_numeric_inventory(inv: dict, header: str) -> str:
    """Render an inventory as a prompt block for per-instance adjudication."""
    lines = [
        f"=== {header} ===",
        f"Computed by the orchestrator (deterministic; complete). Story word "
        f"count: {inv['word_count']:,}. Numeric density: "
        f"{inv['density_per_1000']:.1f} per 1,000 words "
        f"(numerals: {inv['counts']['numeral']}, number-words: "
        f"{inv['counts']['number_word']}, count-verbs: "
        f"{inv['counts']['count_verb']}, math-phrases: "
        f"{inv['counts']['math_phrase']}).",
        f"CAVEAT: {DENSITY_CAVEAT}",
        "",
        "Adjudicate EVERY item below, per instance: KEEP only where a "
        "character with positional access to the quantity acts on it "
        "in-scene (reads it aloud, forges it, breaks the weight) — name "
        "that action. Otherwise revise the line to carry the same pressure "
        "without the number. Numbers as ambient texture, countdown, tally, "
        "or emotional beat are violations regardless of how precise they "
        "feel. You may not waive this list wholesale, and you may not "
        "claim a numeric item is absent: this list is the ground truth.",
        "",
    ]
    for seq in inv["monotone_sequences"]:
        vals = ", ".join(f"{v:g}" for v in seq["values"])
        lines.append(
            f"MONOTONE SEQUENCE (line {seq['start_line']}, "
            f"{seq['direction']}): {vals} — the descending/ascending-ledger "
            f"shape; a known pipeline anchor, not precision."
        )
    if inv["monotone_sequences"]:
        lines.append("")
    shown = inv["entries"][:_MAX_INVENTORY_LISTING]
    for e in shown:
        lines.append(f"L{e['line']} [{e['kind']}] {e['token']!r}: {e['context']}")
    omitted = len(inv["entries"]) - len(shown)
    if omitted > 0:
        lines.append(
            f"... {omitted} additional entries omitted from this listing "
            f"for length; the totals above cover ALL entries, and the "
            f"omitted ones are still violations if unearned."
        )
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# Theme inventory (OQ-214) — mechanical theme-naming / explanation-over-run meter
#
# Built on the _numeric_inventory template (part-for-part above). Same
# structural remedy class (OQ-101): the orchestrator extracts every
# theme-naming-register candidate deterministically and injects the
# complete list into stages 7/8; the model may only adjudicate per
# instance, and can neither waive the list wholesale nor claim a candidate
# is absent. Replaces the waivable standalone-aphorism model scan
# (stage8.md:53-58) — the last self-certifiable absence-claim in the
# editorial audit layer.
#
# THE LOAD-BEARING INVARIANT (why theme is NOT counting). A digit is
# theme-free: `47` means the same in a defect and a masterpiece, so
# _numeric_inventory's false positives are rare and UNCORRELATED with
# quality — which is why the counting meter can arm its gate. Theme-naming's
# extractable surface (sentence-initial repetition, refrains, aphoristic
# closers) is the SAME surface earned prose uses on purpose (rift3's
# institutional creed, the empty-pan's refused ledger-math, McCarthy's whole
# body of work). This meter's false positives are therefore concentrated in
# exactly the prose you least want to flatten.
#
# BUCKET RULE (in-source, do not remove): a kind is density-bearing only if
# flagging it in agent/narrative_transform/originals/rift3.md would NOT be a
# false positive. Refrain fails this test (institutional creed = craft);
# anaphora and causal_chain pass (consecutive sentence-initial triples and
# repeated because/therefore formulas are rarely load-bearing craft).
#   density-bearing (move the auto-gate): anaphora, causal_chain
#   adjudication-only (listed, injected, NEVER density-scored):
#       refrain, aphorism, resonant_closer, word_arithmetic
#
# INVARIANT — the theme-density gate MUST NOT auto-reject; it escalates OPEN
# only. Rationale (do not remove): the counting meter could gate because
# digits do not correlate with merit. Theme-repetition does — the same
# surface is lazy theme-naming AND earned craft. Arming this gate on a
# merit-correlated kind converts it into a craft-suppressor: the hard-ban
# failure in a third costume. Any future change that makes theme-density
# auto-reject reintroduces the exact defect this meter was built under
# adjudication to avoid.
# KILL CONDITION: if any merit-correlated kind (refrain, aphorism,
# resonant_closer, word_arithmetic) is ever promoted to the density gate
# "for determinism," the meter has become a craft-suppressor — revert. The
# per-instance adjudication ("earned by positional access / craft, or
# thesis-restatement?") is the entire safety mechanism; it is what
# distinguishes LLM-lazy repetition (usually the defect) from earned
# repetition (real, rarer, never to be flattened).
# ---------------------------------------------------------------------------

# --- extraction patterns ---------------------------------------------------
_SENT_SPLIT_RE = re.compile(r'(?<=[.!?])["\')\]]?\s+')
_WORDS_RE = re.compile(r"[A-Za-z']+")
# density-bearing: repeated because/therefore-formula connectives (the
# syllogism / thesis-chain tell). Isolated causality is NOT flagged — only
# clusters of >=2 within a short window (see _detect_causal_chain).
_CAUSAL_RE = re.compile(
    r'\b(?:because|therefore|thus|hence|and so|so that|which meant|'
    r'which is why|as a result|and because|could not)\b',
    flags=re.IGNORECASE,
)
# adjudication-only: prose word-arithmetic register (the empty-pan hard
# case). Extends the numeric _MATH_PHRASE_RE register into spelled-out
# operators; kept separate so numeric-meter behavior is unchanged.
_WORD_ARITH_RE = re.compile(
    r'\b[\w%]+\s+(?:minus|plus|divided by)\s+[\w%]+', flags=re.IGNORECASE)
_THE_WAY_RE = re.compile(r'\bthe way\b', flags=re.IGNORECASE)
_GENERIC_SUBJECT_RE = re.compile(
    r'^\W*(?:the|a|an|every|no|all|some|each|any|this|that|these|those|it|'
    r'they|we|you|one|people|everything|nothing|everyone|no one|nobody|'
    r'life|power|memory|history|truth|silence)\b',
    flags=re.IGNORECASE,
)
_ABSTRACT_NOUNS = frozenset((
    "system", "systems", "power", "coordination", "extraction", "meaning",
    "truth", "freedom", "control", "order", "world", "life", "love", "death",
    "memory", "history", "structure", "value", "cost", "price", "silence",
    "justice", "name", "names", "accounting", "keeping", "math", "arithmetic",
    "pattern", "patterns", "way", "ways", "difference", "faith", "hope",
    "fear", "loss", "grief", "time", "work", "labor", "care", "nature",
))

# Density threshold (density-bearing kinds only: anaphora + causal_chain,
# per 1,000 story words) above which the post-stage-8 theme gate fires.
# THIS GATE ESCALATES OPEN, NEVER AUTO-REJECTS (see invariant above).
#
# Calibration corpus: audits/2026-07-12_oq218_scored_snare/ (the OQ-218
# Stage-2 before/after batch). The high-density "before" seeds are embedded
# as STORY A / STORY B inside blind_arm_payload_run{1,2,3}.md, labelled by
# AB_KEY_run{1,2,3}.md (SEED = defect / high, IMPROVED = v0.2 / low).
# Earned negatives (must FLAG but must NOT push density over threshold):
# originals/rift3.md (gauge-owning institutional POV) and
# stories/the-empty-pan_rev2.md (narrator's own survival math, refused).
# Calibration script + raw table: audits/2026-07-13_oq214_theme_meter/.
#
# THRESHOLD_CALIBRATION_RECORD (offline run 2026-07-13, controls PASS,
# theme_density_table.txt):
#   density-bearing (anaphora+causal) per-1000, SEED vs IMPROVED arms:
#     run1: SEED 3.64 / IMPROVED 3.84   (IDENTICAL anaph=18 causal=10 —
#           the improvement did not touch the gateable kinds at all)
#     run2: SEED 3.31 / IMPROVED 3.53   (IDENTICAL anaph=14 causal=5)
#     run3: SEED 9.10 / IMPROVED 7.21   (only run where they diverge)
#   negative controls (clean human originals): 0.00-1.88 (well separated)
#   EARNED-DENSE controls: rift3.md = 5.12, the-empty-pan_rev2 = 3.87.
#
# KEY FINDING (contradicts the naive design expectation, escalated to the
# operator): the density-bearing kinds do NOT separate the OQ-218 defect
# from its v0.2 fix. The SEED->IMPROVED signal lives almost entirely in the
# MERIT-CORRELATED kinds we are forbidden to gate on (refrain 40->20,
# aphorism 66->59 in run1) — anaphora/causal were essentially invariant.
# And earned-dense rift3 (5.12) outscores two of the three SEED defects.
# So the meter CANNOT separate earned-dense from lazy-dense on the gateable
# axis; the threshold is therefore PROVISIONAL, set ABOVE every observed
# earned/good dense story (rift3 5.12, run3-IMPROVED 7.21) so none of them
# gate. Only the single most extreme defect (run3 SEED 9.10) trips it, and
# it merely escalates OPEN. The real value is the full 6-kind candidate list
# injected for per-instance adjudication — where the defect signal actually
# lives — NOT this deliberately narrow auto-gate.
# REOPENS at the first earned-dense encounter above 8.0 (exactly as
# NUMERIC_DENSITY_THRESHOLD was provisional pending variance): a story that
# earns density > 8.0 on anaphora/causal is the datum that recalibrates this.
THEME_DENSITY_THRESHOLD = 8.0  # PROVISIONAL — above all observed earned-dense

# Standing caveat, rendered wherever a theme-density figure is read. The
# gate meters only the DETECTABLE slice of explanation over-run (anaphora,
# causal_chain); the merit-correlated majority (refrain, thesis-endings,
# state-after-show) stays in the adjudication layer and the operator read.
THEME_CAVEAT = (
    "Theme density meters only anaphora + causal_chain (the detectable, "
    "low-merit-correlation slice). refrain/aphorism/resonant_closer/"
    "word_arithmetic are adjudication-only and never move this number; a "
    "green density is NOT evidence explanation over-run is absent — the "
    "meter never auto-rejects, it escalates OPEN for the operator read."
)


def _sentences_with_lines(text: str) -> list[dict]:
    """Split story text into sentences with a starting-line anchor.

    Paragraphs are blank-line separated; sentences split on terminal
    punctuation. The last sentence of each paragraph carries
    is_para_final=True (used by the resonant_closer detector).
    """
    lines = text.splitlines()
    paras: list[list[tuple[int, str]]] = []
    cur: list[tuple[int, str]] = []
    for i, line in enumerate(lines, 1):
        if line.strip():
            cur.append((i, line))
        elif cur:
            paras.append(cur)
            cur = []
    if cur:
        paras.append(cur)

    sents: list[dict] = []
    for para in paras:
        chunks: list[str] = []
        char_line: list[int] = []
        for ln, lt in para:
            stripped = lt.strip()
            if chunks:
                chunks.append(" ")
                char_line.append(ln)
            chunks.append(stripped)
            char_line.extend([ln] * len(stripped))
        joined = "".join(chunks)
        para_sents: list[dict] = []
        pos = 0
        for m in _SENT_SPLIT_RE.finditer(joined):
            sent = joined[pos:m.start() + 1].strip()
            if sent:
                anchor = char_line[pos] if pos < len(char_line) else para[0][0]
                para_sents.append({"line": anchor, "text": sent})
            pos = m.end()
        tail = joined[pos:].strip()
        if tail:
            anchor = char_line[pos] if pos < len(char_line) else para[0][0]
            para_sents.append({"line": anchor, "text": tail})
        if para_sents:
            para_sents[-1]["is_para_final"] = True
        sents.extend(para_sents)
    return sents


def _lead_words(sent: str, n: int) -> list[str]:
    return [w.lower() for w in _WORDS_RE.findall(sent)][:n]


def _detect_anaphora(sents: list[dict]) -> tuple[list[dict], list[dict]]:
    """Density-bearing: runs of >=2 consecutive sentences sharing a
    sentence-initial phrase of >=3 words ("They do not tell us" x3)."""
    entries: list[dict] = []
    groups: list[dict] = []
    i, n = 0, len(sents)
    while i < n:
        shared = _lead_words(sents[i]["text"], 8)
        run = [i]
        k = i + 1
        while k < n:
            wk = _lead_words(sents[k]["text"], 8)
            common = 0
            for a, b in zip(shared, wk):
                if a == b:
                    common += 1
                else:
                    break
            if common >= 3:
                shared = shared[:common]
                run.append(k)
                k += 1
            else:
                break
        if len(run) >= 2:
            phrase = " ".join(shared)
            for idx in run:
                entries.append({
                    "line": sents[idx]["line"], "kind": "anaphora",
                    "token": phrase, "context": sents[idx]["text"][:160],
                })
            groups.append({
                "kind": "anaphora", "phrase": phrase, "count": len(run),
                "lines": [sents[idx]["line"] for idx in run],
            })
            i = k
        else:
            i += 1
    return entries, groups


def _detect_causal_chain(sents: list[dict]) -> tuple[list[dict], list[dict]]:
    """Density-bearing: the because...and because...therefore syllogism tell.
    Fires two ways — (a) a single sentence STACKING >=2 causal connectives
    (the compressed syllogism), or (b) >=2 near-consecutive sentences each
    carrying a connective (the drawn-out chain). Isolated causality (one
    connective, no neighbour) is NOT flagged."""
    per_sent = [_CAUSAL_RE.findall(s["text"]) for s in sents]
    counts = [len(x) for x in per_sent]
    entries: list[dict] = []
    groups: list[dict] = []
    i, n = 0, len(sents)
    while i < n:
        if counts[i] >= 2:  # (a) intra-sentence stack
            for m in _CAUSAL_RE.finditer(sents[i]["text"]):
                entries.append({
                    "line": sents[i]["line"], "kind": "causal_chain",
                    "token": m.group(0), "context": sents[i]["text"][:160],
                })
            groups.append({
                "kind": "causal_chain", "count": counts[i],
                "lines": [sents[i]["line"]], "intra_sentence": True,
            })
            i += 1
            continue
        if counts[i] >= 1:  # (b) cross-sentence cluster, window gap <= 1
            cluster = [i]
            k, gap = i + 1, 0
            while k < n and gap <= 1:
                if counts[k] >= 1:
                    cluster.append(k)
                    gap = 0
                else:
                    gap += 1
                k += 1
            if len(cluster) >= 2:
                for idx in cluster:
                    tok = _CAUSAL_RE.search(sents[idx]["text"]).group(0)
                    entries.append({
                        "line": sents[idx]["line"], "kind": "causal_chain",
                        "token": tok, "context": sents[idx]["text"][:160],
                    })
                groups.append({
                    "kind": "causal_chain", "count": len(cluster),
                    "lines": [sents[idx]["line"] for idx in cluster],
                })
                i = cluster[-1] + 1
                continue
        i += 1
    return entries, groups


def _normalize_sentence(s: str) -> str:
    return re.sub(r"\s+", " ", re.sub(r"[^a-z0-9 ]", "", s.lower())).strip()


def _detect_refrain(sents: list[dict]) -> tuple[list[dict], list[dict]]:
    """Adjudication-only: a normalized sentence recurring >=2 times ("The
    keeping became accounting." x4). Exact detection, but CANNOT distinguish
    lazy from earned (rift3's institutional creed) — so it forces per-
    instance adjudication and never moves the density gate."""
    from collections import defaultdict
    buckets: dict[str, list[dict]] = defaultdict(list)
    for s in sents:
        norm = _normalize_sentence(s["text"])
        if len(norm.split()) >= 3:
            buckets[norm].append(s)
    entries: list[dict] = []
    groups: list[dict] = []
    for norm, occ in buckets.items():
        if len(occ) >= 2:
            for s in occ:
                entries.append({
                    "line": s["line"], "kind": "refrain",
                    "token": norm[:60], "context": s["text"][:160],
                })
            groups.append({
                "kind": "refrain", "text": occ[0]["text"][:80],
                "count": len(occ), "lines": [s["line"] for s in occ],
            })
    return entries, groups


def _detect_aphorism(sents: list[dict]) -> list[dict]:
    """Adjudication-only: stage8.md's standalone-aphorism proxy made
    mechanical — generic sentence-initial subject + abstract-noun density +
    no mid-sentence proper noun / scene deixis. High-recall by design."""
    entries: list[dict] = []
    for s in sents:
        text = s["text"]
        words = text.split()
        if not 3 <= len(words) <= 30:
            continue
        if not _GENERIC_SUBJECT_RE.match(text):
            continue
        mid = [w.strip(".,;:!?\"'()") for w in words[1:]]
        if any(w and w[0].isupper() and w != "I" for w in mid):
            continue  # world-specific proper noun ⇒ not a bare generalization
        toks = {w.lower() for w in _WORDS_RE.findall(text)}
        if not toks & _ABSTRACT_NOUNS:
            continue
        entries.append({
            "line": s["line"], "kind": "aphorism",
            "token": text[:60], "context": text[:160],
        })
    return entries


def _detect_resonant_closer(
    sents: list[dict], aphorism_lines: set[int]
) -> list[dict]:
    """Adjudication-only: paragraph-final sentence landing on a summarizing
    image, often "the way X" (operator Web-Claude read, 2026-07-13), or an
    aphorism-shaped closer."""
    entries: list[dict] = []
    for s in sents:
        if not s.get("is_para_final"):
            continue
        if _THE_WAY_RE.search(s["text"]) or s["line"] in aphorism_lines:
            entries.append({
                "line": s["line"], "kind": "resonant_closer",
                "token": s["text"][:60], "context": s["text"][:160],
            })
    return entries


def _detect_word_arithmetic(sents: list[dict]) -> list[dict]:
    """Adjudication-only: prose arithmetic ("Quota minus rejections equals
    certified placements"). The empty-pan hard case; always flag-not-fail."""
    entries: list[dict] = []
    for s in sents:
        for m in _WORD_ARITH_RE.finditer(s["text"]):
            entries.append({
                "line": s["line"], "kind": "word_arithmetic",
                "token": m.group(0)[:60], "context": s["text"][:160],
            })
    return entries


_THEME_KINDS = (
    "anaphora", "causal_chain",  # density-bearing
    "refrain", "aphorism", "resonant_closer", "word_arithmetic",  # adjudication
)
_THEME_DENSITY_KINDS = ("anaphora", "causal_chain")


def _theme_inventory(text: str) -> dict:
    """Deterministic theme-naming / explanation-over-run extraction. Pure.

    Returns entries (line, kind, token, context), per-kind counts, repetition
    groupings, word count, and density per 1,000 words computed from the two
    DENSITY-BEARING kinds only (anaphora + causal_chain). The merit-correlated
    kinds (refrain, aphorism, resonant_closer, word_arithmetic) are listed for
    per-instance adjudication and NEVER contribute to density_per_1000 — the
    bucket invariant, locked by python/tests/test_theme_inventory.py.
    """
    sents = _sentences_with_lines(text)
    anaphora_e, anaphora_g = _detect_anaphora(sents)
    causal_e, causal_g = _detect_causal_chain(sents)
    refrain_e, refrain_g = _detect_refrain(sents)
    aphorism_e = _detect_aphorism(sents)
    aphorism_lines = {e["line"] for e in aphorism_e}
    closer_e = _detect_resonant_closer(sents, aphorism_lines)
    wordarith_e = _detect_word_arithmetic(sents)

    entries = (anaphora_e + causal_e + refrain_e
               + aphorism_e + closer_e + wordarith_e)
    entries.sort(key=lambda e: (e["line"], e["kind"]))
    counts = {k: 0 for k in _THEME_KINDS}
    for e in entries:
        counts[e["kind"]] += 1
    words = _word_count(text)
    density = 1000.0 * sum(counts[k] for k in _THEME_DENSITY_KINDS) / max(words, 1)
    return {
        "word_count": words,
        "counts": counts,
        "density_per_1000": round(density, 2),
        "density_kinds": list(_THEME_DENSITY_KINDS),
        "threshold": THEME_DENSITY_THRESHOLD,
        "caveat": THEME_CAVEAT,
        "groupings": anaphora_g + causal_g + refrain_g,
        "entries": entries,
    }


def _theme_inventory_density_only(inv: dict) -> dict:
    """Return a copy of a theme inventory keeping only the density-bearing
    entries — used to build the FLAGGED block for the one revision call so
    the model never revises earned refrains/aphorisms out of the story."""
    dense = [e for e in inv["entries"] if e["kind"] in _THEME_DENSITY_KINDS]
    return {**inv, "entries": dense,
            "groupings": [g for g in inv["groupings"]
                          if g["kind"] in _THEME_DENSITY_KINDS]}


def _format_theme_inventory(inv: dict, header: str) -> str:
    """Render a theme inventory as a prompt block for per-instance
    adjudication (mirrors _format_numeric_inventory)."""
    c = inv["counts"]
    out = [
        f"=== {header} ===",
        f"Computed by the orchestrator (deterministic; complete). Story word "
        f"count: {inv['word_count']:,}. Theme density (density-bearing kinds "
        f"only): {inv['density_per_1000']:.1f} per 1,000 words.",
        f"Density-bearing (move the gate): anaphora={c['anaphora']}, "
        f"causal_chain={c['causal_chain']}.",
        f"Adjudication-only (listed, NEVER gated): refrain={c['refrain']}, "
        f"aphorism={c['aphorism']}, resonant_closer={c['resonant_closer']}, "
        f"word_arithmetic={c['word_arithmetic']}.",
        f"CAVEAT: {THEME_CAVEAT}",
        "",
        "Adjudicate EVERY item below, per instance: KEEP only where the "
        "repetition/aphorism/closer is EARNED — a load-bearing device the "
        "prose uses on purpose (an incantatory institutional voice, a "
        "narrator's own survival math acted on in-scene, a refrain whose "
        "recurrence is the point). Otherwise revise: cut the thesis-"
        "restatement, break the syllogism into dramatized consequence, or "
        "vary the sentence-initial repetition. You may not waive this list "
        "wholesale, and you may not claim a theme-naming item is absent: "
        "this list is the ground truth. (High-recall by design — false "
        "positives are EXPECTED and absorbed by this adjudication; the gate "
        "never auto-rejects.)",
        "",
    ]
    for g in inv["groupings"]:
        if g["kind"] == "anaphora":
            out.append(
                f"ANAPHORA (lines {g['lines']}, x{g['count']}): "
                f"'{g['phrase']}...' — consecutive sentence-initial repetition.")
        elif g["kind"] == "causal_chain":
            out.append(
                f"CAUSAL CHAIN (lines {g['lines']}, x{g['count']}): repeated "
                f"because/therefore formula — the syllogism/thesis-chain tell.")
        elif g["kind"] == "refrain":
            out.append(
                f"REFRAIN (lines {g['lines']}, x{g['count']}): "
                f"'{g['text']}' — ADJUDICATION-ONLY (earned refrain is craft).")
    if inv["groupings"]:
        out.append("")
    shown = inv["entries"][:_MAX_INVENTORY_LISTING]
    for e in shown:
        out.append(f"L{e['line']} [{e['kind']}] {e['token']!r}: {e['context']}")
    omitted = len(inv["entries"]) - len(shown)
    if omitted > 0:
        out.append(
            f"... {omitted} additional entries omitted from this listing for "
            f"length; the totals above cover ALL entries, and the omitted "
            f"ones still require adjudication if unearned.")
    return "\n".join(out) + "\n"


# ---------------------------------------------------------------------------
# Invariant contract threading (R13/R14)
#
# Stage 2 writes SECTION 0: INVARIANT CONTRACT — previously orphaned after
# stage 4. The orchestrator extracts it and feeds it to stages 9 and 10 so
# the invariant has a downstream consumer. Stage 0 (the only source-sighted
# stage) additionally authors a surface-free contract + inherent_instrument
# flag carried into stage 2's input.
# ---------------------------------------------------------------------------

_SECTION0_HEADER_RE = re.compile(
    r'^\s{0,3}(?:#{1,6}\s+)?\*{0,2}SECTION 0\s*[:—–-]\s*INVARIANT CONTRACT.*$',
    flags=re.MULTILINE | re.IGNORECASE,
)
_SECTION1_HEADER_RE = re.compile(
    r'^\s{0,3}(?:#{1,6}\s+)?\*{0,2}SECTION 1\b.*$',
    flags=re.MULTILINE | re.IGNORECASE,
)
_STAGE0_CONTRACT_RE = re.compile(
    r'<invariant_contract>.*?</invariant_contract>',
    flags=re.DOTALL | re.IGNORECASE,
)
# Break contract (rides the R14 plumbing): stage 0 authors the break's
# ADDRESS (original_break / prior_status / target_prior, surface-free);
# execution belongs to the story stages downstream. Carried to stages
# 2 (affordance gate), 9 (break naming), 10 (D10, informational).
_STAGE0_BREAK_RE = re.compile(
    r'<break_contract>.*?</break_contract>',
    flags=re.DOTALL | re.IGNORECASE,
)

# Stage-9 falsifier hand-off (operator ruling 2026-07-12): stage 9's blind
# falsifier finding is fed to stage 10 as a MANDATORY D9 adjudication
# target — the externally supplied candidate D9 doesn't get to choose,
# the same architecture as _numeric_inventory for counting. Witnessed
# need: D9 scored 5 on the negative control by refuting a passage of its
# own choosing while walking past the kill passage stage 9 had named.
_S9_FALSIFIER_RE = re.compile(
    r'^\s{0,3}(?:#{1,6}\s+)?\*{0,2}INVARIANT FALSIFIER\*{0,2}\s*:?\s*$'
    r'(.*?)'
    # BREAK is in the alternatives because stage 9's output format places
    # its BREAK section between INVARIANT FALSIFIER and READINESS — without
    # it the falsifier extraction swallows the break finding into the D9
    # payload.
    r'(?=^\s{0,3}(?:#{1,6}\s+)?\*{0,2}(?:BREAK|READINESS|ROUTE)\b)',
    flags=re.MULTILINE | re.DOTALL | re.IGNORECASE,
)


def _extract_stage9_falsifier(stage_9_output: str) -> str:
    """Extract the INVARIANT FALSIFIER section from a stage-9 review.

    Returns '' when the section is absent (pre-threading outputs)."""
    m = _S9_FALSIFIER_RE.search(stage_9_output)
    return m.group(0).strip() + "\n" if m else ""


def _extract_invariant_contract(stage_2_output: str) -> str:
    """Extract SECTION 0: INVARIANT CONTRACT from stage 2 output ('' if absent)."""
    m = _SECTION0_HEADER_RE.search(stage_2_output)
    if not m:
        return ""
    m2 = _SECTION1_HEADER_RE.search(stage_2_output, m.end())
    end = m2.start() if m2 else len(stage_2_output)
    return stage_2_output[m.start():end].strip() + "\n"


def _extract_stage0_contract(stage_0_output: str) -> str:
    """Extract the <invariant_contract> block from stage 0 output ('' if absent)."""
    m = _STAGE0_CONTRACT_RE.search(stage_0_output)
    return m.group(0) if m else ""


def _extract_stage0_break_contract(stage_0_output: str) -> str:
    """Extract the <break_contract> block from stage 0 output ('' if absent)."""
    m = _STAGE0_BREAK_RE.search(stage_0_output)
    return m.group(0) if m else ""


# Stage-2 dominance-ordering clause (OQ-219 routing outcome (a), operator ruling
# 2026-07-13). Injected into the Stage-2 prompt IFF the Stage-0 contract AUTHORS
# missing_floor present="yes" AND primary="yes" — a STRUCTURAL gate, never
# model-inferred (R3(b) architecture, third application). The floor is the grain's
# structural sibling (not a break-species: presupposition vs unreadability); when
# it is the world's Tier-1 real, the grain must be subordinated on-screen so the
# floor's contract-dominance carries into the reader-facing story. INERT on
# grain-primary / no-primary sources (dual-real competition is legitimate there).
# Provenance + the run that settled it: audits/2026-07-13_oq219_missing_floor/.
_STAGE2_DOMINANCE_CLAUSE = (
    "=== DOMINANCE ORDERING (floor-primary source; gated on the Stage-0 contract) ===\n"
    "This source's Stage-0 contract marks missing_floor PRIMARY.\n\n"
    "- The floor is the world's Tier-1 real: the zero-point the system's honest\n"
    "  operation presupposes and cannot audit. Naturalize it as what the\n"
    "  instruments stand ON, not a thing they fail to read.\n"
    "- If the contract also carries a grain (untranslatable_real present), SUBORDINATE\n"
    "  it on-screen: at least one beat where the grain's question resolves or recedes\n"
    "  while the floor's question stands — two-reals subordination, applied floor-over-grain.\n"
    "- The instrument stays HONEST. The floor's native carrier is a true reading\n"
    "  taken from a bought zero (the scale weighs true; ask it to weigh the mountain).\n"
    "  Never smuggle the floor in as miscalibration — a dishonest instrument converts\n"
    "  the floor back into a correctable."
)

_MISSING_FLOOR_TAG_RE = re.compile(r'<missing_floor\b[^>]*>')
_ATTR_PRESENT_YES_RE = re.compile(r'present\s*=\s*["\']yes["\']')
_ATTR_PRIMARY_YES_RE = re.compile(r'primary\s*=\s*["\']yes["\']')


def _contract_marks_floor_primary(stage0_contract: str) -> bool:
    """True iff the Stage-0 invariant contract AUTHORS missing_floor as primary.

    Authored flag ONLY — never model-inferred (OQ-219). Requires the
    <missing_floor> tag to carry BOTH present="yes" AND primary="yes".
    """
    if not stage0_contract:
        return False
    m = _MISSING_FLOOR_TAG_RE.search(stage0_contract)
    if not m:
        return False
    tag = m.group(0)
    return bool(_ATTR_PRESENT_YES_RE.search(tag) and _ATTR_PRIMARY_YES_RE.search(tag))


def _stage2_dominance_suffix(stage0_contract: str) -> str:
    """The dominance-ordering clause text iff the contract marks the floor primary,
    else '' (INERT). This is the seam the free negative-control fixture tests
    (`python/tests/test_stage2_dominance_gate.py`, OQ-219)."""
    return _STAGE2_DOMINANCE_CLAUSE if _contract_marks_floor_primary(stage0_contract) else ""


_WORD_COUNT_LINE_RE = re.compile(
    r'^\s*(?:#{1,6}\s+)?\*{0,2}WORD COUNT:?\*{0,2}.*$',
    flags=re.MULTILINE | re.IGNORECASE,
)


def _rewrite_manifest_word_count(manifest: str, in_words: int, out_words: int) -> str:
    """Replace any model-emitted WORD COUNT line with computed values (R7).

    Models cannot count (witnessed: claimed 13,400 over a 5,927-word file);
    the orchestrator's figures are the only valid ones.
    """
    if not manifest:
        return manifest
    pct = 0.0 if in_words == 0 else (in_words - out_words) / in_words * 100.0
    computed = (
        f"WORD COUNT (computed by orchestrator): {in_words:,} → {out_words:,} "
        f"({pct:+.1f}% reduction)" if in_words else
        f"WORD COUNT (computed by orchestrator): {out_words:,}"
    )
    computed += (
        "\n(Any other word-count figure appearing in this manifest is "
        "model-emitted and must be ignored.)"
    )
    if _WORD_COUNT_LINE_RE.search(manifest):
        manifest, n = _WORD_COUNT_LINE_RE.subn(computed, manifest)
        if n > 1:
            _log.info("Rewrote %d WORD COUNT lines in edit manifest", n)
    else:
        m = _EDIT_MANIFEST_HEADER_RE.search(manifest)
        if m:
            insert_at = m.end()
            manifest = manifest[:insert_at] + "\n\n" + computed + manifest[insert_at:]
    return manifest


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    """Result of a single pipeline stage."""
    step: str           # stage_0 .. stage_6, scope, constraint_gen, prolog_engine
    status: str         # success | error | skipped | gate_halt
    data: Any = None
    error: str = ""
    tokens_in: int = 0
    tokens_out: int = 0
    duration_s: float = 0.0
    model_used: str = ""
    provider: str = ""


@dataclass
class PipelineResult:
    """Aggregated result across all stages."""
    run_id: str = ""
    mode: str = "narrative"
    source_story: str = ""
    stage_outputs: dict[str, str] = field(default_factory=dict)
    steps: list[StepResult] = field(default_factory=list)
    total_tokens_in: int = 0
    total_tokens_out: int = 0
    total_duration_s: float = 0.0
    output_dir: Path | None = None
    story_path: Path | None = None
    original_title: str = ""
    # Constraint engine artifacts
    scope_manifest: dict | None = None
    constraint_stories: list[dict] = field(default_factory=list)
    constraint_report_paths: list[Path] = field(default_factory=list)
    # Editorial pipeline state
    editorial_cycles: int = 0
    review_route: str = ""  # "VALIDATION" or "STRATEGY" or ""


# ---------------------------------------------------------------------------
# Provider abstraction
# ---------------------------------------------------------------------------

class LLMProvider(Protocol):
    """Interface for LLM providers."""
    def call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        """Return (text, tokens_in, tokens_out)."""
        ...

    @property
    def name(self) -> str: ...


class AnthropicProvider:
    """Claude via Anthropic API."""

    name = "anthropic"

    def __init__(self):
        self._client = None

    def _get_client(self):
        if self._client is None:
            import anthropic
            self._client = anthropic.Anthropic()
        return self._client

    def call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        import anthropic

        client = self._get_client()
        # Sonnet 5 / Opus 4.7+ reject non-default sampling params (400);
        # Sonnet 5 runs ADAPTIVE thinking when the field is omitted, which
        # would spend the calibrated per-stage max_tokens caps on thinking
        # — pin it off. Legacy models keep the per-stage temperature.
        # (Same rule as agent/llm_call.py sampling_overrides; kept local —
        # this module is self-contained.)
        kwargs: dict[str, Any] = {
            "model": model,
            "max_tokens": max_tokens,
            "messages": [{"role": "user", "content": prompt}],
        }
        if model.startswith(("claude-sonnet-5", "claude-opus-4-7",
                             "claude-opus-4-8", "claude-fable", "claude-mythos")):
            if model.startswith("claude-sonnet-5"):
                kwargs["thinking"] = {"type": "disabled"}
        else:
            kwargs["temperature"] = temperature
        if system_instruction:
            kwargs["system"] = system_instruction

        response = self._call_with_retry(client, **kwargs)
        total_in = response.usage.input_tokens
        total_out = response.usage.output_tokens

        # Handle pause_turn continuation (web search etc.)
        max_cont = 5
        while getattr(response, "stop_reason", None) == "pause_turn" and max_cont > 0:
            max_cont -= 1
            kwargs["messages"] = [
                {"role": "user", "content": prompt},
                {"role": "assistant", "content": response.content},
            ]
            response = self._call_with_retry(client, **kwargs)
            total_in += response.usage.input_tokens
            total_out += response.usage.output_tokens

        text = self._extract_text(response)
        return text, total_in, total_out

    @staticmethod
    def _extract_text(response) -> str:
        parts = []
        for block in response.content:
            if hasattr(block, "text"):
                parts.append(block.text)
        return "\n".join(parts)

    @staticmethod
    def _call_with_retry(client, max_retries: int = 3, **kwargs):
        import anthropic

        for attempt in range(max_retries):
            try:
                with client.messages.stream(**kwargs) as stream:
                    for _chunk in stream.text_stream:
                        pass
                    return stream.get_final_message()
            except (
                anthropic.RateLimitError,
                anthropic.InternalServerError,
                anthropic.APIConnectionError,
            ) as e:
                if attempt == max_retries - 1:
                    raise
                wait = 2 ** attempt * 2
                _log.warning("Retry %d after %ss: %s", attempt + 1, wait, e)
                time.sleep(wait)
            except anthropic.APIError:
                raise


class GoogleProvider:
    """Gemini via Google GenAI API (google-genai SDK)."""

    name = "google"

    def __init__(self):
        self._client = None

    def _get_client(self):
        if self._client is None:
            from google import genai
            api_key = os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY")
            if api_key:
                self._client = genai.Client(api_key=api_key)
            else:
                self._client = genai.Client()
        return self._client

    def call(
        self,
        prompt: str,
        model: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        from google.genai import types, errors

        client = self._get_client()
        config = types.GenerateContentConfig(
            system_instruction=system_instruction or None,
            temperature=temperature,
            max_output_tokens=max_tokens,
        )

        max_retries = 3
        for attempt in range(max_retries):
            try:
                response = client.models.generate_content(
                    model=model,
                    contents=prompt,
                    config=config,
                )
                text = response.text or ""
                tin = getattr(response.usage_metadata, "prompt_token_count", 0)
                tout = getattr(response.usage_metadata, "candidates_token_count", 0)
                return text, tin, tout
            except errors.ServerError as e:
                if attempt == max_retries - 1:
                    raise
                wait = 2 ** attempt * 2
                _log.warning("Gemini retry %d after %ss (server error): %s", attempt + 1, wait, e)
                time.sleep(wait)
            except errors.ClientError as e:
                if getattr(e, "code", 0) == 429:
                    if attempt == max_retries - 1:
                        raise
                    wait = 2 ** attempt * 5
                    _log.warning("Gemini retry %d after %ss (rate limit): %s", attempt + 1, wait, e)
                    time.sleep(wait)
                else:
                    raise


# ---------------------------------------------------------------------------
# Provider registry
# ---------------------------------------------------------------------------

def _build_providers() -> dict[str, Any]:
    """Build available providers based on environment."""
    providers: dict[str, Any] = {}

    if os.environ.get("ANTHROPIC_API_KEY"):
        providers["anthropic"] = AnthropicProvider()
    else:
        _log.warning("ANTHROPIC_API_KEY not set — Claude stages will fail")

    if os.environ.get("GOOGLE_API_KEY") or os.environ.get("GEMINI_API_KEY"):
        providers["google"] = GoogleProvider()
    else:
        _log.warning("GOOGLE_API_KEY/GEMINI_API_KEY not set — stage 0 will fail")

    return providers


# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

class UKEOrchestrator:
    """Two-provider pipeline for UKE constraint translation.

    Supports two modes:
      - narrative: 11-stage story pipeline (stage0.md .. stage10.md)
        Stages 0-4: Generation (extraction, formalization, naturalization,
                     editorial decisions, story generation)
        Stages 5-10: Editorial (discovery, strategy, structure/rewrite,
                      pacing/subtraction, review, validation)
        Optional constraint engine between stages 1 and 2.
      - artifact: 7-stage software generation (artifact_stage0.md .. artifact_stage6.md)

    Stage 0: Google Gemini (constraint extraction)
    Remaining stages: Anthropic Claude

    Workshop mode: point at a story file with --from-stage stage_5 to run
    the editorial pipeline without the generation stages.
    """

    DEFAULT_MODELS = {
        "stage_0":  ("google",    "gemini-2.5-pro"),
        "stage_1":  ("anthropic", "claude-sonnet-5"),
        "stage_2":  ("anthropic", "claude-sonnet-5"),
        "stage_3":  ("anthropic", "claude-sonnet-5"),
        "stage_4":  ("anthropic", "claude-sonnet-5"),
        "stage_5":  ("anthropic", "claude-sonnet-5"),
        "stage_6":  ("anthropic", "claude-sonnet-5"),
        "stage_7":  ("anthropic", "claude-sonnet-5"),
        "stage_8":  ("anthropic", "claude-sonnet-5"),
        "stage_9":  ("anthropic", "claude-sonnet-5"),
        "stage_10": ("anthropic", "claude-sonnet-5"),
    }

    TEMPERATURES = {
        "stage_0":  0.1,
        "stage_1":  0.1,
        "stage_2":  0.3,
        "stage_3":  0.3,
        "stage_4":  0.8,
        "stage_5":  0.3,
        "stage_6":  0.3,
        "stage_7":  0.7,
        "stage_8":  0.5,
        "stage_9":  0.3,
        "stage_10": 0.2,
    }

    TEMPERATURE_OVERRIDES = {
        "narrative": {
            "stage_2":  0.7,
            "stage_4":  0.8,
            "stage_5":  0.3,
            "stage_6":  0.3,
            "stage_7":  0.7,
            "stage_8":  0.5,
            "stage_9":  0.3,
            "stage_10": 0.2,
        },
        "artifact": {
            "stage_2": 0.1,
            "stage_3": 0.5,
            "stage_4": 0.5,
            "stage_5": 0.7,
            "stage_6": 0.2,
        },
    }

    MAX_TOKENS = {
        # Gemini 2.5 Pro's thinking tokens count against max_output_tokens;
        # at 8192 the R14 contract block (end of the manifest) was truncated
        # off (witnessed 2026-07-12: output cut mid-tag at 4.5KB while the
        # same-cap baseline completed at 6.9KB). Headroom is cheap; the
        # truncation guard in _run_stage_0 fails loud if it ever recurs.
        "stage_0":  24576,
        # stage_1/2/3 raised 2026-07-12: arm-1 run 1783838645 hit the old
        # caps EXACTLY (stage_2 at 8192 — omega log cut mid-word; stage_3
        # at 4096 — blueprint truncated, <numeric_register> never emitted)
        # and downstream stages consumed the partial outputs silently
        # (OQ-216). The cap-hit guard in _call now fails loud on this.
        "stage_1":  16384,
        "stage_2":  16384,
        # stage_3 raised 12288 → 16384 (2026-07-12, OQ-218 Stage-2 run 2):
        # the_eighth_commentary blueprint hit 12288 exactly (cap-hit guard
        # fired, fail-loud as designed); run 1's blueprint reached 11546 —
        # the cap was marginally sized for richer sources, not a one-off.
        "stage_3":  16384,
        "stage_4":  16384,
        "stage_5":  8192,
        "stage_6":  8192,
        "stage_7":  16384,
        "stage_8":  16384,
        "stage_9":  16384,
        "stage_10": 8192,
    }

    def __init__(
        self,
        mode: str = "narrative",
        models: dict[str, tuple[str, str]] | None = None,
        dr_logic_path: str | Path | None = None,
        output_dir: str | Path | None = None,
        skip_engine: bool = False,          # NEW: skip constraint engine
        dry_run: bool = False,
        force_gate: bool = False,
        progress_callback: Callable[[str, str], None] | None = None,
    ):
        self.mode = mode
        mode_config = PIPELINE_MODES[mode]
        self.all_stages = mode_config["stages"]
        self.validation_gates = mode_config["validation_gates"]
        self.air_gap_stage = mode_config["air_gap_stage"]
        self.final_output_dir = mode_config["output_dir"]

        self.models = {**self.DEFAULT_MODELS, **(models or {})}
        self.skip_engine = skip_engine
        self.dry_run = dry_run
        self.force_gate = force_gate
        self._progress = progress_callback or (lambda step, msg: print(f"[{step}] {msg}"))

        # Build mode-specific temperatures
        self.temperatures = {**self.TEMPERATURES}
        self.temperatures.update(self.TEMPERATURE_OVERRIDES.get(mode, {}))

        # Load stage instructions from .md files
        file_prefix = mode_config["file_prefix"]
        self.stage_prompts: dict[str, str] = {}
        for i, stage in enumerate(self.all_stages):
            stage_file = NARRATIVE_TRANSFORM_DIR / f"{file_prefix}{i}.md"
            if stage_file.exists():
                self.stage_prompts[stage] = _load_context_file(str(stage_file))
            else:
                _log.warning("Stage instruction file not found: %s", stage_file)
                self.stage_prompts[stage] = ""

        # Stage 7 rewrites prose, so it operates under stage 4's craft
        # directives. Append the canonical copy from stage4.md at load
        # time — a hand-pasted duplicate in stage7.md drifted stale
        # (Pattern 2: one-canonical-thing-became-two).
        if mode == "narrative" and self.stage_prompts.get("stage_7"):
            self._append_stage4_craft_directives()

        # Load DR logic references
        # - self.dr_logic: combined reference (artifact mode, backward compat)
        # - self.dr_logic_symbolic: formal spec (narrative stages 0, 1 — see STAGE_INPUTS)
        # - self.dr_logic_narrative: translation guide (narrative stage 2)
        if dr_logic_path is None:
            dr_logic_path = LOGIC_NARRATIVE_PATH
        self.dr_logic = ""
        if dr_logic_path and Path(dr_logic_path).exists():
            self.dr_logic = _load_context_file(str(dr_logic_path))

        self.dr_logic_symbolic = ""
        if LOGIC_SYMBOLIC_PATH.exists():
            self.dr_logic_symbolic = _load_context_file(str(LOGIC_SYMBOLIC_PATH))

        self.dr_logic_narrative = ""
        if LOGIC_NARRATIVE_TRANSLATION_PATH.exists():
            self.dr_logic_narrative = _load_context_file(str(LOGIC_NARRATIVE_TRANSLATION_PATH))

        # Output directory for intermediate results
        self.output_dir = Path(output_dir) if output_dir else None

        # Build provider registry
        self.providers = _build_providers()

        # ── NEW: Load constraint engine protocols ────────────────────
        self.engine_protocols: dict[str, str] = {}
        if not self.skip_engine and self.mode == "narrative":
            self._load_engine_protocols()

    # ------------------------------------------------------------------
    # Post-stage-8 instrumentation (R6 density gate, R7 computed counts)
    # ------------------------------------------------------------------

    def _post_process_stage_8(self, result: PipelineResult) -> str:
        """Run after every stage-8 completion, before the output is saved
        or read by stage 9.

        R7: the manifest's WORD COUNT line is overwritten with
        orchestrator-computed values (models cannot count — witnessed
        13,400 claimed over a 5,927-word file).
        R6: if numeric density is still above threshold after the pacing
        pass, issue ONE targeted revision call with the flagged lines,
        then — if still above — flag OPEN for the operator (fail-visible,
        no silent loop).
        """
        raw = result.stage_outputs.get("stage_8", "")
        story, manifest = _split_edit_manifest(raw)

        prev_story, _ = _split_edit_manifest(result.stage_outputs.get("stage_7", ""))
        manifest = _rewrite_manifest_word_count(
            manifest, _word_count(prev_story) if prev_story else 0,
            _word_count(story))

        inv = _numeric_inventory(story)
        self._save_json_sidecar("numeric_inventory_stage_8.json", inv)
        self._progress(
            "numeric_gate",
            f"Stage-8 numeric density {inv['density_per_1000']:.1f}/1000 "
            f"words (threshold {NUMERIC_DENSITY_THRESHOLD}). {DENSITY_CAVEAT}")
        if inv["density_per_1000"] > NUMERIC_DENSITY_THRESHOLD:
            self._progress(
                "numeric_gate",
                f"Numeric density {inv['density_per_1000']:.1f}/1000 words "
                f"exceeds threshold {NUMERIC_DENSITY_THRESHOLD} — "
                f"one targeted revision call")
            revised = self._numeric_revision_call(story, inv, result)
            if revised:
                if self.output_dir:
                    (self.output_dir / "stage_8_output_prenumeric.md").write_text(
                        raw, encoding="utf-8")
                story = revised
                inv2 = _numeric_inventory(story)
                self._save_json_sidecar(
                    "numeric_inventory_stage_8_postrevision.json", inv2)
                manifest = _rewrite_manifest_word_count(
                    manifest, _word_count(prev_story) if prev_story else 0,
                    _word_count(story))
                if inv2["density_per_1000"] > NUMERIC_DENSITY_THRESHOLD:
                    self._flag_numeric_open(inv2)
                else:
                    self._progress(
                        "numeric_gate",
                        f"Revision brought density to "
                        f"{inv2['density_per_1000']:.1f}/1000 words")
            else:
                self._flag_numeric_open(inv)

        # OQ-214: theme-density gate, parallel to the numeric one but
        # gating on anaphora+causal_chain ONLY (the density-bearing kinds).
        # Escalates OPEN, NEVER auto-rejects (see the invariant at
        # _theme_inventory). Measured on the (possibly numeric-revised) story.
        tinv = _theme_inventory(story)
        self._save_json_sidecar("theme_inventory_stage_8.json", tinv)
        self._progress(
            "theme_gate",
            f"Stage-8 theme density {tinv['density_per_1000']:.1f}/1000 "
            f"words (threshold {THEME_DENSITY_THRESHOLD}, density-bearing "
            f"kinds only). {THEME_CAVEAT}")
        if tinv["density_per_1000"] > THEME_DENSITY_THRESHOLD:
            self._progress(
                "theme_gate",
                f"Theme density {tinv['density_per_1000']:.1f}/1000 words "
                f"exceeds threshold {THEME_DENSITY_THRESHOLD} — "
                f"one targeted revision call")
            revised = self._theme_revision_call(story, tinv, result)
            if revised:
                if self.output_dir:
                    (self.output_dir / "stage_8_output_pretheme.md").write_text(
                        story, encoding="utf-8")
                story = revised
                tinv2 = _theme_inventory(story)
                self._save_json_sidecar(
                    "theme_inventory_stage_8_postrevision.json", tinv2)
                manifest = _rewrite_manifest_word_count(
                    manifest, _word_count(prev_story) if prev_story else 0,
                    _word_count(story))
                if tinv2["density_per_1000"] > THEME_DENSITY_THRESHOLD:
                    self._flag_theme_open(tinv2)
                else:
                    self._progress(
                        "theme_gate",
                        f"Revision brought theme density to "
                        f"{tinv2['density_per_1000']:.1f}/1000 words")
            else:
                self._flag_theme_open(tinv)

        combined = story.rstrip() + ("\n\n" + manifest if manifest else "\n")
        result.stage_outputs["stage_8"] = combined
        return combined

    def _numeric_revision_call(
        self, story: str, inv: dict, result: PipelineResult
    ) -> str | None:
        """One targeted revision pass over the flagged numeric lines.

        Returns the revised story text, or None on failure (the caller
        flags OPEN — never a silent retry loop).
        """
        t0 = time.time()
        system = (
            "You are performing a single targeted revision on a finished "
            "story. The orchestrator's deterministic meter found the story "
            "still anchored in counting: numbers used as ambient texture, "
            "countdown, tally, or emotional beat rather than as objects a "
            "character acts on.\n\n"
            "Revise ONLY the flagged lines (and the minimum surrounding "
            "prose needed for continuity). For each flagged number: keep it "
            "only if a character with positional access to that quantity "
            "acts on it in-scene; otherwise rewrite the line to carry the "
            "same pressure through sensation, consequence, or rhythm — "
            "never by hiding the number behind vague phrasing that still "
            "gestures at arithmetic.\n\n"
            "Do not summarize, restructure, cut scenes, or edit unflagged "
            "prose. Output ONLY the complete revised story text — no "
            "commentary, no manifest, no word counts."
        )
        prompt = (
            f"=== STORY ===\n{story}\n\n"
            + _format_numeric_inventory(
                inv, "FLAGGED NUMERIC INVENTORY (revise these)")
            + "\nOutput the complete revised story now."
        )
        try:
            provider_name, model = self.models["stage_8"]
            provider = self.providers.get(provider_name)
            if provider is None:
                raise RuntimeError(f"No provider registered for '{provider_name}'")
            text, tin, tout = provider.call(
                prompt=prompt,
                model=model,
                system_instruction=system,
                temperature=0.4,
                max_tokens=self.MAX_TOKENS.get("stage_8", 16384),
            )
            result.steps.append(StepResult(
                step="numeric_revision", status="success", data=None,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider_name,
            ))
            return text.strip() + "\n" if text and text.strip() else None
        except Exception as e:
            self._progress("numeric_gate", f"Revision call failed: {e}")
            result.steps.append(StepResult(
                step="numeric_revision", status="error", error=str(e),
                duration_s=time.time() - t0,
            ))
            return None

    def _flag_numeric_open(self, inv: dict):
        """Fail-visible: the density gate could not be satisfied this run."""
        msg = (
            f"NUMERIC DENSITY OPEN: {inv['density_per_1000']:.1f}/1000 words "
            f"(threshold {NUMERIC_DENSITY_THRESHOLD}) after the one allowed "
            f"revision call. Operator adjudication required — see "
            f"numeric_inventory_stage_8*.json for the per-line evidence."
        )
        self._progress("numeric_gate", msg)
        if self.output_dir:
            (self.output_dir / "NUMERIC_DENSITY_OPEN.md").write_text(
                msg + "\n", encoding="utf-8")

    def _theme_revision_call(
        self, story: str, inv: dict, result: PipelineResult
    ) -> str | None:
        """One targeted revision pass over the flagged DENSITY-BEARING theme
        lines (anaphora + causal_chain only). The merit-correlated kinds
        (refrain, aphorism, closer, word_arithmetic) are deliberately NOT in
        the revision set — earned refrains must never be revised out. Returns
        the revised story, or None on failure (caller flags OPEN — never a
        silent retry loop)."""
        t0 = time.time()
        system = (
            "You are performing a single targeted revision on a finished "
            "story. The orchestrator's deterministic meter found the story "
            "over-explaining structurally: consecutive sentence-initial "
            "repetition (anaphora) and repeated because/therefore formulas "
            "(causal chains) that restate the theme rather than dramatize "
            "it.\n\n"
            "Revise ONLY the flagged lines (and the minimum surrounding "
            "prose needed for continuity). For each flagged run: keep it "
            "only if the repetition is an EARNED device the prose uses on "
            "purpose (an incantatory voice, a deliberate structural echo); "
            "otherwise vary the sentence openings and break the syllogism "
            "into shown consequence, so the pressure lands through scene "
            "rather than restatement.\n\n"
            "Do NOT touch refrains, aphorisms, or other prose the meter did "
            "not flag here — those are adjudicated elsewhere. Do not "
            "summarize, restructure, cut scenes, or edit unflagged prose. "
            "Output ONLY the complete revised story text — no commentary, no "
            "manifest, no word counts."
        )
        prompt = (
            f"=== STORY ===\n{story}\n\n"
            + _format_theme_inventory(
                _theme_inventory_density_only(inv),
                "FLAGGED THEME INVENTORY (density-bearing; revise these)")
            + "\nOutput the complete revised story now."
        )
        try:
            provider_name, model = self.models["stage_8"]
            provider = self.providers.get(provider_name)
            if provider is None:
                raise RuntimeError(f"No provider registered for '{provider_name}'")
            text, tin, tout = provider.call(
                prompt=prompt,
                model=model,
                system_instruction=system,
                temperature=0.4,
                max_tokens=self.MAX_TOKENS.get("stage_8", 16384),
            )
            result.steps.append(StepResult(
                step="theme_revision", status="success", data=None,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider_name,
            ))
            return text.strip() + "\n" if text and text.strip() else None
        except Exception as e:
            self._progress("theme_gate", f"Revision call failed: {e}")
            result.steps.append(StepResult(
                step="theme_revision", status="error", error=str(e),
                duration_s=time.time() - t0,
            ))
            return None

    def _flag_theme_open(self, inv: dict):
        """Fail-visible: the theme-density gate could not be satisfied this
        run. NEVER an auto-reject — the operator adjudicates (invariant)."""
        msg = (
            f"THEME DENSITY OPEN: {inv['density_per_1000']:.1f}/1000 words "
            f"(threshold {THEME_DENSITY_THRESHOLD}, density-bearing kinds "
            f"anaphora+causal_chain) after the one allowed revision call. "
            f"This gate escalates, it does not reject — operator adjudication "
            f"required; see theme_inventory_stage_8*.json for the per-line "
            f"evidence, and remember the merit-correlated kinds "
            f"(refrain/aphorism/closer/word_arithmetic) are not in this "
            f"number by design."
        )
        self._progress("theme_gate", msg)
        if self.output_dir:
            (self.output_dir / "THEME_DENSITY_OPEN.md").write_text(
                msg + "\n", encoding="utf-8")

    def _append_stage4_craft_directives(self):
        """Append stage4.md's craft directives + prohibitions + checklist
        to the stage 7 system prompt (canonical single copy, R10)."""
        src = self.stage_prompts.get("stage_4", "")
        start = re.search(r'^### CRAFT DIRECTIVES\b', src, flags=re.MULTILINE)
        if not start:
            _log.warning("stage4.md has no '### CRAFT DIRECTIVES' section — "
                         "stage 7 runs without the appended craft directives")
            return
        end = re.search(r'^### Output\b', src, flags=re.MULTILINE)
        block = src[start.start():end.start() if end else len(src)].rstrip()
        self.stage_prompts["stage_7"] = (
            self.stage_prompts["stage_7"].rstrip()
            + "\n\n### Stage 4 Craft Directives Apply\n\n"
            + "(Appended by the orchestrator from stage4.md — the canonical copy. "
            + "Apply these to the rewritten prose.)\n\n"
            + block + "\n"
        )

    def _load_engine_protocols(self):
        """Load UKE_SCOPE, generation prompt, and schema for constraint engine."""
        if not _ensure_dr_engine() or _REPO_ROOT is None:
            self._progress("engine", "DR engine not available — constraint reports disabled")
            self.skip_engine = True
            return

        # gen_prompt, schema, and example are loaded for availability checks
        # (if any is missing, engine is disabled). The actual prompt assembly
        # delegates to story_generator_base.build_prompt() which loads its own copies.
        # These loaded protocols are also the injection point if narrative-derived
        # constraint stories ever need different prompts than analytical ones.
        protocol_files = {
            "uke_scope":  _REPO_ROOT / "prompts" / "uke_scope_v2_json.md",
            "gen_prompt": _REPO_ROOT / "prompts" / "constraint_story_generation_prompt_json.md",
            "schema":     _REPO_ROOT / "schemas" / "constraint_story_schema.json",
            "example":    _REPO_ROOT / "json" / "verification_bottleneck.json",
        }

        for key, path in protocol_files.items():
            if path.exists():
                self.engine_protocols[key] = _load_context_file(str(path))
            else:
                self._progress("engine", f"Missing protocol file: {path.name} — constraint reports disabled")
                self.skip_engine = True
                return

        self._progress("engine", "Constraint engine protocols loaded")

    # ------------------------------------------------------------------
    # Core call dispatcher
    # ------------------------------------------------------------------

    def _call(
        self,
        stage: str,
        prompt: str,
        system_override: str | None = None,
    ) -> tuple[str, int, int, str, str]:
        """Dispatch a call to the provider/model configured for this stage.

        Returns (text, tokens_in, tokens_out, model_used, provider_name).
        """
        provider_name, model = self.models[stage]
        provider = self.providers.get(provider_name)
        if provider is None:
            raise RuntimeError(f"No provider registered for '{provider_name}'")

        system = system_override or self.stage_prompts.get(stage, "")
        temp = self.temperatures.get(stage, 0.3)
        max_tok = self.MAX_TOKENS.get(stage, 8192)

        text, tin, tout = provider.call(
            prompt=prompt,
            model=model,
            system_instruction=system,
            temperature=temp,
            max_tokens=max_tok,
        )
        # Cap-hit guard (OQ-216): an output that used its entire token
        # budget was almost certainly cut mid-thought, and every witnessed
        # truncation (stage_2 @ 8192, stage_3 @ 4096, run 1783838645) was
        # consumed silently by the next stage. Fail loud instead.
        # NOTE: reliable for Anthropic (tokens_out == cap on truncation);
        # Gemini's candidates_token_count EXCLUDES thinking tokens, so a
        # Gemini cap-hit can pass this guard — Gemini stages need semantic
        # closure checks as well (stage 0 has one).
        if tout >= max_tok:
            raise RuntimeError(
                f"{stage} output hit its MAX_TOKENS cap ({tout} >= {max_tok}) "
                f"— output is truncated; raise MAX_TOKENS['{stage}'] or "
                f"reduce the task")
        return text, tin, tout, model, provider_name

    def _call_engine(
        self,
        prompt: str,
        system_instruction: str = "",
        temperature: float = 0.2,
        max_tokens: int = 8192,
    ) -> tuple[str, int, int]:
        """Call Claude for constraint engine steps (SCOPE, generation).

        Uses the stage_1 model config (architect role).
        """
        provider_name, model = self.models["stage_1"]
        provider = self.providers.get(provider_name)
        if provider is None:
            raise RuntimeError(f"No provider registered for '{provider_name}'")

        text, tin, tout = provider.call(
            prompt=prompt,
            model=model,
            system_instruction=system_instruction,
            temperature=temperature,
            max_tokens=max_tokens,
        )
        return text, tin, tout

    # ------------------------------------------------------------------
    # Persistence helpers
    # ------------------------------------------------------------------

    def _save_stage_output(self, stage: str, text: str, result: PipelineResult):
        """Write intermediate output to disk for resume capability."""
        if self.output_dir is None:
            return
        self.output_dir.mkdir(parents=True, exist_ok=True)
        out_path = self.output_dir / f"{stage}_output.md"
        out_path.write_text(text, encoding="utf-8")
        result.stage_outputs[stage] = text

    def _load_stage_output(self, stage: str) -> str | None:
        """Load a previously saved stage output for resume."""
        if self.output_dir is None:
            return None
        path = self.output_dir / f"{stage}_output.md"
        if path.exists():
            return path.read_text(encoding="utf-8")
        return None

    def _save_json_sidecar(self, name: str, data: dict):
        """Write a JSON evidence sidecar into the run directory."""
        if self.output_dir is None:
            return
        self.output_dir.mkdir(parents=True, exist_ok=True)
        (self.output_dir / name).write_text(
            json.dumps(data, indent=2), encoding="utf-8")

    # ------------------------------------------------------------------
    # Output save logic
    # ------------------------------------------------------------------

    @staticmethod
    def _extract_original_title(source_path: Path | None) -> str:
        """Get the original story's title from its content or filename."""
        if source_path is None:
            return "Unknown"
        try:
            text = source_path.read_text(encoding="utf-8")
            title = _extract_title(text)
            if title and title != "untitled":
                return title
        except Exception:
            pass
        return source_path.stem.replace('_', ' ').title()

    @staticmethod
    def _extract_code_block(text: str) -> str | None:
        """Extract the dominant code block from text."""
        stripped = text.strip()
        code_starts = (
            "import ", "export ", "'use client'", '"use client"',
            "const ", "function ", "class ", "type ", "interface ",
            "// ", "/* ", "<!DOCTYPE", "<html",
        )
        if any(stripped.startswith(prefix) for prefix in code_starts):
            return stripped

        fences = list(re.finditer(r'```\w*\n(.*?)```', stripped, re.DOTALL))
        if len(fences) == 1:
            return fences[0].group(1).strip()
        if len(fences) > 1:
            blocks = [(f.group(1).strip(), len(f.group(1))) for f in fences]
            blocks.sort(key=lambda x: x[1], reverse=True)
            if blocks[0][1] > blocks[1][1] * 3:
                return blocks[0][0]
        return None

    @staticmethod
    def _is_code_output(text: str) -> bool:
        """Check if the output is code (for artifact mode)."""
        stripped = text.strip()
        code_indicators = (
            "import ", "export ", "'use client'", '"use client"',
            "const ", "function ", "class ", "type ",
        )
        return any(stripped.startswith(ind) for ind in code_indicators)

    def _save_final_output(
        self,
        content: str,
        original_title: str,
        output_dir: Path,
        is_code: bool = False,
        base_name: str | None = None,
    ) -> Path:
        """Save the final output (story or artifact) to the output directory.

        If base_name is provided, uses it as the filename base instead of
        extracting from content or title.
        """
        if is_code:
            code = self._extract_code_block(content)
            if code:
                content = code
            ext = ".tsx"
            base = base_name or (_title_to_filename(original_title) if original_title != "Unknown" else "artifact")
            trailer = f"\n// Original: {original_title}\n"
        else:
            ext = ".md"
            if base_name:
                base = base_name
            else:
                title = _extract_title(content)
                base = _title_to_filename(title)
            trailer = f"\n\n---\n*Original: {original_title}*\n"

        output_dir.mkdir(parents=True, exist_ok=True)
        out_path = output_dir / f"{base}{ext}"

        if out_path.exists():
            counter = 2
            while out_path.exists():
                out_path = output_dir / f"{base}_{counter}{ext}"
                counter += 1

        out_path.write_text(content + trailer, encoding="utf-8")
        return out_path

    # ------------------------------------------------------------------
    # Validation gate logic
    # ------------------------------------------------------------------

    @staticmethod
    def _check_validation_gate(step: StepResult) -> str:
        """Check if a validation stage output indicates PASS or FAIL."""
        if not step.data:
            return "halt"
        text = step.data
        fail_patterns = [r'\bFAIL\b', r'\bHALT\b', r'VALIDATION:\s*FAIL', r'GATE:\s*FAIL']
        pass_patterns = [r'\bPASS\b', r'VALIDATION:\s*PASS', r'GATE:\s*PASS']
        has_fail = any(re.search(p, text, re.IGNORECASE) for p in fail_patterns)
        has_pass = any(re.search(p, text, re.IGNORECASE) for p in pass_patterns)
        if has_fail:
            return "halt"
        if has_pass:
            return "pass"
        _log.warning("Validation gate output ambiguous for %s, halting", step.step)
        return "halt"

    @staticmethod
    def _parse_review_route(step: StepResult) -> str:
        """Parse Stage 9 Review output for route decision.

        Returns "VALIDATION", "STRATEGY", or "STRATEGY" (conservative default).
        """
        if not step.data:
            return "STRATEGY"
        text = step.data
        # Look for explicit ROUTE: VALIDATION or ROUTE: STRATEGY
        route_match = re.search(r'ROUTE:\s*(VALIDATION|STRATEGY)', text, re.IGNORECASE)
        if route_match:
            return route_match.group(1).upper()
        # Fallback: look for the words in context
        has_validation = bool(re.search(r'→\s*VALIDATION', text, re.IGNORECASE))
        has_strategy = bool(re.search(r'→\s*STRATEGY', text, re.IGNORECASE))
        if has_validation and not has_strategy:
            return "VALIDATION"
        # Conservative default: route to STRATEGY
        _log.warning("Review route ambiguous, defaulting to STRATEGY")
        return "STRATEGY"

    # ------------------------------------------------------------------
    # Artifact-specific prompt suffixes (unchanged from original)
    # ------------------------------------------------------------------

    ARTIFACT_PROMPT_SUFFIXES = {
        "stage_3": (
            "Follow the path selection and system naturalization protocol in your "
            "system instructions. You must output ALL of the following:\n"
            "1. Selected path (A/B/C/D/E) with justification from the decision matrix\n"
            "2. Selected modality with compatibility rating\n"
            "3. Air gap level confirmed\n"
            "4. Complete system architecture naturalization for the selected path\n"
            "5. Constraint-driven aesthetic specification (type-to-interaction and "
            "index-to-feel mappings for EVERY constraint)\n"
            "6. System personality specification (voice, diagnostic vocabulary, "
            "behavioral constants, self-description) — every trait must trace to "
            "a constraint or coupling\n\n"
            "The aesthetic and personality specs must be concrete enough to implement "
            "directly. No placeholders."
        ),
        "stage_4": (
            "Follow the interaction design and indexical revelation protocol in your "
            "system instructions. You must output ALL of the following:\n"
            "1. Canonical state object (complete JavaScript schema with all constraints, "
            "transformation rules, couplings, and system state)\n"
            "2. Index view derivation functions (read functions, not separate data stores)\n"
            "3. Interaction mapping for EVERY constraint from EVERY index position "
            "(metrics displayed, interface feel, available actions)\n"
            "4. At least one hysteresis point that changes what the user CAN DO or "
            "BELIEVES, not just what they see\n"
            "5. Misrecognition tolerance spec with anti-help constraints\n"
            "6. Shock events mapped from transformation rules\n"
            "7. State transition planning for each transformation rule\n"
            "8. Terminal state design\n\n"
            "The canonical state object must be complete and implementable. "
            "Stage 5 will build directly from it."
        ),
        "stage_5": (
            "Follow the artifact generation protocol in your system instructions. "
            "Output ONLY the complete, runnable .tsx file. No markdown wrapping, "
            "no commentary, no explanation, no validation sections. "
            "The entire response should be valid TypeScript/JSX that can be "
            "saved directly as a .tsx file and executed.\n\n"
            "CRITICAL ARCHITECTURE: Use immutable state with pure derivation functions. "
            "Do NOT use a mutable class with notifyListeners(). Instead:\n"
            "- State is a plain object, updated via a pure reducer function\n"
            "- Index views are pure functions of state (deriveIndexView(state, index))\n"
            "- React state management via useState/useReducer, not external subscriptions\n"
            "- No tick() or dispatch() methods on a class — use React's own dispatch\n\n"
            "Every constraint metric must be causally integrated: changing a value "
            "must propagate through couplings and affect system behavior. "
            "If calculateChi exists, verify it receives numeric parameters and "
            "returns a number used in comparisons."
        ),
        "stage_6": (
            "Follow the validation protocol in your system instructions.\n\n"
            "CRITICAL: Do not simply declare PASS. For each test, show your work:\n"
            "1. CONSTRAINT PRESERVATION: For each constraint, quote the code that "
            "implements it. Trace one value change through its coupling chain.\n"
            "2. INDEX DERIVATION: Find every calculateChi or index-derivation call. "
            "List the parameter types. Flag any comparison between an object and "
            "a string/number literal.\n"
            "3. CAUSAL INTEGRATION: Pick 3 constraints. For each, describe what "
            "happens in the code when its value changes. If nothing propagates, "
            "report FAIL.\n"
            "4. HYSTERESIS: Trace the hysteresis implementation. Does it change "
            "available actions or metric readings, or only visual overlays? "
            "Visual-only = FAIL.\n"
            "5. UCZ VARIANCE: Are random/stochastic calls present? Could they "
            "produce different outcomes on repeated runs?\n"
            "6. TYPE SAFETY: Check for patterns like (object === string), "
            "(array.includes(object)), or numeric operations on undefined.\n\n"
            "Output a structured report. End with PASS or FAIL and specific issues."
        ),
    }

    def _get_prompt_suffix(self, stage: str) -> str:
        if self.mode == "artifact" and stage in self.ARTIFACT_PROMPT_SUFFIXES:
            return self.ARTIFACT_PROMPT_SUFFIXES[stage]
        return "Follow the protocol in your system instructions for this stage."

    # ------------------------------------------------------------------
    # Generic stage runner (used by artifact mode)
    # ------------------------------------------------------------------

    def _run_stage_generic(
        self,
        stage: str,
        stage_outputs: dict[str, str],
        source_story: str,
    ) -> StepResult:
        """Data-driven stage runner. Assembles prompt from STAGE_INPUTS config."""
        stage_num = stage.split("_")[1]
        is_air_gap = (stage == self.air_gap_stage)

        if is_air_gap:
            self._progress(stage, f"Running stage {stage_num} (Claude, air gap active)...")
        else:
            self._progress(stage, f"Running stage {stage_num} (Claude)...")

        t0 = time.time()
        input_keys = STAGE_INPUTS[self.mode][stage]

        if is_air_gap:
            assert "source" not in input_keys, f"Air gap violation: source in {stage}"
            assert "stage_0" not in input_keys, f"Air gap violation: stage_0 in {stage}"

        # Review reads blind — stage_9 receives ONLY the edited story plus
        # the two contracts (structural commitments + falsifiers; both are
        # surface-free and carry no strategy, edit-history, or source
        # information, so blindness holds — R13 / break threading).
        mode_config = PIPELINE_MODES.get(self.mode, {})
        if stage == mode_config.get("review_blind_stage"):
            assert input_keys == ["stage_8", "invariant_contract", "break_contract"], (
                f"Review blind violation: {stage} receives {input_keys}, "
                f"expected ['stage_8', 'invariant_contract', 'break_contract']"
            )

        prompt_parts = []
        for key in input_keys:
            if key == "source":
                prompt_parts.append(f"=== SOURCE MATERIAL ===\n{source_story}\n\n")
            elif key == "dr_logic_symbolic":
                if self.dr_logic_symbolic:
                    prompt_parts.append(
                        f"=== SYMBOLIC CONSTRAINT LOGIC REFERENCE ===\n{self.dr_logic_symbolic}\n\n"
                    )
            elif key == "dr_logic_narrative":
                if self.dr_logic_narrative:
                    prompt_parts.append(
                        f"=== NARRATIVE TRANSLATION REFERENCE ===\n{self.dr_logic_narrative}\n\n"
                    )
            elif key == "dr_logic":
                if self.dr_logic:
                    prompt_parts.append(
                        f"=== INDEXED CONSTRAINT LOGIC REFERENCE ===\n{self.dr_logic}\n\n"
                    )
            elif key == "constraint_reports":
                content = stage_outputs.get(key, "")
                if content:
                    prompt_parts.append(
                        f"=== CONSTRAINT ENGINE REPORTS ===\n{content}\n\n"
                    )
            elif key == "invariant_contract":
                content = stage_outputs.get(key, "")
                if content:
                    prompt_parts.append(
                        f"=== INVARIANT CONTRACT (from naturalization; "
                        f"carried by orchestrator) ===\n{content}\n\n"
                    )
                else:
                    prompt_parts.append(
                        "=== INVARIANT CONTRACT ===\n"
                        "NOT AVAILABLE for this run (e.g. workshop/--edit "
                        "mode, or a pre-contract stage 2 output). Invariant "
                        "preservation is UNVERIFIED — say so explicitly; "
                        "never mark it N/A or silently skip it.\n\n"
                    )
            elif key == "break_contract":
                content = stage_outputs.get(key, "")
                if content:
                    prompt_parts.append(
                        f"=== BREAK CONTRACT (Stage 0, source-sighted; "
                        f"surface-free; carried by orchestrator) ===\n"
                        f"{content}\n\n"
                    )
                else:
                    prompt_parts.append(
                        "=== BREAK CONTRACT ===\n"
                        "NOT AVAILABLE for this run (e.g. workshop/--edit "
                        "mode, or a pre-break-contract stage 0 output). "
                        "Break presence is UNVERIFIED — say so explicitly; "
                        "never mark it N/A or silently skip it.\n\n"
                    )
            else:
                content = stage_outputs.get(key, "")
                if content:
                    # e.g. "stage_4" → "STAGE 4", "stage_1_anon" → "STAGE 1 (ANONYMIZED)"
                    if key.endswith("_anon"):
                        snum = key.split("_")[1]
                        label = f"STAGE {snum} (ANONYMIZED)"
                    else:
                        snum = key.split("_")[1]
                        label = f"STAGE {snum}"
                    # R7: models cannot count; every editorial stage receives
                    # the orchestrator-computed word count of the story text
                    # (manifest excluded) and is forbidden to emit its own.
                    if (self.mode == "narrative"
                            and key in ("stage_4", "stage_7", "stage_8")):
                        story_part, _ = _split_edit_manifest(content)
                        label += (
                            f" — ACTUAL WORD COUNT of story text: "
                            f"{_word_count(story_part):,} (computed by "
                            f"orchestrator; any other figure is wrong)"
                        )
                    prompt_parts.append(f"=== {label} OUTPUT ===\n{content}\n\n")

        # Validation mode is a fact the orchestrator knows — never model
        # judgment (witnessed 2026-07-12, OQ-215 arm 2: stage 10 declared
        # FULL (/40) and scored D1/D2 with NO constraint spec provided,
        # on both control runs).
        if self.mode == "narrative" and stage == "stage_10":
            if stage_outputs.get("stage_1_anon"):
                mode_line = (
                    "FULL MODE (/40) — the constraint specification IS "
                    "provided above.")
            else:
                mode_line = (
                    "CRAFT MODE (/25) — NO constraint specification was "
                    "provided. Do NOT score D1, D2, or D5 (mark N/A per "
                    "protocol); do not claim a /40 total.")
            prompt_parts.append(
                f"=== VALIDATION MODE (computed by orchestrator; not yours "
                f"to decide) ===\n{mode_line}\n\n"
            )
            # D9 compose (operator ruling 2026-07-12): the blind stage-9
            # falsifier finding is a mandatory adjudication target — an
            # ADDITION to D9's own strongest-candidate obligation, never
            # a replacement (a stage-9 false-negative must not become an
            # unchallenged PASS).
            s9 = stage_outputs.get("stage_9", "")
            finding = _extract_stage9_falsifier(s9) if s9 else ""
            if finding:
                prompt_parts.append(
                    "=== STAGE-9 BLIND FALSIFIER FINDING (carried by "
                    "orchestrator; MANDATORY D9 adjudication target) ===\n"
                    + finding +
                    "\nD9 obligation: adjudicate THIS finding explicitly — "
                    "refute the specific passage it flags against the text, "
                    "or concede it. You may not substitute a different "
                    "passage. This is an ADDITION to your own "
                    "strongest-candidate obligation, never a replacement.\n\n"
                )
            else:
                prompt_parts.append(
                    "=== STAGE-9 BLIND FALSIFIER FINDING ===\n"
                    "NOT AVAILABLE for this run. Your own "
                    "strongest-candidate obligation stands alone — state "
                    "explicitly that no stage-9 finding was provided.\n\n"
                )

        # R8: strategy targets must respect the downstream output caps —
        # stage 6 once set a 12,500-13,000-word target against a ~12k-word
        # ceiling, and the shortfall was papered over by a fabricated
        # count. Inject the feasible range, derived from MAX_TOKENS.
        if self.mode == "narrative" and stage == "stage_6":
            cap_tokens = min(self.MAX_TOKENS.get("stage_7", 16384),
                             self.MAX_TOKENS.get("stage_8", 16384))
            # ~0.75 words/token for English prose, minus headroom for the
            # edit manifest the same output must carry.
            feasible_words = int(cap_tokens * 0.70)
            prompt_parts.append(
                f"=== FEASIBLE RANGE (computed from output caps) ===\n"
                f"The rewrite and pacing stages can emit at most "
                f"{cap_tokens:,} tokens ≈ {feasible_words:,} words including "
                f"their edit manifest. Set the SCOPE target range with a "
                f"ceiling at or below {feasible_words:,} words — a target "
                f"above it is physically unreachable and produces silent "
                f"shortfalls.\n\n"
            )

        # R6: the rewrite/pacing stages receive the deterministic numeric
        # inventory of the story they are editing. Extraction is the
        # orchestrator's job; the model only adjudicates per instance.
        if self.mode == "narrative" and stage in ("stage_7", "stage_8"):
            story_key = "stage_4" if stage == "stage_7" else "stage_7"
            story_src = stage_outputs.get(story_key, "")
            if story_src:
                story_part, _ = _split_edit_manifest(story_src)
                inv = _numeric_inventory(story_part)
                self._save_json_sidecar(
                    f"numeric_inventory_{story_key}.json", inv)
                prompt_parts.append(_format_numeric_inventory(
                    inv,
                    f"NUMERIC INVENTORY of the story you are editing "
                    f"(computed from {story_key} output)",
                ))
                prompt_parts.append("\n")
                # OQ-214: the same story's deterministic theme-naming /
                # explanation-over-run inventory, injected for per-instance
                # adjudication (replaces stage8.md's waivable aphorism scan).
                tinv = _theme_inventory(story_part)
                self._save_json_sidecar(
                    f"theme_inventory_{story_key}.json", tinv)
                prompt_parts.append(_format_theme_inventory(
                    tinv,
                    f"THEME INVENTORY of the story you are editing "
                    f"(computed from {story_key} output)",
                ))
                prompt_parts.append("\n")

        prompt_parts.append(self._get_prompt_suffix(stage))
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call(stage, prompt)
            self._progress(stage, f"Stage {stage_num} complete ({tin}→{tout} tokens)")
            return StepResult(
                step=stage, status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress(stage, f"Failed: {e}")
            return StepResult(
                step=stage, status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ==================================================================
    # NEW: Constraint Engine Steps
    # ==================================================================

    def _step_scope(self, stage_4_output: str) -> StepResult:
        """Run UKE_SCOPE on Stage 4 narrative to decompose constraint axes.

        Evaluates the generated story to identify constraint dynamics
        actually realized in the narrative, then decomposes them into
        independent axes for constraint story generation and Prolog
        engine evaluation.
        """
        self._progress("scope", "Running UKE_SCOPE on Stage 4 narrative...")
        t0 = time.time()

        prompt = (
            "Analyze the following generated narrative using the UKE_SCOPE protocol.\n\n"
            "This is a story generated from a constraint topology. "
            "Your job is to identify the constraint dynamics actually realized in the narrative "
            "(e.g., 'agency depletion through contradictory authority,' "
            "'unrequited love as asymmetric extraction') "
            "and decompose them into independent axes suitable for constraint story generation.\n\n"
            "CRITICAL: Use abstract structural language for claim_ids, human_readable names, "
            "and structural_delta fields. Extract the GENERAL DYNAMIC, not plot summary.\n\n"
            "=== GENERATED NARRATIVE ===\n"
            f"{stage_4_output}\n\n"
            "Select exactly 3 axes for generation (or fewer if the topology is genuinely simple).\n\n"
            "Remember: OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON."
        )

        try:
            text, tin, tout = self._call_engine(
                prompt,
                system_instruction=self.engine_protocols["uke_scope"],
                temperature=0.2,
                max_tokens=8192,
            )
        except Exception as e:
            self._progress("scope", f"SCOPE call failed: {e}")
            return StepResult(
                step="scope", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Parse JSON
        try:
            from agent.story_generator_base import strip_json_fences
            manifest = json.loads(strip_json_fences(text))
        except (json.JSONDecodeError, ImportError) as e:
            # Fallback strip
            cleaned = re.sub(r'^```\w*\n?', '', text.strip())
            cleaned = re.sub(r'\n?```$', '', cleaned.strip())
            try:
                manifest = json.loads(cleaned)
            except json.JSONDecodeError:
                self._progress("scope", f"JSON parse failed: {e}")
                return StepResult(
                    step="scope", status="error",
                    error=f"JSON parse failed: {e}\nRaw:\n{text[:500]}",
                    duration_s=time.time() - t0,
                )

        # Validate minimum fields
        required = ["axes", "generation_sequence"]
        missing = [f for f in required if f not in manifest]
        if missing:
            self._progress("scope", f"Manifest missing fields: {missing}")
            return StepResult(
                step="scope", status="error",
                error=f"Missing fields: {missing}",
                data=manifest,
                duration_s=time.time() - t0,
            )

        # Log fracture warnings
        fracture = manifest.get("fracture_scan", {})
        if fracture.get("f03_hasty_generalization") or fracture.get("f34_epistemic_trespass"):
            self._progress("scope", f"Fracture warning: {fracture.get('notes', '')}")

        seq = manifest.get("generation_sequence", [])
        self._progress("scope", f"SCOPE complete — {len(seq)} axes: {seq}")

        # Save manifest
        if self.output_dir:
            self.output_dir.mkdir(parents=True, exist_ok=True)
            manifest_path = self.output_dir / "scope_manifest.json"
            manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")

        return StepResult(
            step="scope", status="success", data=manifest,
            tokens_in=tin, tokens_out=tout,
            duration_s=time.time() - t0,
        )

    def _step_generate_constraint_stories(self, manifest: dict) -> StepResult:
        """Generate constraint story JSONs from SCOPE manifest axes.

        Uses the same generation logic as c-orchestrator._step_generate,
        but imports the infrastructure from story_generator_base.
        """
        self._progress("constraint_gen", "Generating constraint story JSONs...")
        t0 = time.time()

        from agent.story_generator_base import (
            process_response, save_story, build_prompt, _SYSTEM_INSTRUCTION,
        )

        sequence = manifest.get("generation_sequence", [])
        axes_by_id = {a["claim_id"]: a for a in manifest.get("axes", [])}
        generated_stories = []
        total_tin, total_tout = 0, 0

        for i, claim_id in enumerate(sequence):
            axis = axes_by_id.get(claim_id)
            if not axis:
                self._progress("constraint_gen", f"Axis {claim_id} not found, skipping")
                continue

            self._progress("constraint_gen", f"[{i+1}/{len(sequence)}] Generating {claim_id}...")

            # Build source description from axis fields
            source_desc = (
                f"TOPIC: {manifest.get('domain', 'Structural Analysis')}\n"
                f"CONSTRAINT: {claim_id}\n"
                f"Structural delta: {axis.get('structural_delta', 'Unknown')}\n"
                f"Primary observable: {axis.get('primary_observable', 'Unknown')}\n"
                f"Hypothesis type: {axis.get('hypothesis', 'Unknown')}"
            )
            if axis.get("beneficiary"):
                source_desc += f"\nBeneficiary: {axis['beneficiary']}"
            if axis.get("victim"):
                source_desc += f"\nVictim: {axis['victim']}"

            # Build upstream context for downstream axes
            upstream_context = ""
            for upstream_id in axis.get("downstream_of", []):
                upstream_story = next(
                    (s for s in generated_stories
                     if s["header"]["constraint_id"] == upstream_id),
                    None,
                )
                if upstream_story:
                    upstream_context += (
                        f"\nUPSTREAM CONSTRAINT: {upstream_id}\n"
                        f"  claimed_type: {upstream_story['base_properties'].get('claimed_type', 'unknown')}\n"
                        f"  affects_constraint: {upstream_id} → {claim_id}\n"
                    )

            prompt = build_prompt(source_desc, upstream_context)

            try:
                text, tin, tout = self._call_engine(
                    prompt,
                    system_instruction=_SYSTEM_INSTRUCTION,
                    temperature=0.2,
                    max_tokens=8192,
                )
                total_tin += tin
                total_tout += tout
            except Exception as e:
                self._progress("constraint_gen", f"API error for {claim_id}: {e}")
                continue

            if not text:
                self._progress("constraint_gen", f"Empty response for {claim_id}")
                continue

            # Process and validate
            story_dict, errors = process_response(text)

            if story_dict is None or errors:
                # Retry once with error feedback
                self._progress("constraint_gen", f"Validation errors for {claim_id}, retrying...")
                feedback = ""
                if errors:
                    feedback = "\nYour previous attempt had these validation errors:\n"
                    for err in errors:
                        feedback += f"  - {err}\n"
                    feedback += "Fix these specific errors while keeping the rest correct.\n"

                retry_prompt = build_prompt(source_desc, upstream_context + feedback)
                try:
                    text, tin2, tout2 = self._call_engine(
                        retry_prompt,
                        system_instruction=_SYSTEM_INSTRUCTION,
                        temperature=0.2,
                        max_tokens=8192,
                    )
                    total_tin += tin2
                    total_tout += tout2
                    story_dict, errors = process_response(text)
                except Exception as e:
                    self._progress("constraint_gen", f"Retry failed for {claim_id}: {e}")
                    continue

            if story_dict is None or errors:
                self._progress("constraint_gen", f"Failed to generate valid story for {claim_id}")
                continue

            # Save to DR corpus (enables Prolog engine to find them)
            json_path, pl_path = save_story(story_dict, overwrite=True)
            if json_path:
                generated_stories.append(story_dict)
                self._progress("constraint_gen", f"Saved {claim_id}")

                # Also save to run output dir
                if self.output_dir:
                    self.output_dir.mkdir(parents=True, exist_ok=True)
                    run_json = self.output_dir / f"{claim_id}.json"
                    run_json.write_text(json.dumps(story_dict, indent=2), encoding="utf-8")

        self._progress(
            "constraint_gen",
            f"Generated {len(generated_stories)}/{len(sequence)} stories"
        )
        return StepResult(
            step="constraint_gen", status="success", data=generated_stories,
            tokens_in=total_tin, tokens_out=total_tout,
            duration_s=time.time() - t0,
        )

    def _step_prolog_engine(self, constraint_ids: list[str]) -> StepResult:
        """Run the Prolog engine pipeline and generate enhanced reports.

        Steps:
        1. run_pipeline (compile .pl, load into engine)
        2. enhanced_report.py (per-constraint diagnostic reports)

        Returns report texts concatenated as the step data.
        """
        if not constraint_ids:
            return StepResult(step="prolog_engine", status="skipped")

        self._progress("prolog_engine", f"Running Prolog engine for {len(constraint_ids)} constraints...")
        t0 = time.time()

        # Step 1: Corpus update (compile JSON → .pl, load into engine)
        try:
            sys.path.insert(0, str(_REPO_ROOT / "python"))
            from run_pipeline import run_pipeline
            pipeline_result = run_pipeline(
                progress=lambda step, msg: self._progress("prolog_engine", f"[pipeline] {msg}"),
                parallel=4,
            )
            if pipeline_result.errors:
                for e in pipeline_result.errors:
                    self._progress("prolog_engine", f"pipeline warning: {e}")
        except Exception as e:
            self._progress("prolog_engine", f"Pipeline failed: {e}")
            return StepResult(
                step="prolog_engine", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Step 2: Enhanced reports
        try:
            proc = subprocess.run(
                ["python3", "python/enhanced_report.py"] + constraint_ids,
                cwd=str(_REPO_ROOT),
                capture_output=True,
                text=True,
                timeout=300,
            )
            if proc.returncode != 0:
                self._progress("prolog_engine", f"enhanced_report.py returned {proc.returncode}")
        except subprocess.TimeoutExpired:
            self._progress("prolog_engine", "Report generation timed out (300s)")
        except Exception as e:
            self._progress("prolog_engine", f"Report generation failed: {e}")
            return StepResult(
                step="prolog_engine", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

        # Collect report texts
        reports_dir = _REPO_ROOT / "outputs" / "constraint_reports"
        report_paths = []
        report_texts = []
        for cid in constraint_ids:
            rpath = reports_dir / f"{cid}_report.md"
            if rpath.exists():
                report_paths.append(rpath)
                try:
                    report_texts.append(rpath.read_text(encoding="utf-8"))
                except Exception:
                    pass

                # Also copy to run output dir
                if self.output_dir:
                    self.output_dir.mkdir(parents=True, exist_ok=True)
                    dest = self.output_dir / f"{cid}_report.md"
                    dest.write_text(rpath.read_text(encoding="utf-8"), encoding="utf-8")

        self._progress(
            "prolog_engine",
            f"Reports generated: {len(report_paths)}/{len(constraint_ids)}"
        )

        # Concatenate reports into a single text block for Stage 4
        combined_reports = ""
        for i, (cid, text) in enumerate(zip(constraint_ids, report_texts)):
            combined_reports += f"\n{'='*60}\n"
            combined_reports += f"CONSTRAINT REPORT: {cid}\n"
            combined_reports += f"{'='*60}\n"
            combined_reports += text
            combined_reports += "\n"

        return StepResult(
            step="prolog_engine", status="success",
            data={"report_paths": report_paths, "combined_text": combined_reports},
            duration_s=time.time() - t0,
        )

    # ------------------------------------------------------------------
    # Pipeline dispatch
    # ------------------------------------------------------------------

    def run(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the pipeline in the configured mode."""
        if self.mode == "narrative":
            return self._run_narrative(source_story, from_stage, source_path)
        return self._run_artifact(source_story, from_stage, source_path)

    # ------------------------------------------------------------------
    # Narrative pipeline (with constraint engine integration)
    # ------------------------------------------------------------------

    def _run_narrative(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the UKE_Narrative pipeline (stages 0-10 + optional constraint engine).

        Stages 0-4: Generation pipeline (extraction, formalization, naturalization,
                     editorial decisions, story generation).
        Stages 5-10: Editorial pipeline (discovery, strategy, structure/rewrite,
                      pacing/subtraction, review, validation).
        Review (stage 9) routes to Strategy (stage 6) or Validation (stage 10).
        Max 2 editorial cycles before exiting for human review.
        """
        result = PipelineResult(
            run_id=f"uke_{int(time.time())}",
            mode="narrative",
            source_story=source_story,
        )
        if self.output_dir:
            result.output_dir = self.output_dir

        result.original_title = self._extract_original_title(source_path)

        t0 = time.time()
        start_idx = self.all_stages.index(from_stage)

        # Load cached outputs for stages before from_stage
        for stage in self.all_stages[:start_idx]:
            cached = self._load_stage_output(stage)
            if cached:
                result.stage_outputs[stage] = cached
                self._progress(stage, f"Loaded from cache ({len(cached)} chars)")
            else:
                self._progress(stage, "WARNING: No cached output found, pipeline may fail")

        # If stage_4 isn't cached but source_story is available (e.g. workshop
        # resume where source_story.txt IS the stage 4 output), use it.
        if "stage_4" not in result.stage_outputs and source_story:
            result.stage_outputs["stage_4"] = source_story
            self._progress("stage_4", "Using source story as stage_4 output")

        # Also check for cached constraint reports
        if self.output_dir:
            cached_reports = self._load_stage_output("constraint_reports")
            if cached_reports:
                result.stage_outputs["constraint_reports"] = cached_reports

            # Also check for cached anonymized Stage 1
            cached_anon = self._load_stage_output("stage_1_anon")
            if cached_anon:
                result.stage_outputs["stage_1_anon"] = cached_anon
                self._progress("cache", "Loaded stage_1_anon from cache")
            elif "stage_0" in result.stage_outputs and "stage_1" in result.stage_outputs:
                # Old run without stage_1_anon — recompute from cached raw stages
                stage_1_anon = self._anonymize_stage_1(
                    result.stage_outputs["stage_0"],
                    result.stage_outputs["stage_1"],
                )
                result.stage_outputs["stage_1_anon"] = stage_1_anon
                self._save_stage_output("stage_1_anon", stage_1_anon, result)
                self._progress("cache", "Recomputed stage_1_anon from cached stages")

            # Restore the invariant contract (R13) — recompute from a
            # cached stage_2 if the sidecar predates contract threading.
            cached_contract = self._load_stage_output("invariant_contract")
            if cached_contract:
                result.stage_outputs["invariant_contract"] = cached_contract
                self._progress("cache", "Loaded invariant_contract from cache")
            elif "stage_2" in result.stage_outputs:
                contract = _extract_invariant_contract(result.stage_outputs["stage_2"])
                if contract:
                    result.stage_outputs["invariant_contract"] = contract
                    self._save_stage_output("invariant_contract", contract, result)
                    self._progress("cache", "Recomputed invariant_contract from cached stage_2")

            cached_c0 = self._load_stage_output("invariant_contract_stage0")
            if cached_c0:
                result.stage_outputs["invariant_contract_stage0"] = cached_c0

            # Restore the break contract — recompute from a cached
            # stage_0 if the sidecar predates break threading.
            cached_break = self._load_stage_output("break_contract")
            if cached_break:
                result.stage_outputs["break_contract"] = cached_break
                self._progress("cache", "Loaded break_contract from cache")
            elif "stage_0" in result.stage_outputs:
                break0 = _extract_stage0_break_contract(
                    result.stage_outputs["stage_0"])
                if break0:
                    break0 = self._anonymize_stage_1(
                        result.stage_outputs["stage_0"], break0)
                    result.stage_outputs["break_contract"] = break0
                    self._save_stage_output("break_contract", break0, result)
                    self._progress(
                        "cache",
                        "Recomputed break_contract from cached stage_0")

            # Also restore scope_manifest for summary output
            manifest_path = self.output_dir / "scope_manifest.json"
            if manifest_path.exists():
                try:
                    result.scope_manifest = json.loads(manifest_path.read_text())
                except Exception:
                    pass

        # ── Stage 0: Constraint Logic Extraction (Gemini) ─────────────
        if start_idx <= 0:
            step = self._run_stage_0(source_story)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_0"] = step.data
            self._save_stage_output("stage_0", step.data, result)

            if self.dry_run:
                self._progress("dry_run", "Stage 0 complete — dry-run stops here")
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result

        # ── Stage 1: Formalization (Claude) ────────────────────────────
        if start_idx <= 1:
            stage_0_out = result.stage_outputs.get("stage_0", "")
            step = self._run_stage_1(stage_0_out)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_1"] = step.data
            self._save_stage_output("stage_1", step.data, result)

        # ── Stage 2: Naturalization (Claude) ──────────────────────────
        if start_idx <= 2:
            stage_1_out = result.stage_outputs.get("stage_1", "")
            stage_0_out = result.stage_outputs.get("stage_0", "")

            # Anonymize Stage 1 to prevent source identity leaking into
            # Stage 2's setting design.  Raw Stage 1 is already saved;
            # anonymized version goes to all downstream stages.
            stage_1_anon = self._anonymize_stage_1(stage_0_out, stage_1_out)
            result.stage_outputs["stage_1_anon"] = stage_1_anon
            self._save_stage_output("stage_1_anon", stage_1_anon, result)

            # R14: Stage 0 is the only source-sighted stage; it authors the
            # Invariant Contract (Detector B is invisible in the anonymized
            # symbolic input) and the inherent_instrument flag. Carry both
            # into stage 2's input, name-scrubbed as an air-gap backstop
            # (stage0.md requires surface-free phrasing).
            contract0 = _extract_stage0_contract(stage_0_out)
            if contract0:
                contract0 = self._anonymize_stage_1(stage_0_out, contract0)
                result.stage_outputs["invariant_contract_stage0"] = contract0
                self._save_stage_output("invariant_contract_stage0", contract0, result)
            else:
                self._progress(
                    "stage_2",
                    "Stage 0 emitted no <invariant_contract> block — stage 2 "
                    "falls back to its own Step-0 detectors")

            # Break contract (rides R14): stage 0 authors the break's
            # address; anonymize as the same air-gap backstop and carry
            # to stage 2 (affordance gate) and stages 9/10 (via
            # STAGE_INPUTS key "break_contract").
            break0 = _extract_stage0_break_contract(stage_0_out)
            if break0:
                break0 = self._anonymize_stage_1(stage_0_out, break0)
                result.stage_outputs["break_contract"] = break0
                self._save_stage_output("break_contract", break0, result)
            else:
                self._progress(
                    "stage_2",
                    "Stage 0 emitted no <break_contract> block — break "
                    "presence will be UNVERIFIED downstream")

            step = self._run_stage_2(stage_1_anon, contract0, break0)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_2"] = step.data
            self._save_stage_output("stage_2", step.data, result)

            # R13: SECTION 0 (INVARIANT CONTRACT) gets a downstream consumer
            # — extract and save it for stages 9 and 10.
            contract = _extract_invariant_contract(step.data)
            if contract:
                result.stage_outputs["invariant_contract"] = contract
                self._save_stage_output("invariant_contract", contract, result)
            else:
                # OQ-216 guard (site witnessed live 2026-07-12, run
                # 112_ergodocity_kids_1783916200: Sonnet-5 stage 2 folded the
                # invariant into SECTION 1 as "Step 0" and the run continued
                # to completion with R13 threading dead — stage 9 could only
                # report "contract not available"). A full run's stage 2 MUST
                # author the extractable SECTION 0 block; fail loud, never
                # warn-and-continue.
                err = ("stage_2 output has no extractable 'SECTION 0: "
                       "INVARIANT CONTRACT' block — R13 threading would run "
                       "UNVERIFIED downstream. The section is mandatory in "
                       "stage2.md; re-run stage 2. See OQ-216.")
                self._progress("stage_2", "ERROR: " + err)
                result.steps.append(StepResult(
                    step="stage_2_section0_guard", status="error", error=err))
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result

        # ── Stage 3: Editorial Decisions (Claude) ─────────────────────
        if start_idx <= 3:
            stage_1_out = result.stage_outputs.get(
                "stage_1_anon", result.stage_outputs.get("stage_1", ""))
            stage_2_out = result.stage_outputs.get("stage_2", "")
            step = self._run_stage_3(stage_1_out, stage_2_out)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_3"] = step.data
            self._save_stage_output("stage_3", step.data, result)

        # ── Stage 4: Narrative Generation (Claude, AIR GAP ENFORCED) ──
        if start_idx <= 4:
            stage_2_out = result.stage_outputs.get("stage_2", "")
            stage_3_out = result.stage_outputs.get("stage_3", "")
            step = self._run_stage_4_narrative(
                stage_2_out, stage_3_out, ""
            )
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_4"] = step.data
            self._save_stage_output("stage_4", step.data, result)

        # ══════════════════════════════════════════════════════════════
        # Constraint Engine (after Stage 4, before editorial pipeline)
        #
        # Runs UKE_SCOPE on Stage 4 output → generates constraint story
        # JSONs → runs Prolog engine → produces diagnostic reports.
        #
        # Reports feed into Stage 5 (Discovery) and later editorial
        # stages. Evaluates the generated narrative, not the
        # formalization.
        # ══════════════════════════════════════════════════════════════
        needs_engine = (
            start_idx <= 5
            and not self.skip_engine
            and "constraint_reports" not in result.stage_outputs
        )
        if needs_engine:
            stage_4_out = result.stage_outputs.get("stage_4", "")

            # Step A: SCOPE decomposition
            step = self._step_scope(stage_4_out)
            result.steps.append(step)

            if step.status == "success" and step.data:
                manifest = step.data
                result.scope_manifest = manifest
                self._save_stage_output(
                    "scope_manifest",
                    json.dumps(manifest, indent=2),
                    result,
                )

                # Step B: Generate constraint story JSONs
                step = self._step_generate_constraint_stories(manifest)
                result.steps.append(step)

                if step.status == "success" and step.data:
                    stories = step.data
                    result.constraint_stories = stories
                    constraint_ids = [
                        s["header"]["constraint_id"] for s in stories
                    ]

                    # Step C: Prolog engine + reports
                    step = self._step_prolog_engine(constraint_ids)
                    result.steps.append(step)

                    if step.status == "success" and step.data:
                        result.constraint_report_paths = step.data.get("report_paths", [])
                        combined_text = step.data.get("combined_text", "")
                        result.stage_outputs["constraint_reports"] = combined_text
                        self._save_stage_output(
                            "constraint_reports", combined_text, result
                        )
                        self._progress(
                            "engine",
                            f"Constraint engine complete — {len(result.constraint_report_paths)} reports ready"
                        )
                    else:
                        self._progress("engine", "Prolog engine failed — continuing without reports")
                else:
                    self._progress("engine", "Constraint story generation failed — continuing without reports")
            else:
                self._progress("engine", "SCOPE failed — continuing without constraint reports")

        elif self.skip_engine:
            self._progress("engine", "Constraint engine skipped (--skip-engine)")

        # ══════════════════════════════════════════════════════════════
        # EDITORIAL PIPELINE (Stages 5-10)
        #
        # Stage 5:  Discovery
        # Stage 6:  Strategy
        # Stage 7:  Structure, Rupture, and Rewrite
        # Stage 8:  Pacing and Subtraction
        # Stage 9:  Review → routes to Stage 6 (STRATEGY) or Stage 10 (VALIDATION)
        # Stage 10: Validation
        #
        # Review routing: max 2 editorial cycles (6→9).
        # On second STRATEGY route, exit for human review.
        # ══════════════════════════════════════════════════════════════

        # ── Stage 5: Discovery ────────────────────────────────────────
        if start_idx <= 5:
            step = self._run_stage_generic("stage_5", result.stage_outputs, source_story)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_5"] = step.data
            self._save_stage_output("stage_5", step.data, result)

        # ── Stages 6-9: Editorial cycle (may repeat up to 2 times) ──
        max_editorial_cycles = 2
        editorial_start = 6 if start_idx <= 6 else start_idx

        while result.editorial_cycles < max_editorial_cycles:
            result.editorial_cycles += 1
            cycle = result.editorial_cycles
            self._progress("editorial", f"Editorial cycle {cycle}/{max_editorial_cycles}")

            # ── Stage 6: Strategy ─────────────────────────────────────
            if editorial_start <= 6:
                # On second cycle, replace discovery report with review assessment
                if cycle > 1 and "stage_9" in result.stage_outputs:
                    # Second-cycle inputs: stage_8 (latest story) + review assessment.
                    # The stage_4 slot must hold a STORY — strip the edit
                    # manifest so editorial apparatus doesn't feed the rewrite.
                    result.stage_outputs["stage_5"] = result.stage_outputs["stage_9"]
                    story_only, _ = _split_edit_manifest(result.stage_outputs["stage_8"])
                    result.stage_outputs["stage_4"] = story_only

                step = self._run_stage_generic("stage_6", result.stage_outputs, source_story)
                result.steps.append(step)
                if step.status == "error":
                    result.total_duration_s = time.time() - t0
                    self._tally(result)
                    return result
                result.stage_outputs["stage_6"] = step.data
                self._save_stage_output("stage_6", step.data, result)

            # ── Stage 7: Structure, Rupture, and Rewrite ──────────────
            if editorial_start <= 7:
                step = self._run_stage_generic("stage_7", result.stage_outputs, source_story)
                result.steps.append(step)
                if step.status == "error":
                    result.total_duration_s = time.time() - t0
                    self._tally(result)
                    return result
                result.stage_outputs["stage_7"] = step.data
                self._save_stage_output("stage_7", step.data, result)

            # ── Stage 8: Pacing and Subtraction ───────────────────────
            if editorial_start <= 8:
                step = self._run_stage_generic("stage_8", result.stage_outputs, source_story)
                result.steps.append(step)
                if step.status == "error":
                    result.total_duration_s = time.time() - t0
                    self._tally(result)
                    return result
                result.stage_outputs["stage_8"] = step.data
                # R6/R7 instrumentation: computed word counts into the
                # manifest, numeric density gate (may issue one targeted
                # revision). Must run before stage 9 reads the output.
                processed = self._post_process_stage_8(result)
                self._save_stage_output("stage_8", processed, result)

            # ── Stage 9: Review (BLIND) ───────────────────────────────
            if editorial_start <= 9:
                step = self._run_stage_generic("stage_9", result.stage_outputs, source_story)
                result.steps.append(step)
                if step.status == "error":
                    result.total_duration_s = time.time() - t0
                    self._tally(result)
                    return result
                result.stage_outputs["stage_9"] = step.data
                self._save_stage_output("stage_9", step.data, result)

                # Parse route decision
                route = self._parse_review_route(step)
                result.review_route = route
                self._progress("review", f"Review routes to: {route}")

                if route == "VALIDATION":
                    break  # proceed to stage 10
                elif route == "STRATEGY":
                    if cycle >= max_editorial_cycles:
                        self._progress(
                            "review",
                            f"STRATEGY requested but at cycle limit ({max_editorial_cycles}). "
                            f"Exiting for human review."
                        )
                        break
                    # Loop back: next iteration will run stages 6-9 again
                    editorial_start = 6
                    continue
            else:
                # Entered mid-cycle (e.g., --from-stage stage_9)
                break

            # If we didn't route to STRATEGY, exit the loop
            break

        # ── Stage 10: Validation ──────────────────────────────────────
        run_validation = (
            result.review_route == "VALIDATION"
            or start_idx >= 10  # explicit --from-stage stage_10
        )
        if run_validation and "stage_8" in result.stage_outputs:
            step = self._run_stage_generic("stage_10", result.stage_outputs, source_story)
            result.steps.append(step)
            result.stage_outputs["stage_10"] = step.data or ""
            self._save_stage_output("stage_10", step.data or "", result)

        # ── Save final story ──────────────────────────────────────────
        # The editorial story is stage_8 output (post-pacing/subtraction).
        # Fall back to stage_4 if editorial pipeline didn't run.
        final_key = "stage_8" if "stage_8" in result.stage_outputs else "stage_4"
        final_text = result.stage_outputs.get(final_key, "")
        # The published story must not ship with the editorial apparatus:
        # split off the EDIT MANIFEST (+ omega log) and keep it as a
        # run-dir sidecar.
        final_text, manifest_text = _split_edit_manifest(final_text)
        if manifest_text and self.output_dir:
            sidecar = self.output_dir / f"{final_key}_edit_manifest.md"
            sidecar.write_text(manifest_text, encoding="utf-8")
            self._progress("save", f"Edit manifest sidecar: {sidecar}")
        if final_text:
            # Compute _revN base name from source path
            rev_base = None
            if source_path and "stage_8" in result.stage_outputs:
                stem = source_path.stem
                # Find next available rev number
                rev_num = 1
                while (STORIES_DIR / f"{stem}_rev{rev_num}.md").exists():
                    rev_num += 1
                rev_base = f"{stem}_rev{rev_num}"

            story_path = self._save_final_output(
                final_text, result.original_title, STORIES_DIR,
                base_name=rev_base,
            )
            result.story_path = story_path
            self._progress("save", f"Final story saved to {story_path}")
            if self.output_dir and self.output_dir != STORIES_DIR:
                self._save_final_output(
                    final_text, result.original_title, self.output_dir,
                    base_name=rev_base,
                )
                self._progress("save", f"Copy saved to {self.output_dir}")

        result.total_duration_s = time.time() - t0
        self._tally(result)
        return result

    # ------------------------------------------------------------------
    # Artifact pipeline (generic loop with validation gates — unchanged)
    # ------------------------------------------------------------------

    def _run_artifact(
        self,
        source_story: str,
        from_stage: str = "stage_0",
        source_path: Path | None = None,
    ) -> PipelineResult:
        """Execute the UKE_Artifact pipeline (7 stages with validation gates)."""
        result = PipelineResult(
            run_id=f"uke_artifact_{int(time.time())}",
            mode="artifact",
            source_story=source_story,
        )
        if self.output_dir:
            result.output_dir = self.output_dir

        result.original_title = self._extract_original_title(source_path)

        t0 = time.time()
        start_idx = self.all_stages.index(from_stage)

        for stage in self.all_stages[:start_idx]:
            cached = self._load_stage_output(stage)
            if cached:
                result.stage_outputs[stage] = cached
                self._progress(stage, f"Loaded from cache ({len(cached)} chars)")
            else:
                self._progress(stage, "WARNING: No cached output found, pipeline may fail")

        for i, stage in enumerate(self.all_stages):
            if i < start_idx:
                continue

            if stage == "stage_0":
                step = self._run_stage_0(source_story)
            else:
                step = self._run_stage_generic(stage, result.stage_outputs, source_story)

            result.steps.append(step)

            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result

            result.stage_outputs[stage] = step.data
            self._save_stage_output(stage, step.data, result)

            if self.dry_run and stage == "stage_0":
                self._progress("dry_run", "Stage 0 complete — dry-run stops here")
                break

            if stage in self.validation_gates and not self.force_gate:
                gate_result = self._check_validation_gate(step)
                if gate_result == "halt":
                    step.status = "gate_halt"
                    self._progress(stage,
                        f"VALIDATION GATE: HALT. Review {stage}_output.md and "
                        f"--resume --from-stage {self.all_stages[i + 1] if i + 1 < len(self.all_stages) else stage}")
                    break

        gen_stage = "stage_5"
        if gen_stage in result.stage_outputs:
            final_text = result.stage_outputs[gen_stage]
            artifact_path = self._save_final_output(
                final_text, result.original_title, ARTIFACTS_DIR,
                is_code=self._is_code_output(final_text),
            )
            result.story_path = artifact_path
            self._progress("save", f"Final artifact saved to {artifact_path}")

        result.total_duration_s = time.time() - t0
        self._tally(result)
        return result

    # ------------------------------------------------------------------
    # Narrative-specific stage implementations
    # ------------------------------------------------------------------

    def _run_stage_0(self, source_story: str) -> StepResult:
        """Stage 0: Constraint Scoping & Extraction (Gemini). Shared by both modes."""
        self._progress("stage_0", "Extracting constraint logic (Gemini)...")
        t0 = time.time()

        prompt_parts = [
            "Analyze the following story to extract its constraint logic.\n\n",
            "=== SOURCE STORY ===\n",
            source_story,
            "\n\n",
        ]
        # Mode-aware logic selection: symbolic for narrative, combined for artifact
        logic_ref = self.dr_logic_symbolic if (self.mode == "narrative" and self.dr_logic_symbolic) else self.dr_logic
        logic_header = "SYMBOLIC CONSTRAINT LOGIC REFERENCE" if (self.mode == "narrative" and self.dr_logic_symbolic) else "INDEXED CONSTRAINT LOGIC REFERENCE"
        if logic_ref:
            prompt_parts.extend([
                f"=== {logic_header} ===\n",
                logic_ref,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the constraint scoping and extraction protocol in your system "
            "instructions. Extract constraint types FROM EACH CHARACTER'S INDEX. "
            "Document indexical variance, power-scaling calculations, "
            "error dynamics, and terminal attractor. "
            "Do NOT reference the story by title or use framework terminology "
            "in constraint descriptions."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_0", prompt)
            # Truncation guard: a stage-0 output cut off mid-manifest
            # (thinking tokens exhausting max_output_tokens) silently costs
            # the run its generation_sequence, deferred constraints, and
            # R14 invariant contract. Fail loud instead (witnessed
            # 2026-07-12: output ended mid-tag at </selection_reason).
            if "<constraint_manifest>" in text and "</constraint_manifest>" not in text:
                self._progress(
                    "stage_0",
                    "TRUNCATED: manifest opened but never closed "
                    f"({len(text)} chars, {tout} output tokens) — raise "
                    "MAX_TOKENS['stage_0'] or reduce the task")
                return StepResult(
                    step="stage_0", status="error",
                    error="stage 0 output truncated mid-manifest "
                          "(</constraint_manifest> missing)",
                    data=text,
                    tokens_in=tin, tokens_out=tout,
                    duration_s=time.time() - t0,
                    model_used=model, provider=provider,
                )
            self._progress("stage_0", f"Extraction complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_0", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_0", f"Failed: {e}")
            return StepResult(
                step="stage_0", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_1(self, stage_0_output: str) -> StepResult:
        """Stage 1: Formalization (Claude). Narrative mode."""
        self._progress("stage_1", "Formalizing constraint specification (Claude)...")
        t0 = time.time()

        prompt_parts = [
            "Formalize the following constraint analysis into an operational "
            "specification with index-sensitive mechanics.\n\n",
            "=== STAGE 0 CONSTRAINT ANALYSIS ===\n",
            stage_0_output,
            "\n\n",
        ]
        logic_ref = self.dr_logic_symbolic or self.dr_logic
        if logic_ref:
            prompt_parts.extend([
                "=== SYMBOLIC CONSTRAINT LOGIC REFERENCE ===\n",
                logic_ref,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the formalization protocol in your system instructions. "
            "Include: indexed classifications with χ calculations, "
            "transformation rules (IF-THEN, index-sensitive), "
            "error manifestations, institutional rationality model (PIR/BIR), "
            "terminal attractor selection, and validation checklist."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_1", prompt)
            self._progress("stage_1", f"Formalization complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_1", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_1", f"Failed: {e}")
            return StepResult(
                step="stage_1", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ── Air Gap: Anonymize Stage 1 before downstream stages ──────────

    @staticmethod
    def _anonymize_stage_1(stage_0_output: str, stage_1_output: str) -> str:
        """Strip source-identifying content from Stage 1 before passing downstream.

        Stage 0's XML contains character names in <character name="X"> tags.
        Stage 1's formalization inherits these names plus source title and
        author references.  All are gravity wells that prevent Stage 2 from
        achieving genuine setting displacement.

        Returns anonymized Stage 1 text with:
        - Character names → structural role labels (Agent_A, Agent_B, ...)
        - Source title stripped from headers and prose
        - Author name references removed
        """
        text = stage_1_output

        # ── Phase 0: Strip variable mapping table (symbolic format) ──
        # New Stage 1 format uses variable names (X₁, X₂, ...) with a
        # mapping table at the top like "X₁ ← [source character name]".
        # Strip the mapping table. Variable names are already anonymous.
        mapping_lines = []
        cleaned_lines = []
        in_mapping_block = False
        for line in text.splitlines():
            # Match lines like "X₁ ← Santiago" or "X₂ ← The Marlin"
            if re.match(r'^X[₀₁₂₃₄₅₆₇₈₉]+\s*[←⟵]\s*.+', line):
                mapping_lines.append(line)
                in_mapping_block = True
                continue
            # Also catch ASCII fallback: "X_1 <- Santiago"
            if re.match(r'^X_?\d+\s*<[-=]\s*.+', line):
                mapping_lines.append(line)
                in_mapping_block = True
                continue
            # Skip blank lines immediately after mapping block
            if in_mapping_block and line.strip() == '':
                in_mapping_block = False
                continue
            in_mapping_block = False
            cleaned_lines.append(line)

        if mapping_lines:
            text = '\n'.join(cleaned_lines)
            _log.info("Anonymization Phase 0: stripped %d variable mapping lines",
                       len(mapping_lines))

        # ── Phase 1: Extract character names from Stage 0 ──
        # Primary: XML tags  <character name="Santiago">
        names_raw = re.findall(
            r'<character\s+name="([^"]+)"', stage_0_output
        )
        # Deduplicate preserving order
        seen: set[str] = set()
        names: list[str] = []
        for n in names_raw:
            if n not in seen:
                seen.add(n)
                names.append(n)

        # Fallback: Stage 1 markdown headers  **Character: Santiago**
        if not names:
            names_raw = re.findall(
                r'\*\*Character:\s+(.+?)\*\*', stage_1_output
            )
            seen = set()
            for n in names_raw:
                if n not in seen:
                    seen.add(n)
                    names.append(n)

        # ── Phase 2: Build replacement map ──
        labels = [
            f"Agent_{chr(65 + i)}" if i < 26 else f"Agent_{i}"
            for i in range(len(names))
        ]
        name_map = dict(zip(names, labels))

        replacements: dict[str, str] = {}
        for name, label in name_map.items():
            # Possessives (curly and straight apostrophes)
            replacements[name + "\u2019s"] = label + "'s"
            replacements[name + "'s"] = label + "'s"
            replacements[name] = label

            # Handle "The X" patterns (e.g., "The Marlin" → also replace "Marlin")
            if name.startswith("The "):
                short = name[4:]
                replacements[short + "\u2019s"] = label + "'s"
                replacements[short + "'s"] = label + "'s"
                replacements[short] = label

            # Compound references: "X's Parents", "X's Family"
            for suffix in ("Parents", "Family", "Household"):
                for apos in ("'s ", "\u2019s "):
                    compound = name + apos + suffix
                    if compound in text:
                        replacements[compound] = label + "_guardians"

        # ── Phase 3: Replace (longest-first to avoid partial matches) ──
        for old, new in sorted(replacements.items(), key=lambda x: -len(x[0])):
            text = text.replace(old, new)

        # ── Phase 4: Strip source title ──
        # Extract title from Stage 1 header: "## Title - Operational Constraint Model"
        title_match = re.search(
            r'^##\s+(.+?)\s*[-\u2013\u2014]\s*Operational\s+Constraint\s+Model',
            text,
            flags=re.MULTILINE,
        )
        source_title = title_match.group(1).strip() if title_match else None

        # Strip title from header
        text = re.sub(
            r'^(##\s+).+?\s*[-\u2013\u2014]\s*(Operational\s+Constraint\s+Model)',
            r'\1\2',
            text,
            flags=re.MULTILINE,
        )

        # Strip title in italic, quoted, and plain-text contexts
        if source_title:
            text = text.replace(f"*{source_title}*", "the source text")
            text = text.replace(f'"{source_title}"', "the source text")
            text = text.replace(f"\u201c{source_title}\u201d", "the source text")
            text = text.replace(source_title, "the source text")

        # ── Phase 5: Strip author name references ──
        author_patterns = [
            r"Hemingway(?:[\u2019']s)?",
            r"Fitzgerald(?:[\u2019']s)?",
            r"Kafka(?:[\u2019']s)?",
            r"Orwell(?:[\u2019']s)?",
            r"Dostoevsky(?:[\u2019']s)?",
            r"Tolstoy(?:[\u2019']s)?",
            r"Wister(?:[\u2019']s)?",
            r"Steinbeck(?:[\u2019']s)?",
            r"Faulkner(?:[\u2019']s)?",
            r"Aesop(?:[\u2019']s)?",
        ]
        for pat in author_patterns:
            text = re.sub(pat, "the source author", text, flags=re.IGNORECASE)

        # ── Phase 6: Log what was anonymized ──
        # Labels ONLY — listing the original names here would carry the
        # source identities downstream in the very text this function
        # exists to scrub (the mapping is recoverable from the saved raw
        # stage_1_output.md when the operator needs it).
        anon_note = (
            f"\n\n<!-- ANONYMIZATION: {len(name_map)} character names replaced "
            f"with structural labels: "
            + ", ".join(name_map.values())
            + " -->\n"
        )
        text += anon_note

        return text

    def _run_stage_2(self, stage_1_output: str, stage0_contract: str = "",
                     stage0_break: str = "") -> StepResult:
        """Stage 2: Naturalization (Claude). Narrative mode.

        stage0_contract: the source-sighted Invariant Contract +
        inherent_instrument flag from Stage 0 (R14), already name-scrubbed.
        stage0_break: the source-sighted Break Contract (original_break /
        prior_status / target_prior), already name-scrubbed. Stage 2's
        only obligation on it is the affordance gate — the world must
        leave the target_prior violation executable.
        """
        self._progress("stage_2", "Designing naturalized context (Claude)...")
        t0 = time.time()

        prompt_parts = [
            "Design a narrative context for the following constraint specification.\n\n",
            "=== CONSTRAINT SPECIFICATION (Stage 1) ===\n",
            stage_1_output,
            "\n\n",
        ]
        if stage0_contract:
            prompt_parts.extend([
                "=== INVARIANT CONTRACT (Stage 0, source-sighted; surface-free) ===\n",
                "Authored by the one stage that saw the source. Use it in Step 0:\n"
                "Detector B (the missing floor) is frequently invisible in the\n"
                "symbolic spec above — this contract carries it. The\n"
                "inherent_instrument flag is a source-sighted FACT: it alone\n"
                "licenses the Scored-Snare exception in the affordance gate;\n"
                "you never decide 'it's inherent this time' yourself.\n\n",
                stage0_contract,
                "\n\n",
            ])
            # OQ-219 outcome (a): floor-primary sources get the dominance-ordering
            # clause so the floor's contract-dominance carries into the read.
            # Structural gate on the authored primary flag; INERT otherwise.
            dominance = _stage2_dominance_suffix(stage0_contract)
            if dominance:
                prompt_parts.extend([dominance, "\n\n"])
                self._progress(
                    "stage_2",
                    "floor-primary contract → dominance-ordering clause injected (OQ-219)")
        if stage0_break:
            prompt_parts.extend([
                "=== BREAK CONTRACT (Stage 0, source-sighted; surface-free) ===\n",
                "The break's ADDRESS, authored by the one stage that saw the\n"
                "source; executing the break belongs to the story stages\n"
                "downstream, never to you. Your one obligation is the\n"
                "affordance gate: reject any naturalization whose substrate\n"
                "FORECLOSES the target_prior violation. The world must leave\n"
                "the break executable; it need not execute it.\n\n",
                stage0_break,
                "\n\n",
            ])
        logic_ref = self.dr_logic_narrative or self.dr_logic
        if logic_ref:
            prompt_parts.extend([
                "=== NARRATIVE TRANSLATION REFERENCE ===\n",
                logic_ref,
                "\n\n",
            ])
        prompt_parts.append(
            "Follow the naturalization protocol in your system instructions. "
            "Create a setting where these exact constraints naturally occur. "
            "Output TWO sections:\n"
            "Section 1: CONTEXT DESCRIPTION (clean, no Omega markers, no framework terms)\n"
            "Section 2: OMEGA LOG (tracking & resolution record)\n\n"
            "DISPLACEMENT REQUIREMENT: The setting must differ from any likely "
            "source material in at least TWO of the following: occupation/profession, "
            "century/era, culture/region, governing institution. If the constraint "
            "specification describes agents in a fishing community, the setting "
            "CANNOT be a fishing community. If the agents operate in a 20th-century "
            "village, the setting CANNOT be a 20th-century village. The structural "
            "topology must be preserved; the surface must be unrecognizable. "
            "Think: same bones, completely different body.\n\n"
            "The setting must be temporally/culturally displaced from any likely source. "
            "Framework must be INVISIBLE. Power differentials must be "
            "naturalized through setting structure."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_2", prompt)
            self._progress("stage_2", f"Naturalization complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_2", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_2", f"Failed: {e}")
            return StepResult(
                step="stage_2", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_3(self, stage_1_output: str, stage_2_output: str) -> StepResult:
        """Stage 3: Editorial Decisions (Claude). Narrative mode."""
        self._progress("stage_3", "Making editorial decisions (Claude)...")
        t0 = time.time()

        prompt_parts = [
            "Make editorial decisions for the following narrative.\n\n",
            "=== CONSTRAINT SPECIFICATION (Stage 1) ===\n",
            stage_1_output,
            "\n\n",
            "=== CONTEXT DESIGN (Stage 2) ===\n",
            stage_2_output,
            "\n\n",
        ]
        prompt_parts.append(
            "Follow the operational specification protocol in your system instructions. "
            "Provide decisions on:\n"
            "1. Terminal attractor verification\n"
            "2. Voice archetype selection (with justification)\n"
            "3. Indexical revelation strategy\n"
            "4. Editorial decisions (length, POV, tense, character count, naming)\n"
            "5. Primary physical marker\n"
            "6. Story blueprint in XML format\n\n"
            "CRITICAL: Verify attractor is compatible with constraint logic "
            "and rationality model."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_3", prompt)
            self._progress("stage_3", f"Editorial decisions complete ({tin}→{tout} tokens)")
            return StepResult(
                step="stage_3", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_3", f"Failed: {e}")
            return StepResult(
                step="stage_3", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    def _run_stage_4_narrative(
        self,
        stage_2_output: str,
        stage_3_output: str,
        constraint_reports: str = "",
    ) -> StepResult:
        """Stage 4: Narrative Generation (Claude).

        ╔══════════════════════════════════════════════════════╗
        ║  AIR GAP ENFORCED: This stage receives ONLY         ║
        ║  Stages 2-3 output. The original story and Stage 0  ║
        ║  output are NEVER included in this call.            ║
        ║                                                     ║
        ║  Constraint engine now runs AFTER Stage 4, so no    ║
        ║  reports are available at generation time.           ║
        ╚══════════════════════════════════════════════════════╝
        """
        self._progress("stage_4", "Generating narrative (Claude, air gap active)...")
        t0 = time.time()

        prompt_parts = [
            "Write a complete story based on the following specifications.\n\n",
            "=== CONTEXT & WORLD (Stage 2) ===\n",
            stage_2_output,
            "\n\n",
            "=== EDITORIAL DECISIONS (Stage 3) ===\n",
            stage_3_output,
            "\n\n",
        ]

        prompt_parts.append(
            "Follow the generation protocol in your system instructions. "
            "Write the story now. Framework must be COMPLETELY INVISIBLE. "
            "Find new events in this world that embody the constraint logic — "
            "this is reimagining, not adaptation. "
            "Stay in the world completely. Trust the structure."
        )
        prompt = "".join(prompt_parts)

        try:
            text, tin, tout, model, provider = self._call("stage_4", prompt)
            self._progress("stage_4", f"Story generated ({tin}→{tout} tokens, {len(text)} chars)")
            return StepResult(
                step="stage_4", status="success", data=text,
                tokens_in=tin, tokens_out=tout,
                duration_s=time.time() - t0,
                model_used=model, provider=provider,
            )
        except Exception as e:
            self._progress("stage_4", f"Failed: {e}")
            return StepResult(
                step="stage_4", status="error", error=str(e),
                duration_s=time.time() - t0,
            )

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _tally(result: PipelineResult):
        result.total_tokens_in = sum(s.tokens_in for s in result.steps)
        result.total_tokens_out = sum(s.tokens_out for s in result.steps)


# ---------------------------------------------------------------------------
# Cost estimation
# ---------------------------------------------------------------------------

# $/MTok (input, output), matched by substring on the recorded model id;
# first match wins. Models are configurable per stage (--stage-N-model),
# so costs are computed per step from StepResult.model_used rather than
# a single hardcoded rate. Unknown/blank models fall back to Sonnet rates
# and the estimate is marked approximate.
MODEL_PRICING = [
    ("opus",             (15.00, 75.00)),
    ("sonnet",           (3.00, 15.00)),
    ("haiku",            (1.00, 5.00)),
    ("gemini-2.5-pro",   (1.25, 10.00)),
    ("gemini-2.5-flash", (0.30, 2.50)),
]
FALLBACK_PRICING = (3.00, 15.00)


def _estimate_cost(steps: list[StepResult]) -> tuple[float, bool]:
    """Return (estimated_cost_usd, all_models_priced)."""
    total = 0.0
    all_priced = True
    for s in steps:
        model = (s.model_used or "").lower()
        for key, rates in MODEL_PRICING:
            if key in model:
                cin, cout = rates
                break
        else:
            cin, cout = FALLBACK_PRICING
            if s.tokens_in or s.tokens_out:
                all_priced = False
        total += s.tokens_in / 1_000_000 * cin + s.tokens_out / 1_000_000 * cout
    return total, all_priced


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

VALID_PROVIDERS = {"google", "anthropic"}


def _parse_model_overrides(args, parser) -> dict[str, tuple[str, str]] | None:
    """Parse --stage-N-model flags into model override dict."""
    overrides = {}
    for stage in ALL_POSSIBLE_STAGES:
        attr = f"{stage}_model"
        value = getattr(args, attr, None)
        if value:
            if ":" not in value:
                parser.error(f"--{stage.replace('_', '-')}-model must be provider:model (got '{value}')")
            provider, model = value.split(":", 1)
            if provider not in VALID_PROVIDERS:
                parser.error(f"Unknown provider '{provider}'. Valid: {VALID_PROVIDERS}")
            overrides[stage] = (provider, model)
    return overrides if overrides else None


def _run_single(args, parser):
    """Run the pipeline on a single story."""
    story_path = None
    source_story = None
    workshop_mode = False

    # ------------------------------------------------------------------
    # --edit mode: clean editorial entry point
    # ------------------------------------------------------------------
    if args.edit:
        edit_path = Path(args.edit)
        if not edit_path.exists():
            parser.error(f"File not found: {edit_path}")

        source_story = edit_path.read_text(encoding="utf-8")
        story_path = edit_path

        # Default to stage_5 unless user specifies a later stage
        from_stage = args.from_stage if args.from_stage != "stage_0" else "stage_5"
        stage_idx = ALL_POSSIBLE_STAGES.index(from_stage)

        model_overrides = _parse_model_overrides(args, parser)

        # Output directory: --resume > auto-detect > create new
        if args.resume:
            output_dir = Path(args.resume)
        elif stage_idx > 5:
            # Resuming a broken editorial — look for existing dir
            slug = _title_to_filename(edit_path.stem)
            base_dir = UKE_OUTPUT_DIR
            existing = sorted(
                [p for p in base_dir.glob(f"{slug}_*") if p.is_dir()],
                key=lambda p: p.stat().st_mtime,
            )
            if existing:
                output_dir = existing[-1]
                _log.info("Resuming broken editorial in: %s", output_dir)
            else:
                parser.error(
                    f"--from-stage {from_stage} but no existing run "
                    f"directory found for '{slug}' in {base_dir}. "
                    f"Use --resume <dir> to specify one."
                )
        else:
            slug = _title_to_filename(edit_path.stem)
            output_dir = UKE_OUTPUT_DIR / f"{slug}_{int(time.time())}"

        output_dir.mkdir(parents=True, exist_ok=True)

        # Pre-cache the file as stage_4 output + source_story
        (output_dir / "stage_4_output.md").write_text(source_story, encoding="utf-8")
        (output_dir / "source_story.txt").write_text(source_story, encoding="utf-8")

        workshop_mode = True
        _log.info("Editorial mode: treating %s as stage_4 output", edit_path.name)

        orch = UKEOrchestrator(
            mode="narrative",
            models=model_overrides if model_overrides else None,
            dr_logic_path=args.dr_logic,
            output_dir=output_dir,
            skip_engine=args.skip_engine,
            dry_run=args.dry_run,
            force_gate=args.force_gate,
        )

        result = orch.run(
            source_story,
            from_stage=from_stage,
            source_path=story_path,
        )

        _print_summary(result, args.mode, workshop_mode)
        return

    # ------------------------------------------------------------------
    # Standard mode (existing --from-stage / --resume / positional logic)
    # ------------------------------------------------------------------

    # Resolve story: explicit file arg takes priority over resume dir's source_story.txt
    path = args.story or getattr(args, "story_file", None)
    if path:
        story_path = Path(path)
        if not story_path.exists():
            parser.error(f"File not found: {story_path}")
        source_story = story_path.read_text(encoding="utf-8")
    elif args.resume:
        source_file = Path(args.resume) / "source_story.txt"
        if source_file.exists():
            source_story = source_file.read_text(encoding="utf-8")
        else:
            parser.error(f"No source_story.txt in {args.resume} and no story file provided")
    else:
        parser.error("Provide a story file or --resume directory")

    model_overrides = _parse_model_overrides(args, parser)
    stage_idx = ALL_POSSIBLE_STAGES.index(args.from_stage) if args.from_stage in ALL_POSSIBLE_STAGES else 0

    # Output directory
    resuming_existing = False
    if args.resume:
        output_dir = Path(args.resume)
        resuming_existing = True
    elif args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        slug = _title_to_filename(story_path.stem) if story_path else "input"
        if args.mode == "narrative":
            base_dir = UKE_OUTPUT_DIR
        else:
            base_dir = Path(".")

        # When resuming from a later stage, find the most recent existing
        # output directory for this story instead of creating a new one.
        if args.from_stage != "stage_0":
            existing = sorted(
                [p for p in base_dir.glob(f"{slug}_*") if p.is_dir()],
                key=lambda p: p.stat().st_mtime,
            )
            if existing:
                output_dir = existing[-1]
                resuming_existing = True
                _log.info("Resuming in existing directory: %s", output_dir)
            else:
                parser.error(
                    f"--from-stage {args.from_stage} but no existing run "
                    f"directory found for '{slug}' in {base_dir}. "
                    f"Use --resume <dir> to specify one, or run from stage_0."
                )
        else:
            if args.mode == "narrative":
                output_dir = base_dir / f"{slug}_{int(time.time())}"
            else:
                output_dir = base_dir / f"uke_artifact_{slug}_{int(time.time())}"

    output_dir.mkdir(parents=True, exist_ok=True)

    # Workshop mode: story file + --from-stage stage_5+ without an existing run.
    # Treats the story as stage_4 output and runs the editorial pipeline.
    # NOT activated when resuming into an existing directory (which already
    # has real stage outputs).
    if not resuming_existing and story_path and stage_idx >= 5:
        workshop_mode = True
        _log.info("Workshop mode: treating %s as stage_4 output for editorial pipeline", story_path.name)

    if not resuming_existing and source_story:
        (output_dir / "source_story.txt").write_text(source_story, encoding="utf-8")

    # In workshop mode, pre-cache the story as stage_4 output so
    # the editorial pipeline can find it.
    if workshop_mode:
        stage_4_path = output_dir / "stage_4_output.md"
        stage_4_path.write_text(source_story, encoding="utf-8")

    skip_engine = args.skip_engine

    orch = UKEOrchestrator(
        mode=args.mode,
        models=model_overrides if model_overrides else None,
        dr_logic_path=args.dr_logic,
        output_dir=output_dir,
        skip_engine=skip_engine,
        dry_run=args.dry_run,
        force_gate=args.force_gate,
    )

    result = orch.run(
        source_story,
        from_stage=args.from_stage,
        source_path=story_path,
    )

    _print_summary(result, args.mode, workshop_mode)


def _print_summary(result, mode: str, workshop_mode: bool = False):
    """Print pipeline run summary."""
    mode_label = f"{mode.upper()} MODE"
    if workshop_mode:
        mode_label += " (editorial)"
    print(f"\n{'=' * 70}")
    print(f"PIPELINE SUMMARY — {mode_label}")
    print(f"{'=' * 70}")

    for s in result.steps:
        tok = f" ({s.tokens_in:,}→{s.tokens_out:,} tokens)" if s.tokens_in else ""
        dur = f" [{s.duration_s:.1f}s]" if s.duration_s else ""
        model = f" ({s.model_used})" if s.model_used else ""
        print(f"  {s.step:20s} {s.status:10s}{tok}{dur}{model}")
        if s.error:
            print(f"    error: {s.error[:200]}")

    print(f"\n  Total tokens: {result.total_tokens_in:,} → {result.total_tokens_out:,}")
    print(f"  Total time:   {result.total_duration_s:.1f}s")

    if result.scope_manifest:
        seq = result.scope_manifest.get("generation_sequence", [])
        print(f"\n  Constraint engine: {len(seq)} axes decomposed, "
              f"{len(result.constraint_stories)} stories generated, "
              f"{len(result.constraint_report_paths)} reports produced")

    if result.editorial_cycles > 0:
        print(f"\n  Editorial cycles: {result.editorial_cycles}")
        if result.review_route:
            print(f"  Review route: {result.review_route}")

    if result.story_path:
        print(f"\n  Output: {result.story_path}")

    cost, all_priced = _estimate_cost(result.steps)
    note = "" if all_priced else " (some steps priced at fallback Sonnet rates)"
    print(f"  Est cost: ~${cost:.2f}{note}")


def _run_batch(args, parser):
    """Run the pipeline on all stories in originals/."""
    originals = sorted(ORIGINALS_DIR.glob("*.md"))
    if not originals:
        parser.error(f"No stories found in {ORIGINALS_DIR}")
        return

    model_overrides = _parse_model_overrides(args, parser)
    mode_config = PIPELINE_MODES[args.mode]
    output_base = mode_config["output_dir"]

    def _is_completed(story_path: Path) -> bool:
        text = story_path.read_text(encoding="utf-8")
        title = _extract_title(text)
        marker = f"Original: {title}"
        for out_file in output_base.glob("*"):
            if not out_file.is_file():
                continue
            try:
                content = out_file.read_text(encoding="utf-8")
                if marker in content:
                    return True
            except Exception:
                continue
        return False

    to_process = []
    skipped = []
    for p in originals:
        if _is_completed(p):
            skipped.append(p)
        else:
            to_process.append(p)

    total = len(originals)
    skip_count = len(skipped)

    print(f"\n{'=' * 70}")
    print(f"UKE BATCH RUN — {args.mode.upper()} MODE")
    print(f"{'=' * 70}")
    print(f"  Source dir:     {ORIGINALS_DIR}")
    print(f"  Output dir:     {output_base}")
    print(f"  Stories found:  {total}")
    print(f"  Engine:         {'enabled' if not args.skip_engine else 'disabled'}")
    if skip_count:
        print(f"  Already done:   {skip_count} (skipping)")
        for p in skipped:
            print(f"    - {p.name}")
    print(f"  To process:     {len(to_process)}")
    print()

    if not to_process:
        print("Nothing to do.")
        return

    batch_results: list[tuple[Path, PipelineResult | None, str]] = []
    batch_t0 = time.time()
    batch_tokens_in = 0
    batch_tokens_out = 0

    for i, story_path in enumerate(to_process, 1):
        source_story = story_path.read_text(encoding="utf-8")
        title = _extract_title(source_story)

        print(f"\n{'─' * 70}")
        print(f"  [{i}/{len(to_process)}] {story_path.name}")
        print(f"  Title: {title}")
        print(f"{'─' * 70}")

        slug = _title_to_filename(story_path.stem)
        if args.mode == "narrative":
            output_dir = UKE_OUTPUT_DIR / f"{slug}_{int(time.time())}"
        else:
            output_dir = Path(f"uke_artifact_{slug}_{int(time.time())}")
        output_dir.mkdir(parents=True, exist_ok=True)

        story_save = output_dir / "source_story.txt"
        story_save.write_text(source_story, encoding="utf-8")

        orch = UKEOrchestrator(
            mode=args.mode,
            models=model_overrides if model_overrides else None,
            dr_logic_path=args.dr_logic,
            output_dir=output_dir,
            skip_engine=args.skip_engine,
            dry_run=args.dry_run,
            force_gate=args.force_gate,
        )

        try:
            result = orch.run(source_story, from_stage="stage_0", source_path=story_path)
            batch_tokens_in += result.total_tokens_in
            batch_tokens_out += result.total_tokens_out

            final_step = result.steps[-1] if result.steps else None
            if final_step and final_step.status == "gate_halt":
                status = "GATE_HALT"
            elif final_step and final_step.status == "error":
                status = "ERROR"
            elif result.story_path:
                status = "OK"
            else:
                status = "INCOMPLETE"

            batch_results.append((story_path, result, status))
            print(f"\n  >> {status} | {result.total_tokens_in:,}→{result.total_tokens_out:,} tokens | {result.total_duration_s:.0f}s")
            if result.story_path:
                print(f"  >> Saved: {result.story_path}")

        except Exception as e:
            _log.error("Pipeline crashed for %s: %s", story_path.name, e)
            batch_results.append((story_path, None, "CRASH"))
            print(f"\n  >> CRASH: {e}")

    batch_duration = time.time() - batch_t0

    print(f"\n\n{'=' * 70}")
    print(f"BATCH SUMMARY — {args.mode.upper()} MODE")
    print(f"{'=' * 70}")
    print()

    print(f"  {'Story':<35s} {'Status':<12s} {'Tokens In':>10s} {'Tokens Out':>10s} {'Time':>7s}")
    print(f"  {'─' * 35} {'─' * 12} {'─' * 10} {'─' * 10} {'─' * 7}")

    for story_path, result, status in batch_results:
        name = story_path.stem[:33]
        if result:
            tin = f"{result.total_tokens_in:,}"
            tout = f"{result.total_tokens_out:,}"
            dur = f"{result.total_duration_s:.0f}s"
        else:
            tin = tout = dur = "—"
        print(f"  {name:<35s} {status:<12s} {tin:>10s} {tout:>10s} {dur:>7s}")

    if skipped:
        for p in skipped:
            name = p.stem[:33]
            print(f"  {name:<35s} {'SKIPPED':<12s} {'—':>10s} {'—':>10s} {'—':>7s}")

    ok_count = sum(1 for _, _, s in batch_results if s == "OK")
    fail_count = sum(1 for _, _, s in batch_results if s != "OK")

    print(f"\n  Total:    {ok_count} OK, {fail_count} failed/halted, {skip_count} skipped")
    print(f"  Tokens:   {batch_tokens_in:,} → {batch_tokens_out:,}")
    print(f"  Duration: {batch_duration:.0f}s ({batch_duration / 60:.1f}m)")

    batch_cost = 0.0
    batch_all_priced = True
    for _, result, _ in batch_results:
        if result:
            cost, all_priced = _estimate_cost(result.steps)
            batch_cost += cost
            batch_all_priced = batch_all_priced and all_priced
    note = "" if batch_all_priced else " (some steps priced at fallback Sonnet rates)"
    print(f"  Est cost: ~${batch_cost:.2f}{note}")


def main():
    parser = argparse.ArgumentParser(
        description="UKE Pipeline — Gemini + Claude constraint translation (narrative or artifact)"
    )
    parser.add_argument(
        "story", nargs="?",
        help="Path to source story file (default: searches narrative_transform/originals/)"
    )
    parser.add_argument("--story", "-s", dest="story_file", help="Path to source story file")
    parser.add_argument(
        "--mode", "-m",
        choices=["narrative", "artifact"],
        default="narrative",
        help="Pipeline mode: narrative (story) or artifact (software)"
    )
    parser.add_argument(
        "--dr-logic",
        default=str(LOGIC_NARRATIVE_PATH),
        help=(
            f"Path to constraint logic reference. Artifact mode default: {LOGIC_NARRATIVE_PATH.name}. "
            f"Narrative mode uses split logic by default: {LOGIC_SYMBOLIC_PATH.name} (stages 0,1,5) "
            f"+ {LOGIC_NARRATIVE_TRANSLATION_PATH.name} (stage 2)."
        ),
    )
    parser.add_argument("--edit", "-e", metavar="FILE",
                        help="Editorial mode: treat FILE as stage_4 output, run constraint engine + stages 5-10")
    parser.add_argument("--output-dir", "-o", help="Directory for intermediate outputs")
    parser.add_argument("--resume", help="Resume from output directory")
    parser.add_argument(
        "--from-stage", default="stage_0",
        choices=ALL_POSSIBLE_STAGES,
        help="Resume from this stage (default: stage_0)"
    )
    parser.add_argument("--skip-engine", action="store_true",
                        help="Skip constraint engine (SCOPE → stories → Prolog reports)")
    parser.add_argument("--force-gate", action="store_true",
                        help="Do not halt on validation gate failures (artifact mode)")
    parser.add_argument("--dry-run", action="store_true", help="Run Stage 0 only")
    parser.add_argument("--batch", action="store_true",
                        help="Process all stories in originals/. Skips already-completed ones.")

    for stage in ALL_POSSIBLE_STAGES:
        parser.add_argument(
            f"--{stage.replace('_', '-')}-model",
            help=f"Override model for {stage} (format: provider:model)",
        )

    args = parser.parse_args()

    mode_config = PIPELINE_MODES[args.mode]
    if args.from_stage not in mode_config["stages"]:
        parser.error(
            f"Stage '{args.from_stage}' not valid for --mode {args.mode}. "
            f"Valid stages: {mode_config['stages']}"
        )

    if args.batch:
        _run_batch(args, parser)
    else:
        _run_single(args, parser)


if __name__ == "__main__":
    main()
