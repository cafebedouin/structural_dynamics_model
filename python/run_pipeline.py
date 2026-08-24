#!/usr/bin/env python3
"""Pipeline orchestrator — runs the full DR analysis pipeline.

Runs the full DR analysis pipeline (Prolog analyses, Python reports,
enrichment) with structured error reporting and parallel execution.

Usage:
    python3 python/run_pipeline.py          # standalone
    from run_pipeline import run_pipeline   # as library
"""

import atexit
import contextlib
import fcntl
import hashlib
import io
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable, Optional

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"
OUTPUTS_DIR = REPO_ROOT / "outputs"
TESTSETS_DIR = PROLOG_DIR / "testsets"
TESTSETS_SOTU_DIR = PROLOG_DIR / "testsets_sotu"

# Ensure sibling modules are importable
if str(Path(__file__).resolve().parent) not in sys.path:
    sys.path.insert(0, str(Path(__file__).resolve().parent))

# ---------------------------------------------------------------------------
# Manifest helpers
# ---------------------------------------------------------------------------

def _git_head_sha() -> str:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=str(REPO_ROOT),
            capture_output=True,
            text=True,
            timeout=10,
        )
        return result.stdout.strip() if result.returncode == 0 else "unknown"
    except Exception:
        return "unknown"


def _git_dirty() -> bool:
    try:
        result = subprocess.run(
            ["git", "status", "--porcelain"],
            cwd=str(REPO_ROOT),
            capture_output=True,
            text=True,
            timeout=10,
        )
        return bool(result.stdout.strip()) if result.returncode == 0 else False
    except Exception:
        return False


# Single source of truth for the corpus fingerprint (OQ-29). Kept importable as
# the private name for back-compat with in-file callers.
from corpus_hash import compute_corpus_hash as _compute_corpus_hash
from corpus_hash import assert_corpus_current
from shared.corpus_legs import LIVE_LEGS  # OQ-306: one home for the live-leg names


def check_orbits_corpus_hash(orbits_path: Path) -> None:
    """Raise RuntimeError if the orbits file is missing corpus_hash OR is stale (OQ-29).

    The corpus_hash must be stamped atomically with orbit generation (via
    regenerate_orbits.py). A missing hash means the file was produced by the old
    two-step path; a mismatched hash means it was computed against a corpus that
    has since moved. Both fail-closed via assert_corpus_current — closing the
    residual presence-only gap (an orbits file left while the corpus moved used to
    pass; now it raises). OQ-29 Thread C.
    """
    try:
        assert_corpus_current(orbits_path, TESTSETS_DIR)
    except RuntimeError as e:
        raise RuntimeError(
            f"{e} (run 'python3 python/sweeps/regenerate_orbits.py' before the pipeline.)"
        ) from e


def build_manifest(run_at: str, testsets_dir: Path = TESTSETS_DIR) -> dict:
    """Build the manifest dict for a pipeline run.

    Args:
        run_at: ISO 8601 UTC timestamp string captured at pipeline start.
        testsets_dir: corpus directory to count (default TESTSETS_DIR). When a
            NON-default corpus is classified (classify_corpus, B1), n_constraints
            is counted there and a `corpus_path` key is stamped so an output can
            never be read as the default corpus. The no-arg pipeline passes the
            default and the manifest stays byte-identical (B1 inertness control).
    """
    # NAMING DEBT, recorded at the emitter (OQ-306 R1c, ruled 2026-08-21).
    # `n_constraints` counts corpus MEMBERS — stories PLUS non-story meta-files
    # (today: *_contradictions.pl axiom files). It is NOT a story count and must
    # not be used as the denominator of a per-story rate; use `n_stories` for
    # that. The name is kept because four consumers use it as a same-run IDENTITY
    # KEY in the three-way glob/per_constraint/manifest gate, not as a semantic
    # count. Renaming is rebuild-era debt — see OQ-306 close.
    n_constraints = len(list(testsets_dir.glob("*.pl"))) if testsets_dir.exists() else 0
    n_sotu = len(list(TESTSETS_SOTU_DIR.glob("*.pl"))) if TESTSETS_SOTU_DIR.exists() else 0
    commit = _git_head_sha()
    manifest = {
        "pipeline_run_at": run_at,
        "n_constraints": n_constraints,
        "n_sotu_constraints": n_sotu,
        "code_commit": commit,
        "code_commit_short": commit[:7] if commit != "unknown" else "unknown",
        "code_dirty": _git_dirty(),
        # 2 (OQ-98): per_constraint entries carry verdict_join (joined headline
        #   verdict + raw inputs) as a sibling of diagnostic_verdict.
        # 3 (OQ-306): per_constraint entries carry member_kind; the document
        #   carries a top-level member_census; the manifest carries n_stories,
        #   n_nonstory_members, nonstory_kinds, n_unclassified. All additive —
        #   the same shape as the 1->2 bump (ce9a26ec).
        #
        # STAMPED 2 HERE ON PURPOSE. build_manifest has FOUR call sites and only
        # two route through inject_manifest (the pipeline_output path and its
        # orbit sidecar); the other two — commentary_census.json,
        # reading_reference_census.json — write a manifest directly and never
        # acquire the OQ-306 keys. Declaring 3 here made those two artifacts
        # advertise keys they do not carry, so a reader branching on
        # `schema_version >= 3` got a KeyError or a silent None. The bump to 3
        # is therefore performed by add_member_census_keys(), ATOMICALLY with
        # the keys it names — a manifest that did not go through that function
        # truthfully reports the shape it actually has. Do not re-raise this
        # literal without moving the keys with it.
        "schema_version": 2,
    }
    # Stamp corpus_path ONLY for a non-default corpus — keeps the no-arg manifest
    # byte-for-byte unchanged (only difference from a default run is the absence of
    # this key, so the inertness diff is empty modulo pipeline_run_at).
    if testsets_dir.resolve() != TESTSETS_DIR.resolve():
        manifest["corpus_path"] = testsets_dir.name
    return manifest


def _resolve_corpus_dir(corpus_path) -> Path:
    """Resolve a corpus_path the way corpus_loader:resolve_corpus_dir/2 does.

    A RELATIVE path anchors against prolog/; an absolute path passes through.
    Canonicalizing BEFORE comparing matters: a naive `corpus_path in LIVE_LEGS`
    silently downgrades an absolute-path live-leg run to continue-scope, which
    is the permissive direction — the run would ship a four-valued artifact
    where it should have refused.
    """
    p = Path(corpus_path)
    if not p.is_absolute():
        p = PROLOG_DIR / p
    try:
        return p.resolve()
    except OSError:
        return p


def _is_refusal_scope(corpus_dir: Path) -> bool:
    """Does a membership-kinding failure HALT this run? (OQ-306 R-B.)

    Hard refusal on the five live legs, where zero unknowns is the standing
    expectation and a nonzero count is a real finding about the corpus. Loud
    continue elsewhere: archived corpora carry documented filename!=subject skew
    (re-derived 2026-08-21 — original_v5 91/702, original_json/testsets
    133/1151), and refusing there would make legitimate retro-audits impossible.
    """
    return corpus_dir in {_resolve_corpus_dir(leg) for leg in LIVE_LEGS}


# R-I (ruled 2026-08-21): the refusal gets a DOCUMENTED hatch, not silence — the
# repo's comparable refusal (corpus_empty) carries `allow_empty_corpus`, and an
# undocumented hatch is the one the next person invents badly. Two conditions
# make this hatch safe to leave in the tree:
#   (1) it selects the EXISTING continue-scope path rather than adding a third
#       branch, so there is no code path unique to the override; and
#   (2) it must NAME ITS AUTHORIZER — presence alone is not enough. The value
#       is stamped into the manifest, so an overridden artifact is never
#       indistinguishable from a clean one. A hatch that produces a
#       clean-LOOKING artifact is the one that gets left on forever.
_UNCLASSIFIED_OVERRIDE_ENV = "SDM_ALLOW_UNCLASSIFIED_MEMBERS"


def add_member_census_keys(manifest: dict, document: dict, corpus_dir: Path) -> dict:
    """Add the OQ-306 additive manifest keys, and refuse per R-B scope.

    Reads `member_census` — the INDEPENDENT Prolog enumeration written by
    json_report:write_member_census/1 — and the per-entry `member_kind` values.
    The two sides come from one DEFINITION read twice, which is what makes the
    cross-boundary identities below able to fail at all.
    """
    census = document.get("member_census")
    if census is None:
        raise SystemExit(
            "OQ-306: pipeline output carries no `member_census`. Either the Prolog "
            "export is older than schema_version 3, or json_report:write_member_census/1 "
            "did not run. Refusing rather than defaulting — an absent census and an "
            "all-zero census must not read the same."
        )

    entries = document.get("per_constraint", [])
    kinds = [e.get("member_kind") for e in entries]
    if any(k is None for k in kinds):
        n_missing = sum(1 for k in kinds if k is None)
        raise SystemExit(
            f"OQ-306: {n_missing} per_constraint entr(ies) carry no `member_kind`. "
            "Refusing — a missing kind must not be defaulted to `story`."
        )

    n_stories = sum(1 for k in kinds if k == "story")
    nonstory_kinds = {}
    for k in kinds:
        # KNOWN non-story kinds only. `unknown` and `dual_family` are counted
        # ONLY in n_unclassified and never appear here or in the D3 baseline —
        # they are not a kind of thing the corpus contains, they are a failure
        # to determine what it contains.
        if k not in ("story", "unknown", "dual_family"):
            nonstory_kinds[k] = nonstory_kinds.get(k, 0) + 1
    n_nonstory = sum(nonstory_kinds.values())
    n_unclassified = sum(1 for k in kinds if k in ("unknown", "dual_family"))

    # --- Cross-boundary identities. EVERY one compares a PROLOG-derived number
    # against a PYTHON-derived one. An identity whose two sides come from the
    # same loop is a total recomputed from its own parts and cannot fail.
    if census.get("story") != n_stories:
        raise SystemExit(
            f"OQ-306 identity (i) FAILED: member_census.story={census.get('story')} "
            f"but python counted {n_stories} story entries. The Prolog census and the "
            "emitted per-entry kinds disagree.")
    for kind, n in nonstory_kinds.items():
        if census.get(kind) != n:
            raise SystemExit(
                f"OQ-306 identity (ii) FAILED for kind `{kind}`: "
                f"member_census={census.get(kind)} but python counted {n}.")
    if n_unclassified != census.get("unknown", 0) + census.get("dual_family", 0):
        raise SystemExit(
            f"OQ-306 identity (iii) FAILED: n_unclassified={n_unclassified} but "
            f"member_census unknown+dual_family="
            f"{census.get('unknown', 0) + census.get('dual_family', 0)}.")

    # NOT asserted: sum(nonstory_kinds.values()) == n_nonstory. Both sides come
    # from the same python pass — vacuous by construction. Recorded here with
    # its reason so a later reader does not "fix" the omission.

    # --- Sum invariant, checked beside the three-way gate.
    total = n_stories + n_nonstory + n_unclassified
    if total != manifest["n_constraints"]:
        raise SystemExit(
            f"OQ-306 sum invariant FAILED: n_stories({n_stories}) + "
            f"n_nonstory_members({n_nonstory}) + n_unclassified({n_unclassified}) "
            f"= {total} != n_constraints({manifest['n_constraints']}).")

    manifest["n_stories"] = n_stories
    # Keys SORTED — R1b, the operator's word.
    manifest["nonstory_kinds"] = dict(sorted(nonstory_kinds.items()))
    manifest["n_nonstory_members"] = n_nonstory
    manifest["n_unclassified"] = n_unclassified
    # The version bump rides WITH the keys, never ahead of them (see build_manifest).
    manifest["schema_version"] = 3

    if n_unclassified:
        bad = sorted(e["id"] for e in entries
                     if e.get("member_kind") in ("unknown", "dual_family"))
        detail = (
            f"{n_unclassified} corpus member(s) could not be kinded as story or as a "
            f"known non-story kind: {', '.join(bad)}"
        )
        remediation = (
            "Remediation: re-key the story to its filename, or verify filename==subject "
            "(OQ-306/OQ-20). A member satisfying BOTH fact families is `dual_family` and "
            "is a discovery, not a defaulting bug. A deliberate NEW non-story kind needs a "
            "kind-taxonomy ruling plus a census-baseline update — it is not a thing to add "
            "by letting this refusal through."
        )
        override = os.environ.get(_UNCLASSIFIED_OVERRIDE_ENV, "").strip()
        if _is_refusal_scope(corpus_dir) and not override:
            raise SystemExit(
                f"OQ-306 REFUSAL ({corpus_dir.name} is a live leg): {detail}\n{remediation}\n"
                f"If this is deliberate, set {_UNCLASSIFIED_OVERRIDE_ENV}=<who-authorized-it> "
                "— the value is recorded in the manifest, so the artifact stays "
                "distinguishable from a clean run."
            )
        # Continue-scope (or an authorized override): ship the artifact, four-valued,
        # with the ids named LOUDLY. Silence here would be the whole defect.
        scope = "live leg, refusal OVERRIDDEN" if override else "non-refusal-scope corpus"
        print(f"[pipeline] OQ-306 WARNING ({scope}): {detail}", file=sys.stderr)
        print(f"[pipeline] {remediation}", file=sys.stderr)
        if override:
            manifest["unclassified_refusal_overridden"] = {
                "authorized_by": override,
                "n_unclassified": n_unclassified,
                "ids": bad,
            }
    return manifest


def inject_manifest(src_path: Path, dst_path: Path, manifest: dict,
                    corpus_dir: Optional[Path] = None) -> None:
    """Read *src_path* (the Prolog export's raw artifact), prepend manifest as
    first key, write *dst_path* (the canonical manifest-bearing artifact).

    Single-writer convention: the swipl export writes pipeline_output.raw.json
    only; THIS function is the sole writer of pipeline_output.json. A direct
    swipl re-export therefore cannot clobber the canonical artifact's
    provenance (swipl_load_path_and_probe_gotchas.md §5).
    """
    with open(src_path, "r", encoding="utf-8") as f:
        data = json.load(f)
    # OQ-306: derive the membership keys HERE, the single wiring point both the
    # default pipeline and classify_corpus route through — one call site, so the
    # keys cannot be present on one path and absent on the other. Mutates
    # `manifest` in place, which is what classify_corpus's three-way gate then
    # reads. Raises SystemExit on a kinding failure per R-B scope.
    add_member_census_keys(manifest, data, corpus_dir or TESTSETS_DIR)
    # manifest goes first; existing keys follow unchanged
    out = {"manifest": manifest}
    out.update(data)
    with open(dst_path, "w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)


# MEASURED WALL-CLOCK, 2026-08-19, swipl 10.0.2, single-process otherwise-idle machine
# (audits/2026-08-18_classb_conversion_rollout/, two full six-leg passes). Put HERE rather
# than only in a docstring because the number a caller needs is the one the code uses:
#     testsets    n= 279   ~35 s
#     haiku       n= 960   ~288 s     <- 12 s inside run_prolog's 300 s default
#     flash       n= 960   ~530 s
#     kimi        n=1005   ~370 s
#     sonnet      n=1001   ~730 s     <- 2.4x the default
#     kernel_v1   n=1106   ~577 s
# Cost per story is NOT constant (flash and haiku are both n=960 and differ by 1.8x), so the
# ceiling is deliberately generous: the slowest measured rate, x3 headroom, floored at the old
# 300 s default so no existing caller ever gets LESS time than before.
_CLASSIFY_SECONDS_PER_STORY = 0.73          # sonnet, the slowest measured leg
_CLASSIFY_HEADROOM = 3.0


def _classify_timeout_for(glob_count: int,
                          soft_timeout: Optional[int]) -> tuple[int, Optional[int]]:
    """Size the swipl ceiling from the corpus rather than from the live leg.

    The failure this removes is not a crash — it is three full-length attempts followed by a
    refusal, which reads as "the corpus is broken" when it means "the clock was set for a
    corpus 3x smaller". run_prolog retries on TIMEOUT, so an undersized ceiling costs
    attempts x ceiling before it says anything at all.
    """
    ceiling = max(300, int(glob_count * _CLASSIFY_SECONDS_PER_STORY * _CLASSIFY_HEADROOM))
    if soft_timeout is None and ceiling > 300:
        # Keep a genuine hang caught early and retried (the OQ-301 giant_comp failure mode
        # run_prolog exists to absorb) instead of parking for the whole generous ceiling.
        soft_timeout = max(300, ceiling // 2)
    return ceiling, soft_timeout


def classify_corpus(corpus_path: str, output_name: str,
                    expected_model: Optional[str],
                    run_at: Optional[str] = None,
                    timeout: Optional[int] = None,
                    soft_timeout: Optional[int] = None) -> dict:
    """Classify a NON-default corpus into its own manifest-bearing output (B1).

    A minimal, gate-free, fresh-process driver for the twin-comparison harness. Runs
    the same _json_report swipl goal the no-arg pipeline runs, but overlays
    config:param(corpus_path) to *corpus_path* via retract-default-then-assert — a
    single deterministic clause, so corpus_loader.pl's non-deterministic param read
    cannot reach a shadowed default on backtrack. Does NOT run the full pipeline
    (that overwrites shared outputs/ + tracked validation_suite.pl) and never touches
    the canonical pipeline_output.json — it writes OUTPUTS_DIR/output_name.

    *corpus_path* is relative to prolog/ (resolved there by corpus_loader).
    *expected_model*: a model-id PREFIX every loaded story_provenance must match
    (e.g. 'claude-haiku-4-5' or 'gemini-2.5-flash'); None for a mixed-model corpus
    (the essay/control regime), which skips the single-model fingerprint.

    Refuses (raises) rather than emit a swap or a partial:
      - zero-glob: a relative-path miss is loud, not a silent empty run.
      - load completeness: corpus_constraint count == glob_count (no file failed to load).
      - provenance fingerprint (single-model corpora): every loaded story_provenance
        model starts with *expected_model* — a count cannot catch a name-identical
        haiku<->flash swap, the model can. Non-vacuous: #story_provenance == glob_count
        is also asserted, so the model-match cannot pass over an empty fact set.
      - raw freshness: the raw artifact is deleted pre-run and must reappear newer.
      - seen == classified: len(per_constraint) == glob_count == manifest.n_constraints.

    *timeout* / *soft_timeout* are forwarded to run_prolog. LEAVE THEM UNSET: the ceiling is
    SIZED FROM THE CORPUS (see _classify_timeout_for), because run_prolog's 300 s default is
    sized on the live leg and silently costs three full-length attempts and then a refusal on
    anything bigger. Pass an explicit value only to override that.

    Returns the manifest dict written into output_name.
    """
    run_at = run_at or datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    corpus_dir = (PROLOG_DIR / corpus_path).resolve()
    glob_count = len(list(corpus_dir.glob("*.pl"))) if corpus_dir.exists() else 0
    if timeout is None:
        timeout, soft_timeout = _classify_timeout_for(glob_count, soft_timeout)
    if glob_count == 0:
        raise RuntimeError(
            f"classify_corpus: zero .pl files at {corpus_dir} (relative path "
            f"{corpus_path!r} did not resolve to a populated corpus) — refusing")

    raw_path = OUTPUTS_DIR / "pipeline_output.raw.json"
    raw_path.unlink(missing_ok=True)
    delete_marker = time.time()

    # Single deterministic overlay clause (retract default first), then the standard
    # export, then an in-process gate that THROWS on mismatch (non-zero exit ->
    # run_prolog raises -> no inject_manifest; the swap/partial is refused, not echoed).
    overlay = (
        "retractall(config:param(corpus_path,_)), "
        f"assertz(config:param(corpus_path,'{corpus_path}')), "
    )
    gate = (
        f"absolute_file_name('{corpus_path}', AbsDir), "
        "format(user_error, '[classify] resolved corpus dir: ~w~n', [AbsDir]), "
        "findall(Mdl, narrative_ontology:story_provenance(_,_,_,_,_,_,Mdl,_), Mdls), "
        "length(Mdls, NProv), "
        "findall(Cc, corpus_loader:corpus_constraint(Cc), Ccs), length(Ccs, NCorp), "
        "format(user_error, '[classify] story_provenance=~w corpus_constraint=~w "
        f"glob={glob_count}~n', [NProv, NCorp]), "
        f"( NCorp =:= {glob_count} -> true ; throw(classify_load_incomplete(NCorp, {glob_count})) )"
    )
    if expected_model is not None:
        # Non-vacuous single-model fingerprint: full provenance coverage + prefix match.
        gate += (
            f", ( NProv =:= {glob_count} -> true ; throw(classify_provenance_coverage(NProv, {glob_count})) )"
            ", ( NProv > 0 -> true ; throw(classify_provenance_empty) )"
            f", ( forall(member(Mm, Mdls), atom_concat('{expected_model}', _, Mm)) "
            f"     -> true ; sort(Mdls, US), throw(classify_model_mismatch('{expected_model}', US)) )"
        )
    goal = overlay + "run_json_report, " + gate
    run_prolog(
        ["stack.pl", "covering_analysis.pl", "maxent_classifier.pl",
         "dirac_classification.pl", "diagnostic_summary.pl",
         "post_synthesis.pl", "json_report.pl"],
        goal, timeout=timeout, soft_timeout=soft_timeout,
    )

    # Raw freshness: must exist and be newer than the pre-run delete.
    if not raw_path.exists():
        raise RuntimeError("classify_corpus: pipeline_output.raw.json not produced")
    if raw_path.stat().st_mtime < delete_marker:
        raise RuntimeError("classify_corpus: raw artifact is stale (older than pre-run delete)")

    manifest = build_manifest(run_at, corpus_dir)
    out_path = OUTPUTS_DIR / output_name
    inject_manifest(raw_path, out_path, manifest, corpus_dir)

    # Seen == classified: glob == per_constraint == manifest.n_constraints. A seen file
    # that failed to classify makes glob-n and per_constraint diverge silently.
    written = json.loads(out_path.read_text(encoding="utf-8"))
    n_pc = len(written.get("per_constraint", []))
    if not (n_pc == glob_count == manifest["n_constraints"]):
        raise RuntimeError(
            f"classify_corpus: seen!=classified — per_constraint={n_pc}, glob={glob_count}, "
            f"manifest.n={manifest['n_constraints']} (a seen file failed to classify; refusing)")
    return manifest


# ---------------------------------------------------------------------------
# OQ-352: the per-leg REPORT driver — report_corpus + its transit guard
# ---------------------------------------------------------------------------
#
# WHY A GUARD AT ALL. Three report stages write a JSON co-product to a path
# HARD-CODED in the Prolog source (verified 2026-08-23 by reading the .pl files,
# not the docs — a scan of all 11 stage modules turned up four further
# `../outputs/...` strings, ALL of which are `%` comment usage-examples, so the
# transit set is exactly these three):
#
#   orbit_report.pl:110              write_orbit_json/1     -> orbit_data.json
#   abductive_report.pl:393          open(...write)         -> abductive_data.json
#   giant_component_analysis.pl:1570 write_provenance_split_json/1
#                                                           -> giant_component_analysis.raw.json
#
# They cannot be redirected without an engine change, so an overlay run WILL
# overwrite the shared artifact. _TransitGuard is the file-level transposition of
# probe_harness:with_retracted/2 — snapshot, pre-delete, run, relocate the
# product into out_dir, restore, assert byte-identical restoration.
#
# ALL GUARD STATE LIVES OUTSIDE outputs/, in <repo>/.report_corpus/ (gitignored):
# lock, journal.json, backups/. Putting the lock or journal under outputs/ would
# violate the driver's own write invariant AND falsify the selftest's isolation
# post-condition ("the whole of outputs/ byte-identical") — which is how that
# post-condition eventually acquires an --exclude flag and stops meaning anything.
#
# THE WRITE INVARIANT: outside the paths enumerated here, the driver creates
# nothing under outputs/ except outputs/legs/<leg>/. The enumerated set is:
#
#   1. TRANSIT_PATHS above — snapshotted, journalled and restored byte-identical.
#   2. outputs/legs/<leg>/ — the driver's own output tree.
#   3. outputs/prolog_children.log — APPEND-ONLY, and enumerated here rather
#      than excluded. It is written by `run_prolog`, a REUSED helper, not by
#      driver logic: one line per swipl child with pid, rc and wall-clock bounds.
#      Suppressing it for report_corpus runs would delete the OQ-301 forensic
#      channel exactly where it matters most — a sweep that executes
#      run_giant_component_analysis ~20x at n~1000 is the population that
#      reported-not-witnessed 7/100 regime would be met in, and correlating a
#      kernel-log "fatal signal 11, pid NNN" with the stage that died is what
#      this file exists for. Recorded as a DECLARED append (OQ-352 execution,
#      2026-08-23), surfaced to the operator rather than adjusted quietly: the
#      selftest's isolation post-condition found it, and it is a pre-existing
#      behaviour of a reused helper, not a new escape.

REPORT_STATE_DIR = REPO_ROOT / ".report_corpus"
TRANSIT_PATHS = (
    "orbit_data.json",
    "abductive_data.json",
    "giant_component_analysis.raw.json",
)


class ReportRefusal(RuntimeError):
    """A report_corpus refusal. Carries a machine-checkable reason `code`.

    The selftest asserts refusals BY CODE, not by message text: a fixture that
    refuses for the wrong reason must fail, not pass silently.
    """

    def __init__(self, code: str, detail: str = ""):
        self.code = code
        self.detail = detail
        super().__init__(f"{code}: {detail}" if detail else code)


def _sha256_file(p: Path) -> str:
    h = hashlib.sha256()
    with open(p, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


class _TransitGuard:
    """Protect the hard-coded transit artifacts across an overlay stage run.

    Lifecycle, and every step of it exists because the window between pre-delete
    and restore is one where a SHARED artifact does not exist on disk:

      (a) LOCK. flock-exclusive on .report_corpus/lock, held for the whole run;
          refuse LOCK_HELD rather than proceed. Serialization is a promise
          everywhere else in this codebase; here it is enforced, because a
          concurrent c-orchestrator colliding with a live delete window destroys
          a shared artifact with no record.

      (b) CRASH PATH. A context manager's `finally` does not survive SIGKILL, so
          the guard writes an ON-DISK journal BEFORE the first delete and
          installs signal/atexit handlers. A journal found at startup means
          restore-then-refuse (TRANSIT_JOURNAL_DIRTY), never silently proceed.
          Backups live in .report_corpus/backups/ so a stray outputs/ sweep
          cannot take them. If the journal names a backup that is MISSING or
          whose sha256 does not match the journalled value, restore is
          impossible: halt with TRANSIT_BACKUP_LOST, printing the artifact path,
          its journalled sha256 and the producing stage, so the operator can
          regenerate rather than discover a silently-absent shared artifact
          later. NEVER downgrade that to a warning — it is the one case where
          restore-then-refuse cannot restore.

      (c) NO-EMIT BRANCH. "Reappeared newer" is asserted only for stages that
          are SUPPOSED to emit a given transit file. A refused or skipped stage
          emits nothing; restore must still run, and the absence is recorded as
          a stage outcome (STAGE_NO_EMIT), not as a missing-artifact refusal.

      (d) --resume. A resumed run must not assume a snapshot taken by a prior
          process: resume reads the journal, and any transit path whose journal
          state is not `clean` forces the (b) recovery path before any stage runs.
    """

    def __init__(self, out_dir: Path, paths=TRANSIT_PATHS):
        self.out_dir = Path(out_dir)
        self.paths = [OUTPUTS_DIR / n for n in paths]
        self.state_dir = REPORT_STATE_DIR
        self.backups = self.state_dir / "backups"
        self.journal_path = self.state_dir / "journal.json"
        self.lock_path = self.state_dir / "lock"
        self._lock_fh = None
        self._entries: list[dict] = []
        self._armed = False

    # --- (b) recovery -----------------------------------------------------
    def recover_if_dirty(self) -> None:
        """Restore from a journal left by a killed process, then REFUSE.

        Refusing after a successful restore is deliberate: the interrupted run's
        artifacts are of unknown provenance, and the operator should decide what
        to re-run. Silently proceeding is how a half-finished sweep becomes a
        published number.
        """
        if not self.journal_path.exists():
            return
        journal = json.loads(self.journal_path.read_text(encoding="utf-8"))
        restored, lost = [], []
        for e in journal.get("entries", []):
            if e.get("state") == "clean" or not e.get("existed"):
                continue
            backup = Path(e["backup"])
            if not backup.exists():
                lost.append((e, "backup file missing"))
                continue
            actual = _sha256_file(backup)
            if actual != e["sha256"]:
                lost.append((e, f"backup sha256 {actual} != journalled {e['sha256']}"))
                continue
            Path(e["path"]).parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(backup, e["path"])
            restored.append(e["path"])
        if lost:
            lines = [
                f"  {e['path']}  sha256={e['sha256']}  stage={e.get('stage', 'unknown')}  ({why})"
                for e, why in lost
            ]
            raise ReportRefusal(
                "TRANSIT_BACKUP_LOST",
                "restore is IMPOSSIBLE for the following shared artifact(s); regenerate "
                "them (a full run_pipeline over the default leg) before trusting "
                "outputs/:\n" + "\n".join(lines),
            )
        self.journal_path.unlink(missing_ok=True)
        raise ReportRefusal(
            "TRANSIT_JOURNAL_DIRTY",
            f"a previous run was killed mid-transit; restored {len(restored)} shared "
            f"artifact(s) ({', '.join(Path(p).name for p in restored) or 'none'}) and "
            "refusing rather than proceeding on unknown state — re-run to continue",
        )

    # --- (a) lock ---------------------------------------------------------
    def acquire(self) -> None:
        self.state_dir.mkdir(parents=True, exist_ok=True)
        self.backups.mkdir(parents=True, exist_ok=True)
        self._lock_fh = open(self.lock_path, "w")
        try:
            fcntl.flock(self._lock_fh, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            self._lock_fh.close()
            self._lock_fh = None
            raise ReportRefusal(
                "LOCK_HELD",
                f"another report_corpus run holds {self.lock_path} — refusing rather "
                "than racing a live transit delete window",
            )
        self.recover_if_dirty()

    def release(self) -> None:
        if self._lock_fh is not None:
            fcntl.flock(self._lock_fh, fcntl.LOCK_UN)
            self._lock_fh.close()
            self._lock_fh = None

    # --- snapshot / restore ------------------------------------------------
    def _write_journal(self, state_note: str) -> None:
        self.journal_path.write_text(json.dumps(
            {"note": state_note, "entries": self._entries}, indent=2), encoding="utf-8")

    def snapshot(self, stage: str) -> None:
        """Snapshot + pre-delete every transit path. Journal BEFORE the first delete."""
        self._entries = []
        for p in self.paths:
            e = {"path": str(p), "stage": stage, "existed": p.exists(),
                 "state": "snapshotted", "sha256": None, "size": None,
                 "backup": str(self.backups / p.name)}
            if p.exists():
                e["sha256"] = _sha256_file(p)
                e["size"] = p.stat().st_size
                shutil.copy2(p, e["backup"])
            self._entries.append(e)
        # Journal is on disk BEFORE any delete — that ordering is the whole point.
        self._write_journal(f"in transit for stage {stage}")
        self._armed = True
        for p in self.paths:
            p.unlink(missing_ok=True)

    def collect_and_restore(self, stage: str, expect_emit: tuple) -> dict:
        """Relocate this stage's products into out_dir, restore the shared files.

        *expect_emit* names the transit files this stage is SUPPOSED to write.
        A file in expect_emit that did not appear is a stage outcome, never an
        assertion failure here — branch (c).
        """
        outcome = {"emitted": [], "no_emit": []}
        self.out_dir.mkdir(parents=True, exist_ok=True)
        for p in self.paths:
            if p.name in expect_emit:
                if p.exists():
                    shutil.copy2(p, self.out_dir / p.name)
                    outcome["emitted"].append(p.name)
                else:
                    outcome["no_emit"].append(p.name)
        # Restore runs for EVERY path regardless of what the stage emitted.
        failures = []
        for e in self._entries:
            p = Path(e["path"])
            p.unlink(missing_ok=True)
            if not e["existed"]:
                e["state"] = "clean"
                continue
            backup = Path(e["backup"])
            if not backup.exists():
                failures.append(f"{p} — backup {backup} vanished")
                continue
            shutil.copy2(backup, p)
            actual = _sha256_file(p)
            if actual != e["sha256"]:
                failures.append(f"{p} — restored sha256 {actual} != {e['sha256']}")
                continue
            e["state"] = "clean"
        if failures:
            self._write_journal("restore FAILED")
            raise ReportRefusal("TRANSIT_RESTORE_FAILED", "; ".join(failures))
        self._armed = False
        self.journal_path.unlink(missing_ok=True)
        return outcome

    def emergency_restore(self) -> None:
        """atexit/signal path. Best effort, never raises — the process is dying."""
        if not self._armed:
            return
        for e in self._entries:
            try:
                if not e["existed"]:
                    Path(e["path"]).unlink(missing_ok=True)
                    continue
                backup = Path(e["backup"])
                if backup.exists():
                    shutil.copy2(backup, e["path"])
            except Exception:
                pass


# --- the stage table --------------------------------------------------------
#
# ONE row per report stage the driver runs. This is a DISPATCH table over the
# stage functions parameterized in commit 1 — NOT a second implementation of
# them (that would be Build Discipline Pattern 2, the silent fork).
#
# Derived from the OQ-352 fact paragraph in ISSUES.md, quoted verbatim and
# pinned at 0c1191239. Nine of the twelve names it enumerates are built here;
# three are deferred (see _SCOPE_DEFERRED). Two further stages are built that
# the quotation does NOT name — `coupling` and `maxent_diag` — because they are
# overlay-capable report stages in _phase_prolog and excluding them would be an
# unexplained hole. 9 + 2 = 11.
#
# `artifacts`   — what the stage writes into out_dir (gated: exist + non-empty).
# `transit`     — hard-coded ../outputs/ co-products it is SUPPOSED to emit.
# `marker`      — an owed section that must appear in the artifact, generalizing
#                 _prolog_giant_comp's OQ-193 '## Provenance split' guard. A
#                 stage with no owed marker carries None; that is a DECLARED
#                 absence, not an oversight.
_REPORT_STAGES = {
    "fingerprint":       {"artifacts": ("fingerprint_report.md",), "transit": (), "marker": None},
    "orbit":             {"artifacts": ("orbit_report.md",), "transit": ("orbit_data.json",), "marker": None},
    "fpn":               {"artifacts": ("fpn_report.md",), "transit": (), "marker": None},
    "maxent":            {"artifacts": ("maxent_report.md",), "transit": (), "marker": None},
    "abductive":         {"artifacts": ("abductive_report.md",), "transit": ("abductive_data.json",), "marker": None},
    "covering":          {"artifacts": ("covering_analysis.md",), "transit": (), "marker": None},
    "giant_comp":        {"artifacts": ("giant_component_analysis.md",),
                          "transit": ("giant_component_analysis.raw.json",),
                          "marker": "## Provenance split (OQ-193)"},
    "coupling":          {"artifacts": ("coupling_protocol.md",), "transit": (), "marker": None},
    "maxent_diag":       {"artifacts": ("maxent_diagnostic_report.md",), "transit": (), "marker": None},
    "commentary_census": {"artifacts": ("commentary_census.json", "commentary_census.md"),
                          "transit": (), "marker": None},
    # trajectory is HAC and must never be co-resident with giant_comp (OQ-182).
    # The driver runs every stage SERIALLY, which satisfies that for free — do
    # not "optimize" this into a parallel set.
    "trajectory":        {"artifacts": ("context_profile_report.md",), "transit": (), "marker": None},
}

# The three tier-2 analyzers named in the OQ's fact paragraph are DEFERRED, and
# the deferral is a TESTED REFUSAL, not a gap: the driver refuses them by name
# and the selftest plants a two-sided fixture pair for it.
#
# The reason names the ACTUAL blocker (re-derived 2026-08-23, INVESTIGATIONS P2):
# the analyzers themselves are already path-parameterized — VarianceAnalyzer and
# PatternMiner take corpus_data_path, SufficiencyTester takes corpus_data_path +
# pipeline_data_path, all three also accept an in-memory data=. What blocks them
# is strictly upstream: _phase_python_tier2 reads outputs/corpus_data.json,
# produced by extract_corpus_data from outputs/output.txt, produced by
# _prolog_validation running prolog/validation_suite.pl — a TRACKED file that
# python_test_suite.build_suite() rewrites from testsets/ ONLY (DATASETS_DIR and
# OUTPUT_FILE are both hardcoded). Running them per-leg means rewriting a tracked
# generator, which the pre-committed stop rule forbids.
#
# Reversal is therefore cheap and its cost is recorded here rather than discovered.
_SCOPE_DEFERRED = ("variance_analysis", "pattern_mining", "index_sufficiency")


def _classify_output_name(leg_name: str) -> str:
    """The canonical per-leg classify-output filename for *leg_name*.

    MIRRORS python/audits/leg_diagnostic_table.py:57-59 EXACTLY, and that is the
    point: OQ-353's Files: line names that script as the step-0 instrument to
    "extend, do not fork", so the driver must look for the file the instrument
    reads. Resolving to a different name would make MISSING_CLASSIFY_OUTPUT fire
    on every leg forever while looking like a working gate.

      testsets              -> pipeline_output.json      (the canonical output)
      testsets_<short>      -> pipeline_output.<short>.json
      anything else         -> pipeline_output.<name>.json   (archive overlays)

    Deliberately ONE name, not a fallback chain. Several archive outputs on disk
    carry the older `pipeline_output_<leg>.json` (underscore) spelling — e.g.
    pipeline_output_original_v6.json at 3b169bb, schema 2 — and accepting both
    spellings would be Build Discipline Pattern 2, two canonical things with no
    queryable fact saying which is current. That v6 file is explicitly
    non-comparable anyway, so the fresh classify OQ-353 needs is written under
    the canonical dotted name and the legacy one is left as an artifact.
    """
    if leg_name == "testsets":
        return "pipeline_output.json"
    short = leg_name[len("testsets_"):] if leg_name.startswith("testsets_") else leg_name
    return f"pipeline_output.{short}.json"


def _leg_overlay(corpus_path: str) -> str:
    """classify_corpus's overlay prefix, verbatim. Do not switch to bare assertz."""
    return ("retractall(config:param(corpus_path,_)), "
            f"assertz(config:param(corpus_path,'{corpus_path}')), ")


def _prompt_hash_token(corpus_dir: Path) -> dict:
    """Recorded outcome (4b), never a refusal on its own.

    Three outcomes, three tokens. PROMPT_HASH_ABSENT is DISTINCT from
    PROMPT_HASH_UNIFORM on purpose: an empty prompt-hash set must never read as
    agreement (Build Discipline Pattern 5, absence satisfying the gate).
    original_v6 carries story_provenance on 0/3380 files, so v6 must COMPLETE
    with ABSENT recorded — it must not refuse.
    """
    # story_provenance/8: (id, prompt_hash, schema_hash, date, run_tag, example,
    # model, sampling). Arg 2 is the prompt hash, arg 7 the model. The fact is
    # emitted across several lines, so the pattern spans them (re.S) and the
    # model is picked out by position, not by guessing at the value's shape.
    rx = re.compile(
        r"story_provenance\(\s*[^,]+,\s*'([0-9a-f]{40})'\s*,"
        r"\s*'[^']*'\s*,\s*'[^']*'\s*,\s*'[^']*'\s*,\s*'[^']*'\s*,\s*'([^']*)'", re.S)
    rx_any = re.compile(r"story_provenance\(", re.S)
    files = sorted(corpus_dir.glob("*.pl"))
    counts: dict = {}
    models: dict = {}
    n_with = 0
    n_any = 0
    for f in files:
        text = f.read_text(encoding="utf-8", errors="replace")
        if rx_any.search(text):
            n_any += 1
        m = rx.search(text)
        if m:
            n_with += 1
            counts[m.group(1)] = counts.get(m.group(1), 0) + 1
            models[m.group(2)] = models.get(m.group(2), 0) + 1
    n_total = len(files)
    coverage = (n_with / n_total) if n_total else 0.0
    if not counts:
        token = "PROMPT_HASH_ABSENT"
    elif len(counts) == 1 and n_with == n_total:
        token = "PROMPT_HASH_UNIFORM"
    else:
        token = "PROMPT_HASH_SPLIT"
    return {"token": token, "hashes": counts, "models": models, "n_with": n_with,
            "n_provenance_any": n_any, "n_total": n_total, "coverage": coverage}


def _write_sidecar(artifact: Path, out_dir: Path, corpus_dir: Path,
                   stage: str, run_at: str) -> Path:
    """One manifest sidecar per artifact, with corpus_hash at TOP LEVEL.

    Top-level and not nested under `manifest`, because corpus_hash.
    assert_corpus_current reads `data.get("corpus_hash")` at the top level — a
    sidecar that nests it would make the guard raise "has no corpus_hash" on a
    perfectly fresh file, i.e. a stamp that cannot be checked by the checker it
    exists for. Mirrors the orbit_data.manifest.json pattern.
    """
    doc = {
        "corpus_hash": _compute_corpus_hash(corpus_dir),
        "stage": stage,
        "artifact": artifact.name,
        "artifact_sha256": _sha256_file(artifact),
        "artifact_bytes": artifact.stat().st_size,
        "manifest": build_manifest(run_at, corpus_dir),
    }
    side = out_dir / (artifact.name + ".manifest.json")
    side.write_text(json.dumps(doc, indent=2), encoding="utf-8")
    return side


def report_corpus(corpus_path: str,
                  out_dir: Optional[Path] = None,
                  stages: Optional[list] = None,
                  expected_model: Optional[str] = None,
                  declared_prompt_hash: Optional[str] = None,
                  run_at: Optional[str] = None,
                  giant_comp_timeout: Optional[int] = None,
                  require_classify_output: bool = True,
                  resume: bool = False,
                  progress=None) -> dict:
    """Run the Phase-2 REPORT stages against an overlay leg. Sibling of classify_corpus.

    classify_corpus runs only run_json_report, so the per-leg outputs carry
    per-story fields plus the top-level `diagnostic` block and nothing else. The
    report-stage tools have only ever run inside the full run_pipeline over the
    DEFAULT leg, writing shared outputs/ — so every corpus-level number they have
    published is a k=1 point estimate on one draw of one corpus. This is the
    missing driver (OQ-352); it gates OQ-353 and OQ-354.

    Per leg:
      - ONE FRESH SWIPL PROCESS PER STAGE (OQ-246: in-process leg iteration
        accumulates narrative_ontology facts, and the tell — two legs
        byte-identical — is WEAKER on same-model redraw pairs, which are expected
        to be near-identical on marginals, so the per-process rule is the only
        guard). run_prolog already spawns a fresh child per call; stages run
        strictly SERIALLY, which also satisfies OQ-182 (trajectory/giant_comp
        never co-resident) for free.
      - classify_corpus's refusals reused verbatim, plus the taxonomy below.
      - GATE THE OUTPUT, not only the input: each artifact must exist, be
        non-empty, carry its owed marker where one is declared, and carry a
        manifest sidecar whose top-level corpus_hash assert_corpus_current
        accepts — asserted same-run, before any cross-leg join.
      - LEG FINGERPRINT around the run (gotchas section 5): the static legs are
        frozen, testsets/ is NOT, and operator topic runs land stories mid-session.
      - RETRY LEDGER per stage per attempt — rc / TIMEOUT / signal / output byte
        count (the 967-byte truncation signature stays checkable), NEVER absorbed
        as "ran".

    REFUSALS (4a) halt and carry a machine-checkable code; the selftest asserts
    each two-sided BY CODE:
      ZERO_GLOB, LOAD_INCOMPLETE, PROVENANCE_COVERAGE, MODEL_MISMATCH,
      PROMPT_HASH_DECLARED_MISMATCH, ARTIFACT_ABSENT, ARTIFACT_EMPTY,
      ARTIFACT_MARKER_MISSING, SIDECAR_HASH_MISMATCH, CORPUS_DRIFT,
      MISSING_CLASSIFY_OUTPUT, SCOPE_TRACKED_GENERATOR, LOCK_HELD,
      TRANSIT_JOURNAL_DIRTY, TRANSIT_RESTORE_FAILED, TRANSIT_BACKUP_LOST.

    RECORDED OUTCOME TOKENS (4b) NEVER halt; they are asserted
    present-and-correctly-valued, not two-sided:
      PROMPT_HASH_UNIFORM / _SPLIT / _ABSENT, STAGE_OK / _TIMEOUT / _SIGNAL /
      _NO_EMIT.

    The 4a/4b split is load-bearing, not bookkeeping: v6 depends on it. v6 must
    COMPLETE with PROMPT_HASH_ABSENT recorded, not refuse — and a two-sided
    fixture is demanded of every 4a code, which ABSENT has no passing counterpart
    for.
    """
    run_at = run_at or datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    # `stages is not None`, NOT `if stages`. An explicit empty list means "run no
    # stages" (the input-refusal fixtures use it); a falsy test collapses that
    # into the None default and silently runs ALL ELEVEN — absence wearing the
    # shape of a default, which is the whole Build Discipline spine. Caught by
    # the selftest's isolation post-condition, which saw eleven swipl children
    # logged by a call that had asked for zero.
    requested = list(stages) if stages is not None else list(_REPORT_STAGES)

    # --- SCOPE_TRACKED_GENERATOR, checked before any work ------------------
    deferred = [s for s in requested if s in _SCOPE_DEFERRED]
    if deferred:
        raise ReportRefusal(
            "SCOPE_TRACKED_GENERATOR",
            f"{', '.join(deferred)} cannot run per-leg: they read "
            "outputs/corpus_data.json <- outputs/output.txt <- the TRACKED "
            "prolog/validation_suite.pl, which python_test_suite.build_suite() "
            "rewrites from testsets/ only. The analyzers are already "
            "path-parameterized; the blocker is that generator. Deferred to a "
            "later OQ rather than rewriting a tracked file mid-sweep.")
    unknown = [s for s in requested if s not in _REPORT_STAGES]
    if unknown:
        raise ReportRefusal("UNKNOWN_STAGE", ", ".join(unknown))

    corpus_dir = _resolve_corpus_dir(corpus_path)
    leg_name = corpus_dir.name
    out_dir = Path(out_dir) if out_dir else (OUTPUTS_DIR / "legs" / leg_name)

    # --- input refusals, reused verbatim from classify_corpus --------------
    glob_files = sorted(corpus_dir.glob("*.pl")) if corpus_dir.exists() else []
    glob_count = len(glob_files)
    if glob_count == 0:
        raise ReportRefusal(
            "ZERO_GLOB",
            f"zero .pl files at {corpus_dir} (relative path {corpus_path!r} did not "
            "resolve to a populated corpus)")

    # --- MISSING_CLASSIFY_OUTPUT (precondition P1) -------------------------
    # Four of OQ-353's pre-registered statistics — orbit_monotonicity,
    # corpus_wasserstein_fracture, arakelov_threshold and the step-0 diagnostic
    # block — are json_report.pl products, i.e. classify_corpus outputs, NOT
    # report-stage outputs. So a leg is only COMPLETE when a SAME-COMMIT classify
    # output sits beside the report artifacts.
    #
    # The exemption route (a CLASSIFY_EXEMPT code letting original_v6 ship a
    # partial set) was CONSIDERED AND REJECTED, recorded here so nobody weakens
    # this refusal later to make v6 pass: OQ-353 needs v6's fresh classify run
    # regardless, since the on-disk pipeline_output_original_v6.json (3b169bb,
    # schema 2) is explicitly non-comparable.
    classify_out = OUTPUTS_DIR / _classify_output_name(leg_name)
    if require_classify_output:
        head = _git_head_sha()
        if not classify_out.exists():
            raise ReportRefusal(
                "MISSING_CLASSIFY_OUTPUT",
                f"{classify_out.name} absent — three of OQ-353's statistics "
                "(orbit_monotonicity, corpus_wasserstein_fracture, "
                "arakelov_threshold) plus the diagnostic block are classify_corpus "
                "products, so a leg without one is INCOMPLETE, not merely missing a "
                "file. Run classify_corpus first.")
        cdoc = json.loads(classify_out.read_text(encoding="utf-8"))
        ccommit = (cdoc.get("manifest") or {}).get("code_commit", "unknown")
        if ccommit != head:
            raise ReportRefusal(
                "MISSING_CLASSIFY_OUTPUT",
                f"{classify_out.name} was produced at code_commit {ccommit[:12]} but "
                f"HEAD is {head[:12]} — a cross-commit pair is not a same-run leg; "
                "'HEAD yields N' is already ambiguous between engine-regime and "
                "corpus and has caused a misread (KNOWN_STATE 2026-07-02). "
                "Re-run classify_corpus at HEAD.")

    # --- 4b: prompt-hash token (recorded, never halts on its own) ----------
    ph = _prompt_hash_token(corpus_dir)

    # --- fingerprint refusals, ACTUALLY WIRED ------------------------------
    # These three were in the documented taxonomy before they were in the code:
    # `expected_model` was accepted and never read, so PROVENANCE_COVERAGE and
    # MODEL_MISMATCH named codes the driver could not produce, and LOAD_INCOMPLETE
    # had nothing behind it either. A refusal table that over-claims is the same
    # defect the table exists to catch (Pattern 1, the dangling wire), so they are
    # wired here rather than trimmed from the docstring.
    #
    # LOAD_INCOMPLETE is checked against the REQUIRED same-commit classify output
    # rather than by re-loading the corpus: classify_corpus already asserted
    # glob == per_constraint == manifest.n_constraints in Prolog at this commit,
    # so comparing its manifest to the CURRENT glob count both inherits that
    # assertion and catches the leg having moved since. Re-loading 3380 files to
    # re-derive a number another gate already certified would be minutes spent to
    # learn nothing new.
    if require_classify_output:
        n_manifest = (cdoc.get("manifest") or {}).get("n_constraints")
        if n_manifest != glob_count:
            raise ReportRefusal(
                "LOAD_INCOMPLETE",
                f"classify manifest counts {n_manifest} members but the leg globs "
                f"{glob_count} now — the corpus moved between the classify run and "
                "this one, so the report artifacts would describe a different corpus "
                "than the statistics beside them")
    if expected_model is not None:
        # Non-vacuous, exactly as classify_corpus's Prolog gate is: full coverage
        # is asserted BEFORE the prefix match, so the match cannot pass over an
        # empty fact set.
        if ph["n_with"] != glob_count:
            raise ReportRefusal(
                "PROVENANCE_COVERAGE",
                f"{ph['n_with']} of {glob_count} stories carry a parseable "
                f"story_provenance ({ph['n_provenance_any']} carry the fact at all) — "
                "a single-model fingerprint over partial coverage is vacuous")
        bad = sorted(m for m in ph["models"] if not m.startswith(expected_model))
        if bad:
            raise ReportRefusal(
                "MODEL_MISMATCH",
                f"expected every story_provenance model to start with "
                f"{expected_model!r}; found {bad}")
    if declared_prompt_hash is not None:
        # Refusal ONLY when the caller DECLARED a hash. An undeclared leg records
        # its token and proceeds.
        if ph["token"] != "PROMPT_HASH_UNIFORM" or declared_prompt_hash not in ph["hashes"]:
            raise ReportRefusal(
                "PROMPT_HASH_DECLARED_MISMATCH",
                f"caller declared {declared_prompt_hash}, leg is {ph['token']} "
                f"with {sorted(ph['hashes'])} at coverage {ph['coverage']:.4f}")

    # --- leg fingerprint BEFORE ------------------------------------------
    hash_before = _compute_corpus_hash(corpus_dir)

    overlay = _leg_overlay(corpus_path)
    guard = _TransitGuard(out_dir)
    # (d) --resume: a resumed run must NOT assume a snapshot taken by a prior
    # process. acquire() calls recover_if_dirty() UNCONDITIONALLY, so any transit
    # path whose journal state is not `clean` forces the (b) recovery path before
    # a single stage runs — on a resumed run and a fresh one alike. That is why
    # `resume` needs no branch here: making recovery conditional on the flag is
    # exactly the shape that lets a fresh run walk past a dirty journal.
    guard.acquire()
    atexit.register(guard.emergency_restore)
    _prev_handlers = {}
    for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        try:
            _prev_handlers[sig] = signal.getsignal(sig)
            signal.signal(sig, lambda s, f: (guard.emergency_restore(), sys.exit(128 + s)))
        except (ValueError, OSError):
            pass

    ledger: list = []
    stage_outcomes: dict = {}
    out_dir.mkdir(parents=True, exist_ok=True)
    try:
        for stage in requested:
            spec = _REPORT_STAGES[stage]
            fn = globals()["_prolog_" + stage]
            kwargs = {"overlay": overlay, "out_dir": out_dir}
            if stage == "commentary_census":
                kwargs["corpus_dir"] = corpus_dir
            # `is not None`, not bare truthiness — the same falsy-vs-absent shape
            # that made stages=[] run all eleven stages. An explicit 0 here should
            # fail LOUDLY (a zero ceiling), never be silently replaced by the 900 s
            # default. Found by sweeping this driver for siblings of that defect.
            if stage == "giant_comp" and giant_comp_timeout is not None:
                kwargs["timeout"] = giant_comp_timeout
                kwargs["soft_timeout"] = max(60, giant_comp_timeout // 2)
            if progress:
                progress("report_corpus", f"[{leg_name}] {stage} ...")

            guard.snapshot(stage)
            t0 = time.time()
            row = {"leg": leg_name, "stage": stage, "attempt": 1,
                   "started": datetime.now(timezone.utc).isoformat()}
            try:
                fn(**kwargs)
                row["token"] = "STAGE_OK"
                row["rc"] = 0
            except subprocess.TimeoutExpired as e:
                row["token"] = "STAGE_TIMEOUT"
                row["detail"] = f"timeout after {e.timeout}s"
            except PrologError as e:
                row["token"] = "STAGE_SIGNAL" if getattr(e, "signalled", False) else "STAGE_ERROR"
                row["detail"] = str(e)[:400]
            except Exception as e:
                row["token"] = "STAGE_ERROR"
                row["detail"] = f"{type(e).__name__}: {str(e)[:400]}"
            finally:
                row["duration_s"] = round(time.time() - t0, 3)
                # Byte counts per artifact, ALWAYS recorded — the 967-byte
                # truncation signature (OQ-301) stays checkable, and an absorbed
                # "ran" is exactly what this ledger exists to prevent.
                row["artifact_bytes"] = {
                    a: ((out_dir / a).stat().st_size if (out_dir / a).exists() else None)
                    for a in spec["artifacts"]}
                transit = guard.collect_and_restore(stage, spec["transit"])
                row["transit_emitted"] = transit["emitted"]
                if transit["no_emit"]:
                    row["transit_no_emit"] = transit["no_emit"]
                    row.setdefault("tokens", []).append("STAGE_NO_EMIT")
                ledger.append(row)
            stage_outcomes[stage] = row["token"]

        # --- OUTPUT GATE, same-run, before any cross-leg join --------------
        sidecars = []
        for stage in requested:
            spec = _REPORT_STAGES[stage]
            for a in list(spec["artifacts"]) + [t for t in spec["transit"]]:
                p = out_dir / a
                if not p.exists():
                    raise ReportRefusal("ARTIFACT_ABSENT", f"{stage} -> {a} not produced")
                if p.stat().st_size == 0:
                    raise ReportRefusal("ARTIFACT_EMPTY", f"{stage} -> {a} is zero bytes")
            if spec["marker"]:
                a = spec["artifacts"][0]
                text = (out_dir / a).read_text(encoding="utf-8", errors="replace")
                if spec["marker"] not in text:
                    raise ReportRefusal(
                        "ARTIFACT_MARKER_MISSING",
                        f"{stage} -> {a} is missing its owed section {spec['marker']!r} "
                        "(a soft-fail or catch-wrap regression on the Prolog side)")
            for a in list(spec["artifacts"]) + list(spec["transit"]):
                side = _write_sidecar(out_dir / a, out_dir, corpus_dir, stage, run_at)
                sidecars.append(side)

        # The sidecar must be accepted by the checker it is written for.
        for side in sidecars:
            try:
                assert_corpus_current(side, corpus_dir)
            except RuntimeError as e:
                raise ReportRefusal("SIDECAR_HASH_MISMATCH", f"{side.name}: {e}")

        # --- CORPUS_DRIFT: the leg must not have moved under the run -------
        hash_after = _compute_corpus_hash(corpus_dir)
        if hash_after != hash_before:
            raise ReportRefusal(
                "CORPUS_DRIFT",
                f"leg {leg_name} moved during the run ({hash_before} -> {hash_after}); "
                "a witness diff-pair is only valid over a FROZEN corpus (gotchas 5) — "
                "operator topic runs land stories mid-session")
    finally:
        for sig, h in _prev_handlers.items():
            try:
                signal.signal(sig, h)
            except (ValueError, OSError):
                pass
        try:
            atexit.unregister(guard.emergency_restore)
        except Exception:
            pass
        guard.release()

    result = {
        "leg": leg_name,
        "corpus_path": corpus_path,
        "corpus_dir": str(corpus_dir),
        "out_dir": str(out_dir),
        "run_at": run_at,
        "code_commit": _git_head_sha(),
        "n_files": glob_count,
        "corpus_hash": hash_before,
        "corpus_hash_after": hash_after,
        "prompt_hash": ph,
        "stages": requested,
        "stage_outcomes": stage_outcomes,
        "retry_ledger": ledger,
        "deferred": list(_SCOPE_DEFERRED),
        "classify_output": str(classify_out) if require_classify_output else None,
    }
    (out_dir / "report_corpus.result.json").write_text(
        json.dumps(result, indent=2), encoding="utf-8")
    return result


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class StepResult:
    name: str
    status: str = "ok"       # "ok" | "error" | "skipped"
    duration_s: float = 0.0
    error: str = ""


@dataclass
class PipelineResult:
    steps: list[StepResult] = field(default_factory=list)
    total_duration_s: float = 0.0
    errors: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------

class PrologError(RuntimeError):
    """Raised when a Prolog subprocess exits non-zero."""


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def salient_stderr(stderr: str, limit: int = 800, tail: int = 8) -> str:
    """Pick the diagnostically useful slice of SWI-Prolog stderr.

    A head-slice of this stream is all noise: SWI emits hundreds of load-time
    warnings ("Local definition ... overrides weak import", "Clauses ... are not
    together in the source-file") before the actual ERROR, so the failure that
    ended the run is thousands of characters in. A head-300 slice masked an
    OQ-60 arithmetic crash in the trajectory step behind two warning lines,
    reporting only that a warning had occurred (Build Discipline Pattern 6 —
    a channel that cannot distinguish its payload from its noise).

    Prefer the ERROR lines (the root cause is the first one); fall back to the
    tail of the stream, never the head.

    *tail* is how many trailing lines the fallback keeps. The 8-line default is
    right for an ordinary goal failure and WRONG for a death-by-signal: see
    run_prolog, which widens it (2026-08-17).
    """
    lines = stderr.splitlines()
    errors = [ln for ln in lines if ln.lstrip().startswith("ERROR")]
    picked = "\n".join(errors) if errors else "\n".join(lines[-tail:]).strip()
    if not picked:
        return "(no stderr)"
    return picked if len(picked) <= limit else picked[:limit] + " ...[truncated]"


def invalidate(*paths) -> None:
    """Delete a step's outputs BEFORE it runs, so a failure cannot leave stale ones.

    The invariant (operator ruling, 2026-08-17, generalising the OQ-193
    giant_comp raw.json rule): *a step that pre-deletes any of its outputs must
    pre-delete ALL of them.* A step writes its artifacts only on success, so on
    a crash every artifact it did NOT pre-delete survives from the previous run
    — and none of the .md surfaces carries a run stamp, so a stale one is
    indistinguishable from a fresh one at the read site (Build Discipline
    Pattern 6: absence wearing a success shape).

    Witnessed 2026-08-17: the giant_comp SIGSEGV correctly left no raw.json and
    correctly left the manifest sidecar unstamped, while
    `giant_component_analysis.md` silently kept the PREVIOUS run's content.

    Deliberate consequence: a downstream consumer of a failed step's artifact
    now fails LOUDLY (file missing) instead of silently reading last run's
    numbers. That is the intended direction — never coerce this back to a
    default-on-missing.
    """
    for p in paths:
        Path(p).unlink(missing_ok=True)


def _log_prolog_child(pid: int, rc, goal: str, t_start, t_end) -> None:
    """Append one line per swipl child: wall-clock bounds + pid + rc + goal.

    Correlating a kernel-log crash ("swipl: fatal signal 11", pid NNN) with the
    step that died previously required reconstructing timing from file mtimes
    (2026-08-17). This makes the correlation a lookup: the pid the kernel names
    appears here with the goal it was running and the window it ran in.
    Append-only, best-effort — a logging failure must never fail a pipeline step.
    """
    try:
        OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)
        line = (f"{t_start.isoformat(timespec='milliseconds')}\t"
                f"{t_end.isoformat(timespec='milliseconds')}\t"
                f"{(t_end - t_start).total_seconds():.2f}s\t"
                f"pid={pid}\trc={rc}\t{goal}\n")
        with open(OUTPUTS_DIR / "prolog_children.log", "a", encoding="utf-8") as f:
            f.write(line)
    except Exception:
        pass


def run_prolog(modules: list[str], goal: str, timeout: int = 300,
               attempts: int = 3, soft_timeout: Optional[int] = None
               ) -> subprocess.CompletedProcess:
    """Run a SWI-Prolog command, retrying TRANSIENT runtime deaths.

    Round 1 (2026-08-17, live corpus n=279, swipl 9.2.9) REPORTED `run_giant_
    component_analysis` failing **7 times in 100** serial, single-process,
    otherwise-idle invocations — 6 futex deadlocks and 1 SIGSEGV inside libswipl.
    The failure looked per-invocation and independent, so 3 attempts takes ~7% to
    ~0.03%. That is why this retry exists.

    Round 2 (2026-08-23, swipl 10.0.2, frozen 285-file corpus snapshot) found the
    regime ABSENT: **0 failures in 150 serial runs**, byte-identical output on every
    row, 95% upper bound ~2% on the per-run rate under independence
    (audits/2026-08-17_giant_comp_segv_hang/WRITEUP.md).

    **That does NOT license deleting this retry**, for four compounding reasons:
    round 1's evidence never reached the audit directory and its binary is purged,
    so the 7/100 baseline is reported-not-witnessed and unrecheckable; the
    interpreter, the corpus and this file all moved in the window, so nothing
    attributes the disappearance to a cause; 150 runs in one window on one box are
    not independent draws if the failure is state-dependent; and the regime is
    recorded absent-now, not disproven. Delete it only after re-measuring — and the
    warning below is what tells you a re-measurement is owed.

    Retry ONLY on death-by-signal and timeout. An ordinary goal failure (rc=1) or a
    Prolog ERROR is deterministic: retrying it burns minutes and hides a real defect.

    *soft_timeout* caps every attempt but the LAST, so a hang is caught early and
    retried while a genuinely slow corpus still gets the full *timeout* on its final
    attempt (giant_comp: ~1.3 s live at n=279, ~6 s at kernel_v1 n=1106, ~6 min at
    original_v6 n=3380 — a fixed tight cap would break the archive path).

    Args:
        modules: List of .pl files to load via -l flags.
        goal: Prolog goal string (without trailing halt).
        timeout: Absolute ceiling, in seconds, for the final attempt.
        attempts: Total tries (1 = the old no-retry behaviour).
        soft_timeout: Per-attempt cap for all but the final attempt.

    Returns:
        subprocess.CompletedProcess with captured stdout/stderr.

    Raises:
        PrologError: On non-zero exit code (after retries are exhausted).
        subprocess.TimeoutExpired: If the final attempt times out.
    """
    last_exc = None
    for attempt in range(1, attempts + 1):
        final = (attempt == attempts)
        cap = timeout if (final or not soft_timeout) else min(soft_timeout, timeout)
        try:
            return _run_prolog_once(modules, goal, cap, attempt, attempts)
        except subprocess.TimeoutExpired as e:
            last_exc = e
            if final:
                raise
            _warn_oq301_retry(modules, goal, attempt, attempts, "timeout")
        except PrologError as e:
            last_exc = e
            # Only a signal death is transient; a goal failure is not.
            if final or not getattr(e, "signalled", False):
                raise
            _warn_oq301_retry(modules, goal, attempt, attempts, "death by signal")
    raise last_exc  # unreachable; kept so the contract is total


def _warn_oq301_retry(modules: list[str], goal: str, attempt: int,
                      attempts: int, kind: str) -> None:
    """OQ-301 watcher: say so, loudly, when THIS retry fires on giant_comp.

    OQ-301 round 2 measured 0/150 on 2026-08-23, so a retry firing here is a
    RECURRENCE of a regime recorded absent — the re-entry condition for
    audits/2026-08-17_giant_comp_segv_hang/, whose driver, frozen detector and
    controls are staged and ready to run.

    Scope, stated because it is the channel's known blind spot: this fires on the
    IN-PIPELINE regime only. A standalone-serial regression goes unwatched.
    """
    if "giant_component_analysis" not in goal and not any(
            "giant_component_analysis" in m for m in modules):
        return
    print(
        f"[OQ-301] giant_comp retry FIRED ({kind}; attempt {attempt}/{attempts}). "
        f"Round 2 (2026-08-23) measured 0 failures in 150 serial runs on swipl "
        f"10.0.2, so this is a RECURRENCE, not the known regime. Re-entry: "
        f"audits/2026-08-17_giant_comp_segv_hang/ "
        f"(TAG=<fresh> ./round2_arms.sh A).",
        file=sys.stderr, flush=True)


def _run_prolog_once(modules: list[str], goal: str, timeout: int,
                     attempt: int, attempts: int) -> subprocess.CompletedProcess:
    """One swipl invocation. See run_prolog for the retry policy."""
    cmd = ["swipl"]
    for mod in modules:
        cmd.extend(["-l", mod])
    cmd.extend(["-g", f"{goal}, halt."])

    # Popen rather than subprocess.run so the child pid is recoverable: it is
    # the join key against the kernel log when a child dies on a signal.
    t_start = datetime.now(timezone.utc)
    proc = subprocess.Popen(
        cmd,
        cwd=str(PROLOG_DIR),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        stdout, stderr = proc.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        proc.kill()
        stdout, stderr = proc.communicate()
        _log_prolog_child(proc.pid, "TIMEOUT", f"{goal} [try {attempt}/{attempts}]",
                          t_start, datetime.now(timezone.utc))
        raise
    result = subprocess.CompletedProcess(cmd, proc.returncode, stdout, stderr)
    _log_prolog_child(proc.pid, result.returncode,
                      f"{goal} [try {attempt}/{attempts}]", t_start,
                      datetime.now(timezone.utc))
    if result.returncode != 0:
        # rc < 0 means the process was KILLED BY A SIGNAL (rc=-11 = SIGSEGV).
        # SWI-Prolog's crash handler then prints a banner plus the full Prolog
        # stack DEEPEST-FRAME-FIRST, and not one of those lines starts with
        # "ERROR" — so the ordinary 8-line tail keeps only the shallow
        # `$toplevel:run_init_goal/2 ... $c_call_prolog/0` frames and discards
        # exactly the frames that name the crashing predicate. Witnessed
        # 2026-08-17: a giant_comp SIGSEGV (pid 402110, kernel-confirmed) whose
        # captured slice was diagnostically empty — the crash site is
        # unrecoverable after the fact because stderr is not persisted.
        # Widen the window on signal deaths only; the ERROR-line path and the
        # ordinary-failure tail are untouched.
        signalled = result.returncode < 0
        detail = salient_stderr(
            result.stderr,
            limit=6000 if signalled else 800,
            tail=120 if signalled else 8,
        )
        err = PrologError(
            f"Prolog goal '{goal}' failed (rc={result.returncode}"
            f"{', KILLED BY SIGNAL' if signalled else ''}"
            f"{f', try {attempt}/{attempts}' if attempts > 1 else ''}): {detail}"
        )
        # Retry policy reads this: a signal death is transient, rc>0 is not.
        err.signalled = signalled
        raise err
    return result


# Preamble markers for reports that produce *_raw output
_PREAMBLE_MARKERS = {
    "orbit":      "<!-- ORBIT_REPORT_START -->",
    "fpn":        "<!-- FPN_REPORT_START -->",
    "maxent":     "<!-- MAXENT_REPORT_START -->",
    "abductive":  "<!-- ABDUCTIVE_REPORT_START -->",
    "trajectory": "<!-- TRAJECTORY_REPORT_START -->",
    "commentary_census": "<!-- COMMENTARY_CENSUS_START -->",
}


def strip_preamble(text: str, marker: str) -> str:
    """Extract content after *marker* line (mirrors sed -n + tail -n +2)."""
    idx = text.find(marker)
    if idx == -1:
        return text
    # Skip marker line itself
    newline = text.find("\n", idx)
    if newline == -1:
        return ""
    return text[newline + 1:]


def _run_step(name: str, fn, progress) -> StepResult:
    """Run *fn* inside a try/except and return a StepResult."""
    t0 = time.time()
    try:
        fn()
        return StepResult(name=name, status="ok", duration_s=time.time() - t0)
    except Exception as e:
        import traceback
        tb = traceback.format_exc()
        msg = f"{name}: {type(e).__name__}: {e}"
        if progress:
            progress("pipeline", f"[WARN] {msg} (non-critical, continuing)")
            progress("pipeline", tb[:500])
        return StepResult(name=name, status="error", duration_s=time.time() - t0, error=f"{msg}\n{tb[:500]}")


def _run_parallel(tasks: list[tuple[str, callable]], progress, parallel: int) -> list[StepResult]:
    """Run [(name, fn), ...] in a ThreadPoolExecutor, return StepResults."""
    results = []
    with ThreadPoolExecutor(max_workers=parallel) as pool:
        futures = {
            pool.submit(_run_step, name, fn, progress): name
            for name, fn in tasks
        }
        for future in as_completed(futures):
            results.append(future.result())
    return results


# ---------------------------------------------------------------------------
# Phase 1: PREP
# ---------------------------------------------------------------------------

def _phase_prep(progress):
    """Generate domain registry and test suite."""
    import python_test_suite

    if progress:
        progress("pipeline", "[PREP] Preparing test suite...")

    # domain_registry.pl regeneration RETIRED (OQ-96 close, 2026-06-11): the
    # consuming module was deleted 2026-02-18 and the last dangling reference
    # was removed 2026-06-10 — regenerating it every run was a
    # producer-without-consumer (Pattern 1).
    python_test_suite.build_suite()

    if progress:
        progress("pipeline", "[PREP] Done.")


def _regenerate_orbits(progress):
    """Regenerate product_site_orbits.json + stamp corpus_hash, atomically, at the
    front of the pipeline so the orbits the manifest_inject check (and the Tier-1
    orbit consumers) read are always fresh against THIS run's corpus.

    Previously a manual pre-pipeline step (OQ-29 left regeneration to
    `regenerate_orbits.py` and made manifest_inject fail-closed on staleness).
    Operator ruling 2026-06-20: run it WITH the pipeline — regeneration is cheap
    (~1.3s on the live corpus) and the friction of the manual step was not worth
    the stale-orbits error it caused. The manifest_inject corpus_hash check stays
    as the fail-closed backstop (catches a regen that failed or was skipped).

    Runs as a subprocess (the script sys.exit()s on failure, which _run_step does
    not catch); a non-zero exit raises RuntimeError so it is recorded as a step
    error and the manifest_inject guard still fires downstream.

    NOTE: regenerate_orbits.py always exports the DEFAULT `testsets/` corpus, which
    is exactly what manifest_inject checks (TESTSETS_DIR, unconditionally). A
    non-default classify_corpus run is unchanged by this step (neither regenerates
    against the non-default corpus — pre-existing, not made worse here).
    """
    if progress:
        progress("pipeline", "[ORBITS] Regenerating product_site_orbits.json + corpus_hash...")
    script = REPO_ROOT / "python" / "sweeps" / "regenerate_orbits.py"
    result = subprocess.run(
        [sys.executable, str(script)],
        cwd=str(REPO_ROOT),
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"regenerate_orbits.py exited {result.returncode}: "
            f"{(result.stderr or result.stdout).strip()[-400:]}"
        )


# ---------------------------------------------------------------------------
# Phase 2: PROLOG ANALYSES (parallel)
# ---------------------------------------------------------------------------
#
# STAGE PARAMETERIZATION (OQ-352, 2026-08-23). Each report stage below takes
# `overlay` / `out_dir` (and where it is needed, `corpus_dir` and timeouts) so
# report_corpus can run it against a NON-default leg. Every parameter defaults
# to the literal the stage used before, so the default pipeline path is INERT BY
# CONSTRUCTION, not by test:
#
#   overlay=""      -> `"" + "run_x"` is the same goal STRING as before;
#   out_dir=OUTPUTS_DIR / corpus_dir=TESTSETS_DIR -> the same paths as before;
#   timeouts        -> today's literals.
#
# `_phase_prolog`'s bare callables therefore keep working unchanged.
#
# DELIBERATELY NOT A STAGE-TABLE REFACTOR: a second stage implementation inside
# report_corpus would be Build Discipline Pattern 2 (the silent fork), and a
# table rewrite has a larger blast radius than the inertness argument can carry.
#
# The overlay string is `classify_corpus`'s verbatim (retractall-then-assertz):
# ONE deterministic clause, so corpus_loader.pl's non-deterministic param read
# cannot reach a shadowed default on backtrack. Do not switch it to a bare
# assertz — config.pl:489 defines the default first and the loader takes the
# first solution, so an appended clause is SILENTLY IGNORED (witnessed
# 2026-06-13: a twin overlay loaded 44 instead of 960 with no error).

def _prolog_validation():
    """Run validation_suite → output.txt."""
    OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)
    header = f"Initializing Validation Suite - {time.strftime('%c')}\n"
    header += "------------------------------------------\n"

    try:
        result = subprocess.run(
            ["swipl", "-g", "[validation_suite], run_dynamic_suite, halt."],
            cwd=str(PROLOG_DIR),
            capture_output=True,
            text=True,
            timeout=300,
        )
        body = result.stdout
    except Exception as e:
        body = f"[WARN] validation_suite failed: {e}\n"

    footer = "------------------------------------------\n"
    footer += f"Test suite completed at: {time.strftime('%c')}\n"

    (OUTPUTS_DIR / "output.txt").write_text(header + body + footer, encoding="utf-8")


def _prolog_fingerprint(overlay: str = "", out_dir: Path = OUTPUTS_DIR,
                        timeout: int = 300):
    """Run fingerprint_report → fingerprint_report.md.

    OVERLAY BRANCH, and it is not cosmetic. fingerprint_report.pl runs via
    `:- initialization(run_fingerprint_report)` (line 23), so the report fires
    while the file LOADS — a goal-prefix overlay would have to execute before
    `[fingerprint_report]`, and at that point `config` is not loaded, so
    `retractall(config:param(...))` raises. The overlay path therefore loads
    config.pl first with -l. The default path keeps the EXACT argv it had
    before (no -l), so inertness here is a construction, not a test result.
    """
    if overlay:
        cmd = ["swipl", "-l", "config.pl", "-g",
               overlay + "[fingerprint_report], halt."]
    else:
        cmd = ["swipl", "-g", "[fingerprint_report], halt."]
    result = subprocess.run(
        cmd,
        cwd=str(PROLOG_DIR),
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    (out_dir / "fingerprint_report.md").write_text(result.stdout, encoding="utf-8")


def _prolog_orbit(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run orbit_report → orbit_report.md + orbit_data.json (sidecar)."""
    invalidate(out_dir / "orbit_report.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl", "orbit_report.pl"],
        overlay + "run_orbit_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["orbit"])
        (out_dir / "orbit_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (out_dir / "orbit_report.md").write_text("", encoding="utf-8")


def _prolog_fpn(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run fpn_report → fpn_report.md."""
    invalidate(out_dir / "fpn_report.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "fpn_report.pl"],
        overlay + "run_fpn_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["fpn"])
        (out_dir / "fpn_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (out_dir / "fpn_report.md").write_text("", encoding="utf-8")


def _prolog_maxent(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run maxent_report → maxent_report.md."""
    invalidate(out_dir / "maxent_report.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl",
         "maxent_classifier.pl", "maxent_report.pl"],
        overlay + "run_maxent_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["maxent"])
        (out_dir / "maxent_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (out_dir / "maxent_report.md").write_text("", encoding="utf-8")


def _prolog_abductive(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run abductive_report → abductive_report.md + abductive_data.json (sidecar)."""
    invalidate(out_dir / "abductive_report.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl",
         "maxent_classifier.pl", "abductive_engine.pl", "abductive_report.pl"],
        overlay + "run_abductive_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["abductive"])
        (out_dir / "abductive_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (out_dir / "abductive_report.md").write_text("", encoding="utf-8")


def _prolog_trajectory(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run context_profile_report (conditional) → context_profile_report.md."""
    invalidate(out_dir / "context_profile_report.md")
    # Check if trajectory is enabled
    try:
        check = subprocess.run(
            ["swipl", "-g",
             "use_module(config), (config:param(trajectory_enabled, 1) -> write(1) ; write(0)), halt."],
            cwd=str(PROLOG_DIR),
            capture_output=True,
            text=True,
            timeout=30,
        )
        enabled = check.stdout.strip() == "1"
    except Exception:
        enabled = False

    if not enabled:
        (out_dir / "context_profile_report.md").write_text("", encoding="utf-8")
        return

    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "dirac_classification.pl",
         "maxent_classifier.pl", "context_profile_mining.pl", "context_profile_report.pl"],
        overlay + "run_trajectory_report",
    )
    raw = result.stdout
    if raw.strip():
        cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["trajectory"])
        (out_dir / "context_profile_report.md").write_text(cleaned, encoding="utf-8")
    else:
        (out_dir / "context_profile_report.md").write_text("", encoding="utf-8")


def _prolog_covering(overlay: str = "", out_dir: Path = OUTPUTS_DIR,
                     timeout: int = 900, soft_timeout: Optional[int] = None):
    """Run covering_analysis → covering_analysis.md."""
    invalidate(out_dir / "covering_analysis.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl"],
        overlay + "run_covering_analysis",
        timeout=timeout,
        soft_timeout=soft_timeout,
    )
    (out_dir / "covering_analysis.md").write_text(result.stdout, encoding="utf-8")


def _prolog_giant_comp(overlay: str = "", out_dir: Path = OUTPUTS_DIR,
                       timeout: int = 900, soft_timeout: Optional[int] = 60):
    """Run giant_component_analysis → giant_component_analysis.md (+ raw.json co-product).

    OQ-193: pre-delete the raw.json co-product FIRST (before anything that can
    raise) so a failed/partial run can never leave a stale sidecar that a later
    manifest stamp would falsely certify as this run's (Path.unlink(missing_ok)
    — a clean checkout has no raw.json, so a bare os.remove would crash every
    first run). Then assert the owed provenance-split section is present in stdout
    before writing the md — a standing marker guard so the standalone surface can
    never silently drop its owed section while the step still reads ok (covers a
    future catch-wrapping / soft-fail regression on the Prolog side).
    """
    # NOTE the raw.json path: giant_component_analysis.pl writes it to a
    # HARD-CODED ../outputs/ path and cannot be redirected without an engine
    # change, so under an overlay it is a TRANSIT file handled by
    # report_corpus._transit_guard, not by out_dir. Pre-deleting the shared
    # path here is still correct (OQ-193) — the guard has already snapshotted
    # and pre-deleted it, and restores it afterwards.
    invalidate(OUTPUTS_DIR / "giant_component_analysis.raw.json",
               out_dir / "giant_component_analysis.md")
    result = run_prolog(
        ["stack.pl", "giant_component_analysis.pl"],
        overlay + "run_giant_component_analysis",
        # Defaults are today's literals: absolute ceiling sized for original_v6
        # (n=3380, ~6 min); soft cap catches a hang early (live runs in ~1.3 s).
        # report_corpus sizes both FROM A MEASURED PROBE rather than trusting
        # that "~6 min" comment, which encodes an unwritten superlinear guess.
        timeout=timeout,
        soft_timeout=soft_timeout,
    )
    if "## Provenance split (OQ-193)" not in result.stdout:
        raise RuntimeError(
            "giant_component_analysis stdout is missing the '## Provenance split "
            "(OQ-193)' section — the owed report surface was dropped (a soft-fail "
            "or catch-wrap regression on the Prolog side); refusing to write a "
            "partial md.")
    (out_dir / "giant_component_analysis.md").write_text(result.stdout, encoding="utf-8")


def _prolog_coupling(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run coupling_protocol → coupling_protocol.md."""
    invalidate(out_dir / "coupling_protocol.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "inferred_coupling_protocol.pl"],
        overlay + "run_coupling_protocol",
    )
    (out_dir / "coupling_protocol.md").write_text(result.stdout, encoding="utf-8")


def _prolog_maxent_diag(overlay: str = "", out_dir: Path = OUTPUTS_DIR):
    """Run maxent_diagnostic → maxent_diagnostic_report.md."""
    invalidate(out_dir / "maxent_diagnostic_report.md")
    result = run_prolog(
        ["stack.pl", "covering_analysis.pl", "maxent_classifier.pl",
         "dirac_classification.pl", "maxent_diagnostic.pl"],
        overlay + "run_maxent_diagnostic",
    )
    (out_dir / "maxent_diagnostic_report.md").write_text(result.stdout, encoding="utf-8")


def _prolog_commentary_census(overlay: str = "", out_dir: Path = OUTPUTS_DIR,
                              corpus_dir: Path = TESTSETS_DIR):
    """Run commentary_census → commentary_census.json + commentary_census.md (OQ-134/OQ-121).

    Corpus-wide commentary-grade census (q6_crosscheck + extraction_reading).
    Prolog computes the per-source bucket histograms (single source of truth);
    this transports the CENSUS* machine lines into a manifest-bearing JSON + the
    human table. COMMENTARY-GRADE: reads engine predicates only, never feeds
    classification — nothing touched here is on the dr_type path.

    Three bucket kinds (OQ-121): out-of-domain (reading doesn't apply — excluded
    from the coverage denominator), absence (in-domain, didn't-look — subtracted
    from the numerator), measured. Three distinct quantities:
      coverage   = (n_in_domain − Σ absence) / n_in_domain   [domain-relative]
      prevalence = fired / n_in_domain                       [a different number]
    Self-checking invariants (fail-loud, never under-report):
      - Σ buckets == n_corpus per source (a dropped/double-counted constraint
        breaks it — structurally enforces commentary_cell's one-bucket contract).
      - n_corpus > 0 (a 0==0 sum would pass vacuously if the corpus didn't load).
      - n_in_domain == n_corpus − Σ out-of-domain (the domain split is consistent).
    Coverage is computed ONLY for sources flagged CENSUS_COVERAGE decidable;
    undecided sources ship coverage null, never a default 1.0 (Pattern 6).
    """
    invalidate(out_dir / "commentary_census.json",
               out_dir / "commentary_census.md")
    result = run_prolog(
        ["stack.pl", "commentary_census.pl"],
        overlay + "run_commentary_census",
    )
    raw = result.stdout

    # Parse the machine block (by line prefix — robust to interleaved load noise).
    sources: dict = {}

    def _src(name):
        return sources.setdefault(
            name, {"n_corpus": None, "n_in_domain": None, "buckets": {},
                   "absence_buckets": [], "out_of_domain_buckets": [],
                   "prevalence_bucket": None, "coverage_decidable": False})

    for line in raw.splitlines():
        parts = line.split()
        if not parts:
            continue
        if parts[0] == "CENSUS_META" and len(parts) == 4 and parts[2] == "n_corpus":
            _src(parts[1])["n_corpus"] = int(parts[3])
        elif parts[0] == "CENSUS_META" and len(parts) == 4 and parts[2] == "n_in_domain":
            _src(parts[1])["n_in_domain"] = int(parts[3])
        elif parts[0] == "CENSUS" and len(parts) == 4:
            _src(parts[1])["buckets"][parts[2]] = int(parts[3])
        elif parts[0] == "CENSUS_ABSENCE" and len(parts) == 3:
            _src(parts[1])["absence_buckets"].append(parts[2])
        elif parts[0] == "CENSUS_OOD" and len(parts) == 3:
            _src(parts[1])["out_of_domain_buckets"].append(parts[2])
        elif parts[0] == "CENSUS_PREVALENCE" and len(parts) == 4:
            _src(parts[1])["prevalence_bucket"] = parts[2]
        elif parts[0] == "CENSUS_COVERAGE" and len(parts) == 3 and parts[2] == "decidable":
            _src(parts[1])["coverage_decidable"] = True

    if not sources:
        raise PrologError(
            "commentary_census: no CENSUS* lines parsed from Prolog output — "
            "the census did not run (corpus not loaded?).")

    for src, d in sources.items():
        n = d["n_corpus"]
        total = sum(d["buckets"].values())
        if n is None:
            raise PrologError(f"commentary_census[{src}]: no CENSUS_META n_corpus line.")
        if n <= 0:
            raise PrologError(
                f"commentary_census[{src}]: n_corpus={n} (corpus did not load) — refusing "
                "(a 0==0 sum would pass the invariant vacuously).")
        if total != n:
            raise PrologError(
                f"commentary_census[{src}]: Σ buckets ({total}) != n_corpus ({n}) — a "
                "constraint was dropped or double-counted (commentary_cell is not "
                "one-solution-per-constraint).")
        # Domain split: n_in_domain = n_corpus − Σ out-of-domain, cross-checked.
        n_ood = sum(d["buckets"].get(b, 0) for b in d["out_of_domain_buckets"])
        n_in_domain = n - n_ood
        if d["n_in_domain"] is not None and d["n_in_domain"] != n_in_domain:
            raise PrologError(
                f"commentary_census[{src}]: n_in_domain mismatch (Prolog "
                f"{d['n_in_domain']} != n_corpus−ood {n_in_domain}).")
        d["n_in_domain"] = n_in_domain
        # Coverage = (n_in_domain − Σ absence)/n_in_domain — DOMAIN-relative, not
        # corpus-relative. Only where decidable; else null (never a defaulted 1.0).
        if d["coverage_decidable"] and n_in_domain > 0:
            absent = sum(d["buckets"].get(b, 0) for b in d["absence_buckets"])
            d["coverage"] = (n_in_domain - absent) / n_in_domain
        else:
            d["coverage"] = None  # "N/A" — decidability unruled (or empty domain)
        # Prevalence = fired / n_in_domain — a DISTINCT quantity from coverage.
        if d["prevalence_bucket"] is not None and n_in_domain > 0:
            d["prevalence"] = d["buckets"].get(d["prevalence_bucket"], 0) / n_in_domain
        else:
            d["prevalence"] = None

    # Manifest carries corpus identity so live-vs-twin is self-labeling (never a
    # hardcoded 0 for an empty cell — the cell is empty FOR THIS corpus).
    run_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    # corpus_dir defaults to TESTSETS_DIR, so build_manifest omits `corpus_path`
    # and the default artifact stays byte-identical; under an overlay BOTH the
    # manifest and the corpus_hash describe the leg that was actually censused.
    # Stamping the live leg's hash onto an overlay leg's census would be exactly
    # the mislabel corpus_hash exists to prevent (OQ-29).
    manifest = build_manifest(run_at, corpus_dir)
    manifest["corpus_hash"] = _compute_corpus_hash(corpus_dir)
    out = {"manifest": manifest, "sources": sources}
    (out_dir / "commentary_census.json").write_text(
        json.dumps(out, ensure_ascii=False, indent=2), encoding="utf-8")

    cleaned = strip_preamble(raw, _PREAMBLE_MARKERS["commentary_census"])
    (out_dir / "commentary_census.md").write_text(
        cleaned if cleaned.strip() else "# Commentary Census (empty)\n", encoding="utf-8")


def _prolog_reading_totality_gate():
    """OQ-137 standing guard: the reading-totality suite as a fail-fast gate.

    Runs prolog/tests/test_reading_totality.pl (registry-driven: every
    reading_registry:aggregatable_reading/3 entry classed total_on_domain is
    proven exactly-one-solution over its declared domain, with its own fired
    positive controls). Sequential and BEFORE the parallel Prolog analyses —
    commentary_census presumes exactly the totality this suite proves (its
    Σ==n_corpus invariant), so a silently-partial reading must stop the run
    here, not surface as a census sum mismatch downstream.

    Fail-closed by construction: run_tests failing (or the unit not loading)
    makes the swipl -g goal fail -> non-zero exit -> PrologError. Deliberate-
    break wiring control witnessed 2026-07-02 (scratch overlay registering an
    always-failing reading turned this step red; clean run green) — see
    KNOWN_STATE 2026-07-02.
    """
    try:
        run_prolog(
            ["stack.pl", "reading_registry.pl", "commentary_census.pl",
             "tests/test_reading_totality.pl"],
            "corpus_loader:load_all_testsets, run_tests(reading_totality)",
        )
    except PrologError as e:
        raise SystemExit(
            "OQ-137 reading-totality gate failed — a registered aggregatable "
            "reading is not exactly-one on its declared domain (or the suite "
            "did not run). Fix the reading to a typed token (design_discipline "
            f"§5) or correct its registry entry, then re-run. Detail: {e}"
        )


def _prolog_epsilon_declaration_gate():
    """OQ-205 standing guard: the ε-declaration suite as a fail-fast gate.

    Runs prolog/tests/test_epsilon_declaration.pl over the live corpus —
    the ENFORCEMENT for the spec §3 fail-closed provenance rule (the
    data_validation checkers it consumes are WARN-only at _prolog_validation).
    Gate-red: (a) any three-site ε drift (epsilon_provenance ValueAsWritten
    vs constraint_metric vs domain_priors); (b) emission-totality breach
    (orphan provenance / census buckets not summing to the corpus);
    (c) in-suite planted-control failure — on the pre-build corpus the drift
    domain is EMPTY, so the planted controls are what keep this gate
    non-vacuous (Pattern 5). NOT gate-red: missing provenance on pre-build
    stories (the loud-null stratum, warning-grade by operator ruling
    2026-07-03).

    Fail-closed by construction: run_tests failing (or the unit not loading)
    makes the swipl -g goal fail -> non-zero exit -> PrologError.

    Second swipl = Control P (spec §6): the four planted fixture stories in
    tests/fixtures/eps_controls/ run through the REAL load path
    (corpus_path overlay asserted BEFORE load_all_testsets — a FRESH process
    is mandatory: the corpus_loaded/0 guard silently ignores an in-process
    overlay-after-load; process exit is the cleanup). Violations must equal
    EXACTLY the planted set (flagged AT the holes): drift == {eps_ctl_drifted},
    loud-null == {eps_ctl_missing}, eps_ctl_clean flag-free (implied by the
    exact sets, two-sided), and eps_ctl_no_epsilon must read ε = unknown
    through get_true_metric (the U1 fallback is DEAD, not rerouted).
    """
    try:
        run_prolog(
            ["stack.pl", "data_validation.pl",
             "tests/test_epsilon_declaration.pl"],
            "corpus_loader:load_all_testsets, run_tests(epsilon_declaration)",
        )
    except PrologError as e:
        raise SystemExit(
            "OQ-205 epsilon-declaration gate failed — three-site ε drift, "
            "orphan provenance, a census-partition breach, or a planted "
            "control not firing. Fix the story file (or the emission) and "
            f"re-run. Detail: {e}"
        )
    try:
        run_prolog(
            ["stack.pl", "data_validation.pl"],
            "retractall(config:param(corpus_path, _)), "
            "assertz(config:param(corpus_path, 'tests/fixtures/eps_controls')), "
            "corpus_loader:load_all_testsets, "
            "findall(Cc, corpus_loader:corpus_constraint(Cc), Ccs), "
            "sort(Ccs, CcsS), length(CcsS, NFix), "
            "( NFix =:= 4 -> true ; throw(ctl_p_fixture_count(NFix)) ), "
            "findall(Cd, ( corpus_loader:corpus_constraint(Cd), "
            "              data_validation:epsilon_provenance_drift(Cd, _) ), Ds0), "
            "sort(Ds0, Ds), "
            "( Ds == [eps_ctl_drifted] -> true ; throw(ctl_p_drift_set(Ds)) ), "
            "findall(Cm, ( corpus_loader:corpus_constraint(Cm), "
            "              data_validation:missing_epsilon_provenance(Cm) ), Ms0), "
            "sort(Ms0, Ms), "
            "( Ms == [eps_ctl_missing] -> true ; throw(ctl_p_loud_null_set(Ms)) ), "
            "( constraint_indexing:get_true_metric(eps_ctl_no_epsilon, extractiveness, unknown) "
            "  -> true ; throw(ctl_p_fallback_not_dead) ), "
            "format(user_error, '[ctl-p] fixture pass green: drift AT eps_ctl_drifted, "
            "loud-null AT eps_ctl_missing, clean flag-free, no-eps reads unknown~n', [])",
        )
    except PrologError as e:
        raise SystemExit(
            "OQ-205 Control P fixture pass failed — the gate's checkers did "
            "not flag exactly the planted fixture set through the real load "
            f"path (tests/fixtures/eps_controls/). Detail: {e}"
        )


def _prolog_residual_signature_gate():
    """OQ-138 standing guard: the seven residual resolve_modal_signature_conflict
    clauses are corpus-inert.

    The residual clauses ROUTE to abstain (unknown) rather than manufacture a type
    (config residual_signature_override_enabled=0). That guard prevents silent
    manufacture at RUNTIME; this gate is the MONITORED surface that makes a future
    fire LOUD — a residual clause firing (a new metric-type × signature co-occurrence)
    aborts the run, so the successor OQ auto-reopens for the owed fire-time discriminant
    ruling rather than the fire passing silently as an abstained seat.

    Runs prolog/tests/test_residual_signature_inert.pl over the live corpus:
    (a) residual_signature_firing count == 0; (b) a non-vacuity positive control
    (the monitor recognizes the residual shape) so the 0 is measured-empty, not
    didn't-look (Pattern 5). Fail-closed by construction (run_tests failing ->
    nonzero exit -> PrologError). NOT dead code: do not remove or fold into the
    parallel tasks. Provenance: audits/2026-07-14_oq138_residual_rewitness/.
    """
    try:
        run_prolog(
            ["stack.pl", "tests/test_residual_signature_inert.pl"],
            "corpus_loader:load_all_testsets, run_tests(residual_signature_inert)",
        )
    except PrologError as e:
        raise SystemExit(
            "OQ-138 residual-signature gate failed — a residual "
            "resolve_modal_signature_conflict clause fired on the corpus. The guard "
            "abstained it to unknown (no manufacture), but the fire-time discriminant "
            "is now owed: reopen the successor OQ and rule the route (route target + "
            f"severity), then re-run. Detail: {e}"
        )


def _prolog_agency_gate():
    """OQ-66 standing guard: the two-gate agency principle is enforced.

    Replaced the old drl_core natural_law_without_beneficiary/1 INERTNESS
    tripwire, which the 2026-06-05 corpus reset left red-for-the-wrong-reason
    for ~7 weeks (0 of its 11 fixture constraints survived the reset, and one
    of its tests passed VACUOUSLY as a \\+ over an absent constraint). The
    deferral it watched is gone: the agent-filtered read landed 2026-07-25
    (ruling 63-A). What is watched now is the two-gate principle at
    prolog/narrative_ontology.pl:398-419 — a non_agent_beneficiary/1 entry
    RELEASES a natural-law certification on its host, so it needs both an
    ontology-kind gate and a host-convergence gate; an unlisted value defaults
    to AGENT (fail-open to status quo).

    First swipl — the suite over the live corpus: registry contents exactly the
    two ruled values, the filter is EXACTLY registry membership (single clause,
    static, no kind inference), nlwb reads the filtered view, and the three
    snare floors are still config constants. Fail-closed by construction
    (run_tests failing, or the unit not loading, makes the swipl -g goal fail
    -> non-zero exit -> PrologError).

    Second swipl = the planted fixture pass, and it is what makes this gate
    NON-VACUOUS. No beneficiary fact in ANY of the five live legs carries a
    registered non-agent value (9,119 facts, zero hits — measured 2026-07-25),
    so on the live corpus the raw and filtered reads are extensionally
    IDENTICAL and a revert of drl_core.pl would keep the suite green. The four
    tests/fixtures/nlwb_controls/ stories are the only place the two readings
    come apart. They run through the REAL load path (corpus_path overlay
    asserted BEFORE load_all_testsets — a FRESH process is mandatory: the
    corpus_loaded/0 guard silently ignores an in-process overlay-after-load;
    process exit is the cleanup) and must satisfy the planted truth table
    EXACTLY: nlwb true at {nonagent_only, no_beneficiary} and false at
    {agent_only, mixed}, with nlwb_ctl_nonagent_only reachable as snare under
    the RAW reading (a fixture whose snare gate never opens produces a no-flip
    that reads exactly like a working control).

    NOT dead code: do not remove or fold into the parallel tasks. Reopen
    condition is in the suite header. Cost: one additional swipl per run.
    Provenance: audits/2026-07-25_oq66_nlwb_filter_cutover/.
    """
    try:
        run_prolog(
            ["stack.pl", "tests/test_agent_beneficiary.pl"],
            "corpus_loader:load_all_testsets, run_tests(agent_beneficiary)",
        )
    except PrologError as e:
        raise SystemExit(
            "OQ-66 agency gate failed — the non_agent_beneficiary registry, the "
            "agent_beneficiary filter, the nlwb filtered read, or the snare floor "
            "provenance broke. If a registry entry was added, it owes the gate-2 "
            f"convergence read (narrative_ontology.pl:398-419). Detail: {e}"
        )
    try:
        run_prolog(
            ["stack.pl"],
            "retractall(config:param(corpus_path, _)), "
            "assertz(config:param(corpus_path, 'tests/fixtures/nlwb_controls')), "
            "corpus_loader:load_all_testsets, "
            "findall(Cc, corpus_loader:corpus_constraint(Cc), Ccs), "
            "sort(Ccs, CcsS), length(CcsS, NFix), "
            "( NFix =:= 4 -> true ; throw(agency_fixture_count(NFix)) ), "
            # The planted truth table: nlwb is TRUE exactly where the filtered
            # reading has no surviving agent-kind beneficiary.
            "findall(Ct, ( corpus_loader:corpus_constraint(Ct), "
            "              drl_core:natural_law_without_beneficiary(Ct) ), Ts0), "
            "sort(Ts0, Ts), "
            "( Ts == [nlwb_ctl_no_beneficiary, nlwb_ctl_nonagent_only] "
            "  -> true ; throw(agency_nlwb_set(Ts)) ), "
            # Reachability control, two-sided on IDENTICAL metrics. All four
            # fixtures author the same ε/supp/theater; the ONLY variable is
            # beneficiary composition. nlwb_ctl_agent_only (unlisted value,
            # survives the filter) must still reach snare, and
            # nlwb_ctl_nonagent_only (registered value, filtered away) must not.
            # Same metrics + opposite outcome ⇒ the block is caused by registry
            # membership, not by weak metrics. Without the positive half, a
            # fixture whose snare gate never opens would produce a no-snare that
            # reads exactly like a working control.
            "constraint_indexing:site_contexts_canonical(Cxs), "
            "findall(At, ( member(Cx, Cxs), "
            "              drl_core:classify_from_metrics(nlwb_ctl_agent_only, 0.80, "
            "                  0.80, 0.75, Cx, At) ), Ats), "
            "( memberchk(snare, Ats) -> true ; throw(agency_snare_unreachable(Ats)) ), "
            "findall(Nt, ( member(Cx, Cxs), "
            "              drl_core:classify_from_metrics(nlwb_ctl_nonagent_only, 0.80, "
            "                  0.80, 0.75, Cx, Nt) ), Nts), "
            "( memberchk(snare, Nts) -> throw(agency_flip_absent(Nts)) ; true ), "
            # MaxEnt mirror coverage (OQ-250). The PROLOG tangled_rope guard at
            # drl_core.pl:426 is structurally DEAD -- that clause requires
            # requires_active_enforcement/1 and nlwb forbids exactly that, so NO
            # fixture can exercise it (the deadness is OQ-250's subject, and it is
            # why this gate covers the mirror instead). But the MaxEnt mirror
            # boolean_spec(tangled_rope, nlwb, forbidden) at
            # maxent_classifier.pl:186 is LIVE -- it evaluates the feature with no
            # enforcement conjunct gating it. Two-sided on identical metrics: the
            # nlwb-TRUE fixture must score strictly worse on tangled_rope than its
            # nlwb-FALSE twin. Relative, not hardcoded, so a penalty-weight change
            # does not turn this red spuriously.
            "maxent_classifier:boolean_log_likelihood(nlwb_ctl_nonagent_only, "
            "    tangled_rope, LLon), "
            "maxent_classifier:boolean_log_likelihood(nlwb_ctl_agent_only, "
            "    tangled_rope, LLao), "
            "( LLon < LLao -> true "
            "  ; throw(agency_maxent_tr_mirror_inert(LLon, LLao)) ), "
            "format(user_error, '[agency] fixture pass green: nlwb AT "
            "{no_beneficiary, nonagent_only}, snare reachable at nonagent_only, "
            "agent_only + mixed unflipped~n', [])",
        )
    except PrologError as e:
        raise SystemExit(
            "OQ-66 agency fixture pass failed — the planted nlwb_controls truth "
            "table did not hold through the real load path "
            "(tests/fixtures/nlwb_controls/). A revert of the agent_beneficiary "
            "read in drl_core.pl surfaces HERE, not in the live-corpus suite "
            f"(the live legs carry zero registered beneficiary values). Detail: {e}"
        )


def _epsilon_stability_sweep():
    """OQ-205 ε-stability sweep (data-side, r=0.02) as a pipeline step.

    Runs python/sweeps/epsilon_stability.py on the live leg. The script's
    Control S selftest runs first fail-closed and its R3 tripwires are fatal
    on the live leg — a non-zero exit here means the stability instrument
    itself is broken (or a kill condition tripped), so the run stops rather
    than ship a report whose stability column silently never looked.
    """
    script = REPO_ROOT / "python" / "sweeps" / "epsilon_stability.py"
    result = subprocess.run(
        [sys.executable, str(script)],
        cwd=str(REPO_ROOT),
        capture_output=True,
        text=True,
        timeout=3600,
    )
    if result.returncode != 0:
        raise SystemExit(
            "OQ-205 epsilon-stability sweep failed (Control S selftest red, "
            "an R3 kill-condition tripwire, or a crash): "
            f"{(result.stderr or result.stdout).strip()[-600:]}"
        )


def _phase_prolog(progress, parallel):
    """Phase 2: run all Prolog analyses in parallel."""
    # Diagnostic — remove after debugging
    if progress:
        import shutil
        progress("pipeline", f"PROLOG_DIR = {PROLOG_DIR}")
        progress("pipeline", f"PROLOG_DIR exists = {PROLOG_DIR.exists()}")
        progress("pipeline", f"swipl path = {shutil.which('swipl')}")
        progress("pipeline", f"testsets count = {len(list(TESTSETS_DIR.glob('*.pl')))}")

    # OQ-137 standing guard — sequential fail-fast, before the parallel set
    # (which includes commentary_census). Raises SystemExit on red.
    if progress:
        progress("pipeline", "[PROLOG] reading-totality gate...")
    _prolog_reading_totality_gate()

    # OQ-205 standing guard — sequential fail-fast, same slot. Raises
    # SystemExit on red. NOT dead code: enforcement for the ε-declaration
    # fail-closed rule (do not remove or fold into the parallel tasks).
    if progress:
        progress("pipeline", "[PROLOG] epsilon-declaration gate...")
    _prolog_epsilon_declaration_gate()

    # OQ-138 standing guard — sequential fail-fast, same slot. Raises SystemExit
    # on red (a residual signature clause fired). NOT dead code: the monitored
    # surface for the residual-clause abstain guard (do not remove or fold in).
    if progress:
        progress("pipeline", "[PROLOG] residual-signature gate...")
    _prolog_residual_signature_gate()

    # OQ-66 standing guard — sequential fail-fast, same slot. Raises SystemExit
    # on red. NOT dead code: the two-gate agency principle's enforcement, and
    # the ONLY place a revert of the drl_core agent-filtered read is visible
    # (the live legs carry zero registered beneficiary values, so the suite
    # alone would stay green). Do not remove or fold into the parallel tasks.
    if progress:
        progress("pipeline", "[PROLOG] agency gate...")
    _prolog_agency_gate()

    if progress:
        progress("pipeline", "[PROLOG] Running analyses...")

    tasks = [
        ("validation",  _prolog_validation),
        ("fingerprint", _prolog_fingerprint),
        ("orbit",       _prolog_orbit),
        ("fpn",         _prolog_fpn),
        ("maxent",      _prolog_maxent),
        ("abductive",   _prolog_abductive),
        ("covering",    _prolog_covering),
        ("giant_comp",  _prolog_giant_comp),
        ("coupling",    _prolog_coupling),
        ("maxent_diag", _prolog_maxent_diag),
        ("commentary_census", _prolog_commentary_census),
    ]
    results = _run_parallel(tasks, progress, parallel)

    # OQ-182: trajectory (HAC clustering) is O(N^2) and memory-heavy like giant_comp;
    # running both concurrently in the thread pool intermittently stalled the pipeline.
    # _run_parallel's `with ThreadPoolExecutor` has joined giant_comp before returning,
    # so running trajectory here guarantees the two heavy stages never co-reside.
    # Order is correctness-irrelevant: trajectory's only output (context_profile_report.md)
    # has no downstream consumer (C0 invariant).
    results.append(_run_step("trajectory", _prolog_trajectory, progress))

    # OQ-205: ε-stability sweep — post-parallel sequential slot (the
    # ThreadPoolExecutor above has joined, so the sweep's swipl never
    # co-resides with giant_comp/trajectory, the OQ-182 rule). SystemExit on
    # red: Control S's selftest and the R3 tripwires are recurring-gate-
    # enforced from here on (ruling/close-honesty — once OQ-205 closes, this
    # is the sole enforcement).
    if progress:
        progress("pipeline", "[PROLOG] epsilon-stability sweep (OQ-205)...")
    _epsilon_stability_sweep()

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[PROLOG] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 3: POST-PROLOG (sequential)
# ---------------------------------------------------------------------------

def _phase_post_prolog(progress):
    """Orbit normalization + JSON report generation."""
    results = []

    # Orbit normalization
    def _orbit_norm():
        if (OUTPUTS_DIR / "orbit_data.json").exists():
            import normalize_orbit_ids
            normalize_orbit_ids.normalize()

    if progress:
        progress("pipeline", "[NORM] Normalizing orbit data IDs...")
    results.append(_run_step("orbit_norm", _orbit_norm, progress))

    # Fingerprint JSON sidecar (fingerprint_report.md produced in Phase 2)
    def _fingerprint_json():
        import parse_fingerprint_json
        parse_fingerprint_json.main()

    if progress:
        progress("pipeline", "[NORM] Generating fingerprint_data.json...")
    results.append(_run_step("fingerprint_json", _fingerprint_json, progress))

    # JSON report (depends on abductive_data.json existing)
    def _json_report():
        run_prolog(
            ["stack.pl", "covering_analysis.pl", "maxent_classifier.pl",
             "dirac_classification.pl", "diagnostic_summary.pl",
             "post_synthesis.pl", "json_report.pl"],
            "run_json_report",
        )

    if progress:
        progress("pipeline", "[JSON] Generating structured JSON report...")
    results.append(_run_step("json_report", _json_report, progress))

    # Routing sink (OQ-128): per-seat author↔engine diff router → routing_sink.json.
    # The engine ROUTES the diff (it does not reclassify); this keeps the sink's
    # output fresh with the corpus (Build Discipline Pattern 1: wire the consumer).
    def _routing_sink():
        run_prolog(
            ["stack.pl", "routing_sink.pl"],
            "routing_sink:run_routing_sink",
        )

    if progress:
        progress("pipeline", "[SINK] Generating routing_sink.json (per-seat author↔engine diff)...")
    results.append(_run_step("routing_sink", _routing_sink, progress))

    # Kernel orbit export (OQ-150): the two orbit-keys pipeline_output.json does not
    # serialise — per-kernel obstruction-class (Tier-1) + per-reading grounding-profile.
    # Feeds the orbit_operator step (run after the manifest). Stamps n_constraints so the
    # joiner can fail-closed on a stale file (Pattern 1: assert same-run before joining).
    def _kernel_orbit_export():
        run_prolog(
            ["stack.pl", "kernel_orbit_export.pl"],
            "kernel_orbit_export:run_kernel_orbit_export",
        )

    if progress:
        progress("pipeline", "[ORBIT] Exporting kernel_obstruction.json (obstruction + grounding)...")
    results.append(_run_step("kernel_orbit_export", _kernel_orbit_export, progress))

    # OQ-58 referential-integrity census (NON-GATING). Pure read-only glob of
    # testsets/*.pl — no engine, no classification. Reports dangling
    # cs_reading_relation edges → distinct missing readings, with the in-degree>=2
    # DEFENSIBLE set. NON-GATING by design: the live corpus is a singleton topical
    # working set (~1 reading/kernel), so nearly every sibling edge dangles and a
    # gate would red-line every run; the value is the standing CENSUS, not a pass/
    # fail. This sidecar — reading_reference_census.json — is the LIVE backlog
    # (Pattern 1: re-run in-pipeline so it can't go stale), NOT the per-generation-
    # run cs_reading_relation_quarantine.json. See ISSUES.md OQ-58 / GAP-07.
    def _reading_linter():
        sys.path.insert(0, str(Path(__file__).resolve().parent / "audits"))
        import reading_reference_linter as L
        if not L.selftest():
            # positive controls failed → rules not trusted; report, do not gate.
            if progress:
                progress("pipeline", "[LINTER] selftest FAILED — census not trusted this run")
            return
        summary = L.summarize(TESTSETS_DIR)
        run_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
        manifest = build_manifest(run_at)
        manifest["corpus_hash"] = _compute_corpus_hash(TESTSETS_DIR)
        (OUTPUTS_DIR / "reading_reference_census.json").write_text(
            json.dumps({"manifest": manifest, "summary": summary},
                       ensure_ascii=False, indent=2), encoding="utf-8")
        if progress:
            progress("pipeline",
                     f"[LINTER] cs_reading_relation: {summary['n_dangling']} dangling → "
                     f"{summary['n_missing']} missing readings / "
                     f"{summary['n_kernels_with_missing']} kernels "
                     f"({len(summary['defensible_ge2'])} id>=2 defensible) — NON-GATING")

    if progress:
        progress("pipeline", "[LINTER] OQ-58 referential-integrity census (non-gating)...")
    results.append(_run_step("reading_linter", _reading_linter, progress))

    return results


# ---------------------------------------------------------------------------
# Phase 4: PYTHON TIER 1 (parallel)
# ---------------------------------------------------------------------------

def _phase_python_tier1(progress, parallel):
    """Type reports, omega, corpus extraction, meta report."""
    if progress:
        progress("pipeline", "[TIER1] Running Python tier 1...")

    import type_reporter
    from orbit_utils import load_orbit_data

    pipeline_data = type_reporter.load_pipeline_data()
    orbit_data = load_orbit_data()

    TYPE_KEYS = ["snare", "rope", "scaffold", "piton", "mountain",
                 "tangled_rope", "false_mountain"]

    def _make_type_fn(key, pdata, odata):
        def fn():
            type_reporter.run_type_report(key, pdata, odata)
        return fn

    tasks = []
    for key in TYPE_KEYS:
        tasks.append((f"type_{key}", _make_type_fn(key, pipeline_data, orbit_data)))

    # Omega reporter
    def _omega():
        import omega_reporter
        omega_reporter.main()
    tasks.append(("omega", _omega))

    # Corpus extraction
    def _corpus():
        from extract_corpus_data import CorpusExtractor
        extractor = CorpusExtractor(str(OUTPUTS_DIR / "output.txt"))
        extractor.extract_all()
        extractor.save_json(str(OUTPUTS_DIR / "corpus_data.json"))
    tasks.append(("corpus_extract", _corpus))

    # Meta reporter (prints to stdout — capture)
    def _meta():
        import meta_reporter
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            reporter = meta_reporter.MetaReporter()
            reporter.parse()
            reporter.generate_report()
        (OUTPUTS_DIR / "meta_report.txt").write_text(buf.getvalue(), encoding="utf-8")
    tasks.append(("meta_report", _meta))

    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[TIER1] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 5: PYTHON TIER 2 (parallel)
# ---------------------------------------------------------------------------

def _phase_python_tier2(progress, parallel):
    """Variance, pattern mining, sufficiency — depend on corpus_data.json."""
    if progress:
        progress("pipeline", "[TIER2] Running Python tier 2...")

    corpus_path = str(OUTPUTS_DIR / "corpus_data.json")
    pipeline_path = str(OUTPUTS_DIR / "pipeline_output.json")

    def _variance():
        from variance_analyzer import VarianceAnalyzer
        analyzer = VarianceAnalyzer(corpus_path)
        analyzer.generate_report(str(OUTPUTS_DIR / "variance_analysis.md"))

    def _pattern():
        from pattern_miner import PatternMiner
        miner = PatternMiner(corpus_path)
        miner.generate_report(str(OUTPUTS_DIR / "pattern_mining.md"))

    def _sufficiency():
        from sufficiency_tester import SufficiencyTester
        tester = SufficiencyTester(corpus_path, pipeline_path)
        tester.generate_report(
            str(OUTPUTS_DIR / "index_sufficiency.md"),
            str(OUTPUTS_DIR / "index_sufficiency.json"),
        )

    tasks = [
        ("variance",    _variance),
        ("pattern",     _pattern),
        ("sufficiency", _sufficiency),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[TIER2] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 6: ENRICHMENT (sequential)
# ---------------------------------------------------------------------------

def _phase_enrichment(progress):
    """Produce enriched_pipeline.json."""
    if progress:
        progress("pipeline", "[ENRICH] Producing enriched_pipeline.json...")

    def _enrich():
        import enrich_pipeline_json
        enrich_pipeline_json.main()

    result = _run_step("enrich_pipeline", _enrich, progress)
    return [result]


# ---------------------------------------------------------------------------
# Phase 7: PYTHON TIER 3 (parallel)
# ---------------------------------------------------------------------------

_ARGV_LOCK = threading.Lock()


def _phase_python_tier3(progress, parallel):
    """Tangled decomposition, classification confidence, boundary normality,
    boolean independence, institutional dissent."""
    if progress:
        progress("pipeline", "[TIER3] Running Python tier 3...")

    def _tangled():
        import tangled_decomposition
        with _ARGV_LOCK:
            saved = sys.argv
            sys.argv = ["tangled_decomposition.py"]
            try:
                tangled_decomposition.main()
            finally:
                sys.argv = saved

    def _confidence():
        import classification_confidence
        classification_confidence.main()

    def _boundary():
        import boundary_normality
        boundary_normality.main()

    def _boolean():
        import boolean_independence
        boolean_independence.main()

    def _dissent():
        import institutional_dissent_analysis
        institutional_dissent_analysis.main()

    tasks = [
        ("tangled_decomposition",      _tangled),
        ("classification_confidence",  _confidence),
        ("boundary_normality",         _boundary),
        ("boolean_independence",       _boolean),
        ("institutional_dissent",      _dissent),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[TIER3] Done ({ok}/{len(results)} succeeded).")

    return results


# ---------------------------------------------------------------------------
# Phase 8: OMEGA ENRICHMENT (sequential)
# ---------------------------------------------------------------------------

def _phase_omega_enrichment(progress):
    """Enrich omega report — conditional on corpus_data.json + omega_data.json."""
    if progress:
        progress("pipeline", "[ENRICH] Enriching omega report...")

    corpus_exists = (OUTPUTS_DIR / "corpus_data.json").exists()
    omega_exists = (OUTPUTS_DIR / "omega_data.json").exists()

    if not (corpus_exists and omega_exists):
        if progress:
            progress("pipeline", "[ENRICH] Skipping omega enrichment (missing dependencies).")
        return [StepResult(name="omega_enrich", status="skipped")]

    def _enrich():
        import omega_enricher
        omega_enricher.main()

    result = _run_step("omega_enrich", _enrich, progress)
    return [result]


# ---------------------------------------------------------------------------
# Phase 9: CROSS-CONSTRAINT ANALYSIS (parallel 9a, then sequential 9b)
# ---------------------------------------------------------------------------

def _phase_cross_constraint(progress, parallel):
    """Phase 9a: scenario convergence + omega cross-constraint (parallel)."""
    if progress:
        progress("pipeline", "[XCON] Running cross-constraint analyses...")

    def _scenario():
        import scenario_convergence
        scenario_convergence.run()

    def _omega_xcon():
        import omega_cross_constraint
        omega_cross_constraint.run()

    tasks = [
        ("scenario_convergence",   _scenario),
        ("omega_cross_constraint",  _omega_xcon),
    ]
    results = _run_parallel(tasks, progress, parallel)

    if progress:
        ok = sum(1 for r in results if r.status == "ok")
        progress("pipeline", f"[XCON] Done ({ok}/{len(results)} succeeded).")

    return results


def _phase_evaluative_convergence(progress):
    """Phase 9b: evaluative convergence synthesis (sequential, always runs)."""
    if progress:
        progress("pipeline", "[EVAL] Running evaluative convergence synthesis...")

    def _eval():
        import evaluative_convergence
        evaluative_convergence.main()

    result = _run_step("evaluative_convergence", _eval, progress)
    return [result]


def _phase_epsilon_authorship_readout(progress):
    """Phase 9c: OQ-78 standing ε-authorship readout (OQ-205 §8; cheap, pure JSON).

    Consumes the U6 provenance emission + the U7 stability artifact; writes
    outputs/epsilon_authorship_readout.{json,md}. Per-stratum mode fraction,
    distinct values, last-digit histogram, exactly-at-threshold count — the
    OQ-78 fingerprint as a standing per-run readout instead of a one-off
    census.
    """
    if progress:
        progress("pipeline", "[EPS-READOUT] epsilon authorship readout (OQ-78)...")

    def _readout():
        script = REPO_ROOT / "python" / "epsilon_authorship_readout.py"
        result = subprocess.run(
            [sys.executable, str(script)],
            cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=300,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"epsilon_authorship_readout.py exited {result.returncode}: "
                f"{(result.stderr or result.stdout).strip()[-400:]}")

    result = _run_step("epsilon_authorship_readout", _readout, progress)
    return [result]


def _phase_q_provenance_readout(progress):
    """Phase 9d: OQ-254 standing Q-provenance readout (cheap, pure JSON).

    Buckets every story's epsilon_provenance generation_run_id against the
    tracked SCOPE-manifest surface (agent/decompose_manifests/): joined /
    joined_archive_not_authoritative / no_run_id_authored (with counted
    breakdown) / run_id_authored_manifest_unreachable (the Pattern-6 bucket).
    The script runs its planted two-sided controls on every invocation and
    exits non-zero if they fail. Writes outputs/q_provenance_readout.{json,md}.
    """
    if progress:
        progress("pipeline", "[Q-READOUT] Q-provenance readout (OQ-254)...")

    def _readout():
        script = REPO_ROOT / "python" / "q_provenance_readout.py"
        result = subprocess.run(
            [sys.executable, str(script)],
            cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=300,
        )
        if result.returncode != 0:
            raise RuntimeError(
                f"q_provenance_readout.py exited {result.returncode}: "
                f"{(result.stderr or result.stdout).strip()[-400:]}")

    result = _run_step("q_provenance_readout", _readout, progress)
    return [result]


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

def run_pipeline(
    progress: Optional[Callable[[str, str], None]] = None,
    parallel: int = 4,
) -> PipelineResult:
    """Execute the full DR analysis pipeline.

    Args:
        progress: Optional callback(step, message) for status updates.
        parallel: Max concurrent threads for parallel phases.

    Returns:
        PipelineResult with per-step results and any errors.
    """
    pipeline_result = PipelineResult()
    t0 = time.time()
    run_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    OUTPUTS_DIR.mkdir(parents=True, exist_ok=True)

    # ── ISSUES.md status-grammar gate (2026-06-04) ──────────────────────────
    # DO NOT REMOVE OR BYPASS THIS GATE. If it fails: fix ISSUES.md so that
    #   python3 python/issues_status.py --check
    # passes (the malformed entries are named in the output), THEN re-run.
    # Rationale: the status ledger is the repo's open-question census; a
    # checker that isn't run isn't checking, and an unparseable ledger
    # silently corrupts the next census. Grammar: `**Status:** <token>` with
    # token in {open, investigating, mitigated, partial, resolved, disposed, future},
    # optionally ` — detail` — see the ISSUES.md footer.
    from issues_status import scan as _issues_scan
    _, _issues_problems = _issues_scan()
    if _issues_problems:
        for _p in _issues_problems:
            _msg = f"[ISSUES-GATE] MALFORMED: {_p}"
            if progress:
                progress("pipeline", _msg)
            print(_msg, file=sys.stderr)
        raise SystemExit(
            f"ISSUES.md status-grammar gate failed ({len(_issues_problems)} "
            "malformed). Fix ISSUES.md — do NOT remove this gate — verify "
            "with `python3 python/issues_status.py --check`, then re-run "
            "the pipeline."
        )

    # --- LOAD-WARNING GATE (OQ-96; sibling of the ISSUES gate above) -------
    # A swipl load warning outside the allowlist (prolog/
    # load_warning_allowlist.txt) aborts the run. Rationale: the
    # domain_registry dangling-module warning was emitted at EVERY load for
    # four months while ad-hoc `grep -v Warning` filters hid it, until the
    # dead reference crashed the validation suite at runtime (OQ-96). Do NOT
    # remove or bypass; to accept a new known-benign warning, add its
    # normalized record to the allowlist (see
    # `python3 python/load_warning_gate.py --baseline`).
    from load_warning_gate import collect_warnings as _lw_collect
    _lw_allow_path = PROLOG_DIR / "load_warning_allowlist.txt"
    _lw_allowed = set()
    if _lw_allow_path.exists():
        _lw_allowed = {ln.strip() for ln in _lw_allow_path.read_text().splitlines()
                       if ln.strip() and not ln.startswith("#")}
    _lw_unexpected = [r for r in _lw_collect() if r not in _lw_allowed]
    if _lw_unexpected:
        for _r in _lw_unexpected:
            _msg = f"[WARNING-GATE] UNEXPECTED: {_r}"
            if progress:
                progress("pipeline", _msg)
            print(_msg, file=sys.stderr)
        raise SystemExit(
            f"swipl load-warning gate failed ({len(_lw_unexpected)} unexpected). "
            "Fix the warning or allowlist it deliberately — verify with "
            "`python3 python/load_warning_gate.py`, then re-run the pipeline."
        )

    # --- AXIS-BOUNDARY GATE (OQ-15 / OQ-135; sibling of the load-warning gate) ---
    # The committer→observer one-seat invariant is convention-not-theorem in its
    # CARDINALITY ("exactly one forward bridge") — the type system privileges the
    # entailment relation `influences` by KIND, but nothing forbids a second
    # crossing. After OQ-15's core was ruled closed (policed-in-place), this guard
    # is the SOLE enforcement of that boundary, so its positive controls must be
    # shown-firing on every run, not assumed: --selftest asserts BOTH planted
    # violations (path-b payload widening, path-c non-influences seam) still fire
    # AND the clean corpus passes. A new un-allowlisted cross-axis read, or a guard
    # that has silently stopped discriminating, HALTS the run. Do NOT remove or
    # bypass; to accept a new boundary edge, add it to
    # prolog/axis_boundary_allowlist.txt deliberately with its role tag.
    from check_axis_boundary import selftest as _axis_selftest
    if _axis_selftest() != 0:
        _msg = ("[AXIS-GATE] selftest failed — a new committer→observer read, or a "
                "guard that stopped discriminating. Verify with "
                "`python3 python/check_axis_boundary.py --selftest`.")
        if progress:
            progress("pipeline", _msg)
        print(_msg, file=sys.stderr)
        raise SystemExit("axis-boundary gate failed; see [AXIS-GATE] above.")

    # --- GRID FIRST-CONTACT GATE (OQ-93 flip ruling, 2026-06-11) ----------
    # Every grid-authoring story gets the three-indicator plausibility audit
    # ONCE before any consumer read (ledger: python/grid_audit_ledger.json).
    # C-echo in a new story HALTS the run and demands the opt-in flip be
    # reverted. Do NOT remove or bypass — per-story exclusion is the
    # fail-closed half of the operator's split κ ruling.
    from grid_first_contact_gate import run_gate as _grid_gate
    if _grid_gate() != 0:
        raise SystemExit(
            "Grid first-contact gate failed — a grid-authoring story fired a "
            "plausibility indicator (see [GRID-GATE] lines). Remove/fix the "
            "story or record an operator waiver in "
            "python/grid_audit_ledger.json, then re-run."
        )

    def collect(step_results):
        if isinstance(step_results, list):
            for sr in step_results:
                pipeline_result.steps.append(sr)
                if sr.status == "error":
                    pipeline_result.errors.append(sr.error)
        else:
            pipeline_result.steps.append(step_results)
            if step_results.status == "error":
                pipeline_result.errors.append(step_results.error)

    # Phase 1: PREP
    collect(_run_step("prep", lambda: _phase_prep(progress), progress))

    # Phase 1b: regenerate orbits (sequential — before the parallel Prolog phase,
    # so no two swipl invocations race the shared product_site_orbits.json, and
    # before manifest_inject's fail-closed corpus_hash check).
    collect(_run_step("regenerate_orbits", lambda: _regenerate_orbits(progress), progress))

    # Phase 2: PROLOG ANALYSES (parallel)
    collect(_phase_prolog(progress, parallel))

    # Phase 3: POST-PROLOG (sequential)
    collect(_phase_post_prolog(progress))

    # Abort if the raw export was not produced (json_report writes the raw
    # file; the canonical pipeline_output.json is written by _manifest_step)
    if not (OUTPUTS_DIR / "pipeline_output.raw.json").exists():
        msg = "pipeline_output.raw.json not produced — aborting downstream phases"
        if progress:
            progress("pipeline", f"[FATAL] {msg}")
        pipeline_result.errors.append(msg)
        pipeline_result.total_duration_s = time.time() - t0
        return pipeline_result

    # Write canonical pipeline_output.json = raw export + manifest; verify orbits
    def _manifest_step():
        manifest = build_manifest(run_at)
        inject_manifest(OUTPUTS_DIR / "pipeline_output.raw.json",
                        OUTPUTS_DIR / "pipeline_output.json", manifest)
        # Sidecar provenance for orbit_data.json. orbit_data.json is a pure
        # id->orbit dict consumed by iterating readers (game_theory_*, sheaf_audit,
        # extract_corpus_data, normalize_orbit_ids, meta_reporter,
        # container_typology_analysis), so a sibling "manifest" key cannot be
        # injected in-file. The sidecar carries the SAME manifest dict, making
        # orbit_data.json provably the same run as pipeline_output.json.
        orbit_manifest_path = OUTPUTS_DIR / "orbit_data.manifest.json"
        with open(orbit_manifest_path, "w", encoding="utf-8") as f:
            json.dump({"manifest": manifest}, f, indent=2)
        orbits_path = OUTPUTS_DIR / "product_site_orbits.json"
        check_orbits_corpus_hash(orbits_path)
        # OQ-193: giant_component_analysis.raw.json sidecar manifest (mirrors the
        # orbit sidecar). Same-run binding is gated on the EXECUTED-STAGE RESULT,
        # not file existence alone: stamp ONLY when the giant_comp step actually
        # ran ok AND raw.json exists. _prolog_giant_comp pre-deletes raw.json, so a
        # failed/skipped stage leaves no file and no stamp — a leftover old sidecar
        # then carries the OLD manifest and mismatches at join time (correct
        # degrade to NOT ASSESSED). A future partial-run refactor that skipped
        # giant_comp could otherwise pair a stale raw.json with a fresh stamp.
        giant_ok = any(
            s.name == "giant_comp" and s.status == "ok"
            for s in pipeline_result.steps
        )
        gc_raw = OUTPUTS_DIR / "giant_component_analysis.raw.json"
        gc_manifest_path = OUTPUTS_DIR / "giant_component_analysis.manifest.json"
        if giant_ok and gc_raw.exists():
            with open(gc_manifest_path, "w", encoding="utf-8") as f:
                json.dump({"manifest": manifest}, f, indent=2)
            _gc_stamp_msg = ("[MANIFEST] Stamped giant_component_analysis.manifest.json "
                             f"(run_at={manifest['pipeline_run_at']})")
        else:
            _gc_stamp_msg = ("[MANIFEST] giant_comp not ok or raw.json absent — "
                             "skipped giant_component_analysis sidecar stamp")
        if progress:
            progress("pipeline",
                     f"[MANIFEST] Stamped pipeline_output.json + orbit_data.manifest.json: "
                     f"run_at={manifest['pipeline_run_at']}, "
                     f"commit={manifest['code_commit_short']}, "
                     f"n={manifest['n_constraints']}, dirty={manifest['code_dirty']}")
            progress("pipeline", _gc_stamp_msg)

    collect(_run_step("manifest_inject", _manifest_step, progress))

    # Phase 4: PYTHON TIER 1 (parallel)
    collect(_phase_python_tier1(progress, parallel))

    # False mountain JSON sidecar (false_mountain_report.md produced in Tier 1)
    def _false_mountain_json():
        import parse_false_mountain_json
        parse_false_mountain_json.main()

    collect(_run_step("false_mountain_json", _false_mountain_json, progress))

    # W1 × sheaf_status join — refresh the ranked obstruction artifact each run so it
    # never goes stale against pipeline_output.json. Depends on the manifest step
    # (its same-run guard reads orbit_data.manifest.json + pipeline_output.json's
    # manifest). Non-critical: a guard failure raises JoinAborted, caught by _run_step.
    def _w1_sheaf_join():
        import w1_sheaf_join
        w1_sheaf_join.main()

    collect(_run_step("w1_sheaf_join", _w1_sheaf_join, progress))

    # Orbit operator (OQ-150 → OQ-53 transpose surface): groups readings (across kernels)
    # and kernels by each declared orbit-key → reading_orbits.json + kernel_orbits.json.
    # Depends on the manifest step (reads pipeline_output.json) AND kernel_orbit_export
    # (reads kernel_obstruction.json). Tier-1 keys are the declared surface; Tier-2 are
    # reported model-relative with their twin-agreement numbers stamped inline per orbit
    # (operator ruling 2026-06-20, OQ-56/OQ-53). Same-run guarded; non-critical (a guard
    # failure is caught by _run_step). Note: the live corpus has few multi-reading kernels,
    # so live orbits are sparse by design — the discovery substrate is the twins.
    def _orbit_operator():
        import orbit_operator
        orbit_operator.build()

    collect(_run_step("orbit_operator", _orbit_operator, progress))

    # Phase 5: PYTHON TIER 2 (parallel) — depends on corpus_data.json
    if (OUTPUTS_DIR / "corpus_data.json").exists():
        collect(_phase_python_tier2(progress, parallel))
    else:
        if progress:
            progress("pipeline", "[TIER2] Skipping (corpus_data.json missing).")

    # Phase 6: ENRICHMENT (sequential)
    collect(_phase_enrichment(progress))

    # Phase 7: PYTHON TIER 3 (parallel) — depends on enriched_pipeline.json + corpus_data.json
    if (OUTPUTS_DIR / "enriched_pipeline.json").exists():
        collect(_phase_python_tier3(progress, parallel))
    else:
        if progress:
            progress("pipeline", "[TIER3] Skipping (enriched_pipeline.json missing).")

    # Phase 8: OMEGA ENRICHMENT (sequential)
    collect(_phase_omega_enrichment(progress))

    # Phase 9: CROSS-CONSTRAINT ANALYSIS (depends on enriched_pipeline.json)
    if (OUTPUTS_DIR / "enriched_pipeline.json").exists():
        collect(_phase_cross_constraint(progress, parallel))
        collect(_phase_evaluative_convergence(progress))
    else:
        if progress:
            progress("pipeline", "[XCON] Skipping (enriched_pipeline.json missing).")

    # Phase 9c: OQ-78 standing readout (OQ-205 §8) — sequential post-report,
    # pure JSON (no swipl).
    collect(_phase_epsilon_authorship_readout(progress))

    # Phase 9d: OQ-254 standing Q-provenance readout — sequential, pure JSON.
    collect(_phase_q_provenance_readout(progress))

    pipeline_result.total_duration_s = time.time() - t0

    if progress:
        ok = sum(1 for s in pipeline_result.steps if s.status == "ok")
        total = len(pipeline_result.steps)
        progress("pipeline",
                 f"Pipeline complete: {ok}/{total} steps OK in "
                 f"{pipeline_result.total_duration_s:.1f}s")

    return pipeline_result


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    def _cli_progress(step, msg):
        print(f"[{step}] {msg}")

    result = run_pipeline(progress=_cli_progress)

    print("\n" + "=" * 60)
    print("PIPELINE SUMMARY")
    print("=" * 60)
    for s in result.steps:
        dur = f" [{s.duration_s:.1f}s]" if s.duration_s else ""
        err = f"  error: {s.error[:200]}" if s.error else ""
        print(f"  {s.name:30s} {s.status:8s}{dur}")
        if err:
            print(err)
    print(f"\n  Total time: {result.total_duration_s:.1f}s")
    if result.errors:
        print(f"  Errors: {len(result.errors)}")
        for e in result.errors:
            print(f"    - {e[:200]}")

    # Exit non-zero only if the critical json_report step failed
    critical_failed = any(
        s.status == "error" and s.name == "json_report"
        for s in result.steps
    )
    sys.exit(1 if critical_failed else 0)
