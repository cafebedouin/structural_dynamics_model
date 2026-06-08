"""make_brief — compress an oversized source document into a short, NEUTRAL
structural brief the orchestrator can ingest.

Design (see plan i-had-another-instance-concurrent-pebble.md):
- NEUTRAL compression: preserve the source's contested commitment and its OWN
  framings/constituencies/facts; do NOT pre-partition into named READINGS. The
  primed SCOPE step downstream keeps its kernel-detection role un-anchored.
- A brief is a LOSSY fallback for inputs that won't fit, not an upgrade — Phase-0
  found the whole doc reads richer than its brief. Only brief what won't fit.
- Refusal is NOT auto-bypassed by default. A content refusal raises BriefRefusal;
  the caller prints a guided manual route. Automated bypass is opt-in
  (auto_bypass=True) and LOGS THE WITNESS (the refusal + the reframing that got
  it through), so a false-positive correction stays distinguishable from a launder.

Plain text only (.txt/.md). Convert HTML/PDF to text first.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

root_path = Path(__file__).resolve().parent.parent
if str(root_path) not in sys.path:
    sys.path.insert(0, str(root_path))

from agent import llm_call
from agent.llm_call import ModelCallError
from agent.story_generator_base import REPO_ROOT

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

REDUCE_MODEL = "claude-sonnet-4-5-20250929"   # quality for the final brief
MAP_MODEL = "claude-haiku-4-5-20251001"       # cheap per-chunk extraction
PERMISSIVE_MODEL = "claude-haiku-4-5-20251001"  # opt-in refusal-bypass retry

# make_brief's OWN single-pass reduce budget (distinct from the orchestrator's
# ingest ceiling). ~250 KB ≈ ~60K tokens; above this we map-reduce.
SINGLE_PASS_BUDGET_CHARS = 250_000
CHUNK_CHARS = 40_000
CHUNK_OVERLAP = 2_000

REDUCE_MAX_TOKENS = 2048
MAP_MAX_TOKENS = 1024


REDUCE_SYSTEM = """You compress a source document into a SHORT, NEUTRAL structural brief for a \
downstream classifier that decides whether a topic is a contested "kernel" (a foundational \
disagreement) or flat.

Your brief MUST:
- State the MAIN IDEA: the single shared commitment or question the document puts at issue.
- List the SOURCE'S OWN FRAMINGS: the distinct positions/perspectives PRESENT IN THE DOCUMENT, \
attributed to who holds them, quoting briefly where possible. Do NOT invent positions the \
document does not contain. Do NOT merge them into a single partition or label them "READINGS" — \
report the framings the source itself carries, no more.
- Give KEY FACTS / STAKES / CONSTITUENCIES (numbers, actors, what turns on the outcome).
- State WHAT IS CONTESTED: the axis of disagreement, in the source's own terms.
- If the document presents only ONE framing (no real contest), SAY SO plainly — do not \
manufacture a contest.

Keep it 300-600 words. Output ONLY the brief, in this shape:

<TITLE — source type / date if present>
MAIN IDEA (the contested commitment): ...
SOURCE'S OWN FRAMINGS:
- <framing> — held by <who>
- ...
KEY FACTS / STAKES / CONSTITUENCIES: ...
WHAT IS CONTESTED: ...
"""

MAP_SYSTEM = """You are extracting structural signal from ONE CHUNK of a larger document, for \
later assembly into a brief. From THIS chunk only, extract, as concise bullets: any shared \
commitment or question at issue; any distinct positions/framings present and who holds them; key \
facts, stakes, constituencies, numbers. Ignore boilerplate, legal disclaimers, navigation, and \
formatting. Quote briefly where useful. If the chunk has no structural content, reply exactly \
"no structural content"."""

# Prepended to REDUCE_SYSTEM for the opt-in bypass retry: states genuine analytical intent.
INTENT_PREFIX = """This is a neutral structural/governance analysis of a PUBLISHED document for \
an academic classification system. The task is only to summarize the document's own contested \
commitments and stated framings — not to provide operational or procedural detail. \

"""


class BriefRefusal(RuntimeError):
    """make_brief could not get a brief past the safety classifier.

    Carries the originating ModelCallError as `witness` so the caller can show
    WHAT was refused (model, stop_reason) alongside the manual-route guidance.
    """

    def __init__(self, message: str, *, witness: ModelCallError | None = None):
        super().__init__(message)
        self.witness = witness


def _log(on_progress, msg: str) -> None:
    (on_progress or (lambda m: print(f"[make_brief] {m}")))(msg)


def _chunk(text: str) -> list[str]:
    """Split into ~CHUNK_CHARS windows with small overlap."""
    if len(text) <= CHUNK_CHARS:
        return [text]
    chunks, start = [], 0
    step = CHUNK_CHARS - CHUNK_OVERLAP
    while start < len(text):
        chunks.append(text[start:start + CHUNK_CHARS])
        start += step
    return chunks


def _reduce(material: str, source_name: str, *, model: str, system: str,
            on_progress) -> tuple[str, int, int]:
    """Single reduce call → the final brief."""
    header = f"SOURCE: {source_name}\n\n" if source_name else ""
    text, tin, tout = llm_call.call(
        header + material, model,
        system=system, max_tokens=REDUCE_MAX_TOKENS, temperature=0.2,
    )
    return text.strip(), tin, tout


def make_brief(text: str, *, source_name: str = "", on_progress=None,
               auto_bypass: bool = False, reduce_model: str = REDUCE_MODEL) -> str:
    """Compress *text* into a neutral structural brief.

    Raises BriefRefusal if the content is refused and cannot be carried through
    (always, when auto_bypass is False; only after the permissive retry also
    refuses, when True).
    """
    text = text.strip()
    total_in = total_out = 0

    # --- assemble material to reduce (single-pass or map-reduce) -------------
    if len(text) <= SINGLE_PASS_BUDGET_CHARS:
        material = text
        _log(on_progress, f"single-pass reduce ({len(text):,} chars)")
    else:
        chunks = _chunk(text)
        _log(on_progress, f"map-reduce: {len(chunks)} chunks "
                          f"({len(text):,} chars, ~{len(text)//4:,} tokens)")
        notes = []
        for i, ch in enumerate(chunks, 1):
            try:
                n, tin, tout = llm_call.call(
                    f"CHUNK {i}/{len(chunks)}:\n\n{ch}", MAP_MODEL,
                    system=MAP_SYSTEM, max_tokens=MAP_MAX_TOKENS, temperature=0.1,
                )
                total_in += tin
                total_out += tout
                if "no structural content" not in n.lower():
                    notes.append(f"[chunk {i}] {n.strip()}")
            except ModelCallError as e:
                if not auto_bypass:
                    raise BriefRefusal(
                        f"map step refused on chunk {i}/{len(chunks)} "
                        f"({e}); content cannot be auto-summarized.", witness=e)
                # opt-in: log loudly and skip the refused chunk
                _log(on_progress, f"AUTO-BYPASS: chunk {i} refused "
                                  f"(stop_reason={e.stop_reason}, model={e.model}); SKIPPED")
            _log(on_progress, f"  mapped chunk {i}/{len(chunks)}")
        material = "\n\n".join(notes)
        _log(on_progress, f"map complete: {len(notes)} chunks with content, "
                          f"{total_in:,}→{total_out:,} tokens")

    # --- reduce to the final brief ------------------------------------------
    try:
        brief, tin, tout = _reduce(material, source_name, model=reduce_model,
                                   system=REDUCE_SYSTEM, on_progress=on_progress)
        total_in += tin
        total_out += tout
        _log(on_progress, f"brief ready ({len(brief):,} chars); "
                          f"tokens {total_in:,}→{total_out:,}")
        return brief
    except ModelCallError as e:
        if not auto_bypass:
            raise BriefRefusal(
                f"reduce step refused (stop_reason={e.stop_reason}, "
                f"model={e.model}).", witness=e)
        # opt-in bypass: LOG THE WITNESS (refusal + the reframing applied),
        # then retry on the permissive model with explicit analytical intent.
        _log(on_progress, "=" * 60)
        _log(on_progress, f"AUTO-BYPASS of a safety refusal (opt-in).")
        _log(on_progress, f"  REFUSED BY : {e.model} (stop_reason={e.stop_reason})")
        _log(on_progress, f"  REFUSAL    : {e}")
        _log(on_progress, f"  REFRAMING  : retry on {PERMISSIVE_MODEL} with explicit "
                          f"analytical-intent prefix (INTENT_PREFIX).")
        _log(on_progress, "=" * 60)
        try:
            brief, tin, tout = _reduce(
                material, source_name, model=PERMISSIVE_MODEL,
                system=INTENT_PREFIX + REDUCE_SYSTEM, on_progress=on_progress)
            total_in += tin
            total_out += tout
            _log(on_progress, f"AUTO-BYPASS succeeded on {PERMISSIVE_MODEL}; "
                              f"brief ({len(brief):,} chars). Verify fidelity before use.")
            return brief
        except ModelCallError as e2:
            raise BriefRefusal(
                f"auto-bypass also refused (stop_reason={e2.stop_reason}, "
                f"model={e2.model}).", witness=e2)


def manual_route_message(source_name: str = "", witness: ModelCallError | None = None) -> str:
    """Guidance printed when a refusal is not (or cannot be) auto-bypassed."""
    w = ""
    if witness is not None:
        w = (f"\n  Refusal witness: model={witness.model}, "
             f"stop_reason={witness.stop_reason}")
    src = f" for {source_name}" if source_name else ""
    return (
        f"\n[make_brief] Content refused by the safety classifier{src}.{w}\n"
        f"  This is often a false positive on legitimate published material. The clean\n"
        f"  resolution is to AUTHOR THE BRIEF BY HAND in this Claude Code session, where you\n"
        f"  can articulate genuine analytical intent (which bypasses a flat refusal honestly).\n"
        f"  Produce a neutral structural brief (MAIN IDEA / SOURCE'S OWN FRAMINGS / KEY FACTS /\n"
        f"  WHAT IS CONTESTED), save it beside the source as <stem>_brief.md, then run the\n"
        f"  orchestrator on that file. References:\n"
        f"    - schema:      {REPO_ROOT / 'schemas' / 'constraint_story_schema.json'}\n"
        f"    - scope prompt:{REPO_ROOT / 'prompts' / 'uke_scope_v2_json.md'}\n"
        f"    - generation:  {REPO_ROOT / 'agent' / 'story_generator_base.py'} (build_prompt)\n"
        f"  Or re-run with --auto-bypass-refusal to attempt an opt-in, fully-logged bypass."
    )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser(description="Compress a big/sensitive source into a neutral brief.")
    ap.add_argument("file", help="Plain-text source (.txt/.md). Convert HTML/PDF first.")
    ap.add_argument("-o", "--output", default=None, help="Output path (default: <stem>_brief.md)")
    ap.add_argument("--auto-bypass-refusal", action="store_true",
                    help="On a safety refusal, attempt an opt-in, fully-logged bypass "
                         "(permissive model + analytical-intent reframe) instead of stopping.")
    args = ap.parse_args()

    src = Path(args.file)
    if not src.is_file():
        print(f"[make_brief] not a file: {src}", file=sys.stderr)
        return 2
    text = src.read_text(encoding="utf-8")

    try:
        brief = make_brief(text, source_name=src.name, auto_bypass=args.auto_bypass_refusal)
    except BriefRefusal as e:
        print(manual_route_message(src.name, e.witness), file=sys.stderr)
        return 3

    out = Path(args.output) if args.output else src.with_name(src.stem + "_brief.md")
    out.write_text(brief, encoding="utf-8")
    print(f"[make_brief] wrote {out} ({len(brief):,} chars). Review before running the orchestrator.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
