#!/usr/bin/env python3
"""rqd_driver.py — run the RQ-d recognition-vs-enumeration arms.

Design obligations this driver is built against, each from a witnessed failure:

  * PERSIST THE RAW DATUM FIRST. The response text is the datum; scores are
    derived. Written to responses/ before any parsing, so a run that dies at
    call 40 leaves 40 recoverable units rather than zero.
  * WRITE-THEN-VERIFY PER UNIT. Each write is read back and asserted non-empty
    before the next call is issued. A mirror gate at the end is necessary but
    not sufficient: `captured == expected` passes when every file is empty.
  * COUNT FROM THE ARTIFACT, NEVER FROM THE LOOP. `len(results)` as a persisted
    count is a claim about persistence sourced from the thing that is not
    persistence. The output gate globs the directory.
  * --dry-run DOES WHAT IT SAYS. The predecessor's help text and console line
    both described a distinction the code never implemented, so a reader
    checking for persistence found two pieces of evidence that the question had
    been considered, and no persistence. Here --dry-run writes nothing and the
    output gate is what proves it.

The two prompts differ ONLY in the cognitive route demanded (read-and-confirm
versus write-the-receiver's-actions) and close with an IDENTICAL required
section, so the scored field is the same for both arms.
"""
import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(Path(__file__).resolve().parent))

from rqd_materials import SPECS  # noqa: E402

OUTDIR = ROOT / "audits" / "2026-08-13_rqd_recognition_vs_enumeration" / "responses"
MODEL = "claude-sonnet-5"
MAX_TOKENS = 2000

CLOSING = """

End your reply with exactly this section, with this exact heading:

PROBLEMS OR GAPS:
- one bullet for each problem or gap that would prevent this design from being
  executed correctly by someone who did not write it
- if there are none, write: None identified."""

RECOGNITION = """You are reviewing a design document. Read it and confirm whether it is \
complete and correct.

--- BEGIN DESIGN ---
{spec}
--- END DESIGN ---

Give your assessment of whether this design is complete and correct.""" + CLOSING

ENUMERATION = """You are receiving a design document as a handoff. Write the prompt that a \
receiving instance would need in order to EXECUTE this design: enumerate the concrete, \
numbered actions they must take, in order, so that someone who did not write the design \
could carry it out.

--- BEGIN DESIGN ---
{spec}
--- END DESIGN ---

Write that receiver prompt now, as a numbered list of concrete actions.""" + CLOSING

PROTOCOLS = {"recognition": RECOGNITION, "enumeration": ENUMERATION}


def unit_path(spec_id, protocol, rep):
    return OUTDIR / f"{spec_id}__{protocol}__r{rep}.json"


def units(reps, only=None):
    for spec in SPECS:
        if only and spec["id"] not in only:
            continue
        for protocol in PROTOCOLS:
            for rep in range(1, reps + 1):
                yield spec, protocol, rep


def output_gate(expected, dry_run):
    """Count and validate from the ARTIFACTS on disk, not from the loop."""
    files = sorted(OUTDIR.glob("*.json")) if OUTDIR.exists() else []
    print(f"\n[output gate] files on disk: {len(files)}  expected: {expected}")
    if dry_run:
        ok = len(files) == 0
        print(f"[output gate] --dry-run wrote nothing: {'YES' if ok else 'NO — LEAKED'}")
        return ok
    problems = []
    if len(files) != expected:
        problems.append(f"count mismatch: {len(files)} != {expected}")
    empty, unparsed, no_text = 0, 0, 0
    for f in files:
        raw = f.read_text()
        if not raw.strip():
            empty += 1
            continue
        try:
            obj = json.loads(raw)
        except json.JSONDecodeError:
            unparsed += 1
            continue
        if not (obj.get("response") or "").strip():
            no_text += 1
    if empty:
        problems.append(f"{empty} empty files")
    if unparsed:
        problems.append(f"{unparsed} unparseable files")
    if no_text:
        problems.append(f"{no_text} files with empty response text")
    print(f"[output gate] empty={empty} unparseable={unparsed} no_response_text={no_text}")
    if problems:
        print("[output gate] RED — " + "; ".join(problems))
        return False
    print("[output gate] GREEN — every unit on disk is non-empty and parses")
    return True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--reps", type=int, default=3)
    ap.add_argument("--only", nargs="*", help="restrict to these spec ids (pilot)")
    ap.add_argument("--dry-run", action="store_true",
                    help="build and print prompts; write NOTHING to responses/")
    args = ap.parse_args()

    todo = list(units(args.reps, args.only))
    print(f"units: {len(todo)}  model: {MODEL}  dry_run: {args.dry_run}")

    if not args.dry_run:
        OUTDIR.mkdir(parents=True, exist_ok=True)

    from agent.llm_call import call, ModelCallError

    issued = skipped = failed = 0
    for spec, protocol, rep in todo:
        p = unit_path(spec["id"], protocol, rep)
        if not args.dry_run and p.exists() and p.read_text().strip():
            skipped += 1
            continue
        prompt = PROTOCOLS[protocol].format(spec=spec["text"].strip())
        if args.dry_run:
            print(f"  [dry-run] {spec['id']:26} {protocol:12} r{rep}  "
                  f"prompt_chars={len(prompt)}")
            continue
        try:
            text, tin, tout = call(prompt, MODEL, max_tokens=MAX_TOKENS)
        except ModelCallError as exc:
            print(f"  FAILED {spec['id']} {protocol} r{rep}: {exc}")
            failed += 1
            continue
        # Persist the raw datum FIRST, then verify this unit before continuing.
        p.write_text(json.dumps({
            "spec_id": spec["id"], "protocol": protocol, "rep": rep,
            "model": MODEL, "tokens_in": tin, "tokens_out": tout,
            "response": text,
        }, indent=2))
        back = json.loads(p.read_text())
        assert back["response"].strip(), f"wrote empty response for {p.name}"
        issued += 1
        print(f"  ok {spec['id']:26} {protocol:12} r{rep}  "
              f"out={tout:4}  chars={len(text)}")

    print(f"\nissued={issued} skipped={skipped} failed={failed}")
    expected = 0 if args.dry_run else len(todo) - failed
    return 0 if output_gate(expected, args.dry_run) else 1


if __name__ == "__main__":
    sys.exit(main())
