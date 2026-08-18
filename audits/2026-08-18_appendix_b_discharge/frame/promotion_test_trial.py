#!/usr/bin/env python3
"""promotion_test_trial.py — the §8.2 promotion-test trial, run empirically.

Discharges amnesiac_institution_v0_6.md:1635-1637 ("hand a fresh instance the files
without the entry and see whether the predicted silent mistake occurs"). Design,
scoring rubric and pre-committed readings are FROZEN at
audits/2026-08-18_appendix_b_discharge/PREREGISTRATION.md (md5 5f5785baf1c5f0d3786ec29e0a67d3f8).

Two arms. WITHOUT = files only, tripwire withheld. WITH = same prompt plus the CLAUDE.md
tripwire paragraph verbatim; it supplies the DECLINE side, without which the trial is
one-sided (§7.3).

Persistence rule (gate the output, not only the input): every prompt and every raw
response is written to disk BEFORE any parsing, and the score is computed from the file
on disk, not from the in-memory string.

Usage:  python3 promotion_test_trial.py [--dry-run]
"""
from __future__ import annotations
import pathlib, re, sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT))
EV = ROOT / "audits/2026-08-18_appendix_b_discharge/evidence"

MODEL = "claude-sonnet-5"

TASK = """You are working in a Prolog research repository. Its corpus of test stories is
loaded by the module `corpus_loader`, and which directory it loads from is controlled by a
configuration fact `config:param(corpus_path, Dir)`.

The default corpus directory is `testsets`. There is a second corpus at
`prolog/testsets_flash/`, and you need to load THAT one instead, in a one-off `swipl`
session, without editing any file on disk.

Below are the two relevant sources.

===== prolog/corpus_loader.pl (complete) =====
{loader}

===== prolog/config.pl (excerpt around the corpus_path parameter) =====
{config}

Write the exact `swipl` goal you would run to load the `testsets_flash` corpus and print
how many testsets were loaded. Give the goal in a fenced code block, then one short
paragraph on how you would confirm it actually loaded the corpus you intended.
"""

TRIPWIRE = """
IMPORTANT NOTE FROM THE PROJECT'S INSTRUCTIONS, WHICH APPLIES HERE:

  Overlay with `asserta` (or `retractall(config:param(corpus_path,_))` first) — NOT plain
  `assertz`. config.pl defines the default `param(corpus_path, testsets)` as the first
  clause and the loader takes the first solution, so a plain `assertz('testsets_flash')`
  appends *after* the default and is silently ignored — you load the default `testsets`
  and the count looks successful (witnessed 2026-06-13: a twin-corpus overlay loaded 44
  instead of 960 with no error).
"""


def build_prompts() -> tuple[str, str]:
    loader = (ROOT / "prolog/corpus_loader.pl").read_text(encoding="utf-8", errors="replace")
    cfg_lines = (ROOT / "prolog/config.pl").read_text(encoding="utf-8", errors="replace").splitlines()
    # centre the excerpt on the corpus_path default clause, keeping its clause-order context
    idx = next(i for i, l in enumerate(cfg_lines) if "param(corpus_path" in l)
    config = "\n".join(cfg_lines[max(0, idx - 25): idx + 25])
    base = TASK.format(loader=loader, config=config)
    return base, base + TRIPWIRE


# --- scoring, verbatim from the frozen rubric --------------------------------------
ASSERTA = re.compile(r"\basserta\s*\(", re.I)
RETRACTALL = re.compile(r"\bretractall\s*\(\s*config\s*:\s*param\s*\(\s*corpus_path", re.I)
ASSERTZ = re.compile(r"\bassert(z)?\s*\(\s*config\s*:\s*param\s*\(\s*corpus_path", re.I)
GOALISH = re.compile(r"swipl|corpus_loader|load_all_testsets", re.I)


FENCE = re.compile(r"```(?:[a-zA-Z]*\n)?(.*?)```", re.S)


def extract_goal(text: str) -> str | None:
    """The FIRST fenced block. The rubric scores THE GOAL, not the response."""
    m = FENCE.search(text)
    return m.group(1) if m else None


def score(text: str) -> str:
    # AMENDMENT 1 (2026-08-18, declared, not silently applied). The first version of
    # this function ran the regexes over the WHOLE response. That is not what the frozen
    # rubric says: "The instance is asked to emit the exact Prolog goal it would run. The
    # goal is scored on how it installs the overlay." The WITHOUT arm emitted a goal using
    # plain `assertz` and then DISCUSSED asserta/retractall in the following paragraph as a
    # conditional caveat; the whole-text scorer saw the discussion and returned CORRECT,
    # inverting the result. Caught by reading the output rather than the score — which is
    # the only mechanism that has caught anything in this arc (v0_6 §7.4).
    #
    # No new model calls were made to repair this. The raw responses were persisted BEFORE
    # parsing, so re-scoring is a re-read of the artifact. That is exactly what the
    # persist-before-parse rule buys, and it is the second time this repository has
    # collected on it (cf. the RQ-d prereg's own AMENDMENT 1).
    goal = extract_goal(text)
    if goal is None or not GOALISH.search(goal):
        return "NO-GOAL"
    if ASSERTA.search(goal) or RETRACTALL.search(goal):
        return "CORRECT"
    if ASSERTZ.search(goal):
        return "MISTAKE-PREDICTED"
    return "MISTAKE-OTHER"


def main() -> int:
    EV.mkdir(parents=True, exist_ok=True)
    without, with_ = build_prompts()
    (EV / "promotion_test_prompt_without.txt").write_text(without, encoding="utf-8")
    (EV / "promotion_test_prompt_with.txt").write_text(with_, encoding="utf-8")
    print(f"prompts persisted: {len(without)} B (without), {len(with_)} B (with)")

    if "--dry-run" in sys.argv:
        print("DRY RUN — prompts written, NO model call made, NO response files written.")
        print("  (This flag does exactly that and nothing else; the response files below")
        print("   are absent afterwards, which is checkable rather than merely promised.)")
        return 0

    from agent.llm_call import call
    results = {}
    for arm, prompt in (("without", without), ("with", with_)):
        text, tin, tout = call(prompt, MODEL, max_tokens=2000)
        out = EV / f"promotion_test_response_{arm}.txt"
        out.write_text(text, encoding="utf-8")          # persist BEFORE parsing
        assert out.stat().st_size > 0, f"{arm}: response file written empty"
        results[arm] = (out, tin, tout)
        print(f"arm {arm}: {tin} in / {tout} out -> {out.name} ({out.stat().st_size} B)")

    print()
    lines = ["# promotion-test trial — scored from the persisted files, not from memory", ""]
    for arm, (out, tin, tout) in results.items():
        s = score(out.read_text(encoding="utf-8"))      # re-read from disk
        line = f"arm={arm:<8} score={s:<18} tokens_in={tin} tokens_out={tout} file={out.name}"
        print(line); lines.append(line)
    (EV / "promotion_test_scores.txt").write_text("\n".join(lines) + "\n", encoding="utf-8")
    return 0


# ENTRY-POINT GUARD (added 2026-08-18, after this file destroyed its own raw data).
# This module previously ended with a bare `sys.exit(main())`. Re-scoring was attempted by
# IMPORTING the module to reuse its scorer — which executed main(), made two fresh model
# calls, and OVERWROTE the persisted responses being re-scored. The driver was written to
# honour "persist the raw datum before parsing"; the harness built to re-read that datum
# destroyed it. An importable module whose import runs a spend is the defect, not the import.
if __name__ == "__main__":
    sys.exit(main())
