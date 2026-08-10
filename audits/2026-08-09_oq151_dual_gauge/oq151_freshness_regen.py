#!/usr/bin/env python3
"""OQ-151 audit: serialized five-leg freshness regeneration with md5 brackets.

Session driver (not committed as reusable tooling): regenerates the four
non-default leg outputs via run_pipeline.classify_corpus and appends the
md5-bracket lines to the audit log. The canonical testsets/ leg is
regenerated separately via run_pipeline.py (its gates apply there).
"""

import hashlib
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python"))
from run_pipeline import classify_corpus  # noqa: E402

AUDIT_LOG = REPO / "audits/2026-08-09_oq151_dual_gauge/audit_log.md"

LEGS = [
    ("testsets_haiku",  "pipeline_output.haiku.json",  "claude-haiku-4-5"),
    ("testsets_flash",  "pipeline_output.flash.json",  "gemini-2.5-flash"),
    ("testsets_kimi",   "pipeline_output.kimi.json",   "kimi-k2"),
    ("testsets_sonnet", "pipeline_output.sonnet.json", "claude-sonnet-5"),
]


def leg_fp(leg):
    files = sorted((REPO / "prolog" / leg).glob("*.pl"))
    h = hashlib.md5()
    for f in files:
        h.update(hashlib.md5(f.read_bytes()).hexdigest().encode())
    return f"{h.hexdigest()} ({len(files)} files)"


def log(line):
    with open(AUDIT_LOG, "a", encoding="utf-8") as f:
        f.write(line + "\n")
    print(line, flush=True)


def main():
    for leg, out, model in LEGS:
        before = leg_fp(leg)
        log(f"- {leg}: fp_before {before}")
        m = classify_corpus(leg, out, model)
        after = leg_fp(leg)
        log(f"- {leg}: fp_after  {after}  "
            f"{'FROZEN' if after == before else '*** MUTATED ***'}")
        log(f"- {leg}: manifest run_at={m['pipeline_run_at']} n={m['n_constraints']} "
            f"commit={m['code_commit_short']} dirty={m['code_dirty']}")
    print("DONE")


if __name__ == "__main__":
    main()
