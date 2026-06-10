#!/usr/bin/env python3
"""load_warning_gate.py — fail loud on UNEXPECTED swipl load warnings (OQ-96).

The defect class this guards: a load-time warning (e.g. `source_sink
'domain_registry' does not exist`, 2026-02 to 2026-06) that every ad-hoc
`grep -v Warning` filter hides, until the dead reference is reached at runtime
months later. The gate loads the stack, captures stderr, normalizes warning
records, and compares against the allowlist of KNOWN-benign warnings
(prolog/load_warning_allowlist.txt):

  - any warning NOT in the allowlist  -> exit 1, listed (the gate's job)
  - allowlisted warning no longer present -> note printed (stale entry; exit 0)

Usage:
    python3 python/load_warning_gate.py            # gate (exit 1 on unexpected)
    python3 python/load_warning_gate.py --baseline # print current normalized set
    python3 python/load_warning_gate.py --allowlist PATH   # alternate allowlist
                                                   # (used by the negative control)

Wired into run_pipeline.py beside the ISSUES-gate: a checker that isn't run
isn't checking.
"""
import argparse
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PROLOG = ROOT / "prolog"
ALLOWLIST = PROLOG / "load_warning_allowlist.txt"


def collect_warnings():
    """Load the stack; return normalized warning records (one string each)."""
    proc = subprocess.run(
        ["swipl", "-g", "[stack], halt", "-t", "halt(1)"],
        cwd=PROLOG, capture_output=True, text=True, timeout=300,
    )
    records, pending_loc = [], None
    for line in proc.stderr.splitlines():
        m = re.match(r"^(Warning|ERROR): (.*)$", line)
        if not m:
            continue
        body = m.group(2).strip()
        # location lines end with ':' and carry a path; message lines follow
        if body.endswith(":") and ("/" in body or ".pl" in body):
            # normalize: strip everything before 'prolog/' so worktrees match
            pending_loc = re.sub(r"^.*?/prolog/", "prolog/", body).rstrip(":")
        else:
            loc = pending_loc or "<no-location>"
            records.append(f"{loc} :: {body}")
            pending_loc = None
    if proc.returncode != 0:
        records.append(f"<load-exit> :: stack load exited {proc.returncode}")
    return records


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--baseline", action="store_true",
                    help="print current normalized warning set and exit 0")
    ap.add_argument("--allowlist", default=str(ALLOWLIST))
    args = ap.parse_args()

    records = collect_warnings()
    if args.baseline:
        for r in records:
            print(r)
        return 0

    allow_path = Path(args.allowlist)
    allowed = set()
    if allow_path.exists():
        allowed = {ln.strip() for ln in allow_path.read_text().splitlines()
                   if ln.strip() and not ln.startswith("#")}

    unexpected = [r for r in records if r not in allowed]
    stale = sorted(allowed - set(records))

    for r in unexpected:
        print(f"[WARNING-GATE] UNEXPECTED: {r}", file=sys.stderr)
    for r in stale:
        print(f"[WARNING-GATE] stale allowlist entry (warning gone — prune it): {r}")
    print(f"[WARNING-GATE] {len(records)} load warnings: "
          f"{len(records) - len(unexpected)} allowlisted, {len(unexpected)} unexpected")
    return 1 if unexpected else 0


if __name__ == "__main__":
    sys.exit(main())
