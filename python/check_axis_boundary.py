#!/usr/bin/env python3
"""check_axis_boundary.py — fail loud on UNEXPECTED committer→observer reads (OQ-15 / OQ-135).

The invariant this guards (v8 §8 item 1, LOAD-BEARING): no committer field
reaches observer computation by any path except as the entailment-typed payload
on the single forward bridge `influences` → detect_necessity_inheritance. This is
a *reachability* property, not a count or import grep (v8 calls those a trap; W1
confirmed grep is blind — cs_drift_mismatch reaches observer machinery
transitively, audit 2026-06-23_oq15_crossaxis_witnesses/).

The reachability core is prolog/check_axis_boundary.pl: it walks the LOADED call
graph (clause/2 over every engine-defined predicate's bodies, descending through
control constructs and meta-calls) and emits one `AXIS_EDGE:` line per
committer→observer boundary-crossing edge. Any transitive observer→committer path
crosses the boundary at exactly one such last-hop edge, so the direct-edge set is
complete for the guarded direction.

This harness mirrors load_warning_gate.py: collect the edges, diff against the
allowlist (prolog/axis_boundary_allowlist.txt), and:
  - any edge NOT allowlisted          -> exit 1, listed (a new cross-axis read)
  - allowlisted edge no longer present -> note printed (stale entry; exit 0)

Usage:
    python3 python/check_axis_boundary.py            # gate (exit 1 on unexpected)
    python3 python/check_axis_boundary.py --baseline # print current edge set
    python3 python/check_axis_boundary.py --allowlist PATH   # alternate allowlist
                                                     # (used by the positive controls)
"""
import argparse
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PROLOG = ROOT / "prolog"
ALLOWLIST = PROLOG / "axis_boundary_allowlist.txt"
PROBE = "check_axis_boundary.pl"


def collect_edges(probe=PROBE):
    """Run the reachability probe; return the sorted set of boundary edges.

    `probe` lets the positive controls point at a wrapper that loads the real
    check_axis_boundary.pl plus a planted cross-axis read — same harness, same
    allowlist, so a planted violation must drive exit 1 end-to-end."""
    proc = subprocess.run(
        ["swipl", "-l", probe, "-g", "run_axis_boundary, halt", "-t", "halt(1)"],
        cwd=PROLOG, capture_output=True, text=True, timeout=300,
    )
    edges = []
    for line in proc.stdout.splitlines():
        line = line.strip()
        if line.startswith("AXIS_EDGE:"):
            edges.append(line[len("AXIS_EDGE:"):].strip())
    if proc.returncode != 0 and not edges:
        # the probe itself failed to run — fail closed, surface stderr
        print(proc.stderr, file=sys.stderr)
        print("[AXIS-GATE] reachability probe did not run cleanly", file=sys.stderr)
    return sorted(set(edges)), proc.returncode


def load_allowlist(path):
    allowed = set()
    p = Path(path)
    if p.exists():
        for ln in p.read_text().splitlines():
            ln = ln.strip()
            if ln and not ln.startswith("#"):
                allowed.add(ln)
    return allowed


def _gate(probe, allowlist=str(ALLOWLIST)):
    """Return (n_unexpected, exit_code) for one gate run — used by selftest."""
    edges, rc = collect_edges(probe)
    allowed = load_allowlist(allowlist)
    unexpected = [e for e in edges if e not in allowed]
    code = 1 if (rc != 0 and not edges) else (1 if unexpected else 0)
    return unexpected, code


def selftest():
    """Negative case passes; BOTH planted-violation controls fire. The clean
    pass is only trusted because these show the guard WOULD flag a violation
    (path b: count-blind payload widening; path c: per-bridge-blind seam)."""
    cases = [
        ("negative (clean corpus)",        PROBE,                            0),
        ("control1 payload-widen (path b)", "tests/axis_boundary_ctl_run1.pl", 1),
        ("control2 nonbridge-seam (path c)", "tests/axis_boundary_ctl_run2.pl", 1),
    ]
    ok = True
    for label, probe, want in cases:
        unexpected, code = _gate(probe)
        verdict = "PASS" if code == want else "FAIL"
        if code != want:
            ok = False
        detail = (" :: " + "; ".join(unexpected)) if unexpected else ""
        print(f"[AXIS-SELFTEST] {verdict}  {label}: exit {code} (want {want}){detail}")
    print(f"[AXIS-SELFTEST] {'ALL PASS' if ok else 'FAILURES PRESENT'}")
    return 0 if ok else 1


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--baseline", action="store_true",
                    help="print current boundary-edge set and exit 0")
    ap.add_argument("--allowlist", default=str(ALLOWLIST))
    ap.add_argument("--probe", default=PROBE,
                    help="alternate probe entry file (positive-control wrappers)")
    ap.add_argument("--selftest", action="store_true",
                    help="run negative case + both planted-violation positive controls")
    args = ap.parse_args()

    if args.selftest:
        return selftest()

    edges, rc = collect_edges(args.probe)
    if args.baseline:
        for e in edges:
            print(e)
        return 0

    allowed = load_allowlist(args.allowlist)
    unexpected = [e for e in edges if e not in allowed]
    stale = sorted(allowed - set(edges))

    for e in unexpected:
        print(f"[AXIS-GATE] UNEXPECTED committer→observer read: {e}", file=sys.stderr)
    for e in stale:
        print(f"[AXIS-GATE] stale allowlist entry (edge gone — prune it): {e}")
    print(f"[AXIS-GATE] {len(edges)} boundary edges: "
          f"{len(edges) - len(unexpected)} allowlisted, {len(unexpected)} unexpected")
    # fail closed if the probe could not run at all
    if rc != 0 and not edges:
        return 1
    return 1 if unexpected else 0


if __name__ == "__main__":
    sys.exit(main())
