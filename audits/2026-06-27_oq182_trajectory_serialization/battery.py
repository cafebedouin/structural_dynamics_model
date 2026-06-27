#!/usr/bin/env python3
"""OQ-182 liveness battery: N post-fix flag=1 pipeline runs, each asserting the
A4 freshness criterion. Cheap insurance against 'the fix did nothing'."""
import re, subprocess, sys, time, os
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "outputs"
AUD = Path(__file__).resolve().parent
N = int(sys.argv[1]) if len(sys.argv) > 1 else 10
STEP_RE = re.compile(r"^\s+(\w+)\s+(ok|error)\s+\[([\d.]+)s\]", re.MULTILINE)
TOTAL_RE = re.compile(r"Total time:\s+([\d.]+)s")

report = OUT / "context_profile_report.md"
pout = OUT / "pipeline_output.json"
results = []
for i in range(1, N + 1):
    run_start = time.time()
    p = subprocess.run(["python3", "python/run_pipeline.py"], cwd=str(REPO),
                       capture_output=True, text=True, timeout=900)
    wall = time.time() - run_start
    steps = {m.group(1): (m.group(2), float(m.group(3))) for m in STEP_RE.finditer(p.stdout)}
    tot = TOTAL_RE.search(p.stdout)
    rep_mt = report.stat().st_mtime if report.exists() else 0
    rep_sz = report.stat().st_size if report.exists() else 0
    pout_mt = pout.stat().st_mtime if pout.exists() else 0
    gc_status, gc_dur = steps.get("giant_comp", ("MISSING", -1))
    tr_status, tr_dur = steps.get("trajectory", ("MISSING", -1))
    n_error = sum(1 for s, _ in steps.values() if s == "error")
    checks = {
        "exit0": p.returncode == 0,
        "no_error_steps": n_error == 0,
        "trajectory_ok": tr_status == "ok",
        "giant_comp_ok": gc_status == "ok",
        "giant_comp_serial_band(<60s)": 0 <= gc_dur < 60,
        "wall_normal(<600s)": wall < 600,
        "report_mtime_advanced": rep_mt >= run_start,
        "report_size>0": rep_sz > 0,
        "pout_mtime_advanced": pout_mt >= run_start,
    }
    green = all(checks.values())
    results.append(green)
    failed = [k for k, v in checks.items() if not v]
    print(f"run {i:2d}: {'GREEN' if green else 'RED'}  wall={wall:.1f}s "
          f"gc={gc_dur:.1f}s/{gc_status} tr={tr_dur:.1f}s/{tr_status} "
          f"rep_sz={rep_sz}  {'' if green else 'FAIL:'+','.join(failed)}")
    sys.stdout.flush()

n_green = sum(results)
print(f"\nBATTERY: {n_green}/{N} GREEN  ->  {'PASS' if n_green == N else 'FAIL'}")
sys.exit(0 if n_green == N else 1)
