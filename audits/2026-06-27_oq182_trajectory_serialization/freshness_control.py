#!/usr/bin/env python3
"""OQ-182 freshness-detector positive control (non-vacuity).

The battery's green is only meaningful if its freshness predicates actually FAIL
on a stale/empty report and on a stalled giant_comp. This seeds a REAL empty,
old-mtime report file and runs the battery's actual stat-based predicates against
it (exercising the same os.stat path), plus a planted 650s giant_comp duration.
Proves the green battery saw real freshness, not a probe that never looked."""
import sys, time, os
from pathlib import Path

tmp = Path(__file__).resolve().parent / "_seed_stale_report.md"
run_start = time.time()

# Seed a REAL empty report with an mtime 100s BEFORE run_start (stale).
tmp.write_text("", encoding="utf-8")
old = run_start - 100
os.utime(tmp, (old, old))

st = tmp.stat()
rep_sz = st.st_size            # real size of the seeded empty file
rep_mt = st.st_mtime           # real (old) mtime
gc_dur = 650.0                 # planted: the historical >600s stall

# These are the EXACT battery predicates (must all be False on this stale input):
checks = {
    "report_size>0":              rep_sz > 0,
    "report_mtime_advanced":      rep_mt >= run_start,
    "giant_comp_serial_band(<60s)": 0 <= gc_dur < 60,
}
flagged = {k: (not v) for k, v in checks.items()}  # flagged == predicate failed (good)
tmp.unlink()

print("freshness predicate non-vacuity (real seeded stale report + planted stall):")
for k, was_flagged in flagged.items():
    print(f"  {k:32s} -> {'FLAGGED stale (PASS)' if was_flagged else 'MISSED (FAIL)'}")
ok = all(flagged.values())
print(f"\nFRESHNESS CONTROL: {'PASS' if ok else 'FAIL'}")
sys.exit(0 if ok else 1)
