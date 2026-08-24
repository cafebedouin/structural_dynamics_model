"""How many live legs can giant_component_analysis actually run on?

Positive control built in: `testsets` (the default leg) is the ONE corpus this
stage has ever run on inside run_pipeline, so it MUST come back OK. If it does
not, the harness is wrong, not the legs.
"""
import sys, subprocess, time
sys.path.insert(0, "python")
import run_pipeline as R
from shared.corpus_legs import LIVE_LEGS

LEGS = ["testsets"] + [l for l in LIVE_LEGS if l != "testsets"] + ["archives/datasets/original_v6"]
for leg in LEGS:
    d = R._resolve_corpus_dir(leg)
    n = len(list(d.glob("*.pl"))) if d.exists() else 0
    if n == 0:
        print(f"{leg:32s} n=0     SKIP (absent)"); continue
    goal = (f"{R._leg_overlay(leg)}"
            "catch_with_backtrace(run_giant_component_analysis, E, "
            "(print_message(error,E), halt(2)))")
    t0 = time.time()
    p = subprocess.run(["swipl", "-l", "stack.pl", "-l", "giant_component_analysis.pl",
                        "-g", goal + ", halt."], cwd=str(R.PROLOG_DIR),
                       capture_output=True, text=True, timeout=2400)
    err = ""
    for line in p.stderr.splitlines():
        if "not a function" in line or "ERROR" in line:
            err = line.strip()[:80]; break
    # last section header reached, to localize
    sec = [l for l in p.stdout.splitlines() if l.startswith("###")]
    print(f"{leg:32s} n={n:<6} {time.time()-t0:6.1f}s  rc={p.returncode}  "
          f"{'OK' if p.returncode==0 else 'THROW'}  last_section={sec[-1][:44] if sec else '-'}")
    if err: print(f"{'':32s}   {err}")
