#!/usr/bin/env python3
"""OQ-182 C0 re-witness: classification-field diff between flag=0 baseline and a
post-fix flag=1 enabled pipeline_output.json. Expect ZERO classification diff
(the fix changes *when* trajectory runs, not *what* it writes).

Compares the classification-bearing sections (per_constraint, diagnostic,
validation, type_hierarchy). Excludes `manifest` (run-varying timestamps) and
`config` (carries the expected trajectory_enabled 0->1 echo, not a classification
effect). Includes a positive control proving the diff has teeth."""
import json, sys, copy
from pathlib import Path

AUD = Path(__file__).resolve().parent
base = json.load(open(AUD / "c0_pipeline_baseline.json"))
enab = json.load(open(AUD / "c0_pipeline_enabled.json"))

SECTIONS = ["per_constraint", "diagnostic", "validation", "type_hierarchy"]

def classification_view(d):
    return {k: d.get(k) for k in SECTIONS}

bv, ev = classification_view(base), classification_view(enab)

# Sanity: config echo really did flip (proves we compared a flag=1 output, not a stale dup)
b_flag = base.get("config", {}).get("trajectory_enabled")
e_flag = enab.get("config", {}).get("trajectory_enabled")
print(f"config.trajectory_enabled: baseline={b_flag}  enabled={e_flag}  "
      f"(expected 0 -> 1; proves the enabled output is genuinely flag=1)")

diff = bv != ev
print(f"\nclassification-field diff (per_constraint/diagnostic/validation/type_hierarchy): "
      f"{'NON-ZERO (FAIL)' if diff else 'ZERO (PASS)'}")
if diff:
    for s in SECTIONS:
        if bv[s] != ev[s]:
            print(f"  section differs: {s}")

# Positive control: plant a single classification field change, confirm caught
ev_planted = copy.deepcopy(ev)
ev_planted["per_constraint"][0]["claimed_type"] = "__PLANTED__"
caught = bv != ev_planted
print(f"positive control (plant claimed_type on entry 0): "
      f"{'CAUGHT (PASS)' if caught else 'MISSED (FAIL)'}")

ok = (not diff) and caught and (b_flag == 0) and (e_flag == 1)
print(f"\nC0 RE-WITNESS: {'PASS' if ok else 'FAIL'}")
sys.exit(0 if ok else 1)
