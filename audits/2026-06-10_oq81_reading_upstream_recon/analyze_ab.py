#!/usr/bin/env python3
"""OQ-81 A/B analysis — applies the AB_PLAN.md pre-registered metrics to ab_runs/.

Metric 1/2: claimed_type per rep + within-arm agreement.
Metric 3: scalar between-arm shift vs within-arm spread.
Metric 4: husk-frame vocabulary leakage in narrative text (counted, with hit terms).
"""
import json, re
from pathlib import Path
from statistics import mean

HERE = Path(__file__).resolve().parent
RUNS = HERE / "ab_runs"

# Husk-reading frame vocabulary (from the manifest's husk_reading commitment +
# expected_structural_delta), minus terms the AXIS spec itself already contains
# (trust/infrastructure/preparedness are axis words — not leakage evidence).
HUSK_TERMS = ["memorial", "ritual", "husk", "institutional memory", "atroph",
              "performance", "bureaucratic continuity", "ceremon"]
# Direct upstream-token leakage: the injected verdict and id appearing in prose.
UPSTREAM_TOKENS = ["tangled_rope", "husk_reading"]

rows = json.loads((RUNS / "summary.json").read_text())

# run_ab.py extracted base_extractiveness/suppression_requirement, which are NOT the
# schema's field names (extractiveness/suppression) — re-extract from the saved stories.
for r in rows:
    p = RUNS / f"{r['tag']}.story.json"
    if p.exists():
        bp = json.loads(p.read_text()).get("base_properties", {})
        r["extractiveness"] = bp.get("extractiveness")
        r["suppression"] = bp.get("suppression")
        r["theater_ratio"] = bp.get("theater_ratio")

def story_text(tag):
    p = RUNS / f"{tag}.story.json"
    if not p.exists():
        return ""
    s = json.loads(p.read_text())
    # All prose-bearing fields, flattened.
    return json.dumps(s, ensure_ascii=False).lower()

print("=== Metric 1/2: claimed_type per rep ===")
by_arm = {}
for r in rows:
    by_arm.setdefault(r["arm"], []).append(r)
for arm, rs in by_arm.items():
    types = [r["claimed_type"] for r in sorted(rs, key=lambda x: x["rep"])]
    agree = max(types.count(t) for t in set(types) if t is not None) if any(types) else 0
    print(f"  Arm {arm}: {types}  within-arm agreement: {agree}/{len(types)}")

print("\n=== Metric 3: scalars (mean [min..max] per arm) ===")
for field in ("extractiveness", "suppression", "theater_ratio"):
    line = f"  {field}: "
    for arm in ("N", "R", "K"):
        vals = [r[field] for r in by_arm.get(arm, []) if isinstance(r[field], (int, float))]
        line += (f"{arm}={mean(vals):.3f}[{min(vals):.2f}..{max(vals):.2f}]  "
                 if vals else f"{arm}=NONE  ")
    print(line)

print("\n=== Metric 4: husk-frame vocabulary + upstream-token leakage ===")
for r in sorted(rows, key=lambda x: (x["arm"], x["rep"])):
    text = story_text(r["tag"])
    hits = {t: text.count(t) for t in HUSK_TERMS if t in text}
    toks = {t: text.count(t) for t in UPSTREAM_TOKENS if t in text}
    print(f"  {r['tag']}: husk-frame hits={hits or '{}'}  upstream-tokens={toks or '{}'}")

print("\n=== Run health ===")
for r in rows:
    flag = "" if r["parse_ok"] and not r["schema_errors"] else "  <-- CHECK"
    print(f"  {r['tag']}: parse={r['parse_ok']} schema_errors={len(r['schema_errors'])} "
          f"stop={r['stop_reason']} in={r['input_tokens']} out={r['output_tokens']}{flag}")
