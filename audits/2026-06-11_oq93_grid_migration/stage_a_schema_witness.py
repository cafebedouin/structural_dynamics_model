#!/usr/bin/env python3
"""Stage A witness — OQ-93 coercion_grid block (+ rider OQ-102(a) basis, when present).

Two parts:
  1. Guard battery: valid/negative cases, each negative required to bite at its
     INTENDED guard (error path checked, not just invalidity).
  2. Two-sided additivity sweep over ALL existing json/: every file's error set
     under the OLD schema (git show <base>:schemas/...) must be IDENTICAL under
     the NEW schema — pre-existing invalids stay invalid identically, zero new
     failures (the OQ-92 Stage-A pattern).

Validator: Draft7 (the pipeline's actual validator, jsonschema 3.2.0).
Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/stage_a_schema_witness.py <old_schema_ref>
where <old_schema_ref> is a git rev holding the pre-change schema (default: HEAD).
"""
import copy
import json
import subprocess
import sys
from pathlib import Path

from jsonschema import Draft7Validator

ROOT = Path(__file__).resolve().parents[2]
SCHEMA = json.load(open(ROOT / "schemas/constraint_story_schema.json"))
BASE = json.load(open(ROOT / "json/demographic_skill_mismatch.json"))
V = Draft7Validator(SCHEMA)

results = []


def err_path(e):
    return "$." + ".".join(str(p) for p in e.absolute_path)


def check(label, doc, expect_valid, must_mention=None):
    errs = list(V.iter_errors(doc))
    valid = not errs
    ok = valid == expect_valid
    detail = ""
    if not expect_valid:
        if valid:
            ok = False
            detail = "EXPECTED INVALID but validated"
        else:
            joined = " | ".join(err_path(e) + ": " + e.message[:90] for e in errs)
            if must_mention and not any(must_mention in err_path(e) + e.message for e in errs):
                ok = False
                detail = f"invalid but NOT at intended guard ({must_mention}); got: {joined}"
            else:
                detail = f"bit at intended guard: {joined[:160]}"
    results.append((label, "PASS" if ok else "FAIL", detail))
    return ok


GRID_METRICS = ["accessibility_collapse", "stakes_inflation", "suppression", "resistance"]
LEVELS = ["structural", "organizational", "class", "individual"]


def full_grid(t0=0, tn=10, lo=0.2, hi=0.8):
    pts = []
    for lv in LEVELS:
        for m in GRID_METRICS:
            pts.append({"metric": m, "level": lv, "time_point": t0, "value": lo})
            pts.append({"metric": m, "level": lv, "time_point": tn, "value": hi})
    return {"t0": t0, "tn": tn, "points": pts}


# --- Part 1: guard battery -------------------------------------------------

d = copy.deepcopy(BASE); d["coercion_grid"] = full_grid()
check("1 full 32-point grid", d, True)

d = copy.deepcopy(BASE)
g = full_grid(); g["points"] = [p for p in g["points"] if p["level"] == "structural"]
d["coercion_grid"] = g
check("2 partial grid (8/32, one level) — legal at schema level", d, True)

d = copy.deepcopy(BASE); d.pop("coercion_grid", None)
check("3 absent block (back-compat, fail-closed downstream)", d, True)

d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["level"] = "cosmic"
d["coercion_grid"] = g
check("4 out-of-enum level ('cosmic')", d, False, must_mention="level")

d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["metric"] = "vibes"
d["coercion_grid"] = g
check("5 out-of-enum grid metric ('vibes')", d, False, must_mention="metric")

# Surfaces stay disjoint in BOTH directions (do NOT extend MeasurementMetric)
d = copy.deepcopy(BASE)
d.setdefault("measurements", []).append({"metric": "suppression", "time_point": 0, "value": 0.5})
check("6 grid metric in scalar measurements[] rejected (disjoint surfaces)",
      d, False, must_mention="metric")

d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["metric"] = "theater_ratio"
d["coercion_grid"] = g
check("7 scalar metric in grid rejected (disjoint surfaces)", d, False, must_mention="metric")

d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["value"] = 1.5
d["coercion_grid"] = g
check("8 value out of [0,1]", d, False, must_mention="value")

d = copy.deepcopy(BASE); g = full_grid(); del g["t0"]
d["coercion_grid"] = g
check("9 missing t0", d, False, must_mention="t0")

d = copy.deepcopy(BASE); d["coercion_grid"] = {"t0": 0, "tn": 10, "points": []}
check("10 empty points array (minItems 1)", d, False, must_mention="points")

d = copy.deepcopy(BASE); g = full_grid()
g["points"].append(dict(g["points"][0]))  # exact duplicate slot
g["points"].pop(1)  # keep length <= 32
d["coercion_grid"] = g
check("11 duplicate (metric,level,time) slot is schema-VALID BY DESIGN (compiler's job, Stage B)",
      d, True)

d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["time_point"] = 5
d["coercion_grid"] = g
check("12 time_point outside {t0,tn} is schema-VALID BY DESIGN (compiler's job, Stage B)",
      d, True)

d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["surprise"] = True
d["coercion_grid"] = g
check("13 extra property on grid point", d, False, must_mention="surprise")

# Rider OQ-102(a), present only after the rider commit: basis on Measurement + grid points
HAS_BASIS = "basis" in SCHEMA["$defs"]["Measurement"]["properties"] if \
    "properties" in SCHEMA["$defs"]["Measurement"] else False
if HAS_BASIS:
    d = copy.deepcopy(BASE)
    d.setdefault("measurements", []).append(
        {"metric": "theater_ratio", "time_point": 0, "value": 0.5, "basis": "projected"})
    check("R1 basis=projected on scalar measurement", d, True)

    d = copy.deepcopy(BASE)
    d.setdefault("measurements", []).append(
        {"metric": "theater_ratio", "time_point": 0, "value": 0.5, "basis": "guessed"})
    check("R2 basis bad enum ('guessed')", d, False, must_mention="basis")

    d = copy.deepcopy(BASE); g = full_grid(); g["points"][0]["basis"] = "observed"
    d["coercion_grid"] = g
    check("R3 basis=observed on grid point", d, True)
else:
    print("  (rider basis field not present in schema yet — R-cases skipped)")

# --- Part 2: two-sided additivity sweep ------------------------------------

old_ref = sys.argv[1] if len(sys.argv) > 1 else "HEAD"
old_text = subprocess.run(
    ["git", "show", f"{old_ref}:schemas/constraint_story_schema.json"],
    cwd=ROOT, capture_output=True, text=True, check=True).stdout
V_OLD = Draft7Validator(json.loads(old_text))

new_fail, delta = 0, []
files = sorted((ROOT / "json").glob("*.json"))
for f in files:
    doc = json.load(open(f))
    e_old = sorted(err_path(e) + ": " + e.message for e in V_OLD.iter_errors(doc))
    e_new = sorted(err_path(e) + ": " + e.message for e in V.iter_errors(doc))
    if e_old != e_new:
        delta.append((f.name, e_old, e_new))
    if e_new and not e_old:
        new_fail += 1
n_invalid_old = sum(1 for f in files if list(V_OLD.iter_errors(json.load(open(f)))))

print(f"base story: json/demographic_skill_mismatch.json   old schema ref: {old_ref}")
for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} battery cases pass")
print(f"additivity sweep: {len(files)} files; pre-existing invalid under OLD schema: "
      f"{n_invalid_old}; error-set deltas old->new: {len(delta)}; NEW failures: {new_fail}")
for name, eo, en in delta[:10]:
    print(f"  DELTA {name}: old={eo} new={en}")
sys.exit(1 if (n_fail or delta or new_fail) else 0)
