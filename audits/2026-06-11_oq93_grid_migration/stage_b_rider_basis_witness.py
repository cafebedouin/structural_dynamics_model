#!/usr/bin/env python3
"""Stage B rider witness — OQ-102(a) basis firing chain, compiler half.

Nothing in the live corpus authors `basis`, so the rider owes a CONSTRUCTED
fixture (preregistration: landed-as-code-never-fired is the rot class this
prevents). Chain witnessed here:
  fixture JSON (basis: projected on a scalar point; basis: observed on a grid
  point) -> compiler emits measurement_basis/2 beside the same MID ->
  diagnostic_summary:measurement_provenance/2 shows a NONZERO projected bucket.
Plus: 0-diff sweep vs the Stage-B-core commit (basis-free corpus must be
byte-identical under the rider).

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/stage_b_rider_basis_witness.py <core_ref>
"""
import copy
import importlib.util
import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "audits/2026-06-11_oq93_grid_migration"
core_ref = sys.argv[1] if len(sys.argv) > 1 else "HEAD"

sys.path.insert(0, str(ROOT / "python"))
import generate_constraint_pl as new_mod

old_src = subprocess.run(
    ["git", "show", f"{core_ref}:python/generate_constraint_pl.py"],
    cwd=ROOT, capture_output=True, text=True, check=True).stdout
with tempfile.NamedTemporaryFile("w", suffix="_core_gen.py", delete=False) as tf:
    tf.write(old_src)
spec = importlib.util.spec_from_file_location("core_gen", tf.name)
old_mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(old_mod)

results = []


def rec(label, ok, detail=""):
    results.append((label, "PASS" if ok else "FAIL", detail))


# 0-diff vs core
files = sorted((ROOT / "json").glob("*.json"))
n_diff = 0
for f in files:
    doc = json.load(open(f))
    try:
        a = old_mod.generate_pl(copy.deepcopy(doc))
    except Exception as e:
        a = repr(e)
    try:
        b = new_mod.generate_pl(copy.deepcopy(doc))
    except Exception as e:
        b = repr(e)
    if a != b:
        n_diff += 1
rec(f"1 rider is 0-diff on the basis-free corpus ({len(files)} files)", n_diff == 0,
    f"{n_diff} diffs")

# Fixture
BASE = json.load(open(ROOT / "json/demographic_skill_mismatch.json"))
fix = copy.deepcopy(BASE)
fix["header"]["constraint_id"] = "basis_fixture_oq102a"
fix["header"].pop("module_name_override", None)
iv = fix["interval"]
fix.setdefault("measurements", []).append(
    {"metric": "theater_ratio", "time_point": iv["end"],
     "value": 0.9, "basis": "projected", "id_override": "bfix_tr_proj"})
GRID_METRICS = ["accessibility_collapse", "stakes_inflation", "suppression", "resistance"]
LEVELS = ["structural", "organizational", "class", "individual"]
pts = []
for lv in LEVELS:
    for m in GRID_METRICS:
        pts.append({"metric": m, "level": lv, "time_point": iv["start"], "value": 0.2})
        pts.append({"metric": m, "level": lv, "time_point": iv["end"], "value": 0.8})
pts[0]["basis"] = "observed"
pts[1]["basis"] = "projected"
fix["coercion_grid"] = {"t0": iv["start"], "tn": iv["end"], "points": pts}

errors = new_mod.validate_json(fix)
rec("2a fixture validates against live schema", not errors, "; ".join(errors[:3]))
out = new_mod.generate_pl(fix)
fix_pl = AUDIT / "basis_fixture_oq102a.pl"
fix_pl.write_text(out)
n_basis = sum(1 for ln in out.splitlines()
              if ln.startswith("narrative_ontology:measurement_basis("))
rec("2b compiler emits 3 measurement_basis facts (1 scalar projected, "
    "1 grid observed, 1 grid projected)", n_basis == 3, f"counted {n_basis}")
rec("2c basis rides the SAME MID as its value (bfix_tr_proj)",
    "narrative_ontology:measurement_basis(bfix_tr_proj, projected)." in out)
rec("2d multifile decl present",
    "narrative_ontology:measurement_basis/2" in out.split("END OF")[0])

# swipl: provenance bucket fires
goal = (
    "use_module(narrative_ontology), use_module(data_repair), "
    "use_module(diagnostic_summary), "
    f"consult('{fix_pl}'), "
    "diagnostic_summary:measurement_provenance(basis_fixture_oq102a, MP), "
    "format('MP=~w~n', [MP]), halt."
)
sw = subprocess.run(["swipl", "-g", goal, "-t", "halt(1)"],
                    cwd=ROOT / "prolog", capture_output=True, text=True, timeout=180)
swout = [l for l in sw.stdout.splitlines() if l.startswith("MP=")]
swout = swout[-1] if swout else (sw.stderr.strip()[-200:] or "no output")
# fixture: 6 scalar (base) + 1 added scalar + 32 grid = 39 total, all authored;
# projected = 2 (bfix_tr_proj + one grid point)
rec("3 measurement_provenance projected bucket NONZERO (expect meas_prov(39,0,0,2,39))",
    "meas_prov(39,0,0,2,39)" in swout, swout)

for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass (core ref {core_ref})")
sys.exit(1 if n_fail else 0)
