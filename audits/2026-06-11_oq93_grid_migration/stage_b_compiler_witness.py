#!/usr/bin/env python3
"""Stage B witness — OQ-93 compiler emission + referential integrity.

Parts:
  1. 0-diff old-vs-new compile over ALL json/ (old compiler from git ref;
     byte-identical outputs required; compile-failure sets must also match).
  2. Pilot grid story: full 32-point grid compiles; emitted facts counted and
     queried in swipl (authored source class, values exact).
  3. Negative battery (each rejection LOUD, on BOTH --validate and
     --no-validate paths where applicable):
       B1 out-of-endpoint time point; t0/tn != interval start/end
       B3 constructed-duplicate slot (the queued positive control for the
          once/1 contract at pattern_analysis.pl)

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/stage_b_compiler_witness.py <old_ref>
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
old_ref = sys.argv[1] if len(sys.argv) > 1 else "HEAD"

sys.path.insert(0, str(ROOT / "python"))
import generate_constraint_pl as new_mod

# Load the OLD compiler from git
old_src = subprocess.run(
    ["git", "show", f"{old_ref}:python/generate_constraint_pl.py"],
    cwd=ROOT, capture_output=True, text=True, check=True).stdout
with tempfile.NamedTemporaryFile("w", suffix="_old_gen.py", delete=False) as tf:
    tf.write(old_src)
    old_path = tf.name
spec = importlib.util.spec_from_file_location("old_gen", old_path)
old_mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(old_mod)

results = []


def rec(label, ok, detail=""):
    results.append((label, "PASS" if ok else "FAIL", detail))


# --- Part 1: 0-diff sweep ---------------------------------------------------
files = sorted((ROOT / "json").glob("*.json"))
n_same = n_diff = 0
both_fail = 0
fail_mismatch = []
for f in files:
    doc = json.load(open(f))
    try:
        out_old = old_mod.generate_pl(copy.deepcopy(doc))
        ok_old = True
    except Exception as e:
        out_old, ok_old = repr(e), False
    try:
        out_new = new_mod.generate_pl(copy.deepcopy(doc))
        ok_new = True
    except Exception as e:
        out_new, ok_new = repr(e), False
    if ok_old != ok_new:
        fail_mismatch.append((f.name, out_old[:80], out_new[:80]))
    elif not ok_old:
        both_fail += 1
    elif out_old == out_new:
        n_same += 1
    else:
        n_diff += 1
        fail_mismatch.append((f.name, "OUTPUT DIFF", ""))
rec(f"1 zero-diff sweep: {n_same} byte-identical, {both_fail} fail-both-identically, "
    f"{n_diff} diffs, {len(fail_mismatch)} mismatches",
    n_diff == 0 and not fail_mismatch)

# --- Part 2: pilot grid story ------------------------------------------------
BASE = json.load(open(ROOT / "json/demographic_skill_mismatch.json"))
GRID_METRICS = ["accessibility_collapse", "stakes_inflation", "suppression", "resistance"]
LEVELS = ["structural", "organizational", "class", "individual"]


def full_grid(t0, tn, lo=0.2, hi=0.8):
    pts = []
    for lv in LEVELS:
        for m in GRID_METRICS:
            pts.append({"metric": m, "level": lv, "time_point": t0, "value": lo})
            pts.append({"metric": m, "level": lv, "time_point": tn, "value": hi})
    return {"t0": t0, "tn": tn, "points": pts}


pilot = copy.deepcopy(BASE)
iv = pilot["interval"]
pilot["header"]["constraint_id"] = "grid_pilot_stage_b"
pilot["header"].pop("module_name_override", None)
pilot["coercion_grid"] = full_grid(iv["start"], iv["end"])
errors = new_mod.validate_json(pilot)
rec("2a pilot validates against live schema", not errors, "; ".join(errors[:3]))
out = new_mod.generate_pl(pilot)
n_grid_facts = sum(1 for ln in out.splitlines()
                   if ln.startswith("narrative_ontology:measurement(") and "_grid_" in ln)
pilot_pl = AUDIT / "grid_pilot_stage_b.pl"
pilot_pl.write_text(out)
rec("2b pilot emits 32 grid measurement facts", n_grid_facts == 32,
    f"counted {n_grid_facts}")

# swipl queryability + authored source class
swipl_goal = (
    "use_module(narrative_ontology), use_module(data_repair), "
    f"consult('{pilot_pl}'), "
    "aggregate_all(count, narrative_ontology:measurement(_, grid_pilot_stage_b, _, _, _), N), "
    "narrative_ontology:measurement(M1, grid_pilot_stage_b, accessibility_collapse(structural), 0, V0), "
    "data_repair:source_class(M1, Cls), "
    "format('N=~w V0=~w CLS=~w~n', [N, V0, Cls]), halt."
)
sw = subprocess.run(["swipl", "-g", swipl_goal, "-t", "halt(1)"],
                    cwd=ROOT / "prolog", capture_output=True, text=True, timeout=120)
swout = sw.stdout.strip().splitlines()[-1] if sw.stdout.strip() else sw.stderr[:200]
n_expect = 32 + len(pilot.get("measurements") or [])
rec(f"2c swipl: {n_expect} measurement facts (32 grid + {n_expect-32} scalar), "
    f"V0=0.2, class=authored",
    f"N={n_expect}" in swout and "V0=0.2" in swout and "CLS=authored" in swout, swout)

# --- Part 3: negative battery -------------------------------------------------


def expect_reject(label, doc, must_mention):
    try:
        new_mod.generate_pl(copy.deepcopy(doc))
        rec(label, False, "EXPECTED ValueError but compiled")
    except ValueError as e:
        ok = must_mention in str(e)
        rec(label, ok, f"raised: {str(e)[:140]}")
    except Exception as e:
        rec(label, False, f"wrong exception: {repr(e)[:120]}")


d = copy.deepcopy(pilot)
d["coercion_grid"]["points"][0]["time_point"] = iv["start"] + 1 if iv["end"] - iv["start"] > 1 else 99
expect_reject("3a out-of-endpoint time point REJECTED (battery 1)", d, "battery 1")

d = copy.deepcopy(pilot)
d["coercion_grid"]["t0"] = iv["start"] + 1
for p in d["coercion_grid"]["points"]:
    if p["time_point"] == iv["start"]:
        p["time_point"] = iv["start"] + 1
expect_reject("3b grid t0 != interval start REJECTED (battery 1, sub-interval)", d, "battery 1")

d = copy.deepcopy(pilot)
d["coercion_grid"]["points"][1] = dict(d["coercion_grid"]["points"][0])
expect_reject("3c constructed-duplicate slot REJECTED (battery 3 / once-1 contract control)",
              d, "battery 3")

# 3d: the duplicate must ALSO reject via the CLI --no-validate path
dup_json = AUDIT / "dup_slot_fixture.json"
dup_json.write_text(json.dumps(d))
cli = subprocess.run(
    [sys.executable, "python/generate_constraint_pl.py", "--no-validate", str(dup_json)],
    cwd=ROOT, capture_output=True, text=True)
rec("3d duplicate rejected on --no-validate CLI path too",
    cli.returncode != 0 and "battery 3" in (cli.stderr + cli.stdout),
    (cli.stderr or cli.stdout).strip().splitlines()[-1][:140] if (cli.stderr or cli.stdout).strip() else "no output")

print(f"old compiler ref: {old_ref}; corpus files: {len(files)}")
for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
for name, a, b in fail_mismatch[:10]:
    print(f"  MISMATCH {name}: old={a} new={b}")
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass")
sys.exit(1 if n_fail else 0)
