#!/usr/bin/env python3
"""Commit-1 second witness: the Phase-2 `arm(repaired)` column (a composition of the
engine's clause-2 body under a repaired guard) vs the post-commit `arm(repaired)` column
(the real repaired clause in the committed source), matched BY ARM FLAG and by
(leg, constraint_id) — never by column position (PREREG 6.1).
"""
import csv, pathlib, sys, collections
DIR = pathlib.Path(__file__).resolve().parent
LEGS = ["testsets","testsets_haiku","testsets_flash","testsets_kimi",
        "testsets_sonnet","archives_datasets_kernel_v1"]

def load(tag, leg):
    with (DIR/f"tsv_{tag}"/f"{leg}.tsv").open() as f:
        return list(csv.DictReader(f, delimiter="\t"))

ok = True
print("| leg | rows | source_arm phase2 | source_arm postfix | arm(repaired) rows equal | first diff |")
print("|---|---:|---|---|---|---|")
tot = 0; tote = 0
for leg in LEGS:
    a = load("phase2", leg); b = load("postfix", leg)
    aa = {r["source_arm"] for r in a}; bb = {r["source_arm"] for r in b}
    da = {r["constraint_id"]: r for r in a}
    db = {r["constraint_id"]: r for r in b}
    assert set(da) == set(db), f"{leg}: id set differs"
    eq = 0; first = ""
    for cid in da:
        if da[cid]["result__arm_repaired"] == db[cid]["result__arm_repaired"]:
            eq += 1
        elif not first:
            first = f"`{cid}`: {da[cid]['result__arm_repaired'][:50]} vs {db[cid]['result__arm_repaired'][:50]}"
            ok = False
    tot += len(da); tote += eq
    print(f"| `{leg}` | {len(da)} | {sorted(aa)} | {sorted(bb)} | **{eq}/{len(da)}** | {first or '—'} |")
print()
print(f"**Total: {tote}/{tot} rows identical on the arm-flag-matched `arm(repaired)` column.**")
# also: the postfix defect column must be NOT_MEASURED (the defect no longer exists in source)
nm = sum(1 for leg in LEGS for r in load("postfix", leg) if r["result__arm_defect"] == "NOT_MEASURED")
print(f"Post-commit `result__arm_defect` = NOT_MEASURED on {nm}/{tot} rows (the defect clause is gone from the source).")
sys.exit(0 if ok else 1)
