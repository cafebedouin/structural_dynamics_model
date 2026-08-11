#!/usr/bin/env python3
"""Verify every OQ-277 control. Run from the audit directory:

    cd audits/2026-08-10_oq277_rq2_crosscoding && python3 controls/verify_controls.py

Re-runnable by design: `HANDOFF_TWINS_AND_DRIVER.md` tells the next instance to re-run this
rather than trust a table, and an instruction to re-run something that exists only as saved
OUTPUT is not executable. (The first version of this control WAS only its output — the same
shape as everything else this arc: a plausible artifact standing in for a checkable one.)

Every check below is TWO-SIDED where a one-sided version could pass by never firing:
the leak fixtures must FIRE, the decoys and anchors must be SILENT, and the layer scoping
must fire on the collocation while staying silent on the bare word. Exit 1 on any failure.
"""
import json
import re
import sys
import glob
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', 'python', 'audits'))
import oq277_lexicon as L  # noqa: E402

F = L.CODER_FACING_FIELDS
HERE = os.path.dirname(os.path.abspath(__file__))
AUDIT = os.path.dirname(HERE)
fail = []


def check(cond, label):
    print(f"   {'PASS' if cond else '*** FAIL':>9}  {label}")
    if not cond:
        fail.append(label)


print("=== OQ-277 CONTROL VERIFICATION (extractor, pre-driver) ===\n")

# ---------------------------------------------------------------- anchors
print("[1] ANCHORS — clean under BOTH lexicons, and every label re-derived from its frozen source")
anch = json.load(open(f"{AUDIT}/controls/anchors.json"))
wu = {u['id']: u for u in json.load(open(f"{AUDIT}/packets/wu_units.json"))['units']}
for u in anch['direction_i']['anchors']:
    hits = [h for d in ('i', 'ii') for f in F for h in L.scan(u.get(f, ''), d)]
    check(not hits, f"anchor {u['id']} (true_label {u['true_label']}) clean under both lists")
for u in anch['direction_ii']['anchors']:
    src = wu[u['wu_unit_id']]
    m = src['metadata']
    check(m['catalog_class'] == m['dataset_class'] == u['true_label'] and m['agreeing'] is True,
          f"anchor {u['id']} label {u['true_label']} == BOTH frozen class fields, agreeing=True")
    hits = [h for d in ('i', 'ii') for f in F for h in L.scan(src.get(f, ''), d)]
    check(not hits, f"anchor {u['id']} text clean under both lists")

# ---------------------------------------------------------------- decoys
print("\n[2] DECOYS — coder-facing in both directions, so clean under both lexicons")
for d in json.load(open(f"{AUDIT}/controls/decoys.json"))['decoys']:
    hits = [h for dr in ('i', 'ii') for f in F for h in L.scan(d.get(f, ''), dr)]
    check(not hits and d['want'] == 'other', f"decoy {d['id']} clean, want={d['want']}")

# ---------------------------------------------------------------- planted leak
print("\n[3] PLANTED LEAK — must FIRE (a matcher that never fires witnesses nothing)")
p = json.load(open(f"{AUDIT}/controls/planted.json"))['planted_leak']
for key, dr in (("direction_i_payload_fixture", "i"), ("direction_ii_payload_fixture", "ii")):
    hits = L.scan(p[key]['unit_text'], dr)
    check(len(hits) > 0, f"{key}: {len(hits)} hits under direction ({dr})")
r5 = {h[2].lower() for h in L.scan(p['direction_ii_payload_fixture']['unit_text'], 'ii')}
for term in ('parasitic', 'cross-cutting'):
    check(term in r5, f"R5 framing term {term!r} caught (must never reach a coder)")
check(any('layer' in t for t in r5), "a layer COLLOCATION caught")

print("\n[4] LAYER SCOPING — collocation fires, bare word survives (over-redaction biases to 'other')")
check(bool(L.scan("our six sorts by system layer", 'ii')), "'sorts by system layer' FIRES")
check(not L.scan("the reporting layer was silent", 'ii'), "bare 'layer' NOT flagged")

# ---------------------------------------------------------------- planted broken unit
print("\n[5] PLANTED BROKEN UNIT — well-formed, clean, and never a member of our_units/")
b = json.load(open(f"{AUDIT}/controls/planted.json"))['planted_broken_unit']['unit']
check(all(f in b for f in F), "all four coder-facing fields present")
check(sum(len(L.scan(b.get(f, ''), 'ii')) for f in F) == 0, "clean under direction (ii)")
check(b['matrix_unit'] is False, "matrix_unit=False (can never enter a cell)")
resident = [f for f in glob.glob(f"{AUDIT}/packets/our_units/*.json")
            if json.load(open(f)).get('extractor') == 'PLANTED']
check(not resident, "not resident in packets/our_units/")

# ---------------------------------------------------------------- disjointness
print("\n[6] DISJOINTNESS — no control incident is any extracted unit's incident")
print("    (tested over UNIT TEXT, not source directories: the coder reads units)")
units = [json.load(open(f)) for f in sorted(glob.glob(f"{AUDIT}/packets/our_units/*.json"))]
probes = {
    "decoy_1 interface rejects a parameter": r"reject\w*[^.]{0,60}param|\b400\b",
    "decoy_2 published report delayed": r"annual report|external reviewer|sign[- ]off",
    "anchor_i_1 stale post-process": r"(post-process|artifact)[^.]{0,120}never re-r|froze at",
    "anchor_i_2 two copies": r"(two copies|test copy)[^.]{0,120}canonical",
    "anchor_i_3 empty-table count gate": r"count *== *0|no beneficiary (?:was )?authored",
}
for n, pat in probes.items():
    hit = [u['source_dir'] for u in units
           if re.search(pat, " ".join(str(u.get(k, '')) for k in F), re.I)]
    check(not hit, f"{n} -> {hit or 'DISJOINT'}")

print("\n[7] DISQUALIFIED-ANCHOR CONTROL — the probe that forced the anchor set must still FIRE")
print("    (a probe that only returns silence cannot distinguish clean from never-looked)")
p6 = [u['source_dir'] for u in units
      if re.search(r"gradient[^.]{0,80}(0\.0|zero)|read exactly 0\.0",
                   " ".join(str(u.get(k, '')) for k in F), re.I)]
check(len(p6) == 1, f"published P6 exemplar still collides with exactly one unit -> {p6}")
check(not any(a['true_label'] == 'P6' for a in anch['direction_i']['anchors']),
      "no P6 anchor is present (the collision is respected, not merely noted)")

# ---------------------------------------------------------------- cells
print("\n[8] CELL ACCOUNTING — quarantine on matrix_unit, never on overlap_source alone")
cells = [u for u in units if u['matrix_unit']]
dirs = {}
for u in cells:
    dirs[u['source_dir']] = dirs.get(u['source_dir'], 0) + 1
check(len(cells) == 22, f"matrix units == 22 (got {len(cells)})")
check(set(dirs.values()) == {1}, "every sampled directory contributes exactly ONE cell")
check(sum(1 for u in units if not u['overlap_source']) == 18,
      "overlap_source alone would give 18 — recorded so the wrong field is never used")

print("\n" + ("ALL CONTROLS PASS" if not fail else f"*** {len(fail)} FAILURES: {fail}"))
sys.exit(1 if fail else 0)
