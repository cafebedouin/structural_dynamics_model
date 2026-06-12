#!/usr/bin/env python3
"""OQ-105 alignment-gate witnesses.

W1 (positive, synthetic): a live cohort-zero JSON with one suppression
    time_point removed must FAIL validate_json with the OQ-105 error.
W2 (negative, live): all five live cohort-zero JSONs pass the gate
    (gate errors only — full validate_json shown for the record).
W3 (positive, real data): the gate run over the archived pre-cohort-zero
    corpus (kernel_v2_test2/json) must flag, at minimum, the 11 OQ-105
    host constraints (subset check, names pinned from the 2026-06-11
    row-sweep census).
"""
import copy
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python"))
from generate_constraint_pl import validate_json, _grid_alignment_errors  # noqa: E402

OQ105_HOSTS = {
    "agenda_conditioning", "digital_colonialism_data_extraction",
    "institutional_trust_erosion", "post_1998_convergence", "scale_ceiling",
    "substantive_employment_reading", "techno_optimist_reading",
    "technocratic_paradigm_vs_human_primacy", "truth_democracy_disinformation",
    "wage_convergence_mechanism", "wage_convergence_sustainability",
}

# --- W1: synthetic misalignment must fire ---
src = json.load(open(REPO / "json" / "institutional_trust_erosion_c0.json"))
broken = copy.deepcopy(src)
removed = None
for i, m in enumerate(broken["measurements"]):
    if m["metric"] == "suppression_requirement" and m["time_point"] not in (0,):
        removed = broken["measurements"].pop(i)
        break
errs = _grid_alignment_errors(broken)
full = validate_json(broken)
w1 = len(errs) == 1 and "OQ-105" in errs[0] and any("OQ-105" in e for e in full)
print(f"W1 synthetic: removed {removed['metric']}@T={removed['time_point']}; "
      f"gate errors={len(errs)}; in validate_json={any('OQ-105' in e for e in full)} "
      f"-> {'PASS' if w1 else 'FAIL'}")
print(f"  error text: {errs[0][:160]}..." if errs else "  NO ERROR (defect)")

# --- W2: live cohort-zero JSONs pass ---
live = sorted((REPO / "json").glob("*_c0.json"))
w2_fail = []
for p in live:
    e = _grid_alignment_errors(json.load(open(p)))
    print(f"W2 live {p.name}: gate errors={len(e)}")
    if e:
        w2_fail.append(p.name)
w2 = len(live) == 5 and not w2_fail
print(f"W2 live corpus: {len(live)} JSONs, misaligned={w2_fail} -> {'PASS' if w2 else 'FAIL'}")

# --- W3: archived pre-cohort-zero corpus, subset check against the named 11 ---
arch = sorted((REPO / "prolog/archives/datasets/kernel_v2_test2/json").glob("*.json"))
flagged = set()
for p in arch:
    try:
        data = json.load(open(p))
    except Exception as ex:
        print(f"W3 unreadable {p.name}: {ex}")
        continue
    if _grid_alignment_errors(data):
        flagged.add(p.stem)
print(f"W3 archive: {len(arch)} JSONs scanned, {len(flagged)} misaligned: {sorted(flagged)}")
missing = OQ105_HOSTS - flagged
w3 = not missing
print(f"W3 subset check (11 OQ-105 hosts all flagged): missing={sorted(missing)} "
      f"-> {'PASS' if w3 else 'FAIL'}")

ok = w1 and w2 and w3
print(f"\nVERDICT: {'ALL WITNESSES PASS' if ok else 'WITNESS FAILURE'}")
sys.exit(0 if ok else 1)
