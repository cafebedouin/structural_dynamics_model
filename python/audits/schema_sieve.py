#!/usr/bin/env python3
"""Schema sieve: group constraints by authored (non-classification) schema elements.

Phase 1 (this script): extract per-constraint feature vectors from
outputs/pipeline_output.json + prolog/testsets/*.pl and save raw extraction
to outputs/schema_sieve/features.json. No analysis here.

Authored/choosable features (the sieve):
  numerics:   eps, sup, tr               (base_extractiveness, suppression, theater_ratio)
  binaries:   emerges_naturally, requires_active_enforcement, has_sunset,
              has_bfo (boltzmann_floor_override), has_acc_collapse
  counts:     n_beneficiaries, n_victims, n_omegas, n_affects, n_dir_overrides,
              n_perspectives
  selections: coordination_type, contexts (sorted authored (P,T,E,S) tuples),
              omega_types (sorted multiset)

Held-out classification space (recorded for comparison, never in the vector):
  claimed_type, profile (canonical 4-perspective computed types), signature.
"""
import json
import os
import re
import sys
from collections import Counter

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PIPE = os.path.join(REPO, "outputs", "pipeline_output.json")
TESTSETS = os.path.join(REPO, "prolog", "testsets")
OUTDIR = os.path.join(REPO, "outputs", "schema_sieve")

RE_COORD = re.compile(r"coordination_type\(\s*([a-z0-9_]+)\s*,\s*([a-z0-9_]+)\s*\)")
RE_SUNSET = re.compile(r"has_sunset_clause\(\s*([a-z0-9_]+)\s*\)")
RE_BFO = re.compile(r"boltzmann_floor_override\(\s*([a-z0-9_]+)\s*,")
RE_DIROV = re.compile(r"^constraint_indexing:directionality_override\(\s*([a-z0-9_]+)\s*,", re.M)
RE_ACC = re.compile(r"^narrative_ontology:constraint_metric\(\s*([a-z0-9_]+)\s*,\s*accessibility_collapse\s*,\s*([0-9.]+)\s*\)", re.M)
RE_AFFECTS = re.compile(r"affects_constraint\(\s*([a-z0-9_]+)\s*,\s*([a-z0-9_]+)\s*\)")


def scan_testsets():
    """One pass over testsets/*.pl for facts not in pipeline output. Keyed by file basename (= corpus id)."""
    out = {}
    for fn in sorted(os.listdir(TESTSETS)):
        if not fn.endswith(".pl"):
            continue
        cid = fn[:-3]
        with open(os.path.join(TESTSETS, fn), encoding="utf-8", errors="replace") as f:
            txt = f.read()
        # strip block comments and %-comments so commentary mentions don't count as facts
        code = re.sub(r"/\*.*?\*/", "", txt, flags=re.S)
        code = re.sub(r"%.*", "", code)
        out[cid] = {
            "coordination_type": next((m.group(2) for m in RE_COORD.finditer(code) if m.group(1) == cid), None),
            "has_sunset": any(m.group(1) == cid for m in RE_SUNSET.finditer(code)),
            "has_bfo": any(m.group(1) == cid for m in RE_BFO.finditer(code)),
            "n_dir_overrides": sum(1 for m in RE_DIROV.finditer(code) if m.group(1) == cid),
            "acc_collapse": next((float(m.group(2)) for m in RE_ACC.finditer(code) if m.group(1) == cid), None),
            "n_affects": sum(1 for m in RE_AFFECTS.finditer(code) if m.group(1) == cid),
        }
    return out


def main():
    with open(PIPE) as f:
        pipe = json.load(f)
    pl = scan_testsets()

    rows = []
    missing_pl = []
    for e in pipe["per_constraint"]:
        cid = e["id"]
        plf = pl.get(cid)
        if plf is None:
            missing_pl.append(cid)
            continue
        contexts = sorted(
            (c["context"]["agent_power"], c["context"]["time_horizon"],
             c["context"]["exit_options"], c["context"]["spatial_scope"])
            for c in e["classifications"]
        )
        rows.append({
            "id": cid,
            # --- authored sieve features ---
            "eps": e["base_extractiveness"],
            "sup": e["suppression"],
            "tr": e["theater_ratio"],
            "emerges_naturally": bool(e["emerges_naturally"]),
            "requires_active_enforcement": bool(e["requires_active_enforcement"]),
            "has_sunset": plf["has_sunset"],
            "has_bfo": plf["has_bfo"],
            "acc_collapse": plf["acc_collapse"],
            "n_dir_overrides": plf["n_dir_overrides"],
            "n_affects": plf["n_affects"],
            "n_beneficiaries": len(e["beneficiaries"]),
            "n_victims": len(e["victims"]),
            "n_omegas": len(e["omegas"]),
            "omega_types": sorted(o["type"] for o in e["omegas"]),
            "coordination_type": plf["coordination_type"],
            "n_perspectives": len(e["classifications"]),
            "contexts": ["|".join(c) for c in contexts],
            # --- held-out classification space ---
            "claimed_type": e["claimed_type"],
            "profile": "/".join(e["perspectives"].get(p, "-") for p in
                                ("powerless", "moderate", "institutional", "analytical")),
            "signature": e["signature"],
        })

    os.makedirs(OUTDIR, exist_ok=True)
    out = {
        "manifest": pipe["manifest"],
        "n_rows": len(rows),
        "missing_pl_files": missing_pl,
        "rows": rows,
    }
    with open(os.path.join(OUTDIR, "features.json"), "w") as f:
        json.dump(out, f, indent=1)

    # positive-control prints: known-count fields must reproduce the grep counts
    print(f"rows: {len(rows)}  (pipeline n_constraints: {pipe['manifest']['n_constraints']})")
    print(f"missing .pl for pipeline ids: {missing_pl}")
    print(f"coordination_type non-null: {sum(1 for r in rows if r['coordination_type'])}  (grep said 1104)")
    print(f"has_sunset: {sum(1 for r in rows if r['has_sunset'])}  (grep said 12)")
    print(f"has_bfo: {sum(1 for r in rows if r['has_bfo'])}  (grep said 163 files)")
    print(f"sum n_dir_overrides: {sum(r['n_dir_overrides'] for r in rows)}  (grep said 832)")
    print(f"acc_collapse authored: {sum(1 for r in rows if r['acc_collapse'] is not None)}  (fact-form grep control below)")


if __name__ == "__main__":
    sys.exit(main())
