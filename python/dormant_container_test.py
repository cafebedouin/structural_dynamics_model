"""Phase 5 (Turn 1 Recon): Dormant-container activation test.

Compares dormant containers (Estates-General, Imperial Court) against
active outer containers and inner containers. Extracts structural metrics
to test whether dormant containers have a distinguishable structural profile.

Run:
    python3 python/dormant_container_test.py
Output:
    outputs/dormant_recon.json
"""
import json
from pathlib import Path

BASE = Path(__file__).parent.parent
PIPELINE_PATH = BASE / "outputs" / "pipeline_output.json"
OUT = BASE / "outputs" / "dormant_recon.json"

# Constraint groups for dormant-container test
GROUPS = {
    "dormant": [
        "estates_general_175_year_dormancy",
        "imperial_court_kyoto_dormant_legitimacy",
    ],
    "active_outer_france": [
        "tennis_court_oath_container_reclassification",
        "parlements_inner_container_authority",
        "salic_law_succession_kernel",
    ],
    "active_outer_japan": [
        "sonno_joi_dormant_activation",
        "satsuma_choshu_independent_capacity",
    ],
    "inner_container": [
        # Systems with operational inner authority vs dormant outer
        "parlements_inner_container_authority",
        "bakuhan_outer_bandwidth_degradation",
        "satsuma_choshu_independent_capacity",
    ],
    # All historical constraints for context
    "all_historical": None,  # filled below
}

# Hypothesized dormant signature fields:
# - high formalization / theater_ratio (preserved form without operational function)
# - low suppression (not actively enforcing)
# - low base_extractiveness (not currently extracting)
# - institutional beneficiaries (formal institutional role claimed)
# - requires_active_enforcement = False (dormant means not enforcing)

POWER_ATOMS = ["powerless", "moderate", "powerful", "organized", "institutional", "analytical"]


def extract_constraint(c):
    """Return a flat dict of fields useful for dormant-container analysis."""
    return {
        "id": c["id"],
        "claimed_type": c.get("claimed_type"),
        "base_extractiveness": c.get("base_extractiveness"),
        "suppression": c.get("suppression"),
        "theater_ratio": c.get("theater_ratio"),
        "resistance": c.get("resistance"),
        "signature": c.get("signature"),
        "purity_score": c.get("purity_score"),
        "purity_band": c.get("purity_band"),
        "perspectives": c.get("perspectives", {}),
        "perspective_chi": {
            atom: (c.get("perspective_chi") or {}).get(atom, {}).get("chi")
                  if isinstance((c.get("perspective_chi") or {}).get(atom), dict)
                  else (c.get("perspective_chi") or {}).get(atom)
            for atom in POWER_ATOMS
        },
    }


def group_summary(constraints):
    """Compute mean/distribution for a group."""
    if not constraints:
        return {"n": 0}
    n = len(constraints)

    def mean(field):
        vals = [c[field] for c in constraints if c.get(field) is not None]
        return round(sum(vals) / len(vals), 4) if vals else None

    from collections import Counter
    sigs = Counter(c.get("signature", "unknown") for c in constraints)
    types = Counter(c.get("claimed_type", "unknown") for c in constraints)

    # Per-perspective type distribution
    persp_types = []
    for c in constraints:
        for t in (c.get("perspectives") or {}).values():
            persp_types.append(t)
    persp_dist = Counter(persp_types)
    total_persp = sum(persp_dist.values())

    # Institutional chi (perspective_chi is {atom: {"chi": float, ...}} in pipeline output)
    inst_chi_vals = []
    for c in constraints:
        pchi = c.get("perspective_chi") or {}
        inst = pchi.get("institutional")
        if isinstance(inst, dict):
            val = inst.get("chi")
        elif isinstance(inst, (int, float)):
            val = inst
        else:
            val = None
        if val is not None:
            inst_chi_vals.append(val)
    inst_chi_mean = round(sum(inst_chi_vals) / len(inst_chi_vals), 4) if inst_chi_vals else None

    return {
        "n": n,
        "constraint_ids": [c["id"] for c in constraints],
        "mean_base_extractiveness": mean("base_extractiveness"),
        "mean_suppression": mean("suppression"),
        "mean_theater_ratio": mean("theater_ratio"),
        "mean_resistance": mean("resistance"),
        "mean_purity_score": mean("purity_score"),
        "claimed_type_distribution": dict(types),
        "signature_distribution": dict(sigs),
        "perspective_type_distribution": dict(persp_dist),
        "institutional_chi_mean": inst_chi_mean,
        "per_constraint": [extract_constraint(c) for c in constraints],
    }


def main():
    print("Loading pipeline output...")
    data = json.loads(PIPELINE_PATH.read_text())
    by_id = {c["id"]: c for c in data.get("per_constraint", [])}

    result = {}
    for group_name, ids in GROUPS.items():
        if ids is None:
            # All historical constraints
            historical_ids = [
                cid for cid in by_id
                if any(pat in cid for pat in [
                    "lycurgan", "helot", "spartan", "oliganthropia", "reformer_king",
                    "krypteia", "leuctra", "vedic", "brahmanical", "jati",
                    "sanskritization", "dharmasastra", "colonial_caste",
                    "maat", "amun", "akhenaten", "amarna", "egypt_three",
                    "estates_general", "parlements", "tennis_court",
                    "salic_law", "imperial_court", "bakuhan", "satsuma",
                    "sonno_joi", "athenian", "mutilation", "sicilian",
                    "trial_of_socrates",
                ])
            ]
            constraints = [by_id[cid] for cid in historical_ids if cid in by_id]
            print(f"  all_historical: {len(constraints)} constraints")
        else:
            constraints = [by_id[cid] for cid in ids if cid in by_id]
            missing = [cid for cid in ids if cid not in by_id]
            print(f"  {group_name}: {len(constraints)} constraints" +
                  (f" (missing: {missing})" if missing else ""))

        result[group_name] = group_summary(constraints)

    result["methodology_note"] = (
        "Dormant hypothesis: dormant containers should show high theater_ratio "
        "(preserved form), low suppression (not actively enforcing), "
        "low base_extractiveness (not currently extracting), and low institutional_chi "
        "(institutional position sees low effective power). "
        "Compare dormant group vs active_outer groups on these dimensions. "
        "If dormant and active_outer profiles are identical, the engine cannot distinguish them."
    )

    OUT.write_text(json.dumps(result, indent=2))
    print(f"\nRecon data saved to {OUT}")


if __name__ == "__main__":
    main()
