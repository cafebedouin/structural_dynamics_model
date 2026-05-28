"""Phase 6 (Turn 1 Recon): Bandwidth refinement tests.

Sub-test A: Bandwidth atrophy through disuse
  Compare estates_general (atrophy) and bakuhan (atrophy) against
  math/logic domains (always-blocked bandwidth) to test whether
  atrophy-blocked vs always-blocked are structurally distinguishable.

Sub-test B: Nominal vs. operational bandwidth
  Compare akhenaten/amarna (nominal high bandwidth + operational constraint)
  against other Egypt constraints where authority is uniformly operational.

Run:
    python3 python/bandwidth_refinement_test.py
Output:
    outputs/bandwidth_recon.json
"""
import json
from collections import Counter
from pathlib import Path

BASE = Path(__file__).parent.parent
PIPELINE_PATH = BASE / "outputs" / "pipeline_output.json"
CORPUS_PATH = BASE / "outputs" / "corpus_data.json"
OUT = BASE / "outputs" / "bandwidth_recon.json"

# Sub-test A: atrophy vs always-blocked
ATROPHY_IDS = [
    "estates_general_175_year_dormancy",
    "bakuhan_outer_bandwidth_degradation",
]
# Always-blocked: sample from math/logic anchored_fixity group
# These will be identified dynamically from corpus data by domain
ALWAYS_BLOCKED_DOMAINS = {
    "mathematics", "mathematical", "mathematical_logic", "physics",
    "theoretical_physics", "cognitive_science", "biological", "evolutionary_biology",
}

# Sub-test B: nominal vs operational bandwidth
NOMINAL_HIGH_IDS = [
    "akhenaten_kernel_revision_failure",
    "amarna_restoration_inner_reassertion",
]
OPERATIONAL_EGYPT_IDS = [
    "maat_interpretive_framework_egypt",
    "amun_priesthood_authority_substrate",
    "egypt_three_millennia_persistence",
]

POWER_ATOMS = ["powerless", "moderate", "powerful", "organized", "institutional", "analytical"]


def extract_fields(c, corpus_entry=None):
    """Return structural fields for bandwidth analysis."""
    fields = {
        "id": c["id"],
        "claimed_type": c.get("claimed_type"),
        "base_extractiveness": c.get("base_extractiveness"),
        "suppression": c.get("suppression"),
        "theater_ratio": c.get("theater_ratio"),
        "resistance": c.get("resistance"),
        "signature": c.get("signature"),
        "perspectives": c.get("perspectives", {}),
        "perspective_chi": {
            atom: (c.get("perspective_chi") or {}).get(atom, {}).get("chi")
                  if isinstance((c.get("perspective_chi") or {}).get(atom), dict)
                  else (c.get("perspective_chi") or {}).get(atom)
            for atom in POWER_ATOMS
        },
    }
    # From corpus: requires_active_enforcement and emerges_naturally
    if corpus_entry:
        fields["requires_active_enforcement"] = corpus_entry.get("requires_active_enforcement")
        fields["emerges_naturally"] = corpus_entry.get("emerges_naturally")
    return fields


def summarize_group(constraints, corpus_by_id=None):
    if not constraints:
        return {"n": 0}
    n = len(constraints)

    def mean(field):
        vals = [c.get(field) for c in constraints if c.get(field) is not None]
        return round(sum(vals) / len(vals), 4) if vals else None

    # Perspective type distribution
    persp_types = []
    for c in constraints:
        for t in (c.get("perspectives") or {}).values():
            persp_types.append(t)
    persp_dist = Counter(persp_types)
    total_persp = sum(persp_dist.values())

    # Distinct types per constraint (types_produced proxy)
    types_produced = []
    for c in constraints:
        persp = c.get("perspectives") or {}
        types_produced.append(len(set(persp.values())))

    # Suppression-extractiveness divergence (nominal-vs-operational signal)
    supp_ext_divergence = []
    for c in constraints:
        s = c.get("suppression")
        e = c.get("base_extractiveness")
        if s is not None and e is not None:
            supp_ext_divergence.append(abs(s - e))

    # requires_active_enforcement rate from corpus
    enf_vals = [c.get("requires_active_enforcement") for c in constraints if corpus_by_id and c["id"] in corpus_by_id]
    enf_rate = None
    if corpus_by_id:
        enf_vals = [corpus_by_id[c["id"]].get("requires_active_enforcement")
                    for c in constraints if c["id"] in corpus_by_id]
        enf_rate = round(
            sum(1 for v in enf_vals if v is True) / len([v for v in enf_vals if v is not None]), 4
        ) if any(v is not None for v in enf_vals) else None

    sigs = Counter(c.get("signature", "unknown") for c in constraints)
    types = Counter(c.get("claimed_type", "unknown") for c in constraints)

    return {
        "n": n,
        "constraint_ids": [c["id"] for c in constraints],
        "mean_suppression": mean("suppression"),
        "mean_base_extractiveness": mean("base_extractiveness"),
        "mean_theater_ratio": mean("theater_ratio"),
        "mean_resistance": mean("resistance"),
        "mean_types_produced": round(sum(types_produced) / len(types_produced), 4) if types_produced else None,
        "mean_suppression_extractiveness_divergence": round(
            sum(supp_ext_divergence) / len(supp_ext_divergence), 4
        ) if supp_ext_divergence else None,
        "requires_active_enforcement_rate": enf_rate,
        "claimed_type_distribution": dict(types),
        "signature_distribution": dict(sigs),
        "perspective_type_distribution": dict(persp_dist),
        "per_constraint": [extract_fields(c) for c in constraints],
    }


def main():
    print("Loading pipeline output...")
    data = json.loads(PIPELINE_PATH.read_text())
    by_id = {c["id"]: c for c in data.get("per_constraint", [])}

    print("Loading corpus data...")
    corpus_raw = json.loads(CORPUS_PATH.read_text())
    corpus_by_id = corpus_raw.get("constraints", {})

    # Sub-test A: atrophy vs always-blocked
    atrophy_constraints = [by_id[cid] for cid in ATROPHY_IDS if cid in by_id]
    missing_a = [cid for cid in ATROPHY_IDS if cid not in by_id]
    if missing_a:
        print(f"  WARNING: atrophy group missing: {missing_a}")

    # Sample up to 20 always-blocked constraints from the anchor domains
    always_blocked = []
    for cid, c in by_id.items():
        cdata = corpus_by_id.get(cid, {})
        domain = (cdata.get("domain") or "").split("/")[0]
        if domain in ALWAYS_BLOCKED_DOMAINS:
            always_blocked.append(c)
        if len(always_blocked) >= 20:
            break

    print(f"Sub-test A: atrophy n={len(atrophy_constraints)}, always_blocked n={len(always_blocked)}")

    # Sub-test B: nominal high vs operational egypt
    nominal_constraints = [by_id[cid] for cid in NOMINAL_HIGH_IDS if cid in by_id]
    operational_constraints = [by_id[cid] for cid in OPERATIONAL_EGYPT_IDS if cid in by_id]
    missing_b = [cid for cid in NOMINAL_HIGH_IDS + OPERATIONAL_EGYPT_IDS if cid not in by_id]
    if missing_b:
        print(f"  WARNING: sub-test B missing: {missing_b}")

    print(f"Sub-test B: nominal n={len(nominal_constraints)}, operational n={len(operational_constraints)}")

    result = {
        "sub_test_a_bandwidth_atrophy": {
            "hypothesis": (
                "Atrophy-blocked bandwidth (Estates-General dormancy, Tokugawa decline) "
                "should be structurally identical to always-blocked bandwidth (math/logic) "
                "IF the engine cannot distinguish them. Different profiles would indicate "
                "distinguishable structural signatures."
            ),
            "atrophy_group": summarize_group(atrophy_constraints, corpus_by_id),
            "always_blocked_group": summarize_group(always_blocked, corpus_by_id),
        },
        "sub_test_b_nominal_vs_operational": {
            "hypothesis": (
                "Nominal-high bandwidth (Akhenaten: pharaonic authority nominally absolute "
                "but operationally constrained by inner-container non-compliance) should show "
                "higher suppression-extractiveness divergence and higher resistance than "
                "uniformly operational Egypt constraints (Ma'at, Amun priesthood)."
            ),
            "nominal_high_bandwidth": summarize_group(nominal_constraints, corpus_by_id),
            "operational_egypt": summarize_group(operational_constraints, corpus_by_id),
        },
        "methodology_note": (
            "Sub-test A: if atrophy and always-blocked show identical profiles on "
            "suppression, types_produced, requires_active_enforcement_rate, the engine "
            "cannot distinguish them without temporal data. "
            "Sub-test B: nominal-vs-operational distinction would be detectable if "
            "resistance and suppression-extractiveness divergence are elevated in the "
            "nominal group relative to the operational group."
        ),
    }

    OUT.write_text(json.dumps(result, indent=2))
    print(f"\nRecon data saved to {OUT}")


if __name__ == "__main__":
    main()
