"""Phase 4 (Turn 1 Recon): Anchored-fixity composability test.

Groups historical constraints by case (Sparta, Hindu, Egypt), extracts
structural metrics from pipeline output, and saves raw data for analysis.

Run:
    python3 python/composability_test.py
Output:
    outputs/composability_recon.json
"""
import json
import re
from collections import Counter
from pathlib import Path

BASE = Path(__file__).parent.parent
PIPELINE_PATH = BASE / "outputs" / "pipeline_output.json"
OUT = BASE / "outputs" / "composability_recon.json"

# Case group patterns (match against constraint_id)
CASE_PATTERNS = {
    "sparta": re.compile(
        r"lycurgan|helot|spartan|oliganthropia|reformer_king|krypteia|leuctra"
    ),
    "hindu": re.compile(
        r"vedic|brahmanical|jati|sanskritization|dharmasastra|colonial_caste"
    ),
    "egypt": re.compile(
        r"maat|amun|akhenaten|amarna|egypt_three"
    ),
}

POWER_ATOMS = ["powerless", "moderate", "powerful", "organized", "institutional", "analytical"]


def group_stats(constraints):
    """Compute structural statistics for a list of per_constraint dicts."""
    if not constraints:
        return {"n": 0, "error": "no constraints"}

    n = len(constraints)

    # Claimed type distribution
    claimed = Counter(c.get("claimed_type", "unknown") for c in constraints)

    # Engine-computed perspective type distributions
    persp_types = []
    for c in constraints:
        for atom, t in (c.get("perspectives") or {}).items():
            persp_types.append(t)
    persp_dist = Counter(persp_types)

    # Tangled-rope ratio (interpretive-accretion proxy)
    total_persp = sum(persp_dist.values())
    tr_ratio = persp_dist.get("tangled_rope", 0) / total_persp if total_persp else 0
    snare_ratio = persp_dist.get("snare", 0) / total_persp if total_persp else 0

    # Mean structural metrics
    def mean_field(field):
        vals = [c.get(field) for c in constraints if c.get(field) is not None]
        return round(sum(vals) / len(vals), 4) if vals else None

    # Signature distribution
    sigs = Counter(c.get("signature", "unknown") for c in constraints)

    # Per-power-atom perspective data
    atom_profiles = {}
    for atom in POWER_ATOMS:
        types_for_atom = []
        chi_for_atom = []
        for c in constraints:
            persp = c.get("perspectives") or {}
            if atom in persp:
                types_for_atom.append(persp[atom])
            chi_data = c.get("perspective_chi") or {}
            if atom in chi_data:
                chi_for_atom.append(chi_data[atom].get("chi"))
        atom_profiles[atom] = {
            "type_distribution": dict(Counter(types_for_atom)),
            "mean_chi": round(sum(v for v in chi_for_atom if v is not None) / len([v for v in chi_for_atom if v is not None]), 4)
                        if any(v is not None for v in chi_for_atom) else None,
        }

    # Perspective variance: how many distinct types appear across positions
    per_constraint_type_counts = []
    for c in constraints:
        persp = c.get("perspectives") or {}
        distinct = len(set(persp.values()))
        per_constraint_type_counts.append(distinct)
    mean_type_variance = round(sum(per_constraint_type_counts) / len(per_constraint_type_counts), 4) if per_constraint_type_counts else None

    return {
        "n": n,
        "constraint_ids": [c["id"] for c in constraints],
        "claimed_type_distribution": dict(claimed),
        "perspective_type_distribution": dict(persp_dist),
        "tangled_rope_ratio": round(tr_ratio, 4),
        "snare_ratio": round(snare_ratio, 4),
        "mean_base_extractiveness": mean_field("base_extractiveness"),
        "mean_suppression": mean_field("suppression"),
        "mean_theater_ratio": mean_field("theater_ratio"),
        "mean_purity_score": mean_field("purity_score"),
        "signature_distribution": dict(sigs),
        "atom_profiles": atom_profiles,
        "mean_perspectival_type_variance": mean_type_variance,
    }


def main():
    print("Loading pipeline output...")
    data = json.loads(PIPELINE_PATH.read_text())
    per_constraint = {c["id"]: c for c in data.get("per_constraint", [])}

    groups = {name: [] for name in CASE_PATTERNS}
    unmatched = []

    for cid, c in per_constraint.items():
        matched = False
        for group_name, pattern in CASE_PATTERNS.items():
            if pattern.search(cid):
                groups[group_name].append(c)
                matched = True
                break
        # Only flag as unmatched if it's from historical domains
        domain = c.get("human_readable", "")
        if not matched:
            pass  # not printing unmatched — only historical constraints matter here

    print(f"Groups found:")
    for name, cs in groups.items():
        print(f"  {name}: {len(cs)} constraints — {[c['id'] for c in cs]}")

    result = {
        "groups": {name: group_stats(cs) for name, cs in groups.items()},
        "methodology_note": (
            "Groups assigned by constraint_id pattern match. "
            "Proxy validity requires comparing engine-computed signatures "
            "(perspective_chi, signature field) against text-derived proxies "
            "(tangled_rope_ratio, suppression). "
            "Seed text may have induced type assignments directly — "
            "see atom_profiles for engine-computed chi values as independent signal."
        ),
    }

    OUT.write_text(json.dumps(result, indent=2))
    print(f"\nRecon data saved to {OUT}")


if __name__ == "__main__":
    main()
