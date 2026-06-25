#!/usr/bin/env python3
"""Game-Theoretic Nash Equilibrium Orbit Detection

For each constraint with H¹>0 (non-constant orbit), computes:
  - nash_distance_structural: minimum single-observer type changes to reach H¹=0
  - nash_stable_structural: True if nash_distance_structural >= 2
  - vulnerable_positions: observer positions whose unilateral change could resolve H¹

"Structural" = uses any chain type as plausible change (lower bound on true Nash
distance). Future metric-constrained version (nash_distance_metric) would restrict
to types reachable under parameter perturbation.

Reads:  outputs/orbit_data.json  (or --input for product site orbits)
        outputs/pipeline_output.json

Writes: outputs/game_theory_nash.json

Usage:  python3 python/game_theory_nash.py
        python3 python/game_theory_nash.py --input outputs/product_site_orbits.json
"""

# NOTE (post-product-site): Nash vulnerability (single-observer resolution) is
# site-dependent. On the 4-point canonical site, 94% of Nash-distance-1 cases
# resolve via institutional observer. On the 156-point product site, ALL 267
# formerly-vulnerable constraints become Nash-stable (distance >= 2) because
# the institutional position occupies a 48-context block rather than a single
# point. Nash distance remains valid as a metric but should not be treated as
# the primary measure of institutional distinctiveness on expanded sites.
# Use block-level analysis (sheaf_analysis:block_consistency/2) instead.

import argparse
import json
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, load_orbits_constraints, h1_band_or_raise, ORBIT_JSON, PIPELINE_JSON, OUTPUT_DIR

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CHAIN_TYPES = ["mountain", "rope", "tangled_rope", "snare"]
OUTPUT_PATH = OUTPUT_DIR / "game_theory_nash.json"


# ---------------------------------------------------------------------------
# Context label extraction — N-context generalisation (v2.0)
# ---------------------------------------------------------------------------

def get_context_labels(orbit_entry):
    """Return sorted context labels from orbit entry.

    Handles both old format (4 power-atom keys: "powerless", "moderate", ...)
    and new product-site format (156 compound keys: "powerless_biographical_trapped_local", ...).
    Sorting ensures stable positional comparisons.
    """
    return sorted(orbit_entry.get("contexts", {}).keys())


def type_vector(orbit_entry):
    """Return (vec, labels) where vec[i] is the type at labels[i]."""
    labels = get_context_labels(orbit_entry)
    ctx = orbit_entry["contexts"]
    return [ctx[c] for c in labels], labels


# ---------------------------------------------------------------------------
# Nash distance computation
# ---------------------------------------------------------------------------

def h1_from_vector(vec):
    """Count disagreeing pairs (H¹ proxy) for an N-element type vector."""
    n = len(vec)
    count = 0
    for i in range(n):
        for j in range(i + 1, n):
            if vec[i] != vec[j]:
                count += 1
    return count


def is_constant(vec):
    """True if all types are identical (H¹=0)."""
    return len(set(vec)) == 1


def compute_nash_distance(vec, labels):
    """Compute structural Nash distance for a non-constant orbit.

    Returns (distance, vulnerable_positions):
      distance=1: some single observer change resolves H¹
      distance=2: requires 2 coordinated changes
      distance=3: requires 3 coordinated changes (3+ distinct types)

    O(n) fast-path: Nash distance equals the minority count when
    minority_count ≤ 2, else min(unique_types, 3).

    Proof sketch: a k-observer change achieves H¹=0 iff it can make all
    n elements identical. The minimum k is the number of elements that
    differ from the majority type (minority_count) when minority_count ≤ 2.
    For minority_count ≥ 3, at least 3 changes are needed, and min(unique,3)
    gives the correct lower bound.

    Equivalent to the original brute-force compute_nash_single_observer /
    compute_nash_two_observer approach, but O(n) instead of O(n³).
    """
    type_counts = Counter(vec)
    n = len(vec)
    majority_type, majority_count = type_counts.most_common(1)[0]
    minority_count = n - majority_count

    if minority_count == 1:
        # Distance 1: exactly one position differs from the majority.
        vulnerable = [labels[i] for i, t in enumerate(vec) if t != majority_type]
        return 1, vulnerable
    elif minority_count == 2:
        # Distance 2: exactly two positions differ; change both to majority.
        return 2, []
    else:
        # Distance 3+: need at least min(unique_types, 3) coordinated changes.
        unique = len(type_counts)
        return min(unique, 3), []


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--input", type=Path, default=ORBIT_JSON,
        help="Orbit data JSON (default: orbit_data.json; use product_site_orbits.json for product site)"
    )
    args = parser.parse_args()

    # Partition out top-level metadata (corpus_hash, OQ-29) when --input is the
    # product-site orbits file; orbit_data.json carries none and is a no-op.
    orbit_data = load_orbits_constraints(args.input)
    pipeline_data = load_json(PIPELINE_JSON, "pipeline_output")

    if not orbit_data or not pipeline_data:
        print("ERROR: Could not load required data files.", file=sys.stderr)
        sys.exit(1)

    # Build H¹ lookup from pipeline
    h1_lookup = {}
    sig_lookup = {}
    for c in pipeline_data.get("per_constraint", []):
        cid = c["id"]
        h1_lookup[cid] = h1_band_or_raise(c, "game_theory_nash")  # OQ-51: loud on null, no silent 0
        sig_lookup[cid] = c.get("signature", "")

    # Process each constraint
    results = []
    for cid, entry in orbit_data.items():
        h1 = h1_lookup.get(cid, 0)
        vec, labels = type_vector(entry)

        if h1 == 0 or is_constant(vec):
            # Dominant strategy exists — constant orbit
            results.append({
                "id": cid,
                "h1_band": h1,
                "orbit": {c: t for c, t in zip(labels, vec)},
                "unique_types": len(set(vec)),
                "nash_distance_structural": 0,
                "nash_stable_structural": False,
                "vulnerable_positions": [],
                "has_dominant_strategy": True,
                "signature": sig_lookup.get(cid, ""),
            })
        else:
            dist, vuln = compute_nash_distance(vec, labels)
            results.append({
                "id": cid,
                "h1_band": h1,
                "orbit": {c: t for c, t in zip(labels, vec)},
                "unique_types": len(set(vec)),
                "nash_distance_structural": dist,
                "nash_stable_structural": dist >= 2,
                "vulnerable_positions": vuln,
                "has_dominant_strategy": False,
                "signature": sig_lookup.get(cid, ""),
            })

    # Corpus-level statistics
    non_constant = [r for r in results if not r["has_dominant_strategy"]]
    nash_dist_counter = Counter(r["nash_distance_structural"] for r in non_constant)
    nash_stable_count = sum(1 for r in non_constant if r["nash_stable_structural"])

    # Vulnerable position frequency
    vuln_counter = Counter()
    for r in non_constant:
        for v in r["vulnerable_positions"]:
            vuln_counter[v] += 1

    # H¹ vs Nash distance cross-tabulation
    h1_nash_cross = {}
    for r in non_constant:
        key = f"h1={r['h1_band']}"
        if key not in h1_nash_cross:
            h1_nash_cross[key] = Counter()
        h1_nash_cross[key][f"nash={r['nash_distance_structural']}"] += 1
    h1_nash_cross = {k: dict(v) for k, v in h1_nash_cross.items()}

    # FCR overlap
    fcr_nash = Counter()
    for r in non_constant:
        if r["signature"] == "false_ci_rope":
            fcr_nash[f"nash={r['nash_distance_structural']}"] += 1

    summary = {
        "total_constraints": len(results),
        "constant_orbits": len(results) - len(non_constant),
        "non_constant_orbits": len(non_constant),
        "nash_distance_distribution": dict(sorted(nash_dist_counter.items())),
        "nash_stable_structural_count": nash_stable_count,
        "nash_stable_structural_pct": round(100 * nash_stable_count / max(len(non_constant), 1), 1),
        "vulnerable_position_frequency": dict(vuln_counter.most_common()),
        "h1_vs_nash_cross": h1_nash_cross,
        "fcr_nash_distribution": dict(fcr_nash),
        "methodology": "structural (any chain type as plausible change — lower bound on true Nash distance)",
    }

    output = {
        "generated": datetime.now().isoformat(),
        "summary": summary,
        "per_constraint": results,
    }

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_PATH, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2)

    # Print summary
    print(f"Game-theoretic Nash analysis complete.")
    print(f"  Total constraints: {summary['total_constraints']}")
    print(f"  Constant orbits (dominant strategy): {summary['constant_orbits']}")
    print(f"  Non-constant orbits: {summary['non_constant_orbits']}")
    print(f"  Nash distance distribution: {summary['nash_distance_distribution']}")
    print(f"  Nash-stable (distance >= 2): {summary['nash_stable_structural_count']} "
          f"({summary['nash_stable_structural_pct']}%)")
    print(f"  Vulnerable positions: {summary['vulnerable_position_frequency']}")
    print(f"  H1 vs Nash cross-tab: {summary['h1_vs_nash_cross']}")
    print(f"  Output: {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
