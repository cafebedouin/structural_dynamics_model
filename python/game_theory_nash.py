#!/usr/bin/env python3
"""Game-Theoretic Nash Equilibrium Orbit Detection

For each constraint with H¹>0 (non-constant orbit), computes:
  - nash_distance_structural: minimum single-observer type changes to reach H¹=0
  - nash_stable_structural: True if nash_distance_structural >= 2
  - vulnerable_positions: observer positions whose unilateral change could resolve H¹

"Structural" = uses any chain type as plausible change (lower bound on true Nash
distance). Future metric-constrained version (nash_distance_metric) would restrict
to types reachable under parameter perturbation.

Reads:  outputs/orbit_data.json
        outputs/pipeline_output.json

Writes: outputs/game_theory_nash.json

Usage:  python3 python/game_theory_nash.py
"""

import json
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, ORBIT_JSON, PIPELINE_JSON, OUTPUT_DIR

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CONTEXTS = ["powerless", "moderate", "institutional", "analytical"]
CHAIN_TYPES = ["mountain", "rope", "tangled_rope", "snare"]
OUTPUT_PATH = OUTPUT_DIR / "game_theory_nash.json"


# ---------------------------------------------------------------------------
# Nash distance computation
# ---------------------------------------------------------------------------

def type_vector(orbit_entry):
    """Extract ordered type vector from orbit_data entry."""
    ctx = orbit_entry["contexts"]
    return [ctx[c] for c in CONTEXTS]


def h1_from_vector(vec):
    """Count disagreeing pairs (H¹ proxy) for a 4-element type vector."""
    count = 0
    for i in range(4):
        for j in range(i + 1, 4):
            if vec[i] != vec[j]:
                count += 1
    return count


def is_constant(vec):
    """True if all 4 types are identical (H¹=0)."""
    return len(set(vec)) == 1


def compute_nash_single_observer(vec):
    """Try all single-observer type changes; return vulnerable positions.

    A position is "vulnerable" if changing that single observer's type
    to some chain type makes the entire vector constant (H¹→0).
    """
    vulnerable = []
    for pos in range(4):
        for new_type in CHAIN_TYPES:
            trial = vec.copy()
            trial[pos] = new_type
            if is_constant(trial):
                vulnerable.append(CONTEXTS[pos])
                break  # one resolution suffices to mark position as vulnerable
    return vulnerable


def compute_nash_two_observer(vec):
    """Check if any pair of observer changes can achieve H¹=0.

    Returns True if some 2-observer change resolves disagreement.
    """
    for p1 in range(4):
        for p2 in range(p1 + 1, 4):
            for t1 in CHAIN_TYPES:
                for t2 in CHAIN_TYPES:
                    trial = vec.copy()
                    trial[p1] = t1
                    trial[p2] = t2
                    if is_constant(trial):
                        return True
    return False


def compute_nash_distance(vec):
    """Compute structural Nash distance for a non-constant orbit.

    Returns (distance, vulnerable_positions):
      distance=1: some single observer change resolves H¹
      distance=2: requires 2 coordinated changes
      distance=3: requires 3 coordinated changes
      distance=4: requires all 4 observers to change (4 distinct types)
    """
    # Distance 1: single-observer resolution
    vulnerable = compute_nash_single_observer(vec)
    if vulnerable:
        return 1, vulnerable

    # Distance 2: two-observer resolution
    if compute_nash_two_observer(vec):
        return 2, []

    # Distance 3+: must be 3 or 4 distinct types
    # With 3 distinct types, 3 changes always suffice (change 3 to match the 4th)
    # With 4 distinct types, 3 changes suffice (change 3 to match remaining)
    unique = len(set(vec))
    if unique <= 4:
        return min(unique, 3), []

    return 4, []


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------

def main():
    orbit_data = load_json(ORBIT_JSON, "orbit_data")
    pipeline_data = load_json(PIPELINE_JSON, "pipeline_output")

    if not orbit_data or not pipeline_data:
        print("ERROR: Could not load required data files.", file=sys.stderr)
        sys.exit(1)

    # Build H¹ lookup from pipeline
    h1_lookup = {}
    sig_lookup = {}
    for c in pipeline_data.get("per_constraint", []):
        cid = c["id"]
        h1_lookup[cid] = c.get("h1_band", 0)
        sig_lookup[cid] = c.get("signature", "")

    # Process each constraint
    results = []
    for cid, entry in orbit_data.items():
        h1 = h1_lookup.get(cid, 0)
        vec = type_vector(entry)

        if h1 == 0 or is_constant(vec):
            # Dominant strategy exists — constant orbit
            results.append({
                "id": cid,
                "h1_band": h1,
                "orbit": {c: t for c, t in zip(CONTEXTS, vec)},
                "unique_types": len(set(vec)),
                "nash_distance_structural": 0,
                "nash_stable_structural": False,
                "vulnerable_positions": [],
                "has_dominant_strategy": True,
                "signature": sig_lookup.get(cid, ""),
            })
        else:
            dist, vuln = compute_nash_distance(vec)
            results.append({
                "id": cid,
                "h1_band": h1,
                "orbit": {c: t for c, t in zip(CONTEXTS, vec)},
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
