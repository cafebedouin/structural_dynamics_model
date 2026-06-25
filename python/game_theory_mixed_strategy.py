#!/usr/bin/env python3
"""Game-Theoretic Mixed Strategy / Dominant Strategy Analysis

For each constraint, computes:
  - has_dominant_strategy: True if orbit is constant (same type all observers)
  - dominant_type: the type if dominant strategy exists
  - mixed_strategy_candidate: MaxEnt distribution (global probabilistic compromise)
  - observer_deviation: TV distance from each observer's pure strategy to MaxEnt
  - max_deviation: maximum observer deviation (measures mixed equilibrium quality)
  - mixed_equilibrium_quality: "tight" / "loose" / "no_equilibrium"

Interpretation: The MaxEnt distribution IS the mixed strategy — the probabilistic
type assignment that maximizes entropy given metric constraints. Each observer plays
a pure strategy (deterministic type from perspective_chi). TV distance from each
observer's pure strategy to the MaxEnt distribution measures how much that observer
must "compromise" to reach the mixed equilibrium.

For constant orbits: the pure strategy IS the dominant strategy. No mixed strategy
needed — the game has a trivial solution.

Reads:  outputs/enriched_pipeline.json
        outputs/orbit_data.json

Writes: outputs/game_theory_mixed_strategy.json

Usage:  python3 python/game_theory_mixed_strategy.py
"""

import json
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, h1_band_or_raise, ENRICHED_PIPELINE_JSON, ORBIT_JSON, OUTPUT_DIR

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

CONTEXTS = ["powerless", "moderate", "institutional", "analytical"]
TYPES = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton", "naturalized"]
OUTPUT_PATH = OUTPUT_DIR / "game_theory_mixed_strategy.json"

# TV distance thresholds
TIGHT_THRESHOLD = 0.25    # max_deviation < 0.25 → tight mixed equilibrium
LOOSE_THRESHOLD = 0.75    # max_deviation < 0.75 → loose mixed equilibrium


# ---------------------------------------------------------------------------
# Total variation distance
# ---------------------------------------------------------------------------

def tv_distance(p, q):
    """Total variation distance between two distributions (as dicts)."""
    all_keys = set(p.keys()) | set(q.keys())
    return 0.5 * sum(abs(p.get(k, 0.0) - q.get(k, 0.0)) for k in all_keys)


def delta_distribution(type_name):
    """Pure strategy: delta distribution on a single type."""
    return {t: (1.0 if t == type_name else 0.0) for t in TYPES}


def empirical_distribution(type_vector):
    """Empirical distribution over types from 4 observers."""
    counts = Counter(type_vector)
    total = len(type_vector)
    return {t: counts.get(t, 0) / total for t in TYPES}


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------

def main():
    enriched = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline")
    orbit_data = load_json(ORBIT_JSON, "orbit_data")

    if not enriched or not orbit_data:
        print("ERROR: Could not load required data files.", file=sys.stderr)
        sys.exit(1)

    per_constraint_enriched = {c["id"]: c for c in enriched.get("per_constraint", [])}

    results = []
    for cid, orbit_entry in orbit_data.items():
        enriched_c = per_constraint_enriched.get(cid, {})
        perspectives = orbit_entry.get("contexts", {})
        type_vector = [perspectives.get(c, "unknown") for c in CONTEXTS]
        is_constant = len(set(type_vector)) == 1

        maxent = enriched_c.get("maxent_probs", {})
        # OQ-51: loud on a null h1_band when the constraint IS in enriched; a cid
        # entirely absent from enriched is a separate (pre-existing) join-coverage
        # concern, left at 0 so this change is scoped to the null-coercion bug.
        h1 = h1_band_or_raise(enriched_c, "game_theory_mixed_strategy") if enriched_c else 0

        if is_constant:
            results.append({
                "id": cid,
                "h1_band": h1,
                "has_dominant_strategy": True,
                "dominant_type": type_vector[0],
                "observer_deviation": {c: 0.0 for c in CONTEXTS},
                "max_deviation": 0.0,
                "mixed_equilibrium_quality": "dominant_strategy",
                "empirical_distribution": delta_distribution(type_vector[0]),
            })
            continue

        # Non-constant orbit: compute mixed strategy analysis
        empirical = empirical_distribution(type_vector)

        # Observer deviations from MaxEnt
        observer_dev_maxent = {}
        for ctx in CONTEXTS:
            pure = delta_distribution(perspectives.get(ctx, "unknown"))
            if maxent:
                observer_dev_maxent[ctx] = round(tv_distance(pure, maxent), 4)
            else:
                observer_dev_maxent[ctx] = None

        # Observer deviations from empirical distribution (the proper mixed
        # strategy candidate: uniform average of 4 observers' pure strategies)
        observer_dev_empirical = {}
        for ctx in CONTEXTS:
            pure = delta_distribution(perspectives.get(ctx, "unknown"))
            observer_dev_empirical[ctx] = round(tv_distance(pure, empirical), 4)

        max_dev_empirical = max(observer_dev_empirical.values())

        # Quality based on empirical deviation (the game-theoretically
        # meaningful measure: how far is each player from the mixed compromise?)
        if max_dev_empirical < TIGHT_THRESHOLD:
            quality = "tight"
        elif max_dev_empirical < LOOSE_THRESHOLD:
            quality = "loose"
        else:
            quality = "no_equilibrium"

        results.append({
            "id": cid,
            "h1_band": h1,
            "has_dominant_strategy": False,
            "dominant_type": None,
            "observer_deviation_maxent": observer_dev_maxent,
            "observer_deviation_empirical": observer_dev_empirical,
            "max_deviation_empirical": max_dev_empirical,
            "mixed_equilibrium_quality": quality,
            "empirical_distribution": empirical,
            "maxent_top_type": enriched_c.get("maxent_top_type", None),
            "maxent_entropy": enriched_c.get("maxent_entropy", None),
        })

    # Corpus statistics
    dominant = [r for r in results if r["has_dominant_strategy"]]
    non_dominant = [r for r in results if not r["has_dominant_strategy"]]

    quality_counter = Counter(r["mixed_equilibrium_quality"] for r in non_dominant)

    # Dominant strategy type distribution
    dom_type_counter = Counter(r["dominant_type"] for r in dominant)

    # Average max deviation by H¹ band
    h1_dev = {}
    for r in non_dominant:
        h1 = r["h1_band"]
        if h1 not in h1_dev:
            h1_dev[h1] = []
        if r.get("max_deviation_empirical") is not None:
            h1_dev[h1].append(r["max_deviation_empirical"])
    h1_mean_dev = {h1: round(sum(ds) / len(ds), 4) for h1, ds in sorted(h1_dev.items()) if ds}

    # MaxEnt convergence: how often does MaxEnt top_type match majority of observers?
    maxent_matches_majority = 0
    for r in non_dominant:
        if r.get("maxent_top_type"):
            majority_type = Counter(
                r["empirical_distribution"].get(t, 0) for t in TYPES
            )
            # More direct: check if maxent_top_type is the most common in orbit
            orbit_types = [r["empirical_distribution"][t] for t in TYPES]
            most_common_type = max(TYPES, key=lambda t: r["empirical_distribution"].get(t, 0))
            if r["maxent_top_type"] == most_common_type:
                maxent_matches_majority += 1

    summary = {
        "total_constraints": len(results),
        "dominant_strategy_count": len(dominant),
        "dominant_strategy_pct": round(100 * len(dominant) / max(len(results), 1), 1),
        "dominant_type_distribution": dict(dom_type_counter.most_common()),
        "non_dominant_count": len(non_dominant),
        "equilibrium_quality_distribution": dict(quality_counter),
        "h1_mean_max_deviation": h1_mean_dev,
        "maxent_matches_orbit_majority": maxent_matches_majority,
        "maxent_matches_orbit_majority_pct": round(
            100 * maxent_matches_majority / max(len(non_dominant), 1), 1
        ),
    }

    output = {
        "generated": datetime.now().isoformat(),
        "summary": summary,
        "per_constraint": results,
    }

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_PATH, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2)

    print(f"Mixed strategy analysis complete.")
    print(f"  Total: {summary['total_constraints']}")
    print(f"  Dominant strategy: {summary['dominant_strategy_count']} ({summary['dominant_strategy_pct']}%)")
    print(f"  Dominant type dist: {summary['dominant_type_distribution']}")
    print(f"  Non-dominant: {summary['non_dominant_count']}")
    print(f"  Equilibrium quality: {summary['equilibrium_quality_distribution']}")
    print(f"  H¹ → mean max deviation: {summary['h1_mean_max_deviation']}")
    print(f"  MaxEnt matches orbit majority: {summary['maxent_matches_orbit_majority']}"
          f" ({summary['maxent_matches_orbit_majority_pct']}%)")
    print(f"  Output: {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
