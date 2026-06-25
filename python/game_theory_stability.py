#!/usr/bin/env python3
"""Game-Theoretic Strategic Stability Analysis

Interprets H¹ persistence barcodes as manipulation resistance:
  - h1_persistence_max: longest H¹ bar duration (0.0-1.0 of parameter range)
  - h1_persistence_mean: mean H¹ bar duration
  - strategic_stability: "resistant" if max > 0.5, "vulnerable" otherwise
  - h1_trajectory: H¹ values across parameter grid

High-persistence H¹ bars = extraction that can't be classified away by
strategic metric adjustment. A constraint with h1_persistence_max=0.95
has perspectival fracture surviving nearly the entire parameter range.

Reads:  python/persistence_results.json
        outputs/pipeline_output.json

Writes: outputs/game_theory_stability.json

Usage:  python3 python/game_theory_stability.py
"""

import json
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, h1_band_or_raise, PIPELINE_JSON, OUTPUT_DIR
from corpus_hash import compute_corpus_hash  # single-source corpus fingerprint (OQ-29)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

PERSISTENCE_PATH = Path(__file__).resolve().parent / "persistence_results.json"
OUTPUT_PATH = OUTPUT_DIR / "game_theory_stability.json"
RESISTANCE_THRESHOLD = 0.5  # H¹ bar surviving >50% of range = resistant


# ---------------------------------------------------------------------------
# Analysis
# ---------------------------------------------------------------------------

def main():
    # Load persistence data
    persistence = load_json(PERSISTENCE_PATH, "persistence_results")
    pipeline = load_json(PIPELINE_JSON, "pipeline_output")

    if not persistence or not pipeline:
        print("ERROR: Could not load required data files.", file=sys.stderr)
        sys.exit(1)

    # Build H¹ lookup
    h1_lookup = {}
    for c in pipeline.get("per_constraint", []):
        h1_lookup[c["id"]] = h1_band_or_raise(c, "game_theory_stability")  # OQ-51: loud on null

    # Extract bars and grid data from first (and currently only) swept param
    results_dict = persistence.get("results", {})
    if not results_dict:
        print("ERROR: No sweep results found.", file=sys.stderr)
        sys.exit(1)

    param_name = list(results_dict.keys())[0]
    param_data = results_dict[param_name]
    bars = param_data.get("bars", [])
    grid_data = param_data.get("grid_data", {})
    sweep_range = param_data.get("sweep_range", 1.0)

    print(f"Analyzing persistence for param: {param_name}")
    print(f"  Total bars: {len(bars)}")
    print(f"  Sweep range: {sweep_range}")

    # Extract H¹ bars per constraint
    h1_bars_by_constraint = {}
    for bar in bars:
        if bar["dimension"] == "h1":
            cid = bar["constraint"]
            if cid not in h1_bars_by_constraint:
                h1_bars_by_constraint[cid] = []
            h1_bars_by_constraint[cid].append({
                "value": bar["value"],
                "birth": bar["birth"],
                "death": bar["death"],
                "duration": bar["duration"],
                "normalized_duration": bar["duration"] / sweep_range if sweep_range > 0 else 0,
            })

    # Build per-constraint results
    per_constraint = []
    all_constraint_ids = set(h1_lookup.keys())

    for cid in sorted(all_constraint_ids):
        h1 = h1_lookup.get(cid, 0)
        bars_for_c = h1_bars_by_constraint.get(cid, [])
        grid_for_c = grid_data.get(cid, [])

        if h1 == 0 and not bars_for_c:
            # H¹=0 at baseline and no H¹ bars — skip detailed analysis
            per_constraint.append({
                "id": cid,
                "h1_band": h1,
                "h1_persistence_max": 0.0,
                "h1_persistence_mean": 0.0,
                "strategic_stability": "not_applicable",
                "h1_bar_count": 0,
                "h1_trajectory": None,
            })
            continue

        # Compute persistence metrics
        if bars_for_c:
            durations = [b["normalized_duration"] for b in bars_for_c]
            max_dur = max(durations)
            mean_dur = sum(durations) / len(durations)
        else:
            max_dur = 0.0
            mean_dur = 0.0

        # Extract trajectory
        trajectory = None
        if grid_for_c:
            trajectory = [{"p": pt["p"], "h1": pt["h1"]} for pt in grid_for_c]

        stability = "resistant" if max_dur > RESISTANCE_THRESHOLD else "vulnerable"
        if h1 == 0 and bars_for_c:
            # H¹=0 at baseline but acquires H¹ under perturbation
            stability = "latent_vulnerable"

        per_constraint.append({
            "id": cid,
            "h1_band": h1,
            "h1_persistence_max": round(max_dur, 4),
            "h1_persistence_mean": round(mean_dur, 4),
            "strategic_stability": stability,
            "h1_bar_count": len(bars_for_c),
            "h1_trajectory": trajectory,
        })

    # Corpus statistics
    non_trivial = [r for r in per_constraint if r["strategic_stability"] != "not_applicable"]
    stability_counter = Counter(r["strategic_stability"] for r in non_trivial)
    h1_pos = [r for r in non_trivial if r["h1_band"] > 0]

    # Persistence distribution for H¹>0
    if h1_pos:
        max_durs = [r["h1_persistence_max"] for r in h1_pos]
        persistence_buckets = {
            "ultra_resistant_gt_0.9": sum(1 for d in max_durs if d > 0.9),
            "resistant_0.5_to_0.9": sum(1 for d in max_durs if 0.5 < d <= 0.9),
            "vulnerable_0.2_to_0.5": sum(1 for d in max_durs if 0.2 < d <= 0.5),
            "fragile_le_0.2": sum(1 for d in max_durs if d <= 0.2),
        }
    else:
        persistence_buckets = {}

    summary = {
        "param_swept": param_name,
        "total_constraints": len(per_constraint),
        "constraints_with_h1_bars": len(h1_bars_by_constraint),
        "constraints_in_grid": len(grid_data),
        "stability_distribution": dict(stability_counter),
        "persistence_buckets_h1_pos": persistence_buckets,
        "resistance_threshold": RESISTANCE_THRESHOLD,
        "methodology": "H¹ persistence barcode duration as fraction of parameter sweep range",
    }

    output = {
        "corpus_hash": compute_corpus_hash(Path(__file__).resolve().parent.parent / "prolog" / "testsets"),
        "generated": datetime.now().isoformat(),
        "summary": summary,
        "per_constraint": per_constraint,
    }

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_PATH, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2)

    print(f"\nStrategic stability analysis complete.")
    print(f"  Constraints with H¹ bars: {len(h1_bars_by_constraint)}")
    print(f"  Stability distribution: {dict(stability_counter)}")
    print(f"  Persistence buckets (H¹>0): {persistence_buckets}")
    print(f"  Output: {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
