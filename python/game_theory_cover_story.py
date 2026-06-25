#!/usr/bin/env python3
"""Game-Theoretic Cover Story Formalization

For each FCR constraint, determines whether the institutional reclassification
(the "cover story") is:
  - nash_forced: without FCR, orbit is still non-constant (H¹>0). The cover
    story is structurally locked — extraction is visible regardless of FCR.
  - contingent: without FCR, orbit becomes constant (H¹→0). FCR manufactures
    the perspectival fracture. The cover story wouldn't survive strategic analysis.
  - no_cover: constraint is not FCR-detected (no cover story to analyze).

Runs Prolog with fcr_override_enabled=0 to get without-FCR orbit data,
then cross-references with Extension 2 Nash distance results.

Reads:  outputs/game_theory_nash.json (Extension 2 output)
        outputs/pipeline_output.json
Runs:   Prolog with FCR disabled
Writes: outputs/game_theory_cover_story.json

Usage:  python3 python/game_theory_cover_story.py
"""

import json
import os
import subprocess
import sys
import tempfile
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, h1_band_or_raise, PIPELINE_JSON, OUTPUT_DIR

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

PROLOG_DIR = Path(__file__).resolve().parent.parent / "prolog"
NASH_PATH = OUTPUT_DIR / "game_theory_nash.json"
OUTPUT_PATH = OUTPUT_DIR / "game_theory_cover_story.json"
CONTEXTS = ["powerless", "moderate", "institutional", "analytical"]


# ---------------------------------------------------------------------------
# Prolog ablation query
# ---------------------------------------------------------------------------

OVERLAY_TEMPLATE = """\
%% Cover story ablation overlay — auto-generated
:- use_module(config).

:- (retract(config:param(fcr_override_enabled, _)) -> true ; true),
   asserta(config:param(fcr_override_enabled, 0)).

:- [stack].
:- use_module(grothendieck_cohomology).
:- use_module(logical_fingerprint).

:- corpus_loader:ensure_corpus_loaded,
   grothendieck_cohomology:cohomology_cleanup,
   findall(C0, logical_fingerprint:known_constraint(C0), Cs0),
   sort(Cs0, Cs),
   forall(
     member(C, Cs),
     (   catch(
           (   grothendieck_cohomology:orbit_vector(C, [T1, T2, T3, T4]),
               grothendieck_cohomology:cohomological_obstruction(C, H0, H1),
               format('DATA\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w~n',
                      [C, T1, T2, T3, T4, H0, H1])
           ),
           Error,
           format(user_error, 'ERROR on ~w: ~w~n', [C, Error])
         )
     )
   ),
   halt.
"""


def run_ablation():
    """Run Prolog with FCR disabled, return per-constraint orbit data."""
    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix="cover_story_")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(OVERLAY_TEMPLATE)

        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        print(f"Running Prolog ablation (FCR disabled)...")
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=300,
            cwd=str(PROLOG_DIR),
        )

        rows = {}
        for line in (proc.stdout + proc.stderr).split("\n"):
            if line.startswith("DATA\t"):
                parts = line.split("\t")
                if len(parts) == 8:
                    cid = parts[1]
                    rows[cid] = {
                        "t_powerless": parts[2],
                        "t_moderate": parts[3],
                        "t_institutional": parts[4],
                        "t_analytical": parts[5],
                        "h0": int(parts[6]),
                        "h1": int(parts[7]),
                    }
        print(f"  Collected {len(rows)} constraints from ablation run.")
        return rows
    finally:
        os.unlink(overlay_path)


# ---------------------------------------------------------------------------
# Main analysis
# ---------------------------------------------------------------------------

def main():
    nash_data = load_json(NASH_PATH, "game_theory_nash")
    pipeline_data = load_json(PIPELINE_JSON, "pipeline_output")

    if not nash_data or not pipeline_data:
        print("ERROR: Could not load required data files.", file=sys.stderr)
        sys.exit(1)

    # Build lookups
    nash_by_id = {r["id"]: r for r in nash_data.get("per_constraint", [])}
    sig_lookup = {}
    h1_lookup = {}
    perspectives_lookup = {}
    for c in pipeline_data.get("per_constraint", []):
        cid = c["id"]
        sig_lookup[cid] = c.get("signature", "")
        h1_lookup[cid] = h1_band_or_raise(c, "game_theory_cover_story")  # OQ-51: loud on null
        perspectives_lookup[cid] = c.get("perspectives", {})

    # Run ablation
    ablation = run_ablation()

    # Analyze each constraint
    results = []
    for cid in sorted(nash_by_id.keys()):
        sig = sig_lookup.get(cid, "")
        h1_with_fcr = h1_lookup.get(cid, 0)
        nash_info = nash_by_id[cid]
        abl = ablation.get(cid, None)

        if sig != "false_ci_rope":
            results.append({
                "id": cid,
                "cover_story_type": "no_cover",
                "h1_with_fcr": h1_with_fcr,
                "h1_without_fcr": abl["h1"] if abl else None,
                "nash_distance_structural": nash_info["nash_distance_structural"],
                "orbit_changed": None,
            })
            continue

        if abl is None:
            results.append({
                "id": cid,
                "cover_story_type": "unknown",
                "h1_with_fcr": h1_with_fcr,
                "h1_without_fcr": None,
                "nash_distance_structural": nash_info["nash_distance_structural"],
                "orbit_changed": None,
            })
            continue

        h1_without = abl["h1"]
        persp_with = perspectives_lookup.get(cid, {})
        orbit_with = [persp_with.get(c, "?") for c in CONTEXTS]
        orbit_without = [abl[f"t_{c}"] for c in CONTEXTS]
        orbit_changed = orbit_with != orbit_without

        if h1_with_fcr > 0 and h1_without > 0:
            cover_type = "nash_forced"         # Fracture persists without FCR
        elif h1_with_fcr > 0 and h1_without == 0:
            cover_type = "contingent"          # FCR manufactured the fracture
        elif orbit_changed:
            cover_type = "type_relabeled"      # Constant orbit, FCR changed type only
        else:
            cover_type = "fcr_no_structural_effect"  # FCR detected but orbit unchanged

        # Enrichment: which positions changed?
        changed_positions = [
            CONTEXTS[i] for i in range(4)
            if orbit_with[i] != orbit_without[i]
        ]

        results.append({
            "id": cid,
            "cover_story_type": cover_type,
            "h1_with_fcr": h1_with_fcr,
            "h1_without_fcr": h1_without,
            "h1_delta": h1_with_fcr - h1_without,
            "nash_distance_structural": nash_info["nash_distance_structural"],
            "orbit_with_fcr": {c: t for c, t in zip(CONTEXTS, orbit_with)},
            "orbit_without_fcr": {c: t for c, t in zip(CONTEXTS, orbit_without)},
            "orbit_changed": orbit_changed,
            "changed_positions": changed_positions,
        })

    # Corpus statistics
    fcr_results = [r for r in results if r["cover_story_type"] in ("nash_forced", "contingent")]
    cover_counter = Counter(r["cover_story_type"] for r in results)

    # Cover type × Nash distance
    cover_nash_cross = {}
    for r in fcr_results:
        ct = r["cover_story_type"]
        if ct not in cover_nash_cross:
            cover_nash_cross[ct] = Counter()
        cover_nash_cross[ct][f"nash={r['nash_distance_structural']}"] += 1
    cover_nash_cross = {k: dict(v) for k, v in cover_nash_cross.items()}

    # H¹ change distribution
    h1_delta_counter = Counter()
    for r in fcr_results:
        h1_delta_counter[r.get("h1_delta", 0)] += 1

    # Changed positions
    changed_pos_counter = Counter()
    for r in fcr_results:
        for p in r.get("changed_positions", []):
            changed_pos_counter[p] += 1

    summary = {
        "total_constraints": len(results),
        "cover_story_distribution": dict(cover_counter),
        "fcr_constraints_analyzed": len(fcr_results),
        "cover_type_x_nash": cover_nash_cross,
        "h1_delta_distribution": dict(sorted(h1_delta_counter.items())),
        "changed_positions_frequency": dict(changed_pos_counter.most_common()),
        "orbit_changed_count": sum(1 for r in fcr_results if r.get("orbit_changed")),
    }

    output = {
        "generated": datetime.now().isoformat(),
        "summary": summary,
        "per_constraint": results,
    }

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_PATH, "w", encoding="utf-8") as f:
        json.dump(output, f, indent=2)

    print(f"\nCover story analysis complete.")
    print(f"  Total: {summary['total_constraints']}")
    print(f"  Distribution: {summary['cover_story_distribution']}")
    print(f"  Cover type × Nash: {summary['cover_type_x_nash']}")
    print(f"  H¹ delta: {summary['h1_delta_distribution']}")
    print(f"  Changed positions: {summary['changed_positions_frequency']}")
    print(f"  Orbits changed by FCR: {summary['orbit_changed_count']}/{len(fcr_results)}")
    print(f"  Output: {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
