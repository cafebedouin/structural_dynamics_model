#!/usr/bin/env python3
"""
Cognitive Displacement Sweep — measures classification boundary thickness
in directionality space by perturbing δ (cognitive displacement).

Phase A: Uniform sweep — same δ applied to all observer positions.
Phase B: Positional sweep — δ applied to one position at a time, others held at 0.

Usage:
  python3 python/cognitive_displacement_sweep.py                    # both phases
  python3 python/cognitive_displacement_sweep.py --phase uniform    # Phase A only
  python3 python/cognitive_displacement_sweep.py --phase positional # Phase B only
  python3 python/cognitive_displacement_sweep.py --points 11        # fewer sweep points
  python3 python/cognitive_displacement_sweep.py --workers 4        # parallel workers

Output:
  python/cognitive_displacement_results.json
"""

import argparse
import json
import os
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

PROLOG_DIR = Path(__file__).resolve().parents[2] / "prolog"
RESULTS_PATH = Path(__file__).resolve().parent / "cognitive_displacement_results.json"

# Standard observer positions (must match drl_core:standard_context/1)
STANDARD_POWERS = ["powerless", "moderate", "institutional", "analytical"]

# All power atoms (for positional_displacement/2 fact table)
ALL_POWERS = ["powerless", "moderate", "powerful", "organized", "institutional", "analytical"]


# ---------------------------------------------------------------------------
# 1. Overlay templates
# ---------------------------------------------------------------------------

UNIFORM_OVERLAY = """\
%% Auto-generated cognitive displacement overlay — DO NOT EDIT
%% Uniform delta = {delta}

:- use_module(config).

:- (   retract(config:param(cognitive_displacement, _))
   ->  true
   ;   true
   ),
   asserta(config:param(cognitive_displacement, {delta})).

:- [stack].
:- bifurcation_export:export_all_classifications, halt.
"""

POSITIONAL_OVERLAY = """\
%% Auto-generated cognitive displacement overlay — DO NOT EDIT
%% Positional: {target_power} = {delta}, all others = 0.0

:- use_module(config).
:- use_module(constraint_indexing).

%% Switch to positional mode
:- (   retract(config:param(cognitive_displacement_profile, _))
   ->  true
   ;   true
   ),
   asserta(config:param(cognitive_displacement_profile, positional)).

%% Set per-position displacements
{positional_assertions}

:- [stack].
:- bifurcation_export:export_all_classifications, halt.
"""


def build_positional_assertions(target_power, delta):
    """Build retract/assert block for positional_displacement/2 facts."""
    lines = []
    for power in ALL_POWERS:
        val = delta if power == target_power else 0.0
        lines.append(
            f":- (   retract(constraint_indexing:positional_displacement({power}, _))\n"
            f"   ->  true\n"
            f"   ;   true\n"
            f"   ),\n"
            f"   asserta(constraint_indexing:positional_displacement({power}, {val}))."
        )
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# 2. Run classification export
# ---------------------------------------------------------------------------

def run_export(overlay_content, timeout_sec=60):
    """Run classification export with a given overlay. Returns classification dict or None."""
    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix="cds_")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(overlay_content)

        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=str(PROLOG_DIR),
        )
        return parse_classify_output(proc.stdout + proc.stderr)

    except subprocess.TimeoutExpired:
        return None
    except Exception as e:
        print(f"  [ERROR] {e}", file=sys.stderr)
        return None
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass


def run_baseline(timeout_sec=60):
    """Run classification export with no perturbation."""
    cmd = [
        "swipl", "-g",
        "[stack], bifurcation_export:export_all_classifications, halt.",
    ]
    proc = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        timeout=timeout_sec,
        cwd=str(PROLOG_DIR),
    )
    return parse_classify_output(proc.stdout + proc.stderr)


def parse_classify_output(output):
    """Parse CLASSIFY lines into {(constraint, power): type} dict."""
    classifications = {}
    for line in output.splitlines():
        if line.startswith("CLASSIFY:"):
            parts = line.split(":")
            if len(parts) == 4:
                _, constraint, power, typ = parts
                classifications[(constraint, power)] = typ
    return classifications


def diff_classifications(baseline, perturbed):
    """Compare two classification dicts. Returns list of flip dicts."""
    flips = []
    for key, base_type in baseline.items():
        pert_type = perturbed.get(key)
        if pert_type and pert_type != base_type:
            constraint, power = key
            flips.append({
                "constraint": constraint,
                "power": power,
                "from": base_type,
                "to": pert_type,
            })
    return flips


# ---------------------------------------------------------------------------
# 3. Sweep worker functions
# ---------------------------------------------------------------------------

def run_uniform_point(delta, timeout_sec=60):
    """Run one uniform sweep point. Returns (delta, flips_list)."""
    overlay = UNIFORM_OVERLAY.format(delta=delta)
    result = run_export(overlay, timeout_sec)
    return (delta, result)


def run_positional_point(target_power, delta, timeout_sec=60):
    """Run one positional sweep point. Returns (target_power, delta, result)."""
    assertions = build_positional_assertions(target_power, delta)
    overlay = POSITIONAL_OVERLAY.format(
        target_power=target_power,
        delta=delta,
        positional_assertions=assertions,
    )
    result = run_export(overlay, timeout_sec)
    return (target_power, delta, result)


# ---------------------------------------------------------------------------
# 4. Phase A: Uniform sweep
# ---------------------------------------------------------------------------

def sweep_uniform(baseline, delta_points, max_workers=2, timeout_sec=60):
    """Run uniform sweep across all delta points."""
    print(f"\n{'='*60}")
    print(f"PHASE A: Uniform Sweep ({len(delta_points)} points)")
    print(f"{'='*60}")

    results = []
    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        futures = {
            executor.submit(run_uniform_point, d, timeout_sec): d
            for d in delta_points if abs(d) > 1e-9  # skip delta=0 (baseline)
        }
        for future in as_completed(futures):
            delta, export = future.result()
            if export is None:
                print(f"  delta={delta:+.4f}: TIMEOUT/ERROR")
                results.append({"delta": delta, "status": "error", "flips": []})
                continue
            flips = diff_classifications(baseline, export)
            print(f"  delta={delta:+.4f}: {len(flips)} flips")
            results.append({
                "delta": delta,
                "status": "ok",
                "flip_count": len(flips),
                "flips": flips,
            })

    # Add baseline point
    results.append({"delta": 0.0, "status": "ok", "flip_count": 0, "flips": []})
    results.sort(key=lambda r: r["delta"])

    # Compute per-constraint first-flip delta
    first_flip = compute_first_flip(results)

    return {
        "sweep_points": results,
        "first_flip_delta": first_flip,
        "total_constraints_flipped": len(first_flip),
    }


def compute_first_flip(sweep_points):
    """For each constraint that flipped, find the smallest |delta| that caused it."""
    first_flip = {}
    for pt in sorted(sweep_points, key=lambda p: abs(p["delta"])):
        for flip in pt.get("flips", []):
            key = (flip["constraint"], flip["power"])
            if key not in first_flip:
                first_flip[key] = {
                    "constraint": flip["constraint"],
                    "power": flip["power"],
                    "first_flip_delta": pt["delta"],
                    "abs_delta": abs(pt["delta"]),
                    "from": flip["from"],
                    "to": flip["to"],
                }
    # Sort by abs_delta (most fragile first)
    return sorted(first_flip.values(), key=lambda x: x["abs_delta"])


# ---------------------------------------------------------------------------
# 5. Phase B: Positional sweep
# ---------------------------------------------------------------------------

def sweep_positional(baseline, delta_points, max_workers=2, timeout_sec=60):
    """Run positional sweep: perturb one position at a time."""
    print(f"\n{'='*60}")
    print(f"PHASE B: Positional Sweep ({len(STANDARD_POWERS)} positions × {len(delta_points)} points)")
    print(f"{'='*60}")

    all_results = {}

    for target_power in STANDARD_POWERS:
        print(f"\n  Position: {target_power}")
        position_results = []

        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            futures = {
                executor.submit(run_positional_point, target_power, d, timeout_sec): d
                for d in delta_points if abs(d) > 1e-9
            }
            for future in as_completed(futures):
                tp, delta, export = future.result()
                if export is None:
                    print(f"    delta={delta:+.4f}: TIMEOUT/ERROR")
                    position_results.append({"delta": delta, "status": "error", "flips": []})
                    continue
                flips = diff_classifications(baseline, export)
                print(f"    delta={delta:+.4f}: {len(flips)} flips")
                position_results.append({
                    "delta": delta,
                    "status": "ok",
                    "flip_count": len(flips),
                    "flips": flips,
                })

        position_results.append({"delta": 0.0, "status": "ok", "flip_count": 0, "flips": []})
        position_results.sort(key=lambda r: r["delta"])

        first_flip = compute_first_flip(position_results)

        all_results[target_power] = {
            "sweep_points": position_results,
            "first_flip_delta": first_flip,
            "total_constraints_flipped": len(first_flip),
        }

    return all_results


# ---------------------------------------------------------------------------
# 6. Summary statistics
# ---------------------------------------------------------------------------

def summarize_results(uniform, positional):
    """Compute summary statistics across both phases."""
    summary = {
        "uniform": {
            "total_flipped": uniform["total_constraints_flipped"],
            "most_fragile": uniform["first_flip_delta"][:5] if uniform["first_flip_delta"] else [],
        },
    }

    if positional:
        pos_summary = {}
        for power, data in positional.items():
            pos_summary[power] = {
                "total_flipped": data["total_constraints_flipped"],
                "most_fragile": data["first_flip_delta"][:3] if data["first_flip_delta"] else [],
            }
        summary["positional"] = pos_summary

        # Rank positions by sensitivity
        ranked = sorted(positional.items(), key=lambda kv: kv[1]["total_constraints_flipped"], reverse=True)
        summary["position_sensitivity_ranking"] = [
            {"power": p, "flips": d["total_constraints_flipped"]} for p, d in ranked
        ]

    return summary


# ---------------------------------------------------------------------------
# 7. Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Cognitive displacement sweep")
    parser.add_argument("--phase", choices=["uniform", "positional", "both"], default="both")
    parser.add_argument("--points", type=int, default=21, help="Number of sweep points (default: 21)")
    parser.add_argument("--range", type=float, default=0.15, help="Max |delta| (default: 0.15)")
    parser.add_argument("--workers", type=int, default=2, help="Parallel workers (default: 2)")
    parser.add_argument("--timeout", type=int, default=60, help="Per-run timeout in seconds")
    args = parser.parse_args()

    delta_points = [
        round(-args.range + i * (2 * args.range / (args.points - 1)), 6)
        for i in range(args.points)
    ]
    print(f"Delta points ({len(delta_points)}): {delta_points[0]:+.4f} to {delta_points[-1]:+.4f}")

    # Baseline
    print("\nRunning baseline classification export...")
    t0 = time.time()
    baseline = run_baseline(args.timeout)
    if baseline is None:
        print("ERROR: Baseline export failed.", file=sys.stderr)
        sys.exit(1)
    print(f"  Baseline: {len(baseline)} classifications ({time.time() - t0:.1f}s)")

    # Phase A
    uniform_results = None
    if args.phase in ("uniform", "both"):
        t0 = time.time()
        uniform_results = sweep_uniform(baseline, delta_points, args.workers, args.timeout)
        print(f"\n  Phase A complete: {uniform_results['total_constraints_flipped']} constraints flipped ({time.time() - t0:.1f}s)")

    # Phase B
    positional_results = None
    if args.phase in ("positional", "both"):
        t0 = time.time()
        positional_results = sweep_positional(baseline, delta_points, args.workers, args.timeout)
        total_pos = sum(d["total_constraints_flipped"] for d in positional_results.values())
        print(f"\n  Phase B complete: {total_pos} total position-specific flips ({time.time() - t0:.1f}s)")

    # Assemble output
    output = {
        "metadata": {
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
            "delta_range": [-args.range, args.range],
            "delta_points": delta_points,
            "baseline_count": len(baseline),
        },
    }
    if uniform_results:
        output["uniform"] = uniform_results
    if positional_results:
        output["positional"] = positional_results

    # Summary
    if uniform_results:
        output["summary"] = summarize_results(
            uniform_results,
            positional_results,
        )

    with open(RESULTS_PATH, "w") as f:
        json.dump(output, f, indent=2)
    print(f"\nResults written to {RESULTS_PATH}")


if __name__ == "__main__":
    main()
