#!/usr/bin/env python3
"""
Persistence Sweep — grid sweep with H1 + W1 topology for persistence barcodes.

For each parameter, sweeps a grid of values across [0.5x, 2.0x] original,
collecting per-constraint classification, H1, and W1 at each grid point.
Computes persistence barcodes: birth/death intervals where topological
features are "active."

Reuses parse_config_params() from config_sensitivity_sweep.py.

Usage:
  python3 python/persistence_sweep.py --params snare_chi_floor
  python3 python/persistence_sweep.py --params "snare_chi|tangled_rope" --workers 4
  python3 python/persistence_sweep.py --params snare_chi_floor --grid-points 5  # quick test

Output:
  python/persistence_results.json
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from config_sensitivity_sweep import parse_config_params, EXCLUDE_PARAMS

# ---------------------------------------------------------------------------
# 1. Prolog overlay template — calls persistence_export
# ---------------------------------------------------------------------------

OVERLAY_TEMPLATE = """\
%% Auto-generated persistence overlay — DO NOT EDIT
%% Perturbs param({name}, {original}) → {perturbed}

:- use_module(config).

:- (   retract(config:param({name}, _))
   ->  true
   ;   true
   ),
   asserta(config:param({name}, {perturbed})).

:- [stack].
:- persistence_export:export_classifications_with_topology, halt.
"""


# ---------------------------------------------------------------------------
# 2. Run persistence export at one parameter value
# ---------------------------------------------------------------------------

def run_persistence_export(name, original, perturbed, prolog_dir, timeout_sec=60):
    """Run classification+H1+W1 export with one param perturbed."""
    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix=f"pers_{name}_")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(OVERLAY_TEMPLATE.format(
                name=name,
                original=original,
                perturbed=perturbed,
            ))

        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=prolog_dir,
        )
        return parse_persistence_output(proc.stdout + proc.stderr)

    except subprocess.TimeoutExpired:
        return None, None, None
    except Exception:
        return None, None, None
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass


def run_baseline_export(prolog_dir, timeout_sec=60):
    """Run persistence export with no perturbation (baseline)."""
    cmd = [
        "swipl", "-g",
        "[stack], persistence_export:export_classifications_with_topology, halt.",
    ]
    proc = subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        timeout=timeout_sec,
        cwd=prolog_dir,
    )
    return parse_persistence_output(proc.stdout + proc.stderr)


def parse_persistence_output(output):
    """Parse CLASSIFY/H1/W1 lines into three dicts."""
    classifications = {}   # {(constraint, context): type}
    h1_values = {}         # {constraint: int}
    w1_values = {}         # {constraint: (w12, w23, w34)}
    for line in output.splitlines():
        if line.startswith("CLASSIFY:"):
            parts = line.split(":")
            if len(parts) == 4:
                _, constraint, context, typ = parts
                classifications[(constraint, context)] = typ
        elif line.startswith("H1:"):
            parts = line.split(":")
            if len(parts) == 3:
                _, constraint, val = parts
                h1_values[constraint] = int(val)
        elif line.startswith("W1:"):
            parts = line.split(":")
            if len(parts) == 5:
                _, constraint, w12, w23, w34 = parts
                w1_values[constraint] = (
                    float(w12), float(w23), float(w34)
                )
    return classifications, h1_values, w1_values


# ---------------------------------------------------------------------------
# 3. Grid construction
# ---------------------------------------------------------------------------

def build_grid(original, n_points, critical_values=None):
    """Build parameter grid with adaptive refinement near critical values.

    Args:
        original: baseline parameter value
        n_points: number of base grid points
        critical_values: list of known critical values for adaptive refinement
    Returns:
        sorted list of grid point values
    """
    lo = original * 0.5
    hi = original * 2.0

    # Handle near-zero and negative
    if abs(original) < 1e-6:
        lo, hi = -2.0, 2.0
    elif original < 0:
        lo, hi = original * 2.0, original * 0.5

    # Base grid: linearly spaced
    step = (hi - lo) / max(n_points - 1, 1)
    grid = [round(lo + i * step, 8) for i in range(n_points)]

    # Adaptive refinement: 10 extra points in ±5% band around each critical value
    if critical_values:
        for cv in critical_values:
            band = abs(cv * 0.05) if abs(cv) > 1e-6 else 0.1
            band_lo = cv - band
            band_hi = cv + band
            band_step = (band_hi - band_lo) / 11
            for i in range(1, 11):
                pt = round(band_lo + i * band_step, 8)
                if lo <= pt <= hi:
                    grid.append(pt)

    # Deduplicate and sort
    grid = sorted(set(grid))
    return grid


def load_critical_values(param_name, bifurcation_path):
    """Load known critical values for a parameter from bifurcation results."""
    if not bifurcation_path.exists():
        return []
    try:
        with open(bifurcation_path) as f:
            data = json.load(f)
        for result in data.get("results", []):
            if result["param"] == param_name:
                cvs = []
                for cv in result.get("critical_values", []):
                    if cv.get("status") == "found":
                        cvs.append(cv["critical_value"])
                return cvs
    except (json.JSONDecodeError, KeyError):
        pass
    return []


# ---------------------------------------------------------------------------
# 4. Grid sweep for one parameter
# ---------------------------------------------------------------------------

# Worker function for parallel grid point evaluation (must be top-level for pickling)
def _grid_point_worker(args):
    name, original, perturbed, prolog_dir, timeout_sec = args
    classifications, h1_values, w1_values = run_persistence_export(
        name, original, perturbed, prolog_dir, timeout_sec
    )
    return perturbed, classifications, h1_values, w1_values


def sweep_parameter_grid(name, original, grid, prolog_dir,
                         workers=1, timeout_sec=60):
    """Sweep one parameter across a grid, collecting topology at each point.

    Returns list of (param_value, classifications, h1, w1) tuples, sorted by param_value.
    """
    results = []
    total = len(grid)

    if workers > 1:
        tasks = [
            (name, original, pval, prolog_dir, timeout_sec)
            for pval in grid
        ]
        with ProcessPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(_grid_point_worker, t): t[2] for t in tasks
            }
            completed = 0
            for future in as_completed(futures):
                pval = futures[future]
                completed += 1
                try:
                    result = future.result()
                    if result[1] is not None:
                        results.append(result)
                    print(f"  [{completed}/{total}] {name}={pval:.6f} OK")
                except Exception as e:
                    print(f"  [{completed}/{total}] {name}={pval:.6f} ERROR: {e}")
    else:
        for i, pval in enumerate(grid):
            classifications, h1_values, w1_values = run_persistence_export(
                name, original, pval, prolog_dir, timeout_sec
            )
            if classifications is not None:
                results.append((pval, classifications, h1_values, w1_values))
            print(f"  [{i+1}/{total}] {name}={pval:.6f} "
                  f"{'OK' if classifications else 'FAIL'}")

    # Sort by parameter value
    results.sort(key=lambda x: x[0])
    return results


# ---------------------------------------------------------------------------
# 5. Persistence barcode computation
# ---------------------------------------------------------------------------

def compute_persistence_bars(grid_results, param_name, baseline_snapshot,
                             w1_threshold=0.01):
    """Compute persistence barcodes from grid sweep results.

    Args:
        grid_results: sorted list of (param_value, classifications, h1, w1)
        param_name: name of the swept parameter
        baseline_snapshot: (classifications, h1, w1) at baseline
        w1_threshold: minimum W1 to count as "active"

    Returns:
        list of persistence bar dicts
    """
    if not grid_results:
        return []

    param_values = [r[0] for r in grid_results]
    sweep_range = param_values[-1] - param_values[0]

    # Identify constraints that change across the sweep (compare every step to baseline)
    baseline_cls, baseline_h1, baseline_w1 = baseline_snapshot
    changed_constraints = set()
    for _, cls, h1, w1 in grid_results:
        if cls is None:
            continue
        for key, typ in cls.items():
            if baseline_cls.get(key) != typ:
                constraint = key[0]
                changed_constraints.add(constraint)
        for c, val in h1.items():
            if baseline_h1.get(c) != val:
                changed_constraints.add(c)
        for c, (w12, w23, w34) in w1.items():
            bw = baseline_w1.get(c, (0, 0, 0))
            if (abs(w12 - bw[0]) > 0.001 or
                abs(w23 - bw[1]) > 0.001 or
                abs(w34 - bw[2]) > 0.001):
                changed_constraints.add(c)

    # Also include ALL constraints for H1 bars (even stable ones have barcodes)
    all_constraints = set()
    for _, _, h1, _ in grid_results:
        if h1:
            all_constraints.update(h1.keys())

    bars = []

    # --- H1 persistence bars ---
    for c in all_constraints:
        current_h1 = None
        birth_val = None
        for pval, _, h1, _ in grid_results:
            h1_val = h1.get(c)
            if h1_val != current_h1:
                # Close previous bar if it was active (H1 > 0)
                if current_h1 is not None and current_h1 > 0:
                    bars.append({
                        "constraint": c,
                        "dimension": "h1",
                        "param": param_name,
                        "value": current_h1,
                        "birth": birth_val,
                        "death": pval,
                        "duration": round(pval - birth_val, 8),
                    })
                # Open new bar if active
                if h1_val is not None and h1_val > 0:
                    birth_val = pval
                else:
                    birth_val = None
                current_h1 = h1_val
        # Close any open bar at range end
        if current_h1 is not None and current_h1 > 0 and birth_val is not None:
            bars.append({
                "constraint": c,
                "dimension": "h1",
                "param": param_name,
                "value": current_h1,
                "birth": birth_val,
                "death": None,
                "duration": round(param_values[-1] - birth_val, 8),
            })

    # --- W1 persistence bars (per edge) ---
    edge_names = ["w1_u1_u2", "w1_u2_u3", "w1_u3_u4"]
    for c in changed_constraints:
        for edge_idx, edge_name in enumerate(edge_names):
            active = False
            birth_val = None
            for pval, _, _, w1 in grid_results:
                w1_tuple = w1.get(c, (0.0, 0.0, 0.0))
                edge_val = w1_tuple[edge_idx]
                is_active = edge_val > w1_threshold
                if is_active and not active:
                    # Birth
                    birth_val = pval
                    active = True
                elif not is_active and active:
                    # Death
                    bars.append({
                        "constraint": c,
                        "dimension": edge_name,
                        "param": param_name,
                        "value": "active",
                        "birth": birth_val,
                        "death": pval,
                        "duration": round(pval - birth_val, 8),
                    })
                    active = False
                    birth_val = None
            # Close open bar
            if active and birth_val is not None:
                bars.append({
                    "constraint": c,
                    "dimension": edge_name,
                    "param": param_name,
                    "value": "active",
                    "birth": birth_val,
                    "death": None,
                    "duration": round(param_values[-1] - birth_val, 8),
                })

    # --- Type persistence bars (per context, changed constraints only) ---
    contexts = ["powerless", "moderate", "institutional", "analytical"]
    for c in changed_constraints:
        for ctx in contexts:
            current_type = None
            birth_val = None
            for pval, cls, _, _ in grid_results:
                typ = cls.get((c, ctx)) if cls else None
                if typ != current_type:
                    if current_type is not None:
                        bars.append({
                            "constraint": c,
                            "dimension": f"type:{ctx}",
                            "param": param_name,
                            "value": current_type,
                            "birth": birth_val,
                            "death": pval,
                            "duration": round(pval - birth_val, 8),
                        })
                    if typ is not None:
                        birth_val = pval
                    else:
                        birth_val = None
                    current_type = typ
            # Close open bar
            if current_type is not None and birth_val is not None:
                bars.append({
                    "constraint": c,
                    "dimension": f"type:{ctx}",
                    "param": param_name,
                    "value": current_type,
                    "birth": birth_val,
                    "death": None,
                    "duration": round(param_values[-1] - birth_val, 8),
                })

    return bars


# ---------------------------------------------------------------------------
# 6. Corpus summary and diagnosis
# ---------------------------------------------------------------------------

def corpus_persistence_summary(bars, sweep_range):
    """Classify bars as short/medium/long relative to sweep range."""
    if sweep_range <= 0:
        return {"short": 0, "medium": 0, "long": 0}
    short = medium = long_ = 0
    for bar in bars:
        frac = bar["duration"] / sweep_range
        if frac < 0.05:
            short += 1
        elif frac < 0.20:
            medium += 1
        else:
            long_ += 1
    return {"short": short, "medium": medium, "long": long_}


def classify_duration(duration, sweep_range):
    """Classify a single bar duration."""
    if sweep_range <= 0:
        return "UNKNOWN"
    frac = duration / sweep_range
    if frac < 0.05:
        return "FRAGILE"
    elif frac < 0.20:
        return "MODERATE"
    else:
        return "ROBUST"


SNARE_CHI_FLOOR_FLIP_CONSTRAINTS = [
    "autonomy_as_character_ideal",
    "conformity_extraction",
    "distributed_memory_as_counter_disposal",
    "epistemic_authority_erosion_through_unresolvable_anomaly",
    "exclusionary_coordination_asymmetry",
    "false_mountain_persistence",
    "hormuz_leverage_paradox",
    "institutional_mandate_vs_autonomy",
    "meritocratic_ideology_as_error_propagation",
    "purity_drift_degradation",
    "role_capture_through_cost_asymmetry",
    "solidarity_mirror_trap",
    "translation_labor_exhaustion",
    "witness_obligation_without_recipient",
]


def snare_chi_floor_diagnosis(bars, sweep_range):
    """Diagnose the 14 snare_chi_floor flip constraints specifically.

    Returns a list of per-constraint diagnosis dicts.
    """
    diagnoses = []
    for c in SNARE_CHI_FLOOR_FLIP_CONSTRAINTS:
        c_bars = [b for b in bars if b["constraint"] == c
                  and b["param"] == "snare_chi_floor"]
        h1_bars = [b for b in c_bars if b["dimension"] == "h1"]
        w1_bars = [b for b in c_bars if b["dimension"].startswith("w1_")]
        type_bars = [b for b in c_bars if b["dimension"] == "type:analytical"]

        # H1 diagnosis
        h1_total_duration = sum(b["duration"] for b in h1_bars)
        h1_classification = classify_duration(h1_total_duration, sweep_range)
        h1_ever_zero = h1_total_duration < sweep_range * 0.99

        # W1 diagnosis (u3_u4 edge is most relevant — analytical boundary)
        w1_u3u4 = [b for b in w1_bars if b["dimension"] == "w1_u3_u4"]
        w1_duration = sum(b["duration"] for b in w1_u3u4)
        w1_classification = classify_duration(w1_duration, sweep_range)

        diagnoses.append({
            "constraint": c,
            "h1_bars": h1_bars,
            "h1_total_duration": round(h1_total_duration, 6),
            "h1_classification": h1_classification,
            "h1_drops_to_zero": h1_ever_zero,
            "w1_u3u4_duration": round(w1_duration, 6),
            "w1_u3u4_classification": w1_classification,
            "type_analytical_bars": type_bars,
            "interpretation": (
                f"H1 persistence: {h1_classification} "
                f"({h1_total_duration/sweep_range*100:.1f}% of range). "
                f"W1(U3→U4): {w1_classification} "
                f"({w1_duration/sweep_range*100:.1f}% of range)."
                if sweep_range > 0 else "No sweep range"
            ),
        })

    return diagnoses


# ---------------------------------------------------------------------------
# 7. Collect changed-only grid data for JSON output
# ---------------------------------------------------------------------------

def collect_changed_grid_data(grid_results, baseline_snapshot):
    """Collect grid data only for constraints that deviate from baseline.

    Returns compact grid data dict: {constraint: [(pval, h1, w1, types), ...]}.
    "Changed" = any deviation from baseline across the FULL grid.
    """
    baseline_cls, baseline_h1, baseline_w1 = baseline_snapshot

    # First pass: identify changed constraints
    changed = set()
    for _, cls, h1, w1 in grid_results:
        if cls is None:
            continue
        for (c, ctx), typ in cls.items():
            if baseline_cls.get((c, ctx)) != typ:
                changed.add(c)
        for c, val in h1.items():
            if baseline_h1.get(c) != val:
                changed.add(c)
        for c, vals in w1.items():
            bw = baseline_w1.get(c, (0, 0, 0))
            if any(abs(a - b) > 0.001 for a, b in zip(vals, bw)):
                changed.add(c)

    # Second pass: collect data for changed constraints
    grid_data = {}
    for c in sorted(changed):
        trajectory = []
        for pval, cls, h1, w1 in grid_results:
            types = {}
            for ctx in ["powerless", "moderate", "institutional", "analytical"]:
                t = cls.get((c, ctx)) if cls else None
                if t:
                    types[ctx] = t
            trajectory.append({
                "p": pval,
                "h1": h1.get(c),
                "w1": list(w1.get(c, [])),
                "types": types,
            })
        grid_data[c] = trajectory

    return grid_data


# ---------------------------------------------------------------------------
# 8. Main orchestration
# ---------------------------------------------------------------------------

def run_persistence_sweep(config_path, prolog_dir, param_filter=None,
                          workers=1, grid_points=50, w1_threshold=0.01,
                          timeout_sec=60, include_grid_data=True):
    """Run persistence sweep for selected parameters."""
    base_dir = Path(prolog_dir).parent
    bifurcation_path = base_dir / "python" / "bifurcation_results.json"

    # Parse params
    params = parse_config_params(config_path)
    params = [p for p in params if p["name"] not in EXCLUDE_PARAMS]

    # Default: sweep only the 6 bifurcation params
    if param_filter is None:
        bif_params = set()
        if bifurcation_path.exists():
            try:
                with open(bifurcation_path) as f:
                    bif_data = json.load(f)
                for r in bif_data.get("results", []):
                    bif_params.add(r["param"])
            except (json.JSONDecodeError, KeyError):
                pass
        if bif_params:
            params = [p for p in params if p["name"] in bif_params]
        else:
            print("WARNING: No bifurcation_results.json found and no --params filter.")
            print("Use --params to select parameters to sweep.")
            return {}, None
    else:
        regex = re.compile(param_filter)
        params = [p for p in params if regex.search(p["name"])]

    if not params:
        print("No parameters matched filter.")
        return {}, None

    print(f"Persistence sweep: {len(params)} parameters, {grid_points} grid points each")
    print(f"W1 threshold: {w1_threshold}")
    print()

    # Baseline
    print("Capturing baseline topology...")
    t0 = time.time()
    baseline_cls, baseline_h1, baseline_w1 = run_baseline_export(prolog_dir, timeout_sec)
    t1 = time.time()
    if baseline_cls is None:
        print("ERROR: Baseline export failed")
        return {}, None
    print(f"Baseline: {len(baseline_cls)} classifications, "
          f"{len(baseline_h1)} H1 values, {len(baseline_w1)} W1 profiles "
          f"in {t1-t0:.1f}s")
    print()

    baseline_snapshot = (baseline_cls, baseline_h1, baseline_w1)

    all_results = {}
    for p in params:
        name = p["name"]
        original = p["value"]
        print(f"Sweeping {name} (baseline={original})...")

        # Build grid with adaptive refinement
        critical_values = load_critical_values(name, bifurcation_path)
        grid = build_grid(original, grid_points, critical_values)
        print(f"  Grid: {len(grid)} points in [{grid[0]:.6f}, {grid[-1]:.6f}]"
              + (f" (refined near {critical_values})" if critical_values else ""))

        # Sweep
        t0 = time.time()
        grid_results = sweep_parameter_grid(
            name, original, grid, prolog_dir,
            workers=workers, timeout_sec=timeout_sec,
        )
        t1 = time.time()
        print(f"  Completed {len(grid_results)}/{len(grid)} points in {t1-t0:.1f}s")

        # Compute persistence barcodes
        bars = compute_persistence_bars(
            grid_results, name, baseline_snapshot, w1_threshold
        )
        sweep_range = grid[-1] - grid[0] if grid else 0
        summary = corpus_persistence_summary(bars, sweep_range)

        param_result = {
            "param": name,
            "original": original,
            "grid_range": [grid[0], grid[-1]] if grid else [],
            "grid_points_requested": len(grid),
            "grid_points_completed": len(grid_results),
            "bars": bars,
            "summary": summary,
            "sweep_range": sweep_range,
        }

        # Grid data (changed constraints only)
        if include_grid_data:
            param_result["grid_data"] = collect_changed_grid_data(
                grid_results, baseline_snapshot
            )

        # snare_chi_floor diagnosis
        if name == "snare_chi_floor":
            param_result["snare_chi_floor_diagnosis"] = snare_chi_floor_diagnosis(
                bars, sweep_range
            )

        all_results[name] = param_result

        # Print summary
        h1_bars = [b for b in bars if b["dimension"] == "h1"]
        w1_bars = [b for b in bars if b["dimension"].startswith("w1_")]
        type_bars = [b for b in bars if b["dimension"].startswith("type:")]
        print(f"  Bars: {len(h1_bars)} H1, {len(w1_bars)} W1, {len(type_bars)} type")
        print(f"  Duration distribution: {summary}")
        print()

    return all_results, baseline_snapshot


def main():
    parser = argparse.ArgumentParser(
        description="Persistence Sweep — grid sweep with H1+W1 topology"
    )
    parser.add_argument(
        "--params", type=str, default=None,
        help="Regex filter for parameter names (default: 6 bifurcation params)"
    )
    parser.add_argument(
        "--workers", type=int, default=1,
        help="Parallel workers (default: 1)"
    )
    parser.add_argument(
        "--grid-points", type=int, default=50,
        help="Base grid points per parameter (default: 50)"
    )
    parser.add_argument(
        "--w1-threshold", type=float, default=0.01,
        help="W1 threshold for 'active' (default: 0.01)"
    )
    parser.add_argument(
        "--timeout", type=int, default=60,
        help="Timeout per Prolog invocation in seconds (default: 60)"
    )
    parser.add_argument(
        "--output", type=str, default=None,
        help="Output JSON path (default: python/persistence_results.json)"
    )
    parser.add_argument(
        "--no-grid-data", action="store_true",
        help="Omit raw grid data from output (bars only)"
    )
    args = parser.parse_args()

    base_dir = Path(__file__).resolve().parent.parent
    config_path = base_dir / "prolog" / "config.pl"
    prolog_dir = str(base_dir / "prolog")

    if not config_path.exists():
        print(f"ERROR: config.pl not found at {config_path}")
        sys.exit(1)

    output_path = args.output or str(base_dir / "python" / "persistence_results.json")

    results, baseline = run_persistence_sweep(
        str(config_path), prolog_dir,
        param_filter=args.params,
        workers=args.workers,
        grid_points=args.grid_points,
        w1_threshold=args.w1_threshold,
        timeout_sec=args.timeout,
        include_grid_data=not args.no_grid_data,
    )

    if not results:
        print("No results produced.")
        sys.exit(1)

    # Assemble output
    output = {
        "sweep_params": {
            "grid_points": args.grid_points,
            "w1_threshold": args.w1_threshold,
            "range_multiplier": [0.5, 2.0],
        },
        "params_swept": list(results.keys()),
        "results": results,
    }

    with open(output_path, "w") as f:
        json.dump(output, f, indent=2)
    print(f"Results saved to {output_path}")

    # Print snare_chi_floor diagnosis if available
    scf = results.get("snare_chi_floor")
    if scf and "snare_chi_floor_diagnosis" in scf:
        print("\n" + "=" * 70)
        print("SNARE_CHI_FLOOR DIAGNOSIS — 14 flip constraints")
        print("=" * 70)
        for d in scf["snare_chi_floor_diagnosis"]:
            print(f"\n  {d['constraint']}:")
            print(f"    {d['interpretation']}")
            if d["h1_drops_to_zero"]:
                print(f"    WARNING: H1 reaches 0 (fracture disappears) in part of range")
            for tb in d.get("type_analytical_bars", []):
                death = tb["death"]
                death_str = "end" if death is None else f"{death:.4f}"
                print(f"    type(analytical): {tb['value']} "
                      f"[{tb['birth']:.4f}, {death_str}] "
                      f"dur={tb['duration']:.4f}")


if __name__ == "__main__":
    main()
