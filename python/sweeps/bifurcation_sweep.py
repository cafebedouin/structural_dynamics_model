#!/usr/bin/env python3
"""
Bifurcation Sweep — finds exact parameter values where type labels flip.

For each numeric param in config.pl, performs a binary search over
[0.5×original, 2.0×original] to locate the critical value at which any
constraint's classification changes. Records per-constraint type transitions.

Reuses parse_config_params() from config_sensitivity_sweep.py.

Usage:
  python3 python/bifurcation_sweep.py [--params PATTERN] [--workers N]
  python3 python/bifurcation_sweep.py --params rope_chi_ceiling  # single-param test

Output:
  python/bifurcation_results.json
"""

# NOTE: This sweep exercises the sigmoid classification path (drl_core:dr_type/3) only.
# The legacy power_modifier/2 path in drl_composition.pl, transition_paths.pl, and
# drl_audit_core.pl is NOT covered by this sweep.
# TODO: Extend coverage when legacy path is migrated to sigmoid. See: legacy-power-modifier-migration.

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

# Import param parser from existing sweep
sys.path.insert(0, str(Path(__file__).resolve().parent))
from config_sensitivity_sweep import parse_config_params, EXCLUDE_PARAMS

# ---------------------------------------------------------------------------
# 1. Prolog overlay template — runs classification export instead of test suite
# ---------------------------------------------------------------------------

OVERLAY_TEMPLATE = """\
%% Auto-generated bifurcation overlay — DO NOT EDIT
%% Perturbs param({name}, {original}) → {perturbed}

:- use_module(config).

:- (   retract(config:param({name}, _))
   ->  true
   ;   true
   ),
   asserta(config:param({name}, {perturbed})).

:- [stack].
:- bifurcation_export:export_all_classifications, halt.
"""


# ---------------------------------------------------------------------------
# 2. Run classification export with one param perturbed
# ---------------------------------------------------------------------------

def run_classification_export(name, original, perturbed, prolog_dir, timeout_sec=30):
    """Run classification export with one param perturbed. Returns dict of classifications."""
    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix=f"bif_{name}_")
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
        return parse_classify_output(proc.stdout + proc.stderr)

    except subprocess.TimeoutExpired:
        return None
    except Exception:
        return None
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass


def run_baseline_export(prolog_dir, timeout_sec=30):
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
        cwd=prolog_dir,
    )
    return parse_classify_output(proc.stdout + proc.stderr)


def parse_classify_output(output):
    """Parse CLASSIFY lines into {(constraint, context): type} dict."""
    classifications = {}
    for line in output.splitlines():
        if line.startswith("CLASSIFY:"):
            parts = line.split(":")
            if len(parts) == 4:
                _, constraint, context, typ = parts
                classifications[(constraint, context)] = typ
    return classifications


def diff_classifications(baseline, perturbed):
    """Compare two classification dicts. Returns list of flip dicts."""
    flips = []
    for key, base_type in baseline.items():
        pert_type = perturbed.get(key)
        if pert_type and pert_type != base_type:
            constraint, context = key
            flips.append({
                "constraint": constraint,
                "context": context,
                "from": base_type,
                "to": pert_type,
            })
    return flips


# ---------------------------------------------------------------------------
# 3. Binary search for bifurcation point
# ---------------------------------------------------------------------------

def find_bifurcation(name, original, direction, baseline, prolog_dir,
                     max_iters=15, tolerance_frac=0.01, timeout_sec=30):
    """Binary search for the critical value where classifications first flip.

    Args:
        direction: "up" or "down"
        Returns: dict with critical_value, tolerance, flips, iterations
                 or None if no bifurcation found in range
    """
    if direction == "up":
        lo, hi = original, original * 2.0
    else:
        lo, hi = original * 0.5, original

    # Handle near-zero and negative originals
    if abs(original) < 1e-6:
        if direction == "up":
            lo, hi = 0.0, 2.0
        else:
            lo, hi = -2.0, 0.0
    elif original < 0:
        if direction == "up":
            lo, hi = original, original * 0.5  # toward zero
        else:
            lo, hi = original * 2.0, original  # away from zero

    # First check: does the boundary produce any flips?
    boundary = hi if direction == "up" else lo
    boundary_result = run_classification_export(name, original, boundary, prolog_dir, timeout_sec)
    if boundary_result is None:
        return None

    boundary_flips = diff_classifications(baseline, boundary_result)
    if not boundary_flips:
        # No flips at boundary — no bifurcation in this range
        return {"status": "no_flip_in_range", "boundary_tested": boundary}

    # Binary search: find the value closest to original where flips first appear
    for i in range(max_iters):
        mid = (lo + hi) / 2.0
        mid = round(mid, 6)

        # Convergence check
        if abs(hi - lo) < abs(original * tolerance_frac) if abs(original) > 1e-6 else abs(hi - lo) < tolerance_frac:
            break

        mid_result = run_classification_export(name, original, mid, prolog_dir, timeout_sec)
        if mid_result is None:
            break

        mid_flips = diff_classifications(baseline, mid_result)
        if mid_flips:
            # Flips exist at mid — narrow toward original
            if direction == "up":
                hi = mid
            else:
                lo = mid
        else:
            # No flips at mid — narrow away from original
            if direction == "up":
                lo = mid
            else:
                hi = mid

    # Final classification at the converged critical point
    critical = hi if direction == "up" else lo
    critical = round(critical, 6)
    final_result = run_classification_export(name, original, critical, prolog_dir, timeout_sec)
    if final_result is None:
        final_flips = boundary_flips
    else:
        final_flips = diff_classifications(baseline, final_result)
        if not final_flips:
            final_flips = boundary_flips
            critical = boundary

    return {
        "status": "found",
        "direction": direction,
        "critical_value": critical,
        "tolerance": round(abs(hi - lo), 6),
        "iterations": i + 1 if 'i' in dir() else 0,
        "flips": final_flips,
        "flip_count": len(final_flips),
    }


# ---------------------------------------------------------------------------
# 4. Per-parameter sweep
# ---------------------------------------------------------------------------

def sweep_one_param(name, original, baseline, prolog_dir, timeout_sec=30):
    """Find bifurcation points for one parameter in both directions."""
    results = {"param": name, "original": original, "critical_values": []}

    for direction in ("up", "down"):
        result = find_bifurcation(
            name, original, direction, baseline, prolog_dir,
            timeout_sec=timeout_sec,
        )
        if result:
            results["critical_values"].append(result)

    return results


# Worker function for parallel execution (must be top-level for pickling)
def _sweep_worker(args):
    name, original, baseline, prolog_dir, timeout_sec = args
    return sweep_one_param(name, original, baseline, prolog_dir, timeout_sec)


# ---------------------------------------------------------------------------
# 5. Main orchestration
# ---------------------------------------------------------------------------

def run_bifurcation_sweep(config_path, prolog_dir, param_filter=None,
                          workers=1, timeout_sec=30):
    """Run bifurcation analysis for all numeric config params."""
    params = parse_config_params(config_path)
    params = [p for p in params if p["name"] not in EXCLUDE_PARAMS]
    if param_filter:
        regex = re.compile(param_filter)
        params = [p for p in params if regex.search(p["name"])]

    print(f"Found {len(params)} parameters to analyze")
    print(f"Estimated max Prolog invocations: {len(params) * 2 * 16}")
    print()

    # Baseline
    print("Capturing baseline classifications...")
    t0 = time.time()
    baseline = run_baseline_export(prolog_dir, timeout_sec=timeout_sec)
    t1 = time.time()
    print(f"Baseline: {len(baseline)} classifications in {t1-t0:.1f}s")
    print()

    if not baseline:
        print("ERROR: Baseline export failed")
        return [], baseline

    # Sweep
    all_results = []
    completed = 0
    total = len(params)

    if workers > 1:
        tasks = [
            (p["name"], p["value"], baseline, prolog_dir, timeout_sec)
            for p in params
        ]
        with ProcessPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(_sweep_worker, t): t[0] for t in tasks
            }
            for future in as_completed(futures):
                name = futures[future]
                completed += 1
                try:
                    result = future.result()
                    all_results.append(result)
                    _print_progress(completed, total, result)
                except Exception as e:
                    print(f"  [{completed}/{total}] {name} ERROR: {e}")
    else:
        for p in params:
            completed += 1
            result = sweep_one_param(
                p["name"], p["value"], baseline, prolog_dir, timeout_sec
            )
            all_results.append(result)
            _print_progress(completed, total, result)

    return all_results, baseline


def _print_progress(completed, total, result):
    """Print one-line progress for a completed parameter."""
    name = result["param"]
    found = [cv for cv in result["critical_values"] if cv.get("status") == "found"]
    no_flip = [cv for cv in result["critical_values"] if cv.get("status") == "no_flip_in_range"]

    if found:
        for cv in found:
            print(f"  [{completed}/{total}] {name} {cv['direction']}: "
                  f"critical={cv['critical_value']:.4f} ({cv['flip_count']} flips, "
                  f"{cv['iterations']} iters)")
    elif no_flip:
        print(f"  [{completed}/{total}] {name}: inert in [0.5x, 2.0x] range")
    else:
        print(f"  [{completed}/{total}] {name}: error")


def print_summary_table(results):
    """Print markdown summary table."""
    print("\n## Bifurcation Summary\n")
    print("| Parameter | Original | Dir | Critical Value | Flips | Distance |")
    print("|-----------|----------|-----|----------------|-------|----------|")

    for r in sorted(results, key=lambda x: x["param"]):
        found = [cv for cv in r["critical_values"] if cv.get("status") == "found"]
        if not found:
            print(f"| {r['param']} | {r['original']} | — | inert | 0 | — |")
        else:
            for cv in found:
                dist = abs(cv["critical_value"] - r["original"])
                pct = (dist / abs(r["original"]) * 100) if abs(r["original"]) > 1e-6 else float("inf")
                print(f"| {r['param']} | {r['original']} | {cv['direction']} | "
                      f"{cv['critical_value']:.4f} | {cv['flip_count']} | "
                      f"{pct:.1f}% |")


# ---------------------------------------------------------------------------
# 6. Entry point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Bifurcation Sweep — find critical parameter values")
    parser.add_argument(
        "--params", type=str, default=None,
        help="Regex filter for parameter names (e.g., 'rope_chi_ceiling')"
    )
    parser.add_argument(
        "--workers", type=int, default=1,
        help="Parallel workers (default: 1, sequential)"
    )
    parser.add_argument(
        "--timeout", type=int, default=30,
        help="Timeout per Prolog invocation in seconds (default: 30)"
    )
    parser.add_argument(
        "--output", type=str, default=None,
        help="JSON output file (default: python/bifurcation_results.json)"
    )
    args = parser.parse_args()

    base_dir = Path(__file__).resolve().parent.parent
    config_path = base_dir / "prolog" / "config.pl"
    prolog_dir = str(base_dir / "prolog")

    if not config_path.exists():
        print(f"ERROR: config.pl not found at {config_path}")
        sys.exit(1)

    output_path = args.output or str(base_dir / "python" / "bifurcation_results.json")

    results, baseline = run_bifurcation_sweep(
        str(config_path), prolog_dir,
        param_filter=args.params,
        workers=args.workers,
        timeout_sec=args.timeout,
    )

    print_summary_table(results)

    # Save results
    with open(output_path, "w") as f:
        json.dump({
            "baseline_count": len(baseline) if baseline else 0,
            "params_analyzed": len(results),
            "results": results,
        }, f, indent=2)
    print(f"\nResults saved to {output_path}")


if __name__ == "__main__":
    main()
