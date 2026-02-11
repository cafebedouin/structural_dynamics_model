#!/usr/bin/env python3
"""
Config Sensitivity Sweep — Investigation 4

Perturbs each numeric param() in config.pl by ±10% and ±25%, runs the full
test suite for each perturbation, and produces a summary table rating each
parameter as Critical/Moderate/Inert.

Approach:
  Instead of modifying config.pl, generates a tiny Prolog overlay that
  retracts the original param and asserts the perturbed value, then loads
  and runs the standard validation suite.

Usage:
  python3 python/config_sensitivity_sweep.py [--params PATTERN] [--workers N]

Output:
  Prints a markdown table to stdout and writes results to
  python/config_sensitivity_results.json
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

# ---------------------------------------------------------------------------
# 1. Parse config.pl for numeric param/2 facts
# ---------------------------------------------------------------------------

def parse_config_params(config_path: str) -> list[dict]:
    """Extract all param(Name, NumericValue) facts from config.pl."""
    params = []
    # Match: param(name, value).  where value is a number (int or float, possibly negative)
    pattern = re.compile(
        r"^\s*param\(\s*(\w+)\s*,\s*(-?\d+(?:\.\d+)?)\s*\)\.",
        re.MULTILINE,
    )
    with open(config_path) as f:
        text = f.read()
    for m in pattern.finditer(text):
        name = m.group(1)
        raw = m.group(2)
        value = float(raw) if "." in raw else int(raw)
        params.append({"name": name, "value": value, "raw": raw})
    return params


# ---------------------------------------------------------------------------
# 2. Generate Prolog overlay and run suite
# ---------------------------------------------------------------------------

PROLOG_OVERLAY_TEMPLATE = """\
%% Auto-generated config overlay — DO NOT EDIT
%% Perturbs param({name}, {original}) → {perturbed}

:- use_module(config).

:- (   retract(config:param({name}, _))
   ->  true
   ;   true
   ),
   asserta(config:param({name}, {perturbed})).

:- [validation_suite].
:- run_dynamic_suite, halt.
"""


def run_perturbed_suite(
    name: str,
    original,
    perturbed,
    prolog_dir: str,
    timeout_sec: int = 600,
) -> dict:
    """Run the full test suite with one param perturbed. Returns result dict."""
    result = {
        "param": name,
        "original": original,
        "perturbed": float(perturbed) if isinstance(perturbed, (int, float)) else 0.0,
        "pass_count": 0,
        "fail_count": 0,
        "error": None,
    }

    # Write temporary overlay file
    overlay_content = PROLOG_OVERLAY_TEMPLATE.format(
        name=name,
        original=original,
        perturbed=perturbed,
    )
    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix=f"sweep_{name}_")
    try:
        with os.fdopen(fd, "w") as f:
            f.write(overlay_content)

        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=prolog_dir,
        )
        output = proc.stdout + proc.stderr

        # Parse pass/fail from output
        # Expected lines: "Passed: 730\nFailed: 0"
        m_pass = re.search(r"^Passed:\s*(\d+)", output, re.MULTILINE)
        m_fail = re.search(r"^Failed:\s*(\d+)", output, re.MULTILINE)
        if m_pass and m_fail:
            result["pass_count"] = int(m_pass.group(1))
            result["fail_count"] = int(m_fail.group(1))
        else:
            # Fallback: count [PASS] and [FAIL] lines
            passes = len(re.findall(r"\[PASS\]", output))
            fails = len(re.findall(r"\[(?:FAIL|AUDIT FAIL)\]", output))
            result["pass_count"] = passes
            result["fail_count"] = fails
            if passes == 0 and fails == 0:
                result["error"] = "Could not parse test results"

    except subprocess.TimeoutExpired:
        result["error"] = f"Timeout after {timeout_sec}s"
    except Exception as e:
        result["error"] = str(e)
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass

    return result


# ---------------------------------------------------------------------------
# 3. Orchestrate the sweep
# ---------------------------------------------------------------------------

def compute_perturbations(value, levels=(0.10, 0.25)):
    """Return list of (label, perturbed_value) for ± each level."""
    perturbations = []
    for level in levels:
        if value == 0:
            # For zero-valued params, perturb by absolute amount
            delta = level
        else:
            delta = abs(value) * level

        for sign, tag in [(1, f"+{int(level*100)}%"), (-1, f"-{int(level*100)}%")]:
            pv = value + sign * delta
            # Round to avoid floating point noise
            if isinstance(value, int):
                pv = round(pv)
            else:
                pv = round(pv, 6)
            perturbations.append((tag, pv))
    return perturbations


def run_sweep(config_path: str, prolog_dir: str, param_filter: str = None,
              workers: int = 1, timeout_sec: int = 600) -> list[dict]:
    """Run the full sensitivity sweep."""
    params = parse_config_params(config_path)
    if param_filter:
        regex = re.compile(param_filter)
        params = [p for p in params if regex.search(p["name"])]

    print(f"Found {len(params)} numeric parameters to sweep")
    total_runs = len(params) * 4  # ±10%, ±25%
    print(f"Total suite runs: {total_runs}")
    print()

    # First, get baseline pass count
    print("Running baseline suite...")
    baseline_cmd = ["swipl", "-g", "[validation_suite], run_dynamic_suite, halt."]
    try:
        proc = subprocess.run(
            baseline_cmd,
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=prolog_dir,
        )
        output = proc.stdout + proc.stderr
        m_pass = re.search(r"^Passed:\s*(\d+)", output, re.MULTILINE)
        m_fail = re.search(r"^Failed:\s*(\d+)", output, re.MULTILINE)
        if m_pass and m_fail:
            baseline_pass = int(m_pass.group(1))
            baseline_fail = int(m_fail.group(1))
        else:
            passes = len(re.findall(r"\[PASS\]", output))
            fails = len(re.findall(r"\[(?:FAIL|AUDIT FAIL)\]", output))
            baseline_pass = passes
            baseline_fail = fails
    except Exception as e:
        print(f"Baseline failed: {e}")
        baseline_pass = 0
        baseline_fail = 0

    print(f"Baseline: {baseline_pass} passed, {baseline_fail} failed")
    print()

    # Run perturbations
    results_by_param = {}
    completed = 0

    tasks = []
    for p in params:
        perturbations = compute_perturbations(p["value"])
        for tag, pv in perturbations:
            tasks.append((p["name"], p["value"], pv, tag))

    if workers > 1:
        with ProcessPoolExecutor(max_workers=workers) as executor:
            futures = {}
            for name, orig, pv, tag in tasks:
                future = executor.submit(
                    run_perturbed_suite, name, orig, pv, prolog_dir, timeout_sec
                )
                futures[future] = (name, tag, pv)

            for future in as_completed(futures):
                name, tag, pv = futures[future]
                completed += 1
                try:
                    result = future.result()
                except Exception as e:
                    result = {"param": name, "perturbed": pv, "error": str(e),
                              "pass_count": 0, "fail_count": 0}

                if name not in results_by_param:
                    results_by_param[name] = {"original": result.get("original"), "perturbations": {}}
                results_by_param[name]["perturbations"][tag] = result

                failures = baseline_pass - result["pass_count"] if result["pass_count"] > 0 else -1
                status = f"{failures} new failures" if failures >= 0 else result.get("error", "?")
                print(f"  [{completed}/{total_runs}] {name} {tag} → {status}")
    else:
        for name, orig, pv, tag in tasks:
            completed += 1
            print(f"  [{completed}/{total_runs}] {name} {tag} ({orig} → {pv})...", end=" ", flush=True)
            result = run_perturbed_suite(name, orig, pv, prolog_dir, timeout_sec)

            if name not in results_by_param:
                results_by_param[name] = {"original": orig, "perturbations": {}}
            results_by_param[name]["perturbations"][tag] = result

            failures = baseline_pass - result["pass_count"] if result["pass_count"] > 0 else -1
            status = f"{failures} new failures" if failures >= 0 else result.get("error", "?")
            print(status)

    # Build summary
    summary = []
    for name, data in sorted(results_by_param.items()):
        row = {
            "param": name,
            "original": data["original"],
            "pm10_failures": 0,
            "pm25_failures": 0,
            "rating": "Inert",
        }

        for tag, r in data["perturbations"].items():
            new_failures = max(0, baseline_pass - r["pass_count"]) if r["pass_count"] > 0 else -1
            if "10%" in tag:
                row["pm10_failures"] = max(row["pm10_failures"], new_failures)
            elif "25%" in tag:
                row["pm25_failures"] = max(row["pm25_failures"], new_failures)

        if row["pm10_failures"] > 0:
            row["rating"] = "Critical"
        elif row["pm25_failures"] > 0:
            row["rating"] = "Moderate"
        elif row["pm10_failures"] < 0 or row["pm25_failures"] < 0:
            row["rating"] = "Error"
        else:
            row["rating"] = "Inert"

        summary.append(row)

    return summary, baseline_pass, baseline_fail, results_by_param


# ---------------------------------------------------------------------------
# 4. Output
# ---------------------------------------------------------------------------

def print_markdown_table(summary, baseline_pass, baseline_fail):
    """Print results as a markdown table."""
    print()
    print(f"## Config Sensitivity Sweep Results")
    print(f"Baseline: {baseline_pass} passed, {baseline_fail} failed")
    print()
    print(f"| Parameter | Current | ±10% Max Failures | ±25% Max Failures | Rating |")
    print(f"|{'-'*40}|{'-'*9}|{'-'*19}|{'-'*19}|{'-'*10}|")

    # Sort: Critical first, then Moderate, then Inert
    rating_order = {"Critical": 0, "Moderate": 1, "Inert": 2}
    for row in sorted(summary, key=lambda r: (rating_order.get(r["rating"], 3), r["param"])):
        failures_10 = row["pm10_failures"] if row["pm10_failures"] >= 0 else "err"
        failures_25 = row["pm25_failures"] if row["pm25_failures"] >= 0 else "err"
        print(f"| {row['param']:<38} | {str(row['original']):>7} | {str(failures_10):>17} | {str(failures_25):>17} | {row['rating']:<8} |")

    # Summary counts
    critical = sum(1 for r in summary if r["rating"] == "Critical")
    moderate = sum(1 for r in summary if r["rating"] == "Moderate")
    inert = sum(1 for r in summary if r["rating"] == "Inert")
    print()
    print(f"**Summary**: {critical} Critical, {moderate} Moderate, {inert} Inert (of {len(summary)} params)")


def main():
    parser = argparse.ArgumentParser(description="Config Sensitivity Sweep")
    parser.add_argument(
        "--params", type=str, default=None,
        help="Regex filter for parameter names (e.g., 'mountain|rope')"
    )
    parser.add_argument(
        "--workers", type=int, default=1,
        help="Parallel workers (default: 1, sequential)"
    )
    parser.add_argument(
        "--timeout", type=int, default=600,
        help="Timeout per suite run in seconds (default: 600)"
    )
    parser.add_argument(
        "--output", type=str, default=None,
        help="JSON output file (default: python/config_sensitivity_results.json)"
    )
    args = parser.parse_args()

    base_dir = Path(__file__).resolve().parent.parent
    config_path = base_dir / "prolog" / "config.pl"
    prolog_dir = str(base_dir / "prolog")

    if not config_path.exists():
        print(f"ERROR: config.pl not found at {config_path}")
        sys.exit(1)

    output_path = args.output or str(base_dir / "python" / "config_sensitivity_results.json")

    summary, bp, bf, raw = run_sweep(
        str(config_path), prolog_dir,
        param_filter=args.params,
        workers=args.workers,
        timeout_sec=args.timeout,
    )

    print_markdown_table(summary, bp, bf)

    # Save raw results
    with open(output_path, "w") as f:
        json.dump({
            "baseline_pass": bp,
            "baseline_fail": bf,
            "summary": summary,
            "raw_results": {
                name: {
                    "original": data["original"],
                    "perturbations": {
                        tag: {
                            "perturbed": r["perturbed"],
                            "pass_count": r["pass_count"],
                            "fail_count": r["fail_count"],
                            "error": r["error"],
                        }
                        for tag, r in data["perturbations"].items()
                    },
                }
                for name, data in raw.items()
            },
        }, f, indent=2)
    print(f"\nRaw results saved to {output_path}")


if __name__ == "__main__":
    main()
