#!/usr/bin/env python3
"""
Directionality Sensitivity Sweep

Companion to config_sensitivity_sweep.py. Perturbs the hardcoded numeric
constants in constraint_indexing.pl that bypass config.pl:

  - power_role_heuristic/4  (11 clauses, lines ~313-323)
  - exit_modulation/2       (6 clauses, lines ~329-334)

These constants directly affect chi (χ) computation via directionality
but are invisible to the config param sweep because they aren't param/2 facts.

Approach:
  Prolog overlay with abolish+assert. For each perturbation, generates a .pl
  file that abolishes all clauses of the target predicate, asserts the full
  clause set with one value perturbed, then loads and runs the validation suite.
  This matches the config sweep's architecture.

Usage:
  python3 python/directionality_sensitivity_sweep.py [--workers N] [--timeout S]

Output:
  Prints a markdown table to stdout and writes results to
  python/directionality_sensitivity_results.json
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

# ---------------------------------------------------------------------------
# 1. Parse hardcoded constants from constraint_indexing.pl
# ---------------------------------------------------------------------------

# Each entry: (predicate_name, arity, clause_pattern, clauses)
# Clauses are parsed at runtime from the source file.

def parse_power_role_heuristic(source: str) -> list[dict]:
    """Extract power_role_heuristic/4 clauses."""
    pattern = re.compile(
        r"^power_role_heuristic\("
        r"\s*(\w+)\s*,"      # power atom
        r"\s*([^,]+)\s*,"    # has_beneficiaries (_, true, false)
        r"\s*([^,]+)\s*,"    # has_victims (_, true, false)
        r"\s*(-?[\d.]+)\s*"  # numeric value
        r"\)\.",
        re.MULTILINE,
    )
    clauses = []
    for m in pattern.finditer(source):
        clauses.append({
            "predicate": "power_role_heuristic",
            "arity": 4,
            "power": m.group(1).strip(),
            "arg2": m.group(2).strip(),
            "arg3": m.group(3).strip(),
            "value": float(m.group(4)),
            "label": f"prh_{m.group(1).strip()}_{m.group(2).strip()}_{m.group(3).strip()}",
        })
    return clauses


def parse_exit_modulation(source: str) -> list[dict]:
    """Extract exit_modulation/2 clauses."""
    pattern = re.compile(
        r"^exit_modulation\("
        r"\s*(\w+)\s*,"       # exit option atom
        r"\s*(-?[\d.]+)\s*"   # numeric value
        r"\)\.",
        re.MULTILINE,
    )
    clauses = []
    for m in pattern.finditer(source):
        clauses.append({
            "predicate": "exit_modulation",
            "arity": 2,
            "exit_option": m.group(1).strip(),
            "value": float(m.group(2)),
            "label": f"em_{m.group(1).strip()}",
        })
    return clauses


def parse_all_constants(ci_path: str) -> tuple[list[dict], list[dict]]:
    """Parse both predicate groups from constraint_indexing.pl."""
    with open(ci_path) as f:
        source = f.read()
    prh = parse_power_role_heuristic(source)
    em = parse_exit_modulation(source)
    return prh, em


# ---------------------------------------------------------------------------
# 2. Generate Prolog overlay
# ---------------------------------------------------------------------------

def build_prh_overlay(prh_clauses: list[dict], perturb_index: int,
                      perturbed_value: float) -> str:
    """Build overlay that abolishes and reasserts power_role_heuristic/4
    with one clause's value perturbed."""
    lines = [
        "%% Auto-generated directionality overlay — DO NOT EDIT",
        f"%% Perturbing power_role_heuristic clause #{perturb_index}",
        "",
        ":- use_module(constraint_indexing).",
        "",
        ":- abolish(constraint_indexing:power_role_heuristic/4).",
        "",
    ]
    for i, c in enumerate(prh_clauses):
        val = perturbed_value if i == perturb_index else c["value"]
        val_str = f"{val:.6f}".rstrip("0").rstrip(".")
        # Ensure at least one decimal place for Prolog float
        if "." not in val_str:
            val_str += ".0"
        lines.append(
            f":- assertz(constraint_indexing:power_role_heuristic("
            f"{c['power']}, {c['arg2']}, {c['arg3']}, {val_str}))."
        )
    lines.extend([
        "",
        ":- [stack].",
        ":- [validation_suite].",
        ":- run_dynamic_suite, halt.",
    ])
    return "\n".join(lines) + "\n"


def build_em_overlay(em_clauses: list[dict], perturb_index: int,
                     perturbed_value: float) -> str:
    """Build overlay that abolishes and reasserts exit_modulation/2
    with one clause's value perturbed."""
    lines = [
        "%% Auto-generated directionality overlay — DO NOT EDIT",
        f"%% Perturbing exit_modulation clause #{perturb_index}",
        "",
        ":- use_module(constraint_indexing).",
        "",
        ":- abolish(constraint_indexing:exit_modulation/2).",
        "",
    ]
    for i, c in enumerate(em_clauses):
        val = perturbed_value if i == perturb_index else c["value"]
        val_str = f"{val:.6f}".rstrip("0").rstrip(".")
        if "." not in val_str:
            val_str += ".0"
        lines.append(
            f":- assertz(constraint_indexing:exit_modulation("
            f"{c['exit_option']}, {val_str}))."
        )
    lines.extend([
        "",
        ":- [stack].",
        ":- [validation_suite].",
        ":- run_dynamic_suite, halt.",
    ])
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# 3. Run one perturbation
# ---------------------------------------------------------------------------

def run_perturbed_suite(overlay_content: str, label: str,
                        prolog_dir: str, timeout_sec: int = 120) -> dict:
    """Run the full test suite with one constant perturbed."""
    result = {
        "label": label,
        "pass_count": 0,
        "fail_count": 0,
        "error": None,
    }

    fd, overlay_path = tempfile.mkstemp(suffix=".pl", prefix=f"dsweep_{label}_")
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

        m_pass = re.search(r"^Passed:\s*(\d+)", output, re.MULTILINE)
        m_fail = re.search(r"^Failed:\s*(\d+)", output, re.MULTILINE)
        if m_pass and m_fail:
            result["pass_count"] = int(m_pass.group(1))
            result["fail_count"] = int(m_fail.group(1))
        else:
            passes = len(re.findall(r"\[PASS\]", output))
            fails = len(re.findall(r"\[(?:FAIL|AUDIT FAIL)\]", output))
            result["pass_count"] = passes
            result["fail_count"] = fails
            if passes == 0 and fails == 0:
                result["error"] = "Could not parse test results"
                # Save first 500 chars of output for debugging
                result["output_preview"] = output[:500]

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
# 4. Perturbation logic (reused from config sweep)
# ---------------------------------------------------------------------------

def compute_perturbations(value, levels=(0.10, 0.25)):
    """Return list of (label, perturbed_value) for ± each level."""
    perturbations = []
    for level in levels:
        if value == 0:
            delta = level  # absolute for zero-valued
        else:
            delta = abs(value) * level

        for sign, tag in [(1, f"+{int(level*100)}%"), (-1, f"-{int(level*100)}%")]:
            pv = value + sign * delta
            pv = round(pv, 6)
            if pv == value:
                continue  # skip identity perturbation
            perturbations.append((tag, pv))
    return perturbations


# ---------------------------------------------------------------------------
# 5. Orchestrate
# ---------------------------------------------------------------------------

def run_baseline(prolog_dir: str, timeout_sec: int) -> tuple[int, int]:
    """Run baseline suite and return (pass, fail) counts."""
    print("Running baseline suite...")
    cmd = ["swipl", "-g", "[stack], [validation_suite], run_dynamic_suite, halt."]
    try:
        proc = subprocess.run(
            cmd, capture_output=True, text=True,
            timeout=timeout_sec, cwd=prolog_dir,
        )
        output = proc.stdout + proc.stderr
        m_pass = re.search(r"^Passed:\s*(\d+)", output, re.MULTILINE)
        m_fail = re.search(r"^Failed:\s*(\d+)", output, re.MULTILINE)
        if m_pass and m_fail:
            return int(m_pass.group(1)), int(m_fail.group(1))
        passes = len(re.findall(r"\[PASS\]", output))
        fails = len(re.findall(r"\[(?:FAIL|AUDIT FAIL)\]", output))
        return passes, fails
    except Exception as e:
        print(f"Baseline failed: {e}")
        return 0, 0


def run_sweep(ci_path: str, prolog_dir: str, workers: int = 1,
              timeout_sec: int = 120) -> tuple[list, int, int, dict]:
    """Run the full directionality sensitivity sweep."""
    prh_clauses, em_clauses = parse_all_constants(ci_path)

    print(f"Parsed {len(prh_clauses)} power_role_heuristic/4 clauses")
    print(f"Parsed {len(em_clauses)} exit_modulation/2 clauses")

    # Build task list: (label, original_value, perturbed_value, overlay_content)
    tasks = []
    for i, c in enumerate(prh_clauses):
        for tag, pv in compute_perturbations(c["value"]):
            label = f"{c['label']}_{tag}"
            overlay = build_prh_overlay(prh_clauses, i, pv)
            tasks.append((c["label"], c["value"], pv, tag, overlay))

    for i, c in enumerate(em_clauses):
        for tag, pv in compute_perturbations(c["value"]):
            label = f"{c['label']}_{tag}"
            overlay = build_em_overlay(em_clauses, i, pv)
            tasks.append((c["label"], c["value"], pv, tag, overlay))

    # Filter out zero-valued constants that produced no perturbations
    total_constants = len(prh_clauses) + len(em_clauses)
    print(f"Total constants: {total_constants}")
    print(f"Total perturbation runs: {len(tasks)}")
    print()

    baseline_pass, baseline_fail = run_baseline(prolog_dir, timeout_sec)
    print(f"Baseline: {baseline_pass} passed, {baseline_fail} failed")
    print()

    if baseline_pass == 0:
        print("ERROR: Baseline produced 0 passes. Aborting sweep.")
        sys.exit(1)

    # Execute perturbations
    results_by_constant = {}
    completed = 0

    if workers > 1:
        with ProcessPoolExecutor(max_workers=workers) as executor:
            futures = {}
            for label, orig, pv, tag, overlay in tasks:
                future = executor.submit(
                    run_perturbed_suite, overlay, label, prolog_dir, timeout_sec
                )
                futures[future] = (label, orig, pv, tag)

            for future in as_completed(futures):
                label, orig, pv, tag = futures[future]
                completed += 1
                try:
                    result = future.result()
                except Exception as e:
                    result = {"label": label, "pass_count": 0, "fail_count": 0,
                              "error": str(e)}

                if label not in results_by_constant:
                    results_by_constant[label] = {"original": orig, "perturbations": {}}
                result["perturbed"] = pv
                results_by_constant[label]["perturbations"][tag] = result

                failures = baseline_pass - result["pass_count"] if result["pass_count"] > 0 else -1
                status = f"{failures} new failures" if failures >= 0 else result.get("error", "?")
                print(f"  [{completed}/{len(tasks)}] {label} {tag} → {status}")
    else:
        for label, orig, pv, tag, overlay in tasks:
            completed += 1
            print(f"  [{completed}/{len(tasks)}] {label} {tag} ({orig} → {pv})...",
                  end=" ", flush=True)
            result = run_perturbed_suite(overlay, label, prolog_dir, timeout_sec)

            if label not in results_by_constant:
                results_by_constant[label] = {"original": orig, "perturbations": {}}
            result["perturbed"] = pv
            results_by_constant[label]["perturbations"][tag] = result

            failures = baseline_pass - result["pass_count"] if result["pass_count"] > 0 else -1
            status = f"{failures} new failures" if failures >= 0 else result.get("error", "?")
            print(status)

    # Build summary
    summary = []
    for label, data in sorted(results_by_constant.items()):
        row = {
            "constant": label,
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

    return summary, baseline_pass, baseline_fail, results_by_constant


# ---------------------------------------------------------------------------
# 6. Output
# ---------------------------------------------------------------------------

def print_markdown_table(summary, baseline_pass, baseline_fail):
    """Print results as a markdown table."""
    print()
    print("## Directionality Sensitivity Sweep Results")
    print(f"Baseline: {baseline_pass} passed, {baseline_fail} failed")
    print()
    print("| Constant | Current | ±10% Max Failures | ±25% Max Failures | Rating |")
    print(f"|{'-'*45}|{'-'*9}|{'-'*19}|{'-'*19}|{'-'*10}|")

    rating_order = {"Critical": 0, "Moderate": 1, "Error": 2, "Inert": 3}
    for row in sorted(summary, key=lambda r: (rating_order.get(r["rating"], 4), r["constant"])):
        failures_10 = row["pm10_failures"] if row["pm10_failures"] >= 0 else "err"
        failures_25 = row["pm25_failures"] if row["pm25_failures"] >= 0 else "err"
        print(
            f"| {row['constant']:<43} | {str(row['original']):>7} | "
            f"{str(failures_10):>17} | {str(failures_25):>17} | {row['rating']:<8} |"
        )

    critical = sum(1 for r in summary if r["rating"] == "Critical")
    moderate = sum(1 for r in summary if r["rating"] == "Moderate")
    errors = sum(1 for r in summary if r["rating"] == "Error")
    inert = sum(1 for r in summary if r["rating"] == "Inert")
    print()
    print(
        f"**Summary**: {critical} Critical, {moderate} Moderate, "
        f"{errors} Error, {inert} Inert (of {len(summary)} constants)"
    )


def main():
    parser = argparse.ArgumentParser(
        description="Directionality Sensitivity Sweep — "
                    "power_role_heuristic/4 and exit_modulation/2"
    )
    parser.add_argument(
        "--workers", type=int, default=4,
        help="Parallel workers (default: 4)"
    )
    parser.add_argument(
        "--timeout", type=int, default=120,
        help="Timeout per suite run in seconds (default: 120)"
    )
    parser.add_argument(
        "--output", type=str, default=None,
        help="JSON output file (default: python/directionality_sensitivity_results.json)"
    )
    args = parser.parse_args()

    base_dir = Path(__file__).resolve().parent.parent
    ci_path = base_dir / "prolog" / "constraint_indexing.pl"
    prolog_dir = str(base_dir / "prolog")

    if not ci_path.exists():
        print(f"ERROR: constraint_indexing.pl not found at {ci_path}")
        sys.exit(1)

    output_path = args.output or str(
        base_dir / "python" / "directionality_sensitivity_results.json"
    )

    summary, bp, bf, raw = run_sweep(
        str(ci_path), prolog_dir,
        workers=args.workers,
        timeout_sec=args.timeout,
    )

    print_markdown_table(summary, bp, bf)

    # Save results
    with open(output_path, "w") as f:
        json.dump({
            "baseline_pass": bp,
            "baseline_fail": bf,
            "summary": summary,
            "raw_results": {
                label: {
                    "original": data["original"],
                    "perturbations": {
                        tag: {
                            "perturbed": r.get("perturbed"),
                            "pass_count": r["pass_count"],
                            "fail_count": r["fail_count"],
                            "error": r["error"],
                        }
                        for tag, r in data["perturbations"].items()
                    },
                }
                for label, data in raw.items()
            },
        }, f, indent=2)
    print(f"\nResults saved to {output_path}")


if __name__ == "__main__":
    main()
