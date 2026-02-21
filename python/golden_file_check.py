#!/usr/bin/env python3
"""Golden-file regression test for dr_type/3 classification outcomes.

Reads pipeline_output.json, extracts the 4-perspective classification vector
for every constraint, and diffs against a committed baseline JSON file.

Usage:
    # Generate baseline (first time or after intentional reclassification):
    python3 golden_file_check.py --generate

    # Check against baseline (default):
    python3 golden_file_check.py

Exit codes:
    0  All classifications match baseline (or baseline generated successfully)
    1  Reclassifications detected (printed to stderr)
    2  Baseline file missing — run with --generate first
"""

import json
import sys
from pathlib import Path

# Canonical perspective order for the classification vector
PERSPECTIVE_ORDER = ["powerless", "moderate", "institutional", "analytical"]

ROOT_DIR = Path(__file__).resolve().parent.parent
PIPELINE_JSON = ROOT_DIR / "outputs" / "pipeline_output.json"
GOLDEN_FILE = ROOT_DIR / "outputs" / "golden_classifications.json"


def extract_classifications(pipeline_path):
    """Extract {constraint_id: [type, type, type, type]} from pipeline_output.json."""
    with open(pipeline_path, "r", encoding="utf-8") as f:
        data = json.load(f)

    result = {}
    for entry in data["per_constraint"]:
        cid = entry["id"]
        perspectives = entry.get("perspectives", {})
        vector = [perspectives.get(p, "unknown") for p in PERSPECTIVE_ORDER]
        result[cid] = vector

    return dict(sorted(result.items()))


def generate_baseline():
    """Generate golden_classifications.json from current pipeline output."""
    if not PIPELINE_JSON.exists():
        print(f"ERROR: {PIPELINE_JSON} not found — run the pipeline first",
              file=sys.stderr)
        return 1

    classifications = extract_classifications(PIPELINE_JSON)
    with open(GOLDEN_FILE, "w", encoding="utf-8") as f:
        json.dump(classifications, f, indent=2, sort_keys=True)

    print(f"Generated {GOLDEN_FILE}")
    print(f"  {len(classifications)} constraints × {len(PERSPECTIVE_ORDER)} perspectives")
    return 0


def check_against_baseline():
    """Compare current pipeline output against committed baseline."""
    if not GOLDEN_FILE.exists():
        print(f"ERROR: Baseline {GOLDEN_FILE} not found", file=sys.stderr)
        print("  Run: python3 golden_file_check.py --generate", file=sys.stderr)
        return 2

    if not PIPELINE_JSON.exists():
        print(f"ERROR: {PIPELINE_JSON} not found — run the pipeline first",
              file=sys.stderr)
        return 1

    with open(GOLDEN_FILE, "r", encoding="utf-8") as f:
        baseline = json.load(f)

    current = extract_classifications(PIPELINE_JSON)

    # Diff
    added = set(current.keys()) - set(baseline.keys())
    removed = set(baseline.keys()) - set(current.keys())
    changed = {
        cid for cid in set(current.keys()) & set(baseline.keys())
        if current[cid] != baseline[cid]
    }

    if not added and not removed and not changed:
        print(f"PASS: All {len(current)} constraints match baseline "
              f"({len(PERSPECTIVE_ORDER)} perspectives each)")
        return 0

    # Report differences
    print("FAIL: Classification regressions detected", file=sys.stderr)
    print(f"  Perspectives: {PERSPECTIVE_ORDER}", file=sys.stderr)
    print(file=sys.stderr)

    if added:
        print(f"  NEW constraints ({len(added)}):", file=sys.stderr)
        for cid in sorted(added):
            print(f"    + {cid}: {current[cid]}", file=sys.stderr)
        print(file=sys.stderr)

    if removed:
        print(f"  REMOVED constraints ({len(removed)}):", file=sys.stderr)
        for cid in sorted(removed):
            print(f"    - {cid}: {baseline[cid]}", file=sys.stderr)
        print(file=sys.stderr)

    if changed:
        print(f"  RECLASSIFIED constraints ({len(changed)}):", file=sys.stderr)
        for cid in sorted(changed):
            old = baseline[cid]
            new = current[cid]
            diffs = []
            for i, p in enumerate(PERSPECTIVE_ORDER):
                if old[i] != new[i]:
                    diffs.append(f"{p}: {old[i]} -> {new[i]}")
            print(f"    ~ {cid}: {', '.join(diffs)}", file=sys.stderr)
        print(file=sys.stderr)

    total_changes = len(added) + len(removed) + len(changed)
    print(f"  Total: {total_changes} difference(s) across "
          f"{len(current)} constraints", file=sys.stderr)
    return 1


def main():
    import argparse
    parser = argparse.ArgumentParser(
        description="Golden-file regression test for dr_type/3 classifications"
    )
    parser.add_argument(
        "--generate", action="store_true",
        help="Generate baseline from current pipeline_output.json"
    )
    args = parser.parse_args()

    if args.generate:
        sys.exit(generate_baseline())
    else:
        sys.exit(check_against_baseline())


if __name__ == "__main__":
    main()
