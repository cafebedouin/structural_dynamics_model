#!/usr/bin/env python3
"""
Red Constraint Spot-Check — Post-Calibration Analysis

Reads pipeline_output.json and produces a table of red-verdict constraints
with their tension sources, convergent rejections, and categorization hints.

Usage:
    python3 python/red_spot_check.py
"""

import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PIPELINE = ROOT / "outputs" / "pipeline_output.json"


def load_pipeline():
    with open(PIPELINE) as f:
        return json.load(f)


def extract_reds(data):
    """Extract all red-verdict constraints with diagnostic detail."""
    reds = []
    for c in data["per_constraint"]:
        dv = c.get("diagnostic_verdict") or {}
        if dv.get("verdict") != "red":
            continue

        # Determine modal resolved type from perspectives
        perspectives = c.get("perspectives", {})
        types = [t for t in perspectives.values() if t]
        modal_type = max(set(types), key=types.count) if types else "unknown"

        # Tension subsystems
        tensions = dv.get("tensions", [])
        tension_subs = [t["subsystem"] for t in tensions]

        # Convergent rejections
        rejections = dv.get("convergent_rejections", [])
        alt_types = []
        rejecting_subs = []
        for r in rejections:
            alt_types.append(r.get("alternative_type", "?"))
            rejecting_subs.extend(r.get("subsystems", []))

        reds.append({
            "id": c["id"],
            "claimed_type": c.get("claimed_type", "?"),
            "det_type": modal_type,
            "signature": c.get("signature"),
            "tension_count": len(tensions),
            "tension_subsystems": tension_subs,
            "tensions": tensions,
            "rejection_count": len(rejections),
            "rejecting_subsystems": rejecting_subs,
            "alternative_types": alt_types,
            "expected_conflicts": dv.get("expected_conflicts", []),
        })
    return reds


def print_report(reds, data):
    """Print a formatted spot-check report."""
    total = len(data["per_constraint"])
    verdicts = {}
    for c in data["per_constraint"]:
        v = (c.get("diagnostic_verdict") or {}).get("verdict", "?")
        verdicts[v] = verdicts.get(v, 0) + 1

    print("=" * 72)
    print("RED CONSTRAINT SPOT-CHECK — Post-Calibration")
    print("=" * 72)
    print()
    print(f"Corpus: {total} constraints")
    print(f"Verdict distribution: " + ", ".join(
        f"{k}: {v} ({100*v/total:.1f}%)" for k, v in sorted(verdicts.items())
    ))
    print(f"Red constraints: {len(reds)}")
    print()

    if not reds:
        print("No red constraints found.")
        return

    print("-" * 72)
    for r in reds:
        print(f"  {r['id']}")
        print(f"    claimed: {r['claimed_type']}  →  resolved: {r['det_type']}"
              f"  (signature: {r['signature']})")
        print(f"    tensions ({r['tension_count']}): {', '.join(r['tension_subsystems'])}")
        for t in r["tensions"]:
            print(f"      - {t['subsystem']}: {t['signal']}")
        if r["rejection_count"]:
            print(f"    convergent rejections: " + ", ".join(
                f"{a} ({', '.join(r['rejecting_subsystems'])})"
                for a in r["alternative_types"]
            ))
        if r["expected_conflicts"]:
            print(f"    absorbed patterns: " + ", ".join(
                ec["pattern"] for ec in r["expected_conflicts"]
            ))
        print()
    print("-" * 72)


def main():
    if not PIPELINE.exists():
        print(f"ERROR: {PIPELINE} not found. Run 'make' first.", file=sys.stderr)
        sys.exit(1)

    data = load_pipeline()
    reds = extract_reds(data)
    print_report(reds, data)


if __name__ == "__main__":
    main()
