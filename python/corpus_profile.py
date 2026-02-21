#!/usr/bin/env python3
"""
Corpus Profile — Aggregate Statistics Baseline

Produces outputs/corpus_profile.json with corpus-wide statistics:
  - Constraint count and loader success rate
  - Type distributions (claimed and modal resolved)
  - Signature distribution
  - Diagnostic signal base rates
  - Verdict distribution (green/yellow/red)
  - Anomalous constraints

Usage:
    python3 python/corpus_profile.py
"""

import json
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PIPELINE = ROOT / "outputs" / "pipeline_output.json"
ENRICHED = ROOT / "outputs" / "enriched_pipeline.json"
OUTPUT = ROOT / "outputs" / "corpus_profile.json"


def load_json(path):
    if not path.exists():
        return None
    with open(path) as f:
        return json.load(f)


def modal_type(perspectives):
    """Return the most common non-null type across perspectives."""
    types = [t for t in perspectives.values() if t]
    if not types:
        return None
    return Counter(types).most_common(1)[0][0]


def build_profile(data):
    constraints = data["per_constraint"]
    total = len(constraints)

    # Type distributions
    claimed_types = Counter(c.get("claimed_type") for c in constraints)
    resolved_types = Counter(modal_type(c.get("perspectives", {})) for c in constraints)

    # Signature distribution
    signatures = Counter(c.get("signature") for c in constraints)

    # Diagnostic signal base rates
    n_false_ci_rope = sum(1 for c in constraints if c.get("signature") == "false_ci_rope")
    n_h1_gt_0 = sum(1 for c in constraints
                    if (c.get("h1_band") or 0) > 0)
    n_with_drift = sum(1 for c in constraints
                       if c.get("drift_events"))
    n_critical_drift = sum(1 for c in constraints
                          if any(e.get("severity") == "critical"
                                 for e in (c.get("drift_events") or [])))

    # Broadly stressed: multiple critical drift event types
    n_broadly_stressed = 0
    for c in constraints:
        drift_events = c.get("drift_events") or []
        crit_types = set(e["type"] for e in drift_events if e.get("severity") == "critical")
        if len(crit_types) >= 3:
            n_broadly_stressed += 1

    n_crit_extr_accum = sum(
        1 for c in constraints
        if any(e.get("type") == "extraction_accumulation" and e.get("severity") == "critical"
               for e in (c.get("drift_events") or []))
    )

    # Verdict distribution
    verdicts = Counter(
        (c.get("diagnostic_verdict") or {}).get("verdict", "missing")
        for c in constraints
    )

    # Subsystem availability (from first non-null verdict)
    subsystems_available = None
    subsystems_unavailable = None
    for c in constraints:
        dv = c.get("diagnostic_verdict")
        if dv and dv.get("verdict"):
            subsystems_available = dv.get("subsystems_available")
            subsystems_unavailable = dv.get("subsystems_unavailable", [])
            break

    # Anomalous constraints
    null_types = [c["id"] for c in constraints
                  if modal_type(c.get("perspectives", {})) is None]
    standard_types = {"mountain", "rope", "scaffold", "piton", "tangled_rope", "snare"}
    nonstandard = [
        c["id"] for c in constraints
        if (modal_type(c.get("perspectives", {})) or "") not in standard_types
        and modal_type(c.get("perspectives", {})) is not None
    ]

    # Abductive trigger stats
    n_with_abd = sum(1 for c in constraints
                     if any(t["subsystem"] == "abductive"
                            for t in (c.get("diagnostic_verdict") or {}).get("tensions", [])))

    profile = {
        "corpus_size": total,
        "type_distribution": {
            "claimed": dict(claimed_types.most_common()),
            "modal_resolved": dict(resolved_types.most_common()),
        },
        "signature_distribution": dict(signatures.most_common()),
        "signal_base_rates": {
            "false_ci_rope_pct": round(100 * n_false_ci_rope / total, 1) if total else 0,
            "h1_gt_0_pct": round(100 * n_h1_gt_0 / total, 1) if total else 0,
            "with_drift_events_pct": round(100 * n_with_drift / total, 1) if total else 0,
            "critical_drift_pct": round(100 * n_critical_drift / total, 1) if total else 0,
            "broadly_stressed_pct": round(100 * n_broadly_stressed / total, 1) if total else 0,
            "critical_extraction_accumulation_pct": round(100 * n_crit_extr_accum / total, 1) if total else 0,
        },
        "verdict_distribution": dict(verdicts.most_common()),
        "subsystems_available": subsystems_available,
        "subsystems_unavailable": subsystems_unavailable,
        "abductive_tensions": n_with_abd,
        "anomalies": {
            "null_type_constraints": null_types,
            "nonstandard_type_constraints": nonstandard,
        },
    }
    return profile


def main():
    if not PIPELINE.exists():
        print(f"ERROR: {PIPELINE} not found. Run 'make' first.", file=sys.stderr)
        sys.exit(1)

    data = load_json(PIPELINE)
    profile = build_profile(data)

    with open(OUTPUT, "w") as f:
        json.dump(profile, f, indent=2)
    print(f"Wrote {OUTPUT} ({profile['corpus_size']} constraints)")

    # Print summary
    print(f"\nVerdict distribution:")
    for k, v in profile["verdict_distribution"].items():
        pct = 100 * v / profile["corpus_size"]
        print(f"  {k}: {v} ({pct:.1f}%)")
    print(f"\nSignal base rates:")
    for k, v in profile["signal_base_rates"].items():
        print(f"  {k}: {v}%")
    print(f"\nSubsystems available: {profile['subsystems_available']}")
    print(f"Subsystems unavailable: {profile['subsystems_unavailable']}")
    print(f"Abductive tensions: {profile['abductive_tensions']}")


if __name__ == "__main__":
    main()
