"""Corpus profile — query and JSON functions."""

from collections import Counter

from shared.loader import h1_band_or_raise


def modal_type(perspectives):
    """Return the most common non-null type across perspectives."""
    types = [t for t in perspectives.values() if t]
    if not types:
        return None
    return Counter(types).most_common(1)[0][0]


def build_profile(data):
    """Build the corpus profile dict from pipeline data."""
    constraints = data["per_constraint"]
    total = len(constraints)

    claimed_types = Counter(c.get("claimed_type") for c in constraints)
    resolved_types = Counter(modal_type(c.get("perspectives", {})) for c in constraints)
    signatures = Counter(c.get("signature") for c in constraints)

    n_false_ci_rope = sum(1 for c in constraints if c.get("signature") == "false_ci_rope")
    n_h1_gt_0 = sum(1 for c in constraints
                    if h1_band_or_raise(c, "corpus_profile") > 0)  # OQ-51: loud on null
    n_with_drift = sum(1 for c in constraints
                       if c.get("drift_events"))
    n_critical_drift = sum(1 for c in constraints
                          if any(e.get("severity") == "critical"
                                 for e in (c.get("drift_events") or [])))

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

    verdicts = Counter(
        (c.get("diagnostic_verdict") or {}).get("verdict", "missing")
        for c in constraints
    )

    subsystems_available = None
    subsystems_unavailable = None
    for c in constraints:
        dv = c.get("diagnostic_verdict")
        if dv and dv.get("verdict"):
            subsystems_available = dv.get("subsystems_available")
            subsystems_unavailable = dv.get("subsystems_unavailable", [])
            break

    null_types = [c["id"] for c in constraints
                  if modal_type(c.get("perspectives", {})) is None]
    standard_types = {"mountain", "rope", "scaffold", "piton", "tangled_rope", "snare"}
    nonstandard = [
        c["id"] for c in constraints
        if (modal_type(c.get("perspectives", {})) or "") not in standard_types
        and modal_type(c.get("perspectives", {})) is not None
    ]

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


def query(data: dict) -> dict:
    """Pipeline data -> context (profile dict). Also used for stdout summary."""
    pipeline = data["pipeline"]
    profile = build_profile(pipeline)
    return {"profile": profile}


def json_fn(data: dict):
    """Pipeline data -> JSON-serializable profile."""
    pipeline = data["pipeline"]
    return build_profile(pipeline)
