"""Red spot check — query function."""


def extract_reds(data):
    """Extract all red-verdict constraints with diagnostic detail."""
    reds = []
    for c in data["per_constraint"]:
        dv = c.get("diagnostic_verdict") or {}
        if dv.get("verdict") != "red":
            continue

        perspectives = c.get("perspectives", {})
        types = [t for t in perspectives.values() if t]
        modal_type = max(set(types), key=types.count) if types else "unknown"

        tensions = dv.get("tensions", [])
        tension_subs = [t["subsystem"] for t in tensions]

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


def query(data: dict) -> dict:
    """Pipeline data -> template context for red spot check."""
    pipeline = data["pipeline"]
    total = len(pipeline["per_constraint"])

    verdicts = {}
    for c in pipeline["per_constraint"]:
        v = (c.get("diagnostic_verdict") or {}).get("verdict", "?")
        verdicts[v] = verdicts.get(v, 0) + 1

    reds = extract_reds(pipeline)

    # Pre-format verdict distribution string
    verdict_parts = []
    for k, v in sorted(verdicts.items()):
        verdict_parts.append(f"{k}: {v} ({100*v/total:.1f}%)")
    verdict_str = ", ".join(verdict_parts)

    return {
        "total": total,
        "verdict_str": verdict_str,
        "reds": reds,
        "red_count": len(reds),
    }
