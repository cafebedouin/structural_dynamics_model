"""
Cross-constraint omega narrowing (v6.9).

Reads enriched_pipeline.json, groups constraints by shared beneficiary,
and narrows omega spaces using domain-level findings: when multiple
constraints share a beneficiary and show convergent classification
or abductive signals, each constraint's omega resolution space is
annotated with that cross-constraint context.

Usage: python3 python/omega_cross_constraint.py
Output: outputs/omega_cross_constraint.json
        outputs/omega_cross_constraint_report.md
"""

import json
from pathlib import Path
from collections import defaultdict, Counter

_REPO_ROOT = Path(__file__).resolve().parent.parent
PIPELINE = _REPO_ROOT / "outputs/enriched_pipeline.json"
OUT_JSON = _REPO_ROOT / "outputs/omega_cross_constraint.json"
OUT_MD   = _REPO_ROOT / "outputs/omega_cross_constraint_report.md"

EXTRACTIVE_TYPES = {"snare", "tangled_rope", "piton"}
PROTECTIVE_TYPES = {"mountain", "rope", "scaffold"}


def build_beneficiary_index(entries):
    """Map beneficiary → [constraint_id]."""
    idx = defaultdict(list)
    for e in entries:
        for b in e.get("beneficiaries", []):
            idx[b].append(e["id"])
    return idx


def group_findings(group_ids, entry_map):
    """
    Collect classification types, abductive trigger classes, and drift
    event types for a group of constraints sharing a beneficiary.
    Returns a dict keyed by constraint_id.
    """
    findings = {}
    for cid in group_ids:
        e = entry_map.get(cid, {})
        # abductive_triggers uses 'trigger_class' key (not 'trigger_type')
        triggers = [t.get("trigger_class") for t in e.get("abductive_triggers", []) if t.get("trigger_class")]
        drifts   = [d.get("type") for d in e.get("drift_events", []) if d.get("type")]
        findings[cid] = {
            "claimed_type": e.get("claimed_type"),
            "abductive_triggers": triggers,
            "drift_events": drifts,
        }
    return findings


def narrowing_notes_for_omega(omega, group_ids, findings, entry_map):
    """
    Given one omega and the beneficiary group's cross-constraint findings,
    return a list of narrowing notes (strings). Empty list = no narrowing.
    """
    notes = []

    types_in_group = {findings[c]["claimed_type"] for c in group_ids if findings[c]["claimed_type"]}
    extractive_types = types_in_group & EXTRACTIVE_TYPES
    extractive_count = sum(
        1 for c in group_ids if findings[c]["claimed_type"] in EXTRACTIVE_TYPES
    )
    total = len(group_ids)

    # Signal 1: Extractive convergence narrows omega space for conceptual omegas
    if extractive_count >= 2 and omega.get("type") == "conceptual":
        notes.append(
            f"{extractive_count}/{total} constraints in this beneficiary group are classified "
            f"extractive ({sorted(extractive_types)}). Omega resolution must account for "
            "coordinated extraction: reform scenarios that treat this constraint in isolation "
            "will underestimate resistance."
        )

    # Signal 2: false_summit_mountain signature in group narrows naturalization omegas
    false_summit_count = 0
    for cid in group_ids:
        e = entry_map.get(cid, {})
        if e.get("signature") == "false_summit_mountain":
            false_summit_count += 1
    if false_summit_count > 0:
        notes.append(
            f"{false_summit_count} constraint(s) in this beneficiary group carry the "
            "false_summit_mountain signature, indicating naturalized construction. "
            "Omega resolution scenarios should address the naturalization mechanism, "
            "not just the surface classification."
        )

    # Signal 3: Convergent abductive triggers narrow investigation priorities
    all_triggers = []
    for c in group_ids:
        all_triggers.extend(findings[c]["abductive_triggers"])
    trigger_counts = Counter(all_triggers)
    # Triggers appearing in ≥2 constraints, or ≥ half the group
    dominant = [t for t, n in trigger_counts.items() if n >= max(2, total // 2)]
    if dominant and omega.get("severity") in ("critical", "major"):
        notes.append(
            f"Convergent abductive signals across group: {dominant}. "
            "High-severity omega investigation should treat these as group-level "
            "phenomena, not isolated constraint anomalies."
        )

    return notes


def run():
    if not PIPELINE.exists():
        print(f"[omega_cross_constraint] {PIPELINE} not found — run pipeline first.")
        return

    data = json.loads(PIPELINE.read_text())
    entries = data.get("per_constraint", [])
    entry_map = {e["id"]: e for e in entries}

    bene_idx = build_beneficiary_index(entries)
    # Only process groups with ≥ 2 constraints
    groups = {b: ids for b, ids in bene_idx.items() if len(ids) >= 2}

    results = {}
    total_narrowings = 0

    for beneficiary, group_ids in sorted(groups.items()):
        findings = group_findings(group_ids, entry_map)
        group_result = {
            "beneficiary": beneficiary,
            "group_size": len(group_ids),
            "group_ids": group_ids,
            "constraints": {},
        }

        for cid in group_ids:
            e = entry_map.get(cid, {})
            omega_narrowings = []
            for omega in e.get("omegas", []):
                notes = narrowing_notes_for_omega(omega, group_ids, findings, entry_map)
                if notes:
                    omega_narrowings.append({
                        "omega_id": omega.get("id"),
                        "omega_type": omega.get("type"),
                        "omega_severity": omega.get("severity"),
                        "narrowing_notes": notes,
                    })

            if omega_narrowings:
                group_result["constraints"][cid] = omega_narrowings
                total_narrowings += len(omega_narrowings)

        if group_result["constraints"]:
            results[beneficiary] = group_result

    OUT_JSON.write_text(json.dumps(results, indent=2))
    _write_report(results, OUT_MD)
    print(
        f"[omega_cross_constraint] {len(results)} beneficiary groups, "
        f"{total_narrowings} omega narrowings written to {OUT_JSON}"
    )


def _write_report(results, path):
    lines = ["# Cross-Constraint Omega Narrowing Report\n\n"]
    if not results:
        lines.append("No cross-constraint omega narrowing detected.\n")
    else:
        total_narrowings = sum(
            len(omegas)
            for group in results.values()
            for omegas in group["constraints"].values()
        )
        lines.append(
            f"**{len(results)} beneficiary groups** with cross-constraint omega narrowing. "
            f"**{total_narrowings} total narrowings.**\n\n"
        )
        for bene, group in results.items():
            lines.append(f"## Beneficiary: `{bene}` (n={group['group_size']})\n\n")
            lines.append(f"**Group:** {', '.join(group['group_ids'])}\n\n")
            for cid, omegas in group["constraints"].items():
                lines.append(f"### {cid}\n\n")
                for o in omegas:
                    sev = o.get("omega_severity", "?")
                    otype = o.get("omega_type", "?")
                    lines.append(f"**Omega `{o['omega_id']}`** [{sev}, {otype}]\n\n")
                    for note in o["narrowing_notes"]:
                        lines.append(f"- {note}\n")
                    lines.append("\n")
    path.write_text("".join(lines))


if __name__ == "__main__":
    run()
