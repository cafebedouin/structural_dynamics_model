"""
Scenario-level convergence diagnostic (v6.9).

Groups constraints by shared beneficiary and detects asymmetric convergence
patterns: situations where a set of linked constraints produces a diagnostic
signal that is not visible from any single constraint in isolation.

Four patterns detected:
  mountain_extraction_cover   — Mountain alongside extractive types (same beneficiary)
  coordinated_extraction      — All-extractive group (no protective types)
  convergent_accumulation     — ≥2 constraints with extraction_accumulation drift
  convergent_abductive        — Dominant abductive trigger class across group

Usage: python3 python/scenario_convergence.py
Output: outputs/scenario_convergence.json
        outputs/scenario_convergence_report.md
"""

import json
from pathlib import Path
from collections import defaultdict, Counter

PIPELINE = Path("outputs/enriched_pipeline.json")
OUT_JSON = Path("outputs/scenario_convergence.json")
OUT_MD   = Path("outputs/scenario_convergence_report.md")

EXTRACTIVE_TYPES = {"snare", "tangled_rope", "piton"}
PROTECTIVE_TYPES = {"mountain", "rope", "scaffold"}
MIN_GROUP_SIZE   = 2


def detect_flags(group_ids, entry_map):
    """
    Detect asymmetric convergence flags for a beneficiary group.
    Returns list of {"flag": str, "description": str, "evidence": dict}.
    """
    flags = []

    types = {cid: entry_map.get(cid, {}).get("claimed_type") for cid in group_ids}
    type_set = {t for t in types.values() if t}
    extractive = type_set & EXTRACTIVE_TYPES
    protective = type_set & PROTECTIVE_TYPES

    # Collect per-constraint data
    drift_by_constraint = {
        cid: [d.get("type") for d in entry_map.get(cid, {}).get("drift_events", []) if d.get("type")]
        for cid in group_ids
    }
    # abductive_triggers uses 'trigger_class' key
    triggers_by_constraint = {
        cid: [t.get("trigger_class") for t in entry_map.get(cid, {}).get("abductive_triggers", []) if t.get("trigger_class")]
        for cid in group_ids
    }
    sig_by_constraint = {
        cid: entry_map.get(cid, {}).get("signature")
        for cid in group_ids
    }

    # Pattern 1: Mountain + extractive type with same beneficiary
    mountain_ids = [c for c in group_ids if types[c] == "mountain"]
    snare_ids    = [c for c in group_ids if types[c] in EXTRACTIVE_TYPES]
    if mountain_ids and snare_ids:
        # Check if any mountains have false_summit_mountain signature
        fsm_mountains = [c for c in mountain_ids if sig_by_constraint[c] == "false_summit_mountain"]
        note = (
            f"Mountain constraints ({mountain_ids}) and extractive constraints "
            f"({snare_ids}) share beneficiary. The Mountain may provide legitimating "
            "cover for the extractive constraints — naturalized constraint protecting "
            "active extraction."
        )
        if fsm_mountains:
            note += (
                f" Note: {fsm_mountains} carry false_summit_mountain signature, "
                "indicating the mountain classification itself has been overridden — "
                "the legitimation structure is already partially dismantled in the engine."
            )
        flags.append({
            "flag": "mountain_extraction_cover",
            "description": note,
            "evidence": {
                "mountain_ids": mountain_ids,
                "extractive_ids": snare_ids,
                "fsm_overridden": fsm_mountains,
            },
        })

    # Pattern 2: All-extractive group — coordinated extraction
    if extractive and not protective:
        flags.append({
            "flag": "coordinated_extraction",
            "description": (
                f"All {len(group_ids)} constraints in group are extractive types "
                f"({sorted(extractive)}). Beneficiary is extracting across multiple "
                "constraint dimensions simultaneously. Single-constraint reform will "
                "be insufficient — the extraction has multiple structural anchors."
            ),
            "evidence": {
                "types": dict(Counter(types.values())),
                "all_extractive": sorted(extractive),
            },
        })

    # Pattern 3: Convergent extraction_accumulation drift
    accumulation_ids = [
        c for c in group_ids
        if "extraction_accumulation" in drift_by_constraint[c]
    ]
    if len(accumulation_ids) >= 2:
        flags.append({
            "flag": "convergent_accumulation",
            "description": (
                f"{len(accumulation_ids)} constraints in group show extraction_accumulation "
                f"drift ({accumulation_ids}). Temporal pattern suggests coordinated or "
                "systemic extraction pressure — the accumulation is not constraint-local."
            ),
            "evidence": {
                "accumulating_ids": accumulation_ids,
                "group_size": len(group_ids),
            },
        })

    # Pattern 4: Dominant abductive trigger class
    all_triggers = []
    for c in group_ids:
        all_triggers.extend(triggers_by_constraint[c])
    trigger_counts = Counter(all_triggers)
    total = len(group_ids)
    dominant = [
        t for t, n in trigger_counts.items()
        if n >= max(2, (total + 1) // 2)  # ≥ half the group, minimum 2
    ]
    if dominant:
        flags.append({
            "flag": "convergent_abductive",
            "description": (
                f"Dominant abductive trigger classes across group: {dominant}. "
                "Group-level diagnostic convergence. Trigger counts: "
                + str({t: trigger_counts[t] for t in dominant})
                + ". Individual constraint reports understate the pattern."
            ),
            "evidence": {
                "dominant_triggers": dominant,
                "trigger_counts": dict(trigger_counts),
                "group_size": total,
            },
        })

    return flags


def run():
    if not PIPELINE.exists():
        print(f"[scenario_convergence] {PIPELINE} not found — run pipeline first.")
        return

    data = json.loads(PIPELINE.read_text())
    entries = data.get("per_constraint", [])
    entry_map = {e["id"]: e for e in entries}

    bene_idx = defaultdict(list)
    for e in entries:
        for b in e.get("beneficiaries", []):
            bene_idx[b].append(e["id"])

    groups = {b: ids for b, ids in bene_idx.items() if len(ids) >= MIN_GROUP_SIZE}

    results = []
    for beneficiary, group_ids in sorted(groups.items()):
        flags = detect_flags(group_ids, entry_map)
        if flags:
            type_dist = Counter(
                entry_map.get(c, {}).get("claimed_type") for c in group_ids
            )
            results.append({
                "beneficiary": beneficiary,
                "group_size": len(group_ids),
                "group_ids": group_ids,
                "type_distribution": {k: v for k, v in type_dist.items() if k},
                "convergence_flags": flags,
            })

    OUT_JSON.write_text(json.dumps(results, indent=2))
    _write_report(results, OUT_MD)
    print(
        f"[scenario_convergence] {len(results)} groups with convergence flags "
        f"written to {OUT_JSON}"
    )


def _write_report(results, path):
    lines = ["# Scenario-Level Convergence Diagnostic\n\n"]
    if not results:
        lines.append("No convergence patterns detected.\n")
    else:
        flag_counts = Counter(
            f["flag"]
            for r in results
            for f in r["convergence_flags"]
        )
        lines.append(
            f"**{len(results)} beneficiary groups** with convergence flags.\n\n"
        )
        lines.append("**Pattern counts:**\n")
        for flag, count in sorted(flag_counts.items()):
            lines.append(f"- `{flag}`: {count}\n")
        lines.append("\n---\n\n")

        for r in results:
            lines.append(f"## Beneficiary: `{r['beneficiary']}` (n={r['group_size']})\n\n")
            lines.append(f"**Group:** {', '.join(r['group_ids'])}\n\n")
            lines.append(f"**Type distribution:** {r['type_distribution']}\n\n")
            for f in r["convergence_flags"]:
                lines.append(f"### [{f['flag']}]\n\n")
                lines.append(f"{f['description']}\n\n")
    path.write_text("".join(lines))


if __name__ == "__main__":
    run()
