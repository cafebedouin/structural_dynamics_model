"""
Evaluative convergence analysis (v1.0).

Groups constraints by shared beneficiary (scenario_convergence.json) and
network adjacency (contamination_network in enriched_pipeline.json), then
detects four cross-constraint convergence patterns:

  convergent_signature       — ≥2 constraints in group share same signature
  convergent_institutional   — ≥2 constraints share institutional type while
                               analytical disagrees
  convergent_drift           — ≥2 constraints share same critical-severity drift
  cover_story_topology       — one constraint (face) structurally conceals extractive
                               neighbors via contamination network

Usage: python3 python/evaluative_convergence.py
Output: outputs/evaluative_convergence.json
"""

import hashlib
import json
import sys
from collections import Counter, defaultdict, deque
from datetime import datetime, timezone
from pathlib import Path

_REPO_ROOT = Path(__file__).resolve().parent.parent
PIPELINE = _REPO_ROOT / "outputs/enriched_pipeline.json"
SCENARIO = _REPO_ROOT / "outputs/scenario_convergence.json"
OMEGA    = _REPO_ROOT / "outputs/omega_cross_constraint.json"
OUT_JSON = _REPO_ROOT / "outputs/evaluative_convergence.json"


# ---------------------------------------------------------------------------
# Input loading
# ---------------------------------------------------------------------------

def load_inputs(pipeline_path, scenario_path, omega_path):
    """Load all three input files.

    pipeline_path is required — exits 1 if missing.
    scenario_path returns [] when missing.
    omega_path returns {} when missing.
    """
    if not pipeline_path.exists():
        print(f"[evaluative_convergence] {pipeline_path} not found — run pipeline first.")
        sys.exit(1)

    pipeline_dict = json.loads(pipeline_path.read_text(encoding="utf-8"))

    scenario_data = []
    if scenario_path.exists():
        scenario_data = json.loads(scenario_path.read_text(encoding="utf-8"))

    omega_data = {}
    if omega_path.exists():
        omega_data = json.loads(omega_path.read_text(encoding="utf-8"))

    return pipeline_dict, scenario_data, omega_data


# ---------------------------------------------------------------------------
# Index builders
# ---------------------------------------------------------------------------

def build_signature_index(per_constraint_list):
    """Return {constraint_id: signature} for all constraints."""
    return {e["id"]: e.get("signature") for e in per_constraint_list}


# ---------------------------------------------------------------------------
# Set construction — Source 2: network adjacency
# ---------------------------------------------------------------------------

def build_network_adjacency_sets(per_constraint_list):
    """Build undirected graph from contamination_network.neighbors (explicit edges only)
    and return connected components with ≥2 members as frozensets.

    Only edge_type == "explicit" is used. shared_beneficiary and shared_victim edges
    are structural inferences that span the full corpus and would create a single
    giant component — they are excluded here.

    Dangling references (neighbors not in per_constraint_list) are skipped.
    """
    known_ids = {e["id"] for e in per_constraint_list}

    adjacency = defaultdict(set)
    for entry in per_constraint_list:
        cid = entry["id"]
        cn = entry.get("contamination_network")
        if cn is None:
            continue
        for neighbor in cn.get("neighbors", []):
            if neighbor.get("edge_type") != "explicit":
                continue
            nid = neighbor.get("constraint_id")
            if nid and nid in known_ids:
                adjacency[cid].add(nid)
                adjacency[nid].add(cid)

    visited = set()
    components = []
    for cid in known_ids:
        if cid in visited or cid not in adjacency:
            continue
        # BFS
        component = set()
        queue = deque([cid])
        while queue:
            node = queue.popleft()
            if node in visited:
                continue
            visited.add(node)
            component.add(node)
            for neighbor in adjacency.get(node, set()):
                if neighbor not in visited:
                    queue.append(neighbor)
        if len(component) >= 2:
            components.append(frozenset(component))

    return components


# ---------------------------------------------------------------------------
# Set construction — Source 1: beneficiary groups
# ---------------------------------------------------------------------------

def build_beneficiary_sets(scenario_data):
    """Extract (beneficiary, frozenset_of_ids) tuples from scenario_convergence data.

    scenario_data is a bare array (Turn 3 confirmed schema).
    """
    result = []
    for entry in scenario_data:
        beneficiary = entry.get("beneficiary")
        group_ids = entry.get("group_ids", [])
        if beneficiary and len(group_ids) >= 2:
            result.append((beneficiary, frozenset(group_ids)))
    return result


# ---------------------------------------------------------------------------
# Set merging — deduplication
# ---------------------------------------------------------------------------

def merge_sets(beneficiary_sets, network_components):
    """Merge beneficiary and network sets, deduplicating network sets that are
    subsets of any beneficiary set.

    Returns list of (set_id, shared_beneficiary, frozenset_of_member_ids).
    """
    bene_id_sets = [ids for _, ids in beneficiary_sets]
    merged = []

    # Add all beneficiary sets
    for beneficiary, ids in beneficiary_sets:
        merged.append((beneficiary, beneficiary, ids))

    # Add network components not covered by any beneficiary set
    for component in network_components:
        is_covered = any(component <= bene_ids for bene_ids in bene_id_sets)
        if not is_covered:
            sorted_ids = sorted(component)
            fingerprint = hashlib.md5(",".join(sorted_ids).encode()).hexdigest()[:8]
            set_id = f"network_{fingerprint}"
            merged.append((set_id, None, component))

    return merged


# ---------------------------------------------------------------------------
# Pattern detectors
# ---------------------------------------------------------------------------

def detect_cover_story_topology(constraint_id, entry, signature_index):
    """Detect cover story face: one constraint with clean metrics contaminated by
    extractive neighbors that share its structural signature.

    Conditions (all required):
      (a) base_extractiveness is not None and <= 0.25
      (b) contamination_network.intrinsic_purity >= 0.80
      (c) contamination_network.propagation_delta <= -0.15
      (d) ≥1 neighbor with neighbor_type in {snare, tangled_rope}
          AND neighbor_purity is not None AND neighbor_purity < 0.40
      (e) ≥1 neighbor satisfying (d) shares signature with this constraint

    Returns evidence dict or None.
    """
    # (a)
    base_eps = entry.get("base_extractiveness")
    if base_eps is None or base_eps > 0.25:
        return None

    cn = entry.get("contamination_network")
    if cn is None:
        return None

    # (b)
    intrinsic = cn.get("intrinsic_purity")
    if intrinsic is None or intrinsic < 0.80:
        return None

    # (c)
    delta = cn.get("propagation_delta")
    if delta is None or delta > -0.15:
        return None

    entry_signature = entry.get("signature")
    neighbors = cn.get("neighbors", [])

    # (d) qualifying neighbors (extractive type + low purity)
    qualifying = []
    for n in neighbors:
        ntype = n.get("neighbor_type")
        npurity = n.get("neighbor_purity")
        if ntype not in ("snare", "tangled_rope"):
            continue
        if npurity is None or npurity >= 0.40:
            continue
        qualifying.append(n)

    if not qualifying:
        return None

    # (e) at least one qualifying neighbor shares signature
    matching = []
    for n in qualifying:
        nid = n.get("constraint_id")
        if nid is None:
            continue
        nsig = signature_index.get(nid)
        if nsig is not None and nsig == entry_signature:
            matching.append(n)

    if not matching:
        return None

    return {
        "face_constraint": constraint_id,
        "extractive_members": [n["constraint_id"] for n in qualifying],
        "face_base_extractiveness": base_eps,
        "face_intrinsic_purity": intrinsic,
        "face_propagation_delta": delta,
        "qualifying_neighbors": [
            {
                "constraint_id": n["constraint_id"],
                "neighbor_type": n["neighbor_type"],
                "neighbor_purity": n["neighbor_purity"],
                "shared_signature": entry_signature,
            }
            for n in matching
        ],
    }


def detect_convergent_signature(member_ids, signature_index):
    """Fire when ≥2 constraints in the group share the same non-null signature.

    Returns evidence dict (most frequent shared signature) or None.
    """
    sig_to_ids = defaultdict(list)
    for cid in member_ids:
        sig = signature_index.get(cid)
        if sig is not None:
            sig_to_ids[sig].append(cid)

    shared = {sig: ids for sig, ids in sig_to_ids.items() if len(ids) >= 2}
    if not shared:
        return None

    dominant = max(shared, key=lambda s: len(shared[s]))
    return {
        "shared_signature": dominant,
        "constraints": shared[dominant],
    }


def detect_convergent_institutional(member_ids, per_constraint_index):
    """Fire when ≥2 constraints share the same institutional type while each
    has analytical != institutional (individual-level split, group-level convergence).

    Returns evidence dict or None.
    """
    splits = []
    for cid in member_ids:
        entry = per_constraint_index.get(cid, {})
        perspectives = entry.get("perspectives") or {}
        inst = perspectives.get("institutional")
        anal = perspectives.get("analytical")
        if inst and anal and inst != anal:
            splits.append((cid, inst, anal))

    if len(splits) < 2:
        return None

    inst_counts = Counter(inst for _, inst, _ in splits)
    dominant_inst, count = inst_counts.most_common(1)[0]
    if count < 2:
        return None

    matching = [(cid, inst, anal) for cid, inst, anal in splits if inst == dominant_inst]
    anal_counts = Counter(anal for _, _, anal in matching)
    dominant_anal = anal_counts.most_common(1)[0][0]

    return {
        "institutional_type": dominant_inst,
        "analytical_type": dominant_anal,
        "constraints_with_split": [cid for cid, _, _ in matching],
    }


def detect_convergent_drift(member_ids, per_constraint_index):
    """Fire when ≥2 constraints in the group share the same drift type at
    critical severity.

    Returns evidence dict (most frequent qualifying drift type) or None.
    """
    drift_type_to_ids = defaultdict(set)
    for cid in member_ids:
        entry = per_constraint_index.get(cid, {})
        for d in entry.get("drift_events") or []:
            if d.get("severity") == "critical" and d.get("type"):
                drift_type_to_ids[d["type"]].add(cid)

    qualifying = {dt: sorted(ids) for dt, ids in drift_type_to_ids.items() if len(ids) >= 2}
    if not qualifying:
        return None

    dominant = max(qualifying, key=lambda dt: len(qualifying[dt]))
    return {
        "shared_drift_type": dominant,
        "severity": "critical",
        "constraints": qualifying[dominant],
    }


# ---------------------------------------------------------------------------
# Defensibility
# ---------------------------------------------------------------------------

def build_defensibility(patterns):
    """Build defensibility assessment from fired patterns."""
    constrained = []
    indefensible = []
    pattern_names = {p["pattern"] for p in patterns}

    if "convergent_signature" in pattern_names and "convergent_institutional" in pattern_names:
        indefensible.append({
            "position": "Treating all constraints in this set as independent coordination mechanisms",
            "ruled_out_by": (
                "Convergent signature and convergent institutional classification: "
                "all constraints share the same structural signature and the same "
                "institutional observer type, indicating coordinated rather than "
                "independent operation."
            ),
        })
        constrained.append(
            "Reform interventions must address the group as a coordinated system, "
            "not individual constraints."
        )

    cover = next((p for p in patterns if p["pattern"] == "cover_story_topology"), None)
    if cover:
        face = cover["evidence"].get("face_constraint", "?")
        neighbors_str = ", ".join(
            n["constraint_id"]
            for n in cover["evidence"].get("qualifying_neighbors", [])
        )
        indefensible.append({
            "position": f"Treating {face} as benign without accounting for network contamination",
            "ruled_out_by": (
                f"Cover story topology: {face} has base extractiveness ≤0.25 and "
                f"intrinsic purity ≥0.80 but propagation delta ≤-0.15 from "
                f"extractive neighbors ({neighbors_str}). The clean appearance is "
                "structurally produced by the contamination network."
            ),
        })

    drift = next((p for p in patterns if p["pattern"] == "convergent_drift"), None)
    if drift:
        indefensible.append({
            "position": "Current type classifications for all constraints in this set are stable",
            "ruled_out_by": (
                f"Convergent critical-severity drift "
                f"({drift['evidence']['shared_drift_type']}) across "
                f"{len(drift['evidence']['constraints'])} constraints indicates "
                "active systemic instability, not constraint-local drift."
            ),
        })
        constrained.append(
            "Type classifications for this set should be treated as temporally "
            "unstable pending drift resolution."
        )

    return {
        "constrained_positions": constrained,
        "indefensible_positions": indefensible,
    }


# ---------------------------------------------------------------------------
# Top-level set builder
# ---------------------------------------------------------------------------

def build_constraint_sets(pipeline_dict, scenario_data, omega_data):
    """Build all constraint sets and detect patterns for each."""
    per_constraint_list = pipeline_dict.get("per_constraint", [])
    per_constraint_index = {e["id"]: e for e in per_constraint_list}
    signature_index = build_signature_index(per_constraint_list)

    beneficiary_sets = build_beneficiary_sets(scenario_data)
    network_components = build_network_adjacency_sets(per_constraint_list)
    merged = merge_sets(beneficiary_sets, network_components)

    constraint_sets = []

    for set_id, shared_beneficiary, member_ids_frozen in merged:
        member_ids = sorted(member_ids_frozen)

        patterns = []

        sig_result = detect_convergent_signature(member_ids, signature_index)
        if sig_result:
            patterns.append({
                "pattern": "convergent_signature",
                "evidence": sig_result,
                "constraints_involved": sig_result["constraints"],
            })

        inst_result = detect_convergent_institutional(member_ids, per_constraint_index)
        if inst_result:
            patterns.append({
                "pattern": "convergent_institutional",
                "evidence": inst_result,
                "constraints_involved": inst_result["constraints_with_split"],
            })

        drift_result = detect_convergent_drift(member_ids, per_constraint_index)
        if drift_result:
            patterns.append({
                "pattern": "convergent_drift",
                "evidence": drift_result,
                "constraints_involved": drift_result["constraints"],
            })

        # Cover story: check each member; keep first qualifying face
        for cid in member_ids:
            entry = per_constraint_index.get(cid, {})
            cover = detect_cover_story_topology(cid, entry, signature_index)
            if cover:
                patterns.append({
                    "pattern": "cover_story_topology",
                    "evidence": cover,
                    "constraints_involved": (
                        [cover["face_constraint"]] + cover["extractive_members"]
                    ),
                })
                break  # at most one cover story face per set

        if not patterns:
            continue

        defensibility = build_defensibility(patterns) if len(patterns) >= 2 else {
            "constrained_positions": [],
            "indefensible_positions": [],
        }

        constraint_sets.append({
            "set_id": set_id,
            "constraints": member_ids,
            "shared_beneficiary": shared_beneficiary,
            "convergence_patterns": patterns,
            "defensibility": defensibility,
        })

    return constraint_sets


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    pipeline_dict, scenario_data, omega_data = load_inputs(PIPELINE, SCENARIO, OMEGA)

    constraint_sets = build_constraint_sets(pipeline_dict, scenario_data, omega_data)

    output = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "constraint_sets": constraint_sets,
    }

    OUT_JSON.write_text(json.dumps(output, indent=2), encoding="utf-8")
    print(
        f"[evaluative_convergence] {len(constraint_sets)} constraint sets "
        f"written to {OUT_JSON}"
    )


if __name__ == "__main__":
    main()
