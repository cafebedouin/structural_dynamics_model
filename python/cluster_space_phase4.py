#!/usr/bin/env python3
"""
Cluster-Space Audit Phase 4: FCR/FSM Precedence Investigation

Part A: Per-constraint profile for the 3 FCR-caught mountains
         (age_related_capacity_erosion, introspective_access_limits,
          physical_airgap_authenticity)
Part B: Group centroid computation (4 groups), 4x4 centroid cosine matrix,
        3x4 constraint-to-centroid table, MaxEnt top-3 per constraint
Part C: Narrative digest

Outputs:
  outputs/cluster_space_phase4.md
  outputs/cluster_space_phase4.json
"""

import json
from pathlib import Path

import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

ROOT     = Path(__file__).resolve().parent.parent
PIPELINE = ROOT / "outputs" / "pipeline_output.json"
PHASE3   = ROOT / "outputs" / "cluster_space_phase3.json"
OUT_MD   = ROOT / "outputs" / "cluster_space_phase4.md"
OUT_JSON = ROOT / "outputs" / "cluster_space_phase4.json"

# Copied verbatim from cluster_space_audit.py for self-containment
SIGNATURE_FLAGS    = ["false_natural_law", "false_ci_rope", "natural_law",
                      "constructed_high_extraction", "coupling_invariant_rope"]
MAXENT_TYPES       = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"]
OBSERVER_POSITIONS = ["powerless", "moderate", "institutional", "analytical"]

GROUP_LABELS = ["clean_mountain", "fsm_caught", "fcr_caught_mountain",
                "genuine_fcr_tangled_rope"]


# ─────────────────────────────────────────────────────────────────
# Data loading
# ─────────────────────────────────────────────────────────────────

def load_pipeline():
    print("Loading pipeline_output.json ...", flush=True)
    raw = json.loads(PIPELINE.read_text())
    idx = {}
    for c in raw["per_constraint"]:
        cid = c.get("id") or c.get("constraint_id")
        if cid:
            idx[cid] = c
    print(f"  {len(idx)} constraints", flush=True)
    return idx


def load_fcr_ids():
    d = json.loads(PHASE3.read_text())
    ids = d.get("bin2_fsm_nonfired", [])
    print(f"FCR-caught IDs from Phase 3: {ids}", flush=True)
    return ids


# ─────────────────────────────────────────────────────────────────
# Metric vector (identical to cluster_space_audit.py)
# ─────────────────────────────────────────────────────────────────

def build_metric_vector(c):
    """16-dim: chi×4, epsilon, maxent×6, signature flags×5."""
    pchi = c.get("perspective_chi") or {}
    vec = []
    for pos in OBSERVER_POSITIONS:
        vec.append(float((pchi.get(pos) or {}).get("chi", 0.0)))
    eps = float((pchi.get("powerless") or {}).get(
        "epsilon", c.get("base_extractiveness", 0.0)))
    vec.append(eps)
    mp = c.get("maxent_probs") or {}
    for t in MAXENT_TYPES:
        vec.append(float(mp.get(t, 1.0 / 6)))
    sig = c.get("signature") or ""
    for flag in SIGNATURE_FLAGS:
        vec.append(1.0 if sig == flag else 0.0)
    return vec


# ─────────────────────────────────────────────────────────────────
# Part A — Per-constraint profiles
# ─────────────────────────────────────────────────────────────────

def extract_profile(c):
    cid  = c.get("id") or c.get("constraint_id", "")
    pchi = c.get("perspective_chi") or {}

    chi_by_obs = {}
    for pos in OBSERVER_POSITIONS:
        chi_by_obs[pos] = float((pchi.get(pos) or {}).get("chi", 0.0))

    chi_vals   = list(chi_by_obs.values())
    chi_spread = max(chi_vals) - min(chi_vals) if chi_vals else 0.0
    hi_obs     = max(chi_by_obs, key=chi_by_obs.get) if chi_by_obs else None
    lo_obs     = min(chi_by_obs, key=chi_by_obs.get) if chi_by_obs else None

    eps = float((pchi.get("powerless") or {}).get(
        "epsilon", c.get("base_extractiveness", 0.0)))

    mp = c.get("maxent_probs") or {}
    maxent_top3 = sorted(mp.items(), key=lambda kv: kv[1], reverse=True)[:3]

    return {
        "id":            cid,
        "human_readable": c.get("human_readable", ""),
        "topic_domain":  c.get("topic_domain", ""),
        "epsilon":       eps,
        "signature":     c.get("signature", ""),
        "claimed_type":  c.get("claimed_type", ""),
        "beneficiaries": c.get("beneficiaries") or [],
        "victims":       c.get("victims") or [],
        "chi_by_observer": chi_by_obs,
        "chi_spread":    round(chi_spread, 4),
        "chi_hi_observer": hi_obs,
        "chi_lo_observer": lo_obs,
        "maxent_top3":   [(t, round(v, 4)) for t, v in maxent_top3],
    }


# ─────────────────────────────────────────────────────────────────
# Part B — Group definitions and centroid computation
# ─────────────────────────────────────────────────────────────────

def partition_groups(constraints, fcr_ids):
    """
    Group 1: clean mountains (mountain, empty beneficiaries AND victims)
    Group 2: FSM-caught   (mountain, signature==false_summit_mountain)
    Group 3: FCR-caught mountains (mountain, signature==false_ci_rope)
    Group 4: genuine FCR tangled_ropes (tangled_rope, signature==false_ci_rope)
    """
    g1, g2, g3, g4 = [], [], [], []
    for c in constraints.values():
        ct  = c.get("claimed_type", "")
        sig = c.get("signature", "")
        ben = c.get("beneficiaries") or []
        vic = c.get("victims") or []

        if ct == "mountain" and sig == "false_summit_mountain":
            g2.append(c)
        elif ct == "mountain" and sig == "false_ci_rope":
            g3.append(c)
        elif ct == "mountain" and not ben and not vic:
            g1.append(c)
        elif ct == "tangled_rope" and sig == "false_ci_rope":
            g4.append(c)

    print(f"Groups: G1={len(g1)}, G2={len(g2)}, G3={len(g3)}, G4={len(g4)}",
          flush=True)

    # Sanity: G3 should match fcr_ids exactly
    g3_ids = sorted(c.get("id") or c.get("constraint_id") for c in g3)
    if sorted(fcr_ids) != g3_ids:
        print(f"  WARNING: FCR-caught IDs mismatch — phase3={sorted(fcr_ids)}, "
              f"computed={g3_ids}", flush=True)

    return g1, g2, g3, g4


def group_centroid(group):
    """Mean of 16-dim metric vectors across all group members."""
    vecs = np.array([build_metric_vector(c) for c in group], dtype=float)
    return vecs.mean(axis=0)


# ─────────────────────────────────────────────────────────────────
# Output generation
# ─────────────────────────────────────────────────────────────────

def fmt_f(x, decimals=4):
    return f"{x:.{decimals}f}"


def write_md(profiles, group_sizes, centroid_matrix, constraint_centroid_table,
             narrative):
    lines = []
    lines.append("# Cluster-Space Phase 4: FCR/FSM Precedence Investigation\n")

    # ── Part A ──────────────────────────────────────────────────
    lines.append("## Part A — FCR-Caught Mountain Profiles\n")
    lines.append(
        "The three beneficiary-having mountains whose signature is `false_ci_rope` "
        "(FCR) rather than `false_summit_mountain` (FSM).\n"
    )

    hdr = ("| ID | ε | sig | χ_pow | χ_mod | χ_inst | χ_ana "
           "| χ_spread | beneficiaries | victims |")
    sep = ("|---|---|---|---|---|---|---|---|---|---|")
    lines.append(hdr)
    lines.append(sep)
    for p in profiles:
        chi = p["chi_by_observer"]
        ben = ", ".join(p["beneficiaries"]) or "—"
        vic = ", ".join(p["victims"]) or "—"
        row = (f"| {p['id']} "
               f"| {fmt_f(p['epsilon'],3)} "
               f"| {p['signature']} "
               f"| {fmt_f(chi.get('powerless',0),4)} "
               f"| {fmt_f(chi.get('moderate',0),4)} "
               f"| {fmt_f(chi.get('institutional',0),4)} "
               f"| {fmt_f(chi.get('analytical',0),4)} "
               f"| {fmt_f(p['chi_spread'],4)} "
               f"| {ben} "
               f"| {vic} |")
        lines.append(row)
    lines.append("")

    lines.append("### MaxEnt Top-3 per Constraint\n")
    for p in profiles:
        top3 = ", ".join(f"{t} ({fmt_f(v,4)})" for t, v in p["maxent_top3"])
        lines.append(f"- **{p['id']}**: {top3}")
    lines.append("")

    # ── Part B ──────────────────────────────────────────────────
    lines.append("## Part B — Group Centroid Analysis\n")

    lines.append("### B1 — Group Sizes\n")
    labels_short = ["G1 clean_mountain", "G2 fsm_caught",
                    "G3 fcr_caught_mountain", "G4 genuine_fcr_tangled_rope"]
    for lbl, (k, n) in zip(labels_short, group_sizes.items()):
        lines.append(f"- **{lbl}**: {n} constraints")
    lines.append("")

    lines.append("### B2 — 4×4 Centroid Cosine Similarity Matrix\n")
    col_hdr = "| | G1 | G2 | G3 | G4 |"
    col_sep = "|---|---|---|---|---|"
    lines.append(col_hdr)
    lines.append(col_sep)
    for i, row_lbl in enumerate(["G1", "G2", "G3", "G4"]):
        cells = " | ".join(fmt_f(centroid_matrix[i][j], 4) for j in range(4))
        lines.append(f"| {row_lbl} | {cells} |")
    lines.append("")

    lines.append("### B3 — 3×4 Constraint-to-Centroid Similarities\n")
    hdr2 = "| Constraint | sim_G1 | sim_G2 | sim_G3* | sim_G4 | Closest (excl G3) |"
    sep2 = "|---|---|---|---|---|---|"
    lines.append(hdr2)
    lines.append(sep2)
    for row in constraint_centroid_table:
        lines.append(
            f"| {row['id']} "
            f"| {fmt_f(row['sim_g1'],4)} "
            f"| {fmt_f(row['sim_g2'],4)} "
            f"| {fmt_f(row['sim_g3'],4)}* "
            f"| {fmt_f(row['sim_g4'],4)} "
            f"| {row['closest_excl_g3']} |"
        )
    lines.append("")
    lines.append("\\* sim_G3 is near 1.0 by construction (constraints are G3 members); "
                 "Closest column excludes G3.")
    lines.append("")

    # ── Part C ──────────────────────────────────────────────────
    lines.append("## Part C — Narrative Digest\n")
    for bullet in narrative:
        lines.append(f"- {bullet}")
    lines.append("")

    OUT_MD.write_text("\n".join(lines))
    print(f"Wrote {OUT_MD}", flush=True)


# ─────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────

def main():
    constraints = load_pipeline()
    fcr_ids     = load_fcr_ids()

    # ── Part A ──────────────────────────────────────────────────
    profiles = []
    for cid in fcr_ids:
        c = constraints.get(cid)
        if c is None:
            print(f"  WARNING: {cid} not found in pipeline", flush=True)
            continue
        profiles.append(extract_profile(c))
    print(f"Part A: {len(profiles)} profiles extracted", flush=True)

    # ── Part B ──────────────────────────────────────────────────
    g1, g2, g3, g4 = partition_groups(constraints, fcr_ids)

    centroids = []
    for grp, name in zip([g1, g2, g3, g4], GROUP_LABELS):
        if grp:
            centroids.append(group_centroid(grp))
        else:
            centroids.append(np.zeros(16))
            print(f"  WARNING: {name} is empty — centroid is zero vector", flush=True)

    centroid_mat = np.array(centroids)
    sim_matrix   = cosine_similarity(centroid_mat)

    # 3×4 constraint-to-centroid table
    group_keys  = ["G1", "G2", "G3", "G4"]
    label_map   = {
        "G1": "clean_mountain",
        "G2": "fsm_caught",
        "G3": "fcr_caught_mountain",
        "G4": "genuine_fcr_tangled_rope",
    }
    constraint_centroid_table = []
    for p in profiles:
        c   = constraints[p["id"]]
        vec = np.array([build_metric_vector(c)])
        sims = cosine_similarity(vec, centroid_mat)[0]
        row = {
            "id":     p["id"],
            "sim_g1": round(float(sims[0]), 4),
            "sim_g2": round(float(sims[1]), 4),
            "sim_g3": round(float(sims[2]), 4),
            "sim_g4": round(float(sims[3]), 4),
        }
        # Closest excluding G3 (self-group — these constraints are G3 members,
        # so sim_g3 ≈ 1.0 by construction and is not diagnostic)
        non_self = [(0, sims[0]), (1, sims[1]), (3, sims[3])]
        best_non_self = max(non_self, key=lambda t: t[1])
        row["closest_excl_g3"] = label_map[group_keys[best_non_self[0]]]
        constraint_centroid_table.append(row)

    # ── Part C — Narrative digest ────────────────────────────────
    g3_vs_g1 = sim_matrix[2][0]
    g3_vs_g2 = sim_matrix[2][1]
    g3_vs_g4 = sim_matrix[2][3]

    maxent_wants = []
    for p in profiles:
        top_type = p["maxent_top3"][0][0] if p["maxent_top3"] else "unknown"
        maxent_wants.append(f"{p['id']} → {top_type}")
    maxent_top_types = set(p["maxent_top3"][0][0] for p in profiles if p["maxent_top3"])

    # Key finding: individual non-self closest group
    non_self_closest = set(r["closest_excl_g3"] for r in constraint_centroid_table)
    individual_summary = (
        "All three Bin-2 FCR-caught mountains (excluding self-group G3) sit closest to "
        f"**{non_self_closest.pop()}** among G1/G2/G4."
        if len(non_self_closest) == 1 else
        "Bin-2 FCR-caught mountains split across groups (excluding self): "
        + ", ".join(f"{r['id']} → {r['closest_excl_g3']}" for r in constraint_centroid_table)
    )

    # Phase 3 blind spot: 8 FCR-carrying mountains without beneficiaries were in Bin 1
    phase3_g3_total  = len(g3)
    phase3_g3_bin2   = len(fcr_ids)
    phase3_g3_hidden = phase3_g3_total - phase3_g3_bin2

    maxent_narrative = (
        "MaxEnt top-1 types: " + "; ".join(maxent_wants) + ". "
        + ("MaxEnt unanimously predicts mountain, consistent with metric placement."
           if maxent_top_types == {"mountain"} else
           "MaxEnt top types diverge from mountain, partially contradicting metric placement.")
    )

    # FCR/FSM verdict: if G3 clusters with G2 (FSM-caught) rather than G4 (genuine FCR TR),
    # the FCR/FSM distinction is NOT a metric-space distinction — it is a signature-pathway
    # distinction among constraints that share the same metric profile.
    if g3_vs_g2 >= g3_vs_g1 and g3_vs_g2 >= g3_vs_g4:
        precedence_verdict = (
            "FCR suppression of FSM is **neither supported nor refuted** by metric "
            "geometry in the expected sense. The FCR-caught mountains (G3) cluster "
            f"with FSM-caught mountains (G2, sim={fmt_f(g3_vs_g2,4)}), not with "
            f"genuine FCR tangled_ropes (G4, sim={fmt_f(g3_vs_g4,4)}) or clean "
            f"mountains (G1, sim={fmt_f(g3_vs_g1,4)}). The FCR/FSM split is a "
            "signature-pathway distinction among metrically similar beneficiary-having "
            "mountains, not a metric-space separation."
        )
    elif g3_vs_g4 > g3_vs_g1:
        precedence_verdict = (
            f"FCR suppression of FSM is **supported** by metric geometry: G3 sits "
            f"closer to G4 genuine FCR tangled_ropes ({fmt_f(g3_vs_g4,4)}) than to "
            f"G1 clean mountains ({fmt_f(g3_vs_g1,4)})."
        )
    else:
        precedence_verdict = (
            f"FCR suppression of FSM is **not supported** by metric geometry: G3 sits "
            f"closer to G1 clean mountains ({fmt_f(g3_vs_g1,4)}) than to G4 genuine "
            f"FCR tangled_ropes ({fmt_f(g3_vs_g4,4)})."
        )

    phase3_blind_spot = (
        f"Phase 3 Bin 1 blind spot: G3 (FCR-carrying mountains) has {phase3_g3_total} "
        f"members total — {phase3_g3_bin2} with beneficiaries (Bin 2, profiled above) "
        f"and {phase3_g3_hidden} without beneficiaries that Phase 3 silently included "
        f"in its clean-mountain Bin 1 count. Phase 3 Bin 1 (403, not 411) excludes them "
        f"in this analysis."
    )

    narrative = [
        f"4×4 centroid similarities (G3 row): "
        f"vs G1={fmt_f(g3_vs_g1,4)}, vs G2={fmt_f(g3_vs_g2,4)}, "
        f"vs G4={fmt_f(g3_vs_g4,4)}.",
        individual_summary,
        maxent_narrative,
        precedence_verdict,
        phase3_blind_spot,
    ]

    # ── Output ──────────────────────────────────────────────────
    group_sizes = {
        "1": len(g1),
        "2": len(g2),
        "3": len(g3),
        "4": len(g4),
    }

    write_md(profiles, group_sizes,
             sim_matrix.tolist(), constraint_centroid_table, narrative)

    result = {
        "metadata": {
            "fcr_caught_ids": fcr_ids,
            "group_sizes":    group_sizes,
            "group_labels":   GROUP_LABELS,
        },
        "part_a": {
            "profiles": profiles,
        },
        "part_b": {
            "centroid_cosine_matrix":     [[round(v, 4) for v in row]
                                           for row in sim_matrix.tolist()],
            "group_labels":               GROUP_LABELS,
            "constraint_centroid_table":  constraint_centroid_table,
        },
        "part_c": {
            "narrative": narrative,
        },
    }
    OUT_JSON.write_text(json.dumps(result, indent=2))
    print(f"Wrote {OUT_JSON}", flush=True)


if __name__ == "__main__":
    main()
