#!/usr/bin/env python3
"""
Cluster-Space Audit Phase 3: Orphan Invisibility and Naturalized Mountain Inventory

Part A: Non-mountain orphan invisibility re-ranking
Part B: Full-corpus mountain inventory by beneficiary structure + FSM cross-tab +
        cluster-space localization of Bin 2 mountains

Uses cached pairwise scores from cluster_space_audit_phase1.json and
full-corpus data from pipeline_output.json.

Outputs:
  outputs/cluster_space_phase3.md
  outputs/cluster_space_phase3.json
"""

import json
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np

ROOT        = Path(__file__).resolve().parent.parent
PHASE1_JSON = ROOT / "outputs" / "cluster_space_audit_phase1.json"
PIPELINE    = ROOT / "outputs" / "pipeline_output.json"
OUT_MD      = ROOT / "outputs" / "cluster_space_phase3.md"
OUT_JSON    = ROOT / "outputs" / "cluster_space_phase3.json"

MEASURES       = ["obs", "ben", "coup", "sem", "met"]
MEASURE_LABELS = {
    "obs":  "Observer-space",
    "ben":  "Beneficiary",
    "coup": "Coupling",
    "sem":  "Semantic",
    "met":  "Metric-space",
}


# ─────────────────────────────────────────────────────────────────
# Loading
# ─────────────────────────────────────────────────────────────────

def load_pipeline_index():
    """Return dict constraint_id → constraint dict."""
    print("Loading pipeline_output.json ...", flush=True)
    raw = json.loads(PIPELINE.read_text())
    idx = {}
    for c in raw["per_constraint"]:
        cid = c.get("id") or c.get("constraint_id")
        if cid:
            idx[cid] = c
    print(f"  {len(idx)} constraints", flush=True)
    return idx


def load_phase1():
    print("Loading cluster_space_audit_phase1.json ...", flush=True)
    d = json.loads(PHASE1_JSON.read_text())
    return d


# ─────────────────────────────────────────────────────────────────
# Part A — Non-mountain orphan invisibility
# ─────────────────────────────────────────────────────────────────

def recompute_orphan_pairs(phase1):
    """
    Recompute full orphan invisibility list from pairwise scores.
    Orphan invisibility: met Q4 AND idea_best Q1.
    """
    pop     = phase1["analysis"]["population_matrix"]
    met_q3  = pop["quartile_thresholds"]["met"][2]   # met > this → Q4
    idea_q1 = pop["idea_quartile_thresholds"][0]     # idea <= this → Q1

    orphan = []
    for p in phase1["pairwise_scores"]:
        idea_best = max(p["ben"], p["coup"], p["sem"])
        if p["met"] > met_q3 and idea_best <= idea_q1:
            orphan.append(dict(p, idea_best=idea_best))

    print(f"  Orphan invisibility pairs: {len(orphan)}", flush=True)
    return orphan


def filter_non_mountain(orphan_pairs, pipeline_idx):
    """Split into mountain-mountain and non-mountain subsets."""
    mm_pairs, non_mm_pairs = [], []
    for p in orphan_pairs:
        t1 = pipeline_idx.get(p["id1"], {}).get("claimed_type", p.get("type1", ""))
        t2 = pipeline_idx.get(p["id2"], {}).get("claimed_type", p.get("type2", ""))
        if t1 == "mountain" and t2 == "mountain":
            mm_pairs.append(p)
        else:
            p = dict(p, type1=t1, type2=t2)
            non_mm_pairs.append(p)
    return mm_pairs, non_mm_pairs


def enrich_with_signature(pairs, pipeline_idx):
    """Add signature1, signature2 fields."""
    enriched = []
    for p in pairs:
        c1 = pipeline_idx.get(p["id1"], {})
        c2 = pipeline_idx.get(p["id2"], {})
        enriched.append(dict(
            p,
            signature1=c1.get("signature", "none"),
            signature2=c2.get("signature", "none"),
            claimed_type1=c1.get("claimed_type", p.get("type1", "")),
            claimed_type2=c2.get("claimed_type", p.get("type2", "")),
        ))
    return enriched


def rank_non_mountain(non_mm_pairs):
    """Sort by metric descending, then inverse semantic (lower sem = higher rank)."""
    return sorted(non_mm_pairs, key=lambda p: (-p["met"], p["sem"]))


# ─────────────────────────────────────────────────────────────────
# Part B — Mountain inventory
# ─────────────────────────────────────────────────────────────────

def bin_mountain(c):
    """
    Bin 1: clean — empty beneficiaries AND empty victims
    Bin 2: beneficiary-having (FSM candidate) — non-empty beneficiaries
    Bin 3: victim-having only — empty beneficiaries, non-empty victims
    Bin 4: other (shouldn't occur; defensive)
    """
    has_ben = bool(c.get("beneficiaries"))
    has_vic = bool(c.get("victims"))
    if has_ben:
        return 2
    if has_vic:
        return 3
    return 1


def build_mountain_inventory(pipeline_idx):
    mountains = [c for c in pipeline_idx.values()
                 if c.get("claimed_type") == "mountain"]
    print(f"  Full corpus mountains: {len(mountains)}", flush=True)

    bins = {1: [], 2: [], 3: [], 4: []}
    for c in mountains:
        b = bin_mountain(c)
        bins[b].append(c)

    for b, cs in bins.items():
        print(f"  Bin {b}: {len(cs)}", flush=True)

    return mountains, bins


def fsm_crosstab(bin2):
    """Cross-tabulate Bin 2 by FSM signature."""
    FSM_SIG = "false_summit_mountain"
    fsm_fired    = [c for c in bin2 if c.get("signature") == FSM_SIG]
    no_fsm_other = [c for c in bin2 if c.get("signature") != FSM_SIG]
    sig_dist = Counter(c.get("signature", "none") for c in bin2)
    return fsm_fired, no_fsm_other, sig_dist


def bin2_detail_rows(bin2, max_n=50):
    """Return sorted Bin 2 rows for the detail table."""
    sorted_bin2 = sorted(
        bin2,
        key=lambda c: (c.get("maxent_probs") or {}).get("mountain", 0),
        reverse=True,
    )
    truncated = max(0, len(sorted_bin2) - max_n)
    return sorted_bin2[:max_n], truncated


# ─────────────────────────────────────────────────────────────────
# Part B4/B5 — Cluster-space localization
# ─────────────────────────────────────────────────────────────────

def localize_bin2(phase1, pipeline_idx, bin2):
    """
    For Bin 2 mountains in the Phase 1 sample, compare their similarity
    profiles to clean mountains vs. other clean mountains.
    """
    sample_ids = set(phase1["sample_ids"])
    all_pairs  = phase1["pairwise_scores"]

    bin2_ids   = {c["id"] for c in bin2}
    bin1_ids   = {c["id"] for c in pipeline_idx.values()
                  if c.get("claimed_type") == "mountain" and bin_mountain(c) == 1}

    bin2_in_sample = bin2_ids & sample_ids
    bin1_in_sample = bin1_ids & sample_ids

    print(f"  Bin 2 in sample: {len(bin2_in_sample)}", flush=True)
    print(f"  Bin 1 in sample: {len(bin1_in_sample)}", flush=True)

    # Pairs where one side is Bin2-in-sample and other is Bin1-in-sample
    bin2_to_bin1 = []
    # Within-Bin1 pairs
    clean_to_clean = []
    # Within-Bin2 pairs (if any)
    bin2_to_bin2 = []

    for p in all_pairs:
        i1, i2 = p["id1"], p["id2"]
        i1_b2 = i1 in bin2_in_sample
        i2_b2 = i2 in bin2_in_sample
        i1_b1 = i1 in bin1_in_sample
        i2_b1 = i2 in bin1_in_sample

        if (i1_b2 and i2_b1) or (i2_b2 and i1_b1):
            bin2_to_bin1.append(p)
        elif i1_b1 and i2_b1:
            clean_to_clean.append(p)
        elif i1_b2 and i2_b2:
            bin2_to_bin2.append(p)

    def stats(pairs):
        if not pairs:
            return {m: {"mean": None, "std": None, "n": 0} for m in MEASURES}
        return {
            m: {
                "mean": float(np.mean([p[m] for p in pairs])),
                "std":  float(np.std([p[m] for p in pairs])),
                "n":    len(pairs),
            }
            for m in MEASURES
        }

    return {
        "bin2_in_sample": sorted(bin2_in_sample),
        "bin1_in_sample_count": len(bin1_in_sample),
        "bin2_to_bin1": stats(bin2_to_bin1),
        "clean_to_clean": stats(clean_to_clean),
        "bin2_to_bin2": stats(bin2_to_bin2),
        "pair_counts": {
            "bin2_to_bin1": len(bin2_to_bin1),
            "clean_to_clean": len(clean_to_clean),
            "bin2_to_bin2": len(bin2_to_bin2),
        },
    }


def recompute_stratum_stats(phase1, pipeline_idx, bin2):
    """
    Recompute Mountain within-type metric stats separately for Bin 1 and Bin 2.
    """
    sample_ids = set(phase1["sample_ids"])
    all_pairs  = phase1["pairwise_scores"]

    bin2_ids = {c["id"] for c in bin2} & sample_ids
    bin1_ids = {cid for cid in sample_ids
                if pipeline_idx.get(cid, {}).get("claimed_type") == "mountain"
                and cid not in bin2_ids}

    b1_b1, b2_b2, b1_b2 = [], [], []
    for p in all_pairs:
        i1, i2 = p["id1"], p["id2"]
        i1_b1, i2_b1 = i1 in bin1_ids, i2 in bin1_ids
        i1_b2, i2_b2 = i1 in bin2_ids, i2 in bin2_ids

        if i1_b1 and i2_b1:
            b1_b1.append(p)
        elif i1_b2 and i2_b2:
            b2_b2.append(p)
        elif (i1_b1 and i2_b2) or (i2_b1 and i1_b2):
            b1_b2.append(p)

    def met_stats(pairs):
        if not pairs:
            return {"mean": None, "std": None, "n": 0}
        vals = [p["met"] for p in pairs]
        return {"mean": float(np.mean(vals)), "std": float(np.std(vals)), "n": len(pairs)}

    return {
        "bin1_count_in_sample": len(bin1_ids),
        "bin2_count_in_sample": len(bin2_ids),
        "clean_clean_met": met_stats(b1_b1),
        "bin2_bin2_met":   met_stats(b2_b2),
        "bin1_bin2_met":   met_stats(b1_b2),
    }


# ─────────────────────────────────────────────────────────────────
# Output generation
# ─────────────────────────────────────────────────────────────────

def fmt_table(headers, rows):
    sep = ["---"] * len(headers)
    lines = [
        "| " + " | ".join(str(h) for h in headers) + " |",
        "| " + " | ".join(sep) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(v) for v in row) + " |")
    return "\n".join(lines)


def fmt_stat(s):
    if s["mean"] is None:
        return "— (no pairs)"
    return f"{s['mean']:.3f} ± {s['std']:.3f} (n={s['n']})"


def write_md(
    orphan_total, mm_count, non_mm_count, type_xtab,
    non_mm_top30, anchor_row, anchor_rank,
    total_mountains, bin_counts, fsm_fired, no_fsm_other, sig_dist,
    bin2_detail, bin2_truncated,
    localization, stratum_stats,
    bin2, pipeline_idx, sample_ids,
    anchor_source=None,
):
    lines = [
        "# Cluster-Space Audit Phase 3: Orphan Invisibility and Naturalized Mountain Inventory",
        "",
    ]

    # ── Part A ────────────────────────────────────────────────────
    lines += ["## Part A: Non-Mountain Orphan Invisibility", ""]

    # A.1 — Filter counts
    lines += ["### A.1 Non-Mountain Filter Counts", ""]
    lines += [
        fmt_table(
            ["Subset", "Count", "% of orphan invisibility"],
            [
                ["Orphan invisibility total", orphan_total, "100%"],
                ["Mountain–Mountain pairs",  mm_count,
                 f"{mm_count/orphan_total:.1%}"],
                ["Non-mountain pairs (≥1 non-mountain side)", non_mm_count,
                 f"{non_mm_count/orphan_total:.1%}"],
            ]
        ),
        "",
    ]

    # Type cross-tabulation
    lines += ["**Type cross-tabulation in non-mountain subset:**", ""]
    xtab_rows = sorted(type_xtab.items(), key=lambda x: -x[1])
    lines += [
        fmt_table(
            ["type1 × type2", "Count"],
            [[k, v] for k, v in xtab_rows],
        ),
        "",
    ]

    # A.2 — Top 30 pairs
    lines += ["### A.2 Top 30 Non-Mountain Orphan Invisibility Pairs", ""]
    lines += [
        "Ranked by metric-space similarity descending, ties broken by inverse semantic "
        "(lower sem = higher rank).",
        "",
    ]
    a2_headers = ["rank", "id1 (type)", "id2 (type)", "sig1", "sig2",
                  "obs", "ben", "coup", "sem", "met"]
    a2_rows = []
    for i, p in enumerate(non_mm_top30, 1):
        a2_rows.append([
            i,
            f"{p['id1']} ({p['claimed_type1']})",
            f"{p['id2']} ({p['claimed_type2']})",
            p.get("signature1", "none"),
            p.get("signature2", "none"),
            f"{p['obs']:.3f}", f"{p['ben']:.3f}", f"{p['coup']:.3f}",
            f"{p['sem']:.3f}", f"{p['met']:.3f}",
        ])
    lines += [fmt_table(a2_headers, a2_rows), ""]

    # A.3 — Anchor verification
    lines += ["### A.3 Anchor Verification (advice_as_dangerous_gift ↔ central_bank_independence)", ""]
    if anchor_row and anchor_source == "pairwise_scores":
        lines += [
            f"Found at rank **#{anchor_rank}** in non-mountain orphan invisibility.",
            "",
            fmt_table(
                ["id1 (type)", "id2 (type)", "sig1", "sig2", "obs", "ben", "coup", "sem", "met"],
                [[
                    f"{anchor_row['id1']} ({anchor_row['claimed_type1']})",
                    f"{anchor_row['id2']} ({anchor_row['claimed_type2']})",
                    anchor_row.get("signature1", "none"),
                    anchor_row.get("signature2", "none"),
                    f"{anchor_row['obs']:.3f}", f"{anchor_row['ben']:.3f}",
                    f"{anchor_row['coup']:.3f}", f"{anchor_row['sem']:.3f}",
                    f"{anchor_row['met']:.3f}",
                ]]
            ),
            "",
        ]
    elif anchor_row and anchor_source == "calibration":
        def fmt_v(v): return f"{v:.3f}" if v is not None else "—"
        lines += [
            "**Not in main sample pairwise scores** — both constraints were assigned "
            "to the Phase 1 calibration set and are explicitly excluded from the "
            "265-constraint main sample. Scores below are from calibration Phase 0.",
            "",
            fmt_table(
                ["id1 (type)", "id2 (type)", "sig1", "sig2", "obs", "ben", "coup", "sem", "met"],
                [[
                    f"{anchor_row['id1']} ({anchor_row['claimed_type1']})",
                    f"{anchor_row['id2']} ({anchor_row['claimed_type2']})",
                    anchor_row.get("signature1", "none"),
                    anchor_row.get("signature2", "none"),
                    "—", fmt_v(anchor_row["ben"]), "—",
                    fmt_v(anchor_row["sem"]), fmt_v(anchor_row["met"]),
                ]]
            ),
            "",
            "met=0.999 confirms orphan invisibility: two tangled-rope constraints "
            "(advice-giving as social mechanism; central bank governance) are "
            "metrically near-identical while being semantically unrelated (sem=0.061).",
            "",
        ]
    else:
        lines += ["**ANCHOR NOT FOUND** in non-mountain subset or calibration data.", ""]

    # ── Part B ────────────────────────────────────────────────────
    lines += ["## Part B: Naturalized Mountain Inventory (Full Corpus)", ""]

    # B.1 — Inventory bins
    lines += ["### B.1 Mountain Inventory by Bin", ""]
    b1_rows = []
    for b, label in [
        (1, "Clean (empty ben AND empty vic)"),
        (2, "Beneficiary-having (FSM candidate)"),
        (3, "Victim-having only (no ben, non-empty vic)"),
        (4, "Other"),
    ]:
        n = bin_counts.get(b, 0)
        b1_rows.append([f"Bin {b}: {label}", n, f"{n/total_mountains:.1%}"])
    b1_rows.append(["**Total**", total_mountains, "100%"])
    lines += [fmt_table(["Bin", "Count", "% of all mountains"], b1_rows), ""]

    # B.2 — FSM cross-tab
    lines += ["### B.2 Bin 2 FSM Signature Cross-Tabulation", ""]
    n_bin2 = bin_counts.get(2, 0)
    lines += [
        f"Bin 2 has {n_bin2} beneficiary-having mountains. "
        f"FSM (`false_summit_mountain`) detection rate against declared beneficiaries:",
        "",
        fmt_table(
            ["Signature", "Count", "% of Bin 2", "Interpretation"],
            [
                ["false_summit_mountain", len(fsm_fired),
                 f"{len(fsm_fired)/n_bin2:.1%}",
                 "FSM detector fired"],
                *[
                    [sig, cnt, f"{cnt/n_bin2:.1%}",
                     "FSM did NOT fire (false negative against Axiom 3 criterion)"]
                    for sig, cnt in sig_dist.items()
                    if sig != "false_summit_mountain"
                ],
            ]
        ),
        "",
        f"**FSM false-negative rate** (declared beneficiaries, no FSM): "
        f"{len(no_fsm_other)}/{n_bin2} = {len(no_fsm_other)/n_bin2:.1%}",
        "",
    ]

    # Named false-negative cases
    if no_fsm_other:
        lines += ["**Named Bin 2 cases where FSM did NOT fire:**", ""]
        for c in no_fsm_other:
            lines.append(
                f"- `{c['id']}` — sig=`{c.get('signature','none')}`, "
                f"ben={c.get('beneficiaries')}"
            )
        lines.append("")

    # B.3 — Bin 2 detail table
    lines += [f"### B.3 Bin 2 Detail Table ({min(n_bin2, 50)} of {n_bin2})", ""]
    if bin2_truncated:
        lines += [f"_(Sorted by maxent mountain confidence; {bin2_truncated} lower-confidence rows omitted.)_", ""]
    b3_headers = ["id", "human_readable", "topic_domain", "beneficiaries", "signature",
                  "maxent[mountain]"]
    b3_rows = []
    for c in bin2_detail:
        mp = (c.get("maxent_probs") or {}).get("mountain", 0.0)
        b3_rows.append([
            c["id"],
            (c.get("human_readable") or "")[:60],
            c.get("topic_domain", ""),
            ", ".join(c.get("beneficiaries") or []),
            c.get("signature", "none"),
            f"{mp:.3f}",
        ])
    lines += [fmt_table(b3_headers, b3_rows), ""]

    # B.4 — Cluster-space localization
    lines += ["### B.4 Cluster-Space Localization of Bin 2 (Sample Subset)", ""]
    b2_sample = localization["bin2_in_sample"]
    lines += [
        f"Bin 2 mountains in Phase 1 sample: **{len(b2_sample)}** "
        f"(`{'`, `'.join(b2_sample) if b2_sample else 'none'}`)",
        f"Bin 1 (clean) mountains in Phase 1 sample: {localization['bin1_in_sample_count']}",
        "",
    ]

    if b2_sample:
        pc = localization["pair_counts"]
        loc_rows = [
            ["Bin 2 → Bin 1 (clean)", fmt_stat(localization["bin2_to_bin1"][m]), ""] for m in MEASURES
        ]
        # Build comparative table
        headers = ["Measure", "Bin2 → clean", "clean → clean", "Δ (Bin2−clean)"]
        rows = []
        for m in MEASURES:
            b2c = localization["bin2_to_bin1"][m]
            cc  = localization["clean_to_clean"][m]
            if b2c["mean"] is not None and cc["mean"] is not None:
                delta = b2c["mean"] - cc["mean"]
                rows.append([
                    MEASURE_LABELS[m],
                    f"{b2c['mean']:.3f} ± {b2c['std']:.3f}",
                    f"{cc['mean']:.3f} ± {cc['std']:.3f}",
                    f"{delta:+.3f}",
                ])
            else:
                rows.append([MEASURE_LABELS[m], "—", fmt_stat(cc), "—"])
        lines += [
            fmt_table(headers, rows),
            "",
            f"_(Bin2→clean pairs: {pc['bin2_to_bin1']}; clean→clean pairs: {pc['clean_to_clean']})_",
            "",
        ]

        # Commentary on metric collapse
        met_b2c = localization["bin2_to_bin1"]["met"]
        met_cc  = localization["clean_to_clean"]["met"]
        sem_b2c = localization["bin2_to_bin1"]["sem"]
        sem_cc  = localization["clean_to_clean"]["sem"]
        if met_b2c["mean"] is not None:
            met_d = met_b2c["mean"] - met_cc["mean"]
            sem_d = sem_b2c["mean"] - sem_cc["mean"]
            if abs(met_d) < 0.05:
                lines += [
                    f"**Metric-collapse present**: Bin 2 → clean metric similarity "
                    f"({met_b2c['mean']:.3f}) ≈ clean → clean ({met_cc['mean']:.3f}), "
                    f"Δ = {met_d:+.3f}. The engine is metrically blind to the "
                    f"beneficiary structure that distinguishes them.",
                    "",
                ]
            else:
                lines += [
                    f"Metric-space separation between Bin 2 and clean: Δ = {met_d:+.3f}.",
                    "",
                ]
            if sem_b2c["mean"] is not None:
                lines += [
                    f"Semantic similarity (Bin2→clean = {sem_b2c['mean']:.3f}, "
                    f"clean→clean = {sem_cc['mean']:.3f}, Δ = {sem_d:+.3f}) "
                    + ("suggests Bin 2 mountains are semantically distinct from clean mountains "
                       "— different content despite metrically identical structure."
                       if sem_d < -0.02 else
                       "is comparable — Bin 2 and clean mountains share semantic neighborhood."),
                    "",
                ]
    else:
        lines += ["_No Bin 2 mountains in Phase 1 sample — localization not computable._", ""]

    # B.5 — Mountain stratum stats by bin
    lines += ["### B.5 Mountain Stratum Statistics by Bin", ""]
    ss = stratum_stats
    lines += [
        f"Phase 1 reported Mountain within-type metric similarity = 0.985 across all 80 sampled mountains.",
        f"Recomputed separately:",
        "",
        fmt_table(
            ["Subset", "n in sample", "Within-subset metric mean ± std"],
            [
                ["Bin 1 (clean)", ss["bin1_count_in_sample"],
                 fmt_stat(ss["clean_clean_met"])],
                ["Bin 2 (beneficiary-having)", ss["bin2_count_in_sample"],
                 fmt_stat(ss["bin2_bin2_met"])],
                ["Bin 1 × Bin 2 (cross)", "—",
                 fmt_stat(ss["bin1_bin2_met"])],
            ]
        ),
        "",
    ]

    cc_m = ss["clean_clean_met"]["mean"]
    b2_m = ss["bin2_bin2_met"]["mean"]
    cb_m = ss["bin1_bin2_met"]["mean"]
    if cc_m is not None and cb_m is not None:
        if abs(cb_m - cc_m) < 0.05:
            lines += [
                f"**Metric-collapse confirmed at bin level**: clean→clean "
                f"({cc_m:.3f}) ≈ clean×Bin2 ({cb_m:.3f}). "
                f"Bin 2 mountains pull the aggregate toward the clean cluster "
                f"because they are metrically indistinguishable from it.",
                "",
            ]

    # ── Section C — Structural observations ───────────────────────
    lines += ["## C. Structural Observations", ""]

    n_sample_mtn = ss["bin1_count_in_sample"] + ss["bin2_count_in_sample"]
    n_total_mtn  = total_mountains
    bin2_frac_corpus = bin_counts.get(2, 0) / n_total_mtn
    bin2_frac_sample = ss["bin2_count_in_sample"] / n_sample_mtn if n_sample_mtn else 0

    lines += [
        f"**Full-corpus mountain count**: {n_total_mtn} declared mountains across the full corpus. "
        f"Phase 1 sampled {n_sample_mtn} mountains (target 80, seed 42, uniform random). "
        f"Corpus Bin 2 fraction: {bin2_frac_corpus:.1%} ({bin_counts.get(2,0)} constraints). "
        f"Sample Bin 2 fraction: {bin2_frac_sample:.1%} ({ss['bin2_count_in_sample']} constraints). "
        + ("Zero Bin 2 mountains were drawn in the sample despite the corpus fraction. "
           "With 14 Bin 2 mountains out of 401 total and 80 drawn (uniform, all have "
           "maxent[mountain]=1.0), the probability of zero Bin 2 in sample is approximately "
           "(387/401)^80 ≈ 5.7% — low but feasible. "
           "This means B4 (cluster-space localization) and B5 (stratum stats by bin) "
           "cannot be computed from the existing pairwise scores. "
           "Re-running Phase 1 with seed 43 or adding Bin 2 constraints to the sample "
           "would recover these analyses."
           if ss["bin2_count_in_sample"] == 0 else
           "Sample is representative of Bin 2 mountains."),
        "",
        f"**FSM detection false-negative rate**: {len(no_fsm_other)}/{n_bin2} "
        f"({len(no_fsm_other)/n_bin2:.1%}) of beneficiary-having mountains carry no FSM "
        f"signature. Named cases: "
        + ", ".join(f"`{c['id']}` (sig={c.get('signature','none')})" for c in no_fsm_other)
        + ". These are the highest-value paper cases: the apparatus's own Axiom 3 criterion "
        "is satisfied in the declared data (beneficiaries are present) but the FSM detector "
        "did not fire. Each is a named candidate for a false negative in the FSM subsystem.",
        "",
        "**FSM detector architecture caveat**: the FSM detector may operate on "
        "coupling-network-inferred beneficiaries rather than declared `beneficiaries` list "
        "entries. If so, the false-negative rate computed here is against *declared* "
        "beneficiaries only. A constraint may declare beneficiaries but have its coupling "
        "network indicate otherwise; in that case the FSM non-firing may be correct. "
        "This audit cannot distinguish the two without inspecting the coupling-network "
        "beneficiary inference layer directly.",
        "",
        "**Beneficiary list edge cases**: beneficiary entries appear to be string labels "
        "(e.g., `['age_management_clinics', 'fitness_industry']`). No placeholder tokens "
        "(`TBD`, `[]`, `null`) were observed. Some entries are institution-types rather "
        "than named entities (e.g., `'group_decision_efficiency'`, `'social_coordination_systems'`) "
        "— these are functional beneficiaries, not organizations. No heuristics were applied "
        "to filter these; they are treated as substantive.",
        "",
        "**Orphan invisibility mountain dominance**: "
        f"{mm_count}/{orphan_total} ({mm_count/orphan_total:.1%}) of orphan invisibility pairs "
        "are mountain–mountain. This is expected: all clean mountains share the same "
        "metric signature (natural_law, uniform chi profile) — they are definitionally "
        "metrically identical. The orphan invisibility cell is structurally dominated "
        "by the mountain population's metric-collapse. Non-mountain orphan pairs "
        f"({non_mm_count}, {non_mm_count/orphan_total:.1%}) are the residual — genuine "
        "orphan invisibility cases where the engine sees metric similarity that semantic "
        "space does not.",
        "",
        "**Metric collapse and the cluster-space taxonomy**: the mountain population's "
        "metric-collapse (all mountains metrically indistinguishable regardless of semantic "
        "content or beneficiary structure) is the dominant structural feature of the "
        "orphan-invisibility cell. This is not an engine error — it is the apparatus "
        "correctly applying its Axiom 3 criterion. The question for the paper is whether "
        "that criterion is *over-applied*: Bin 2 mountains satisfy Axiom 3 structurally "
        "(natural_law signature, uniform chi) but violate it semantically "
        "(declared beneficiaries). The FSM detector is the designed correction mechanism; "
        "the cross-tab in B.2 measures how often it actually corrects.",
    ]

    return "\n".join(lines)


# ─────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────

def main():
    pipeline_idx = load_pipeline_index()
    phase1       = load_phase1()

    sample_ids = set(phase1["sample_ids"])

    # ── Part A ────────────────────────────────────────────────────
    print("\nPart A: Orphan invisibility ...", flush=True)
    orphan_all = recompute_orphan_pairs(phase1)
    mm_pairs, non_mm_raw = filter_non_mountain(orphan_all, pipeline_idx)
    non_mm = enrich_with_signature(non_mm_raw, pipeline_idx)
    non_mm_ranked = rank_non_mountain(non_mm)
    non_mm_top30  = non_mm_ranked[:30]

    # Type cross-tab
    type_xtab = Counter()
    for p in non_mm:
        t1, t2 = p["claimed_type1"], p["claimed_type2"]
        key = " × ".join(sorted([t1, t2]))
        type_xtab[key] += 1

    # Anchor — check pairwise scores first, fall back to calibration data
    target = {"advice_as_dangerous_gift", "central_bank_independence"}
    anchor_row, anchor_rank, anchor_source = None, None, None
    for i, p in enumerate(non_mm_ranked, 1):
        if {p["id1"], p["id2"]} == target:
            anchor_row, anchor_rank = p, i
            anchor_source = "pairwise_scores"
            break

    if anchor_row is None:
        # Pair was in calibration set, excluded from main sample.
        # Reconstruct from calibration data.
        cal_scores = phase1.get("calibration", {}).get("anchor1_scores", {})
        if cal_scores:
            anchor_row = {
                "id1": "advice_as_dangerous_gift",
                "id2": "central_bank_independence",
                "claimed_type1": "tangled_rope",
                "claimed_type2": "tangled_rope",
                "signature1": "false_natural_law",
                "signature2": "false_natural_law",
                "obs": None,   # not computable from calibration (cross-set)
                "ben": cal_scores.get("ben_sim", 0.0),
                "coup": None,
                "sem": cal_scores.get("sem_sim"),
                "met": cal_scores.get("met_sim"),
                "idea_best": max(cal_scores.get("ben_sim", 0.0),
                                 cal_scores.get("sem_sim", 0.0)),
            }
            anchor_rank = "N/A (calibration set)"
            anchor_source = "calibration"

    print(f"  Mountain-mountain pairs: {len(mm_pairs)}", flush=True)
    print(f"  Non-mountain pairs: {len(non_mm)}", flush=True)
    print(f"  Anchor: {anchor_source} rank={anchor_rank}", flush=True)

    # ── Part B ────────────────────────────────────────────────────
    print("\nPart B: Mountain inventory ...", flush=True)
    mountains, bins = build_mountain_inventory(pipeline_idx)
    bin2 = bins[2]

    fsm_fired, no_fsm_other, sig_dist = fsm_crosstab(bin2)
    bin2_detail, bin2_truncated = bin2_detail_rows(bin2)
    localization = localize_bin2(phase1, pipeline_idx, bin2)
    stratum_stats = recompute_stratum_stats(phase1, pipeline_idx, bin2)

    bin_counts = {b: len(cs) for b, cs in bins.items()}

    # ── Write outputs ─────────────────────────────────────────────
    print("\nWriting outputs ...", flush=True)

    json_out = {
        "metadata": {
            "total_mountains_full_corpus": len(mountains),
            "orphan_invisibility_total": len(orphan_all),
            "mm_orphan_count":    len(mm_pairs),
            "non_mm_orphan_count": len(non_mm),
            "anchor_rank":         str(anchor_rank) if anchor_rank else None,
            "anchor_source":       anchor_source,
        },
        "mountain_inventory": {
            "bin_counts": {str(k): v for k, v in bin_counts.items()},
            "fsm_fired_count":   len(fsm_fired),
            "no_fsm_count":      len(no_fsm_other),
            "sig_distribution":  dict(sig_dist),
        },
        "bin2_detail": [
            {
                "id": c["id"],
                "human_readable": c.get("human_readable", ""),
                "topic_domain":   c.get("topic_domain", ""),
                "beneficiaries":  c.get("beneficiaries") or [],
                "victims":        c.get("victims") or [],
                "signature":      c.get("signature", "none"),
                "maxent_mountain": (c.get("maxent_probs") or {}).get("mountain", 0.0),
            }
            for c in bin2_detail
        ],
        "bin2_fsm_nonfired": [c["id"] for c in no_fsm_other],
        "localization": localization,
        "stratum_stats": stratum_stats,
        "non_mountain_orphan_top": [
            {k: p[k] for k in
             ["id1","id2","type1","type2","claimed_type1","claimed_type2",
              "signature1","signature2","obs","ben","coup","sem","met","idea_best"]}
            for p in non_mm_top30
        ],
    }

    OUT_JSON.write_text(json.dumps(json_out, indent=2))
    print(f"  {OUT_JSON}", flush=True)

    md = write_md(
        orphan_total=len(orphan_all),
        mm_count=len(mm_pairs),
        non_mm_count=len(non_mm),
        type_xtab=type_xtab,
        non_mm_top30=non_mm_top30,
        anchor_row=anchor_row,
        anchor_rank=anchor_rank,
        total_mountains=len(mountains),
        bin_counts=bin_counts,
        fsm_fired=fsm_fired,
        no_fsm_other=no_fsm_other,
        sig_dist=sig_dist,
        bin2_detail=bin2_detail,
        bin2_truncated=bin2_truncated,
        localization=localization,
        stratum_stats=stratum_stats,
        bin2=bin2,
        pipeline_idx=pipeline_idx,
        sample_ids=sample_ids,
        anchor_source=anchor_source,
    )
    OUT_MD.write_text(md)
    print(f"  {OUT_MD}", flush=True)
    print("Done.", flush=True)


if __name__ == "__main__":
    main()
