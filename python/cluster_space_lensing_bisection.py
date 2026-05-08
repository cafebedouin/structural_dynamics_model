#!/usr/bin/env python3
"""
Cluster-Space Audit Phase 2: Lensing Zone Authorship Bisection

Bisects the 2,870 lensing-zone pairs from Phase 1 into:
  - Likely axis-split (same topic_domain prefix — same author, different probe angle)
  - Likely genuine-lensing (different prefix — cross-domain classification disagreement)

Uses cached pairwise scores from cluster_space_audit_phase1.json.
Reads topic_domain from pipeline_output.json.

Outputs:
  outputs/cluster_space_lensing_bisection.md
  outputs/cluster_space_lensing_bisection.json
"""

import json
from pathlib import Path

import numpy as np

ROOT       = Path(__file__).resolve().parent.parent
PHASE1_JSON = ROOT / "outputs" / "cluster_space_audit_phase1.json"
PIPELINE    = ROOT / "outputs" / "pipeline_output.json"
OUT_MD      = ROOT / "outputs" / "cluster_space_lensing_bisection.md"
OUT_JSON    = ROOT / "outputs" / "cluster_space_lensing_bisection.json"

# ─────────────────────────────────────────────────────────────────
# 1. Load
# ─────────────────────────────────────────────────────────────────

def load_topic_domains():
    """Return dict: constraint_id → topic_domain string (or None)."""
    print("Loading pipeline_output.json for topic_domain ...", flush=True)
    raw = json.loads(PIPELINE.read_text())
    td = {}
    for c in raw["per_constraint"]:
        cid = c.get("id") or c.get("constraint_id")
        if cid:
            td[cid] = c.get("topic_domain") or c.get("domain")
    print(f"  {sum(1 for v in td.values() if v)} / {len(td)} constraints have topic_domain",
          flush=True)
    return td


def load_lensing_pairs():
    """Return the full lensing zone pair list from Phase 1 JSON."""
    print("Loading Phase 1 results ...", flush=True)
    d = json.loads(PHASE1_JSON.read_text())
    lensing = d["analysis"]["outliers"]["lensing_zone"]

    # Phase 1 outliers only contain top-20; we need all lensing pairs.
    # Recompute from full pairwise scores using the stored quartile thresholds.
    pop     = d["analysis"]["population_matrix"]
    qt      = pop["quartile_thresholds"]
    idea_qt = pop["idea_quartile_thresholds"]

    obs_q1  = qt["obs"][0]
    idea_q3 = idea_qt[2]        # idea > Q3 means top quartile

    all_pairs = d["pairwise_scores"]
    print(f"  {len(all_pairs)} total pairs", flush=True)

    # Restore idea_best for each pair
    lensing_all = []
    for p in all_pairs:
        idea_best = max(p["ben"], p["coup"], p["sem"])
        if idea_best > idea_q3 and p["obs"] <= obs_q1:
            p = dict(p)
            p["idea_best"] = idea_best
            lensing_all.append(p)

    print(f"  {len(lensing_all)} lensing-zone pairs", flush=True)
    return lensing_all, d


# ─────────────────────────────────────────────────────────────────
# 2. Topic-domain similarity
# ─────────────────────────────────────────────────────────────────

def topic_prefix(domain: str) -> str:
    """Return the string before the first '/', or the full string if none."""
    return domain.split("/")[0] if domain else ""


def topic_match(td1, td2) -> float:
    """
    1.0 — exact match
    0.5 — shared prefix before first '/'
    0.0 — no match (or either missing)
    """
    if not td1 or not td2:
        return 0.0
    if td1 == td2:
        return 1.0
    if topic_prefix(td1) == topic_prefix(td2):
        return 0.5
    return 0.0


# ─────────────────────────────────────────────────────────────────
# 3. Bisection
# ─────────────────────────────────────────────────────────────────

def bisect(lensing_pairs, topic_domains):
    axis_split    = []
    genuine       = []
    missing_count = 0

    for p in lensing_pairs:
        td1 = topic_domains.get(p["id1"])
        td2 = topic_domains.get(p["id2"])
        if not td1 or not td2:
            missing_count += 1
        tm = topic_match(td1, td2)
        p  = dict(p, topic_domain1=td1 or "", topic_domain2=td2 or "",
                  topic_match=tm)
        if tm >= 0.5:
            axis_split.append(p)
        else:
            genuine.append(p)

    print(f"  Axis-split (topic_match >= 0.5): {len(axis_split)}", flush=True)
    print(f"  Genuine-lensing (topic_match == 0.0): {len(genuine)}", flush=True)
    print(f"  Pairs with at least one missing topic_domain: {missing_count}", flush=True)
    return axis_split, genuine, missing_count


# ─────────────────────────────────────────────────────────────────
# 4. Ranking
# ─────────────────────────────────────────────────────────────────

PRIORITY_TYPES = {"mountain", "tangled_rope"}


def rank_pairs(pairs, top_n=20):
    """Sort by priority (mountain/TR on at least one side) then sem descending."""
    def key(p):
        priority = 0 if (p.get("type1") in PRIORITY_TYPES or
                         p.get("type2") in PRIORITY_TYPES) else 1
        return (priority, -p["sem"])
    return sorted(pairs, key=key)[:top_n]


# ─────────────────────────────────────────────────────────────────
# 5. Distributional comparison
# ─────────────────────────────────────────────────────────────────

def dist_stats(pairs, measure):
    vals = [p[measure] for p in pairs]
    if not vals:
        return {"mean": None, "std": None, "n": 0}
    return {
        "mean": float(np.mean(vals)),
        "std":  float(np.std(vals)),
        "n":    len(vals),
    }


def compare_distributions(lensing_all, axis_split, genuine):
    measures = ["obs", "ben", "coup", "sem", "met"]
    result = {}
    for m in measures:
        result[m] = {
            "overall":     dist_stats(lensing_all, m),
            "axis_split":  dist_stats(axis_split, m),
            "genuine":     dist_stats(genuine, m),
        }
    return result


# ─────────────────────────────────────────────────────────────────
# 6. Anchor verification
# ─────────────────────────────────────────────────────────────────

def find_anchor(axis_split, genuine):
    target = {"eu_russian_asset_freeze_2025", "russian_asset_freezing"}
    for p in axis_split:
        if {p["id1"], p["id2"]} == target:
            return p, "axis_split"
    for p in genuine:
        if {p["id1"], p["id2"]} == target:
            return p, "genuine_lensing"
    return None, "not_found"


# ─────────────────────────────────────────────────────────────────
# 7. Output generation
# ─────────────────────────────────────────────────────────────────

MEASURE_LABELS = {
    "obs":  "Observer-space",
    "ben":  "Beneficiary",
    "coup": "Coupling",
    "sem":  "Semantic",
    "met":  "Metric-space",
}


def fmt_table(headers, rows):
    sep = ["---"] * len(headers)
    lines = [
        "| " + " | ".join(str(h) for h in headers) + " |",
        "| " + " | ".join(sep) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(v) for v in row) + " |")
    return "\n".join(lines)


def pair_row(p):
    return [
        f"{p['id1']} ({p.get('type1','?')})",
        f"{p['id2']} ({p.get('type2','?')})",
        p.get("topic_domain1", ""),
        p.get("topic_domain2", ""),
        f"{p['obs']:.3f}",
        f"{p['ben']:.3f}",
        f"{p['coup']:.3f}",
        f"{p['sem']:.3f}",
        f"{p['met']:.3f}",
    ]


PAIR_HEADERS = [
    "id1 (type)", "id2 (type)", "topic_domain1", "topic_domain2",
    "obs", "ben", "coup", "sem", "met",
]


def sensitivity_analysis(lensing_all, topic_domains):
    """
    Strict: exact topic_domain match only.
    Loose: any shared word (tokenised on '/' and '_').
    """
    exact_count = sum(
        1 for p in lensing_all
        if (topic_domains.get(p["id1"]) and topic_domains.get(p["id2"]) and
            topic_domains.get(p["id1"]) == topic_domains.get(p["id2"]))
    )

    def shared_word(td1, td2):
        if not td1 or not td2:
            return False
        w1 = set(td1.replace("/", "_").split("_"))
        w2 = set(td2.replace("/", "_").split("_"))
        return bool(w1 & w2)

    loose_count = sum(
        1 for p in lensing_all
        if shared_word(topic_domains.get(p["id1"]), topic_domains.get(p["id2"]))
    )

    return exact_count, loose_count


def edge_cases(lensing_all, axis_split, genuine, topic_domains):
    """
    Surface a handful of suspicious cases:
    - Same domain but semantically distant (high topic_match, low sem)
    - Different domain but semantically close (low topic_match, high sem)
    """
    # Same domain, lowest sem
    same_low_sem = sorted(
        [p for p in axis_split if p["sem"] < 0.15],
        key=lambda p: p["sem"]
    )[:3]

    # Different domain, highest sem
    diff_high_sem = sorted(
        [p for p in genuine if p["sem"] > 0.3],
        key=lambda p: -p["sem"]
    )[:3]

    return same_low_sem, diff_high_sem


def write_md(counts, axis_top, genuine_top, dist, anchor_pair,
             anchor_location, exact_count, loose_count,
             missing_count, lensing_all, axis_split, genuine,
             same_low_sem, diff_high_sem):

    measures = ["obs", "ben", "coup", "sem", "met"]
    n_total  = counts["total"]
    n_axis   = counts["axis_split"]
    n_genuine = counts["genuine"]

    lines = [
        "# Lensing Zone Authorship Bisection",
        "",
        "Re-ranks the 2,870 Phase 1 lensing-zone pairs (idea Q4 × obs Q1) "
        "using topic_domain prefix as a proxy for authorship intent.",
        "",
    ]

    # ── Section 1: Bisection counts ──────────────────────────────
    lines += ["## 1. Bisection Counts and topic_domain Coverage", ""]
    lines += [
        f"| Subset | Count | Fraction of lensing zone |",
        f"| --- | --- | --- |",
        f"| Lensing zone overall | {n_total} | 100% |",
        f"| Likely axis-split (topic_match ≥ 0.5) | {n_axis} | {n_axis/n_total:.1%} |",
        f"| Likely genuine-lensing (topic_match = 0.0) | {n_genuine} | {n_genuine/n_total:.1%} |",
        f"| Pairs with ≥1 missing topic_domain | {missing_count} | {missing_count/n_total:.1%} |",
        "",
    ]

    exact_share = exact_count / n_total
    lines += [
        f"Of the {n_axis} axis-split pairs, "
        f"{exact_count} ({exact_count/n_axis:.1%} of axis-split, "
        f"{exact_share:.1%} of all lensing) have **exact** topic_domain match; "
        f"{n_axis - exact_count} ({(n_axis - exact_count)/n_axis:.1%}) match on prefix only.",
        "",
    ]

    # ── Section 2: Axis-split top 20 ─────────────────────────────
    lines += ["## 2. Top 20 Likely-Axis-Split Pairs", ""]
    lines += ["Ranked by: Mountain/TR priority, then semantic similarity descending.", ""]
    lines += [fmt_table(PAIR_HEADERS, [pair_row(p) for p in axis_top]), ""]

    # ── Section 3: Genuine-lensing top 20 ────────────────────────
    lines += ["## 3. Top 20 Likely-Genuine-Lensing Pairs", ""]
    lines += ["Ranked by: Mountain/TR priority, then semantic similarity descending.", ""]
    lines += [fmt_table(PAIR_HEADERS, [pair_row(p) for p in genuine_top]), ""]

    # ── Section 4: Distributional comparison ─────────────────────
    lines += ["## 4. Distributional Comparison (5 measures × 3 subsets)", ""]
    headers = ["Measure", "Lensing overall (mean±std)",
               "Axis-split (mean±std)", "Genuine-lensing (mean±std)"]
    rows = []
    for m in measures:
        d = dist[m]
        def fmt(s): return f"{s['mean']:.3f} ± {s['std']:.3f}" if s["mean"] is not None else "—"
        rows.append([MEASURE_LABELS[m], fmt(d["overall"]),
                     fmt(d["axis_split"]), fmt(d["genuine"])])
    lines += [fmt_table(headers, rows), ""]

    # commentary on metric-space difference
    met = dist["met"]
    if met["axis_split"]["mean"] is not None and met["genuine"]["mean"] is not None:
        delta = met["axis_split"]["mean"] - met["genuine"]["mean"]
        lines += [
            f"Metric-space mean: axis-split {met['axis_split']['mean']:.3f} vs "
            f"genuine-lensing {met['genuine']['mean']:.3f} "
            f"(Δ = {delta:+.3f}). "
            + ("Axis-split pairs are metrically closer — consistent with "
               "same-author probes producing variants of the same structural pattern."
               if delta > 0.05 else
               "No strong metric-space separation between subsets." if abs(delta) <= 0.05 else
               "Genuine-lensing pairs are metrically closer — unexpected; "
               "cross-domain pairs sharing idea-space may share structural features."),
            "",
        ]

    sem = dist["sem"]
    if sem["axis_split"]["mean"] is not None and sem["genuine"]["mean"] is not None:
        delta = sem["axis_split"]["mean"] - sem["genuine"]["mean"]
        lines += [
            f"Semantic mean: axis-split {sem['axis_split']['mean']:.3f} vs "
            f"genuine-lensing {sem['genuine']['mean']:.3f} "
            f"(Δ = {delta:+.3f}). "
            + ("Axis-split pairs are semantically more similar — expected if same-author "
               "probes are slight re-framings of the same constraint."
               if delta > 0.05 else
               "Semantic similarity is similar across subsets — topic_domain match "
               "predicts domain proximity but not necessarily semantic proximity."
               if abs(delta) <= 0.05 else
               "Genuine-lensing pairs are semantically more similar — suggests "
               "cross-domain constraints can share more surface content than same-domain variants."),
            "",
        ]

    # ── Section 5: Anchor verification ───────────────────────────
    lines += ["## 5. Anchor Verification (eu_russian / russian_asset_freezing)", ""]
    if anchor_pair and anchor_location != "not_found":
        lines += [
            f"Found in: **{anchor_location.replace('_', ' ')}** subset.",
            "",
            fmt_table(PAIR_HEADERS, [pair_row(anchor_pair)]),
            "",
            f"topic_match = {anchor_pair.get('topic_match', '?'):.1f} "
            f"(topic_domain1=`{anchor_pair.get('topic_domain1', 'N/A')}`, "
            f"topic_domain2=`{anchor_pair.get('topic_domain2', 'N/A')}`)",
            "",
        ]
    else:
        lines += ["**ANCHOR NOT FOUND** in lensing zone — pair may not satisfy idea Q4 × obs Q1.", ""]

    # ── Section 6: Structural observations ───────────────────────
    lines += ["## 6. Structural Observations", ""]
    lines += [
        f"**topic_domain coverage**: {counts['has_topic_domain']} of "
        f"{counts['sample_size']} constraints in the Phase 1 sample have "
        f"topic_domain populated ({counts['has_topic_domain']/counts['sample_size']:.1%}). "
        f"{missing_count} lensing pairs had at least one side missing.",
        "",
        f"**Sensitivity to topic_match definition**: under strict (exact-only) matching, "
        f"{exact_count} pairs would be in the axis-split subset ({exact_count/n_total:.1%} of lensing). "
        f"Under loose (any shared word) matching, {loose_count} pairs qualify "
        f"({loose_count/n_total:.1%}). Current rule (prefix) gives {n_axis} "
        f"({n_axis/n_total:.1%}). The spread ({loose_count - exact_count} pairs) "
        f"reflects the ambiguity in multi-word hierarchical domain strings.",
        "",
        "**Edge cases — same domain, semantically distant** "
        "(topic_match ≥ 0.5 but sem < 0.15 — possible false axis-split classification):",
    ]
    if same_low_sem:
        for p in same_low_sem:
            lines.append(
                f"  - `{p['id1']}` ({p.get('topic_domain1','?')}) ↔ "
                f"`{p['id2']}` ({p.get('topic_domain2','?')}): "
                f"sem={p['sem']:.3f}, met={p['met']:.3f}"
            )
    else:
        lines.append("  - None found (all axis-split pairs have sem ≥ 0.15).")

    lines += [
        "",
        "**Edge cases — different domain, semantically close** "
        "(topic_match = 0.0 but sem > 0.30 — possible false genuine-lensing classification):",
    ]
    if diff_high_sem:
        for p in diff_high_sem:
            lines.append(
                f"  - `{p['id1']}` ({p.get('topic_domain1','?')}) ↔ "
                f"`{p['id2']}` ({p.get('topic_domain2','?')}): "
                f"sem={p['sem']:.3f}, met={p['met']:.3f}"
            )
    else:
        lines.append("  - None found (no genuine-lensing pairs have sem > 0.30).")

    lines += [
        "",
        "**Proxy limitation**: topic_domain is a coarse structural proxy for authorship "
        "intent. Same-author cross-domain probes (deliberately spanning domains) and "
        "different-author same-domain coincidences (independently filing similar constraints) "
        "will both be mis-bisected. The proxy identifies structural domain-proximity, "
        "not behavioral authorship. The bisection is a working hypothesis, not a "
        "behavioral ground truth.",
        "",
        "**Coupling and beneficiary sparsity**: both measures are near-zero across all "
        "three subsets. The lensing zone is defined by high semantic similarity, which "
        "is uncorrelated with coupling (ρ≈0.01) and beneficiary (ρ≈0.02) from Phase 1. "
        "No subset separation is expected or observed on these measures.",
    ]

    return "\n".join(lines)


# ─────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────

def main():
    topic_domains = load_topic_domains()
    lensing_all, phase1 = load_lensing_pairs()

    # Coverage in the sample
    sample_ids = phase1["sample_ids"]
    has_td = sum(1 for cid in sample_ids if topic_domains.get(cid))

    print("Bisecting ...", flush=True)
    axis_split, genuine, missing_count = bisect(lensing_all, topic_domains)

    axis_top    = rank_pairs(axis_split)
    genuine_top = rank_pairs(genuine)

    dist = compare_distributions(lensing_all, axis_split, genuine)

    anchor_pair, anchor_location = find_anchor(axis_split, genuine)
    print(f"Anchor: {anchor_location}", flush=True)

    exact_count, loose_count = sensitivity_analysis(lensing_all, topic_domains)
    same_low_sem, diff_high_sem = edge_cases(lensing_all, axis_split, genuine, topic_domains)

    counts = {
        "total":           len(lensing_all),
        "axis_split":      len(axis_split),
        "genuine":         len(genuine),
        "missing_td_pairs": missing_count,
        "sample_size":     len(sample_ids),
        "has_topic_domain": has_td,
    }

    # Write JSON
    json_out = {
        "metadata": {
            "source": "cluster_space_audit_phase1.json",
            "lensing_zone_total": len(lensing_all),
            "axis_split_count":   len(axis_split),
            "genuine_count":      len(genuine),
            "exact_match_count":  exact_count,
            "loose_match_count":  loose_count,
            "missing_td_pairs":   missing_count,
            "anchor_location":    anchor_location,
        },
        "axis_split_pairs": [
            {k: p[k] for k in
             ["id1","id2","type1","type2","topic_domain1","topic_domain2",
              "topic_match","obs","ben","coup","sem","met","idea_best"]}
            for p in axis_split
        ],
        "genuine_lensing_pairs": [
            {k: p[k] for k in
             ["id1","id2","type1","type2","topic_domain1","topic_domain2",
              "topic_match","obs","ben","coup","sem","met","idea_best"]}
            for p in genuine
        ],
        "distributions": dist,
        "anchor": anchor_pair,
    }
    OUT_JSON.write_text(json.dumps(json_out, indent=2))
    print(f"Wrote {OUT_JSON}", flush=True)

    md = write_md(counts, axis_top, genuine_top, dist,
                  anchor_pair, anchor_location,
                  exact_count, loose_count, missing_count,
                  lensing_all, axis_split, genuine,
                  same_low_sem, diff_high_sem)
    OUT_MD.write_text(md)
    print(f"Wrote {OUT_MD}", flush=True)
    print("Done.", flush=True)


if __name__ == "__main__":
    main()
