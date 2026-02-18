#!/usr/bin/env python3
"""Classification Confidence Analysis (v1.0)

Full-corpus confidence analysis comparing MaxEnt shadow classifier distributions
with deterministic type assignments. Includes override vs raw comparison to reveal
which constraints are "held in place" by signature overrides.

Imports MaxEnt functions from tangled_decomposition.py rather than duplicating.

Outputs:
  outputs/classification_confidence_data.json  - per-constraint confidence data
  outputs/classification_confidence_report.md  - analysis report

Usage:
  python3 python/classification_confidence.py
"""

import json
import sys
from collections import Counter, defaultdict
from pathlib import Path

from shared.loader import load_json, load_all_data, PIPELINE_JSON, OUTPUT_DIR
from shared.constants import MAXENT_TYPES, N_TYPES, shannon_entropy
from tangled_decomposition import maxent_classify, apply_signature_override

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

CONFIDENCE_REPORT = OUTPUT_DIR / "classification_confidence_report.md"


def compute_confidence_metrics(cid, c, dist):
    """Compute confidence metrics for a single constraint.

    Returns dict with: confidence, rival_type, rival_prob, margin, entropy, band, boundary.
    """
    claimed = c.get("claimed_type")
    if not claimed or not dist:
        return None

    confidence = dist.get(claimed, 0.0)

    # Find rival: argmax excluding claimed_type
    rival_type = None
    rival_prob = -1.0
    for t, p in dist.items():
        if t != claimed and p > rival_prob:
            rival_type = t
            rival_prob = p

    margin = confidence - rival_prob if rival_prob >= 0 else confidence
    entropy = shannon_entropy(dist)

    # Band classification
    if confidence > 0.8 and margin > 0.5:
        band = "deep"
    elif confidence >= 0.5:
        band = "moderate"
    else:
        band = "borderline"

    boundary = (claimed, rival_type) if rival_type else None

    return {
        "id": cid,
        "claimed_type": claimed,
        "confidence": round(confidence, 6),
        "rival_type": rival_type,
        "rival_prob": round(rival_prob, 6),
        "margin": round(margin, 6),
        "entropy": round(entropy, 6),
        "band": band,
        "boundary": f"{claimed}->{rival_type}" if rival_type else None,
        "signature": c.get("signature"),
        "purity_score": c.get("purity_score"),
        "purity_band": c.get("purity_band"),
        "domain": c.get("domain"),
        "human_readable": c.get("human_readable"),
    }

# ---------------------------------------------------------------------------
# Override vs raw comparison (Section 3.5)
# ---------------------------------------------------------------------------

def compute_override_comparison(cid, c, dist_override, dist_raw):
    """Compare with-override and without-override distributions."""
    claimed = c.get("claimed_type")
    if not claimed:
        return None

    conf_override = dist_override.get(claimed, 0.0)
    conf_raw = dist_raw.get(claimed, 0.0)

    # Band for each
    def band_for(conf, margin):
        if conf > 0.8 and margin > 0.5:
            return "deep"
        elif conf >= 0.5:
            return "moderate"
        return "borderline"

    # Override band
    rival_override = max((p for t, p in dist_override.items() if t != claimed), default=0.0)
    margin_override = conf_override - rival_override
    band_o = band_for(conf_override, margin_override)

    # Raw band
    rival_raw = max((p for t, p in dist_raw.items() if t != claimed), default=0.0)
    margin_raw = conf_raw - rival_raw
    band_r = band_for(conf_raw, margin_raw)

    # Top type for each
    top_override = max(dist_override, key=dist_override.get) if dist_override else None
    top_raw = max(dist_raw, key=dist_raw.get) if dist_raw else None

    return {
        "confidence_override": round(conf_override, 6),
        "confidence_raw": round(conf_raw, 6),
        "confidence_delta": round(conf_override - conf_raw, 6),
        "band_override": band_o,
        "band_raw": band_r,
        "band_changed": band_o != band_r,
        "top_type_override": top_override,
        "top_type_raw": top_raw,
        "top_type_changed": top_override != top_raw,
        "margin_override": round(margin_override, 6),
        "margin_raw": round(margin_raw, 6),
    }

# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def make_ascii_histogram(values, bins=20, width=50, lo=0.0, hi=1.0):
    """Create ASCII histogram."""
    if not values:
        return "  (no data)\n"
    bin_width = (hi - lo) / bins
    counts = [0] * bins
    for v in values:
        idx = min(int((v - lo) / bin_width), bins - 1)
        counts[idx] += 1
    max_count = max(counts) if counts else 1
    lines = []
    for i, cnt in enumerate(counts):
        lo_edge = lo + i * bin_width
        hi_edge = lo_edge + bin_width
        bar_len = int(cnt / max_count * width) if max_count > 0 else 0
        bar = "#" * bar_len
        lines.append(f"  {lo_edge:5.2f}-{hi_edge:5.2f} | {bar} ({cnt})")
    return "\n".join(lines)


def generate_report(all_metrics, override_data, constraints, dists_override, dists_raw):
    """Generate the full classification confidence report."""
    lines = []
    lines.append("# Classification Confidence Analysis Report")
    lines.append("")
    lines.append("*Generated by `python/classification_confidence.py` — full-corpus confidence")
    lines.append("analysis comparing MaxEnt shadow classifier with deterministic assignments.*")
    lines.append("")

    total = len(all_metrics)

    # --- Section 1: Corpus-wide summary ---
    lines.append("## 1. Corpus-Wide Confidence Summary")
    lines.append("")

    band_counts = Counter(m["band"] for m in all_metrics)
    lines.append(f"Total constraints analyzed: **{total}**")
    lines.append("")
    lines.append("| Band | Count | % |")
    lines.append("|------|------:|----:|")
    for band in ["deep", "moderate", "borderline"]:
        cnt = band_counts.get(band, 0)
        pct = 100 * cnt / max(total, 1)
        lines.append(f"| {band} | {cnt} | {pct:.1f}% |")
    lines.append("")

    confidences = [m["confidence"] for m in all_metrics]
    if confidences:
        mean_conf = sum(confidences) / len(confidences)
        sorted_conf = sorted(confidences)
        median_conf = sorted_conf[len(sorted_conf) // 2]
        lines.append("| Statistic | Value |")
        lines.append("|-----------|------:|")
        lines.append(f"| Mean confidence | {mean_conf:.4f} |")
        lines.append(f"| Median confidence | {median_conf:.4f} |")
        lines.append(f"| Min confidence | {min(confidences):.4f} |")
        lines.append(f"| Max confidence | {max(confidences):.4f} |")
        lines.append("")

    # --- Section 2: Per-type sections ---
    lines.append("## 2. Per-Type Confidence Analysis")
    lines.append("")

    for typ in MAXENT_TYPES:
        type_metrics = [m for m in all_metrics if m["claimed_type"] == typ]
        if not type_metrics:
            continue

        lines.append(f"### {typ} ({len(type_metrics)} constraints)")
        lines.append("")

        type_band_counts = Counter(m["band"] for m in type_metrics)
        lines.append("| Band | Count | % |")
        lines.append("|------|------:|----:|")
        for band in ["deep", "moderate", "borderline"]:
            cnt = type_band_counts.get(band, 0)
            pct = 100 * cnt / max(len(type_metrics), 1)
            lines.append(f"| {band} | {cnt} | {pct:.1f}% |")
        lines.append("")

        # Rival type distribution
        rival_counts = Counter(m["rival_type"] for m in type_metrics if m["rival_type"])
        if rival_counts:
            lines.append("**Top rival types:**")
            lines.append("")
            lines.append("| Rival | Count | % |")
            lines.append("|-------|------:|----:|")
            for rival, cnt in rival_counts.most_common(5):
                pct = 100 * cnt / len(type_metrics)
                lines.append(f"| {rival} | {cnt} | {pct:.1f}% |")
            lines.append("")

        # Borderline cases
        borderline = [m for m in type_metrics if m["band"] == "borderline"]
        if borderline:
            lines.append(f"**Borderline cases ({len(borderline)}):**")
            lines.append("")
            lines.append("| Constraint | Confidence | Rival | Rival Prob | Margin | Entropy |")
            lines.append("|------------|-----------|-------|-----------|--------|---------|")
            for m in sorted(borderline, key=lambda x: x["confidence"])[:15]:
                lines.append(f"| {m['id']} | {m['confidence']:.4f} | {m['rival_type']} | "
                             f"{m['rival_prob']:.4f} | {m['margin']:.4f} | {m['entropy']:.4f} |")
            if len(borderline) > 15:
                lines.append(f"| ... | ({len(borderline) - 15} more) | | | | |")
            lines.append("")

    # --- Section 3: Boundary map ---
    lines.append("## 3. Boundary Population Map")
    lines.append("")
    lines.append("Boundaries with > 5 constraints (claimed_type -> rival_type):")
    lines.append("")

    boundary_groups = defaultdict(list)
    for m in all_metrics:
        if m["boundary"]:
            boundary_groups[m["boundary"]].append(m)

    big_boundaries = [(bnd, members) for bnd, members in boundary_groups.items() if len(members) > 5]
    big_boundaries.sort(key=lambda x: -len(x[1]))

    if big_boundaries:
        lines.append("| Boundary | Count | Mean Confidence | Mean Margin | Mean Entropy |")
        lines.append("|----------|------:|----------------:|------------:|-------------:|")
        for bnd, members in big_boundaries:
            cnt = len(members)
            mc = sum(m["confidence"] for m in members) / cnt
            mm = sum(m["margin"] for m in members) / cnt
            me = sum(m["entropy"] for m in members) / cnt
            lines.append(f"| {bnd} | {cnt} | {mc:.4f} | {mm:.4f} | {me:.4f} |")
        lines.append("")

        # Detail for top 5 boundaries
        for bnd, members in big_boundaries[:5]:
            lines.append(f"### {bnd} ({len(members)} constraints)")
            lines.append("")
            # Band breakdown
            bb = Counter(m["band"] for m in members)
            lines.append(f"  Bands: deep={bb.get('deep', 0)}, moderate={bb.get('moderate', 0)}, borderline={bb.get('borderline', 0)}")
            lines.append("")
            # Dominant signature
            sig_counts = Counter(m["signature"] for m in members if m["signature"])
            if sig_counts:
                top_sig = sig_counts.most_common(1)[0]
                lines.append(f"  Dominant signature: {top_sig[0]} ({top_sig[1]}/{len(members)})")
                lines.append("")
    else:
        lines.append("(no boundaries with > 5 constraints)")
        lines.append("")

    # --- Section 4: Cross-diagnostic correlations ---
    lines.append("## 4. Cross-Diagnostic Correlations")
    lines.append("")

    # 4a: MaxEnt borderline vs low-purity
    borderline_ids = {m["id"] for m in all_metrics if m["band"] == "borderline"}
    low_purity_ids = {m["id"] for m in all_metrics
                      if m.get("purity_band") in ("degraded", "contaminated")}

    overlap_purity = borderline_ids & low_purity_ids
    lines.append("### Borderline vs Low Purity")
    lines.append("")
    lines.append(f"| Set | Count |")
    lines.append(f"|-----|------:|")
    lines.append(f"| MaxEnt borderline | {len(borderline_ids)} |")
    lines.append(f"| Low purity (degraded/contaminated) | {len(low_purity_ids)} |")
    lines.append(f"| Overlap | {len(overlap_purity)} |")
    if borderline_ids:
        jaccard = len(overlap_purity) / len(borderline_ids | low_purity_ids) if (borderline_ids | low_purity_ids) else 0
        lines.append(f"| Jaccard similarity | {jaccard:.4f} |")
    lines.append("")

    # 4b: Confidence by signature
    lines.append("### Confidence by Structural Signature")
    lines.append("")
    sig_groups = defaultdict(list)
    for m in all_metrics:
        sig = m.get("signature") or "none"
        sig_groups[sig].append(m["confidence"])

    lines.append("| Signature | Count | Mean Conf | Median Conf | % Borderline |")
    lines.append("|-----------|------:|----------:|------------:|-------------:|")
    for sig, confs in sorted(sig_groups.items(), key=lambda x: -len(x[1])):
        cnt = len(confs)
        mean_c = sum(confs) / cnt
        sc = sorted(confs)
        med_c = sc[len(sc) // 2]
        bline = sum(1 for c in confs if c < 0.5) / cnt * 100
        lines.append(f"| {sig} | {cnt} | {mean_c:.4f} | {med_c:.4f} | {bline:.1f}% |")
    lines.append("")

    # --- Section 5: Override impact ---
    lines.append("## 5. Override Impact Analysis")
    lines.append("")

    if override_data:
        band_changes = [d for d in override_data.values() if d and d["band_changed"]]
        type_changes = [d for d in override_data.values() if d and d["top_type_changed"]]

        lines.append(f"| Metric | Count | % of corpus |")
        lines.append(f"|--------|------:|------------:|")
        lines.append(f"| Band changed (override vs raw) | {len(band_changes)} | {100*len(band_changes)/max(total,1):.1f}% |")
        lines.append(f"| Top type changed | {len(type_changes)} | {100*len(type_changes)/max(total,1):.1f}% |")
        lines.append("")

        # Band transition matrix
        if band_changes:
            lines.append("### Band Transitions (override -> raw)")
            lines.append("")
            transitions = Counter((d["band_override"], d["band_raw"]) for d in band_changes)
            lines.append("| From (override) | To (raw) | Count |")
            lines.append("|-----------------|----------|------:|")
            for (b_from, b_to), cnt in transitions.most_common():
                lines.append(f"| {b_from} | {b_to} | {cnt} |")
            lines.append("")

        # Top type changes
        if type_changes:
            lines.append("### Top Type Changes (override -> raw)")
            lines.append("")
            type_trans = Counter((d["top_type_override"], d["top_type_raw"]) for d in type_changes)
            lines.append("| Override Top | Raw Top | Count |")
            lines.append("|-------------|---------|------:|")
            for (t_from, t_to), cnt in type_trans.most_common(10):
                lines.append(f"| {t_from} | {t_to} | {cnt} |")
            lines.append("")

        # Override impact by signature
        lines.append("### Override Impact by Signature")
        lines.append("")
        sig_override = defaultdict(lambda: {"total": 0, "band_changed": 0, "type_changed": 0, "deltas": []})
        for cid, od in override_data.items():
            if not od:
                continue
            sig = all_metrics_by_id.get(cid, {}).get("signature") or "none" if 'all_metrics_by_id' in dir() else "unknown"
            # Use constraints dict instead
            sig = constraints.get(cid, {}).get("signature") or "none"
            so = sig_override[sig]
            so["total"] += 1
            if od["band_changed"]:
                so["band_changed"] += 1
            if od["top_type_changed"]:
                so["type_changed"] += 1
            so["deltas"].append(od["confidence_delta"])

        lines.append("| Signature | Total | Band Changed | Type Changed | Mean Conf Delta |")
        lines.append("|-----------|------:|-------------:|-------------:|----------------:|")
        for sig, so in sorted(sig_override.items(), key=lambda x: -x[1]["band_changed"]):
            if so["band_changed"] > 0 or so["type_changed"] > 0:
                mean_delta = sum(so["deltas"]) / len(so["deltas"]) if so["deltas"] else 0
                lines.append(f"| {sig} | {so['total']} | {so['band_changed']} | {so['type_changed']} | {mean_delta:+.4f} |")
        lines.append("")

        # FNL tangled_ropes analysis (the 48 at psi=0.4762)
        lines.append("### FNL (false_natural_law) Tangled Rope Cluster")
        lines.append("")
        fnl_tangled = {cid: od for cid, od in override_data.items()
                       if od and constraints.get(cid, {}).get("signature") == "false_natural_law"
                       and constraints.get(cid, {}).get("claimed_type") == "tangled_rope"}
        if fnl_tangled:
            lines.append(f"Total FNL tangled_rope constraints: **{len(fnl_tangled)}**")
            lines.append("")

            fnl_confs_override = [d["confidence_override"] for d in fnl_tangled.values()]
            fnl_confs_raw = [d["confidence_raw"] for d in fnl_tangled.values()]
            fnl_band_changed = sum(1 for d in fnl_tangled.values() if d["band_changed"])
            fnl_type_changed = sum(1 for d in fnl_tangled.values() if d["top_type_changed"])

            lines.append("| Metric | With Override | Without Override |")
            lines.append("|--------|-------------:|----------------:|")
            lines.append(f"| Mean confidence | {sum(fnl_confs_override)/len(fnl_confs_override):.4f} | {sum(fnl_confs_raw)/len(fnl_confs_raw):.4f} |")
            lines.append(f"| Min confidence | {min(fnl_confs_override):.4f} | {min(fnl_confs_raw):.4f} |")
            lines.append(f"| Max confidence | {max(fnl_confs_override):.4f} | {max(fnl_confs_raw):.4f} |")
            lines.append("")
            lines.append(f"Band changes: **{fnl_band_changed}** / {len(fnl_tangled)}")
            lines.append(f"Top type changes: **{fnl_type_changed}** / {len(fnl_tangled)}")
            lines.append("")

            # Show raw confidence distribution for FNL cluster
            lines.append("Raw confidence distribution (without overrides):")
            lines.append("")
            lines.append("```")
            lines.append(make_ascii_histogram(fnl_confs_raw, bins=10, width=40))
            lines.append("```")
            lines.append("")

            # Show individual FNL constraints with biggest deltas
            fnl_sorted = sorted(fnl_tangled.items(), key=lambda x: abs(x[1]["confidence_delta"]), reverse=True)
            lines.append("Top 10 FNL constraints by confidence delta:")
            lines.append("")
            lines.append("| Constraint | Override Conf | Raw Conf | Delta | Band Change |")
            lines.append("|------------|------------:|--------:|------:|:-----------:|")
            for cid, d in fnl_sorted[:10]:
                bc = "Y" if d["band_changed"] else ""
                lines.append(f"| {cid} | {d['confidence_override']:.4f} | {d['confidence_raw']:.4f} | "
                             f"{d['confidence_delta']:+.4f} | {bc} |")
            lines.append("")
        else:
            lines.append("(no FNL tangled_rope constraints found)")
            lines.append("")
    else:
        lines.append("(override comparison not available)")
        lines.append("")

    # --- Section 6: Confidence histogram ---
    lines.append("## 6. Confidence Distribution (Full Corpus)")
    lines.append("")
    lines.append("```")
    lines.append(f"  confidence = P(claimed_type) from MaxEnt distribution")
    lines.append(f"  Bands: deep (>0.8 AND margin>0.5), moderate (0.5-0.8), borderline (<0.5)")
    lines.append("")
    lines.append(make_ascii_histogram(confidences))
    lines.append("```")
    lines.append("")

    # Entropy histogram
    entropies = [m["entropy"] for m in all_metrics]
    lines.append("### Entropy Distribution")
    lines.append("")
    lines.append("```")
    lines.append(f"  entropy = normalized Shannon entropy (0=certain, 1=max uncertainty)")
    lines.append("")
    lines.append(make_ascii_histogram(entropies))
    lines.append("```")
    lines.append("")

    # Cross-references
    lines.append("## 7. Cross-References")
    lines.append("")
    lines.append("- **tangled_rope_decomposition_report.md** - Tangled rope fiber decomposition")
    lines.append("- **maxent_report.md** - MaxEnt shadow classifier hard disagreements")
    lines.append("- **maxent_diagnostic_report.md** - MaxEnt diagnostic details")
    lines.append("- **orbit_report.md** - Orbit-based perspective analysis")
    lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("[CONFIDENCE] Loading data...", file=sys.stderr)
    constraints = load_all_data()
    print(f"[CONFIDENCE] Loaded {len(constraints)} constraints.", file=sys.stderr)

    # --- Read distributions from pipeline_output.json ---
    pipeline_raw = load_json(PIPELINE_JSON, "pipeline_output")
    per_constraint = pipeline_raw.get("per_constraint", [])

    dists_override = {}
    dists_raw = {}
    for entry in per_constraint:
        cid = entry.get("id")
        probs = entry.get("maxent_probs")
        if probs and isinstance(probs, dict):
            dists_override[cid] = probs
        raw_probs = entry.get("raw_maxent_probs")
        if raw_probs and isinstance(raw_probs, dict):
            dists_raw[cid] = raw_probs

    print(f"[CONFIDENCE] Override distributions: {len(dists_override)}, Raw distributions: {len(dists_raw)}.", file=sys.stderr)

    # Fall back to Python replication only if pipeline lacks distributions
    if not dists_override:
        print("[CONFIDENCE] No maxent_probs in pipeline_output.json, computing via Python replication.", file=sys.stderr)
        dists_override = maxent_classify(constraints, apply_overrides=True)
    if not dists_raw:
        print("[CONFIDENCE] No raw_maxent_probs in pipeline_output.json, computing via Python replication.", file=sys.stderr)
        dists_raw = maxent_classify(constraints, apply_overrides=False)

    # --- Build per-constraint confidence metrics from enriched fields ---
    print("[CONFIDENCE] Building per-constraint confidence metrics...", file=sys.stderr)
    all_metrics = []
    enriched_by_id = {e["id"]: e for e in per_constraint}

    for cid, c in sorted(constraints.items()):
        enriched = enriched_by_id.get(cid, {})
        dist = dists_override.get(cid, {})

        # Use pre-computed enriched fields if available, else compute
        if enriched.get("confidence") is not None:
            m = {
                "id": cid,
                "claimed_type": c.get("claimed_type"),
                "confidence": enriched["confidence"],
                "rival_type": enriched.get("rival_type"),
                "rival_prob": enriched.get("rival_prob", 0.0),
                "margin": enriched.get("confidence_margin", 0.0),
                "entropy": enriched.get("confidence_entropy", 0.0),
                "band": enriched.get("confidence_band"),
                "boundary": enriched.get("boundary"),
                "signature": c.get("signature"),
                "purity_score": c.get("purity_score"),
                "purity_band": c.get("purity_band"),
                "domain": c.get("domain"),
                "human_readable": c.get("human_readable"),
            }
        else:
            m = compute_confidence_metrics(cid, c, dist)

        if m:
            all_metrics.append(m)

    print(f"[CONFIDENCE] Built metrics for {len(all_metrics)} constraints.", file=sys.stderr)

    # --- Override comparison from pipeline distributions ---
    print("[CONFIDENCE] Computing override vs raw comparison...", file=sys.stderr)
    override_data = {}
    for cid, c in constraints.items():
        d_o = dists_override.get(cid, {})
        d_r = dists_raw.get(cid, {})
        override_data[cid] = compute_override_comparison(cid, c, d_o, d_r)

    band_changes = sum(1 for d in override_data.values() if d and d["band_changed"])
    type_changes = sum(1 for d in override_data.values() if d and d["top_type_changed"])
    print(f"[CONFIDENCE] Override impact: {band_changes} band changes, {type_changes} top-type changes.", file=sys.stderr)

    # --- Generate report only (no JSON data file — data lives in pipeline_output.json) ---
    print("[CONFIDENCE] Generating report...", file=sys.stderr)
    report = generate_report(all_metrics, override_data, constraints, dists_override, dists_raw)
    with open(CONFIDENCE_REPORT, "w", encoding="utf-8") as f:
        f.write(report)
    print(f"[CONFIDENCE] Wrote {CONFIDENCE_REPORT}", file=sys.stderr)

    print("[CONFIDENCE] Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
