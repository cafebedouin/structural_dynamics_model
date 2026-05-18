"""
Turn 1 reconnaissance: group corpus constraints by L1 domain,
compute per-group raw statistics, write to container_typology_recon_data.json.
No analysis — purely aggregation.
"""

import json
import math
from collections import Counter
from pathlib import Path

BASE = Path(__file__).parent.parent
CORPUS_PATH = BASE / "outputs" / "corpus_data.json"
OUT_PATH = BASE / "outputs" / "container_typology_recon_data.json"


def orbit_entropy(sigs):
    """Shannon entropy of orbit signature distribution."""
    if not sigs:
        return 0.0
    counter = Counter(sigs)
    total = len(sigs)
    return -sum((n / total) * math.log2(n / total) for n in counter.values())


def main():
    with open(CORPUS_PATH) as f:
        data = json.load(f)
    constraints = data["constraints"]
    print(f"Loaded {len(constraints)} constraints")

    # Group by L1 domain
    by_l1 = {}
    for cid, c in constraints.items():
        domain = c.get("domain", "") or ""
        l1 = domain.split("/")[0].strip() if domain else "UNKNOWN"
        by_l1.setdefault(l1, []).append((cid, c))

    print(f"Distinct L1 domains: {len(by_l1)}")

    results = {}
    for l1, items in sorted(by_l1.items(), key=lambda x: -len(x[1])):
        n = len(items)

        type_counter = Counter()
        sig_counter = Counter()
        beneficiary_counter = Counter()
        orbit_sigs = []
        extractiveness_vals = []
        suppression_vals = []
        variance_ratio_vals = []
        types_produced_vals = []
        requires_enforcement_count = 0
        emerges_naturally_count = 0
        resistance_not_null_count = 0

        for cid, c in items:
            type_counter[c.get("claimed_type", "unknown")] += 1

            analysis = c.get("analysis", {}) or {}
            sig_counter[analysis.get("structural_signature", "unknown")] += 1

            for b in (c.get("beneficiaries") or []):
                beneficiary_counter[b] += 1

            for osig in (analysis.get("orbit_signature") or []):
                orbit_sigs.append(osig)

            metrics = c.get("metrics", {}) or {}
            ex = metrics.get("extractiveness")
            if ex is not None:
                extractiveness_vals.append(float(ex))
            sup = metrics.get("suppression")
            if sup is not None:
                suppression_vals.append(float(sup))

            vr = analysis.get("variance_ratio")
            if vr is not None:
                variance_ratio_vals.append(float(vr))

            tp = analysis.get("types_produced")
            if tp is not None:
                types_produced_vals.append(int(tp))

            if metrics.get("requires_enforcement"):
                requires_enforcement_count += 1
            if metrics.get("emerges_naturally"):
                emerges_naturally_count += 1
            if metrics.get("resistance") is not None:
                resistance_not_null_count += 1

        def safe_mean(vals):
            return round(sum(vals) / len(vals), 4) if vals else None

        results[l1] = {
            "n_constraints": n,
            "type_distribution": dict(type_counter.most_common()),
            "signature_distribution": dict(sig_counter.most_common()),
            "mean_extractiveness": safe_mean(extractiveness_vals),
            "mean_suppression": safe_mean(suppression_vals),
            "requires_enforcement_rate": round(requires_enforcement_count / n, 4),
            "emerges_naturally_rate": round(emerges_naturally_count / n, 4),
            "resistance_not_null_rate": round(resistance_not_null_count / n, 4),
            "mean_variance_ratio": safe_mean(variance_ratio_vals),
            "mean_types_produced": safe_mean(types_produced_vals),
            "top_beneficiaries": beneficiary_counter.most_common(10),
            "orbit_signature_entropy": round(orbit_entropy(orbit_sigs), 4),
            "orbit_signature_distribution": dict(Counter(orbit_sigs).most_common(6)),
        }

    # Compute L2 breakdown for top 6 L1 domains
    l2_breakdown = {}
    top_l1 = sorted(results.keys(), key=lambda k: -results[k]["n_constraints"])[:6]
    for l1 in top_l1:
        by_l2 = {}
        for cid, c in by_l1[l1]:
            domain = c.get("domain", "") or ""
            parts = domain.split("/")
            l2 = parts[1].strip() if len(parts) > 1 else "_root"
            by_l2.setdefault(l2, 0)
            by_l2[l2] += 1
        l2_breakdown[l1] = sorted(by_l2.items(), key=lambda x: -x[1])

    viable = {k: v for k, v in results.items() if v["n_constraints"] >= 20}
    print(f"L1 domains with 20+ constraints: {len(viable)}")

    output = {
        "total_constraints": len(constraints),
        "total_l1_domains": len(results),
        "viable_domains": len(viable),
        "by_domain": results,
        "l2_breakdown_top6": l2_breakdown,
        "viable_threshold": 20,
    }

    with open(OUT_PATH, "w") as f:
        json.dump(output, f, indent=2)
    print(f"Wrote {OUT_PATH}")

    # Print summary for turn 1 document
    print("\nViable domains (20+ constraints):")
    for k in sorted(viable.keys(), key=lambda k: -viable[k]["n_constraints"]):
        v = viable[k]
        print(
            f"  {k:30s}  n={v['n_constraints']:4d}  "
            f"ex={v['mean_extractiveness']}  "
            f"vr={v['mean_variance_ratio']}  "
            f"tp={v['mean_types_produced']}  "
            f"enf={v['requires_enforcement_rate']}"
        )

    print("\nL2 breakdown for top 6 L1 domains:")
    for l1, l2s in l2_breakdown.items():
        viable_l2 = [(k, n) for k, n in l2s if n >= 20]
        print(f"  {l1}: {len(l2s)} L2 sub-domains; {len(viable_l2)} with 20+ constraints")
        for k, n in l2s[:5]:
            print(f"    {k}: {n}")


if __name__ == "__main__":
    main()
