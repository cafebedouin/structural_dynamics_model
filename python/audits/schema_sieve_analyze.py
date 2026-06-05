#!/usr/bin/env python3
"""Schema sieve Phase 2: analyze outputs/schema_sieve/features.json.

Questions:
  Q1. How much does each authored non-classification feature tell you about the
      type (claimed_type) and about the computed 4-perspective profile?
      (normalized mutual information per feature)
  Q2. Metric twins: constraints with IDENTICAL (eps, sup, tr) — do they classify
      identically? Where they diverge, what authored feature discriminates?
  Q3. Sieve cells: group by the non-numeric authored vector (binaries +
      coordination_type + counts). Which cells are type-pure, which span types?
  Q4. Inverse: which features are INERT (high corpus presence, ~zero MI)?
"""
import json
import math
import os
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
FEAT = os.path.join(HERE, "outputs", "schema_sieve", "features.json")
OUT = os.path.join(HERE, "outputs", "schema_sieve", "analysis.json")


def entropy(counter):
    n = sum(counter.values())
    if n == 0:
        return 0.0
    return -sum((c / n) * math.log2(c / n) for c in counter.values() if c)


def mutual_info(pairs):
    """MI(X;Y) from list of (x,y) pairs, plus H(Y)."""
    n = len(pairs)
    cx, cy, cxy = Counter(), Counter(), Counter()
    for x, y in pairs:
        cx[x] += 1
        cy[y] += 1
        cxy[(x, y)] += 1
    mi = 0.0
    for (x, y), c in cxy.items():
        p = c / n
        mi += p * math.log2(p * n * n / (cx[x] * cy[y]))
    return mi, entropy(cy)


def bin_num(v, width=0.1):
    if v is None:
        return "absent"
    return f"{math.floor(round(v, 6) / width) * width:.1f}"


def main():
    with open(FEAT) as f:
        data = json.load(f)
    rows = data["rows"]
    n = len(rows)
    print(f"manifest: run_at={data['manifest']['pipeline_run_at']} n={n} commit={data['manifest']['code_commit_short']} dirty={data['manifest']['code_dirty']}")
    print()

    # ---- targets ----
    ty = Counter(r["claimed_type"] for r in rows)
    prof = Counter(r["profile"] for r in rows)
    print(f"claimed_type dist: {dict(ty.most_common())}")
    print(f"H(claimed_type) = {entropy(ty):.3f} bits;  distinct profiles = {len(prof)}, H(profile) = {entropy(prof):.3f} bits")
    print()

    # ---- Q1: per-feature MI ----
    def feat_value(r, name):
        if name in ("eps", "sup", "tr", "acc_collapse"):
            return bin_num(r[name])
        if name == "omega_types":
            return ",".join(r["omega_types"]) or "none"
        if name == "contexts":
            return ";".join(r["contexts"])
        if name == "context_powers":
            return ",".join(sorted({c.split("|")[0] for c in r["contexts"]}))
        if name == "context_exits":
            return ",".join(sorted({c.split("|")[2] for c in r["contexts"]}))
        return str(r[name])

    features = ["eps", "sup", "tr", "acc_collapse", "emerges_naturally",
                "requires_active_enforcement", "has_sunset", "has_bfo",
                "n_dir_overrides", "n_affects", "n_beneficiaries", "n_victims",
                "n_omegas", "omega_types", "coordination_type", "n_perspectives",
                "context_powers", "context_exits", "contexts"]

    q1 = []
    for fname in features:
        vals = [feat_value(r, fname) for r in rows]
        nlevels = len(set(vals))
        mi_t, h_t = mutual_info(list(zip(vals, (r["claimed_type"] for r in rows))))
        mi_p, h_p = mutual_info(list(zip(vals, (r["profile"] for r in rows))))
        q1.append({"feature": fname, "levels": nlevels,
                   "nmi_claimed": round(mi_t / h_t, 3), "nmi_profile": round(mi_p / h_p, 3)})
    q1.sort(key=lambda d: -d["nmi_claimed"])
    print("Q1: normalized MI of each authored feature vs held-out classification")
    print(f"{'feature':28s} {'levels':>6s} {'nmi(type)':>10s} {'nmi(profile)':>12s}")
    for d in q1:
        print(f"{d['feature']:28s} {d['levels']:6d} {d['nmi_claimed']:10.3f} {d['nmi_profile']:12.3f}")
    print()

    # ---- Q2: metric twins ----
    groups = defaultdict(list)
    for r in rows:
        groups[(r["eps"], r["sup"], r["tr"])].append(r)
    multi = {k: v for k, v in groups.items() if len(v) > 1}
    het_type = {k: v for k, v in multi.items() if len({r["claimed_type"] for r in v}) > 1}
    het_prof = {k: v for k, v in multi.items() if len({r["profile"] for r in v}) > 1}
    sizes = Counter(len(v) for v in groups.values())
    print("Q2: exact metric triples (eps, sup, tr)")
    print(f"  distinct triples: {len(groups)} over {n} constraints; group-size dist: {dict(sorted(sizes.items()))}")
    print(f"  multi-member triples: {len(multi)} covering {sum(len(v) for v in multi.values())} constraints")
    print(f"  ... with heterogeneous claimed_type: {len(het_type)}")
    print(f"  ... with heterogeneous computed profile: {len(het_prof)}")
    # what discriminates type-heterogeneous twins?
    disc = Counter()
    for k, v in het_type.items():
        by_t = defaultdict(list)
        for r in v:
            by_t[r["claimed_type"]].append(r)
        ts = list(by_t)
        for i in range(len(ts)):
            for j in range(i + 1, len(ts)):
                a, b = by_t[ts[i]][0], by_t[ts[j]][0]
                for fname in ("emerges_naturally", "requires_active_enforcement", "has_sunset",
                              "has_bfo", "acc_collapse", "n_dir_overrides", "coordination_type",
                              "n_beneficiaries", "n_victims", "contexts", "n_perspectives"):
                    if feat_value(a, fname) != feat_value(b, fname):
                        disc[fname] += 1
    print(f"  discriminating authored features across type-heterogeneous twin pairs: {dict(disc.most_common())}")
    ex = sorted(het_type.items(), key=lambda kv: -len(kv[1]))[:5]
    for k, v in ex:
        print(f"    triple {k}: " + "; ".join(f"{r['id']}[{r['claimed_type']}]" for r in v[:6]) + (" ..." if len(v) > 6 else ""))
    print()

    # ---- Q3: sieve cells over non-numeric authored vector ----
    def cell_key(r):
        return (r["emerges_naturally"], r["requires_active_enforcement"], r["has_sunset"],
                r["has_bfo"], r["acc_collapse"] is not None, r["coordination_type"],
                r["n_dir_overrides"] > 0)

    cells = defaultdict(list)
    for r in rows:
        cells[cell_key(r)].append(r)
    print("Q3: sieve cells = (emerges, enforce, sunset, bfo, acc, coord_type, has_dirov)")
    print(f"  occupied cells: {len(cells)} (theoretical space is much larger)")
    pure = sum(1 for v in cells.values() if len({r['claimed_type'] for r in v}) == 1)
    print(f"  type-pure cells: {pure}/{len(cells)}")
    big = sorted(cells.items(), key=lambda kv: -len(kv[1]))[:12]
    print(f"  {'cell':70s} {'n':>5s}  type-mix")
    for k, v in big:
        tmix = Counter(r["claimed_type"] for r in v)
        kk = f"em={int(k[0])},enf={int(k[1])},sun={int(k[2])},bfo={int(k[3])},acc={int(k[4])},coord={k[5]},dir={int(k[6])}"
        print(f"  {kk:70s} {len(v):5d}  {dict(tmix.most_common(4))}")
    print()

    # cells that span types with strong minority mass (pattern outside the taxonomy?)
    print("  cells >=10 members where majority type < 60% (most type-mixed):")
    for k, v in sorted(cells.items(), key=lambda kv: -len(kv[1])):
        if len(v) < 10:
            continue
        tmix = Counter(r["claimed_type"] for r in v)
        maj = tmix.most_common(1)[0][1] / len(v)
        if maj < 0.60:
            kk = f"em={int(k[0])},enf={int(k[1])},coord={k[5]},dir={int(k[6])},bfo={int(k[3])}"
            print(f"    {kk:55s} n={len(v):4d} {dict(tmix.most_common())}")
    print()

    # ---- Q4: coordination_type x claimed_type table ----
    print("Q4: coordination_type x claimed_type (rows = coord types with n>=10)")
    ct = defaultdict(Counter)
    for r in rows:
        ct[r["coordination_type"]][r["claimed_type"]] += 1
    types_order = [t for t, _ in ty.most_common()]
    print(f"  {'coordination_type':34s} {'n':>5s} " + " ".join(f"{t[:9]:>9s}" for t in types_order))
    for k, c in sorted(ct.items(), key=lambda kv: -sum(kv[1].values())):
        tot = sum(c.values())
        if tot < 10:
            continue
        print(f"  {str(k):34s} {tot:5d} " + " ".join(f"{c.get(t,0)/tot:9.2f}" for t in types_order))

    with open(OUT, "w") as f:
        json.dump({"manifest": data["manifest"], "q1": q1,
                   "q2": {"n_triples": len(groups), "n_multi": len(multi),
                          "n_het_type": len(het_type), "n_het_profile": len(het_prof),
                          "discriminators": dict(disc),
                          "het_type_groups": {str(k): [(r["id"], r["claimed_type"], r["profile"]) for r in v]
                                              for k, v in het_type.items()}},
                   "q3_cells": {str(k): dict(Counter(r["claimed_type"] for r in v))
                                for k, v in cells.items()}}, f, indent=1)
    print(f"\nsaved: {OUT}")


if __name__ == "__main__":
    main()
