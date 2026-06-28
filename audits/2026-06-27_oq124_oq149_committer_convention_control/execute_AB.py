#!/usr/bin/env python3
"""Phase 2 — Field A fork decomposition + Field B covariation (pre-registered rules).

Field A: classify each fork-slot FCR limb as SOURCE1_ONLY / BOTH / SOURCE2_ONLY and apply
the pinned rule (SOURCE1_ONLY share >= 0.5 => CONVENTION; else SIGNAL if CHE-side
base_extractiveness > FCR-side on >= 0.8 of forks). The Prolog retraction control is run
separately (retraction_control.pl).

Field B: per-slot forecloses_fraction directionality (sign test) + Spearman covariation with
authored base_extractiveness on disagreeing vs agreeing slots (positive control).
"""
import json
import re
import glob
from collections import Counter
from pathlib import Path

A = Path(__file__).resolve().parent
REPO = A.parents[1]
OUT = REPO / "outputs"
RE_CLAIM_ROPE = re.compile(r"constraint_claim\(\s*[a-z0-9_]+\s*,\s*rope\s*\)")
RE_RELATION = re.compile(r"cs_reading_relation\([^,]+,\s*[^,]+,\s*([a-z_]+)\)")
ROPE_CEIL = 0.45  # config:param(rope_epsilon_ceiling, 0.45), witnessed in recon_pin


def load_pc(name):
    return {e["id"]: e for e in json.load(open(OUT / name))["per_constraint"]}


def has_claim_rope(twin, cid):
    f = REPO / "prolog" / f"testsets_{twin}" / f"{cid}.pl"
    return bool(RE_CLAIM_ROPE.search(open(f).read())) if f.exists() else False


def min_epsilon(entry):
    pcx = entry.get("perspective_chi") or {}
    eps = [v.get("epsilon") for v in pcx.values()
           if isinstance(v, dict) and v.get("epsilon") is not None]
    return min(eps) if eps else None


def relations(twin, cid):
    f = REPO / "prolog" / f"testsets_{twin}" / f"{cid}.pl"
    return RE_RELATION.findall(open(f).read()) if f.exists() else []


def ranks(xs):
    order = sorted(range(len(xs)), key=lambda i: xs[i])
    r = [0.0] * len(xs)
    # average ranks for ties
    i = 0
    while i < len(order):
        j = i
        while j + 1 < len(order) and xs[order[j + 1]] == xs[order[i]]:
            j += 1
        avg = (i + j) / 2.0
        for k in range(i, j + 1):
            r[order[k]] = avg
        i = j + 1
    return r


def spearman(xs, ys):
    n = len(xs)
    if n < 3:
        return None
    rx, ry = ranks(xs), ranks(ys)
    mx, my = sum(rx) / n, sum(ry) / n
    cov = sum((rx[i] - mx) * (ry[i] - my) for i in range(n))
    vx = sum((rx[i] - mx) ** 2 for i in range(n)) ** 0.5
    vy = sum((ry[i] - my) ** 2 for i in range(n)) ** 0.5
    return round(cov / (vx * vy), 4) if vx and vy else None


def sign_test_p(diffs):
    """Two-sided sign test: P(|deviation| this extreme) via normal approx on nonzero diffs."""
    pos = sum(1 for d in diffs if d > 0)
    neg = sum(1 for d in diffs if d < 0)
    n = pos + neg
    if n == 0:
        return {"pos": pos, "neg": neg, "n": 0, "p_approx": 1.0}
    # normal approx to binomial(n, 0.5)
    mu = n / 2.0
    sd = (n * 0.25) ** 0.5
    z = (abs(pos - mu) - 0.5) / sd if sd else 0.0
    # two-sided p via erfc
    import math
    p = math.erfc(z / (2 ** 0.5))
    return {"pos": pos, "neg": neg, "n": n, "z": round(z, 3), "p_approx": round(p, 5)}


def main():
    H = load_pc("pipeline_output.haiku.json")
    F = load_pc("pipeline_output.flash.json")
    res = {}

    # ---------------- Field A ----------------
    fork = json.load(open(A / "recon_reproduce.json"))["fieldA_fork_ids"]
    fieldA = {}
    for dirn, ids in fork.items():
        fcr_twin = "flash" if dirn == "haiku_CHE_flash_FCR" else "haiku"
        che_twin = "haiku" if fcr_twin == "flash" else "flash"
        classes = Counter()
        che_gt_fcr = 0
        anomalies = []
        for cid in ids:
            fcr_entry = (F if fcr_twin == "flash" else H)[cid]
            che_entry = (H if che_twin == "haiku" else F)[cid]
            claim = has_claim_rope(fcr_twin, cid)
            eps = min_epsilon(fcr_entry)
            if eps is None:
                anomalies.append((cid, "no_eps"))
                continue
            low_eps = eps <= ROPE_CEIL
            if claim and not low_eps:
                cls = "SOURCE1_ONLY"
            elif claim and low_eps:
                cls = "BOTH"
            elif (not claim) and low_eps:
                cls = "SOURCE2_ONLY"
            else:  # no claim, eps > ceiling -> shouldn't be FCR via either source
                cls = "ANOMALY_no_source"
                anomalies.append((cid, f"eps={eps},claim={claim}"))
            classes[cls] += 1
            che_ext = che_entry.get("base_extractiveness")
            fcr_ext = fcr_entry.get("base_extractiveness")
            if che_ext is not None and fcr_ext is not None and che_ext > fcr_ext:
                che_gt_fcr += 1
        n = sum(classes.values())
        s1_share = round(classes["SOURCE1_ONLY"] / n, 4) if n else None
        magnitude_share = round(che_gt_fcr / n, 4) if n else None
        # pinned rule
        if s1_share is not None and s1_share >= 0.5:
            verdict = "CONVENTION"
        elif s1_share is not None and s1_share < 0.5 and magnitude_share is not None and magnitude_share >= 0.8:
            verdict = "SIGNAL"
        else:
            verdict = "MIXED_OPEN"
        fieldA[dirn] = {
            "fcr_twin": fcr_twin, "n_fork": n, "classes": dict(classes),
            "source1_only_share": s1_share,
            "che_ext_gt_fcr_ext_share": magnitude_share,
            "verdict": verdict, "anomalies": anomalies,
        }
    # size-weighted overall (report both, no quiet asymmetry)
    res["fieldA"] = fieldA

    # ---------------- Field B ----------------
    slots = sorted(set(H) & set(F))
    rows = []
    for cid in slots:
        rh, rf = relations("haiku", cid), relations("flash", cid)
        if not rh or not rf:
            continue
        ext_h = H[cid].get("base_extractiveness")
        ext_f = F[cid].get("base_extractiveness")
        ffrac_h = sum(1 for x in rh if x == "forecloses") / len(rh)
        ffrac_f = sum(1 for x in rf if x == "forecloses") / len(rf)
        agree = tuple(sorted(rh)) == tuple(sorted(rf))
        rows.append({"cid": cid, "agree": agree,
                     "ffrac_h": ffrac_h, "ffrac_f": ffrac_f,
                     "ext_h": ext_h, "ext_f": ext_f})

    # (a) directionality: paired flash - haiku forecloses_fraction
    diffs = [r["ffrac_f"] - r["ffrac_h"] for r in rows]
    mean_diff = sum(diffs) / len(diffs)
    st = sign_test_p(diffs)

    # (b) covariation per model, on disagreeing vs agreeing slots
    def cov_subset(subset):
        rr = [r for r in rows if r["agree"] == subset]
        h = [(r["ffrac_h"], r["ext_h"]) for r in rr if r["ext_h"] is not None]
        f = [(r["ffrac_f"], r["ext_f"]) for r in rr if r["ext_f"] is not None]
        return {
            "n": len(rr),
            "spearman_haiku": spearman([x[0] for x in h], [x[1] for x in h]),
            "spearman_flash": spearman([x[0] for x in f], [x[1] for x in f]),
        }
    disagree = cov_subset(False)
    agree = cov_subset(True)

    # pinned rule
    def passes(c):
        return (c["spearman_haiku"] is not None and c["spearman_flash"] is not None
                and c["spearman_haiku"] >= 0.20 and c["spearman_flash"] >= 0.20)
    signal = passes(disagree) and passes(agree)
    lean = st["p_approx"] < 0.05
    if signal:
        verdictB = "SIGNAL"
    elif lean and not signal:
        verdictB = "CONVENTION"
    else:
        verdictB = "INDETERMINATE_OPEN"

    res["fieldB"] = {
        "n_slots": len(rows),
        "directionality_flash_minus_haiku": {
            "mean_diff": round(mean_diff, 4), **st,
            "marginal_forecloses_frac_haiku": round(sum(r["ffrac_h"] for r in rows) / len(rows), 4),
            "marginal_forecloses_frac_flash": round(sum(r["ffrac_f"] for r in rows) / len(rows), 4),
        },
        "covariation_disagreeing": disagree,
        "covariation_agreeing_control": agree,
        "verdict": verdictB,
    }

    json.dump(res, open(A / "execute_AB.json", "w"), indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
