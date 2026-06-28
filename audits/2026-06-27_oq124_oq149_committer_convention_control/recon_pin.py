#!/usr/bin/env python3
"""Phase-0 pinning recon — inputs needed to PIN the pre-registration.

(1) Field A: for each CHE<->FCR fork pair, on the FCR side, is appears_as_rope riding
    source-1 (authored constraint_claim(C,rope) present) or only available via source-2
    (Eps <= rope_epsilon_ceiling)? This both quantifies the convention-share and lets us
    SELECT pre-named positive-control pairs (FCR side has constraint_claim rope AND Eps
    above the rope ceiling, so retraction must collapse appears_as_rope -> must flip).
(2) Field B: cross-twin stability of base_extractiveness (witness it is a settled covariate).
"""
import json
import re
import glob
from pathlib import Path

A = Path(__file__).resolve().parent
REPO = A.parents[1]
OUT = REPO / "outputs"

RE_CLAIM_ROPE = re.compile(r"constraint_claim\(\s*[a-z0-9_]+\s*,\s*rope\s*\)")


def load_pc(name):
    return {e["id"]: e for e in json.load(open(OUT / name))["per_constraint"]}


def has_claim_rope(twin, cid):
    f = REPO / "prolog" / f"testsets_{twin}" / f"{cid}.pl"
    return bool(RE_CLAIM_ROPE.search(open(f).read())) if f.exists() else None


def min_epsilon(entry):
    pcx = entry.get("perspective_chi") or {}
    eps = [v.get("epsilon") for v in pcx.values() if isinstance(v, dict) and v.get("epsilon") is not None]
    return min(eps) if eps else None


def main():
    H = load_pc("pipeline_output.haiku.json")
    F = load_pc("pipeline_output.flash.json")
    fork = json.load(open(A / "recon_reproduce.json"))["fieldA_fork_ids"]
    # rope_epsilon_ceiling from config
    cfg = open(REPO / "prolog" / "config.pl").read()
    m = re.search(r"param\(rope_epsilon_ceiling,\s*([0-9.]+)\)", cfg)
    rope_ceil = float(m.group(1)) if m else None

    res = {"rope_epsilon_ceiling": rope_ceil, "directions": {}}
    for dirn, ids in fork.items():
        # FCR side of the fork: in CHE->FCR (haiku CHE/flash FCR) the FCR model is flash;
        # in FCR->CHE the FCR model is haiku.
        fcr_twin = "flash" if dirn == "haiku_CHE_flash_FCR" else "haiku"
        che_twin = "haiku" if fcr_twin == "flash" else "flash"
        rows = []
        for cid in ids:
            fcr_entry = (F if fcr_twin == "flash" else H)[cid]
            che_entry = (H if che_twin == "haiku" else F)[cid]
            rows.append({
                "cid": cid,
                "fcr_side_claim_rope": has_claim_rope(fcr_twin, cid),
                "che_side_claim_rope": has_claim_rope(che_twin, cid),
                "fcr_side_min_eps": min_epsilon(fcr_entry),
                "che_side_base_ext": che_entry.get("base_extractiveness"),
                "fcr_side_base_ext": fcr_entry.get("base_extractiveness"),
            })
        n = len(rows)
        src1 = sum(1 for r in rows if r["fcr_side_claim_rope"])
        # control candidates: FCR side rides source-1 (claim rope) AND min eps ABOVE ceiling
        # (so source-2 cannot rescue appears_as_rope after retraction) -> MUST flip.
        ctrl = [r["cid"] for r in rows
                if r["fcr_side_claim_rope"] and r["fcr_side_min_eps"] is not None
                and rope_ceil is not None and r["fcr_side_min_eps"] > rope_ceil]
        res["directions"][dirn] = {
            "fcr_twin": fcr_twin, "n_fork": n,
            "fcr_side_has_claim_rope": src1,
            "fcr_side_source1_share": round(src1 / n, 4) if n else None,
            "control_candidates_count": len(ctrl),
            "control_candidates_sample": ctrl[:5],
            "rows_sample": rows[:6],
        }

    # (2) base_extractiveness cross-twin stability
    common = sorted(set(H) & set(F))
    pairs = [(H[c].get("base_extractiveness"), F[c].get("base_extractiveness"))
             for c in common
             if H[c].get("base_extractiveness") is not None and F[c].get("base_extractiveness") is not None]
    n = len(pairs)
    # spearman via rank correlation (no scipy): use simple pearson on ranks
    def ranks(xs):
        order = sorted(range(len(xs)), key=lambda i: xs[i])
        r = [0] * len(xs)
        for rank, i in enumerate(order):
            r[i] = rank
        return r
    hx = [p[0] for p in pairs]; fx = [p[1] for p in pairs]
    rh, rf = ranks(hx), ranks(fx)
    mh = sum(rh) / n; mf = sum(rf) / n
    cov = sum((rh[i] - mh) * (rf[i] - mf) for i in range(n))
    vh = sum((rh[i] - mh) ** 2 for i in range(n)) ** 0.5
    vf = sum((rf[i] - mf) ** 2 for i in range(n)) ** 0.5
    spearman = cov / (vh * vf) if vh and vf else None
    mean_abs_diff = sum(abs(p[0] - p[1]) for p in pairs) / n
    res["base_extractiveness_stability"] = {
        "n": n, "spearman_rho_cross_twin": round(spearman, 4) if spearman else None,
        "mean_abs_diff": round(mean_abs_diff, 4),
        "haiku_mean": round(sum(hx) / n, 4), "flash_mean": round(sum(fx) / n, 4),
    }

    json.dump(res, open(A / "recon_pin.json", "w"), indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
