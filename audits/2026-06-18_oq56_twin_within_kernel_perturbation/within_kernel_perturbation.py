#!/usr/bin/env python3
"""OQ-56 D1 — within-kernel reading perturbation on the twins.

Hold model + observer-seat fixed; vary the reading within a kernel. Runs on the live
4-perspective-seat schema (the reading_diff operator is schema-stranded — see
PRE_REGISTRATION.md). Reproducible, deterministic, no engine re-run.
"""
import json, random
from collections import defaultdict, Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = Path(__file__).resolve().parent / "results.json"
SEATS = ["powerless", "moderate", "institutional", "analytical"]
PERMUTE_N = 1000
SEED = 20260618


def load(twin):
    d = json.load(open(ROOT / f"outputs/pipeline_output.{twin}.json"))
    pc = d["per_constraint"]
    items = pc.values() if isinstance(pc, dict) else pc
    rows = {}
    for v in items:
        rows[v["id"]] = dict(
            kernel=v["id"].split("__", 1)[0],
            persp={s: (v.get("perspectives") or {}).get(s) for s in SEATS},
            claimed=v.get("claimed_type"),
        )
    return rows


def kernels(rows):
    by = defaultdict(list)
    for cid, r in rows.items():
        by[r["kernel"]].append(cid)
    return {k: sorted(v) for k, v in by.items() if len(v) >= 2}


def seat_disparity(rows, reading_ids, seat):
    types = {rows[c]["persp"][seat] for c in reading_ids if rows[c]["persp"][seat]}
    return len(types)


def depth_vector(rows, reading_ids):
    return tuple(1 if seat_disparity(rows, reading_ids, s) >= 2 else 0 for s in SEATS)


def wilson_lo(k, n, z=1.96):
    if n == 0:
        return 0.0
    p = k / n
    d = 1 + z * z / n
    c = p + z * z / (2 * n)
    m = z * ((p * (1 - p) / n + z * z / (4 * n * n)) ** 0.5)
    return (c - m) / d


def main():
    H, F = load("haiku"), load("flash")
    KH, KF = kernels(H), kernels(F)

    res = {"inputs": {"haiku_n": len(H), "flash_n": len(F),
                      "haiku_multi_kernels": len(KH), "flash_multi_kernels": len(KF)}}

    # ---- positive control 2: claimed_type twin agreement (expect ~0.721) ----
    both_ids = set(H) & set(F)
    ct = sum(1 for c in both_ids if H[c]["claimed"] == F[c]["claimed"]) / len(both_ids)
    res["control_claimed_type_agreement"] = round(ct, 4)

    # ---- M1: seat-depth gradient (per twin) ----
    res["M1_seat_depth_rate"] = {}
    for name, (rows, K) in [("haiku", (H, KH)), ("flash", (F, KF))]:
        rate = {s: round(sum(1 for ids in K.values()
                             if seat_disparity(rows, ids, s) >= 2) / len(K), 4)
                for s in SEATS}
        any_depth = round(sum(1 for ids in K.values()
                              if any(seat_disparity(rows, ids, s) >= 2 for s in SEATS))
                          / len(K), 4)
        res["M1_seat_depth_rate"][name] = {"per_seat": rate, "any_seat": any_depth}

    # ---- positive control 3: non-vacuity ----
    maxrate = max(max(res["M1_seat_depth_rate"][t]["per_seat"].values()) for t in ("haiku", "flash"))
    res["control_non_vacuity_max_seat_rate"] = maxrate
    if maxrate == 0.0:
        res["HALT"] = "non-vacuity control failed: no within-kernel depth at any seat"
        json.dump(res, open(OUT, "w"), indent=2)
        print(json.dumps(res, indent=2)); return

    # ---- M2: model-invariance of the depth-vector ----
    common = sorted(set(KH) & set(KF))
    dvH = {k: depth_vector(H, KH[k]) for k in common}
    dvF = {k: depth_vector(F, KF[k]) for k in common}
    agree = sum(1 for k in common if dvH[k] == dvF[k])
    n = len(common)
    rng = random.Random(SEED)
    fvals = list(dvF.values())
    perm = []
    for _ in range(PERMUTE_N):
        sh = fvals[:]; rng.shuffle(sh)
        perm.append(sum(1 for a, b in zip((dvH[k] for k in common), sh) if a == b) / n)
    perm.sort()
    band95 = perm[int(0.95 * PERMUTE_N)]
    obs = agree / n
    lo = wilson_lo(agree, n)
    res["M2_depth_vector_invariance"] = dict(
        common_kernels=n, observed_agreement=round(obs, 4), wilson95_lo=round(lo, 4),
        permute_band95=round(band95, 4), permute_mean=round(sum(perm) / len(perm), 4),
        verdict=("PASS_model_invariant" if lo > band95 and band95 < 1.0
                 else "OPEN_or_model_dependent"))
    # also: per-seat invariance of the depth bit
    res["M2_per_seat_bit_agreement"] = {
        s: round(sum(1 for k in common
                     if (seat_disparity(H, KH[k], s) >= 2) == (seat_disparity(F, KF[k], s) >= 2))
                 / n, 4) for s in SEATS}

    # ---- M3: orbit grouping (exploratory) ----
    def signature(rows, ids):
        return (len(ids), depth_vector(rows, ids),
                tuple(sorted(Counter(rows[c]["claimed"] for c in ids).items())))
    sigH = {k: signature(H, KH[k]) for k in KH}
    sigF = {k: signature(F, KF[k]) for k in KF}
    res["M3_orbits"] = dict(
        haiku_distinct_signatures=len(set(sigH.values())),
        flash_distinct_signatures=len(set(sigF.values())),
        haiku_largest_orbit=max(Counter(sigH.values()).values()),
        common_kernels=n,
        same_signature_across_models=round(sum(1 for k in common if sigH[k] == sigF[k]) / n, 4),
        # coarser orbit: just the depth-vector class
        depth_vector_class_agreement=round(sum(1 for k in common if dvH[k] == dvF[k]) / n, 4),
        haiku_depth_vector_distribution=dict(Counter(str(v) for v in dvH.values())),
    )

    json.dump(res, open(OUT, "w"), indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
