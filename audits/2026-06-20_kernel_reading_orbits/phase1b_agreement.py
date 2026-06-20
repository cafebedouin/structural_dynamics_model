#!/usr/bin/env python3
"""Phase 1b — agreement test for the two Prolog-only orbit keys (OQ-150).

Inputs: phase1b_{testsets_haiku,testsets_flash}.tsv produced by phase1b_probe.pl
(read-only swipl probe over each twin corpus). Same method as Phase 1: cross-twin
membership agreement, Wilson-95 lo, permutation band95 (N=1000, seed 20260620).
Judged against the extraction baseline (0.721) for declarability, not just beats-chance.
"""
import json
import random
from collections import Counter
from pathlib import Path

A = Path(__file__).resolve().parent
SEED = 20260620
N = 1000
BASELINE = 0.721


def wilson_lo(k, n, z=1.96):
    if n == 0:
        return 0.0
    p = k / n
    d = 1 + z * z / n
    c = p + z * z / (2 * n)
    m = z * ((p * (1 - p) / n + z * z / (4 * n * n)) ** 0.5)
    return (c - m) / d


def parse(twin, tag):
    out = {}
    for line in open(A / f"phase1b_{twin}.tsv"):
        p = line.rstrip("\n").split("\t")
        if p[0] == tag:
            out[p[1]] = p[2]
    return out


def test(name, H, F, rng, drop="none"):
    common = sorted(set(H) & set(F))
    keyed = [i for i in common if H[i] != drop and F[i] != drop]
    n = len(keyed)
    agree = sum(1 for i in keyed if H[i] == F[i])
    obs = agree / n
    lo = wilson_lo(agree, n)
    fv = [F[i] for i in keyed]
    hv = [H[i] for i in keyed]
    perm = []
    for _ in range(N):
        sh = fv[:]
        rng.shuffle(sh)
        perm.append(sum(1 for a, b in zip(hv, sh) if a == b) / n)
    perm.sort()
    band95 = perm[int(0.95 * N)]
    return {
        "name": name, "n_keyed": n, "agreement": round(obs, 4),
        "wilson95_lo": round(lo, 4), "permute_band95": round(band95, 4),
        "distinct_haiku": len(set(hv)), "distinct_flash": len(set(fv)),
        "largest_orbit_haiku": round(max(Counter(hv).values()) / n, 4),
        "largest_orbit_flash": round(max(Counter(fv).values()) / n, 4),
        "beats_chance": lo > band95,
        "tier": ("membership_reproducible_at_baseline" if obs >= 0.70
                 else "above_chance_membership_fragile"),
    }


def main():
    rng = random.Random(SEED)
    res = {"baseline": BASELINE, "seed": SEED, "permute_n": N}
    gH, gF = parse("testsets_haiku", "GROUND"), parse("testsets_flash", "GROUND")
    res["ctrl_grounding_none_rate"] = {
        "haiku": sum(1 for v in gH.values() if v == "none"),
        "flash": sum(1 for v in gF.values() if v == "none")}
    res["axiom_grounding_profile"] = test("axiom_grounding_profile", gH, gF, rng)
    oH, oF = parse("testsets_haiku", "OBSTRUCT"), parse("testsets_flash", "OBSTRUCT")
    res["obstruction_dist_haiku"] = dict(Counter(oH.values()))
    res["obstruction_dist_flash"] = dict(Counter(oF.values()))
    res["kernel_obstruction_class"] = test("kernel_obstruction_class", oH, oF, rng, drop="\x00")
    json.dump(res, open(A / "phase1b_results.json", "w"), indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
