#!/usr/bin/env python3
"""oq347_core_null.py — is the situation-fixed core above what marginals alone predict?

Operator ruling 2026-08-25: a bare joint count (6/871 across 21 pairs) is not interpretable —
pairwise-above-chance and joint-near-zero are consistent, and 0.7% joint could be ABOVE or BELOW
what independent pairwise rates predict. This runs the same seed-label permutation at the CORE
level: each paired leg's records (the (h1_band, verdict, signature) triple as one unit, so every
leg's joint field distribution is preserved) are independently shuffled across the all-pairs
intersection; the core is recomputed per iteration. Leg-level permutation handles the pair
dependence (pairs share legs) correctly. One-sided p with the +1 correction, both directions
reported.

Usage: oq347_core_null.py --core-json outputs/coherent_<H>/situation_fixed_core.json
                          --seed 3471 [--iterations 2000] [--dir outputs]
"""
from __future__ import annotations

import argparse
import json
import random
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
if str(REPO / "python") not in sys.path:
    sys.path.insert(0, str(REPO / "python"))

FIELDS = (
    lambda r: r.get("h1_band"),
    lambda r: (r.get("verdict_join") or {}).get("verdict"),
    lambda r: r.get("signature"),
)


def core_size(legvals: dict, pairs, n: int) -> int:
    """#ids where every pair agrees on all three fields with non-null values."""
    core = 0
    for i in range(n):
        ok = True
        for a, b in pairs:
            ta, tb = legvals[a][i], legvals[b][i]
            if ta != tb or None in ta:
                ok = False
                break
        if ok:
            core += 1
    return core


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--core-json", required=True)
    ap.add_argument("--seed", type=int, required=True,
                    help="RNG seed — recorded, passed in, never drawn")
    ap.add_argument("--iterations", type=int, default=2000)
    ap.add_argument("--dir", default=str(REPO / "outputs"))
    args = ap.parse_args()
    core_d = json.load(open(args.core_json))
    pairs = [tuple(p) for p in core_d["pairs"]]
    inter = sorted(set(core_d["core"]) | set(core_d["agreed_on_null"]) | set(core_d["disagree"]))
    n = len(inter)
    out_dir = Path(args.dir)

    legvals = {}
    for leg in sorted({l for p in pairs for l in p}):
        suffix = leg[len("testsets_"):]
        rows = {r["id"]: r for r in
                json.load(open(out_dir / f"pipeline_output.{suffix}.json"))["per_constraint"]}
        legvals[leg] = [tuple(f(rows[i]) for f in FIELDS) for i in inter]

    obs = core_size(legvals, pairs, n)
    print(f"observed core: {obs}/{n} ({obs/n:.2%})  pairs={len(pairs)}  legs={len(legvals)}")
    assert obs == len(core_d["core"]), \
        f"reproduction control FAILED: recomputed core {obs} != recorded {len(core_d['core'])}"
    print(f"  [reproduction control: recomputed == recorded core size {len(core_d['core'])}]")

    rng = random.Random(args.seed)
    ge = le = 0
    total = 0
    dist = []
    for _ in range(args.iterations):
        perm = {leg: rng.sample(vals, n) for leg, vals in legvals.items()}
        c = core_size(perm, pairs, n)
        dist.append(c)
        total += c
        if c >= obs:
            ge += 1
        if c <= obs:
            le += 1
    mean = total / args.iterations
    dist.sort()
    print(f"null (marginals-only, {args.iterations} iters, seed {args.seed}): "
          f"mean={mean:.2f}  p2.5={dist[int(0.025*args.iterations)]}  "
          f"p97.5={dist[int(0.975*args.iterations)]}  max={dist[-1]}")
    print(f"p(null >= observed) = {(1+ge)/(1+args.iterations):.4f}   "
          f"p(null <= observed) = {(1+le)/(1+args.iterations):.4f}")
    if mean > 0:
        print(f"excess ratio observed/null_mean = {obs/mean:.1f}x")


if __name__ == "__main__":
    main()
