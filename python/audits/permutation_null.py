#!/usr/bin/env python3
"""permutation_null.py — OQ-348: does same-seed agreement beat each leg pair's marginals?

For a leg pair and a field, OBSERVED agreement is the fraction of shared ids where the two
legs' values are equal (paired_agreement.py's convention exactly, including None == None — the
permutation preserves each leg's marginals, Nones included, so the test is fair to that
convention). The NULL is seed-label permutation: shuffle leg B's values across the shared ids
and recompute; repeat --iterations times with a RECORDED RNG seed passed in, never drawn.
One-sided p with the +1 correction: p = (1 + #{null >= observed}) / (1 + iterations).

Fields: h1_band, verdict_join.verdict, signature (the h1/verdict axis of OQ-348's 2x2) and
base_extractiveness by exact equality (the ε axis; ε is authored and rail-quantized per model,
so exact-match marginal chance is meaningful).

Run on >=3 CROSS-model pairs AND on the within-model floors — the floors are the known-positive
arm: if same-seed structure does not beat the null on a pure redraw pair, the instrument is
wrong, not the corpora (discrimination witness, not an extra).

Interpretation of the 2x2 (beats-null on h1/verdict x beats-null on ε) is the WRITEUP's job —
this script names no cell.

Usage: permutation_null.py --seed 348 [--iterations 2000] [--dir outputs]
                           [--pairs haiku2:sonnet2 flash2:kimi ...]
       (legs named WITHOUT the testsets_ prefix, as in the output filenames)
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

FIELDS = {
    "h1_band": lambda r: r.get("h1_band"),
    "verdict": lambda r: (r.get("verdict_join") or {}).get("verdict"),
    "signature": lambda r: r.get("signature"),
    "epsilon": lambda r: r.get("base_extractiveness"),
}


def load(leg: str, out_dir: Path):
    p = out_dir / f"pipeline_output.{leg}.json"
    if not p.exists():
        print(f"  [{leg}] no output at {p} — skipped")
        return None
    d = json.load(open(p))
    return d["manifest"], {r["id"]: r for r in d["per_constraint"]}


def perm_test(xs, ys, iterations: int, rng: random.Random):
    """(observed agreement, null mean, null max, one-sided p) for value lists xs, ys."""
    n = len(xs)
    obs = sum(1 for a, b in zip(xs, ys) if a == b) / n
    ge, total = 0, 0.0
    ys = list(ys)
    for _ in range(iterations):
        rng.shuffle(ys)
        a = sum(1 for x, y in zip(xs, ys) if x == y) / n
        total += a
        if a >= obs:
            ge += 1
    return obs, total / iterations, (1 + ge) / (1 + iterations)


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, required=True,
                    help="RNG seed — RECORDED, passed in, never drawn (plan 2026-08-25)")
    ap.add_argument("--iterations", type=int, default=2000)
    ap.add_argument("--dir", default=str(REPO / "outputs"),
                    help="directory holding pipeline_output.<leg>.json (default outputs/ — the "
                         "ADOPTED coherent set)")
    ap.add_argument("--pairs", nargs="+", required=True,
                    help="legA:legB (output-name legs, no testsets_ prefix)")
    ap.add_argument("--json", default=None)
    args = ap.parse_args()
    out_dir = Path(args.dir)
    rng = random.Random(args.seed)
    print(f"permutation null: seed={args.seed} iterations={args.iterations} dir={out_dir}")

    results = []
    for spec in args.pairs:
        a, b = spec.split(":")
        va, vb = load(a, out_dir), load(b, out_dir)
        if va is None or vb is None:
            results.append({"pair": spec, "status": "SKIPPED_MISSING_LEG"})
            continue
        (ma, ra), (mb, rb) = va, vb
        coherent = ma["code_commit_short"] == mb["code_commit_short"]
        ids = sorted(set(ra) & set(rb))
        if len(ids) < 100:
            print(f"{a} vs {b}: only {len(ids)} shared ids — refusing (below min population)")
            results.append({"pair": spec, "status": "REFUSED_MIN_POPULATION", "n": len(ids)})
            continue
        row = {"pair": spec, "n": len(ids), "coherent_commits": coherent, "fields": {}}
        print(f"\n{a} vs {b}  n={len(ids)}"
              + ("" if coherent else f"  <-- commits differ: {ma['code_commit_short']} vs {mb['code_commit_short']}"))
        for k, f in FIELDS.items():
            xs = [f(ra[i]) for i in ids]
            ys = [f(rb[i]) for i in ids]
            obs, null_mean, p = perm_test(xs, ys, args.iterations, rng)
            beats = p < 0.05
            row["fields"][k] = {"observed": obs, "null_mean": null_mean, "p": p, "beats_null": beats}
            print(f"  {k:10} observed={obs:.3f}  null_mean={null_mean:.3f}  "
                  f"p={p:.4f}  {'BEATS null' if beats else 'does NOT beat null'}")
        results.append(row)

    if args.json:
        json.dump({"seed": args.seed, "iterations": args.iterations,
                   "dir": str(out_dir), "results": results}, open(args.json, "w"), indent=1)
        print(f"\n[json] {args.json}")


if __name__ == "__main__":
    main()
