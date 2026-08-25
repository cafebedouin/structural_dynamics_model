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

--condition-on-eps (OQ-348 re-specification, operator ruling 2026-08-25): the marginals-only
null above is MIS-SPECIFIED for the amplification question — every leg is authored from the
same seed pool, so ε (a property of the authored text) covaries across legs at the same seed BY
CONSTRUCTION and cannot serve as the negative control. This scheme permutes leg B's records only
WITHIN exact (ε_A, ε_B) strata, so the null already contains the full seed-keyed ε structure;
observed excess over it is engine structure beyond input passthrough. Built-in invariance
control: the ε field's observed == null_mean EXACTLY under this scheme (the permutation cannot
change ε pairing) — printed and asserted. Singleton strata permit no swap and contribute
identity, which biases the null TOWARD the observed value (conservative); the stratum-size
diagnostics are printed so that conservatism is visible.

Usage: permutation_null.py --seed 348 [--iterations 2000] [--dir outputs]
                           [--pairs haiku2:sonnet2 flash2:kimi ...] [--condition-on-eps]
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
    """(observed agreement, null mean, one-sided p) for value lists xs, ys."""
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


def perm_test_stratified(xs, ys, strata, iterations: int, rng: random.Random):
    """perm_test, but ys is shuffled only WITHIN the given strata (list of index lists).
    Preserves the joint (ε_A, ε_B) structure exactly; singleton strata contribute identity."""
    n = len(xs)
    obs = sum(1 for a, b in zip(xs, ys) if a == b) / n
    ge, total = 0, 0.0
    ys = list(ys)
    for _ in range(iterations):
        for idxs in strata:
            if len(idxs) < 2:
                continue
            vals = [ys[i] for i in idxs]
            rng.shuffle(vals)
            for i, v in zip(idxs, vals):
                ys[i] = v
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
    ap.add_argument("--condition-on-eps", action="store_true",
                    help="permute within exact (ε_A, ε_B) strata — the null keeps the seed-keyed "
                         "input structure (OQ-348 re-specification, 2026-08-25)")
    ap.add_argument("--json", default=None)
    args = ap.parse_args()
    out_dir = Path(args.dir)
    rng = random.Random(args.seed)
    print(f"permutation null: seed={args.seed} iterations={args.iterations} dir={out_dir}"
          + ("  scheme=WITHIN-(εA,εB)-STRATA" if args.condition_on_eps else "  scheme=marginals-only"))

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
        row = {"pair": spec, "n": len(ids), "coherent_commits": coherent,
               "scheme": "within_eps_strata" if args.condition_on_eps else "marginals_only",
               "fields": {}}
        print(f"\n{a} vs {b}  n={len(ids)}"
              + ("" if coherent else f"  <-- commits differ: {ma['code_commit_short']} vs {mb['code_commit_short']}"))
        strata = None
        if args.condition_on_eps:
            cells = {}
            for pos, i in enumerate(ids):
                key = (ra[i].get("base_extractiveness"), rb[i].get("base_extractiveness"))
                cells.setdefault(key, []).append(pos)
            strata = list(cells.values())
            n_single = sum(1 for s in strata if len(s) < 2)
            frozen = sum(len(s) for s in strata if len(s) < 2)
            row["strata"] = {"n_cells": len(strata), "n_singleton": n_single,
                            "ids_frozen": frozen, "frozen_share": frozen / len(ids)}
            print(f"  (εA,εB) strata: {len(strata)} cells, {n_single} singletons — "
                  f"{frozen} ids ({frozen/len(ids):.0%}) frozen in place (conservative)")
        for k, f in FIELDS.items():
            xs = [f(ra[i]) for i in ids]
            ys = [f(rb[i]) for i in ids]
            if strata is not None:
                obs, null_mean, p = perm_test_stratified(xs, ys, strata, args.iterations, rng)
            else:
                obs, null_mean, p = perm_test(xs, ys, args.iterations, rng)
            beats = p < 0.05
            row["fields"][k] = {"observed": obs, "null_mean": null_mean, "p": p, "beats_null": beats}
            note = ""
            if strata is not None and k == "epsilon":
                assert abs(obs - null_mean) < 1e-12, \
                    f"invariance control FAILED: ε moved under within-(εA,εB) permutation ({obs} vs {null_mean})"
                note = "  [invariance control: observed==null exactly, scheme verified]"
            print(f"  {k:10} observed={obs:.3f}  null_mean={null_mean:.3f}  "
                  f"p={p:.4f}  {'BEATS null' if beats else 'does NOT beat null'}{note}")
        results.append(row)

    if args.json:
        json.dump({"seed": args.seed, "iterations": args.iterations,
                   "dir": str(out_dir), "results": results}, open(args.json, "w"), indent=1)
        print(f"\n[json] {args.json}")


if __name__ == "__main__":
    main()
