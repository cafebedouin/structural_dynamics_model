#!/usr/bin/env python3
"""OQ-71 Step 6 — excess-over-control machinery on six-dim fingerprint dumps.

Inputs (all produced by lineage_fingerprint_probe.py + Step 0/1 artifacts):
  --depth-tagged    tagged TSV for the depth arm (run-tagged corpus)
  --live-tagged     tagged TSV for the live corpus (baseline + control source)
  --lineage         lineage.json sidecar (levels, parents)
  --control         control_membership.json
  --control-seeds   never_generated_seeds.json (control sibling lengths)
  --depth-seeds     lineage_seeds.json (depth sibling lengths)
  --matched-n       per-arm sample size for the excess comparison (default: largest
                    n both arms support in the powered stratum)

Statistic (pre-registered, OQ-71): WITHIN-LENGTH-STRATUM excess-over-control —
powered stratum = sibling-length 2+ pooled; length-1 reported separately
(control stratification: length-1 slopes, pctile <=0.002; lengths 2-4 flat).
Excess = E[distinct 5-dim classes in depth sample] - E[same in control sample]
at matched n, K resamples; plus rarefaction slope at matched n.

PILOT NOTE: at depth-arm n < 300 this output is a MACHINERY check, not an H1
readout. Do not read pilot excess as signal (OQ-71 pilot ruling).
"""
import argparse
import json
import random
import statistics
from collections import Counter
from pathlib import Path


def split_six(six):
    i = six.index(")", len("six(shift("))
    return six[len("six("):i + 1], six[i + 2:-1]


def load_tagged(path):
    out = {}
    for line in Path(path).read_text().splitlines():
        cid, six = line.split("\t", 1)
        out[cid] = split_six(six)
    return out


def expected_distinct(fives, n, K, rng):
    if n > len(fives):
        return None
    return statistics.mean(len(set(rng.sample(fives, n))) for _ in range(K))


def slope_at(fives, n, K, rng, dn=10):
    a = expected_distinct(fives, n - dn, K, rng)
    b = expected_distinct(fives, n, K, rng)
    return None if (a is None or b is None) else (b - a) / dn


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--depth-tagged", required=True)
    ap.add_argument("--live-tagged", required=True)
    ap.add_argument("--lineage", required=True)
    ap.add_argument("--control", required=True)
    ap.add_argument("--control-seeds", required=True)
    ap.add_argument("--depth-seeds", required=True)
    ap.add_argument("--matched-n", type=int, default=None)
    ap.add_argument("--K", type=int, default=2000)
    ap.add_argument("--seed", type=int, default=71)
    ap.add_argument("--out", default=None)
    args = ap.parse_args()
    rng = random.Random(args.seed)

    depth = load_tagged(args.depth_tagged)
    live = load_tagged(args.live_tagged)
    lineage = {n["constraint_id"]: n for n in json.loads(Path(args.lineage).read_text())}
    ctl_ids = set(json.loads(Path(args.control).read_text())["constraint_ids"])
    sib_c = {s["constraint_id"]: len(s["sibling_reading_ids"])
             for s in json.loads(Path(args.control_seeds).read_text())}
    sib_d = {s["constraint_id"]: len(s["sibling_reading_ids"])
             for s in json.loads(Path(args.depth_seeds).read_text())}

    # baseline realized set = live corpus EXCLUDING control (else control novelty = 0
    # by construction; both arms are scored against the same non-arm baseline)
    baseline = {f for c, (_, f) in live.items() if c not in ctl_ids}
    ctl = {c: live[c] for c in ctl_ids if c in live}

    print(f"depth arm: {len(depth)} stories, "
          f"{len({f for _, f in depth.values()})} distinct 5-dim classes")
    print(f"control:   {len(ctl)} stories, "
          f"{len({f for _, f in ctl.values()})} distinct 5-dim classes")
    print(f"baseline (live minus control): {len(baseline)} classes realized")

    # corpus-novel DIAGNOSTIC only (undersampled-baseline caveat, OQ-71)
    nov_d = {f for _, f in depth.values()} - baseline
    nov_c = {f for _, f in ctl.values()} - baseline
    print(f"\n[diagnostic] classes not in baseline: depth {len(nov_d)}, control {len(nov_c)}")

    # within-stratum excess
    strat = {"len2plus": (lambda L: L >= 2), "len1": (lambda L: L == 1)}
    results = {}
    for name, pred in strat.items():
        d5 = [f for c, (_, f) in depth.items() if pred(sib_d.get(c, -1))]
        c5 = [f for c, (_, f) in ctl.items() if pred(sib_c.get(c, -1))]
        n = args.matched_n or min(len(d5), len(c5))
        if n < 10:
            print(f"\nstratum {name}: depth n={len(d5)}, control n={len(c5)} — "
                  f"too small, reported descriptively only")
            results[name] = {"depth_n": len(d5), "control_n": len(c5),
                             "verdict": "underpowered"}
            continue
        ed = expected_distinct(d5, n, args.K, rng)
        ec = expected_distinct(c5, n, args.K, rng)
        sd = slope_at(d5, n, args.K, rng)
        sc = slope_at(c5, n, args.K, rng)
        # resampling spread of the control estimate for a noise yardstick
        boots = [len(set(rng.sample(c5, n))) for _ in range(args.K)]
        lo, hi = sorted(boots)[int(0.025 * args.K)], sorted(boots)[int(0.975 * args.K)]
        print(f"\nstratum {name} @ matched n={n} (depth n={len(d5)}, control n={len(c5)}):")
        print(f"  E[distinct]: depth {ed:.1f}  control {ec:.1f}  excess {ed - ec:+.1f}"
              f"  (control 95% resample band [{lo},{hi}])")
        print(f"  slope@n: depth {sd:.3f}  control {sc:.3f}  excess {sd - sc:+.3f}")
        results[name] = {"matched_n": n, "depth_n": len(d5), "control_n": len(c5),
                         "E_depth": ed, "E_control": ec, "excess": ed - ec,
                         "slope_depth": sd, "slope_control": sc,
                         "control_resample_band": [lo, hi]}

    # per-level census (descriptive; H2 needs within-level matched n at scale)
    print("\nper-level census (level -> stories, distinct classes, new vs baseline, "
          "new vs shallower levels):")
    seen_shallower = set()
    lvl_rows = {}
    for lvl in sorted({lineage[c]["level"] for c in depth if c in lineage}):
        cs = [c for c in depth if c in lineage and lineage[c]["level"] == lvl]
        fs = {depth[c][1] for c in cs}
        row = (len(cs), len(fs), len(fs - baseline), len(fs - baseline - seen_shallower))
        lvl_rows[lvl] = row
        print(f"  L{lvl}: {row[0]:>3} stories  {row[1]:>3} classes  "
              f"{row[2]:>3} non-baseline  {row[3]:>3} new-vs-shallower")
        seen_shallower |= fs

    if args.out:
        Path(args.out).write_text(json.dumps(
            {"strata": results, "per_level": lvl_rows,
             "diagnostic_novel": {"depth": len(nov_d), "control": len(nov_c)},
             "params": {"K": args.K, "seed": args.seed}}, indent=2))
        print(f"\nwrote {args.out}")


if __name__ == "__main__":
    main()
