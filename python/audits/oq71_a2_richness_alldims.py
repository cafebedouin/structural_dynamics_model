"""OQ-71 Phase A2 — close the list-inflation caveat over ALL 5 dims.

The powered H1 readout reported "NOT list-inflation" using only 2 of the 5
structural dims (mean |props| 3.83 vs 3.77, |voids| 1.98 vs 2.25). That left
open whether the 1.5x distinct-class excess rides cardinality proliferation
through actors / drift / zone. This closes the 2-of-5 gap.

Method (matched to the powered readout: length-2+ stratum, matched n, K=2000,
seed 71). For each arm at matched n, decompose richness:
  - JOINT distinct 5-dim classes  (the headline excess)
  - per-dim MARGINAL distinct values, all 5 dims
  - props/voids mean list length  (the original cardinality proxy)
Reasoning: if the JOINT excess far exceeds every single dim's MARGINAL excess,
the extra classes are new *combinations* across dims, not any one dim getting
richer (cardinality proliferation). A per-dim marginal that itself carries the
excess would be the proliferation signature.

Positive control (required — a clean "no proliferation" null is byte-identical
to a probe that never measured the dim): synthesise an inflated-props control
arm and confirm the metric DOES flag the planted cardinality jump.
"""
import argparse
import json
import random
import statistics
from pathlib import Path

AUDIT = Path(__file__).resolve().parent.parent.parent / "audits" / "2026-06-04_oq71_depth_lineage"


def split_six(six):
    i = six.index(")", len("six(shift("))
    return six[len("six("):i + 1], six[i + 2:-1]


def load_tagged(path):
    out = {}
    for line in Path(path).read_text().splitlines():
        cid, six = line.split("\t", 1)
        out[cid] = split_six(six)
    return out


def parse5(f):
    """Split a 5-dim string into (props, voids, actors, drift, zone) substrings."""
    props = f[: f.index("]") + 1]
    rest = f[len(props) + 1:]
    voids = rest[: rest.index("]") + 1]
    rest2 = rest[len(voids) + 1:]
    actors = rest2[: rest2.index(")") + 1]
    rest3 = rest2[len(actors) + 1:]
    drift = rest3[: rest3.index(")") + 1]
    zone = rest3[rest3.index(")") + 2:]
    return props, voids, actors, drift, zone


def list_len(dim_str):
    """Cardinality of a [a,b,c] list dim ([] -> 0)."""
    inner = dim_str.strip()[1:-1]
    return 0 if not inner else inner.count(",") + 1


DIMS = ["props", "voids", "actors", "drift", "zone"]


def richness(fives, n, K, rng):
    """At matched n, K resamples: joint distinct 5-tuples + per-dim marginal
    distinct values + props/voids mean list length."""
    if n > len(fives):
        return None
    joint, marg = [], {d: [] for d in DIMS}
    plen, vlen = [], []
    for _ in range(K):
        sample = rng.sample(fives, n)
        parsed = [parse5(f) for f in sample]
        joint.append(len({tuple(p) for p in parsed}))
        for i, d in enumerate(DIMS):
            marg[d].append(len({p[i] for p in parsed}))
        plen.append(statistics.mean(list_len(p[0]) for p in parsed))
        vlen.append(statistics.mean(list_len(p[1]) for p in parsed))
    return {
        "joint_distinct": statistics.mean(joint),
        "marginal_distinct": {d: statistics.mean(marg[d]) for d in DIMS},
        "mean_props_len": statistics.mean(plen),
        "mean_voids_len": statistics.mean(vlen),
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--matched-n", type=int, default=294)
    ap.add_argument("--K", type=int, default=2000)
    ap.add_argument("--seed", type=int, default=71)
    args = ap.parse_args()
    rng = random.Random(args.seed)

    depth = load_tagged(AUDIT / "depth_sixdim_tagged.tsv")
    live = load_tagged(AUDIT / "live_sixdim_tagged.tsv")
    ctl_ids = list(json.loads((AUDIT / "control_membership.json").read_text())["constraint_ids"])
    sib_c = {s["constraint_id"]: len(s["sibling_reading_ids"])
             for s in json.loads(Path("outputs/completion_seeds/never_generated_seeds.json").read_text())}
    sib_d = {s["constraint_id"]: len(s["sibling_reading_ids"])
             for s in json.loads((AUDIT / "lineage_seeds.json").read_text())}

    # Control = the 300 FROZEN ids in control_membership.json (the durable authority).
    # WITNESSED DRIFT: never_generated_seeds.json was regenerated 2026-06-13 and is
    # missing 26 of the 300, so it can no longer reproduce the audit's length-2+
    # stratum (294). The per-dim marginal-vs-joint decomposition is a structural
    # property robust to the stratum, so PRIMARY runs on the full frozen arms
    # (drift-immune); SECONDARY runs the current length-2+ stratum as a cross-check.
    ctl_full = [f for c in ctl_ids if c in live for _, f in [live[c]]]
    depth_full = [f for _, f in depth.values()]
    ctl_missing = sum(1 for c in ctl_ids if c not in sib_c)

    d5_strat = [f for c, (_, f) in depth.items() if sib_d.get(c, -1) >= 2]
    c5_strat = [f for c in ctl_ids if c in live and sib_c.get(c, -1) >= 2 for _, f in [live[c]]]

    print(f"control seed drift: {ctl_missing}/300 control ids absent from current "
          f"never_generated_seeds.json (audit stratum was 294; now {len(c5_strat)})")

    views = {
        "full_frozen": (depth_full, ctl_full),
        "len2plus_current": (d5_strat, c5_strat),
    }
    all_out = {}
    for view, (d5, c5) in views.items():
        n = min(args.matched_n, len(d5), len(c5))
        print(f"\n========== view={view}: depth n={len(d5)}, control n={len(c5)}, "
              f"matched n={n}, K={args.K} ==========")
        rd = richness(d5, n, args.K, rng)
        rc = richness(c5, n, args.K, rng)
        all_out[view] = run_view(view, d5, c5, n, rd, rc, args)

    (AUDIT / "a2_richness_alldims_results.json").write_text(
        json.dumps({"control_seed_drift_missing": ctl_missing, "views": all_out}, indent=2))
    print(f"\nwrote {AUDIT / 'a2_richness_alldims_results.json'}")


def run_view(view, d5, c5, n, rd, rc, args):

    print(f"\n{'dim':>14}  {'depth':>8}  {'control':>8}  {'excess':>8}")
    print(f"{'JOINT 5-dim':>14}  {rd['joint_distinct']:8.1f}  {rc['joint_distinct']:8.1f}"
          f"  {rd['joint_distinct'] - rc['joint_distinct']:+8.1f}")
    for d in DIMS:
        md, mc = rd["marginal_distinct"][d], rc["marginal_distinct"][d]
        print(f"{'marg ' + d:>14}  {md:8.1f}  {mc:8.1f}  {md - mc:+8.1f}")
    print(f"{'mean|props|':>14}  {rd['mean_props_len']:8.2f}  {rc['mean_props_len']:8.2f}"
          f"  {rd['mean_props_len'] - rc['mean_props_len']:+8.2f}")
    print(f"{'mean|voids|':>14}  {rd['mean_voids_len']:8.2f}  {rc['mean_voids_len']:8.2f}"
          f"  {rd['mean_voids_len'] - rc['mean_voids_len']:+8.2f}")

    # interpretation: joint excess vs the LARGEST single-dim marginal excess
    joint_excess = rd["joint_distinct"] - rc["joint_distinct"]
    marg_excess = {d: rd["marginal_distinct"][d] - rc["marginal_distinct"][d] for d in DIMS}
    max_marg = max(marg_excess.values())
    print(f"\njoint excess {joint_excess:+.1f}  vs  largest single-dim marginal excess "
          f"{max_marg:+.1f} ({max(marg_excess, key=marg_excess.get)})")
    verdict = ("new-combinations" if joint_excess > 1.5 * max(max_marg, 0.1)
               else "proliferation-not-ruled-out")
    print(f"verdict: {verdict}")

    # POSITIVE CONTROL — inflate control props with a per-story unique token;
    # mean|props| and props-marginal must jump, proving the metric catches it.
    rng2 = random.Random(args.seed)
    inflated = []
    for i, f in enumerate(c5):
        p = parse5(f)
        inner = p[0].strip()[1:-1]
        newprops = "[" + (inner + "," if inner else "") + f"planted_{i}]"
        inflated.append(newprops + "," + ",".join(p[1:]))
    rctl = richness(inflated, n, args.K, rng2)
    ctrl_ok = (rctl["mean_props_len"] > rc["mean_props_len"] + 0.9
               and rctl["marginal_distinct"]["props"] > rc["marginal_distinct"]["props"] + 50)
    print(f"\n[positive control] inflated-props arm: mean|props| {rc['mean_props_len']:.2f}"
          f" -> {rctl['mean_props_len']:.2f}; props-marginal {rc['marginal_distinct']['props']:.1f}"
          f" -> {rctl['marginal_distinct']['props']:.1f}  ==> metric flags inflation: {ctrl_ok}")

    return {
        "matched_n": n, "K": args.K, "seed": args.seed,
        "depth_n": len(d5), "control_n": len(c5),
        "depth": rd, "control": rc,
        "joint_excess": joint_excess, "marginal_excess": marg_excess,
        "largest_marginal_excess_dim": max(marg_excess, key=marg_excess.get),
        "verdict": verdict,
        "positive_control": {"metric_flags_inflation": ctrl_ok,
                             "inflated_mean_props_len": rctl["mean_props_len"],
                             "inflated_props_marginal": rctl["marginal_distinct"]["props"]},
    }


if __name__ == "__main__":
    main()
