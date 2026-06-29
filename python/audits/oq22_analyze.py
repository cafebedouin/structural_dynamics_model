#!/usr/bin/env python3
"""
OQ-22 starvation analysis — reads the Phase-0/1 census TSVs (oq22_starvation_census.py) and
derives the two SEPARATE outputs the plan mandates kept apart:

  OUTPUT 1 (analytic boundary, gate geometry): the minimum width of a single same-type χ-band,
    over the (constraint, observer) configurations ACTUALLY SWEPT — honestly scoped, NOT a floor
    over the full reachable non-χ input space (that is combinatorial; not derived here).
  OUTPUT 2 (corpus census, empirical): which constraints' observed observer-χ vector sits within a
    single same-type band of its own realized map (= Hub-1 STARVED), and among those, subset
    (a) type varies across observers (→ provably Hub-2-sourced) and (b) type fixed at a Hub-2-gated
    value (rope-via-immutability / mountain-via-immutability).

Also answers the CRITICAL kill condition (plan §"the type-band is not the config-gate partition"):
  does the cascade map the four config thresholds {0.35,0.45,0.66,0.90} to four NON-overlapping
  types? Answered from realized band boundaries + gate-ownership. If realized boundaries match the
  config partition AND every band is one config-gap wide, the simple partition is admissible and
  `starved` could use it; if bands are wider / boundaries differ, the per-constraint map is mandatory.

`starved` operational definition (Phase-1 band SCREEN; Phase 2 is the arbiter that refines it):
  Let [χ_min, χ_max] be the observed χ span across the 4 standard observers. The constraint is
  band-screen starved iff ALL 4 observers have a realized map AND, for EVERY observer, that whole
  span lies inside ONE band of THAT observer's map — i.e. holding the observer's context fixed and
  sweeping χ across the full realized range never crosses a type boundary. Then Hub 1 has no
  dynamic range to flip the type at any observer, so any cross-observer type variation is Hub-2's.

Read-only; prints a report and writes census_<corpus>.tsv + a summary json under the audit dir.

Usage:
    python3 python/audits/oq22_analyze.py                 # all corpora present
    python3 python/audits/oq22_analyze.py testsets        # one
"""

import csv
import json
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT_DIR = ROOT / "audits" / "2026-06-28_oq22_hub_starvation"

OBSERVERS = ["powerless", "moderate", "institutional", "analytical"]
GATE_VALS = [0.0, 0.35, 0.45, 0.66, 0.90]
SWEEP_LO, SWEEP_HI = -0.5, 1.6      # band-sweep endpoints (artifacts, not real boundaries)
HUB2_GATED_TYPES = {"rope", "mountain"}   # types whose gate consults effective_immutability (Hub 2)
EPS = 1e-9


def load_tsv(name, corpus):
    p = OUT_DIR / f"{name}_{corpus}.tsv"
    if not p.exists():
        return None
    return list(csv.DictReader(p.open(), delimiter="\t"))


def fnum(s):
    try:
        return float(s)
    except (TypeError, ValueError):
        return None


def analyze(corpus):
    base = load_tsv("base", corpus)
    obs = load_tsv("obs", corpus)
    bands = load_tsv("bands", corpus)
    gate = load_tsv("gateown", corpus)
    if base is None or obs is None or bands is None:
        print(f"  [skip] {corpus}: missing TSVs")
        return None

    # bands[id][obs] = sorted list of (lo, hi, type)
    bmap = defaultdict(lambda: defaultdict(list))
    for b in bands:
        bmap[b["id"]][b["obs"]].append((float(b["lo"]), float(b["hi"]), b["type"]))
    for cid in bmap:
        for o in bmap[cid]:
            bmap[cid][o].sort()

    # obs[id][obs] = {chi, immut, mtype, ftype}
    omap = defaultdict(dict)
    for r in obs:
        omap[r["id"]][r["obs"]] = r

    # ---- OUTPUT 1: min interior single-type band width over swept configs ----
    # GRID = the χ sweep step. A "band" of width <= 1 grid step is a knife-edge artifact of the
    # strict/inclusive gate seams (e.g. χ=0.35 owned by neither naturalized [χ<0.35] nor
    # tangled_rope [χ>0.35]; if rope's non-χ gates also fail it falls to a measure-zero `unknown`
    # point). Those are a real gate-GEOMETRY finding (reported separately as knife_edges) but are
    # NOT a meaningful band-width floor, so the floor is the min over NON-degenerate bands.
    GRID = 0.001
    min_w = None
    min_w_where = None
    knife_edges = []                   # (cid, obs, chi, type) measure-zero seam points
    boundary_vals = defaultdict(int)   # realized interior boundary χ -> count
    for cid in bmap:
        for o, blist in bmap[cid].items():
            for i, (lo, hi, t) in enumerate(blist):
                interior_lo = lo > SWEEP_LO + EPS
                interior_hi = hi < SWEEP_HI - EPS
                if interior_lo:
                    boundary_vals[round(lo, 3)] += 1
                if interior_lo and interior_hi:          # bounded both sides = a real type-band
                    w = hi - lo
                    if w <= GRID + EPS:                   # knife-edge seam, not a band-width floor
                        knife_edges.append((cid, o, round(lo, 3), t))
                        continue
                    if min_w is None or w < min_w:
                        min_w, min_w_where = w, (cid, o, lo, hi, t)
    knife_edge_types = defaultdict(int)
    for _c, _o, _v, t in knife_edges:
        knife_edge_types[t] += 1

    # ---- kill condition: realized boundaries vs config partition + gate ownership ----
    config_set = {0.0, 0.35, 0.45, 0.66, 0.90}
    realized_boundaries = sorted(boundary_vals)
    off_config = [v for v in realized_boundaries if min(abs(v - c) for c in config_set) > 0.0015]
    gate_ownership = defaultdict(lambda: defaultdict(int))   # gateval -> type -> count
    if gate:
        for g in gate:
            gate_ownership[float(g["gateval"])][g["type"]] += 1

    # ---- OUTPUT 2: per-constraint starvation census ----
    census = []
    for cid in sorted(omap):
        cells = omap[cid]
        chis = {o: fnum(cells.get(o, {}).get("chi")) for o in OBSERVERS}
        defined = [o for o in OBSERVERS if chis[o] is not None]
        has_full_map = all(o in bmap[cid] and bmap[cid][o] for o in OBSERVERS) and len(defined) == 4
        row = {
            "id": cid,
            "n_obs_defined": len(defined),
            "has_full_map": has_full_map,
            "chi_min": min((chis[o] for o in defined), default=None),
            "chi_max": max((chis[o] for o in defined), default=None),
            "mtype_vec": [cells.get(o, {}).get("mtype", "NA") for o in OBSERVERS],
            "ftype_vec": [cells.get(o, {}).get("ftype", "NA") for o in OBSERVERS],
            "immut_vec": [cells.get(o, {}).get("immut", "NA") for o in OBSERVERS],
        }
        row["chi_span"] = (row["chi_max"] - row["chi_min"]) if has_full_map else None

        if not has_full_map:
            row.update(starved=False, starved_reason="no_full_map",
                       subset_a=False, subset_b=False)
            census.append(row)
            continue

        cmin, cmax = row["chi_min"], row["chi_max"]
        # span ⊆ single band, for every observer's own map?
        span_in_one = {}
        for o in OBSERVERS:
            ok = any(lo - EPS <= cmin and cmax <= hi + EPS for (lo, hi, _t) in bmap[cid][o])
            span_in_one[o] = ok
        starved = all(span_in_one.values())
        row["span_in_one_band"] = span_in_one
        row["starved"] = starved

        mtypes = set(row["mtype_vec"])
        ftypes = set(row["ftype_vec"])
        row["mtype_varies"] = len(mtypes) > 1
        row["ftype_varies"] = len(ftypes) > 1
        # (a): starved AND metric type varies across observers -> the variation cannot be Hub-1's
        row["subset_a"] = starved and row["mtype_varies"]
        # (b): starved AND metric type fixed at a Hub-2-gated value across all observers
        row["subset_b"] = (starved and not row["mtype_varies"]
                           and next(iter(mtypes)) in HUB2_GATED_TYPES)
        row["starved_reason"] = "" if starved else "hub1_has_range"
        census.append(row)

    starved_rows = [r for r in census if r["starved"]]
    a_rows = [r for r in census if r.get("subset_a")]
    b_rows = [r for r in census if r.get("subset_b")]
    nomap = [r for r in census if not r["has_full_map"]]
    # contrast: NON-starved constraints whose metric-type varies (normal two-hub — Hub-1 has range)
    contrast_rows = [r for r in census if r["has_full_map"] and not r["starved"]
                     and len(set(r["mtype_vec"])) > 1]

    summary = {
        "corpus": corpus,
        "n_constraints": len(census),
        "n_no_full_map": len(nomap),
        "n_starved": len(starved_rows),
        "n_subset_a_hub2_sourced_variation": len(a_rows),
        "n_subset_b_hub2_gated_fixed": len(b_rows),
        "n_contrast_nonstarved_type_varies": len(contrast_rows),
        "min_nondegenerate_band_width_swept": min_w,
        "min_band_width_where": min_w_where,
        "n_knife_edge_seam_points": len(knife_edges),
        "knife_edge_types": dict(knife_edge_types),
        "realized_interior_boundaries": realized_boundaries,
        "off_config_boundaries": off_config,
        "gate_ownership": {str(k): dict(v) for k, v in sorted(gate_ownership.items())},
    }

    # write census tsv
    cpath = OUT_DIR / f"census_{corpus}.tsv"
    with cpath.open("w") as f:
        f.write("id\tn_obs_defined\thas_full_map\tchi_min\tchi_max\tchi_span\t"
                "starved\tsubset_a\tsubset_b\tmtype_vec\tftype_vec\timmut_vec\n")
        for r in census:
            f.write("\t".join(str(x) for x in [
                r["id"], r["n_obs_defined"], r["has_full_map"], r["chi_min"], r["chi_max"],
                r["chi_span"], r["starved"], r.get("subset_a"), r.get("subset_b"),
                "|".join(r["mtype_vec"]), "|".join(r["ftype_vec"]), "|".join(r["immut_vec"]),
            ]) + "\n")

    # report
    print(f"\n{'='*78}\nCORPUS {corpus}: {summary['n_constraints']} constraints "
          f"({summary['n_no_full_map']} without a full 4-observer map)")
    print(f"{'='*78}")
    print(f"OUTPUT 1 (gate geometry, swept configs):")
    print(f"  min NON-DEGENERATE single-type band width = {min_w}")
    if min_w_where:
        print(f"    at {min_w_where[0]} / {min_w_where[1]}: band [{min_w_where[2]},{min_w_where[3]}] = {min_w_where[4]}")
    print(f"  knife-edge seam points (measure-zero, width<=1 grid): {len(knife_edges)}  "
          f"types={dict(knife_edge_types)}")
    print(f"  realized interior boundaries: {realized_boundaries}")
    print(f"  boundaries OFF the config partition {sorted(config_set)}: {off_config}")
    print(f"  KILL CONDITION (4 thresholds -> 4 non-overlapping types?) gate ownership:")
    for g in GATE_VALS:
        own = dict(gate_ownership.get(g, {}))
        print(f"    χ={g:.2f}: {own}")
    print(f"OUTPUT 2 (empirical census):")
    print(f"  STARVED (band screen): {summary['n_starved']}")
    print(f"  subset (a) starved & metric-type varies across observers (Hub-2-sourced): "
          f"{summary['n_subset_a_hub2_sourced_variation']}")
    print(f"  subset (b) starved & fixed at Hub-2-gated type (rope/mountain): "
          f"{summary['n_subset_b_hub2_gated_fixed']}")
    print(f"  contrast: NON-starved & metric-type varies (normal two-hub, Hub-1 has range): "
          f"{summary['n_contrast_nonstarved_type_varies']}")
    if starved_rows:
        print("  --- starved constraints ---")
        for r in starved_rows[:40]:
            print(f"    {r['id']}  span={r['chi_span']:.4f}  mtypes={r['mtype_vec']}  "
                  f"a={r.get('subset_a')} b={r.get('subset_b')}")
        if len(starved_rows) > 40:
            print(f"    ... +{len(starved_rows)-40} more (see census_{corpus}.tsv)")
    print(f"  wrote census_{corpus}.tsv")
    return summary


def main(argv):
    if len(argv) > 1:
        corpora = argv[1:]
    else:
        corpora = [p.name[len("base_"):-len(".tsv")]
                   for p in sorted(OUT_DIR.glob("base_*.tsv"))]
    summaries = [s for c in corpora if (s := analyze(c)) is not None]
    (OUT_DIR / "summary.json").write_text(json.dumps(summaries, indent=2))
    print(f"\nwrote summary.json ({len(summaries)} corpora)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
