#!/usr/bin/env python3
"""Twin-model cross-classification comparison harness (Part B2).

Joins 2 twin `pipeline_output.*.json` (same engine + commit, different generating
model) over the LIVE id intersection and adjudicates the pre-registered H1/H2
falsifiers (see audits/2026-06-13_twin_comparison/PRE_REGISTRATION.md). n-agnostic:
keys on the intersection, so it re-runs unchanged as the corpora grow.

A third, disjoint corpus (the mixed essay/control regime) may be supplied via
`--essay`; it contributes per-corpus marginal tables only (labelled not-paired).

Usage:
    python3 python/audits/twin_comparison.py \
        --twin haiku=outputs/pipeline_output.haiku.json \
        --twin flash=outputs/pipeline_output.flash.json \
        [--essay mixed=outputs/pipeline_output.json] \
        --permute 1000 \
        --outdir audits/2026-06-13_twin_comparison

Validity guards (refuse-to-join): schema_version != 2; two inputs sharing
corpus_path; twins differing in code_commit.
"""
import argparse
import hashlib
import json
import math
import random
import re
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
OUTPUTS_DIR = REPO_ROOT / "outputs"

# Structural fields (H1) and continuous fields (H2). Perspective order fixed.
PERSPECTIVES = ["powerless", "moderate", "institutional", "analytical"]
STRUCTURAL_FIELDS = (
    ["verdict"] + [f"persp:{p}" for p in PERSPECTIVES] + ["signature", "claimed_type"]
)
CONTINUOUS_FIELDS = ["theater_ratio"] + [f"chi:{p}" for p in PERSPECTIVES]
H2_MIN_PAIRS = 30  # below this a continuous field's H2 ships OPEN (statistic not stood up)


# --------------------------------------------------------------------------- field access
def get_struct(entry, field):
    if field == "verdict":
        return (entry.get("verdict_join") or {}).get("verdict")
    if field.startswith("persp:"):
        return (entry.get("perspectives") or {}).get(field.split(":", 1)[1])
    return entry.get(field)  # signature, claimed_type


def get_cont(entry, field):
    if field == "theater_ratio":
        return entry.get("theater_ratio")
    if field.startswith("chi:"):
        pc = (entry.get("perspective_chi") or {}).get(field.split(":", 1)[1]) or {}
        return pc.get("chi")
    return None


def populated(v):
    """A structural field counts as populated unless it is null/absent/empty.
    NOTE: 'unknown' is a real engine outcome (honest-unknown, OQ-37) and counts as
    populated — both engines independently typing a story 'unknown' is agreement."""
    return v is not None and v != ""


def isnum(v):
    return isinstance(v, (int, float)) and not isinstance(v, bool)


# --------------------------------------------------------------------------- statistics
def wilson(k, n, z=1.96):
    """(point, lo, hi) Wilson score interval for a binomial proportion."""
    if n == 0:
        return (None, None, None)
    p = k / n
    denom = 1 + z * z / n
    centre = (p + z * z / (2 * n)) / denom
    half = (z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))) / denom
    return (p, max(0.0, centre - half), min(1.0, centre + half))


def percentile(xs, q):
    if not xs:
        return None
    s = sorted(xs)
    idx = (q / 100) * (len(s) - 1)
    lo = int(math.floor(idx)); hi = int(math.ceil(idx))
    if lo == hi:
        return s[lo]
    return s[lo] + (s[hi] - s[lo]) * (idx - lo)


# --------------------------------------------------------------------------- H1 / H2
def struct_rate(hvals, fvals):
    """agreement-rate over both-populated pairs; returns (rate, agree, n_both, one_sided)."""
    both = [(a, b) for a, b in zip(hvals, fvals) if populated(a) and populated(b)]
    one_sided = sum(1 for a, b in zip(hvals, fvals)
                    if populated(a) != populated(b))
    if not both:
        return (None, 0, 0, one_sided)
    agree = sum(1 for a, b in both if a == b)
    return (agree / len(both), agree, len(both), one_sided)


def analyse_structural(inter, haiku, flash, field, n_perm, rng):
    hvals = [get_struct(haiku[i], field) for i in inter]
    fvals = [get_struct(flash[i], field) for i in inter]
    rate, agree, n_both, one_sided = struct_rate(hvals, fvals)
    point, lo, hi = wilson(agree, n_both)
    # permutation null: shuffle flash values, recompute conditional agreement-rate
    null = []
    if n_both:
        for _ in range(n_perm):
            fs = fvals[:]
            rng.shuffle(fs)
            r, _, _, _ = struct_rate(hvals, fs)
            if r is not None:
                null.append(r)
    band95 = percentile(null, 95)
    h1 = None
    if lo is not None and band95 is not None:
        h1 = bool(lo > band95)
    # disparity exemplars (up to 5 ids where both populated and differ)
    exemplars = [
        {"id": i, "haiku": a, "flash": b}
        for i, a, b in zip(inter, hvals, fvals)
        if populated(a) and populated(b) and a != b
    ][:5]
    return {
        "field": field,
        "n_both_populated": n_both,
        "one_sided": one_sided,
        "agreement": agree,
        "disparity": n_both - agree,
        "agreement_rate": rate,
        "wilson_lo": lo, "wilson_hi": hi,
        "permute_band_95": band95,
        "n_permutations": len(null),
        "H1_holds": h1,
        "disparity_exemplars": exemplars,
    }


def analyse_continuous(inter, haiku, flash, field, n_perm, rng):
    hv = [get_cont(haiku[i], field) for i in inter]
    fv = [get_cont(flash[i], field) for i in inter]
    pairs = [(a, b) for a, b in zip(hv, fv) if isnum(a) and isnum(b)]
    if len(pairs) < H2_MIN_PAIRS:
        return {
            "field": field, "n_both_numeric": len(pairs),
            "status": "OPEN",
            "reason": f"only {len(pairs)} both-numeric pairs (< {H2_MIN_PAIRS}); "
                      f"statistic not stood up",
        }
    hnum = [a for a, b in pairs]
    fnum = [b for a, b in pairs]
    obs = sum(abs(a - b) for a, b in pairs) / len(pairs)
    null = []
    for _ in range(n_perm):
        fs = fnum[:]
        rng.shuffle(fs)
        null.append(sum(abs(a - b) for a, b in zip(hnum, fs)) / len(hnum))
    band5, band95 = percentile(null, 5), percentile(null, 95)
    if obs > band95:
        tail = "above"   # pre-registered literal: H2 holds (true pairs MORE dispersed)
    elif obs < band5:
        tail = "below"   # natural invariance reading: true pairs MORE similar than chance
    else:
        tail = "within"  # indistinguishable from chance re-pairing
    return {
        "field": field, "n_both_numeric": len(pairs),
        "status": "MEASURED",
        "observed_mean_abs_delta": obs,
        "permute_band_5": band5, "permute_band_95": band95,
        "tail": tail,
        "H2_holds_literal": bool(obs > band95),
    }


# --------------------------------------------------------------------------- marginals
def marginal_table(records, field):
    c = Counter()
    for e in records.values():
        v = get_struct(e, field)
        c[v if populated(v) else "<null>"] += 1
    return dict(c.most_common())


# --------------------------------------------------------------------------- conditioned (2026-07-04)
# Additive conditioned re-analyses for OQ-125 (Track A) and OQ-123 (Track B).
# Pre-registration: audits/2026-07-04_twin_conditioned/PRE_REGISTRATION.md —
# decision rules are read literally off that document. RNG ISOLATION: every
# analysis below draws from its own random.Random(f"{seed}:{tag}") instance and
# never touches the global rng the unconditioned path consumes.

COND_MIN_N = 30            # frozen: conditioned subset floor (Track A per-seat)
POWER_BENCHMARK = 0.672    # frozen: parent institutional-seat agreement rate
FLOAT_MARGIN = 0.10        # frozen: r_ab - r_all threshold for (a)
ASYM_MIN_N = 10            # frozen: asymmetry probes report-only below this

_VIC_RE = re.compile(r"^narrative_ontology:constraint_victim\(", re.M)
_MET_RE = re.compile(
    r"^narrative_ontology:constraint_metric\(\s*[a-z0-9_]+\s*,\s*"
    r"(extractiveness|suppression_requirement)\s*,\s*([0-9.eE+-]+)\s*\)\.", re.M)


def idset_hash(ids):
    return hashlib.sha256(",".join(sorted(ids)).encode()).hexdigest()[:16]


def source_victim_status(corpus_dir):
    """Per-id status from the source .pl (sources witnessed sentinel-free):
    authored | imputed (data_repair bridge gate E>0.46 AND S>0.40) | absent."""
    out = {}
    for p in sorted(Path(corpus_dir).glob("*.pl")):
        txt = p.read_text(encoding="utf-8")
        if _VIC_RE.search(txt):
            out[p.stem] = "authored"
            continue
        mets = {m.group(1): float(m.group(2)) for m in _MET_RE.finditer(txt)}
        eligible = mets.get("extractiveness", 0) > 0.46 and \
            mets.get("suppression_requirement", 0) > 0.40
        out[p.stem] = "imputed" if eligible else "absent"
    return out


def _delta_stats(hnum, fnum, n_perm, rng):
    obs = sum(abs(a - b) for a, b in zip(hnum, fnum)) / len(hnum)
    null = []
    for _ in range(n_perm):
        fs = fnum[:]
        rng.shuffle(fs)
        null.append(sum(abs(a - b) for a, b in zip(hnum, fs)) / len(hnum))
    return obs, percentile(null, 5), percentile(null, 95)


def track_a(inter, ha, hb, n_perm, seed):
    """OQ-125: per typed χ seat, condition BOTH observed and band on the
    same-side (type-agreeing) id set; disagreeing subset = reach control."""
    seats = []
    for seat in PERSPECTIVES:
        rows = []
        for i in inter:
            th, tf = get_struct(ha[i], f"persp:{seat}"), get_struct(hb[i], f"persp:{seat}")
            ch, cf = get_cont(ha[i], f"chi:{seat}"), get_cont(hb[i], f"chi:{seat}")
            if populated(th) and populated(tf) and isnum(ch) and isnum(cf):
                rows.append((i, th == tf, ch, cf))
        same = [(i, ch, cf) for i, eq, ch, cf in rows if eq]
        diff = [(i, ch, cf) for i, eq, ch, cf in rows if not eq]
        rec = {"field": f"chi:{seat}", "n_typed_numeric_pairs": len(rows),
               "same_side": {"n": len(same), "idset_sha256_16": idset_hash([i for i, _, _ in same])},
               "disagreeing_control": {"n": len(diff),
                                       "idset_sha256_16": idset_hash([i for i, _, _ in diff])}}
        if len(same) < COND_MIN_N:
            rec["status"] = "OPEN"
            rec["reason"] = f"same-side n={len(same)} < {COND_MIN_N}"
            seats.append(rec)
            continue
        rng_c = random.Random(f"{seed}:condA:{seat}")
        obs, b5, b95 = _delta_stats([c for _, c, _ in same], [f for _, _, f in same],
                                    n_perm, rng_c)
        rec["status"] = "MEASURED"
        rec["same_side"].update({"observed_mean_abs_delta": obs,
                                 "permute_band_5": b5, "permute_band_95": b95})
        if obs < b5:
            verdict = "VALUE_INVARIANCE_BEYOND_H1"
        elif obs > b95:
            verdict = "ANOMALOUS_ABOVE"
        else:
            verdict = "H1_ENTAILED_COLOCATION"
        rec["verdict"] = verdict
        # reach control on the disagreeing subset
        ctl = rec["disagreeing_control"]
        if len(diff) >= 2:
            rng_d = random.Random(f"{seed}:condA_ctl:{seat}")
            obs_d, b5_d, b95_d = _delta_stats([c for _, c, _ in diff],
                                              [f for _, _, f in diff], n_perm, rng_d)
            ctl.update({"observed_mean_abs_delta": obs_d,
                        "permute_band_5": b5_d, "permute_band_95": b95_d,
                        "band_width": b95_d - b5_d,
                        "observed_below_band": bool(obs_d < b5_d)})
            # frozen reach criterion: conditioned same-side observed (the claimed
            # effect) would register below-band inside the disagreeing band
            ctl["reach_ok"] = bool(obs < b5_d)
            ctl["reading"] = ("informative" if ctl["reach_ok"]
                              else "INERT — band too wide to admit the conditioned "
                                   "effect; says nothing (not a pass)")
        else:
            ctl["reading"] = "VACUOUS — <2 disagreeing pairs"
        seats.append(rec)
    return seats


def _cell_agreement(ids, ha, hb, field, n_perm, rng):
    hv = [get_struct(ha[i], field) for i in ids]
    fv = [get_struct(hb[i], field) for i in ids]
    rate, agree, n_both, one_sided = struct_rate(hv, fv)
    point, lo, hi = wilson(agree, n_both)
    null = []
    if n_both >= 2:
        for _ in range(n_perm):
            fs = fv[:]
            rng.shuffle(fs)
            r, _, _, _ = struct_rate(hv, fs)
            if r is not None:
                null.append(r)
    return {"n_ids": len(ids), "n_both_populated": n_both, "agreement": agree,
            "agreement_rate": rate, "wilson_lo": lo, "wilson_hi": hi,
            "permute_band_5": percentile(null, 5),
            "permute_band_95": percentile(null, 95),
            "idset_sha256_16": idset_hash(ids)}


def _asymmetry(n_first, n_second, label_first, label_second):
    n = n_first + n_second
    rec = {label_first: n_first, label_second: n_second, "n": n}
    if n == 0:
        rec["reading"] = "VACUOUS — no pairs"
        return rec
    k = max(n_first, n_second)
    _, lo, _ = wilson(k, n)
    rec["larger_share_wilson_lo"] = lo
    if n < ASYM_MIN_N:
        rec["reading"] = f"REPORT-ONLY — n={n} < {ASYM_MIN_N}"
    elif lo is not None and lo > 0.5:
        direction = label_first if n_first >= n_second else label_second
        rec["reading"] = f"DIRECTIONAL ({direction}) — (c1)-signature"
    else:
        rec["reading"] = "symmetric — (a)-like"
    return rec


def track_b(inter, ha, hb, status_a, status_b, n_perm, seed, la, lb):
    """OQ-123: three-cell authored/imputed partition of persp:powerless
    agreement, decision from authored-both alone with frozen power floor."""
    missing = [i for i in inter if i not in status_a or i not in status_b]
    if missing:
        raise SystemExit(f"REFUSE: {len(missing)} matched ids missing from source "
                         f"corpora (first: {missing[:3]})")
    cells = {}
    for i in inter:
        cells.setdefault((status_a[i], status_b[i]), []).append(i)

    named = {
        "authored_both": cells.get(("authored", "authored"), []),
        "imputed_both": cells.get(("imputed", "imputed"), []),
        "imputed_one": cells.get(("authored", "imputed"), [])
        + cells.get(("imputed", "authored"), []),
    }
    residual_keys = [k for k in cells
                     if "absent" in k]
    result = {"partition_counts": {f"{a}({la})×{b}({lb})": len(v)
                                   for (a, b), v in sorted(cells.items())},
              "marginals": {la: dict(Counter(status_a[i] for i in inter)),
                            lb: dict(Counter(status_b[i] for i in inter))}}

    field = "persp:powerless"
    cell_stats = {}
    for name, ids in named.items():
        if not ids:
            cell_stats[name] = {"n_ids": 0, "reading": "VACUOUS — empty cell"}
            continue
        rng_c = random.Random(f"{seed}:condB:{name}")
        cell_stats[name] = _cell_agreement(ids, ha, hb, field, n_perm, rng_c)
    for key in residual_keys:
        ids = cells[key]
        rng_c = random.Random(f"{seed}:condB:residual:{key[0]}x{key[1]}")
        stat = _cell_agreement(ids, ha, hb, field, n_perm, rng_c)
        stat["role"] = "residual (reported only; no decision rule)"
        cell_stats[f"residual:{key[0]}({la})×{key[1]}({lb})"] = stat
    result["cells_powerless"] = cell_stats

    # unconditioned rate over ALL matched pairs (r_all), same run
    rng_all = random.Random(f"{seed}:condB:all")
    all_stat = _cell_agreement(list(inter), ha, hb, field, n_perm, rng_all)
    result["unconditioned_powerless"] = all_stat
    r_all = all_stat["agreement_rate"]

    # decision — authored-both alone, frozen power floor + rules
    ab = cell_stats["authored_both"]
    decision = {"cell": "authored_both"}
    if ab.get("n_both_populated", 0) == 0:
        decision["outcome"] = "OPEN (empty decision cell)"
    else:
        n_ab, b95, b5 = ab["n_both_populated"], ab["permute_band_95"], ab["permute_band_5"]
        k_bench = round(POWER_BENCHMARK * n_ab)
        _, bench_lo, _ = wilson(k_bench, n_ab)
        powered = bench_lo is not None and b95 is not None and bench_lo > b95
        decision.update({"power_benchmark": POWER_BENCHMARK,
                         "benchmark_wilson_lo": bench_lo, "band95": b95,
                         "powered": bool(powered)})
        if not powered:
            decision["outcome"] = ("OPEN (underpowered) — benchmark effect would not "
                                   f"register at n={n_ab}; B4 NOT armed")
        else:
            r_ab, lo, hi = ab["agreement_rate"], ab["wilson_lo"], ab["wilson_hi"]
            decision["r_ab_minus_r_all"] = r_ab - r_all
            if lo > b95 and (r_ab - r_all) >= FLOAT_MARGIN:
                decision["outcome"] = "(a) FLOAT/DRAG — closure as (a)/(c1); B4 not armed"
            elif lo > b95:
                decision["outcome"] = ("PERSISTENT DIVERGENCE — (a) imputation-drag "
                                       "REFUTED; (b)-or-(c2) LIVE; B4 not auto-armed "
                                       "(operator discretion)")
            elif hi < b5:
                decision["outcome"] = "LOW — (b)-or-(c2) LIVE; ARMS B4 (disambiguation)"
            else:
                decision["outcome"] = ("CHANCE-LEVEL — reads against (a); "
                                       "(b)-or-(c2)-leaning; B4 not armed")
    result["decision"] = decision

    # asymmetry probes (c1)
    n_ai = len(cells.get(("authored", "imputed"), []))   # la authored, lb imputed = lb omits
    n_ia = len(cells.get(("imputed", "authored"), []))   # la omits
    result["asymmetry_primary_imputed_one"] = _asymmetry(
        n_ia, n_ai, f"{la}-omits(imputed)", f"{lb}-omits(imputed)")
    la_omits = sum(len(v) for (a, b), v in cells.items()
                   if a != "authored" and b == "authored")
    lb_omits = sum(len(v) for (a, b), v in cells.items()
                   if a == "authored" and b != "authored")
    result["asymmetry_secondary_omission"] = _asymmetry(
        la_omits, lb_omits, f"{la}-omits(any)", f"{lb}-omits(any)")

    # seat-gradient bootstrap (all matched pairs, all four seats)
    rng_bs = random.Random(f"{seed}:condB:bootstrap")
    boots = {p: [] for p in PERSPECTIVES}
    vals = {p: [(get_struct(ha[i], f"persp:{p}"), get_struct(hb[i], f"persp:{p}"))
                for i in inter] for p in PERSPECTIVES}
    n_inter = len(inter)
    for _ in range(1000):
        idx = [rng_bs.randrange(n_inter) for _ in range(n_inter)]
        for p in PERSPECTIVES:
            both = [(a, b) for a, b in (vals[p][j] for j in idx)
                    if populated(a) and populated(b)]
            if both:
                boots[p].append(sum(1 for a, b in both if a == b) / len(both))
    grad = {p: {"rate_ci_2_5": percentile(boots[p], 2.5),
                "rate_ci_97_5": percentile(boots[p], 97.5)} for p in PERSPECTIVES}
    others_lo = min(grad[p]["rate_ci_2_5"] for p in PERSPECTIVES if p != "powerless")
    grad["gradient_robust"] = bool(grad["powerless"]["rate_ci_97_5"] < others_lo)
    result["seat_gradient_bootstrap"] = grad
    return result


def sonnet_control(control_dir, status_a, status_b, la, lb):
    """§B controls (i)+(ii): per-corpus classifier slices + frozen authoring-LEVEL
    comparison. NON-BLIND (marginals seen at recon; disclosed in pre-reg)."""
    st = source_victim_status(control_dir)
    counts = dict(Counter(st.values()))
    n_c = len(st)
    k_c = counts.get("authored", 0)
    rate_c, lo_c, hi_c = wilson(k_c, n_c)
    rows = {"control_dir": str(control_dir), "n": n_c, "slices": counts,
            "classifier_control_i": {
                "authored_slice_populated": k_c > 0,
                "imputed_slice_populated": counts.get("imputed", 0) > 0,
                "reading": ("one-sided/THIN — imputed slice "
                            f"n={counts.get('imputed', 0)} < 10"
                            if counts.get("imputed", 0) < 10 else "both slices populated"),
                "scope": "does NOT control the pair-crossing (three-cell) logic"}}
    twin_rates = {}
    for label, status in ((la, status_a), (lb, status_b)):
        n_t = len(status)
        k_t = sum(1 for s in status.values() if s == "authored")
        r, lo, hi = wilson(k_t, n_t)
        twin_rates[label] = {"authored": k_t, "n": n_t, "rate": r,
                             "wilson_lo": lo, "wilson_hi": hi}
    pooled = (sum(v["authored"] for v in twin_rates.values())
              / sum(v["n"] for v in twin_rates.values()))
    max_label = max(twin_rates, key=lambda k: twin_rates[k]["rate"])
    max_rate = twin_rates[max_label]["rate"]
    disjoint_above = lo_c is not None and lo_c > twin_rates[max_label]["wilson_hi"]
    if (rate_c - max_rate) >= 0.20 and disjoint_above:
        verdict = "(c1) — twins under-author powerless victims vs sonnet"
    elif abs(rate_c - pooled) < 0.10:
        verdict = "(a)-supporting — authoring level model-general"
    else:
        verdict = "INDETERMINATE"
    rows["authoring_level_ii"] = {
        "sonnet_rate": rate_c, "sonnet_wilson": [lo_c, hi_c],
        "twin_rates": twin_rates, "pooled_twin_rate": pooled,
        "verdict": verdict,
        "blind": False,
        "note": "non-blind (marginals observed at recon; disclosed in pre-reg); "
                "says NOTHING about (c2)"}
    return rows


# --------------------------------------------------------------------------- guards / load
def load_input(spec):
    """spec = 'label=path'; path relative to repo root, OUTPUTS_DIR, or absolute."""
    label, _, path = spec.partition("=")
    if not path:
        raise SystemExit(f"--twin/--essay must be label=path, got {spec!r}")
    p = Path(path)
    for cand in (p, REPO_ROOT / path, OUTPUTS_DIR / path):
        if cand.exists():
            data = json.loads(cand.read_text(encoding="utf-8"))
            return label, cand, data
    raise SystemExit(f"input not found: {path}")


def index_by_id(data):
    return {e["id"]: e for e in data.get("per_constraint", [])}


def manifest_of(data, label):
    m = data.get("manifest") or {}
    if m.get("schema_version") != 2:
        raise SystemExit(f"REFUSE: input {label!r} schema_version="
                         f"{m.get('schema_version')} != 2")
    return m


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--twin", action="append", required=True,
                    help="label=path (exactly two)")
    ap.add_argument("--essay", action="append", default=[],
                    help="label=path (disjoint distribution corpus; marginals only)")
    ap.add_argument("--permute", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=20260613)
    ap.add_argument("--outdir", default="audits/2026-06-13_twin_comparison")
    ap.add_argument("--conditioned-outdir", default=None,
                    help="if set, run the 2026-07-04 conditioned analyses "
                         "(OQ-125 Track A + OQ-123 Track B) into this dir")
    ap.add_argument("--source", action="append", default=[],
                    help="label=corpus_dir of a twin's source .pl files "
                         "(required for Track B; labels must match --twin)")
    ap.add_argument("--control", default=None,
                    help="label=corpus_dir of the unpaired third-model control")
    args = ap.parse_args()

    if len(args.twin) != 2:
        raise SystemExit(f"REFUSE: need exactly two --twin inputs, got {len(args.twin)}")
    if args.permute < 1000:
        raise SystemExit(f"REFUSE: --permute {args.permute} < 1000 (pre-registered N>=1000)")
    rng = random.Random(args.seed)

    twins = [load_input(s) for s in args.twin]
    (la, pa, da), (lb, pb, db) = twins
    ma, mb = manifest_of(da, la), manifest_of(db, lb)

    # refuse-to-join guards
    cpa, cpb = ma.get("corpus_path"), mb.get("corpus_path")
    if cpa is not None and cpa == cpb:
        raise SystemExit(f"REFUSE: both twins share corpus_path={cpa!r} (corpus vs itself)")
    if ma.get("code_commit") != mb.get("code_commit"):
        raise SystemExit(
            f"REFUSE: twins differ in code_commit ({ma.get('code_commit_short')} vs "
            f"{mb.get('code_commit_short')}) — model-difference would alias onto "
            f"code-difference. Re-classify both at one commit.")

    ha, hb = index_by_id(da), index_by_id(db)
    inter = sorted(set(ha) & set(hb))
    if not inter:
        raise SystemExit("REFUSE: empty id intersection")

    structural = [analyse_structural(inter, ha, hb, f, args.permute, rng)
                  for f in STRUCTURAL_FIELDS]
    continuous = [analyse_continuous(inter, ha, hb, f, args.permute, rng)
                  for f in CONTINUOUS_FIELDS]

    # essay / distribution corpora — marginals only
    essays = []
    for spec in args.essay:
        le, pe, de = load_input(spec)
        me = manifest_of(de, le)
        recs = index_by_id(de)
        essays.append({
            "label": le, "path": str(pe), "n": len(recs),
            "manifest": {k: me.get(k) for k in
                         ("code_commit_short", "n_constraints", "corpus_path")},
            "marginals": {f: marginal_table(recs, f) for f in STRUCTURAL_FIELDS},
        })

    result = {
        "pre_registration": "audits/2026-06-13_twin_comparison/PRE_REGISTRATION.md",
        "permutations": args.permute, "seed": args.seed,
        "twins": [
            {"label": la, "path": str(pa),
             "manifest": {k: ma.get(k) for k in
                          ("code_commit_short", "n_constraints", "corpus_path")}},
            {"label": lb, "path": str(pb),
             "manifest": {k: mb.get(k) for k in
                          ("code_commit_short", "n_constraints", "corpus_path")}},
        ],
        "intersection_n": len(inter),
        "structural_H1": structural,
        "continuous_H2": continuous,
        "essay_marginals": essays,
        "notes": [
            "verdict_join.verdict is the only headline verdict (OQ-98).",
            "verdict & perspectives are CORRELATED, not independent confirmations "
            "(verdict folds perspectives via compute_verdict/4).",
            "signature agreement is STRUCTURAL-coding, not detection (OQ-70).",
            "Per-field adjudication only; no aggregate H1 claim.",
        ],
    }

    outdir = REPO_ROOT / args.outdir
    outdir.mkdir(parents=True, exist_ok=True)
    (outdir / "twin_comparison.json").write_text(
        json.dumps(result, indent=2, ensure_ascii=False), encoding="utf-8")
    write_results_md(outdir / "RESULTS.md", result, la, lb)
    print(f"wrote {outdir/'twin_comparison.json'} and RESULTS.md "
          f"(intersection n={len(inter)})")

    # ------------------------------------------------------------ conditioned
    if args.conditioned_outdir:
        sources = dict(s.partition("=")[::2] for s in args.source)
        for lbl in (la, lb):
            if lbl not in sources:
                raise SystemExit(f"REFUSE: --conditioned-outdir needs --source "
                                 f"{lbl}=<corpus_dir>")
        status_a = source_victim_status(REPO_ROOT / sources[la])
        status_b = source_victim_status(REPO_ROOT / sources[lb])
        cond = {
            "pre_registration":
                "audits/2026-07-04_twin_conditioned/PRE_REGISTRATION.md",
            "permutations": args.permute, "seed": args.seed,
            "rng_isolation": "per-analysis random.Random(f'{seed}:{tag}')",
            "inputs": [{"label": l, "path": str(p),
                        "sha256_16": hashlib.sha256(p.read_bytes()).hexdigest()[:16]}
                       for l, p in ((la, pa), (lb, pb))],
            "intersection_n": len(inter),
            "track_a_oq125": track_a(inter, ha, hb, args.permute, args.seed),
            "track_b_oq123": track_b(inter, ha, hb, status_a, status_b,
                                     args.permute, args.seed, la, lb),
        }
        # internal consistency: Track B's unconditioned rate must equal the
        # unconditioned structural persp:powerless rate from this same run
        s_pow = next(s for s in structural if s["field"] == "persp:powerless")
        r_all = cond["track_b_oq123"]["unconditioned_powerless"]["agreement_rate"]
        if s_pow["agreement_rate"] != r_all:
            raise SystemExit("INTERNAL: Track B r_all != unconditioned "
                             "persp:powerless rate")
        cond["r_all_consistency"] = "equal to unconditioned persp:powerless rate"
        if args.control:
            lc, _, dc = args.control.partition("=")
            cond["sonnet_control"] = sonnet_control(REPO_ROOT / dc, status_a,
                                                    status_b, la, lb)
            cond["sonnet_control"]["label"] = lc
        cdir = REPO_ROOT / args.conditioned_outdir
        cdir.mkdir(parents=True, exist_ok=True)
        (cdir / "twin_conditioned.json").write_text(
            json.dumps(cond, indent=2, ensure_ascii=False), encoding="utf-8")
        write_conditioned_md(cdir / "RESULTS_CONDITIONED.md", cond, la, lb)
        print(f"wrote {cdir/'twin_conditioned.json'} and RESULTS_CONDITIONED.md")


def write_results_md(path, r, la, lb):
    L = []
    L.append("# Twin-model cross-classification — RESULTS\n")
    L.append(f"Pre-registration: `{r['pre_registration']}` (committed before this run).\n")
    L.append("## Inputs\n")
    L.append("| label | commit | n_constraints | corpus_path |")
    L.append("|---|---|---|---|")
    for t in r["twins"]:
        m = t["manifest"]
        L.append(f"| {t['label']} | {m['code_commit_short']} | {m['n_constraints']} "
                 f"| {m.get('corpus_path')} |")
    L.append(f"\n**Matched intersection: n = {r['intersection_n']}** "
             f"(both twins classified at commit "
             f"{r['twins'][0]['manifest']['code_commit_short']}).\n")
    L.append(f"Permutations: {r['permutations']}; seed {r['seed']}.\n")

    L.append("## H1 — structural type model-invariance (per-field; NOT aggregated)\n")
    L.append("agreement-rate over both-populated pairs; H1 holds iff Wilson-95% lower "
             "bound > permute band (95th pct). verdict & perspectives are CORRELATED "
             "(verdict folds perspectives) — not independent confirmations.\n")
    L.append("| field | both-pop | agree | disp | one-sided | rate | Wilson-95 lo | "
             "band95 | H1 |")
    L.append("|---|---|---|---|---|---|---|---|---|")
    for s in r["structural_H1"]:
        rate = "—" if s["agreement_rate"] is None else f"{s['agreement_rate']:.3f}"
        lo = "—" if s["wilson_lo"] is None else f"{s['wilson_lo']:.3f}"
        b95 = "—" if s["permute_band_95"] is None else f"{s['permute_band_95']:.3f}"
        h1 = {True: "HOLDS", False: "FALSIFIED", None: "n/a"}[s["H1_holds"]]
        L.append(f"| `{s['field']}` | {s['n_both_populated']} | {s['agreement']} | "
                 f"{s['disparity']} | {s['one_sided']} | {rate} | {lo} | {b95} | {h1} |")

    L.append("\n### Disparity exemplars (both populated, values differ)\n")
    for s in r["structural_H1"]:
        if s["disparity_exemplars"]:
            L.append(f"- `{s['field']}`:")
            for ex in s["disparity_exemplars"]:
                L.append(f"  - `{ex['id']}`: {la}={ex['haiku']!r} vs {lb}={ex['flash']!r}")

    L.append("\n## H2 — continuous drift (per-field)\n")
    L.append("observed mean|Δ| (haiku−flash, paired) vs permuted-Δ band; pre-registered "
             "literal: H2 holds iff observed > band95 (true pairs MORE dispersed than "
             "chance). 'below' = natural invariance tail (more similar than chance).\n")
    L.append("| field | both-numeric | obs mean\\|Δ\\| | band5 | band95 | tail | status |")
    L.append("|---|---|---|---|---|---|---|")
    for c in r["continuous_H2"]:
        if c["status"] == "OPEN":
            L.append(f"| `{c['field']}` | {c['n_both_numeric']} | — | — | — | — | OPEN |")
        else:
            L.append(f"| `{c['field']}` | {c['n_both_numeric']} | "
                     f"{c['observed_mean_abs_delta']:.4f} | {c['permute_band_5']:.4f} | "
                     f"{c['permute_band_95']:.4f} | {c['tail']} | "
                     f"{'H2' if c['H2_holds_literal'] else 'no'} |")

    if r["essay_marginals"]:
        L.append("\n## Essay / distribution corpus (disjoint — marginals only, NOT paired)\n")
        for e in r["essay_marginals"]:
            L.append(f"### {e['label']} (n={e['n']}, commit "
                     f"{e['manifest']['code_commit_short']})\n")
            for f in ["verdict", "signature"]:
                L.append(f"- `{f}`: {e['marginals'][f]}")

    L.append("\n## Validity notes\n")
    for n in r["notes"]:
        L.append(f"- {n}")
    path.write_text("\n".join(L) + "\n", encoding="utf-8")


def _f(v, nd=4):
    return "—" if v is None else f"{v:.{nd}f}"


def write_conditioned_md(path, c, la, lb):
    L = []
    L.append("# Twin conditioned re-analyses — RESULTS (OQ-125 / OQ-123)\n")
    L.append(f"Pre-registration: `{c['pre_registration']}` (committed before this run).")
    L.append(f"Inputs: " + "; ".join(f"{i['label']} `{i['path']}` "
                                     f"sha256 `{i['sha256_16']}…`" for i in c["inputs"]))
    L.append(f"Intersection n = {c['intersection_n']}; permutations "
             f"{c['permutations']}; seed {c['seed']}; {c['rng_isolation']}.\n")

    L.append("## Track A — OQ-125 conditioned |Δχ| (same-side = same seat type)\n")
    L.append("Observed AND band computed from the SAME same-side id set (stamped).\n")
    L.append("| field | typed pairs | same-side n | idset | obs mean\\|Δχ\\| | band5 | "
             "band95 | verdict |")
    L.append("|---|---|---|---|---|---|---|---|")
    for s in c["track_a_oq125"]:
        ss = s["same_side"]
        if s.get("status") == "OPEN":
            L.append(f"| `{s['field']}` | {s['n_typed_numeric_pairs']} | {ss['n']} | "
                     f"`{ss['idset_sha256_16']}` | — | — | — | OPEN ({s['reason']}) |")
        else:
            L.append(f"| `{s['field']}` | {s['n_typed_numeric_pairs']} | {ss['n']} | "
                     f"`{ss['idset_sha256_16']}` | {_f(ss['observed_mean_abs_delta'])} | "
                     f"{_f(ss['permute_band_5'])} | {_f(ss['permute_band_95'])} | "
                     f"{s['verdict']} |")
    L.append("\n### Disagreeing-subset reach control\n")
    L.append("| field | n | obs | band5 | band95 | below-band | reach | reading |")
    L.append("|---|---|---|---|---|---|---|---|")
    for s in c["track_a_oq125"]:
        d = s["disagreeing_control"]
        L.append(f"| `{s['field']}` | {d['n']} | "
                 f"{_f(d.get('observed_mean_abs_delta'))} | "
                 f"{_f(d.get('permute_band_5'))} | {_f(d.get('permute_band_95'))} | "
                 f"{d.get('observed_below_band', '—')} | {d.get('reach_ok', '—')} | "
                 f"{d['reading']} |")

    b = c["track_b_oq123"]
    L.append("\n## Track B — OQ-123 partition (persp:powerless)\n")
    L.append("Partition counts: " + ", ".join(f"{k}={v}" for k, v in
                                              b["partition_counts"].items()))
    L.append(f"\nMarginals: {b['marginals']}\n")
    L.append("| cell | n ids | both-pop | agree | rate | Wilson lo | Wilson hi | "
             "band5 | band95 |")
    L.append("|---|---|---|---|---|---|---|---|---|")
    rows = list(b["cells_powerless"].items()) + [
        ("UNCONDITIONED (all matched)", b["unconditioned_powerless"])]
    for name, s in rows:
        if s.get("n_ids", 0) == 0 or "agreement_rate" not in s:
            L.append(f"| {name} | {s.get('n_ids', 0)} | — | — | — | — | — | — | — |"
                     f" <!-- {s.get('reading', '')} -->")
            continue
        L.append(f"| {name} | {s['n_ids']} | {s['n_both_populated']} | "
                 f"{s['agreement']} | {_f(s['agreement_rate'])} | "
                 f"{_f(s['wilson_lo'])} | {_f(s['wilson_hi'])} | "
                 f"{_f(s['permute_band_5'])} | {_f(s['permute_band_95'])} |")
    L.append("\n### Decision (authored-both alone; frozen rules)\n")
    for k, v in b["decision"].items():
        L.append(f"- {k}: {v}")
    L.append("\n### Asymmetry probes ((c1))\n")
    L.append(f"- primary (imputed-one): {b['asymmetry_primary_imputed_one']}")
    L.append(f"- secondary (any omission): {b['asymmetry_secondary_omission']}")
    L.append("\n### Seat-gradient bootstrap (95% percentile CIs)\n")
    g = b["seat_gradient_bootstrap"]
    for p in PERSPECTIVES:
        L.append(f"- {p}: [{_f(g[p]['rate_ci_2_5'])}, {_f(g[p]['rate_ci_97_5'])}]")
    L.append(f"- gradient robust (powerless hi < min other lo): "
             f"{g['gradient_robust']}")

    if "sonnet_control" in c:
        sc = c["sonnet_control"]
        L.append("\n## Sonnet control (`" + sc["control_dir"] + "`, unpaired)\n")
        L.append(f"- slices: {sc['slices']} (n={sc['n']})")
        L.append(f"- (i) classifier control: {sc['classifier_control_i']}")
        ii = sc["authoring_level_ii"]
        L.append(f"- (ii) authoring level: sonnet {_f(ii['sonnet_rate'], 3)} "
                 f"vs twins " + ", ".join(
                     f"{k}={_f(v['rate'], 3)}" for k, v in ii["twin_rates"].items())
                 + f" (pooled {_f(ii['pooled_twin_rate'], 3)}) → **{ii['verdict']}**")
        L.append(f"  - {ii['note']}")

    L.append("\n## Scope\n")
    L.append("- OQ-125 headline covers the 4 TYPED χ fields; `theater_ratio` stays "
             "exploratory (no seat type to condition on).")
    L.append("- One twin pair earns 'model-sensitive/invariant HERE' (haiku vs flash, "
             "this corpus, this commit) — never 'in general'.")
    path.write_text("\n".join(L) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
