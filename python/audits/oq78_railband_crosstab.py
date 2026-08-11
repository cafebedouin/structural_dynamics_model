#!/usr/bin/env python3
"""
OQ-78 idiom-half close — rail ∩ band cross-tab instrument (zero-spend, pre-registered).

Answers the two pinned conditions over the (claimed_type × ε) joint:

  (i)  RAIL     — does the .x8 last-digit rail persist WITHIN the claimed_type bands
                  for the test model, at a concentration above a pre-committed
                  effect-size floor?
  (ii) BANDING  — do the test model's per-claimed_type ε quantile bands stay
                  near-separable at a threshold bootstrap-calibrated on the archive?

PHASE GATE (load-bearing).  `--phase calibration` refuses to read any sonnet-5 datum:
the test stratum is filtered out at load, so the calibration/power pass structurally
cannot see the (claimed_type × ε) joint it is meant to be blind to.  `--phase test`
lifts the filter and requires a frozen prereg md5 on the command line, so a test read
cannot happen before the freeze.

STATISTICS

  PRIMARY (paired, operator ruling 2026-08-10).  The twin legs are one seed set
  re-authored per model (957 four-way matched ids), so topic and claimed_type mix are
  held fixed by construction.  Over the matched set:

      tv_model_digit = mean over models of TV( P_m(last digit) , P_pooled(last digit) )

  Under a per-model digit habit this is large; under model-label permutation within
  each quadruple it collapses to sampling noise.  That permutation is the paired
  known-negative, and the paired floor is calibrated against it — NOT inherited from
  the unpaired floor (paired and unpaired nulls do not behave alike).

  SECONDARY (unpaired, pre-committed).  Per stratum × claimed_type localization over
  the band grid: argmax last digit + concentration = (share at argmax − 1/|digit
  support|), an effect size applied identically at every n, never an α.

  CONFLICT RULE, pinned before any test datum is visible: where paired and unpaired
  disagree, THE PAIRED RESULT GOVERNS the verdict and the disagreement is a headline
  WRITEUP finding — never arbitrated once both are visible.

NULL CONSTRUCTION.  The band grid is the literal set of distinct ε values observed in
the CALIBRATION strata + the archive only.  The test stratum is excluded from null
construction; test mass outside the grid is reported as `off_grid_share` and counts as
non-rail.

Usage:
    python3 python/audits/oq78_railband_crosstab.py --phase calibration
    python3 python/audits/oq78_railband_crosstab.py --phase test --prereg-md5 <md5>
"""

import argparse
import hashlib
import json
import random
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "python"))
from epsilon_authorship_readout import stratum_of, last_digit  # noqa: E402

OUTPUTS = ROOT / "outputs"
ARCHIVE = ROOT / "prolog" / "archives" / "datasets" / "kernel_v2_test2" / "json"

TEST_MODEL = "claude-sonnet-5"
MIN_TYPE_N = 5                 # minimum per-type cell for BANDING; exclusions listed
# Minimum per-type cell for LOCALIZATION (operator ruling 2026-08-10). The uniform-digit
# null concentration is p50=0.300 at n=5 and p50=0.200 at n=10 — a floor of 0.25 sits
# BELOW the null median there, so small cells fire on noise. At n>=50 the null p99 is
# <=0.136 and the floor clears it. Cells below this are EXCLUDED and listed, never scored.
MIN_LOCALIZATION_N = 50
RNG_SEED = 20260810            # fixed: the synthetics are reproducible
N_PERM = 2000                  # permutation replicates for the known-negatives
N_BOOT = 2000                  # bootstrap replicates for the separability threshold

# The four twin legs, with the model each MUST fingerprint as (verified by
# classify_corpus's own refusal at generation time; re-asserted here from the data).
LEGS = [
    ("haiku",  "oq78_leg_haiku.json",  "claude-haiku-4-5"),
    ("flash",  "oq78_leg_flash.json",  "gemini-2.5-flash"),
    ("kimi",   "oq78_leg_kimi.json",   "kimi-k2.6"),
    ("sonnet", "oq78_leg_sonnet.json", "claude-sonnet-5"),
]
CALIBRATION_LEGS = ["haiku", "flash", "kimi"]   # sonnet is the TEST side


# ---------------------------------------------------------------- loading

class Story:
    __slots__ = ("cid", "eps", "ctype", "author", "kind", "source")

    def __init__(self, cid, eps, ctype, author, kind, source):
        self.cid, self.eps, self.ctype = cid, eps, ctype
        self.author, self.kind, self.source = author, kind, source

    @property
    def digit(self):
        return last_digit(self.eps)


def _per_constraint(path):
    doc = json.loads(Path(path).read_text(encoding="utf-8"))
    per = doc["per_constraint"]
    return (list(per.values()) if isinstance(per, dict) else per), doc.get("manifest", {})


def load_leg(name, filename, source_tag):
    """Load a classify_corpus leg output; stratum/author via the imported stratum_of."""
    path = OUTPUTS / filename
    if not path.exists():
        raise SystemExit(f"missing leg output {path} — run the classify_corpus pass first")
    per, manifest = _per_constraint(path)
    out = []
    for e in per:
        eps = e.get("base_extractiveness")
        if eps is None:
            continue
        kind, author, _route = stratum_of(e)
        out.append(Story(e["id"], float(eps), e.get("claimed_type"),
                         author, kind, source_tag))
    return out, manifest


DEFAULT_LEG_FROZEN = (ROOT / "audits" / "2026-08-10_oq78_idiom_close" / "evidence"
                      / "pipeline_output.frozen.slim.json")


def load_default_leg(path=None):
    """Default `testsets/` leg — the per-stratum inventory and the secondary stratum.

    Reads a FROZEN slim slice by default, not the live outputs/pipeline_output.json:
    the live default leg moved 243 -> 249 mid-session under an operator topic run
    (twin-leg md5s were unchanged throughout), and it feeds null construction, so an
    unfrozen read would make the band grid irreproducible.
    """
    per, manifest = _per_constraint(path or DEFAULT_LEG_FROZEN)
    out = []
    for e in per:
        eps = e.get("base_extractiveness")
        if eps is None:
            continue
        kind, author, _route = stratum_of(e)
        out.append(Story(e["id"], float(eps), e.get("claimed_type"),
                         author, kind, "default"))
    return out, manifest


def load_archive():
    """kernel_v2_test2 comparator: base_properties.{extractiveness,claimed_type}."""
    out = []
    for f in sorted(ARCHIVE.glob("*.json")):
        d = json.loads(f.read_text(encoding="utf-8"))
        bp = d.get("base_properties", {})
        eps, ctype = bp.get("extractiveness"), bp.get("claimed_type")
        if eps is None or ctype is None:
            continue
        cid = d.get("header", {}).get("constraint_id") or f.stem
        out.append(Story(cid, float(eps), ctype, "archive_kernel_v2_test2",
                         "archive", "archive"))
    return out


# ------------------------------------------------------- band grid + bands

def band_grid(stories):
    """Literal ε value set from NON-TEST data only. Frozen as literal values."""
    return sorted({round(s.eps, 6) for s in stories})


def digit_support(grid):
    return sorted({last_digit(v) for v in grid})


def quantile(vals, q):
    """Linear-interpolated quantile on the sorted sample (pinned definition)."""
    if not vals:
        return None
    xs = sorted(vals)
    if len(xs) == 1:
        return xs[0]
    pos = q * (len(xs) - 1)
    lo = int(pos)
    hi = min(lo + 1, len(xs) - 1)
    return xs[lo] + (xs[hi] - xs[lo]) * (pos - lo)


def bands_by_type(stories, min_n=MIN_TYPE_N):
    """Per-claimed_type 10th–90th percentile ε interval. Exclusions returned, never silent."""
    by = defaultdict(list)
    for s in stories:
        if s.ctype:
            by[s.ctype].append(s.eps)
    bands, excluded = {}, {}
    for t, vals in by.items():
        if len(vals) < min_n:
            excluded[t] = len(vals)
            continue
        bands[t] = {"n": len(vals), "p10": quantile(vals, 0.10),
                    "p90": quantile(vals, 0.90), "median": quantile(vals, 0.50),
                    "distinct": len(set(round(v, 6) for v in vals))}
    return bands, excluded


def max_pairwise_overlap(bands, types):
    """Max overlap fraction between the [p10,p90] intervals of *types*, ordered by median.

    Overlap fraction = |intersection| / min(|a|,|b|); a degenerate (zero-width) band
    that sits inside another counts as full overlap (1.0), which is the conservative
    reading for condition (ii).
    """
    present = [t for t in types if t in bands]
    if len(present) < 2:
        return None, present
    present.sort(key=lambda t: bands[t]["median"])
    worst = 0.0
    for i in range(len(present) - 1):
        for j in range(i + 1, len(present)):
            a, b = bands[present[i]], bands[present[j]]
            inter = min(a["p90"], b["p90"]) - max(a["p10"], b["p10"])
            if inter <= 0:
                continue
            widths = [a["p90"] - a["p10"], b["p90"] - b["p10"]]
            denom = min(w for w in widths)
            frac = 1.0 if denom <= 0 else min(1.0, inter / denom)
            worst = max(worst, frac)
    return worst, present


def _auc(a, b):
    """P(x<y) + 0.5·P(x=y) — probability of superiority, tie-aware.

    Ties matter here: ε lives on a coarse ~0.01 grid with heavy point masses, so a
    measure that ignored them would read a shared 0.68 mode as separation.
    """
    n = len(a) * len(b)
    if n == 0:
        return None
    s = sum(1.0 if x < y else (0.5 if x == y else 0.0) for x in a for y in b)
    return s / n


def worst_pair_auc(stories, types, min_n=MIN_TYPE_N):
    """(ii) MEASURE: min over ADJACENT type pairs (ordered by median ε) of the AUC.

    Replaces the p10–p90 interval-overlap measure, which was VACUOUS: it scored 1.0 on
    the comparator itself (archive rope p90=0.68, dragged by the three documented
    kernel-reading exceptions), so its bootstrap threshold calibrated to 1.0 and every
    possible banding passed. The p25–p75 variant is vacuous in the opposite direction
    (0.000 on all four non-test legs). AUC is the only candidate that varies across legs
    — 0.813 archive / 0.839 haiku / 0.885 kimi / 0.908 flash — so it is the only one
    carrying information. Operator ruling 2026-08-10.
    """
    by = defaultdict(list)
    for s in stories:
        if s.ctype in types:
            by[s.ctype].append(s.eps)
    present = [t for t in types if len(by[t]) >= min_n]
    if len(present) < 2:
        return None, present, {}
    present.sort(key=lambda t: quantile(by[t], 0.50))
    pairs = {}
    for i in range(len(present) - 1):
        a_, b_ = present[i], present[i + 1]
        pairs[f"{a_}<{b_}"] = round(_auc(by[a_], by[b_]), 4)
    return min(pairs.values()), present, pairs


def bootstrap_auc_threshold(archive, types, rng, n_boot=N_BOOT, q=0.05):
    """Calibrate the (ii) PRECONDITION threshold on the ARCHIVE's own banding.

    Resamples within-type at the archive's own cell sizes and takes the q-quantile of
    the resulting worst-pair AUC. Threshold = "the test banding is no worse than the
    comparator's own 5th-percentile banding", with quantile noise at the archive's small
    cells (rope n=10 / snare n=17) priced in.

    NOTE (operator, 2026-08-10): (ii) is a PRECONDITION, not a falsifier — it confirms
    band structure has not collapsed so the rail reading is interpretable. Falsification
    sits with (i) and the paired contrast.
    """
    by = defaultdict(list)
    for s in archive:
        if s.ctype in types:
            by[s.ctype].append(s.eps)
    draws = []
    for _ in range(n_boot):
        resampled = [Story("b", rng.choice(vals), t, "boot", "boot", "boot")
                     for t, vals in by.items() for _ in range(len(vals))]
        w, _p, _pairs = worst_pair_auc(resampled, types)
        if w is not None:
            draws.append(w)
    draws.sort()
    return (quantile(draws, q), len(draws), {
        "p05": round(quantile(draws, 0.05), 4), "p10": round(quantile(draws, 0.10), 4),
        "p50": round(quantile(draws, 0.50), 4), "p95": round(quantile(draws, 0.95), 4)})


# ------------------------------------------------ unpaired localization

def localize(stories, grid, support):
    """argmax last digit over the band grid + concentration above uniform.

    Off-grid mass counts as NON-RAIL and is reported, never dropped silently.
    """
    n_all = len(stories)
    if n_all == 0:
        return None
    gridset = set(grid)
    on = [s for s in stories if round(s.eps, 6) in gridset]
    off_share = (n_all - len(on)) / n_all
    if not on:
        return {"n": n_all, "n_on_grid": 0, "off_grid_share": round(off_share, 4),
                "argmax_digit": None, "argmax_share": None, "concentration": None}
    digits = Counter(s.digit for s in on)
    d, c = digits.most_common(1)[0]
    share = c / len(on)
    return {"n": n_all, "n_on_grid": len(on), "off_grid_share": round(off_share, 4),
            "argmax_digit": d, "argmax_share": round(share, 4),
            "concentration": round(share - 1.0 / len(support), 4),
            "digit_histogram": {str(k): v for k, v in sorted(digits.items())},
            "share_at_8": round(digits.get(8, 0) / len(on), 4)}


def localize_by_type(stories, grid, support, min_n=MIN_LOCALIZATION_N):
    by = defaultdict(list)
    for s in stories:
        if s.ctype:
            by[s.ctype].append(s)
    rows, excluded = {}, {}
    for t, ms in by.items():
        if len(ms) < min_n:
            excluded[t] = len(ms)
            continue
        rows[t] = localize(ms, grid, support)
    return rows, excluded


def unpaired_known_negative(stories, grid, support, rng, n_perm=N_PERM):
    """Known-negative: last digits permuted within band-grid support at the OBSERVED
    cell size. Returns the null concentration distribution — the statistic must not fire."""
    gridset = set(grid)
    n_on = sum(1 for s in stories if round(s.eps, 6) in gridset)
    if n_on == 0:
        return None
    draws = []
    for _ in range(n_perm):
        digits = [rng.choice(support) for _ in range(n_on)]
        c = Counter(digits).most_common(1)[0][1]
        draws.append(c / n_on - 1.0 / len(support))
    draws.sort()
    return {"n_on_grid": n_on, "null_p50": round(quantile(draws, 0.50), 4),
            "null_p95": round(quantile(draws, 0.95), 4),
            "null_p99": round(quantile(draws, 0.99), 4),
            "null_max": round(draws[-1], 4)}


def unpaired_mde(cell_n, support, floor, rng, n_sim=1500, power=0.80):
    """Minimum detectable effect at *cell_n*: smallest excess share δ at one digit for
    which the observed concentration clears *floor* with probability >= power."""
    S = len(support)
    base = 1.0 / S
    for delta in [i / 100.0 for i in range(1, int((1 - base) * 100) + 1)]:
        p_star = base + delta
        if p_star >= 1.0:
            break
        hits = 0
        for _ in range(n_sim):
            digits = []
            for _ in range(cell_n):
                if rng.random() < p_star:
                    digits.append(support[0])
                else:
                    digits.append(rng.choice(support))
            c = Counter(digits).most_common(1)[0][1]
            if c / cell_n - base >= floor:
                hits += 1
        if hits / n_sim >= power:
            return round(delta, 3)
    return None


# --------------------------------------------------- paired (PRIMARY)

def matched_set(legs_by_name, names):
    """Ids present in every named leg (the matched-seed quadruple set)."""
    sets = [set(s.cid for s in legs_by_name[n]) for n in names]
    return sorted(set.intersection(*sets))


def tv(p, q, support):
    return 0.5 * sum(abs(p.get(d, 0.0) - q.get(d, 0.0)) for d in support)


def paired_statistic(legs_by_name, names, ids, support):
    """tv_model_digit = mean over models of TV(P_m, P_pooled) on the matched set.

    Companion descriptives: all-agree rate, per-model argmax digit + share.
    """
    idx = {n: {s.cid: s for s in legs_by_name[n]} for n in names}
    per_model_digits = {n: [idx[n][i].digit for i in ids] for n in names}
    pooled = Counter()
    for n in names:
        pooled.update(per_model_digits[n])
    tot = sum(pooled.values())
    P_pool = {d: pooled[d] / tot for d in pooled}
    rows, tvs = {}, []
    for n in names:
        c = Counter(per_model_digits[n])
        P_m = {d: c[d] / len(ids) for d in c}
        t = tv(P_m, P_pool, set(list(P_pool) + list(P_m)))
        tvs.append(t)
        d, k = c.most_common(1)[0]
        rows[n] = {"argmax_digit": d, "argmax_share": round(k / len(ids), 4),
                   "concentration": round(k / len(ids) - 1.0 / len(support), 4),
                   "tv_vs_pooled": round(t, 4),
                   "share_at_8": round(c.get(8, 0) / len(ids), 4),
                   "digit_histogram": {str(x): y for x, y in sorted(c.items())}}
    agree = sum(1 for i in ids
                if len({idx[n][i].digit for n in names}) == 1) / len(ids)
    return {"n_matched": len(ids), "models": names,
            "tv_model_digit": round(sum(tvs) / len(tvs), 4),
            "all_agree_rate": round(agree, 4),
            "per_model": rows}


def paired_known_negative(legs_by_name, names, ids, support, rng, n_perm=N_PERM):
    """Known-negative: permute the MODEL LABELS within each quadruple.

    Destroys any per-model habit while preserving each seed's digit multiset, so a
    genuine model effect collapses and a composition artifact survives. tv_model_digit
    must go silent.
    """
    idx = {n: {s.cid: s for s in legs_by_name[n]} for n in names}
    per_seed = [[idx[n][i].digit for n in names] for i in ids]
    draws = []
    for _ in range(n_perm):
        cols = defaultdict(Counter)
        for row in per_seed:
            shuffled = row[:]
            rng.shuffle(shuffled)
            for k, d in enumerate(shuffled):
                cols[k][d] += 1
        pooled = Counter()
        for k in cols:
            pooled.update(cols[k])
        tot = sum(pooled.values())
        P_pool = {d: pooled[d] / tot for d in pooled}
        ts = []
        for k in cols:
            c = cols[k]
            P_m = {d: c[d] / len(ids) for d in c}
            ts.append(tv(P_m, P_pool, set(list(P_pool) + list(P_m))))
        draws.append(sum(ts) / len(ts))
    draws.sort()
    return {"n_perm": n_perm, "null_p50": round(quantile(draws, 0.50), 5),
            "null_p95": round(quantile(draws, 0.95), 5),
            "null_p99": round(quantile(draws, 0.99), 5),
            "null_max": round(draws[-1], 5)}


# ------------------------------------------------------------- residue

def residue_check(legs_by_name, names, ids):
    """Rider 4: the stories falling OUTSIDE the matched set — is the dropout
    concentrated in one claimed_type? If so the matched set's composition is selected."""
    matched = set(ids)
    out = {}
    for n in names:
        inn = [s for s in legs_by_name[n] if s.cid in matched]
        res = [s for s in legs_by_name[n] if s.cid not in matched]
        ci = Counter(s.ctype for s in inn)
        cr = Counter(s.ctype for s in res)
        ti, tr = sum(ci.values()), sum(cr.values())
        out[n] = {
            "n_matched": ti, "n_residue": tr,
            "matched_type_share": {k: round(v / ti, 4) for k, v in ci.most_common()},
            "residue_type_share": ({k: round(v / tr, 4) for k, v in cr.most_common()}
                                   if tr else {}),
            "max_share_delta": (round(max(
                abs(ci.get(t, 0) / ti - cr.get(t, 0) / tr)
                for t in set(list(ci) + list(cr))), 4) if tr else None),
        }
    return out


# ---------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--phase", choices=["calibration", "test"], required=True)
    ap.add_argument("--prereg-md5", default=None,
                    help="md5 of the frozen PREREGISTRATION.md; REQUIRED for --phase test")
    ap.add_argument("--floor", type=float, default=None,
                    help="pre-committed unpaired concentration floor (test phase)")
    ap.add_argument("--paired-floor", type=float, default=None,
                    help="pre-committed paired tv_model_digit floor (test phase)")
    ap.add_argument("--sep-threshold", type=float, default=None,
                    help="pre-committed (ii) max-overlap threshold (test phase)")
    ap.add_argument("--out", default=None)
    a = ap.parse_args()

    if a.phase == "test":
        missing = [k for k, v in (("--prereg-md5", a.prereg_md5), ("--floor", a.floor),
                                  ("--paired-floor", a.paired_floor),
                                  ("--sep-threshold", a.sep_threshold)) if v is None]
        if missing:
            raise SystemExit(f"--phase test requires the frozen values: {', '.join(missing)}")
        prereg = ROOT / "audits" / "2026-08-10_oq78_idiom_close" / "PREREGISTRATION.md"
        actual = hashlib.md5(prereg.read_bytes()).hexdigest()
        if actual != a.prereg_md5:
            raise SystemExit(f"prereg md5 mismatch: on disk {actual} != supplied {a.prereg_md5}")

    rng = random.Random(RNG_SEED)
    result = {"phase": a.phase, "rng_seed": RNG_SEED, "min_type_n": MIN_TYPE_N}

    # ---- load
    legs_by_name, leg_manifests = {}, {}
    for name, fn, expect in LEGS:
        st, mf = load_leg(name, fn, name)
        legs_by_name[name] = st
        leg_manifests[name] = mf
        authors = Counter(s.author for s in st)
        # per-instrument pre-flight: attribution VERIFIED from the data, not the dir name
        bad = [au for au in authors if not str(au).startswith(expect)]
        if bad:
            raise SystemExit(f"leg {name}: author attribution mismatch, expected "
                             f"prefix {expect!r}, saw {sorted(authors)}")
    default, default_manifest = load_default_leg()
    archive = load_archive()

    result["manifests"] = {"default": default_manifest,
                           **{f"leg_{k}": v for k, v in leg_manifests.items()}}
    result["leg_attribution"] = {n: dict(Counter(s.author for s in legs_by_name[n]))
                                 for n in legs_by_name}
    result["archive"] = {"n": len(archive),
                         "source": "prolog/archives/datasets/kernel_v2_test2/json"}

    # ---- default-leg strata inventory (all strata; the ε joint of the test stratum is
    #      NOT read in the calibration phase — see the filter below)
    strata = defaultdict(list)
    for s in default:
        strata[(s.kind, s.author)].append(s)
    result["default_leg_strata"] = [
        {"kind": k, "author": au, "n": len(ms),
         "claimed_type_counts": dict(Counter(m.ctype for m in ms).most_common())}
        for (k, au), ms in sorted(strata.items(), key=lambda kv: -len(kv[1]))]

    # ---- the 90-story secondary (rider 1): default-leg sonnet-5 NOT in the sonnet leg
    sonnet_leg_ids = {s.cid for s in legs_by_name["sonnet"]}
    default_s5 = [s for s in default if s.author == TEST_MODEL]
    secondary_90 = [s for s in default_s5 if s.cid not in sonnet_leg_ids]
    dupes = [s.cid for s in default_s5 if s.cid in sonnet_leg_ids]
    result["test_side_inventory"] = {
        "sonnet_leg_n": len(legs_by_name["sonnet"]),
        "default_leg_sonnet5_n": len(default_s5),
        "overlap_ids": sorted(dupes), "overlap_n": len(dupes),
        "secondary_unique_n": len(secondary_90),
        "secondary_claimed_type_counts":
            dict(Counter(s.ctype for s in secondary_90).most_common()),
        "note": ("the secondary NEVER enters the pinned conditions (operator ruling "
                 "2026-08-10); a divergence there is a caveat on the close's SCOPE, "
                 "not a condition failure — weak instrument at this n"),
    }

    # ---- id intersections
    def ids(x):
        return {s.cid for s in x}
    arch_ids = ids(archive)
    calib_default = [s for s in default if s.author != TEST_MODEL]
    result["id_intersections"] = {
        "calibration_default_vs_archive": len(ids(calib_default) & arch_ids),
        "calibration_default_n": len(calib_default), "archive_n": len(arch_ids),
        "cross_leg": {f"{a_}x{b_}": len(ids(legs_by_name[a_]) & ids(legs_by_name[b_]))
                      for i, a_ in enumerate([n for n, _, _ in LEGS])
                      for b_ in [n for n, _, _ in LEGS][i + 1:]},
    }

    # ---- band grid: CALIBRATION strata + archive ONLY (test excluded from the null)
    calib_pool = calib_default + [s for n in CALIBRATION_LEGS for s in legs_by_name[n]] \
        + archive
    grid = band_grid(calib_pool)
    support = digit_support(grid)
    result["band_grid"] = {"n_values": len(grid), "values": grid,
                           "digit_support": support,
                           "uniform_expectation": round(1.0 / len(support), 4),
                           "sources": ["default-leg non-sonnet-5 strata",
                                       "twin legs haiku/flash/kimi", "archive"]}

    # ---- archive bands + bootstrap separability threshold
    arch_bands, arch_excl = bands_by_type(archive)
    overlap_types = ["rope", "tangled_rope", "snare"]
    arch_overlap, arch_present = max_pairwise_overlap(arch_bands, overlap_types)
    arch_auc, auc_present, arch_pairs = worst_pair_auc(archive, overlap_types)
    thr, n_draws, thr_q = bootstrap_auc_threshold(archive, overlap_types, rng)
    result["archive_bands"] = {
        "bands": arch_bands, "excluded_below_min_n": arch_excl,
        "comparator_types": overlap_types, "types_present": arch_present,
        "worst_pair_auc": arch_auc, "pairwise_auc": arch_pairs,
        "auc_types_present": auc_present,
        "bootstrap_auc_threshold_p05": (round(thr, 4) if thr is not None else None),
        "bootstrap_auc_quantiles": thr_q, "bootstrap_draws": n_draws,
        "retired_measure_p10_p90_max_overlap": (round(arch_overlap, 4)
                                                if arch_overlap is not None else None),
        "retired_measure_note": ("p10–p90 max-overlap scored 1.0 on the comparator "
                                 "ITSELF, so its threshold calibrated to 1.0 and every "
                                 "banding passed — vacuous; retained as a descriptive "
                                 "only, never as the (ii) criterion"),
        "condition_ii_role": ("PRECONDITION — confirms band structure has not collapsed "
                              "so the rail reading is interpretable; NOT a falsifier "
                              "(operator ruling 2026-08-10)"),
    }

    # (ii) measure on every non-test leg — the four-leg banding stability observation,
    # recorded BEFORE the test read so it cannot be reconstructed afterward.
    result["banding_across_legs"] = {}
    for name in CALIBRATION_LEGS:
        w, pres, pairs = worst_pair_auc(legs_by_name[name], overlap_types)
        b, ex = bands_by_type(legs_by_name[name])
        result["banding_across_legs"][f"leg_{name}"] = {
            "worst_pair_auc": w, "pairwise_auc": pairs, "types_present": pres,
            "bands": b, "excluded_below_min_n": ex}
    result["banding_across_legs"]["archive_kernel_v2_test2"] = {
        "worst_pair_auc": arch_auc, "pairwise_auc": arch_pairs,
        "types_present": auc_present, "bands": arch_bands}
    result["banding_note"] = (
        "All three twin legs band BETTER than the archive comparator (archive is the "
        "WEAKEST bander in the set), so the p05 threshold is a LOW BAR — a pass must "
        "not be read as 'banding is strong'. Four independent models banding stably "
        "under the same feeding path is positive evidence the band-half is "
        "hypothesis-driven rather than model-mediated, which lowers the prior on "
        "outcome cells 3 and 4.")

    # ---- calibration-side unpaired localization (the family map)
    fam = {}
    for name in CALIBRATION_LEGS:
        pooled = localize(legs_by_name[name], grid, support)
        by_t, excl = localize_by_type(legs_by_name[name], grid, support)
        fam[f"leg_{name}"] = {"pooled": pooled, "by_type": by_t,
                              "excluded_below_min_n": excl}
    for (k, au), ms in strata.items():
        if au == TEST_MODEL:
            continue
        if len(ms) < MIN_TYPE_N:
            continue
        by_t, excl = localize_by_type(ms, grid, support)
        fam[f"default_{k}_{au}"] = {"pooled": localize(ms, grid, support),
                                    "by_type": by_t, "excluded_below_min_n": excl}
    fam["archive_kernel_v2_test2"] = {
        "pooled": localize(archive, grid, support),
        "by_type": localize_by_type(archive, grid, support)[0],
        "excluded_below_min_n": localize_by_type(archive, grid, support)[1]}
    result["family_map_unpaired"] = fam

    # ---- internal kind-check: authored vs derived sonnet-4.5 (pinned criterion)
    a45 = "claude-sonnet-4-5-20250929"
    auth = strata.get(("authored", a45), [])
    der = strata.get(("derived", a45), [])
    if auth and der:
        la, ld = localize(auth, grid, support), localize(der, grid, support)
        floor_note = ("DIVERGENCE requires different argmax AND both legs above the "
                      "effect-size floor; anything less is concordant-or-inconclusive, "
                      "recorded, non-blocking")
        result["kind_check_sonnet45"] = {"authored": la, "derived": ld,
                                         "same_argmax": la["argmax_digit"] == ld["argmax_digit"],
                                         "criterion": floor_note}

    # ---- unpaired known-negative (per calibration leg, at its observed cell size)
    result["unpaired_known_negative"] = {
        n: unpaired_known_negative(legs_by_name[n], grid, support, rng)
        for n in CALIBRATION_LEGS}

    # ---- PAIRED: calibration phase uses the 3 calibration legs only (blind preserved)
    paired_names = CALIBRATION_LEGS if a.phase == "calibration" else [n for n, _, _ in LEGS]
    mids = matched_set(legs_by_name, paired_names)
    result["paired"] = {
        "legs": paired_names,
        "statistic": paired_statistic(legs_by_name, paired_names, mids, support),
        "known_negative_label_permutation":
            paired_known_negative(legs_by_name, paired_names, mids, support, rng),
        "residue_check": residue_check(legs_by_name, paired_names, mids),
    }

    # ---- MDE at the relevant cell sizes
    if a.floor is not None:
        # Only cells the floor is actually APPLIED to (n >= MIN_LOCALIZATION_N). Below
        # that the uniform null itself clears the floor, so an "MDE" there is the
        # false-positive artifact, not a detectable effect — reporting it would invite
        # exactly the misread the min-cell rule exists to prevent.
        cells = sorted({c for c in (len(mids), len(legs_by_name["sonnet"]),
                                    len(secondary_90), 165, 690, 59, 60)
                        if c >= MIN_LOCALIZATION_N})
        result["unpaired_mde_at_floor"] = {
            str(c): unpaired_mde(c, support, a.floor, rng) for c in cells}
        result["unpaired_mde_note"] = (
            "smallest excess share above uniform detectable with power>=0.80 at the "
            f"pre-committed floor {a.floor}; cells below n={MIN_LOCALIZATION_N} are "
            "excluded from scoring entirely and are not tabulated here")

    # ---- TEST PHASE ONLY: the (claimed_type × ε) joint for sonnet-5
    if a.phase == "test":
        test_stories = legs_by_name["sonnet"]
        t_bands, t_excl = bands_by_type(test_stories)
        t_overlap, t_present = max_pairwise_overlap(t_bands, overlap_types)
        t_auc, t_auc_present, t_pairs = worst_pair_auc(test_stories, overlap_types)
        by_t, ex_t = localize_by_type(test_stories, grid, support)
        pooled = localize(test_stories, grid, support)
        cond_i_pooled = pooled["concentration"] is not None and \
            pooled["concentration"] < a.floor
        per_type_fire = {t: (r["concentration"] is not None and r["concentration"] >= a.floor)
                         for t, r in by_t.items()}
        # (ii) is a PRECONDITION: banding must not have collapsed below the comparator's
        # own 5th-percentile banding. Threshold is a LOW BAR by construction.
        cond_ii = t_auc is not None and t_auc >= a.sep_threshold

        pr = result["paired"]["statistic"]
        pn = result["paired"]["known_negative_label_permutation"]
        paired_fires = pr["tv_model_digit"] >= a.paired_floor
        son = pr["per_model"]["sonnet"]
        others = {k: v for k, v in pr["per_model"].items() if k != "sonnet"}
        paired_model_bound = paired_fires and any(
            v["argmax_digit"] != son["argmax_digit"] for v in others.values())

        # Secondary (rider 1) — identical statistic, POOLED ONLY (operator ruling
        # 2026-08-10: its per-type cells are ~55/10/5 and cannot clear the floor; the
        # pooled n=90 null p99 is 0.111, below the 0.25 floor, so the pooled read is
        # licensed). NEVER a pinned condition: a divergence here is a caveat on the
        # close's SCOPE (model-bound vs regime-bound-within-model), not a condition
        # failure. Concordance reassuring, discordance a flag to escalate, neither
        # decisive at this n.
        sec = {"n": len(secondary_90), "read": "pooled only",
               "why_pooled_only": ("per-type cells ~55/10/5; uniform-digit null p50 is "
                                   "0.300 at n=5 and 0.200 at n=10 — below the floor, "
                                   "so per-type cells would fire on noise")}
        if secondary_90:
            sec["pooled"] = localize(secondary_90, grid, support)
            sec["concordant_with_test_stratum"] = (
                sec["pooled"]["argmax_digit"] == pooled["argmax_digit"])

        result["test_read"] = {
            "test_stratum": "testsets_sonnet (claude-sonnet-5)",
            "n": len(test_stories),
            "bands": t_bands, "excluded_below_min_n": t_excl,
            "types_present_in_comparator_set": t_auc_present,
            "worst_pair_auc": t_auc, "pairwise_auc": t_pairs,
            "sep_threshold": a.sep_threshold,
            "retired_measure_p10_p90_max_overlap":
                (round(t_overlap, 4) if t_overlap is not None else None),
            "localization_pooled": pooled,
            "localization_by_type": by_t, "localization_excluded": ex_t,
            "condition_i_pooled_below_floor": cond_i_pooled,
            "condition_i_per_type_fires": per_type_fire,
            "condition_ii_near_separable": cond_ii,
            "paired_primary": {"tv_model_digit": pr["tv_model_digit"],
                               "floor": a.paired_floor,
                               "fires": paired_fires,
                               "null_p99": pn["null_p99"],
                               "model_bound": paired_model_bound,
                               "per_model": pr["per_model"],
                               "all_agree_rate": pr["all_agree_rate"]},
            "secondary_default_leg_sonnet5": sec,
            "conflict_rule": ("PAIRED GOVERNS the verdict; a paired/unpaired "
                              "disagreement is a headline WRITEUP finding, pinned "
                              "before the read (operator ruling 2026-08-10)"),
        }

    out = Path(a.out) if a.out else (OUTPUTS / f"oq78_railband_{a.phase}.json")
    out.write_text(json.dumps(result, indent=2), encoding="utf-8")
    print(json.dumps(result, indent=2))
    print(f"\nwrote {out}", file=sys.stderr)


if __name__ == "__main__":
    main()
