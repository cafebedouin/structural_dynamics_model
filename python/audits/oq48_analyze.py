#!/usr/bin/env python3
"""
OQ-48 distribution-break analysis (plan step 2).

Consumes the per-twin ROW dumps written by oq48_threshold_distributions.py and applies the
PRE-REGISTERED verdict rule (see plan do-oq-48-tranquil-dusk.md). Distribution-break
recalibration, NOT supervised error-minimization: the LLM-authored twins carry no external
ground-truth labels, so we locate where each metric distribution actually cleaves and ask
whether the 691-era cut still sits in that gap or has drifted into mass.

Order of operations (pinned):
  0. Break-finder POSITIVE CONTROL — planted bimodal gap at 0.45 must be recovered as a
     VALIDATED break (same bandwidth + lobe-mass + Dip rules as real data) before any real
     metric is touched. A "no break found" on real data is a finding only if this passes.
  1. Per metric (eps, supp, tr, chi) per twin: 50-bin histogram + Gaussian-KDE antimode scan;
     candidate break = KDE local min with bins both sides >= 5 readings; validated break adds
     bandwidth robustness (|dloc| <= 0.02 over h, 0.8h, 1.2h Scott) + lobe-mass ratio (larger
     side <= 4x smaller) + Hartigan Dip test rejects unimodality at alpha = 0.05.
  2. Per in-scope threshold: apply the verdict rule (ROBUST / DRIFTED / MODEL-CONFOUNDED),
     twin-swap falsification on DRIFTED candidates, cross-metric POSSIBLY-INDUCED flag.
  3. Emit verdict_table.csv and threshold_evidence.json.

DEVIATIONS (documented per CLAUDE.md one-sentence-flag, not silent walk-backs):
  * chi is unbounded above 1 (chi = eps*f(d)*sigma(S); observed up to ~1.23), so its KDE/
    histogram run over the observed range, NOT clipped to [0,1] — clipping piles >1 mass into
    the boundary bin and manufactures a false antimode there; the in-scope chi cuts (0.35,
    0.66) are interior and unaffected.
  * Hartigan's Dip test is a whole-distribution property, so it is computed once per
    metric/twin and gates that metric's breaks (matches the pinned alpha = 0.05 multimodality
    guard). diptest IS installed in this env; no substitution needed.
"""

import csv
import hashlib
import json
import sys
from pathlib import Path

import numpy as np
from scipy.stats import gaussian_kde, spearmanr

import diptest

ROOT = Path(__file__).resolve().parents[2]
AUDIT_DIR = ROOT / "audits" / "2026-06-18_oq48_recalibration"

METRIC_CODE_COMMIT = "0a629077"  # last commit touching drl_core.pl/config.pl (verify with git log)

# In-scope calibrated thresholds: (label, metric_key, current_value)
THRESHOLDS = [
    ("mountain_extractiveness_max",     "eps", 0.25),
    ("snare_epsilon_floor",             "eps", 0.46),
    ("tangled_rope_epsilon_floor",      "eps", 0.30),
    ("rope_chi_ceiling",                "chi", 0.35),
    ("snare_chi_floor",                 "chi", 0.66),
    ("snare_suppression_floor",         "supp", 0.60),
    ("tangled_rope_suppression_floor",  "supp", 0.40),
]

METRICS = ["eps", "supp", "tr", "chi"]
TWINS = ["testsets_haiku", "testsets_flash"]

# Break-finder validation tunables (pinned in plan)
BW_VARIANTS = [1.0, 0.8, 1.2]      # Scott's-rule multipliers
BW_TOL = 0.02                      # antimode location stability across bandwidths
LOBE_RATIO_MAX = 4.0               # larger lobe <= 4x smaller
MIN_LOBE_BINS_COUNT = 5            # a real lobe bin holds >= 5 readings
DIP_ALPHA = 0.05
N_BINS = 50
NEAR_CUT = 0.05                    # "within a validated trough" / "in mass" half-window
AGREE_TOL = 0.05                   # cross-twin trough-location agreement
BOOT_N = 600
BOOT_RESAMPLES = 20
BOOT_SD_MAX = 0.03
RHO_INDUCED = 0.6
GRID_POINTS = 1000
RNG = np.random.default_rng(20260618)


# ---------------------------------------------------------------------------
# Break detection
# ---------------------------------------------------------------------------
def _grid_range(data):
    lo = min(0.0, float(np.min(data)))
    hi = max(1.0, float(np.max(data)))
    pad = 0.02 * (hi - lo)
    return lo - pad, hi + pad


def _kde_density(data, grid, bw_mult):
    scott = len(data) ** (-1.0 / 5.0)        # 1-D Scott factor
    kde = gaussian_kde(data, bw_method=scott * bw_mult)
    return kde(grid)


def _local_minima(dens):
    idx = []
    for i in range(1, len(dens) - 1):
        if dens[i] < dens[i - 1] and dens[i] < dens[i + 1]:
            idx.append(i)
    return idx


def _local_maxima(dens):
    idx = []
    for i in range(1, len(dens) - 1):
        if dens[i] > dens[i - 1] and dens[i] > dens[i + 1]:
            idx.append(i)
    return idx


def _lobe_bins_ok(data, loc, lo, hi):
    """A real lobe (bin count >= MIN_LOBE_BINS_COUNT) exists on each side of `loc`."""
    counts, edges = np.histogram(data, bins=N_BINS, range=(lo, hi))
    centers = 0.5 * (edges[:-1] + edges[1:])
    left = counts[centers < loc]
    right = counts[centers > loc]
    left_ok = left.size > 0 and left.max() >= MIN_LOBE_BINS_COUNT
    right_ok = right.size > 0 and right.max() >= MIN_LOBE_BINS_COUNT
    return left_ok and right_ok


def _lobe_mass_ok(data, loc):
    left = int(np.sum(data < loc))
    right = int(np.sum(data >= loc))
    if left == 0 or right == 0:
        return False
    return max(left, right) <= LOBE_RATIO_MAX * min(left, right)


def _trough_depth(grid, dens, min_idx):
    """Topographic prominence: min(bounding-peak densities) - trough density."""
    maxima = _local_maxima(dens)
    left_peaks = [m for m in maxima if m < min_idx]
    right_peaks = [m for m in maxima if m > min_idx]
    lp = max((dens[m] for m in left_peaks), default=dens[:min_idx].max() if min_idx > 0 else dens[min_idx])
    rp = max((dens[m] for m in right_peaks), default=dens[min_idx:].max())
    return float(min(lp, rp) - dens[min_idx])


def find_validated_breaks(data):
    """Return list of validated breaks: dicts {loc, depth}. Also returns diagnostics dict."""
    data = np.asarray(data, dtype=float)
    lo, hi = _grid_range(data)
    grid = np.linspace(lo, hi, GRID_POINTS)

    # Dip test (whole-distribution multimodality gate)
    dip_stat, dip_p = diptest.diptest(data)
    multimodal = dip_p < DIP_ALPHA

    dens_main = _kde_density(data, grid, 1.0)
    min_idx = _local_minima(dens_main)

    # bandwidth-variant antimode locations (excluding main)
    variant_locs = []
    for mult in BW_VARIANTS[1:]:
        d = _kde_density(data, grid, mult)
        variant_locs.append([grid[i] for i in _local_minima(d)])

    candidates = []
    validated = []
    for i in min_idx:
        loc = float(grid[i])
        bins_ok = _lobe_bins_ok(data, loc, lo, hi)
        if not bins_ok:
            continue
        candidates.append(loc)
        bw_ok = all(
            any(abs(loc - vl) <= BW_TOL for vl in locs) for locs in variant_locs
        )
        mass_ok = _lobe_mass_ok(data, loc)
        if bw_ok and mass_ok and multimodal:
            validated.append({"loc": loc, "depth": _trough_depth(grid, dens_main, i)})

    diag = {
        "n": int(len(data)),
        "dip_stat": float(dip_stat),
        "dip_p": float(dip_p),
        "multimodal": bool(multimodal),
        "candidate_locs": [round(c, 4) for c in candidates],
        "validated": [{"loc": round(v["loc"], 4), "depth": round(v["depth"], 4)} for v in validated],
    }
    return validated, diag


# ---------------------------------------------------------------------------
# Positive control: planted bimodal gap at 0.45
# ---------------------------------------------------------------------------
def break_finder_positive_control():
    lobe_a = RNG.normal(0.25, 0.05, 600)
    lobe_b = RNG.normal(0.65, 0.05, 600)
    sample = np.clip(np.concatenate([lobe_a, lobe_b]), 0.0, 1.0)
    validated, diag = find_validated_breaks(sample)
    recovered = [v for v in validated if abs(v["loc"] - 0.45) <= 0.03]
    ok = len(recovered) >= 1
    return ok, {"recovered_at": [round(v["loc"], 4) for v in recovered], **diag}


# ---------------------------------------------------------------------------
# Verdict rule
# ---------------------------------------------------------------------------
def nearest_break(breaks, cut):
    if not breaks:
        return None
    return min(breaks, key=lambda b: abs(b["loc"] - cut))


def cut_in_trough(breaks, cut):
    """True iff a validated break sits within +/-NEAR_CUT of the cut."""
    return any(abs(b["loc"] - cut) <= NEAR_CUT for b in breaks)


def no_drift_triple(data, breaks, cut):
    nb = nearest_break(breaks, cut)
    dist = abs(nb["loc"] - cut) if nb else None
    mass_frac = float(np.mean(np.abs(np.asarray(data, float) - cut) <= NEAR_CUT))
    depth = nb["depth"] if nb else None
    return {
        "dist_cut_to_nearest_trough": round(dist, 4) if dist is not None else None,
        "mass_fraction_within_0.05": round(mass_frac, 4),
        "trough_depth": round(depth, 4) if depth is not None else None,
    }


def bootstrap_trough_sd(data, cut):
    """SD of the cut-nearest validated-break location over BOOT_RESAMPLES subsamples."""
    data = np.asarray(data, float)
    locs = []
    for _ in range(BOOT_RESAMPLES):
        sub = RNG.choice(data, size=min(BOOT_N, len(data)), replace=True)
        v, _d = find_validated_breaks(sub)
        nb = nearest_break(v, cut)
        if nb is not None:
            locs.append(nb["loc"])
    if len(locs) < 2:
        return None, len(locs)
    return float(np.std(locs)), len(locs)


def twin_swap_falsification(data_h, data_f, cut, alt_loc):
    """Pool both twins, randomly re-split into two equal groups, re-run cross-twin agreement.
    If the permuted groups STILL agree at ~alt_loc, the break is non-specific (per plan).
    Returns fraction of permutations that reproduce agreement at the alt location."""
    pooled = np.concatenate([np.asarray(data_h, float), np.asarray(data_f, float)])
    n_half = len(pooled) // 2
    hits = 0
    trials = 20
    for _ in range(trials):
        perm = RNG.permutation(pooled)
        g1, g2 = perm[:n_half], perm[n_half:]
        v1, _ = find_validated_breaks(g1)
        v2, _ = find_validated_breaks(g2)
        nb1, nb2 = nearest_break(v1, cut), nearest_break(v2, cut)
        if nb1 and nb2 and abs(nb1["loc"] - nb2["loc"]) <= AGREE_TOL \
           and abs(0.5 * (nb1["loc"] + nb2["loc"]) - alt_loc) <= AGREE_TOL:
            hits += 1
    return hits / trials


def verdict_for_threshold(label, mkey, cut, data, breaks):
    h, f = "testsets_haiku", "testsets_flash"
    bh, bf = breaks[h][mkey], breaks[f][mkey]
    in_h = cut_in_trough(bh, cut)
    in_f = cut_in_trough(bf, cut)

    row = {
        "label": label, "metric": mkey, "current_value": cut,
        "haiku_validated_breaks": [b["loc"] for b in bh],
        "flash_validated_breaks": [b["loc"] for b in bf],
        "verdict": None, "proposed_value": None, "possibly_induced": False,
        "no_drift_haiku": None, "no_drift_flash": None,
        "drift_detail": {}, "notes": "",
    }

    # ROBUST: cut within a validated trough on BOTH twins
    if in_h and in_f:
        row["verdict"] = "ROBUST"
        row["no_drift_haiku"] = no_drift_triple(data[h][mkey], bh, cut)
        row["no_drift_flash"] = no_drift_triple(data[f][mkey], bf, cut)
        return row

    # DRIFTED requires: (a) cut in mass on BOTH twins (no trough within +/-0.05)
    if in_h or in_f:
        row["verdict"] = "MODEL-CONFOUNDED"
        row["notes"] = ("cut sits in a trough on one twin but in mass on the other "
                        f"(haiku in_trough={in_h}, flash in_trough={in_f})")
        return row

    # (b) both twins show a validated break at a consistent alternate location
    nb_h, nb_f = nearest_break(bh, cut), nearest_break(bf, cut)
    if nb_h is None or nb_f is None:
        row["verdict"] = "MODEL-CONFOUNDED"
        row["notes"] = "cut in mass but at least one twin has no validated alternate break"
        return row
    loc_h, loc_f = nb_h["loc"], nb_f["loc"]
    if abs(loc_h - loc_f) > AGREE_TOL:
        row["verdict"] = "MODEL-CONFOUNDED"
        row["notes"] = (f"twins disagree on alternate trough location "
                        f"(haiku={loc_h:.4f}, flash={loc_f:.4f}, |d|>{AGREE_TOL})")
        return row
    alt = 0.5 * (loc_h + loc_f)

    # (c) bootstrap stability of the alternate-trough location, BOTH twins
    sd_h, k_h = bootstrap_trough_sd(data[h][mkey], cut)
    sd_f, k_f = bootstrap_trough_sd(data[f][mkey], cut)
    row["drift_detail"] = {
        "alt_loc_haiku": round(loc_h, 4), "alt_loc_flash": round(loc_f, 4),
        "alt_loc_mean": round(alt, 4),
        "bootstrap_sd_haiku": round(sd_h, 4) if sd_h is not None else None,
        "bootstrap_sd_flash": round(sd_f, 4) if sd_f is not None else None,
        "bootstrap_hits_haiku": k_h, "bootstrap_hits_flash": k_f,
    }
    if sd_h is None or sd_f is None or sd_h > BOOT_SD_MAX or sd_f > BOOT_SD_MAX:
        row["verdict"] = "MODEL-CONFOUNDED"
        row["notes"] = ("DRIFTED clause (c) failed: bootstrap trough-location SD exceeds "
                        f"{BOOT_SD_MAX} or break not recovered in resamples "
                        f"(sd_h={sd_h}, sd_f={sd_f})")
        return row

    # Twin-swap falsification (pre-registered): persists -> non-specific -> downgrade
    swap_frac = twin_swap_falsification(data[h][mkey], data[f][mkey], cut, alt)
    row["drift_detail"]["twin_swap_agreement_fraction"] = round(swap_frac, 3)
    if swap_frac >= 0.5:
        row["verdict"] = "MODEL-CONFOUNDED"
        row["notes"] = (f"twin-swap falsification: random label permutation still agrees at "
                        f"{alt:.3f} in {swap_frac:.0%} of trials -> non-specific")
        return row

    # Survived all DRIFTED clauses -> propose the alternate
    row["verdict"] = "DRIFTED"
    row["proposed_value"] = round(alt, 4)

    # Cross-metric POSSIBLY-INDUCED flag (non-decisive)
    rhos = []
    for tw in (h, f):
        rho, _p = spearmanr(data[tw][mkey], data[tw]["chi"])
        rhos.append(rho)
    row["drift_detail"]["spearman_rho_vs_chi"] = [round(r, 3) for r in rhos]
    if mkey != "chi" and all(abs(r) >= RHO_INDUCED for r in rhos):
        # POSSIBLY-INDUCED only if the chi threshold for that region is ROBUST -- recorded
        # by the caller after all rows computed (needs chi verdicts). Flag candidacy here.
        row["_induced_candidate"] = True
    return row


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------
def load_rows(twin):
    tsv = AUDIT_DIR / f"rows_{twin}.tsv"
    data = {m: [] for m in METRICS}
    ids = []
    with open(tsv) as fh:
        reader = csv.DictReader(fh, delimiter="\t")
        for r in reader:
            ids.append(r["id"])
            for m in METRICS:
                v = r[m]
                data[m].append(np.nan if v == "unknown" else float(v))
    # drop NaN per metric (unknowns), keep arrays metric-local
    clean = {}
    n_unknown = {}
    for m in METRICS:
        arr = np.asarray(data[m], float)
        mask = ~np.isnan(arr)
        clean[m] = arr[mask]
        n_unknown[m] = int(np.sum(~mask))
    content_hash = hashlib.sha256(tsv.read_bytes()).hexdigest()
    return clean, ids, n_unknown, content_hash


def main():
    print("=" * 78)
    print("OQ-48 break-finder POSITIVE CONTROL (planted gap at 0.45)")
    print("=" * 78)
    pc_ok, pc_diag = break_finder_positive_control()
    print(json.dumps(pc_diag, indent=2))
    print(f"POSITIVE CONTROL: {'PASS' if pc_ok else 'FAIL'}")
    if not pc_ok:
        print("!! Break-finder failed to recover the planted gap — a 'no break' on real data "
              "would be a dead probe. Aborting before touching real data.")
        return 1

    # Load twins
    data, ids, n_unknown, content_hash = {}, {}, {}, {}
    for tw in TWINS:
        data[tw], ids[tw], n_unknown[tw], content_hash[tw] = load_rows(tw)
        print(f"\n{tw}: loaded; unknowns per metric = {n_unknown[tw]}; "
              f"content sha256 = {content_hash[tw][:16]}...")

    # Per-metric breaks per twin
    breaks = {tw: {} for tw in TWINS}
    metric_diag = {tw: {} for tw in TWINS}
    for tw in TWINS:
        for m in METRICS:
            v, diag = find_validated_breaks(data[tw][m])
            breaks[tw][m] = v
            metric_diag[tw][m] = diag

    # Per-threshold verdicts
    rows = []
    for label, mkey, cut in THRESHOLDS:
        row = verdict_for_threshold(label, mkey, cut, data, breaks)
        rows.append(row)

    # Resolve POSSIBLY-INDUCED: needs the chi-threshold verdicts in the same region
    chi_robust = {r["label"]: (r["verdict"] == "ROBUST") for r in rows if r["metric"] == "chi"}
    any_chi_robust = any(chi_robust.values())
    for row in rows:
        if row.pop("_induced_candidate", False) and any_chi_robust:
            row["possibly_induced"] = True
            row["notes"] = (row["notes"] + " | POSSIBLY-INDUCED: |rho(metric,chi)|>=0.6 on both "
                            "twins and a chi-region threshold is ROBUST (non-decisive flag)").strip(" |")

    # ---- emit verdict_table.csv ----
    csv_path = AUDIT_DIR / "verdict_table.csv"
    with open(csv_path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["label", "metric", "current_value", "verdict", "proposed_value",
                    "possibly_induced", "haiku_breaks", "flash_breaks", "notes"])
        for r in rows:
            w.writerow([
                r["label"], r["metric"], r["current_value"], r["verdict"],
                r["proposed_value"] if r["proposed_value"] is not None else "",
                "yes" if r["possibly_induced"] else "",
                ";".join(f"{x:.4f}" for x in r["haiku_validated_breaks"]),
                ";".join(f"{x:.4f}" for x in r["flash_validated_breaks"]),
                r["notes"],
            ])

    # ---- emit threshold_evidence.json ----
    evidence = {
        "audit": "OQ-48 recalibration-readiness",
        "generated_for_corpus": "twins (testsets_haiku=960, testsets_flash=960)",
        "metric_code_commit": METRIC_CODE_COMMIT,
        "content_hash_sha256": content_hash,           # per-twin TSV content hash (true anchor)
        "n_unknown_per_metric": n_unknown,
        "positive_control": {"passed": pc_ok, **pc_diag},
        "verdict_rule_params": {
            "bw_variants": BW_VARIANTS, "bw_tol": BW_TOL, "lobe_ratio_max": LOBE_RATIO_MAX,
            "min_lobe_bins_count": MIN_LOBE_BINS_COUNT, "dip_alpha": DIP_ALPHA, "n_bins": N_BINS,
            "near_cut": NEAR_CUT, "agree_tol": AGREE_TOL, "bootstrap_n": BOOT_N,
            "bootstrap_resamples": BOOT_RESAMPLES, "bootstrap_sd_max": BOOT_SD_MAX,
            "rho_induced": RHO_INDUCED, "rng_seed": 20260618,
        },
        "per_metric_diagnostics": metric_diag,
        "threshold_verdicts": rows,
    }
    json_path = AUDIT_DIR / "threshold_evidence.json"
    json_path.write_text(json.dumps(evidence, indent=2))

    # ---- console summary ----
    print("\n" + "=" * 78)
    print("VERDICT TABLE")
    print("=" * 78)
    print(f"{'threshold':<32}{'metric':<6}{'cut':>6}  {'verdict':<17}{'proposed':>9}")
    for r in rows:
        prop = f"{r['proposed_value']}" if r["proposed_value"] is not None else "-"
        ind = "  [POSSIBLY-INDUCED]" if r["possibly_induced"] else ""
        print(f"{r['label']:<32}{r['metric']:<6}{r['current_value']:>6}  "
              f"{r['verdict']:<17}{prop:>9}{ind}")
    print(f"\nwrote {csv_path.relative_to(ROOT)}")
    print(f"wrote {json_path.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
