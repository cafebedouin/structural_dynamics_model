#!/usr/bin/env python3
"""OQ-131 Q1 — six-vs-four observer site cohomology probe.

Measurable Ω_E arm: re-measure H⁰/H¹ on the live corpus + both committed twins
under the six-observer site modes (canonical_6, power_only_4/6) added at engine
commit a06b5c7f. Deterministic re-measurement on a fixed substrate.

Adjudication is against audits/2026-06-15_oq131_six_observer/PRE_REGISTRATION.md.

Design (pre-registered):
  * Headline = mean over constraints of (H¹₆ − H¹₄)/9  (9 = C(6,2) − C(4,2)).
  * Permutation null, LOAD-BEARING: N=1000, seed=20260615, per corpus.
  * Exchangeability gate (dr_type pure fn of C) witnessed by a re-run diff.
  * Seat-marginal entropy guard (halt < 0.20 normalized bits).
  * Orbit-split / co-classification = the redundancy witness (NOT the null).
  * Matched-stratum model read on the 873 non-grid ids (87 haiku-grid excluded).

One swipl process per (corpus × site config) — no cached_obstruction/3 bleed.
Serial only (OQ-77); nothing writes outputs/pipeline_output.json.
"""
from __future__ import annotations

import hashlib
import json
import math
import re
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
AUDIT_DIR = ROOT / "audits" / "2026-06-15_oq131_six_observer"
RAW_DIR = AUDIT_DIR / "raw"

# ---- pre-registered constants (DO NOT change after the run) ----
N_PERM = 1000
PERM_SEED = 20260615
ENTROPY_HALT = 0.20  # normalized bits; below ⇒ band uninterpretable
NINE = 9             # C(6,2) - C(4,2)

CORPORA = {
    "live": "testsets",
    "haiku": "testsets_haiku",
    "flash": "testsets_flash",
}

# Seat bundles (declared-revisable). bundle(TimeHorizon, ExitOptions, SpatialScope).
REAL_POWERFUL = "bundle(generational,mobile,global)"
REAL_ORGANIZED = "bundle(generational,constrained,national)"
BASELINE_TES = "bundle(biographical,mobile,national)"

# Per-seat sweep ladders (PRE_REGISTRATION §6). The OTHER seat pinned realistic.
POWERFUL_LADDER = {
    "real": REAL_POWERFUL,
    "trapped_local": "bundle(biographical,trapped,local)",
    "mobile_national": "bundle(biographical,mobile,national)",
    "analytical_global": "bundle(civilizational,analytical,global)",
}
ORGANIZED_LADDER = {
    "real": REAL_ORGANIZED,
    "trapped_local": "bundle(biographical,trapped,local)",
    "mobile_global": "bundle(generational,mobile,global)",
    "analytical_global": "bundle(civilizational,analytical,global)",
}

# Authored-grid metric predicates (data_repair:grid_provenance source grid).
GRID_METRICS = re.compile(r"\b(accessibility_collapse|stakes_inflation|suppression|resistance)\(")


# ---------------------------------------------------------------------------
# Phase A — Prolog extraction (one swipl per cell)
# ---------------------------------------------------------------------------

_OVERLAY_TMPL = """\
%% OQ-131 six-observer probe overlay — auto-generated, DO NOT EDIT
:- use_module(library(http/json)).
:- use_module(config).
:- retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, '{corpus_path}')).
:- retractall(config:param(site_mode,_)), asserta(config:param(site_mode, {site_mode})).
{bundle_overlays}:- ['{stack}'].
:- use_module('{groth}').
:- use_module('{cidx}').
:- (   corpus_loader:load_all_testsets
   ->  true
   ;   write(user_error, 'OQ131_LOAD_FAILED'), nl(user_error), halt(2)
   ).

oq131_export(OutPath) :-
    findall(_{{id: IdS, orbit: OrbitS, len: Len, h0: H0, h1: H1}},
        (   corpus_loader:corpus_constraint(C),
            atom_string(C, IdS),
            grothendieck_cohomology:orbit_vector(C, V),
            length(V, Len),
            maplist(atom_string, V, OrbitS),
            grothendieck_cohomology:cohomological_obstruction(C, H0, H1)
        ),
        Rows),
    length(Rows, N),
    setup_call_cleanup(
        open(OutPath, write, S),
        json_write_dict(S, _{{n: N, rows: Rows}}),
        close(S)).

:- (   oq131_export('{outpath}')
   ->  true
   ;   write(user_error, 'OQ131_EXPORT_FAILED'), nl(user_error), halt(3)
   ),
   halt.
"""


def _corpus_hash(corpus_dir: Path) -> str:
    pairs = []
    for p in sorted(corpus_dir.glob("*.pl")):
        pairs.append(p.name + "\n" + p.read_text(encoding="utf-8", errors="replace"))
    return hashlib.sha256("\n---\n".join(pairs).encode()).hexdigest()[:12]


def _git_commit() -> str:
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"], cwd=ROOT,
            capture_output=True, text=True, check=True,
        ).stdout.strip()
    except Exception:
        return "unknown"


def _bundle_overlay_lines(cfg: dict) -> str:
    lines = []
    for key, param in (
        ("powerful", "observer_bundle_powerful"),
        ("organized", "observer_bundle_organized"),
        ("baseline", "observer_baseline_tes"),
    ):
        if key in cfg.get("bundles", {}):
            lines.append(
                f":- retractall(config:param({param},_)), "
                f"asserta(config:param({param}, {cfg['bundles'][key]}))."
            )
    return ("\n".join(lines) + "\n") if lines else ""


def run_cell(corpus_key: str, cfg: dict, timeout: int = 600) -> dict:
    """One swipl process. Returns {rows: [...], n, ...meta}."""
    corpus_path = CORPORA[corpus_key]
    out_fd = tempfile.NamedTemporaryFile(suffix=".json", dir=PROLOG_DIR, delete=False)
    out_path = Path(out_fd.name)
    out_fd.close()
    overlay = _OVERLAY_TMPL.format(
        corpus_path=corpus_path,
        site_mode=cfg["site_mode"],
        bundle_overlays=_bundle_overlay_lines(cfg),
        stack=PROLOG_DIR / "stack.pl",
        groth=PROLOG_DIR / "grothendieck_cohomology.pl",
        cidx=PROLOG_DIR / "constraint_indexing.pl",
        outpath=out_path,
    )
    with tempfile.NamedTemporaryFile(suffix=".pl", dir=PROLOG_DIR, mode="w", delete=False) as f:
        f.write(overlay)
        overlay_path = Path(f.name)
    try:
        r = subprocess.run(
            ["swipl", "-q", "-g", f"consult('{overlay_path}'), halt.", "-t", "halt(1)"],
            cwd=PROLOG_DIR, capture_output=True, text=True, timeout=timeout,
        )
        if not out_path.exists() or out_path.stat().st_size == 0:
            sys.stderr.write(
                f"[oq131] empty output {corpus_key}/{cfg['name']} rc={r.returncode}\n"
                f"  stderr tail: {r.stderr[-400:]}\n"
            )
            return {"error": "empty", "rc": r.returncode, "stderr": r.stderr[-400:]}
        data = json.loads(out_path.read_text())
        return data
    except subprocess.TimeoutExpired:
        return {"error": "timeout"}
    finally:
        for p in (overlay_path, out_path):
            try:
                p.unlink()
            except OSError:
                pass


def cell_configs() -> list[dict]:
    """The site configs run per corpus."""
    cfgs = [
        {"name": "base4", "site_mode": "canonical", "bundles": {}},
        {"name": "canonical_6", "site_mode": "canonical_6",
         "bundles": {"powerful": REAL_POWERFUL, "organized": REAL_ORGANIZED}},
        {"name": "canonical_6_rerun", "site_mode": "canonical_6",
         "bundles": {"powerful": REAL_POWERFUL, "organized": REAL_ORGANIZED}},
        {"name": "power_only_4", "site_mode": "power_only_4",
         "bundles": {"baseline": BASELINE_TES}},
        {"name": "power_only_6", "site_mode": "power_only_6",
         "bundles": {"baseline": BASELINE_TES}},
    ]
    # per-seat sweep: powerful ladder (organized pinned realistic)
    for tag, bundle in POWERFUL_LADDER.items():
        if tag == "real":
            continue
        cfgs.append({"name": f"sweep_powerful_{tag}", "site_mode": "canonical_6",
                     "bundles": {"powerful": bundle, "organized": REAL_ORGANIZED}})
    # organized ladder (powerful pinned realistic)
    for tag, bundle in ORGANIZED_LADDER.items():
        if tag == "real":
            continue
        cfgs.append({"name": f"sweep_organized_{tag}", "site_mode": "canonical_6",
                     "bundles": {"powerful": REAL_POWERFUL, "organized": bundle}})
    return cfgs


def extract_all() -> dict:
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    commit = _git_commit()
    results = {}
    for ckey in CORPORA:
        chash = _corpus_hash(PROLOG_DIR / CORPORA[ckey])
        results[ckey] = {"corpus_hash": chash, "cells": {}}
        for cfg in cell_configs():
            sys.stderr.write(f"[oq131] running {ckey}/{cfg['name']} ...\n")
            data = run_cell(ckey, cfg)
            data["_meta"] = {"corpus": ckey, "corpus_hash": chash,
                             "code_commit": commit, "config": cfg}
            results[ckey]["cells"][cfg["name"]] = data
            (RAW_DIR / f"{ckey}__{cfg['name']}.json").write_text(json.dumps(data, indent=1))
    (RAW_DIR / "_extract_meta.json").write_text(
        json.dumps({"commit": commit, "n_perm": N_PERM, "seed": PERM_SEED,
                    "corpora": {k: results[k]["corpus_hash"] for k in CORPORA}}, indent=1))
    return results


# ---------------------------------------------------------------------------
# Phase B — analysis
# ---------------------------------------------------------------------------

def _rows_by_id(cell: dict) -> dict:
    return {r["id"]: r for r in cell.get("rows", [])}


def _nine_pair_disagree(four: list, s5: str, s6: str) -> int:
    """Disagreements among the 9 new pairs (those involving index 4 or 5)."""
    c = sum(1 for t in four if t != s5)
    c += sum(1 for t in four if t != s6)
    c += 1 if s5 != s6 else 0
    return c


def _normalized_entropy(types: list) -> tuple[float, float, int]:
    """Returns (raw_bits, normalized, k_distinct)."""
    cnt = Counter(types)
    n = sum(cnt.values())
    k = len(cnt)
    if n == 0 or k <= 1:
        return 0.0, 0.0, k
    h = -sum((v / n) * math.log2(v / n) for v in cnt.values())
    return h, h / math.log2(k), k


def _permutation_band(four_list, s5_list, s6_list) -> dict:
    import random
    rng = random.Random(PERM_SEED)
    n = len(four_list)
    null_means = []
    for _ in range(N_PERM):
        p5 = s5_list[:]
        p6 = s6_list[:]
        rng.shuffle(p5)
        rng.shuffle(p6)
        total = 0
        for i in range(n):
            total += _nine_pair_disagree(four_list[i], p5[i], p6[i])
        null_means.append(total / n / NINE)
    null_means.sort()

    def pct(q):
        idx = min(len(null_means) - 1, max(0, int(round(q * (len(null_means) - 1)))))
        return null_means[idx]

    return {"lo": pct(0.025), "med": pct(0.5), "hi": pct(0.975),
            "min": null_means[0], "max": null_means[-1]}


def analyze_corpus(ckey: str, corpus: dict) -> dict:
    cells = corpus["cells"]
    base4 = _rows_by_id(cells["base4"])
    c6 = _rows_by_id(cells["canonical_6"])
    c6r = _rows_by_id(cells["canonical_6_rerun"])
    po4 = _rows_by_id(cells.get("power_only_4", {}))
    po6 = _rows_by_id(cells.get("power_only_6", {}))

    ids = sorted(set(base4) & set(c6))
    out = {"corpus_hash": corpus["corpus_hash"], "n_constraints": len(ids)}

    # --- length-6 / fallback witness (gating) ---
    bad_len = [i for i in ids if c6[i]["len"] != 6]
    out["fallback_len6_violations"] = len(bad_len)
    out["fallback_examples"] = bad_len[:5]

    # --- exchangeability gate: canonical_6 vs rerun, orbit identity ---
    mism = [i for i in ids if i in c6r and c6[i]["orbit"] != c6r[i]["orbit"]]
    out["exchangeability"] = {
        "rerun_compared": len([i for i in ids if i in c6r]),
        "orbit_mismatches": len(mism),
        "pass": len(mism) == 0,
        "examples": mism[:5],
    }

    # --- per-constraint deltas + 9-pair basis check ---
    deltas, nine_direct, basis_ok = [], [], 0
    s5_list, s6_list, four_list = [], [], []
    new_type_seat5, new_type_seat6, echoes_both = 0, 0, 0
    splits = 0
    h0_4 = h0_6 = 0
    for i in ids:
        four = base4[i]["orbit"]
        six = c6[i]["orbit"]
        if len(six) != 6 or len(four) != 4:
            continue
        s5, s6 = six[4], six[5]
        # 9-pair basis: (H1_6 - H1_4) must equal direct 9-pair disagreement
        delta = c6[i]["h1"] - base4[i]["h1"]
        nd = _nine_pair_disagree(four, s5, s6)
        deltas.append(delta)
        nine_direct.append(nd)
        if delta == nd:
            basis_ok += 1
        four_list.append(four)
        s5_list.append(s5)
        s6_list.append(s6)
        # redundancy / co-classification
        n5 = s5 not in four
        n6 = s6 not in four
        if n5:
            new_type_seat5 += 1
        if n6:
            new_type_seat6 += 1
        if (not n5) and (not n6):
            echoes_both += 1
        if len(set(six)) > len(set(four)):
            splits += 1
        h0_4 += 1 if base4[i]["h0"] == 1 else 0
        h0_6 += 1 if c6[i]["h0"] == 1 else 0

    n = len(deltas)
    out["headline_mean_delta_over_9"] = (sum(deltas) / n / NINE) if n else None
    out["mean_raw_delta"] = (sum(deltas) / n) if n else None
    out["delta_range"] = [min(deltas), max(deltas)] if deltas else None
    out["nine_pair_basis_pass"] = basis_ok
    out["nine_pair_basis_total"] = n
    out["nine_pair_basis_ok"] = (basis_ok == n)

    # --- liveness / redundancy witness (arm 5/6) ---
    out["new_type_seat5_count"] = new_type_seat5
    out["new_type_seat6_count"] = new_type_seat6
    out["new_type_either_count"] = sum(
        1 for i in ids
        if len(c6[i]["orbit"]) == 6
        and (c6[i]["orbit"][4] not in base4[i]["orbit"]
             or c6[i]["orbit"][5] not in base4[i]["orbit"]))
    out["echoes_both_count"] = echoes_both
    out["orbit_splits"] = splits
    out["h0_singletons_4seat"] = h0_4
    out["h0_singletons_6seat"] = h0_6
    out["liveness_pass"] = out["new_type_either_count"] >= 1

    # --- seat-marginal entropy guard ---
    h5_raw, h5_norm, k5 = _normalized_entropy(s5_list)
    h6_raw, h6_norm, k6 = _normalized_entropy(s6_list)
    out["seat5_entropy"] = {"raw_bits": round(h5_raw, 4), "normalized": round(h5_norm, 4),
                            "k_distinct": k5, "marginal": dict(Counter(s5_list))}
    out["seat6_entropy"] = {"raw_bits": round(h6_raw, 4), "normalized": round(h6_norm, 4),
                            "k_distinct": k6, "marginal": dict(Counter(s6_list))}
    out["entropy_degenerate"] = (h5_norm < ENTROPY_HALT) or (h6_norm < ENTROPY_HALT)

    # --- permutation null (only if exchangeability holds & not degenerate) ---
    observed = out["headline_mean_delta_over_9"]
    if out["exchangeability"]["pass"] and not out["entropy_degenerate"] and n:
        band = _permutation_band(four_list, s5_list, s6_list)
        if observed > band["hi"]:
            verdict = "above_band__structured_constraint_correlated"
        elif observed < band["lo"]:
            verdict = "below_band__consonant_suppressing"
        else:
            verdict = "within_band__combinatorial_only"
        out["permutation"] = {"observed": observed, "band": band, "verdict": verdict,
                              "n_perm": N_PERM, "seed": PERM_SEED}
    else:
        out["permutation"] = {"observed": observed, "band": None,
                              "verdict": "WITHHELD",
                              "reason": ("exchangeability_failed" if not out["exchangeability"]["pass"]
                                         else "entropy_degenerate" if out["entropy_degenerate"]
                                         else "no_data")}

    # --- single-coordinate control (power_only_4 vs power_only_6) ---
    if po4 and po6:
        pids = sorted(set(po4) & set(po6))
        pdeltas = []
        for i in pids:
            if po6[i]["len"] == 6 and po4[i]["len"] == 4:
                pdeltas.append(po6[i]["h1"] - po4[i]["h1"])
        out["power_only_control"] = {
            "n": len(pdeltas),
            "mean_delta_over_9": (sum(pdeltas) / len(pdeltas) / NINE) if pdeltas else None,
            "mean_raw_delta": (sum(pdeltas) / len(pdeltas)) if pdeltas else None,
        }

    # --- per-seat sweep map ---
    sweep = {}
    for name, cell in cells.items():
        if not name.startswith("sweep_"):
            continue
        srows = _rows_by_id(cell)
        sd = []
        for i in ids:
            if i in srows and srows[i]["len"] == 6 and base4[i]["len"] == 4:
                sd.append(srows[i]["h1"] - base4[i]["h1"])
        sweep[name] = {"n": len(sd),
                       "mean_delta_over_9": (sum(sd) / len(sd) / NINE) if sd else None}
    # realistic joint anchor
    sweep["realistic_joint(canonical_6)"] = {
        "n": n, "mean_delta_over_9": out["headline_mean_delta_over_9"]}
    out["per_seat_sweep"] = sweep

    return out


def _grid_ids(corpus_dir: Path) -> set:
    ids = set()
    for p in corpus_dir.glob("*.pl"):
        if GRID_METRICS.search(p.read_text(encoding="utf-8", errors="replace")):
            ids.add(p.stem)
    return ids


def analyze_model_comparison(results: dict, per_corpus: dict) -> dict:
    """Matched-stratum haiku-vs-flash on the non-grid ids."""
    haiku_grid = _grid_ids(PROLOG_DIR / "testsets_haiku")
    flash_grid = _grid_ids(PROLOG_DIR / "testsets_flash")
    h_c6 = _rows_by_id(results["haiku"]["cells"]["canonical_6"])
    h_b4 = _rows_by_id(results["haiku"]["cells"]["base4"])
    f_c6 = _rows_by_id(results["flash"]["cells"]["canonical_6"])
    f_b4 = _rows_by_id(results["flash"]["cells"]["base4"])

    # Pairing key = constraint id (file base name) present in both twins.
    both = set(h_c6) & set(f_c6) & set(h_b4) & set(f_b4)
    non_grid = sorted(i for i in both if i not in haiku_grid and i not in flash_grid)
    grid_only = sorted(i for i in both if i in haiku_grid or i in flash_grid)

    def mean_delta(rows_b4, rows_c6, idset):
        ds = []
        for i in idset:
            if rows_c6[i]["len"] == 6 and rows_b4[i]["len"] == 4:
                ds.append((rows_c6[i]["h1"] - rows_b4[i]["h1"]) / NINE)
        return (sum(ds) / len(ds)) if ds else None, len(ds)

    h_mean, h_n = mean_delta(h_b4, h_c6, non_grid)
    f_mean, f_n = mean_delta(f_b4, f_c6, non_grid)
    hg_mean, hg_n = mean_delta(h_b4, h_c6, grid_only)
    return {
        "haiku_grid_ids": len(haiku_grid),
        "flash_grid_ids": len(flash_grid),
        "matched_intersection": len(both),
        "non_grid_matched": len(non_grid),
        "grid_substratum": len(grid_only),
        "haiku_nongrid_headline": h_mean,
        "flash_nongrid_headline": f_mean,
        "residual_gap_haiku_minus_flash": (h_mean - f_mean) if (h_mean is not None and f_mean is not None) else None,
        "n_haiku": h_n, "n_flash": f_n,
        "haiku_grid_substratum_headline": hg_mean, "n_grid": hg_n,
    }


def main():
    results = extract_all()
    analysis = {"per_corpus": {}, "pre_registration": str(AUDIT_DIR / "PRE_REGISTRATION.md"),
                "n_perm": N_PERM, "seed": PERM_SEED}
    for ckey in CORPORA:
        # guard: every cell must have rows
        for cname, cell in results[ckey]["cells"].items():
            if "rows" not in cell:
                analysis.setdefault("cell_errors", []).append(f"{ckey}/{cname}: {cell}")
        analysis["per_corpus"][ckey] = analyze_corpus(ckey, results[ckey])
    analysis["model_comparison"] = analyze_model_comparison(results, analysis["per_corpus"])
    (AUDIT_DIR / "analysis.json").write_text(json.dumps(analysis, indent=2))
    _print_summary(analysis)


def _print_summary(a: dict):
    print("\n" + "=" * 72)
    print("OQ-131 SIX-OBSERVER PROBE — SUMMARY (adjudicate vs PRE_REGISTRATION.md)")
    print("=" * 72)
    if a.get("cell_errors"):
        print("\n!! CELL ERRORS:")
        for e in a["cell_errors"]:
            print("   ", e)
    for ckey, c in a["per_corpus"].items():
        print(f"\n[{ckey}]  n={c['n_constraints']}  hash={c['corpus_hash']}")
        print(f"  fallback len6 violations : {c['fallback_len6_violations']} "
              f"({'ABORT' if c['fallback_len6_violations'] else 'ok'})")
        print(f"  exchangeability gate     : {'PASS' if c['exchangeability']['pass'] else 'FAIL'} "
              f"(mismatches={c['exchangeability']['orbit_mismatches']})")
        print(f"  9-pair basis             : {'PASS' if c['nine_pair_basis_ok'] else 'FAIL'} "
              f"({c['nine_pair_basis_pass']}/{c['nine_pair_basis_total']})")
        print(f"  HEADLINE (H1_6-H1_4)/9   : {c['headline_mean_delta_over_9']!r}  "
              f"(mean raw delta={c['mean_raw_delta']!r}, range={c['delta_range']})")
        print(f"  liveness (new type @5/6) : {c['new_type_either_count']} "
              f"({'PASS' if c['liveness_pass'] else 'redundant'})  "
              f"seat5={c['new_type_seat5_count']} seat6={c['new_type_seat6_count']} "
              f"echoes_both={c['echoes_both_count']}")
        print(f"  orbit splits / H0 4->6   : splits={c['orbit_splits']}  "
              f"H0 {c['h0_singletons_4seat']}->{c['h0_singletons_6seat']}")
        print(f"  seat5 entropy(norm)      : {c['seat5_entropy']['normalized']} "
              f"(k={c['seat5_entropy']['k_distinct']})   "
              f"seat6: {c['seat6_entropy']['normalized']} (k={c['seat6_entropy']['k_distinct']})  "
              f"{'DEGENERATE' if c['entropy_degenerate'] else 'ok'}")
        p = c["permutation"]
        if p["band"]:
            print(f"  permutation null         : observed={p['observed']:.4f}  "
                  f"band=[{p['band']['lo']:.4f},{p['band']['hi']:.4f}]  => {p['verdict']}")
        else:
            print(f"  permutation null         : WITHHELD ({p['reason']})")
        if "power_only_control" in c:
            pc = c["power_only_control"]
            print(f"  power_only control       : mean(delta/9)={pc['mean_delta_over_9']!r} (n={pc['n']})")
        print("  per-seat sweep (mean delta/9):")
        for name, s in sorted(c["per_seat_sweep"].items()):
            print(f"      {name:40s} {s['mean_delta_over_9']!r}  (n={s['n']})")
    mc = a["model_comparison"]
    print(f"\n[model comparison]  haiku_grid={mc['haiku_grid_ids']} flash_grid={mc['flash_grid_ids']} "
          f"matched={mc['matched_intersection']} non_grid={mc['non_grid_matched']}")
    print(f"  haiku non-grid headline  : {mc['haiku_nongrid_headline']!r} (n={mc['n_haiku']})")
    print(f"  flash non-grid headline  : {mc['flash_nongrid_headline']!r} (n={mc['n_flash']})")
    print(f"  residual gap (haiku-flash): {mc['residual_gap_haiku_minus_flash']!r}")
    print(f"  haiku grid substratum     : {mc['haiku_grid_substratum_headline']!r} (n={mc['n_grid']})")
    print("=" * 72)


if __name__ == "__main__":
    main()
