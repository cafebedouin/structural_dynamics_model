#!/usr/bin/env python3
"""census_sweep.py — config-sensitivity sweep over the commentary census (OQ-134/OQ-121).

Pairs the perturb.py overlay method (retract/asserta a `config:param`, run a Prolog
goal, diff against a baseline) with the commentary census as the MEASUREMENT
SURFACE. For each perturbation it re-runs `run_commentary_census` under the
overlaid param and diffs the per-source bucket histograms — plus the three
distinct quantities the census separates: n_in_domain, coverage, prevalence.

Why the census (not the product-site export) is the right surface here:
  - it is COMMENTARY-grade (never feeds classification), so the sweep is pure
    observation of how config moves the READING — no feedback;
  - it already carries `coverage`/`n_in_domain`, perturb.py's blind-vs-stable
    disambiguator (OQ-29): a flat census with coverage reported is INERT, not a
    false "stable";
  - it separates coverage (did we measure?) from prevalence (fired rate) from
    domain size — so a param that moves a rate via the DENOMINATOR (domain
    shrink) is distinguishable from one that finds more blindspots.

Serialization: one swipl process per perturbation (the CLAUDE.md rule). Each run
self-loads the corpus via run_commentary_census -> ensure_corpus_loaded.

Usage (from repo root):
  python3 python/sweeps/census_sweep.py                 # the curated default sweep
  python3 python/sweeps/census_sweep.py --param snare_epsilon_floor --to 0.85
  python3 python/sweeps/census_sweep.py --corpus testsets_haiku   # overlay a corpus
"""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
OUTPUTS_DIR = ROOT / "outputs"

# Single source of truth for the corpus fingerprint (OQ-29).
if str(ROOT / "python") not in sys.path:
    sys.path.insert(0, str(ROOT / "python"))
from corpus_hash import compute_corpus_hash as _compute_corpus_hash

# The curated default sweep. Each entry perturbs ONE config param; the null
# control (same value) proves the diff machinery reports 0 when nothing moves.
DEFAULT_SWEEP = [
    ("null_control",          "snare_epsilon_floor",          0.46),   # baseline value -> expect Δ=0
    ("snare_eps_floor_up",    "snare_epsilon_floor",          0.85),   # within-extractive: snare->tangled
    ("snare_eps_floor_down",  "snare_epsilon_floor",          0.20),
    ("snare_chi_floor_up",    "snare_chi_floor",              0.90),
    ("tangled_eps_floor_up",  "tangled_rope_extraction_floor", 0.50),  # raise the extractive ε floor
    ("tangled_eps_floor_max", "tangled_rope_extraction_floor", 0.95),  # above the ceil — domain hunt
    ("tangled_chi_floor_up",  "tangled_rope_chi_floor",       0.85),
]


def _run_census(overlay_goals: list[str], corpus: str | None) -> dict:
    """Run run_commentary_census under optional param/corpus overlays; parse it.

    overlay_goals: Prolog goals executed (comma-joined) BEFORE run_commentary_census.
    Returns {source: {n_corpus, n_in_domain, buckets, absence, ood, prevalence_bucket,
                      coverage, prevalence}}.
    """
    goals = list(overlay_goals)
    if corpus:
        # asserta-first (default-first-clause trap) for corpus_path.
        goals = [f"asserta(config:param(corpus_path,'{corpus}'))"] + goals
    prefix = (", ".join(goals) + ", ") if goals else ""
    goal = f"{prefix}run_commentary_census, halt"
    result = subprocess.run(
        ["swipl", "-l", "stack.pl", "-l", "commentary_census.pl", "-g", goal,
         "-t", "halt(1)"],
        cwd=str(PROLOG_DIR), capture_output=True, text=True, timeout=600,
    )
    if result.returncode != 0:
        # A perturbation may violate a RELATIONAL config invariant (config_validation
        # halts on load, e.g. "rope_epsilon_ceiling must be < snare_epsilon_floor").
        # That is a finding about the config's reachable surface, not a sweep error —
        # surface it as a rejected perturbation, don't abort the whole sweep.
        viol = [ln.strip() for ln in result.stderr.splitlines()
                if "CONFIG ERROR" in ln or "violated" in ln or "violation" in ln]
        raise ConfigRejected("; ".join(viol) or result.stderr[-300:].strip())
    return _parse_census(result.stdout)


class ConfigRejected(Exception):
    """A perturbation that config_validation refuses (violates a relational invariant)."""


def _parse_census(raw: str) -> dict:
    sources: dict = {}

    def src(name):
        return sources.setdefault(
            name, {"n_corpus": None, "n_in_domain": None, "buckets": {},
                   "absence": [], "ood": [], "prevalence_bucket": None,
                   "coverage_decidable": False})

    for line in raw.splitlines():
        p = line.split()
        if not p:
            continue
        if p[0] == "CENSUS_META" and len(p) == 4 and p[2] == "n_corpus":
            src(p[1])["n_corpus"] = int(p[3])
        elif p[0] == "CENSUS_META" and len(p) == 4 and p[2] == "n_in_domain":
            src(p[1])["n_in_domain"] = int(p[3])
        elif p[0] == "CENSUS" and len(p) == 4:
            src(p[1])["buckets"][p[2]] = int(p[3])
        elif p[0] == "CENSUS_ABSENCE" and len(p) == 3:
            src(p[1])["absence"].append(p[2])
        elif p[0] == "CENSUS_OOD" and len(p) == 3:
            src(p[1])["ood"].append(p[2])
        elif p[0] == "CENSUS_PREVALENCE" and len(p) == 4:
            src(p[1])["prevalence_bucket"] = p[2]
        elif p[0] == "CENSUS_COVERAGE" and len(p) == 3 and p[2] == "decidable":
            src(p[1])["coverage_decidable"] = True

    if not sources:
        raise RuntimeError("no CENSUS* lines parsed — census did not run")

    for d in sources.values():
        n, nid = d["n_corpus"], d["n_in_domain"]
        total = sum(d["buckets"].values())
        if n is None or n <= 0 or total != n:
            raise RuntimeError(f"census sum invariant broken: Σ={total} n={n}")
        if nid is None:
            nid = n
        if d["coverage_decidable"] and nid > 0:
            absent = sum(d["buckets"].get(b, 0) for b in d["absence"])
            d["coverage"] = (nid - absent) / nid
        else:
            d["coverage"] = None
        if d["prevalence_bucket"] is not None and nid > 0:
            d["prevalence"] = d["buckets"].get(d["prevalence_bucket"], 0) / nid
        else:
            d["prevalence"] = None
    return sources


def _overlay_goals(param: str, value: float) -> list[str]:
    return [f"retractall(config:param({param},_))",
            f"asserta(config:param({param},{value}))"]


def _diff(base: dict, pert: dict) -> dict:
    """Per-source diff: bucket deltas + n_in_domain/coverage/prevalence deltas."""
    out = {}
    for srcname in base:
        b, p = base[srcname], pert.get(srcname, {})
        all_buckets = sorted(set(b["buckets"]) | set(p.get("buckets", {})))
        bucket_deltas = {
            k: {"base": b["buckets"].get(k, 0), "pert": p.get("buckets", {}).get(k, 0),
                "delta": p.get("buckets", {}).get(k, 0) - b["buckets"].get(k, 0)}
            for k in all_buckets
            if p.get("buckets", {}).get(k, 0) - b["buckets"].get(k, 0) != 0
        }

        def d2(x, y):
            if x is None or y is None:
                return None
            return round(y - x, 4)

        out[srcname] = {
            "bucket_deltas": bucket_deltas,
            "n_in_domain": {"base": b["n_in_domain"], "pert": p.get("n_in_domain"),
                            "delta": (p.get("n_in_domain") or 0) - (b["n_in_domain"] or 0)},
            "coverage": {"base": b["coverage"], "pert": p.get("coverage"),
                         "delta": d2(b["coverage"], p.get("coverage"))},
            "prevalence": {"base": b["prevalence"], "pert": p.get("prevalence"),
                           "delta": d2(b["prevalence"], p.get("prevalence"))},
            "moved": bool(bucket_deltas),
        }
    return out


def _print_run(label, param, value, diff):
    print(f"\n=== {label}  ({param} -> {value}) ===")
    for srcname, dd in diff.items():
        flags = []
        nid = dd["n_in_domain"]
        if nid["delta"]:
            flags.append(f"n_in_domain {nid['base']}->{nid['pert']} (Δ{nid['delta']:+d})")
        cov = dd["coverage"]
        if cov["delta"] not in (None, 0):
            flags.append(f"coverage {cov['base']:.3f}->{cov['pert']:.3f} (Δ{cov['delta']:+.3f})")
        prev = dd["prevalence"]
        if prev["delta"] not in (None, 0):
            flags.append(f"prevalence {prev['base']:.3f}->{prev['pert']:.3f} (Δ{prev['delta']:+.3f})")
        status = "MOVED" if dd["moved"] else "inert"
        print(f"  [{srcname}] {status}" + (("  " + "; ".join(flags)) if flags else ""))
        for bk, bd in dd["bucket_deltas"].items():
            print(f"      {bk}: {bd['base']} -> {bd['pert']}  (Δ{bd['delta']:+d})")


def main():
    ap = argparse.ArgumentParser(description="config-sensitivity sweep over the commentary census")
    ap.add_argument("--param", help="single param to perturb (with --to)")
    ap.add_argument("--to", type=float, help="perturbed value for --param")
    ap.add_argument("--corpus", default=None, help="overlay corpus_path (e.g. testsets_haiku)")
    ap.add_argument("--out", default=str(OUTPUTS_DIR / "census_sweep.json"))
    args = ap.parse_args()

    corpus_dir = (PROLOG_DIR / (args.corpus or "testsets"))
    corpus_hash = _compute_corpus_hash(corpus_dir) if corpus_dir.exists() else "unknown"

    print(f"[census_sweep] corpus={args.corpus or 'testsets'} hash={corpus_hash}")
    baseline = _run_census([], args.corpus)
    for s, d in baseline.items():
        print(f"  baseline[{s}] n_corpus={d['n_corpus']} n_in_domain={d['n_in_domain']} "
              f"coverage={d['coverage']} prevalence={d['prevalence']}")

    if args.param and args.to is not None:
        sweep = [(f"{args.param}_to_{args.to}", args.param, args.to)]
    else:
        sweep = DEFAULT_SWEEP

    results = []
    for label, param, value in sweep:
        try:
            pert = _run_census(_overlay_goals(param, value), args.corpus)
        except ConfigRejected as e:
            print(f"\n=== {label}  ({param} -> {value}) ===")
            print(f"  CONFIG-REJECTED (relational invariant): {e}")
            results.append({"label": label, "param": param, "value": value,
                            "config_rejected": str(e)})
            continue
        diff = _diff(baseline, pert)
        _print_run(label, param, value, diff)
        # Harness positive control: re-applying the BASELINE value must move nothing.
        # A non-zero null-control diff means the overlay/parse/diff machinery is
        # manufacturing differences — fail loud (the perturb.py inertness discipline).
        if label == "null_control" and any(d["moved"] for d in diff.values()):
            raise AssertionError(
                "null_control perturbed the census — the sweep harness is "
                "manufacturing diffs (overlay or parse bug).")
        results.append({"label": label, "param": param, "value": value, "diff": diff})

    out = {
        "corpus": args.corpus or "testsets",
        "corpus_hash": corpus_hash,
        "baseline": {s: {k: d[k] for k in ("n_corpus", "n_in_domain", "coverage", "prevalence")}
                     for s, d in baseline.items()},
        "perturbations": results,
    }
    Path(args.out).write_text(json.dumps(out, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"\n[census_sweep] wrote {args.out}")


if __name__ == "__main__":
    main()
