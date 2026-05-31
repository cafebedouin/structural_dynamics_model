#!/usr/bin/env python3
"""
Representation Robustness Sweep — Bridge Paper Validation Task 1

For each parameter perturbation, classifies 3,301 constraints at 4 canonical
contexts using a Prolog overlay (same pattern as config_sensitivity_sweep.py)
and counts how many constraints change their H¹=0/H¹>0 (sheaf/presheaf) status.

Uses a reduced-corpus strategy: run on all 851 manifest presheaves + 200 random
sheaves. If any crossings appear, notes them — this is a diagnostic run.

Usage:
  python3 python/representation_robustness_sweep.py [--workers N]
"""

import json
import math
import os
import random
import subprocess
import sys
import tempfile
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO_ROOT / "prolog"
OUTPUTS_DIR = REPO_ROOT / "outputs"
OUTPUT_JSON = REPO_ROOT / "python" / "representation_robustness_results.json"

# ─── Baseline H¹ per constraint (from product_site_orbits.json) ───────────────
def load_baseline():
    path = OUTPUTS_DIR / "product_site_orbits.json"
    with open(path) as f:
        data = json.load(f)
    # H¹=0 → sheaf, H¹>0 → presheaf (on canonical 4-point site use orbit_data.json)
    # Use the CANONICAL site H¹ (from pipeline_output.json perspectives)
    # Actually we need the canonical H1. Load pipeline_output.json
    pipe_path = OUTPUTS_DIR / "pipeline_output.json"
    with open(pipe_path) as f:
        pipe_data = json.load(f)
    # perspectives gives the 4-context types; compute H1 from them
    baseline = {}
    for item in pipe_data["per_constraint"]:
        cid = item["id"]
        persp = item.get("perspectives", {})
        types = list(persp.values())
        if len(types) < 4:
            continue
        unique = set(types)
        if len(unique) == 1:
            baseline[cid] = 0  # H¹=0, sheaf
        else:
            # Count disagreeing pairs
            obs = list(persp.values())
            h1 = sum(1 for i in range(len(obs)) for j in range(i+1,len(obs)) if obs[i]!=obs[j])
            baseline[cid] = h1
    return baseline

# ─── Parameter settings to sweep ──────────────────────────────────────────────
def build_param_settings():
    settings = []

    # Sigmoid shape
    for k in [3.0, 4.5, 6.0, 7.5, 9.0, 12.0]:
        settings.append({"param": "sigmoid_steepness", "value": k, "baseline": 6.0})
    for d0 in [0.35, 0.42, 0.50, 0.58, 0.65]:
        settings.append({"param": "sigmoid_midpoint", "value": d0, "baseline": 0.50})
    for L in [-0.30, -0.20, -0.10, 0.00]:
        settings.append({"param": "sigmoid_lower", "value": L, "baseline": -0.20})
    for U in [1.20, 1.35, 1.50, 1.65, 1.80]:
        settings.append({"param": "sigmoid_upper", "value": U, "baseline": 1.50})

    # Scope modifiers (scaled by factor, deviation from 1.0 is multiplied)
    for factor in [0.5, 0.75, 1.0, 1.5, 2.0]:
        settings.append({
            "param": "scope_modifier_factor",
            "value": factor,
            "baseline": 1.0,
            "multi": {
                # sigma_new = 1.0 + (sigma_baseline - 1.0) * factor
                "scope_modifier_local":    1.0 + (0.8 - 1.0) * factor,
                "scope_modifier_national": 1.0,
                "scope_modifier_global":   1.0 + (1.2 - 1.0) * factor,
            }
        })

    # Exit modulation (scaled by factor)
    for factor in [0.0, 0.5, 1.0, 2.0, 3.0]:
        settings.append({
            "param": "exit_modulation_factor",
            "value": factor,
            "baseline": 1.0,
            "multi": {
                "exit_modulation_trapped":     0.05 * factor,
                "exit_modulation_constrained": 0.02 * factor,
                "exit_modulation_mobile":      0.00,
                "exit_modulation_arbitrage":   -0.03 * factor,
                "exit_modulation_analytical":  0.00,
            }
        })

    # Classification thresholds
    for v in [0.55, 0.60, 0.66, 0.72, 0.78]:
        settings.append({"param": "snare_chi_floor", "value": v, "baseline": 0.66})
    for v in [0.25, 0.30, 0.35, 0.40, 0.45]:
        settings.append({"param": "rope_chi_ceiling", "value": v, "baseline": 0.35})

    return settings

# ─── Prolog overlay template ───────────────────────────────────────────────────
SINGLE_PARAM_OVERLAY = """\
%% Auto-generated robustness overlay
:- use_module(config).
:- (retract(config:param({param}, _)) -> true ; true),
   asserta(config:param({param}, {value})).
"""

MULTI_PARAM_OVERLAY = """\
%% Auto-generated robustness overlay (multi-param)
:- use_module(config).
{retracts_asserts}
"""

EXIT_MOD_OVERLAY = """\
%% Auto-generated exit_modulation overlay
:- use_module(constraint_indexing).
:- retractall(constraint_indexing:exit_modulation(_, _)).
:- asserta(constraint_indexing:exit_modulation(trapped,     {trapped})).
:- asserta(constraint_indexing:exit_modulation(constrained, {constrained})).
:- asserta(constraint_indexing:exit_modulation(mobile,      0.00)).
:- asserta(constraint_indexing:exit_modulation(arbitrage,   {arbitrage})).
:- asserta(constraint_indexing:exit_modulation(analytical,  0.00)).
"""

# ─── Prolog query to compute H¹ for a subset of constraints ───────────────────
QUERY_TEMPLATE = """\
:- use_module(library(lists)).
{overlay}
:- ['{prolog_dir}/stack'].
:- ['{prolog_dir}/data_repair'].

run_robustness_check :-
    corpus_loader:load_all_testsets,
    Targets = {targets},
    forall(member(C, Targets), (
        constraint_indexing:site_contexts_canonical(Ctxs),
        findall(T, (member(Ctx, Ctxs), drl_core:dr_type(C, Ctx, T)), Types),
        sort(Types, Uniq),
        (Uniq = [_] -> H1 = 0 ; 
            length(Types, _),
            findall(1, (nth1(I,Types,T1), nth1(J,Types,T2), I<J, T1\\=T2), Ones),
            length(Ones, H1)
        ),
        format("~w ~w~n", [C, H1])
    )).
"""

def build_exit_mod_overlay(factor):
    return EXIT_MOD_OVERLAY.format(
        trapped=round(0.05 * factor, 4),
        constrained=round(0.02 * factor, 4),
        arbitrage=round(-0.03 * factor, 4)
    )

def build_scope_overlay(factor):
    lines = []
    for param, val in [
        ("scope_modifier_local",    round(1.0 + (0.8-1.0)*factor, 4)),
        ("scope_modifier_national", 1.0),
        ("scope_modifier_global",   round(1.0 + (1.2-1.0)*factor, 4)),
    ]:
        lines.append(f":- (retract(config:param({param}, _)) -> true ; true), asserta(config:param({param}, {val})).")
    return "%% scope overlay\n:- use_module(config).\n" + "\n".join(lines)

def run_one_setting(setting, targets, prolog_dir):
    """Run one parameter setting, return dict with crossing counts."""
    param = setting["param"]
    value = setting["value"]

    # Build overlay
    if param == "exit_modulation_factor":
        overlay_code = build_exit_mod_overlay(value)
    elif param == "scope_modifier_factor":
        overlay_code = build_scope_overlay(value)
    else:
        overlay_code = SINGLE_PARAM_OVERLAY.format(param=param, value=value)

    # Write temp overlay file
    with tempfile.NamedTemporaryFile(suffix=".pl", mode="w", delete=False) as f:
        overlay_path = f.name
        f.write(overlay_code)

    targets_str = "[" + ",".join(targets) + "]"
    query_pl = QUERY_TEMPLATE.format(
        overlay=f":- ['{overlay_path}'].",
        prolog_dir=prolog_dir,
        targets=targets_str,
    )

    with tempfile.NamedTemporaryFile(suffix=".pl", mode="w", delete=False) as f:
        query_path = f.name
        f.write(query_pl)

    try:
        result = subprocess.run(
            ["swipl", "-g", f"['{query_path}'], run_robustness_check, halt", "-t", "halt(1)"],
            capture_output=True, text=True, timeout=300,
            cwd=str(prolog_dir)
        )
        lines = result.stdout.strip().split("\n")
        h1_map = {}
        for line in lines:
            parts = line.strip().split()
            if len(parts) == 2:
                try:
                    h1_map[parts[0]] = int(parts[1])
                except ValueError:
                    pass
        return {"setting": setting, "h1_map": h1_map, "error": None}
    except Exception as e:
        return {"setting": setting, "h1_map": {}, "error": str(e)}
    finally:
        os.unlink(overlay_path)
        os.unlink(query_path)

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--workers", type=int, default=4)
    args = parser.parse_args()

    print("[robustness] Loading baseline H¹ values...", file=sys.stderr)
    baseline = load_baseline()
    total = len(baseline)
    print(f"[robustness] {total} constraints in baseline", file=sys.stderr)

    # Build target set: all presheaves + 200 random sheaves
    presheaves = [c for c, h in baseline.items() if h > 0]
    sheaves    = [c for c, h in baseline.items() if h == 0]
    random.seed(42)
    sample_sheaves = random.sample(sheaves, min(200, len(sheaves)))
    targets = presheaves + sample_sheaves
    print(f"[robustness] Targets: {len(presheaves)} presheaves + {len(sample_sheaves)} sheaves = {len(targets)}", file=sys.stderr)

    settings = build_param_settings()
    # Skip baseline (factor=1.0) settings
    settings = [s for s in settings if not (s["value"] == s["baseline"])]
    print(f"[robustness] {len(settings)} parameter settings to test", file=sys.stderr)

    results = []
    crossings_summary = []

    prolog_dir = str(PROLOG_DIR)
    with ProcessPoolExecutor(max_workers=args.workers) as executor:
        futures = {
            executor.submit(run_one_setting, s, targets, prolog_dir): s
            for s in settings
        }
        for i, future in enumerate(as_completed(futures)):
            setting = futures[future]
            res = future.result()
            h1_map = res["h1_map"]
            if not h1_map:
                print(f"[{i+1}/{len(settings)}] {setting['param']}={setting['value']}: ERROR {res['error']}", file=sys.stderr)
                continue

            # Count crossings vs baseline
            sp_crossings = 0  # sheaf → presheaf
            ps_crossings = 0  # presheaf → sheaf
            for c in targets:
                if c not in h1_map:
                    continue
                base_h1 = baseline.get(c, 0)
                new_h1 = h1_map[c]
                if base_h1 == 0 and new_h1 > 0:
                    sp_crossings += 1
                elif base_h1 > 0 and new_h1 == 0:
                    ps_crossings += 1

            total_cross = sp_crossings + ps_crossings
            rec = {
                "param": setting["param"],
                "value": setting["value"],
                "baseline": setting["baseline"],
                "n_tested": len(h1_map),
                "sp_crossings": sp_crossings,
                "ps_crossings": ps_crossings,
                "total_crossings": total_cross,
            }
            results.append(rec)
            crossings_summary.append(rec)
            print(f"[{i+1}/{len(settings)}] {setting['param']}={setting['value']}: "
                  f"S→P={sp_crossings}, P→S={ps_crossings}, total={total_cross}",
                  file=sys.stderr)

    # Sort results for table output
    results.sort(key=lambda r: (r["param"], r["value"]))

    print("\n" + "=" * 90)
    print(f"{'Parameter':<28} | {'Value':>8} | {'Tested':>7} | {'S→P':>5} | {'P→S':>5} | {'Total':>6}")
    print("-" * 90)
    for r in results:
        print(f"{r['param']:<28} | {r['value']:>8} | {r['n_tested']:>7} | "
              f"{r['sp_crossings']:>5} | {r['ps_crossings']:>5} | {r['total_crossings']:>6}")

    print("\n" + "=" * 90)
    n_zero = sum(1 for r in results if r["total_crossings"] == 0)
    n_any  = sum(1 for r in results if r["total_crossings"] > 0)
    max_r  = max(results, key=lambda r: r["total_crossings"]) if results else None
    print(f"Total settings tested:     {len(results)}")
    print(f"Settings with 0 crossings: {n_zero}/{len(results)}")
    print(f"Settings with crossings:   {n_any}/{len(results)}")
    if max_r:
        print(f"Max crossings:             {max_r['total_crossings']} at {max_r['param']}={max_r['value']}")

    # Save JSON
    with open(OUTPUT_JSON, "w") as f:
        json.dump({
            "settings_tested": len(results),
            "zero_crossing_settings": n_zero,
            "any_crossing_settings": n_any,
            "max_crossings": max_r["total_crossings"] if max_r else 0,
            "max_at": max_r,
            "results": results
        }, f, indent=2)
    print(f"\nResults written to {OUTPUT_JSON}")

if __name__ == "__main__":
    main()
