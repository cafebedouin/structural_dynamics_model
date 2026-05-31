#!/usr/bin/env python3
"""
Witness backlog runner — sweeps params in demotion_pass.py and records survivors.

Two modes:
  Default: sweeps PERTURBABLE_UNPERTURBED at ±10% float.
  --integer-only: sweeps ERRORED_UNTESTED at ±1 integer steps (the params whose
    float ±10% produced config_schema type rejection in the prior batch).

Writes results to outputs/witness_backlog_results.json (integer mode) or same file.

Usage:
    python3 python/sweeps/witness_backlog.py [--dry-run] [--limit N] [--resume]
    python3 python/sweeps/witness_backlog.py --integer-only [--dry-run]
"""

import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(ROOT / "python"))

from sweeps.demotion_pass import (
    run_demotion_pass, load_kernel_ids,
    PERTURBABLE_UNPERTURBED, ERRORED_UNTESTED, PROLOG_DIR, CI_PATH,
    _ERRORED_UNTESTED,
)
from sweeps.perturb import perturb

OUT_PATH = ROOT / "outputs" / "witness_backlog_results.json"
OUT_PATH_INT = ROOT / "outputs" / "witness_backlog_integer_results.json"

# Config-violation guards: don't sweep past known config constraints.
# snare_epsilon_floor must stay >= rope_epsilon_ceiling (0.45) and >= 0.46.
FLOOR_GUARDS: dict[str, float] = {
    "snare_epsilon_floor": 0.46,
}


def sweep_values(param: str, original: float) -> list[float]:
    """Generate [0.9x, x, 1.1x] float values, respecting guards."""
    lo = round(original * 0.9, 6)
    hi = round(original * 1.1, 6)
    floor = FLOOR_GUARDS.get(param)
    if floor is not None and lo < floor:
        lo = floor
    values = sorted({lo, original, hi})
    if len(values) == 1:
        return [original]
    return values


def integer_sweep_values(original: float) -> list[int]:
    """Generate [val-1, val, val+1] integer values for integer-typed params.

    Floor at 0 — no negative values. For flags (val=0 or val=1), [0,1,2] covers
    disabled/baseline/over-enabled. For trajectory_enabled=0: [0,1] (no val-1=-1).
    """
    v = round(original)
    lo = max(0, v - 1)
    hi = v + 1
    return sorted({lo, v, hi})


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true", help="Print params to sweep without running")
    ap.add_argument("--limit", type=int, default=None, help="Max params to sweep (for testing)")
    ap.add_argument("--resume", action="store_true", help="Skip params already in output file")
    ap.add_argument("--integer-only", action="store_true",
                    help="Sweep only ERRORED_UNTESTED params at ±1 integer steps")
    a = ap.parse_args()

    config_pl = PROLOG_DIR / "config.pl"
    testsets_dir = PROLOG_DIR / "testsets"

    if a.integer_only:
        # Sweep the 19 errored-untested params at ±1 integer steps
        rows = run_demotion_pass(config_pl, testsets_dir)
        backlog = [r for r in rows if r["category"] == ERRORED_UNTESTED]
        out_path = OUT_PATH_INT
        value_fn = lambda r: integer_sweep_values(r["value"])
        print(f"Integer backlog: {len(backlog)} errored-untested params to sweep at ±1")
    else:
        rows = run_demotion_pass(config_pl, testsets_dir)
        backlog = [r for r in rows if r["category"] == PERTURBABLE_UNPERTURBED]
        out_path = OUT_PATH
        value_fn = lambda r: sweep_values(r["param"], r["value"])
        print(f"Backlog: {len(backlog)} params to sweep")

    if a.limit:
        backlog = backlog[:a.limit]

    # Load existing results if resuming
    existing: dict = {}
    if a.resume and out_path.exists():
        existing = json.loads(out_path.read_text())
        print(f"Resuming: {len(existing)} already done, {len(backlog) - len(existing)} remaining")
        backlog = [r for r in backlog if r["param"] not in existing]

    if a.dry_run:
        for r in backlog:
            vals = value_fn(r)
            print(f"  {r['param']} = {r['value']}  →  {vals}")
        return

    results: dict = dict(existing)
    survivors: list[str] = []

    for i, r in enumerate(backlog):
        param = r["param"]
        original = r["value"]
        values = value_fn(r)
        print(f"[{i+1}/{len(backlog)}] {param} = {original}  →  {values}", flush=True)

        try:
            result = perturb(param, values)
        except Exception as e:
            print(f"  ERROR: {e}", file=sys.stderr)
            results[param] = {"error": str(e)}
            continue

        # Check for survivors: any value with coverage>0 AND fold_survival<1.0
        for val, kr in result["results"].items():
            if isinstance(kr, dict) and "error" not in kr:
                for kernel, kdata in kr.items():
                    if kdata["coverage"] > 0 and kdata["fold_survival"] < 1.0:
                        if param not in survivors:
                            survivors.append(param)
                            print(f"  *** SURVIVOR: {param}@{val} kernel={kernel} "
                                  f"fold_survival={kdata['fold_survival']:.3f} "
                                  f"coverage={kdata['coverage']:.3f}")

        results[param] = result
        # Write incrementally so resume works
        out_path.write_text(json.dumps(results, indent=2))

    print(f"\n=== BATCH COMPLETE ===")
    print(f"Total params swept: {len(backlog)}")
    print(f"Survivors: {len(survivors)}")
    for s in survivors:
        print(f"  {s}")
    print(f"Results written to: {out_path}")


if __name__ == "__main__":
    main()
