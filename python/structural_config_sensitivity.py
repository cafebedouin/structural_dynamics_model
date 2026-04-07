#!/usr/bin/env python3
"""
Structural Config Sensitivity Study v2

Tests ~83 parameter settings against three structural metrics on the
156-context product site, replacing the Feb 2026 test-suite pass/fail study
which was too coarse to detect sheaf/presheaf boundary crossings.

Metrics per setting:
  1. Binary crossings: how many constraints change H1=0 ↔ H1>0 status
  2. Type flips: total (constraint × context) pairs that change classification
  3. Block changes: how many constraints change all_constant ↔ mixed status

Parameter groups:
  A: Sigmoid shape (24 settings)
  B: Classification thresholds (23 settings)
  C: Scope and exit modulation (9 settings)
  D: Canonical d values (15 settings)
  E: Immutability table swaps (6 discrete settings)
  F: Alternative power functions (6 settings)

Usage:
  python3 python/structural_config_sensitivity.py                    # all groups, 2 workers
  python3 python/structural_config_sensitivity.py --groups A B F     # specific groups
  python3 python/structural_config_sensitivity.py --workers 4        # more parallelism

Output:
  python/structural_config_sensitivity_results.json
"""

import argparse
import json
import os
import subprocess
import sys
import tempfile
import time
from collections import defaultdict
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = ROOT / "prolog"
BASELINE_PATH = ROOT / "outputs" / "product_site_orbits.json"
RESULTS_PATH = Path(__file__).resolve().parent / "structural_config_sensitivity_results.json"


# ---------------------------------------------------------------------------
# Parameter table
# ---------------------------------------------------------------------------

# Each entry: (group, label, overlay_type, overlay_args)
# overlay_type is one of: config_param, scope_factor, exit_factor,
#                          immutability_swap, power_function_variant
SETTINGS = []

# --- Group A: Sigmoid shape ---
for val in [1.5, 3.0, 4.5, 7.5, 9.0, 12.0]:
    SETTINGS.append(("A", f"sigmoid_steepness={val}", "config_param", ("sigmoid_steepness", val)))
for val in [0.20, 0.35, 0.42, 0.58, 0.65, 0.80]:
    SETTINGS.append(("A", f"sigmoid_midpoint={val}", "config_param", ("sigmoid_midpoint", val)))
for val in [-0.40, -0.30, -0.10, 0.00, 0.10]:
    SETTINGS.append(("A", f"sigmoid_lower={val}", "config_param", ("sigmoid_lower", val)))
for val in [0.80, 1.00, 1.20, 1.35, 1.65, 1.80, 2.00]:
    SETTINGS.append(("A", f"sigmoid_upper={val}", "config_param", ("sigmoid_upper", val)))

# --- Group B: Classification thresholds ---
for val in [0.45, 0.55, 0.60, 0.72, 0.78, 0.85]:
    SETTINGS.append(("B", f"snare_chi_floor={val}", "config_param", ("snare_chi_floor", val)))
for val in [0.15, 0.25, 0.30, 0.40, 0.45, 0.55]:
    SETTINGS.append(("B", f"rope_chi_ceiling={val}", "config_param", ("rope_chi_ceiling", val)))
for val in [0.01, 0.03, 0.08, 0.10]:
    # mountain_suppression_ceiling baseline=0.05
    SETTINGS.append(("B", f"mountain_suppression_ceiling={val}", "config_param", ("mountain_suppression_ceiling", val)))
for val in [0.25, 0.35, 0.50, 0.60]:
    SETTINGS.append(("B", f"tangled_rope_chi_floor={val}", "config_param", ("tangled_rope_chi_floor", val)))
for val in [0.65, 0.75, 1.00]:
    SETTINGS.append(("B", f"tangled_rope_chi_ceil={val}", "config_param", ("tangled_rope_chi_ceil", val)))
for val in [0.15, 0.20, 0.40]:
    SETTINGS.append(("B", f"tangled_rope_epsilon_floor={val}", "config_param", ("tangled_rope_epsilon_floor", val)))

# --- Group C: Scope and exit modulation ---
# scope_modifier_factor F: new_σ = 1.0 + F × (baseline_σ − 1.0)
# baselines: local=0.8, national=1.0, global=1.2
for F in [0.0, 0.5, 1.5, 2.0, 3.0]:
    SETTINGS.append(("C", f"scope_factor={F}", "scope_factor", (F,)))
# exit_modulation_factor F: new_mod = F × baseline_mod
# baselines: trapped=0.05, identity_locked=0.04, constrained=0.02,
#            mobile=0.00, arbitrage=-0.03, analytical=0.00
for F in [0.0, 0.5, 2.0, 3.0]:
    SETTINGS.append(("C", f"exit_factor={F}", "exit_factor", (F,)))

# --- Group D: Canonical d values ---
for val in [0.85, 0.90, 0.95]:
    SETTINGS.append(("D", f"canonical_d_powerless={val}", "config_param", ("canonical_d_powerless", val)))
for val in [0.50, 0.55, 0.70, 0.75]:
    SETTINGS.append(("D", f"canonical_d_moderate={val}", "config_param", ("canonical_d_moderate", val)))
for val in [0.05, 0.10, 0.15, 0.20]:
    SETTINGS.append(("D", f"canonical_d_institutional={val}", "config_param", ("canonical_d_institutional", val)))
for val in [0.60, 0.65, 0.80, 0.85]:
    SETTINGS.append(("D", f"canonical_d_analytical={val}", "config_param", ("canonical_d_analytical", val)))

# --- Group E: Immutability table swaps ---
# Each: (time_horizon, exit_option, old_value, new_value)
IMMUTABILITY_SWAPS = [
    ("biographical",  "constrained",    "mountain", "rope"),
    ("biographical",  "trapped",        "mountain", "rope"),
    ("civilizational","analytical",     "mountain", None),    # remove mountain clause only
    ("generational",  "trapped",        "mountain", "rope"),
    ("immediate",     "constrained",    "mountain", "rope"),
    ("immediate",     "mobile",         "rope",     "mountain"),
]
for swap in IMMUTABILITY_SWAPS:
    t, e, old_v, new_v = swap
    label = f"immut_{t}_{e}:{old_v}→{new_v if new_v else 'removed'}"
    SETTINGS.append(("E", label, "immutability_swap", swap))

# --- Group F: Alternative power functions ---
for variant in ["piecewise_linear", "piecewise_no_flip", "sqrt_flip",
                "quadratic_flip", "step_flip", "sigmoid_shifted"]:
    SETTINGS.append(("F", f"power_function={variant}", "power_function_variant", (variant,)))

# ---------------------------------------------------------------------------
# Exit modulation baselines (from constraint_indexing.pl lines 401-406)
# ---------------------------------------------------------------------------
EXIT_MOD_BASELINES = {
    "trapped":         0.05,
    "identity_locked": 0.04,
    "constrained":     0.02,
    "mobile":          0.00,
    "arbitrage":      -0.03,
    "analytical":      0.00,
}

# Scope modifier baselines (from config.pl)
SCOPE_MOD_BASELINES = {
    "local":       0.8,
    "regional":    0.9,
    "national":    1.0,
    "continental": 1.1,
    "global":      1.2,
    "universal":   1.0,
}

# ---------------------------------------------------------------------------
# Overlay generators
# ---------------------------------------------------------------------------

# Common overlay parts.
# We use bifurcation_export:export_product_classifications (streaming CLASSIFY
# lines to stdout) instead of run_product_export_to (large in-memory JSON).
# site_mode=product selects the 156-context product site.
_PRELUDE = """\
%% Auto-generated structural config sensitivity overlay — DO NOT EDIT
:- use_module(config).
:- (retract(config:param(site_mode, _)) -> true ; true),
   asserta(config:param(site_mode, product)).
"""

_TAIL = """\
:- [stack].
:- bifurcation_export:export_product_classifications, halt.
"""


def _fmt_prolog_float(v):
    """Format a Python float as a Prolog number literal."""
    if isinstance(v, float) and v == int(v) and abs(v) < 1e9:
        return str(int(v))
    return repr(v)


def make_config_param_overlay(param, value):
    param_str = _fmt_prolog_float(value)
    return (
        _PRELUDE
        + f":- (retract(config:param({param}, _)) -> true ; true),\n"
        + f"   asserta(config:param({param}, {param_str})).\n"
        + _TAIL
    )


def make_scope_factor_overlay(F):
    lines = [_PRELUDE]
    for scope, baseline in SCOPE_MOD_BASELINES.items():
        new_val = 1.0 + F * (baseline - 1.0)
        new_val_str = _fmt_prolog_float(round(new_val, 6))
        lines.append(
            f":- (retract(config:param(scope_modifier_{scope}, _)) -> true ; true),\n"
            f"   asserta(config:param(scope_modifier_{scope}, {new_val_str})).\n"
        )
    lines.append(_TAIL)
    return "".join(lines)


def make_exit_factor_overlay(F):
    # Exit modulation facts live in constraint_indexing.pl, not config.pl.
    # Modify them after [stack] loads constraint_indexing.
    lines = [_PRELUDE, ":- [stack].\n",
             ":- dynamic constraint_indexing:exit_modulation/2.\n",
             ":- retractall(constraint_indexing:exit_modulation(_, _)).\n"]
    for exit_opt, baseline in EXIT_MOD_BASELINES.items():
        new_val = F * baseline
        new_val_str = _fmt_prolog_float(round(new_val, 6))
        lines.append(
            f":- asserta(constraint_indexing:exit_modulation({exit_opt}, {new_val_str})).\n"
        )
    lines.append(":- bifurcation_export:export_product_classifications, halt.\n")
    return "".join(lines)


def make_immutability_swap_overlay(time_h, exit_opt, old_val, new_val):
    lines = [_PRELUDE, ":- [stack].\n",
             ":- dynamic constraint_indexing:effective_immutability/3.\n"]
    if old_val is not None:
        lines.append(
            f":- retract(constraint_indexing:effective_immutability({time_h}, {exit_opt}, {old_val})).\n"
        )
    if new_val is not None:
        lines.append(
            f":- asserta(constraint_indexing:effective_immutability({time_h}, {exit_opt}, {new_val})).\n"
        )
    lines.append(":- bifurcation_export:export_product_classifications, halt.\n")
    return "".join(lines)


def make_power_function_overlay(variant):
    return (
        _PRELUDE
        + f":- (retract(config:param(power_function, _)) -> true ; true),\n"
        + f"   asserta(config:param(power_function, {variant})).\n"
        + _TAIL
    )


def make_overlay(overlay_type, overlay_args):
    if overlay_type == "config_param":
        param, value = overlay_args
        return make_config_param_overlay(param, value)
    elif overlay_type == "scope_factor":
        (F,) = overlay_args
        return make_scope_factor_overlay(F)
    elif overlay_type == "exit_factor":
        (F,) = overlay_args
        return make_exit_factor_overlay(F)
    elif overlay_type == "immutability_swap":
        time_h, exit_opt, old_val, new_val = overlay_args
        return make_immutability_swap_overlay(time_h, exit_opt, old_val, new_val)
    elif overlay_type == "power_function_variant":
        (variant,) = overlay_args
        return make_power_function_overlay(variant)
    else:
        raise ValueError(f"Unknown overlay_type: {overlay_type!r}")


# ---------------------------------------------------------------------------
# Prolog run
# ---------------------------------------------------------------------------

def run_setting(overlay_type, overlay_args, prolog_dir):
    """
    Run product-site classification for one parameter setting.

    Uses bifurcation_export:export_product_classifications (streaming CLASSIFY
    lines to stdout) — the same proven approach as product_site_delta_sweep.py.
    Avoids building a large in-memory JSON structure in Prolog.

    Returns {cid: {"contexts": {ctx_key: type}}} or None on error.
    """
    prolog_dir = Path(prolog_dir)
    overlay_content = make_overlay(overlay_type, overlay_args)
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".pl", dir=prolog_dir, delete=False, prefix="scs_"
    ) as pl_f:
        pl_f.write(overlay_content)
        overlay_path = pl_f.name

    try:
        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        result = subprocess.run(
            cmd, cwd=prolog_dir, capture_output=True, text=True, timeout=600
        )
        if result.returncode != 0:
            print(f"  [WARNING] exit code {result.returncode}", file=sys.stderr)
            if result.stderr:
                print(result.stderr[-1500:], file=sys.stderr)
            return None

        # Parse CLASSIFY:<cid>:<ctx_key>:<type> lines from stdout
        output = {}
        for line in result.stdout.splitlines():
            if not line.startswith("CLASSIFY:"):
                continue
            parts = line.split(":")
            if len(parts) != 4:
                continue
            _, cid, ctx_key, typ = parts
            if cid not in output:
                output[cid] = {}
            output[cid][ctx_key] = typ

        if not output:
            print("  [WARNING] no CLASSIFY lines in output", file=sys.stderr)
            if result.stderr:
                print(result.stderr[-500:], file=sys.stderr)
            return None

        # Wrap to match baseline structure expected by compute_metrics
        return {cid: {"contexts": ctxs} for cid, ctxs in output.items()}

    except subprocess.TimeoutExpired:
        print("  [ERROR] timed out", file=sys.stderr)
        return None
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass


# ---------------------------------------------------------------------------
# Metric computation
# ---------------------------------------------------------------------------

def get_blocks(contexts):
    """Group context classifications by power level (first token in key)."""
    blocks = defaultdict(dict)
    for ctx, typ in contexts.items():
        power = ctx.split("_")[0]
        blocks[power][ctx] = typ
    return dict(blocks)


def classify_presheaf(contexts):
    """'all_constant' if every power-level block is uniform, else 'mixed'."""
    blocks = get_blocks(contexts)
    if all(len(set(v.values())) == 1 for v in blocks.values()):
        return "all_constant"
    return "mixed"


def compute_metrics(baseline, perturbed):
    """
    Compare perturbed orbits against baseline.

    baseline, perturbed: {cid: {'h0': int, 'h1': int, 'contexts': {ctx: type}}}

    Returns:
      binary: {'s_to_p': N, 'p_to_s': N, 'total': N}
      type_flips: int
      block_changes: {'constant_to_mixed': N, 'mixed_to_constant': N, 'total': N}
    """
    s_to_p = 0
    p_to_s = 0
    type_flips = 0
    const_to_mixed = 0
    mixed_to_const = 0

    for cid, base_entry in baseline.items():
        if cid not in perturbed:
            continue
        pert_entry = perturbed[cid]

        base_h1_pos = base_entry["h1"] > 0
        # Recompute from contexts in case perturbed JSON format differs
        pert_ctxs = pert_entry.get("contexts", {})
        pert_types = set(pert_ctxs.values())
        pert_h1_pos = len(pert_types) > 1

        # Metric 1: binary crossings
        if not base_h1_pos and pert_h1_pos:
            s_to_p += 1
        elif base_h1_pos and not pert_h1_pos:
            p_to_s += 1

        # Metric 2: type flips
        base_ctxs = base_entry.get("contexts", {})
        for ctx, base_typ in base_ctxs.items():
            pert_typ = pert_ctxs.get(ctx)
            if pert_typ is not None and pert_typ != base_typ:
                type_flips += 1

        # Metric 3: block consistency changes
        base_bc = classify_presheaf(base_ctxs)
        pert_bc = classify_presheaf(pert_ctxs)
        if base_bc != pert_bc:
            if base_bc == "all_constant":
                const_to_mixed += 1
            else:
                mixed_to_const += 1

    return {
        "binary": {
            "s_to_p": s_to_p,
            "p_to_s": p_to_s,
            "total": s_to_p + p_to_s,
        },
        "type_flips": type_flips,
        "block_changes": {
            "constant_to_mixed": const_to_mixed,
            "mixed_to_constant": mixed_to_const,
            "total": const_to_mixed + mixed_to_const,
        },
    }


# ---------------------------------------------------------------------------
# Worker
# ---------------------------------------------------------------------------

# Module-level baseline — set in main() before ProcessPoolExecutor, inherited
# by worker processes via fork (Linux default). Workers compute metrics
# themselves and return only the small metrics dict, not the full perturbed
# orbits (which would be ~33MB per future, accumulating across all tasks).
_baseline = None


def _worker(args):
    idx, group, label, overlay_type, overlay_args, prolog_dir = args
    t0 = time.time()
    perturbed = run_setting(overlay_type, overlay_args, prolog_dir)
    elapsed = time.time() - t0
    metrics = compute_metrics(_baseline, perturbed) if perturbed is not None else None
    return idx, group, label, overlay_type, metrics, elapsed


# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

def print_table(rows, group_filter=None):
    hdr = (
        f"{'Grp':<3} | {'Parameter/Setting':<38} | {'S→P':>4} | {'P→S':>4} | "
        f"{'Binary':>6} | {'TypeFlips':>9} | {'BlkΔ':>5}"
    )
    sep = "-" * len(hdr)
    print(hdr)
    print(sep)
    for row in rows:
        if group_filter and row["group"] not in group_filter:
            continue
        m = row["metrics"]
        if m is None:
            print(f"{row['group']:<3} | {row['label']:<38} | {'FAIL':>4} | {'':>4} | {'':>6} | {'':>9} | {'':>5}")
        else:
            print(
                f"{row['group']:<3} | {row['label']:<38} | "
                f"{m['binary']['s_to_p']:>4} | {m['binary']['p_to_s']:>4} | "
                f"{m['binary']['total']:>6} | {m['type_flips']:>9} | "
                f"{m['block_changes']['total']:>5}"
            )
    print(sep)


def print_summary(rows):
    from statistics import mean
    groups = {}
    for row in rows:
        if row["metrics"] is None:
            continue
        g = row["group"]
        if g not in groups:
            groups[g] = []
        groups[g].append(row)

    print("\nSUMMARY BY GROUP (binary crossings):")
    group_means = []
    for g in sorted(groups):
        grows = groups[g]
        vals = [r["metrics"]["binary"]["total"] for r in grows]
        avg = mean(vals) if vals else 0
        mx = max(vals) if vals else 0
        mx_label = grows[vals.index(mx)]["label"] if vals else ""
        group_means.append((avg, g))
        print(f"  Group {g}: mean={avg:.1f}, max={mx} at [{mx_label}]")

    group_means.sort(reverse=True)
    hierarchy = " > ".join(f"Group {g}" for _, g in group_means if _ > 0)
    if hierarchy:
        print(f"\n  Sensitivity hierarchy: {hierarchy}")
    else:
        print("\n  All groups: zero binary crossings (product site fully stable)")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Structural config sensitivity v2",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "Memory note: each worker runs one swipl process loading the full corpus.\n"
            "  --workers 1: safe on any machine (default)\n"
            "  --workers 2: needs ~4G+ free RAM\n"
            "  --workers 4: needs ~8G+ free RAM (may OOM on WSL2)"
        ),
    )
    parser.add_argument(
        "--groups", nargs="+", default=list("ABCDEF"),
        choices=list("ABCDEF"), metavar="GROUP",
        help="Groups to run (default: A B C D E F)"
    )
    parser.add_argument(
        "--workers", type=int, default=1,
        help="Parallel workers (default: 1)"
    )
    args = parser.parse_args()

    selected_groups = set(args.groups)

    # Load baseline into module-level var so workers inherit it via fork.
    # Workers compute metrics themselves — they return only the small metrics
    # dict, not the full 33MB perturbed-orbits dict. This prevents futures from
    # accumulating 33MB each in the main process.
    global _baseline
    if not BASELINE_PATH.exists():
        print(f"ERROR: baseline not found at {BASELINE_PATH}", file=sys.stderr)
        sys.exit(1)
    print(f"Loading baseline from {BASELINE_PATH.name}...", file=sys.stderr)
    with open(BASELINE_PATH) as f:
        _baseline = json.load(f)
    n_constraints = len(_baseline)
    n_presheaves = sum(1 for e in _baseline.values() if e["h1"] > 0)
    print(f"  {n_constraints} constraints, {n_presheaves} presheaves, "
          f"{n_constraints - n_presheaves} sheaves", file=sys.stderr)

    # Filter settings by selected groups
    selected = [
        (i, group, label, otype, oargs)
        for i, (group, label, otype, oargs) in enumerate(SETTINGS)
        if group in selected_groups
    ]
    # Map from SETTINGS index → entry tuple (for error recovery)
    idx_to_entry = {i: (g, label, otype) for i, g, label, otype, _ in selected}

    print(f"\nRunning {len(selected)} settings across groups {sorted(selected_groups)}", file=sys.stderr)
    print(f"Using {args.workers} worker(s), ~80s per setting", file=sys.stderr)

    # Run in parallel
    worker_args = [
        (i, g, label, otype, oargs, str(PROLOG_DIR))
        for i, g, label, otype, oargs in selected
    ]

    completed = {}
    with ProcessPoolExecutor(max_workers=args.workers) as executor:
        futures = {executor.submit(_worker, wa): wa[0] for wa in worker_args}
        done_count = 0
        for future in as_completed(futures):
            done_count += 1
            orig_idx = futures[future]
            del futures[future]  # release reference so Future can be GC'd
            try:
                idx, group, label, overlay_type, metrics, elapsed = future.result()
                if metrics is None:
                    print(f"  [{done_count}/{len(selected)}] {group}/{label}: FAILED in {elapsed:.0f}s",
                          file=sys.stderr)
                else:
                    b = metrics["binary"]
                    print(
                        f"  [{done_count}/{len(selected)}] {group}/{label}: "
                        f"binary={b['total']} (S→P={b['s_to_p']}, P→S={b['p_to_s']}), "
                        f"flips={metrics['type_flips']}, "
                        f"blk={metrics['block_changes']['total']} "
                        f"[{elapsed:.0f}s]",
                        file=sys.stderr,
                    )
                completed[idx] = {
                    "group": group,
                    "label": label,
                    "overlay_type": overlay_type,
                    "metrics": metrics,
                    "elapsed_s": round(elapsed, 1),
                }
            except Exception as exc:
                g, label, otype = idx_to_entry.get(orig_idx, ("?", "?", "?"))
                print(f"  [{done_count}/{len(selected)}] {g}/{label}: ERROR: {exc}", file=sys.stderr)
                completed[orig_idx] = {
                    "group": g,
                    "label": label,
                    "overlay_type": otype,
                    "metrics": None,
                    "elapsed_s": 0,
                }

    # Sort by original order
    rows = [completed[i] for i in sorted(completed)]

    # Print table
    print()
    print_table(rows)
    print_summary(rows)

    # Save results
    output = {
        "metadata": {
            "groups_run": sorted(selected_groups),
            "n_settings": len(selected),
            "n_constraints": n_constraints,
            "n_presheaves": n_presheaves,
            "baseline_path": str(BASELINE_PATH),
        },
        "settings": rows,
    }
    with open(RESULTS_PATH, "w") as f:
        json.dump(output, f, indent=2)
    print(f"\nResults saved to {RESULTS_PATH}", file=sys.stderr)


if __name__ == "__main__":
    main()
