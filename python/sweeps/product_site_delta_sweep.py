#!/usr/bin/env python3
"""
δ-Sweep on the 156-Point Product Site — Task 2 of the v6.11 DR paper update.

Runs cognitive displacement (δ) perturbation over the full product site to answer:
  Q1: Is the sheaf/presheaf boundary δ-stable on the product site?
  Q2: Does δ perturbation affect within-block structure (minority fraction)?
  Q3: Does δ perturbation make all-constant presheaves mixed?

Compares results against the unperturbed baseline from outputs/product_site_orbits.json.

Usage:
  python3 python/product_site_delta_sweep.py               # spot-check: 4 delta values
  python3 python/product_site_delta_sweep.py --workers 4   # more parallelism
  python3 python/product_site_delta_sweep.py --full        # 4-point baseline too

Output:
  python/product_site_delta_results.json
"""

import argparse
import json
import os
import random
import subprocess
import sys
import tempfile
import time
from collections import Counter, defaultdict
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
ORBITS_PATH = ROOT / "outputs" / "product_site_orbits.json"
DISPLACEMENT_PATH = Path(__file__).resolve().parent / "cognitive_displacement_results.json"
RESULTS_PATH = Path(__file__).resolve().parent / "product_site_delta_results.json"

# Corpus fingerprint for staleness stamping (OQ-29)
sys.path.insert(0, str(ROOT / "python"))
from corpus_hash import compute_corpus_hash

POWERS = ["powerless", "moderate", "institutional", "analytical"]

# Spot-check delta values (uniform only for initial analysis)
SPOT_CHECK_DELTAS = [-0.10, -0.05, 0.05, 0.10]

# Subsample: all presheaves + 200 random sheaves (seeded for reproducibility)
RANDOM_SEED = 42
N_RANDOM_SHEAVES = 200

# ---------------------------------------------------------------------------
# Overlay template
# ---------------------------------------------------------------------------

PRODUCT_OVERLAY = """\
%% Auto-generated product-site delta overlay — DO NOT EDIT
%% Uniform delta = {delta}

:- use_module(config).

:- (   retract(config:param(cognitive_displacement, _))
   ->  true
   ;   true
   ),
   asserta(config:param(cognitive_displacement, {delta})).

:- (   retract(config:param(site_mode, _))
   ->  true
   ;   true
   ),
   asserta(config:param(site_mode, product)).

:- [stack].
:- bifurcation_export:export_product_classifications, halt.
"""


# ---------------------------------------------------------------------------
# Data loading and structure
# ---------------------------------------------------------------------------

def load_baseline():
    """Load product_site_orbits.json as the δ=0 baseline."""
    with open(ORBITS_PATH) as f:
        return json.load(f)


def get_blocks(contexts: dict) -> dict:
    """Group context classifications by power level."""
    blocks = defaultdict(dict)
    for ctx, typ in contexts.items():
        power = ctx.split("_")[0]
        blocks[power][ctx] = typ
    return dict(blocks)


def classify_presheaf(contexts: dict) -> str:
    """Classify presheaf as 'all_constant' or 'mixed'."""
    blocks = get_blocks(contexts)
    if all(len(set(v.values())) == 1 for v in blocks.values()):
        return "all_constant"
    return "mixed"


def compute_minority_fraction(block_ctxs: dict) -> float:
    """Minority fraction = 1 - (majority count / total) for one block."""
    if not block_ctxs:
        return 0.0
    counts = Counter(block_ctxs.values())
    majority = max(counts.values())
    return 1.0 - majority / len(block_ctxs)


def build_subsample(baseline: dict) -> tuple[set, set]:
    """Return (presheaf_ids, sheaf_ids) for subsample."""
    presheaves = {cid for cid, e in baseline.items() if e["h1"] > 0}
    sheaves = [cid for cid, e in baseline.items() if e["h1"] == 0]
    rng = random.Random(RANDOM_SEED)
    sampled_sheaves = set(rng.sample(sheaves, min(N_RANDOM_SHEAVES, len(sheaves))))
    return presheaves, sampled_sheaves


# ---------------------------------------------------------------------------
# Prolog classification run
# ---------------------------------------------------------------------------

def run_product_delta(delta: float, prolog_dir: Path) -> dict | None:
    """
    Run bifurcation_export:export_product_classifications with given delta.
    Returns dict: {constraint_id: {ctx_key: type}} or None on error.
    """
    overlay_content = PRODUCT_OVERLAY.format(delta=delta)
    with tempfile.NamedTemporaryFile(
        mode="w", suffix=".pl", dir=prolog_dir, delete=False, prefix="pdelta_"
    ) as f:
        f.write(overlay_content)
        overlay_path = f.name

    try:
        cmd = ["swipl", "-g", f"consult('{overlay_path}'), halt(0)."]
        result = subprocess.run(
            cmd, cwd=prolog_dir, capture_output=True, text=True, timeout=600
        )
        if result.returncode not in (0, 1):
            print(f"  [WARNING] delta={delta}: exit code {result.returncode}", file=sys.stderr)
            return None

        # Parse CLASSIFY:<id>:<ctx_key>:<type> lines
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
        return output

    except subprocess.TimeoutExpired:
        print(f"  [ERROR] delta={delta}: timed out", file=sys.stderr)
        return None
    finally:
        try:
            os.unlink(overlay_path)
        except OSError:
            pass


def _run_delta_worker(args):
    """Worker function for ProcessPoolExecutor."""
    delta, prolog_dir = args
    return delta, run_product_delta(delta, Path(prolog_dir))


# ---------------------------------------------------------------------------
# Analysis
# ---------------------------------------------------------------------------

def analyze_delta(
    delta: float,
    perturbed: dict,
    baseline: dict,
    presheaf_ids: set,
    sheaf_ids: set,
) -> dict:
    """
    Compare perturbed classifications against baseline.
    Returns analysis results for this delta value.
    """
    results = {
        "delta": delta,
        "n_classified": len(perturbed),
        "binary_crossings": {
            "sheaf_to_presheaf": [],  # was H1=0, now H1>0
            "presheaf_to_sheaf": [],  # was H1>0, now H1=0
        },
        "within_block_flips": {
            "all_constant_to_mixed": [],   # Q3
            "mixed_structure_changed": [],  # block dominant type changed
        },
        "minority_fraction_changes": [],  # Q2: per-block changes for mixed presheaves
        "q1_binary_count": 0,
        "q2_mean_minority_change": None,
        "q3_all_constant_to_mixed": 0,
        "q3_which_powers_develop_variation": Counter(),
    }

    minority_changes = []

    for cid in presheaf_ids | sheaf_ids:
        if cid not in baseline or cid not in perturbed:
            continue

        base_entry = baseline[cid]
        pert_ctxs = perturbed[cid]
        base_ctxs = base_entry["contexts"]
        was_sheaf = base_entry["h1"] == 0

        # Compute H1 for perturbed
        pert_types = set(pert_ctxs.values())
        pert_h1_pos = len(pert_types) > 1

        # Q1: Binary sheaf/presheaf crossings
        if was_sheaf and pert_h1_pos:
            results["binary_crossings"]["sheaf_to_presheaf"].append(cid)
        elif not was_sheaf and not pert_h1_pos:
            results["binary_crossings"]["presheaf_to_sheaf"].append(cid)

        if not was_sheaf:  # presheaf analysis
            base_sub = classify_presheaf(base_ctxs)
            pert_sub = classify_presheaf(pert_ctxs)

            # Q3: All-constant → mixed
            if base_sub == "all_constant" and pert_sub == "mixed":
                results["within_block_flips"]["all_constant_to_mixed"].append(cid)
                # Which power levels developed variation?
                pert_blocks = get_blocks(pert_ctxs)
                for power, block_ctxs in pert_blocks.items():
                    if len(set(block_ctxs.values())) > 1:
                        results["q3_which_powers_develop_variation"][power] += 1

            # Mixed presheaf block structure changes
            elif base_sub == "mixed":
                base_blocks = get_blocks(base_ctxs)
                pert_blocks = get_blocks(pert_ctxs)
                changed = False
                for power in POWERS:
                    base_block = base_blocks.get(power, {})
                    pert_block = pert_blocks.get(power, {})
                    # Check dominant type change
                    if base_block and pert_block:
                        base_majority = Counter(base_block.values()).most_common(1)[0][0]
                        pert_majority = Counter(pert_block.values()).most_common(1)[0][0]
                        if base_majority != pert_majority:
                            changed = True
                        # Q2: minority fraction change
                        base_mf = compute_minority_fraction(base_block)
                        pert_mf = compute_minority_fraction(pert_block)
                        minority_changes.append(pert_mf - base_mf)
                if changed:
                    results["within_block_flips"]["mixed_structure_changed"].append(cid)

    # Summaries
    results["q1_binary_count"] = (
        len(results["binary_crossings"]["sheaf_to_presheaf"])
        + len(results["binary_crossings"]["presheaf_to_sheaf"])
    )
    results["q2_mean_minority_change"] = (
        sum(minority_changes) / len(minority_changes) if minority_changes else 0.0
    )
    results["q3_all_constant_to_mixed"] = len(
        results["within_block_flips"]["all_constant_to_mixed"]
    )
    results["q3_which_powers_develop_variation"] = dict(
        results["q3_which_powers_develop_variation"].most_common()
    )

    # Keep lists small for JSON
    results["binary_crossings"]["sheaf_to_presheaf_ids"] = results["binary_crossings"]["sheaf_to_presheaf"][:20]
    results["binary_crossings"]["presheaf_to_sheaf_ids"] = results["binary_crossings"]["presheaf_to_sheaf"][:20]
    del results["binary_crossings"]["sheaf_to_presheaf"]
    del results["binary_crossings"]["presheaf_to_sheaf"]

    results["within_block_flips"]["all_constant_to_mixed_ids"] = results["within_block_flips"]["all_constant_to_mixed"][:20]
    del results["within_block_flips"]["all_constant_to_mixed"]

    results["within_block_flips"]["mixed_structure_changed_ids"] = results["within_block_flips"]["mixed_structure_changed"][:20]
    del results["within_block_flips"]["mixed_structure_changed"]

    return results


def get_4point_binary_crossings(delta: float) -> int | None:
    """
    Look up 4-point binary crossings from existing cognitive_displacement_results.json.
    A binary crossing on 4-point site = H1 flips between 0 and >0.
    Since H1 = # disagreeing pairs among 4 positions, a single flip can change it.
    We approximate: count constraints where ANY position flips type in the sweep.
    """
    if not DISPLACEMENT_PATH.exists():
        return None
    with open(DISPLACEMENT_PATH) as f:
        data = json.load(f)

    # Find the sweep point closest to this delta
    best_point = None
    best_diff = float("inf")
    for point in data.get("uniform", {}).get("sweep_points", []):
        if point.get("status") == "ok":
            d = abs(point["delta"] - delta)
            if d < best_diff:
                best_diff = d
                best_point = point

    if best_point is None or best_diff > 0.01:
        return None

    # Count constraints that flipped (approximation of binary crossings)
    return best_point.get("flip_count", None)


# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

def print_results(delta_results: list, presheaf_ids: set):
    n_presheaves = len(presheaf_ids)
    sep = "=" * 70
    print(sep)
    print("δ-SWEEP ON PRODUCT SITE (156-point)")
    print(sep)
    print(f"{'δ':>6} | {'binary cross (156pt)':>20} | {'within-block flips':>18} | {'4pt flips (approx)':>18}")
    print("-" * 70)
    for dr in delta_results:
        delta = dr["delta"]
        binary = dr["q1_binary_count"]
        wb = (
            len(dr["within_block_flips"]["all_constant_to_mixed_ids"])
            + len(dr["within_block_flips"]["mixed_structure_changed_ids"])
        )
        four_pt = get_4point_binary_crossings(delta)
        four_pt_str = str(four_pt) if four_pt is not None else "N/A"
        print(f"{delta:>6.2f} | {binary:>20} | {wb:>18} | {four_pt_str:>18}")
    print(sep)

    print("\nQ1: Binary boundary δ-stability:")
    for dr in delta_results:
        delta = dr["delta"]
        s2p = len(dr["binary_crossings"]["sheaf_to_presheaf_ids"])
        p2s = len(dr["binary_crossings"]["presheaf_to_sheaf_ids"])
        print(f"  δ={delta:+.2f}: sheaf→presheaf={s2p}, presheaf→sheaf={p2s}")
    total_crossings = sum(dr["q1_binary_count"] for dr in delta_results)
    four_pt_total = sum(
        x for x in (get_4point_binary_crossings(dr["delta"]) for dr in delta_results)
        if x is not None
    )
    print(f"  Total binary crossings (all δ): {total_crossings} (product site)")
    print(f"  Comparable 4-point crossings:   {four_pt_total}")
    if total_crossings == 0:
        print("  => Product site is MORE stable than 4-point site under δ perturbation")
    elif total_crossings < four_pt_total:
        print("  => Product site is MORE stable than 4-point site under δ perturbation")
    else:
        print("  => Product site shows comparable or less stability than 4-point site")

    print("\nQ2: Within-block δ-sensitivity (mixed presheaves):")
    for dr in delta_results:
        delta = dr["delta"]
        mc = dr.get("q2_mean_minority_change", 0.0)
        print(f"  δ={delta:+.2f}: mean minority fraction change = {mc:+.4f}")

    print("\nQ3: All-constant presheaves → mixed under δ:")
    for dr in delta_results:
        delta = dr["delta"]
        n = dr["q3_all_constant_to_mixed"]
        powers = dr["q3_which_powers_develop_variation"]
        print(f"  δ={delta:+.2f}: {n} constraints gain within-block variation")
        if powers:
            for p, cnt in powers.items():
                print(f"    {p}: {cnt} blocks develop variation")
    print(sep)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="δ-Sweep on product site")
    parser.add_argument("--workers", type=int, default=2, help="Parallel workers")
    parser.add_argument("--deltas", nargs="+", type=float, default=SPOT_CHECK_DELTAS,
                        help="Delta values to sweep")
    args = parser.parse_args()

    print("Loading baseline from product_site_orbits.json...")
    baseline = load_baseline()
    print(f"  {len(baseline)} constraints")

    presheaf_ids, sampled_sheaf_ids = build_subsample(baseline)
    subsample = presheaf_ids | sampled_sheaf_ids
    print(f"  Presheaves: {len(presheaf_ids)}, Sampled sheaves: {len(sampled_sheaf_ids)}")
    print(f"  Total subsample: {len(subsample)}")

    # Count all-constant vs mixed presheaves in baseline
    n_all_constant = sum(
        1 for cid in presheaf_ids
        if classify_presheaf(baseline[cid]["contexts"]) == "all_constant"
    )
    n_mixed = len(presheaf_ids) - n_all_constant
    print(f"  All-constant presheaves: {n_all_constant}")
    print(f"  Mixed presheaves: {n_mixed}")

    print(f"\nRunning δ-sweep at δ = {args.deltas}")
    print(f"  Using {args.workers} parallel worker(s)")
    print(f"  Full corpus (3301 × 156) per δ value — Prolog handles classification")
    print(f"  Estimated ~2-3 min per δ value\n")

    delta_results = []
    worker_args = [(d, str(PROLOG_DIR)) for d in args.deltas]

    with ProcessPoolExecutor(max_workers=args.workers) as executor:
        futures = {executor.submit(_run_delta_worker, wa): wa[0] for wa in worker_args}
        for future in as_completed(futures):
            delta_val = futures[future]
            try:
                delta, perturbed = future.result()
                if perturbed is None:
                    print(f"  δ={delta:+.2f}: FAILED", file=sys.stderr)
                    continue
                n_classified = sum(len(v) for v in perturbed.values())
                print(f"  δ={delta:+.2f}: classified {len(perturbed)} constraints × {n_classified // max(len(perturbed), 1)} contexts avg")
                analysis = analyze_delta(delta, perturbed, baseline, presheaf_ids, sampled_sheaf_ids)
                delta_results.append(analysis)
            except Exception as e:
                print(f"  δ={delta_val:+.2f}: ERROR: {e}", file=sys.stderr)

    # Sort by delta value
    delta_results.sort(key=lambda x: x["delta"])

    print_results(delta_results, presheaf_ids)

    output = {
        "corpus_hash": compute_corpus_hash(ROOT / "prolog" / "testsets"),
        "metadata": {
            "delta_values": args.deltas,
            "n_presheaves": len(presheaf_ids),
            "n_sampled_sheaves": len(sampled_sheaf_ids),
            "n_all_constant_presheaves": n_all_constant,
            "n_mixed_presheaves": n_mixed,
            "random_seed": RANDOM_SEED,
        },
        "delta_results": delta_results,
    }
    with open(RESULTS_PATH, "w") as f:
        json.dump(output, f, indent=2)
    print(f"\nResults saved to {RESULTS_PATH}")


if __name__ == "__main__":
    main()
