#!/usr/bin/env python3
"""
Oracle Gap Analysis — Task 1 of the v6.11 DR paper update.

Measures how much of the 156-point product-site orbit structure can be predicted
from limited position information, with and without block-structure knowledge.

Addresses: Theorem 4 revision — does the oracle gap shrink when the observer
knows the P×E×S block structure?

Output: oracle_gap_results.json + formatted table to stdout
"""

import json
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ORBITS_PATH = ROOT / "outputs" / "product_site_orbits.json"
PIPELINE_PATH = ROOT / "outputs" / "pipeline_output.json"
RESULTS_PATH = Path(__file__).resolve().parent / "oracle_gap_results.json"

# Corpus fingerprint for staleness stamping (OQ-29)
import sys
sys.path.insert(0, str(ROOT / "python"))
from corpus_hash import compute_corpus_hash

POWERS = ["powerless", "moderate", "institutional", "analytical"]

CANONICAL_CONTEXTS = {
    "powerless": "powerless_biographical_trapped_local",
    "moderate": "moderate_biographical_mobile_national",
    "institutional": "institutional_generational_arbitrage_national",
    "analytical": "analytical_civilizational_analytical_global",
}


def load_data():
    with open(ORBITS_PATH) as f:
        orbits = json.load(f)
    with open(PIPELINE_PATH) as f:
        pipeline = json.load(f)
    perspectives = {e["id"]: e["perspectives"] for e in pipeline["per_constraint"]}
    return orbits, perspectives


def classify_constraint(contexts: dict) -> str:
    """Return 'sheaf', 'all_constant_presheaf', or 'mixed_presheaf'."""
    types = set(contexts.values())
    if len(types) == 1:
        return "sheaf"
    blocks = defaultdict(set)
    for ctx, typ in contexts.items():
        power = ctx.split("_")[0]
        blocks[power].add(typ)
    if all(len(v) == 1 for v in blocks.values()):
        return "all_constant_presheaf"
    return "mixed_presheaf"


def get_blocks(contexts: dict) -> dict:
    """Group context classifications by power level."""
    blocks = defaultdict(dict)
    for ctx, typ in contexts.items():
        power = ctx.split("_")[0]
        blocks[power][ctx] = typ
    return dict(blocks)


# ---------------------------------------------------------------------------
# Task 1.1: 4-point oracle gap
# ---------------------------------------------------------------------------

def compute_4point_oracle_gap(orbits: dict, perspectives: dict) -> dict:
    """
    For each constraint: pick the best single position (the one whose type,
    assumed for all 4 positions, gives the most correct predictions).
    Report average prediction rate, separately for sheaves and presheaves.
    """
    total_by_cat = defaultdict(list)
    best_observer_counts = Counter()

    for cid, entry in orbits.items():
        if cid not in perspectives:
            continue
        persp = perspectives[cid]  # {powerless: type, moderate: type, ...}
        positions = [p for p in POWERS if p in persp]
        if len(positions) < 4:
            continue

        cat = classify_constraint(entry["contexts"])

        best_rate = 0.0
        best_pos = None
        for oracle_pos in positions:
            oracle_type = persp[oracle_pos]
            correct = sum(1 for p in positions if persp[p] == oracle_type)
            rate = correct / len(positions)
            if rate > best_rate:
                best_rate = rate
                best_pos = oracle_pos

        total_by_cat[cat].append(best_rate)
        if best_pos:
            best_observer_counts[best_pos] += 1

    results = {}
    for cat, rates in total_by_cat.items():
        results[cat] = {
            "n": len(rates),
            "mean_predictable_pct": round(sum(rates) / len(rates) * 100, 2),
        }

    # Overall
    all_rates = [r for rates in total_by_cat.values() for r in rates]
    results["corpus_wide"] = {
        "n": len(all_rates),
        "mean_predictable_pct": round(sum(all_rates) / len(all_rates) * 100, 2),
    }
    results["best_observer_frequency"] = dict(best_observer_counts.most_common())
    return results


# ---------------------------------------------------------------------------
# Task 1.2: Product-site, 1 canonical context + block rule
# ---------------------------------------------------------------------------

def compute_1context_block_rule(orbits: dict) -> dict:
    """
    For each constraint: pick the canonical context for one power block (powerless block).
    - Sheaves (H¹=0): all 156 contexts agree, so 1 context predicts all → 100%.
    - Presheaves: apply block rule to the known block (predict all 39 = known type),
      predict nothing (wrong) for the other 3 blocks.
    Report fraction of 156 correctly predicted.
    """
    # Use powerless canonical as the "one known context"
    known_power = "powerless"
    known_ctx = CANONICAL_CONTEXTS[known_power]

    total_by_cat = defaultdict(list)

    for cid, entry in orbits.items():
        contexts = entry["contexts"]
        if known_ctx not in contexts:
            continue

        cat = classify_constraint(contexts)
        known_type = contexts[known_ctx]
        blocks = get_blocks(contexts)

        if cat == "sheaf":
            # All 156 contexts agree — knowing 1 predicts all
            rate = 1.0
        else:
            correct = 0
            for power, block_ctxs in blocks.items():
                if power == known_power:
                    # Predict all 39 = known_type
                    correct += sum(1 for t in block_ctxs.values() if t == known_type)
                # else: predict nothing = 0 correct for other blocks
            rate = correct / len(contexts)

        total_by_cat[cat].append(rate)

    results = {}
    for cat, rates in total_by_cat.items():
        results[cat] = {
            "n": len(rates),
            "mean_predictable_pct": round(sum(rates) / len(rates) * 100, 2),
        }
    all_rates = [r for rates in total_by_cat.values() for r in rates]
    results["corpus_wide"] = {
        "n": len(all_rates),
        "mean_predictable_pct": round(sum(all_rates) / len(all_rates) * 100, 2),
    }
    return results


# ---------------------------------------------------------------------------
# Task 1.3: Product-site, 4 canonical contexts + block rule
# ---------------------------------------------------------------------------

def compute_4canonical_block_rule(orbits: dict) -> dict:
    """
    For each constraint: look up all 4 canonical context types.
    Predict every context in each power block = its canonical context's type.
    Report fraction of 156 correctly predicted.
    """
    total_by_cat = defaultdict(list)

    for cid, entry in orbits.items():
        contexts = entry["contexts"]
        # Check all canonical contexts present
        if not all(ctx in contexts for ctx in CANONICAL_CONTEXTS.values()):
            continue

        cat = classify_constraint(contexts)
        blocks = get_blocks(contexts)

        correct = 0
        for power in POWERS:
            canonical_ctx = CANONICAL_CONTEXTS[power]
            predicted_type = contexts[canonical_ctx]
            block_ctxs = blocks.get(power, {})
            correct += sum(1 for t in block_ctxs.values() if t == predicted_type)

        rate = correct / len(contexts)
        total_by_cat[cat].append(rate)

    results = {}
    for cat, rates in total_by_cat.items():
        results[cat] = {
            "n": len(rates),
            "mean_predictable_pct": round(sum(rates) / len(rates) * 100, 2),
        }
    all_rates = [r for rates in total_by_cat.values() for r in rates]
    results["corpus_wide"] = {
        "n": len(all_rates),
        "mean_predictable_pct": round(sum(all_rates) / len(all_rates) * 100, 2),
    }
    return results


# ---------------------------------------------------------------------------
# Task 1.4: Misprediction breakdown by axis
# ---------------------------------------------------------------------------

def compute_misprediction_breakdown(orbits: dict) -> dict:
    """
    For mixed presheaves where 4-canonical + block rule prediction < 100%:
    identify which axis (T, E, S) drives the mispredictions and which power blocks.
    Context key format: power_time_exit_scope (4 components)
    """
    mispred_counts_by_power = Counter()
    mispred_counts_by_axis = defaultdict(lambda: defaultdict(int))
    per_constraint_mispred = []
    n_analyzed = 0
    n_perfect = 0

    for cid, entry in orbits.items():
        contexts = entry["contexts"]
        if not all(ctx in contexts for ctx in CANONICAL_CONTEXTS.values()):
            continue
        if classify_constraint(contexts) != "mixed_presheaf":
            continue

        blocks = get_blocks(contexts)
        n_analyzed += 1
        mispredicted = 0

        for power in POWERS:
            canonical_ctx = CANONICAL_CONTEXTS[power]
            predicted_type = contexts[canonical_ctx]
            # Parse canonical context axes
            canonical_parts = canonical_ctx.split("_")  # [power, time, exit, scope]
            # Note: exit_options may have underscores (e.g., "identity_locked")
            # All our values are single words, so split is safe at 3 parts after power
            # Format: power_time_exit_scope where each is a single token
            block_ctxs = blocks.get(power, {})

            for ctx, actual_type in block_ctxs.items():
                if actual_type != predicted_type:
                    mispredicted += 1
                    mispred_counts_by_power[power] += 1
                    # Identify which axis differs from canonical
                    parts = ctx.split("_")
                    canon_parts = canonical_ctx.split("_")
                    # parts[0]=power, [1]=time, [2]=exit, [3]=scope
                    # For 4-token contexts this works; for multi-token exits we need care
                    # All exit tokens in our vocab are single words, so len==4
                    if len(parts) == 4 and len(canon_parts) == 4:
                        if parts[1] != canon_parts[1]:
                            mispred_counts_by_axis[power]["T"] += 1
                        if parts[2] != canon_parts[2]:
                            mispred_counts_by_axis[power]["E"] += 1
                        if parts[3] != canon_parts[3]:
                            mispred_counts_by_axis[power]["S"] += 1

        per_constraint_mispred.append(mispredicted)
        if mispredicted == 0:
            n_perfect += 1

    if not per_constraint_mispred:
        return {"error": "no mixed presheaves found"}

    mean_mispred = sum(per_constraint_mispred) / len(per_constraint_mispred)
    total_mispred = sum(per_constraint_mispred)

    # Axis breakdown: what fraction of mispredictions involve each axis
    axis_totals = {"T": 0, "E": 0, "S": 0}
    for power_data in mispred_counts_by_axis.values():
        for ax, cnt in power_data.items():
            axis_totals[ax] += cnt
    grand_total = sum(axis_totals.values()) or 1

    return {
        "n_mixed_presheaves": n_analyzed,
        "n_perfect_prediction": n_perfect,
        "mean_mispredicted_contexts": round(mean_mispred, 2),
        "mean_mispredicted_pct": round(mean_mispred / 156 * 100, 2),
        "mispredictions_by_power": dict(mispred_counts_by_power.most_common()),
        "mispredictions_by_power_pct": {
            p: round(cnt / total_mispred * 100, 1)
            for p, cnt in mispred_counts_by_power.items()
        },
        "axis_involvement": {
            ax: round(cnt / grand_total * 100, 1)
            for ax, cnt in axis_totals.items()
        },
        "per_power_axis_breakdown": {
            p: dict(ax_data)
            for p, ax_data in mispred_counts_by_axis.items()
        },
    }


# ---------------------------------------------------------------------------
# Formatting
# ---------------------------------------------------------------------------

def print_results(r4pt, r1ctx, r4ctx, rmispred):
    sep = "=" * 65
    print(sep)
    print("ORACLE GAP ANALYSIS")
    print(sep)

    print("\nOriginal 4-point site (best single observer):")
    best_obs = max(r4pt.get("best_observer_frequency", {}).items(),
                   key=lambda x: x[1], default=("?", 0))[0]
    print(f"  Best observer (most frequently optimal): {best_obs}")
    for cat in ["sheaf", "all_constant_presheaf", "mixed_presheaf", "corpus_wide"]:
        if cat in r4pt:
            d = r4pt[cat]
            label = cat.replace("_", " ")
            gap = 100 - d["mean_predictable_pct"]
            print(f"  {label:35s}: {d['mean_predictable_pct']:6.2f}% predictable  "
                  f"(oracle gap {gap:.2f}%,  n={d['n']})")

    print("\nProduct-site: 1 canonical context + block rule (powerless block known):")
    for cat in ["sheaf", "all_constant_presheaf", "mixed_presheaf", "corpus_wide"]:
        if cat in r1ctx:
            d = r1ctx[cat]
            label = cat.replace("_", " ")
            gap = 100 - d["mean_predictable_pct"]
            print(f"  {label:35s}: {d['mean_predictable_pct']:6.2f}% predictable  "
                  f"(oracle gap {gap:.2f}%,  n={d['n']})")

    print("\nProduct-site: 4 canonical contexts + block rule:")
    for cat in ["sheaf", "all_constant_presheaf", "mixed_presheaf", "corpus_wide"]:
        if cat in r4ctx:
            d = r4ctx[cat]
            label = cat.replace("_", " ")
            gap = 100 - d["mean_predictable_pct"]
            print(f"  {label:35s}: {d['mean_predictable_pct']:6.2f}% predictable  "
                  f"(oracle gap {gap:.2f}%,  n={d['n']})")
    print("  ^ gap = information content of within-block variation")

    print("\nMisprediction breakdown (mixed presheaves, 4-canonical + block rule):")
    print(f"  n mixed presheaves analyzed:     {rmispred.get('n_mixed_presheaves', '?')}")
    print(f"  n with perfect prediction:       {rmispred.get('n_perfect_prediction', '?')}")
    print(f"  mean mispredicted contexts:      {rmispred.get('mean_mispredicted_contexts', '?')} "
          f"/ 156  ({rmispred.get('mean_mispredicted_pct', '?')}%)")
    print("\n  Mispredictions by power block:")
    for p, cnt in (rmispred.get("mispredictions_by_power", {}) or {}).items():
        pct = (rmispred.get("mispredictions_by_power_pct") or {}).get(p, "?")
        print(f"    {p:15s}: {cnt:6d}  ({pct}%)")
    print("\n  Axis involvement (fraction of mispredictions involving each axis):")
    for ax, pct in (rmispred.get("axis_involvement") or {}).items():
        print(f"    {ax}: {pct}%")
    print(sep)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("Loading data...")
    orbits, perspectives = load_data()
    print(f"  Orbits: {len(orbits)} constraints")
    print(f"  Perspectives: {len(perspectives)} constraints")

    print("Computing 4-point oracle gap...")
    r4pt = compute_4point_oracle_gap(orbits, perspectives)

    print("Computing 1-context + block rule oracle gap...")
    r1ctx = compute_1context_block_rule(orbits)

    print("Computing 4-canonical + block rule oracle gap...")
    r4ctx = compute_4canonical_block_rule(orbits)

    print("Computing misprediction breakdown...")
    rmispred = compute_misprediction_breakdown(orbits)

    print_results(r4pt, r1ctx, r4ctx, rmispred)

    results = {
        "corpus_hash": compute_corpus_hash(ROOT / "prolog" / "testsets"),
        "4point_oracle_gap": r4pt,
        "1context_block_rule": r1ctx,
        "4canonical_block_rule": r4ctx,
        "misprediction_breakdown": rmispred,
    }
    with open(RESULTS_PATH, "w") as f:
        json.dump(results, f, indent=2)
    print(f"\nResults saved to {RESULTS_PATH}")


if __name__ == "__main__":
    main()
