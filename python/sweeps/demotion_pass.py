#!/usr/bin/env python3
"""
Demotion pass — sort every engine param into:
  perturbed-and-survived     (instrument)
  perturbable-but-unperturbed (fabrication-with-an-option)
  unperturbable-by-construction (declared floor)

Output drives the witness backlog for _WITNESSED_PARAMS expansion.
Witnessing follows sorting; the sort tells you which kernels to witness first.

See: docs/engine_handoff.md §4.4, ISSUES.md OQ-30, OQ-31.

Usage:
    python3 python/sweeps/demotion_pass.py
    python3 python/sweeps/demotion_pass.py --json-out outputs/demotion_pass_results.json
"""

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"

# ---------------------------------------------------------------------------
# Category constants
# ---------------------------------------------------------------------------

PERTURBED_SURVIVED   = "perturbed-and-survived"
PERTURBABLE_UNPERTURBED = "perturbable-but-unperturbed"
UNPERTURBABLE        = "unperturbable-by-construction"

# ---------------------------------------------------------------------------
# Known unperturbable params (empirically confirmed or declared by design)
# Record reason; do NOT infer from code reading alone.
# ---------------------------------------------------------------------------

_UNPERTURBABLE: dict[str, str] = {
    # Legacy chi path only — zero dr_type flips confirmed by bifurcation sweep.
    # Documented in config.pl:63 comment.
    "power_modifier_analytical": "legacy chi path only; zero dr_type flips (bifurcation sweep)",

    # Signature-locked on ALL TESTED kernels (end_of_life_decision_authority: blind;
    # animal_moral_status: coverage>0, fold_survival=1.0 — false_natural_law unconditional).
    # NOTE: "all tested kernels" ≠ "all kernels by construction." A false_ci_rope reading
    # with chi near the floor and sufficient metric variance could still flip. If the demotion
    # sort output shows such a reading, move it to perturbable-but-unperturbed. See OQ-30.
    "tangled_rope_chi_floor": "signature-locked on all tested kernels (see OQ-30)",

    # Scope exclusions: regional/continental/universal scopes appear in no canonical context
    # and their scope_modifier values have not been validated. Documented in CLAUDE.md
    # Critical Distinctions / site_contexts_product.
    "scope_modifier_regional":     "scope excluded from product site (calibration-based, no canonical context)",
    "scope_modifier_continental":  "scope excluded from product site (calibration-based, no canonical context)",
    "scope_modifier_universal":    "scope excluded from product site (calibration-based, no canonical context)",
}

# ---------------------------------------------------------------------------
# Confirmed governing params (from enhanced_report.py _WITNESSED_PARAMS)
# ---------------------------------------------------------------------------

_WITNESSED: dict[str, dict] = {
    # snare_epsilon_floor x end_of_life_decision_authority:
    # boundary at +8.7% (0.46→0.50), 39 flips, coverage=0.167
    "snare_epsilon_floor": {
        "kernels": ["end_of_life_decision_authority"],
        "boundary_pct": 8.7,
        "flips": 39,
        "coverage": 0.167,
    },
}

# ---------------------------------------------------------------------------
# Param loader
# ---------------------------------------------------------------------------

def load_numeric_params(config_pl: Path) -> dict[str, float]:
    """Read all param(name, value). clauses from config.pl; return numeric ones."""
    text = config_pl.read_text(encoding="utf-8")
    pattern = re.compile(r"^\s*param\(\s*(\w+)\s*,\s*(-?\d+(?:\.\d+)?)\s*\)\.",
                         re.MULTILINE)
    return {m.group(1): float(m.group(2)) for m in pattern.finditer(text)}


def load_kernel_ids(testsets_dir: Path) -> set[str]:
    """Return set of distinct kernel_ids from cs_kernel_id/2 facts."""
    pattern = re.compile(r"cs_kernel_id\(\s*\w+\s*,\s*(\w+)\s*\)")
    ids: set[str] = set()
    for pl in testsets_dir.glob("*.pl"):
        for m in pattern.finditer(pl.read_text(encoding="utf-8", errors="replace")):
            ids.add(m.group(1))
    return ids

# ---------------------------------------------------------------------------
# Priority heuristic for perturbable-but-unperturbed
# ---------------------------------------------------------------------------

def _priority(param: str) -> int:
    """Lower = higher priority for witnessing.

    Epsilon params beat chi params: witness evidence shows epsilon params
    (snare_epsilon_floor) produce final-type flips through the signature layer where
    chi params (tangled_rope_chi_floor) are signature-locked on all tested kernels.
    """
    if "epsilon" in param:
        return 0
    if "chi" in param:
        return 1
    return 2


# ---------------------------------------------------------------------------
# Main sort
# ---------------------------------------------------------------------------

def run_demotion_pass(config_pl: Path, testsets_dir: Path) -> list[dict]:
    params = load_numeric_params(config_pl)
    kernel_ids = load_kernel_ids(testsets_dir)

    rows: list[dict] = []

    for param, value in sorted(params.items()):
        if param in _UNPERTURBABLE:
            category = UNPERTURBABLE
            reason = _UNPERTURBABLE[param]
            priority = None
            kernel_coverage = None
        elif param in _WITNESSED:
            w = _WITNESSED[param]
            category = PERTURBED_SURVIVED
            reason = (f"boundary at +{w['boundary_pct']}%: "
                      f"{w['flips']} flips, coverage={w['coverage']:.3f} "
                      f"on {', '.join(w['kernels'])}")
            priority = None
            kernel_coverage = len(w["kernels"]) / len(kernel_ids) if kernel_ids else 0
        else:
            category = PERTURBABLE_UNPERTURBED
            reason = "no witness run exists for any kernel"
            priority = _priority(param)
            kernel_coverage = None

        rows.append({
            "param": param,
            "value": value,
            "category": category,
            "reason": reason,
            "priority": priority,
            "kernel_coverage": kernel_coverage,
        })

    # Sort: unperturbable first (settled), then survived (settled), then backlog by priority
    _cat_order = {UNPERTURBABLE: 0, PERTURBED_SURVIVED: 1, PERTURBABLE_UNPERTURBED: 2}
    rows.sort(key=lambda r: (_cat_order[r["category"]], r.get("priority") or 99, r["param"]))
    return rows


def _summarize(rows: list[dict], kernel_ids: set[str]) -> None:
    counts = {PERTURBED_SURVIVED: 0, PERTURBABLE_UNPERTURBED: 0, UNPERTURBABLE: 0}
    for r in rows:
        counts[r["category"]] += 1

    print(f"\nDemotion pass: {len(rows)} numeric params, {len(kernel_ids)} kernels")
    print(f"  {PERTURBED_SURVIVED}:     {counts[PERTURBED_SURVIVED]}")
    print(f"  {PERTURBABLE_UNPERTURBED}: {counts[PERTURBABLE_UNPERTURBED]}")
    print(f"  {UNPERTURBABLE}:  {counts[UNPERTURBABLE]}")

    backlog = [r for r in rows if r["category"] == PERTURBABLE_UNPERTURBED]
    print(f"\nWitness backlog (top 10 by priority):")
    for r in backlog[:10]:
        print(f"  [{r['priority']}] {r['param']} = {r['value']}")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--json-out", help="Write full result JSON to this path")
    a = ap.parse_args()

    config_pl = PROLOG_DIR / "config.pl"
    testsets_dir = PROLOG_DIR / "testsets"

    rows = run_demotion_pass(config_pl, testsets_dir)
    kernel_ids = load_kernel_ids(testsets_dir)
    _summarize(rows, kernel_ids)

    if a.json_out:
        sys.path.insert(0, str(ROOT / "python"))
        from sweeps.perturb import _compute_corpus_hash
        out = {
            "corpus_hash": _compute_corpus_hash(testsets_dir),
            "n_params": len(rows),
            "n_kernels": len(kernel_ids),
            "rows": rows,
        }
        Path(a.json_out).write_text(json.dumps(out, indent=2))
        print(f"\nFull results written to {a.json_out}")


if __name__ == "__main__":
    main()
