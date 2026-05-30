#!/usr/bin/env python3
"""
Demotion pass — sort every engine param into:
  perturbed-and-survived     (instrument: kernel)
  perturbable-but-unperturbed (instrument: kernel — backlog)
  reachable-but-locked       (instrument: kernel — coverage>0, fold_survival=1.0 universally)
  unperturbable-by-construction (instrument: kernel — coverage=0 on all kernels)

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

PERTURBED_SURVIVED      = "perturbed-and-survived"
PERTURBABLE_UNPERTURBED = "perturbable-but-unperturbed"
REACHABLE_LOCKED        = "reachable-but-locked"
UNPERTURBABLE           = "unperturbable-by-construction"

# ---------------------------------------------------------------------------
# Confirmed governing params (from enhanced_report.py _WITNESSED_PARAMS and
# perturb() runs). Every entry requires a pasted perturb witness before it
# can appear here — do NOT classify from code-reading alone.
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

    # tangled_rope_chi_floor: FINAL-TYPE flips witnessed (naturalized↔tangled_rope).
    # Flips only on false_ci_rope readings; false_natural_law readings (animal_moral_status
    # kernel) are covered but locked — coverage>0, fold_survival=1.0. Per-kernel-per-param:
    # locked where false_natural_law, free where false_ci_rope (OQ-30 confirmed).
    # Perturb witness 2026-05-29: ±5% from 0.40, 6 affected kernels total.
    "tangled_rope_chi_floor": {
        "kernels": [
            "ai_risk_governance_priority", "jurisprudential_method_kernel",
            "vaccine_mandate_balance", "sovereign_legitimacy",
            "equal_protection_clause", "woman_female_category",
        ],
        "boundary_pct": -5.0,  # direction: lower floor exposes naturalized→tangled_rope
        "flips": 12,            # 3 flips × 4 kernels at 0.38; 6 more at 0.42
        "coverage": 0.173,      # max coverage: jurisprudential_method_kernel at 0.38
    },
}

# ---------------------------------------------------------------------------
# Reachable-but-locked: coverage>0 on some kernels, fold_survival=1.0 universally
# on those kernels (signature override absorbs every metric flip).
# Requires per-kernel perturb witness before entry. Currently empty: no param
# has been found where every kernel it reaches is signature-locked.
# ---------------------------------------------------------------------------

_REACHABLE_LOCKED: dict[str, str] = {
    # (none yet — tangled_rope_chi_floor moved to _WITNESSED because it flips
    # on false_ci_rope kernels; false_natural_law kernels show reachable-but-locked
    # behavior per-kernel but that is a reading-level finding, not a param-level one)
}

# ---------------------------------------------------------------------------
# Genuinely unperturbable: coverage=0 on ALL kernels under the kernel instrument.
# Perturb witness required for each entry (not inferred from rules).
# ---------------------------------------------------------------------------

_GENUINELY_UNPERTURBABLE: dict[str, str] = {
    # power_modifier_analytical: coverage=0 on all 38 kernels at ±8.7%.
    # Legacy chi path only — σ(analytical) path does not reach the canonical
    # χ = ε × f(d) × σ(S) decision point that the kernel instrument exercises.
    # Perturb witness 2026-05-29: 38 blind/stable at 1.05/1.15/1.25.
    "power_modifier_analytical": (
        "coverage=0 on all 38 kernels (legacy chi path; "
        "not on canonical χ=ε×f(d)×σ(S) decision path)"
    ),

    # Scope exclusions: regional/continental/universal appear in no canonical context.
    # coverage=0 on all 38 kernels confirmed by perturb 2026-05-29.
    "scope_modifier_regional":    "coverage=0 on all 38 kernels (scope excluded from product site)",
    "scope_modifier_continental": "coverage=0 on all 38 kernels (scope excluded from product site)",
    "scope_modifier_universal":   "coverage=0 on all 38 kernels (scope excluded from product site)",
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


def load_no_kernel_readings(testsets_dir: Path) -> list[dict]:
    """Return list of testset files with no cs_kernel_id fact.

    These readings are outside the kernel instrument's scope. They are NOT
    added to the param count. Each entry carries:
      file: stem name
      status: per-instrument probe result (see census in ISSUES.md OQ-32 and
              reading_backlog section of _summarize output)
    """
    has_kernel = re.compile(r"cs_kernel_id\(")
    readings = []
    for pl in sorted(testsets_dir.glob("*.pl")):
        text = pl.read_text(encoding="utf-8", errors="replace")
        if not has_kernel.search(text):
            readings.append({"file": pl.stem})
    return readings

# ---------------------------------------------------------------------------
# Priority heuristic for perturbable-but-unperturbed
# ---------------------------------------------------------------------------

def _priority(param: str) -> int:
    """Lower = higher priority for witnessing.

    Epsilon params beat chi params: snare_epsilon_floor produced final-type flips
    through the signature layer. tangled_rope_chi_floor (now witnessed) showed the
    same is possible for chi params at false_ci_rope readings.
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
        if param in _GENUINELY_UNPERTURBABLE:
            category = UNPERTURBABLE
            reason = _GENUINELY_UNPERTURBABLE[param]
            priority = None
            kernel_coverage = None
        elif param in _REACHABLE_LOCKED:
            category = REACHABLE_LOCKED
            reason = _REACHABLE_LOCKED[param]
            priority = None
            kernel_coverage = None
        elif param in _WITNESSED:
            w = _WITNESSED[param]
            category = PERTURBED_SURVIVED
            reason = (f"boundary at {w['boundary_pct']:+.1f}%: "
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
            "instrument": "kernel",
        })

    # Sort: unperturbable first (settled), reachable-locked next (settled),
    # then survived (settled), then backlog by priority
    _cat_order = {
        UNPERTURBABLE: 0,
        REACHABLE_LOCKED: 1,
        PERTURBED_SURVIVED: 2,
        PERTURBABLE_UNPERTURBED: 3,
    }
    rows.sort(key=lambda r: (_cat_order[r["category"]], r.get("priority") or 99, r["param"]))
    return rows


def _summarize(rows: list[dict], kernel_ids: set[str], no_kernel: list[dict]) -> None:
    counts = {
        PERTURBED_SURVIVED: 0,
        PERTURBABLE_UNPERTURBED: 0,
        REACHABLE_LOCKED: 0,
        UNPERTURBABLE: 0,
    }
    for r in rows:
        counts[r["category"]] += 1

    print(f"\nDemotion pass: {len(rows)} numeric params, {len(kernel_ids)} kernels")
    print(f"  (instrument: kernel — all counts relative to kernel instrument)")
    print(f"  {UNPERTURBABLE}:  {counts[UNPERTURBABLE]}")
    print(f"  {REACHABLE_LOCKED}:         {counts[REACHABLE_LOCKED]}")
    print(f"  {PERTURBED_SURVIVED}:     {counts[PERTURBED_SURVIVED]}")
    print(f"  {PERTURBABLE_UNPERTURBED}: {counts[PERTURBABLE_UNPERTURBED]}")
    total = sum(counts.values())
    print(f"  TOTAL: {total}")

    backlog = [r for r in rows if r["category"] == PERTURBABLE_UNPERTURBED]
    print(f"\nWitness backlog (top 10 by priority):")
    for r in backlog[:10]:
        print(f"  [{r['priority']}] {r['param']} = {r['value']}")

    # Reading backlog — separate granularity, NOT added to param count
    print(f"\nReading backlog (no cs_kernel_id — outside kernel instrument):")
    print(f"  N={len(no_kernel)} readings on current corpus (c70e6a2b1aad)")
    print(f"  = 56.5% of 223-testset corpus; confirmed by full linkage census:")
    print(f"    43 no CS linkage; 32 contradiction-only; 44 partial CS; 7 full-CS-no-kernel-id")
    print(f"    (7 full-CS with cs_axiom/cs_reading_relation but no cs_kernel_id — potential authoring gap)")
    print(f"    All 126 correctly excluded: kernel instrument uses cs_kernel_id only")
    print(f"  Class capability: witnessed on testsets_3000 via bifurcation_sweep.py")
    print(f"    (14 final-type flips, false_ci_rope readings, snare_chi_floor=0.655)")
    print(f"    — corpus boundary holds: testsets_3000 ≠ c70e6a2b1aad; these entries UNRUN")
    print(f"  Per-instrument status on current corpus (census 2026-05-29):")
    print(f"    epsilon_sensitivity.py (MaxEnt Fisher): runnable-flat")
    print(f"      witness: behavioral_competence_reading, fisher_analytical_raw=1.129")
    print(f"    bifurcation_sweep.py: broken — OQ-32 (path bug, parent.parent after reorg)")
    print(f"    cognitive_displacement_sweep.py: broken — same reorg path bug")
    print(f"    persistence_sweep.py: broken — same reorg path bug")
    print(f"    product_site_delta_sweep.py: broken — same reorg path bug")
    print(f"    representation_robustness_sweep.py: broken — same reorg path bug")
    print(f"    structural_config_sensitivity.py: broken — same reorg path bug")
    print(f"    range_sweep.py: out-of-scope (hardcodes testsets_3000 corpus)")
    print(f"    f2d_sensitivity_crossing.py: unprobed (Python-only, no kernel dependency)")
    print(f"    game_theory_delta_sensitivity.py: unprobed (Python-only, no kernel dependency)")
    print(f"    game_theory_pi_sensitivity.py: unprobed (Python-only, no kernel dependency)")
    print(f"    position_geometry_metric_sensitivity.py: unprobed (Python-only, needs bc_coupling_audit/sotu)")
    print(f"  NOTE: reading count ({len(no_kernel)}) is NOT added to param total ({len(rows)})")


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
    no_kernel = load_no_kernel_readings(testsets_dir)
    _summarize(rows, kernel_ids, no_kernel)

    if a.json_out:
        sys.path.insert(0, str(ROOT / "python"))
        from sweeps.perturb import _compute_corpus_hash
        out = {
            "corpus_hash": _compute_corpus_hash(testsets_dir),
            "n_params": len(rows),
            "n_kernels": len(kernel_ids),
            "rows": rows,
            "reading_backlog": {
                "corpus": "c70e6a2b1aad",
                "n_readings": len(no_kernel),
                "readings": no_kernel,
                "instrument_status": {
                    "epsilon_sensitivity": "runnable-flat (behavioral_competence_reading fisher_analytical_raw=1.129)",
                    "bifurcation_sweep": "broken — OQ-32 (parent.parent path bug after 2026-05-28 reorg)",
                    "cognitive_displacement_sweep": "broken — same reorg path bug as OQ-32",
                    "persistence_sweep": "broken — same reorg path bug as OQ-32",
                    "product_site_delta_sweep": "broken — same reorg path bug as OQ-32",
                    "representation_robustness_sweep": "broken — same reorg path bug as OQ-32",
                    "structural_config_sensitivity": "broken — same reorg path bug as OQ-32",
                    "range_sweep": "out-of-scope (hardcodes testsets_3000 corpus)",
                    "f2d_sensitivity_crossing": "unprobed (Python-only, reads d/epsilon/scope_mod, no kernel dependency)",
                    "game_theory_delta_sensitivity": "unprobed (Python-only, reads perspectives/perspective_chi, no kernel dependency)",
                    "game_theory_pi_sensitivity": "unprobed (Python-only, same as delta)",
                    "position_geometry_metric_sensitivity": "unprobed (Python-only, needs bc_coupling_audit/sotu files)",
                },
                "class_capability_witness": {
                    "script": "python/sweeps/bifurcation_sweep.py",
                    "result_file": "python/bifurcation_results.json",
                    "corpus": "testsets_3000",
                    "flips": 14,
                    "param": "snare_chi_floor",
                    "critical_value": 0.654844,
                    "note": "testsets_3000 ≠ current corpus c70e6a2b1aad; class capability witnessed, current-corpus entries UNRUN",
                },
                "note": "n_readings is NOT added to n_params; separate instrument, separate granularity",
            },
        }
        Path(a.json_out).write_text(json.dumps(out, indent=2))
        print(f"\nFull results written to {a.json_out}")


if __name__ == "__main__":
    main()
