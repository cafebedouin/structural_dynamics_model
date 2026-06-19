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
CI_PATH = PROLOG_DIR / "constraint_indexing.pl"

# ---------------------------------------------------------------------------
# Category constants
# ---------------------------------------------------------------------------

PERTURBED_SURVIVED      = "perturbed-and-survived"
PERTURBABLE_UNPERTURBED = "perturbable-but-unperturbed"
REACHABLE_LOCKED        = "reachable-but-locked"
UNPERTURBABLE           = "unperturbable-by-construction"
SHADOWED                = "inert-at-current-config (shadowed)"
ERRORED_UNTESTED        = "errored-untested (float sweep config-rejected; no witness)"

# ---------------------------------------------------------------------------
# Confirmed governing params (from enhanced_report.py _WITNESSED_PARAMS and
# perturb() runs). Every entry requires a pasted perturb witness before it
# can appear here — do NOT classify from code-reading alone.
# ---------------------------------------------------------------------------

_WITNESSED: dict[str, dict] = {
    # ── witness_backlog.py batch 2026-05-29 (outputs/witness_backlog_results.json) ──
    # Only entries with pasted perturb witness. Values: max coverage, max flips across kernels.

    # snare_epsilon_floor x end_of_life_decision_authority (pre-batch):
    "snare_epsilon_floor": {
        "kernels": ["end_of_life_decision_authority"],
        "boundary_pct": 8.7,
        "flips": 39,
        "coverage": 0.167,
    },

    # tangled_rope_chi_floor (pre-batch, ±5% sweep):
    "tangled_rope_chi_floor": {
        "kernels": [
            "ai_risk_governance_priority", "jurisprudential_method_kernel",
            "vaccine_mandate_balance", "sovereign_legitimacy",
            "equal_protection_clause", "woman_female_category",
        ],
        "boundary_pct": -5.0,
        "flips": 12,
        "coverage": 0.173,
    },

    # ── ±10% batch survivors (new, 2026-05-29) ──

    "rope_chi_ceiling": {
        "kernels": ["sovereign_legitimacy"],
        "boundary_pct": -10.0,  # both directions; sovereign_legitimacy cov=0.197
        "flips": 20,
        "coverage": 0.197,
    },
    "snare_chi_floor": {
        "kernels": [
            "ai_risk_governance_priority", "equal_protection_clause",
            "honor_settlement_legitimacy", "jurisprudential_method_kernel",
            "nuclear_impossibility_kernel", "second_amendment_text",
            "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category",
        ],
        "boundary_pct": -10.0,  # both directions; max coverage=0.538 (honor_settlement_legitimacy)
        "flips": 32,            # max flips=32 (vaccine_mandate_balance)
        "coverage": 0.538,
    },
    "tangled_rope_chi_ceil": {
        "kernels": ["latin_correctness"],
        "boundary_pct": -10.0,  # 0.9→0.81; cov=0.167, 39 flips
        "flips": 39,
        "coverage": 0.167,
    },
    "em_constrained": {
        "kernels": ["ai_risk_governance_priority", "jurisprudential_method_kernel", "vaccine_mandate_balance"],
        "boundary_pct": 10.0,  # +10% (0.02→0.022); cov=0.006, 3 flips each kernel
        "flips": 3,
        "coverage": 0.006,
    },
    "em_trapped": {
        "kernels": ["equal_protection_clause", "nuclear_impossibility_kernel",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 0.05→0.045; cov=0.010, 3-4 flips per kernel
        "flips": 4,
        "coverage": 0.010,
    },
    "piton_theater_floor": {
        "kernels": ["latin_correctness"],
        "boundary_pct": -10.0,  # 0.7→0.63; cov=0.083, fold_survival=0.917
        "flips": 7,              # estimated from 0.917 × 84 contexts
        "coverage": 0.083,
    },
    "prh_analytical____": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "latin_correctness", "legitimacy_of_imposed_practice",
                    "nuclear_impossibility_kernel", "second_amendment_text", "sovereign_legitimacy",
                    "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 0.72→0.648; ai_risk cov=0.028
        "flips": 8,
        "coverage": 0.028,
    },
    "prh_institutional_true__": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause",
                    "jurisprudential_method_kernel", "legitimacy_of_imposed_practice",
                    "nuclear_impossibility_kernel", "second_amendment_text",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 0.15→0.135; cov=0.013
        "flips": 4,
        "coverage": 0.013,
    },
    "prh_moderate___true": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "latin_correctness", "legitimacy_of_imposed_practice",
                    "nuclear_impossibility_kernel", "second_amendment_text", "sovereign_legitimacy",
                    "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 0.7→0.63; cov=0.019
        "flips": 6,
        "coverage": 0.019,
    },
    "prh_organized___true": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "jurisprudential_method_kernel",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 0.45→0.405; cov=0.006
        "flips": 3,
        "coverage": 0.006,
    },
    "prh_powerless___true": {
        "kernels": ["honor_settlement_legitimacy", "sovereign_legitimacy", "vaccine_mandate_balance"],
        "boundary_pct": -10.0,  # 0.85→0.765; cov=0.064 (honor_settlement)
        "flips": 10,
        "coverage": 0.064,
    },
    "scope_modifier_global": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "latin_correctness", "nuclear_impossibility_kernel",
                    "second_amendment_text", "sovereign_legitimacy", "vaccine_mandate_balance",
                    "woman_female_category"],
        "boundary_pct": -10.0,  # 1.2→1.08; cov=0.006 (ai_risk); +10% also flips some
        "flips": 3,
        "coverage": 0.006,
    },
    "scope_modifier_local": {
        "kernels": ["equal_protection_clause", "honor_settlement_legitimacy", "latin_correctness",
                    "legitimacy_of_imposed_practice", "nuclear_impossibility_kernel",
                    "second_amendment_text", "sovereign_legitimacy", "vaccine_mandate_balance",
                    "woman_female_category"],
        "boundary_pct": -10.0,  # 0.8→0.72; cov=0.010 (equal_protection)
        "flips": 3,
        "coverage": 0.010,
    },
    "scope_modifier_national": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "nuclear_impossibility_kernel",
                    "second_amendment_text", "sovereign_legitimacy", "vaccine_mandate_balance",
                    "woman_female_category"],
        "boundary_pct": -10.0,  # 1.0→0.9; cov=0.034 (ai_risk)
        "flips": 8,
        "coverage": 0.034,
    },
    "sigmoid_lower": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "legitimacy_of_imposed_practice",
                    "nuclear_impossibility_kernel", "second_amendment_text",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # -0.2→-0.22; cov=0.028 (ai_risk)
        "flips": 8,
        "coverage": 0.028,
    },
    "sigmoid_midpoint": {
        "kernels": ["ai_risk_governance_priority", "animal_moral_status", "competence_exercise_validity",
                    "equal_protection_clause", "honor_settlement_legitimacy", "jurisprudential_method_kernel",
                    "kodashim_corpus", "latin_correctness", "legitimacy_of_imposed_practice",
                    "market_as_natural_default", "nuclear_impossibility_kernel", "second_amendment_text",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 0.5→0.45; max cov=0.094 (ai_risk)
        "flips": 30,             # estimated max (ai_risk at 0.906 fold_survival over ~300 contexts)
        "coverage": 0.094,
    },
    "sigmoid_steepness": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "legitimacy_of_imposed_practice",
                    "nuclear_impossibility_kernel", "second_amendment_text",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 6.0→5.4; cov=0.053
        "flips": 16,
        "coverage": 0.053,
    },
    "sigmoid_upper": {
        "kernels": ["ai_risk_governance_priority", "equal_protection_clause", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "latin_correctness", "legitimacy_of_imposed_practice",
                    "nuclear_impossibility_kernel", "second_amendment_text",
                    "sovereign_legitimacy", "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -10.0,  # 1.5→1.35; cov=0.053
        "flips": 16,
        "coverage": 0.053,
    },
    "snare_suppression_floor": {
        "kernels": ["end_of_life_decision_authority", "honor_settlement_legitimacy",
                    "jurisprudential_method_kernel", "latin_correctness", "second_amendment_text",
                    "vaccine_mandate_balance"],
        "boundary_pct": 10.0,   # 0.6→0.66; latin_correctness cov=0.083, fold_survival=0.917
        "flips": 7,
        "coverage": 0.083,
    },

    # ── integer-step batch 2026-05-29 (outputs/witness_backlog_integer_results.json) ──

    # boltzmann_min_classifications: +1 (3→4) flips rope→scaffold in kodashim_corpus.
    # Mechanism: raising the minimum required Boltzmann classifications triggers
    # coordination_dead (or similar) path, shifting scaffold-eligible constraints.
    "boltzmann_min_classifications": {
        "kernels": ["kodashim_corpus"],
        "boundary_pct": 33.3,  # +1/3 (3→4); cov=0.333, fold_survival=0.667, 156 flips
        "flips": 156,
        "coverage": 0.333,
    },
    # critical_mass_threshold: both directions (−1 and +1) produce flips across 8+ kernels.
    # Bidirectional boundary — lowers at 2 (tangled_rope→naturalized) and raises at 4 (naturalized→snare).
    "critical_mass_threshold": {
        "kernels": ["end_of_life_decision_authority", "equal_protection_clause",
                    "honor_settlement_legitimacy", "jurisprudential_method_kernel",
                    "nuclear_impossibility_kernel", "sovereign_legitimacy",
                    "vaccine_mandate_balance", "woman_female_category"],
        "boundary_pct": -33.3,  # both directions; max cov=0.250 (honor, val=2), fs=0.750
        "flips": 87,
        "coverage": 0.250,
    },
    # fcr_override_enabled: disabling (1→0) produces tangled_rope→scaffold in multiple kernels.
    # Large flips (cov=0.333–0.500). Some → unknown (classification failure without FCR override).
    # Note: tangled_rope→unknown means fcr_override is load-bearing for classification path.
    "fcr_override_enabled": {
        "kernels": ["latin_correctness", "nuclear_impossibility_kernel", "reformation_event_boundary",
                    "sovereign_legitimacy", "statute_of_anne_ip_foundation", "vaccine_mandate_balance"],
        "boundary_pct": -100.0,  # disable (1→0); max cov=0.500 (reformation_event_boundary, statute_of_anne)
        "flips": 156,
        "coverage": 0.500,
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

    # Integer-typed params: ±1 integer-step sweep 2026-05-29, coverage=0 at all valid values.
    # Enable flags: valid range [0,1] by oneof constraint — val=2 schema-rejected, not untested.
    # Count thresholds: valid range [val-1, val, val+1] ran clean (no range constraint hit).
    # Witness: outputs/witness_backlog_integer_results.json.
    "abductive_enabled":             "coverage=0 at valid integer range [0,1] (abductive disabled → no type change)",
    "cohomology_enabled":            "coverage=0 at valid integer range [0,1] (cohomology disabled → no type change)",
    "fpn_enabled":                   "coverage=0 at valid integer range [0,1] (FPN disabled → no type change)",
    "maxent_enabled":                "coverage=0 at valid integer range [0,1] (MaxEnt disabled → no type change)",
    "network_drift_hub_escalation":  "coverage=0 at valid integer range [0,1]",
    "post_synthesis_enabled":        "coverage=0 at valid integer range [0,1]",
    "trajectory_enabled":            "coverage=0 at valid integer range [0,1]",
    "abductive_hub_conflict_h1_threshold": "coverage=0 at [3,4,5] (integer ±1)",
    "abductive_stress_convergence_min":    "coverage=0 at [3,4,5] (integer ±1)",
    "constructed_beneficiary_min":         "coverage=0 at [1,2,3] (integer ±1)",
    "fpn_max_iterations":                  "coverage=0 at [19,20,21] (integer ±1)",
    "network_cascade_count_threshold":     "coverage=0 at [2,3,4] (integer ±1)",
    "network_contamination_risk_threshold":"coverage=0 at [1,2,3] (integer ±1)",
    "network_hub_degree_threshold":        "coverage=0 at [2,3,4] (integer ±1)",
    "network_shared_agent_min":            "coverage=0 at [1,2] (val=0 schema-rejected; ±1 above baseline clean)",
    "post_synthesis_green_trigger_threshold": "coverage=0 at [1,2,3] (integer ±1)",
}

# ---------------------------------------------------------------------------
# ERRORED_UNTESTED: params whose config.pl value has no decimal point.
# The ±10% float sweep generates values like 0.9 or 1.1 which fail Prolog
# config_schema type check ("expected integer") — error from 2026-05-29 batch:
#   ERROR: CONFIG ERROR: param(abductive_enabled, 0.9) has wrong type (expected integer)
# These are NOT unperturbable-by-construction (that needs witnessed coverage=0).
# These are NOT inert (they produced no run, not a clean inert run).
# They are untested. The ±1 integer-step sweep witnesses them (run:
#   python3 python/sweeps/witness_backlog.py --integer-only ).
# 19 params as of 2026-05-29 (grep: grep "^\s*param(" prolog/config.pl |
#   grep -vE ",\s*-?[0-9]+\." | grep -E ",\s*-?[0-9]+").
_ERRORED_UNTESTED: dict[str, str] = {
    # All 19 integer-typed params have now been swept via ±1 integer steps
    # (witness_backlog.py --integer-only, 2026-05-29).
    # 3 survived (boltzmann_min_classifications, critical_mass_threshold, fcr_override_enabled)
    # → moved to _WITNESSED above.
    # 16 were inert (coverage=0 at all valid integer values) → moved to _GENUINELY_UNPERTURBABLE
    # below. The val+1 errors for enable flags (oneof([0,1]) schema constraint) are structurally
    # invalid domain, not untested. ERRORED_UNTESTED is now empty.
}

# ---------------------------------------------------------------------------
# SHADOWED: on declared path, inert at current config due to a flag/profile.
# Coverage=0 expected; batch confirms. Must NOT be labeled unperturbable-by-
# construction — they are perturbable if the blocking flag changes.
# ---------------------------------------------------------------------------

_SHADOWED: dict[str, str] = {
    # positional_displacement/2 × 6 power levels
    # Shadowing mechanism: cognitive_displacement_profile=uniform (config.pl).
    # When profile=uniform, resolve_displacement uses global cognitive_displacement
    # param instead of per-position positional_displacement. If profile is changed
    # to 'positional', these become live. Batch coverage=0 confirms shadowed, not dead.
    "pd_powerless":     "positional_displacement path; shadowed by cognitive_displacement_profile=uniform",
    "pd_moderate":      "positional_displacement path; shadowed by cognitive_displacement_profile=uniform",
    "pd_powerful":      "positional_displacement path; shadowed by cognitive_displacement_profile=uniform",
    "pd_organized":     "positional_displacement path; shadowed by cognitive_displacement_profile=uniform",
    "pd_institutional": "positional_displacement path; shadowed by cognitive_displacement_profile=uniform",
    "pd_analytical":    "positional_displacement path; shadowed by cognitive_displacement_profile=uniform",
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


def load_supplementary_params(ci_path: Path) -> dict[str, float]:
    """Read the 23 numeric declarations in constraint_indexing.pl.

    Returns {param_key: value} using the same key convention as perturb.py:
      PRH: prh_{power}_{arg2}_{arg3}
      EM:  em_{exit_option}
      PD:  pd_{power}  (SHADOWED — cognitive_displacement_profile=uniform)
    """
    text = ci_path.read_text(encoding="utf-8")
    result: dict[str, float] = {}
    prh_pat = re.compile(
        r"^power_role_heuristic\(\s*(\w+)\s*,\s*(\w+|_)\s*,\s*(\w+|_)\s*,\s*(-?\d+(?:\.\d+)?)\s*\)\.",
        re.MULTILINE,
    )
    for m in prh_pat.finditer(text):
        key = f"prh_{m.group(1)}_{m.group(2)}_{m.group(3)}"
        result[key] = float(m.group(4))
    em_pat = re.compile(
        r"^exit_modulation\(\s*(\w+)\s*,\s*(-?\d+(?:\.\d+)?)\s*\)\.",
        re.MULTILINE,
    )
    for m in em_pat.finditer(text):
        result[f"em_{m.group(1)}"] = float(m.group(2))
    pd_pat = re.compile(
        r"^positional_displacement\(\s*(\w+)\s*,\s*(-?\d+(?:\.\d+)?)\s*\)\.",
        re.MULTILINE,
    )
    for m in pd_pat.finditer(text):
        result[f"pd_{m.group(1)}"] = float(m.group(2))
    return result


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
    supp   = load_supplementary_params(CI_PATH)
    all_params = {**params, **supp}  # 168 + 23 = 191
    kernel_ids = load_kernel_ids(testsets_dir)

    rows: list[dict] = []

    for param, value in sorted(all_params.items()):
        if param in _SHADOWED:
            category = SHADOWED
            reason = _SHADOWED[param]
            priority = None
            kernel_coverage = None
        elif param in _ERRORED_UNTESTED:
            category = ERRORED_UNTESTED
            reason = _ERRORED_UNTESTED[param]
            priority = None
            kernel_coverage = None
        elif param in _GENUINELY_UNPERTURBABLE:
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

    # Sort: shadowed + unperturbable + reachable-locked (settled), survived, then backlog
    _cat_order = {
        SHADOWED: 0,
        ERRORED_UNTESTED: 1,
        UNPERTURBABLE: 2,
        REACHABLE_LOCKED: 3,
        PERTURBED_SURVIVED: 4,
        PERTURBABLE_UNPERTURBED: 5,
    }
    rows.sort(key=lambda r: (
        _cat_order[r["category"]],
        99 if r.get("priority") is None else r.get("priority"),  # None=settled, 0=epsilon>1=chi>2=other
        r["param"],
    ))
    return rows


def _summarize(rows: list[dict], kernel_ids: set[str], no_kernel: list[dict]) -> None:
    counts = {
        SHADOWED: 0,
        ERRORED_UNTESTED: 0,
        PERTURBED_SURVIVED: 0,
        PERTURBABLE_UNPERTURBED: 0,
        REACHABLE_LOCKED: 0,
        UNPERTURBABLE: 0,
    }
    for r in rows:
        counts[r["category"]] += 1

    print(f"\nDemotion pass: {len(rows)} numeric params (168 config + 23 supplementary), {len(kernel_ids)} kernels")
    print(f"  (instrument: kernel — all counts relative to kernel instrument)")
    print(f"  {SHADOWED}:  {counts[SHADOWED]}")
    print(f"    (on declared path but blocked by current config flag; coverage=0 expected)")
    print(f"  {ERRORED_UNTESTED}: {counts[ERRORED_UNTESTED]}")
    print(f"    (float ±10% config-rejected; NO WITNESS EXISTS — not inert, not swept)")
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
        from corpus_hash import compute_corpus_hash as _compute_corpus_hash
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
