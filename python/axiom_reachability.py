"""Axiom Reachability Analysis for the DR Engine.

Enumerates all orbit patterns (4-element type tuples) that the classify_from_metrics
cascade can produce for some valid input, and compares to the 42 orbit patterns
observed in the corpus. The difference — unrealized-but-reachable orbits — is the
structural finding.

Approach: grid sweep over (ε, suppression, theater) at threshold boundaries ±0.01,
for each boolean feature combination and δ displacement value. Pure Python cascade
replication — no Prolog subprocess calls.

Usage: cd python && python3 axiom_reachability.py
"""

import json
import math
import sys
from itertools import product
from collections import Counter, defaultdict
from pathlib import Path

from sigmoid import sigmoid_f, CANONICAL_D
from shared.loader import read_config, load_json, OUTPUT_DIR
from corpus_hash import compute_corpus_hash  # single-source corpus fingerprint (OQ-29)

# =============================================================================
# CONFIG
# =============================================================================

CFG = read_config()

# Cascade thresholds
MOUNTAIN_SUPP_CEIL = CFG["mountain_suppression_ceiling"]      # 0.05
MOUNTAIN_EPS_MAX   = CFG["mountain_extractiveness_max"]        # 0.25
SNARE_CHI_FLOOR    = CFG["snare_chi_floor"]                    # 0.66
SNARE_EPS_FLOOR    = CFG["snare_epsilon_floor"]                # 0.46
SNARE_SUPP_FLOOR   = CFG["snare_suppression_floor"]            # 0.60
ROPE_CHI_CEIL      = CFG["rope_chi_ceiling"]                   # 0.35
ROPE_EPS_CEIL      = CFG["rope_epsilon_ceiling"]               # 0.45
TR_CHI_FLOOR       = CFG["tangled_rope_chi_floor"]             # 0.40
TR_CHI_CEIL        = CFG["tangled_rope_chi_ceil"]              # 0.90
TR_EPS_FLOOR       = CFG["tangled_rope_epsilon_floor"]         # 0.30
TR_SUPP_FLOOR      = CFG["tangled_rope_suppression_floor"]     # 0.40
SCAFFOLD_CHI_CEIL  = CFG["scaffold_extraction_ceil"]           # 0.45
PITON_CHI_CEIL     = CFG["piton_extraction_ceiling"]           # 0.45
PITON_EPS_FLOOR    = CFG["piton_epsilon_floor"]                # 0.10
PITON_THEATER_FLOOR= CFG["piton_theater_floor"]                # 0.70

# Sigmoid params
SIG_L = CFG["sigmoid_lower"]
SIG_U = CFG["sigmoid_upper"]
SIG_M = CFG["sigmoid_midpoint"]
SIG_K = CFG["sigmoid_steepness"]

# Standard observer contexts: (power, scope, exit_options, immutability)
# U1=powerless: biographical+trapped → mountain immutability
# U2=moderate:  biographical+mobile  → rope immutability
# U3=institutional: generational+arbitrage → rope immutability
# U4=analytical: civilizational+analytical → mountain AND rope (non-deterministic)
OBSERVERS = [
    {"name": "powerless",     "power": "powerless",     "scope": CFG["scope_modifier_local"],    "exit": "trapped",    "immutability": "mountain"},
    {"name": "moderate",      "power": "moderate",      "scope": CFG["scope_modifier_national"], "exit": "mobile",     "immutability": "rope"},
    {"name": "institutional", "power": "institutional", "scope": CFG["scope_modifier_national"], "exit": "arbitrage",  "immutability": "rope"},
    {"name": "analytical",    "power": "analytical",    "scope": CFG["scope_modifier_global"],   "exit": "analytical", "immutability": None},  # branching
]

# Power role heuristic table: (power, has_beneficiaries, has_victims) → base_d
# From constraint_indexing.pl:325-335
POWER_ROLE_HEURISTIC = {
    ("powerless",     True):  0.85,  # has_victims=True
    ("powerless",     False): 0.90,  # has_victims=False
    ("moderate",      True):  0.70,
    ("moderate",      False): 0.65,
    ("powerful",      True):  0.50,
    ("powerful",      False): 0.46,
    ("organized",     True):  0.45,
    ("organized",     False): 0.40,
    ("institutional", True):  0.15,  # key: True = has_beneficiaries (not has_victims)
    ("institutional", False): 0.10,
    ("analytical",    True):  0.72,  # same regardless
    ("analytical",    False): 0.72,
}

EXIT_MODULATION = {
    "trapped": 0.05, "identity_locked": 0.04, "constrained": 0.02,
    "mobile": 0.00, "arbitrage": -0.03, "analytical": 0.00,
}

CRITICAL_MASS_THRESHOLD = CFG.get("critical_mass_threshold", 3.0)

ALL_TYPES = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton", "naturalized", "unknown"]

# =============================================================================
# GRID POINTS — threshold boundaries ±0.01 + uniform interior samples
# =============================================================================

def make_grid(thresholds, lo=0.0, hi=1.0, interior_step=0.10):
    """Build grid from threshold boundaries ±0.01 plus uniform samples."""
    pts = {lo, hi}
    for t in thresholds:
        for offset in [-0.01, 0.0, 0.01]:
            v = t + offset
            if lo <= v <= hi:
                pts.add(round(v, 4))
    v = lo
    while v <= hi:
        pts.add(round(v, 4))
        v += interior_step
    return sorted(pts)

EPS_GRID = make_grid(
    [PITON_EPS_FLOOR, MOUNTAIN_EPS_MAX, TR_EPS_FLOOR, ROPE_CHI_CEIL, TR_CHI_FLOOR,
     ROPE_EPS_CEIL, SNARE_EPS_FLOOR],
    0.0, 1.0, 0.10)

SUPP_GRID = make_grid(
    [MOUNTAIN_SUPP_CEIL, TR_SUPP_FLOOR, SNARE_SUPP_FLOOR],
    0.0, 1.0, 0.15)

THEATER_GRID = make_grid([PITON_THEATER_FLOOR], 0.0, 1.0, 0.25)

# δ displacement is subsumed by independent d sweep per observer.
# Any δ shift just changes d_eff = d + δ, which our D_GRID already covers.

# =============================================================================
# BOOLEAN FEATURE COMBINATIONS
# =============================================================================
# 7 booleans: emerges_naturally, requires_enforcement, has_coordination,
#             has_asymmetric_extraction, has_beneficiary, coordination_dead,
#             has_sunset_clause
#
# Constraint: natural_law_wo_beneficiary = emerges_naturally AND NOT requires_enforcement AND NOT has_beneficiary

BOOL_NAMES = [
    "emerges_naturally", "requires_enforcement", "has_coordination",
    "has_asymmetric_extraction", "has_beneficiary", "coordination_dead",
    "has_sunset_clause", "has_victim"
]

def make_bool_combos():
    """Generate all 256 boolean combinations as dicts."""
    combos = []
    for bits in product([False, True], repeat=8):
        combo = dict(zip(BOOL_NAMES, bits))
        # Derived: natural_law_without_beneficiary
        combo["natural_law_wo_beneficiary"] = (
            combo["emerges_naturally"] and
            not combo["requires_enforcement"] and
            not combo["has_beneficiary"]
        )
        # Derived: scaffold_temporality = has_sunset_clause OR NOT requires_enforcement
        combo["scaffold_temporality"] = (
            combo["has_sunset_clause"] or not combo["requires_enforcement"]
        )
        # Derived: has structural data (beneficiary or victim)
        combo["has_structural_data"] = (
            combo["has_beneficiary"] or combo["has_victim"]
        )
        combos.append(combo)
    return combos

BOOL_COMBOS = make_bool_combos()

# =============================================================================
# SIGNATURE OVERRIDE TABLE
# =============================================================================
# Signatures that change the metric-based type. We enumerate which signatures
# are possible for each boolean combo, then apply overrides.

SIGNATURES = [
    "natural_law", "false_natural_law", "coupling_invariant_rope", "false_ci_rope",
    "coordination_scaffold", "piton_signature",
    "constructed_low_extraction", "constructed_high_extraction", "constructed_constraint",
    "ambiguous"
]

def signature_override(metric_type, signature):
    """Apply signature override to metric-based type. Returns adjusted type."""
    # Unconditional overrides (strongest)
    if signature == "natural_law":
        return "mountain"
    if signature == "false_natural_law":
        return "tangled_rope"
    if signature == "coupling_invariant_rope":
        return "rope"
    if signature == "false_ci_rope":
        # With FCR override enabled + no perspectival variance → tangled_rope
        # For reachability: consider both branches
        return "tangled_rope"  # override case (most common)

    # Conditional overrides (depend on metric_type)
    if signature == "coordination_scaffold":
        if metric_type in ("mountain", "unknown"):
            return "rope"
    elif signature == "constructed_low_extraction":
        if metric_type == "mountain":
            return "rope"
        if metric_type == "unknown":
            return "rope"
    elif signature == "constructed_high_extraction":
        if metric_type == "mountain":
            return "tangled_rope"
        if metric_type == "unknown":
            return "snare"
    elif signature == "constructed_constraint":
        if metric_type in ("mountain", "unknown"):
            return "tangled_rope"
    elif signature == "piton_signature":
        if metric_type == "unknown":
            return "piton"

    # No override — keep metric type
    return metric_type


def fcr_override_both_branches(metric_type):
    """FCR signature can preserve metric type (if perspectival variance) or override to tangled_rope."""
    return {metric_type, "tangled_rope"}

# =============================================================================
# CASCADE REPLICATION
# =============================================================================

def classify_at_observer(eps, supp, theater, chi, immutability, bools):
    """Replicate classify_from_metrics/6 for one observer position.

    Returns the metric-based type (before signature overrides).
    immutability: 'mountain' or 'rope' for this observer context.
    """
    nlwb = bools["natural_law_wo_beneficiary"]

    # 1. Mountain
    if (supp <= MOUNTAIN_SUPP_CEIL and
        eps <= MOUNTAIN_EPS_MAX and
        bools["emerges_naturally"] and
        immutability == "mountain"):
        return "mountain"

    # 2. Piton (dead coordination)
    if bools["coordination_dead"]:
        if eps > PITON_EPS_FLOOR and theater >= PITON_THEATER_FLOOR:
            return "piton"

    # 3. Snare
    if (not nlwb and
        chi >= SNARE_CHI_FLOOR and
        eps >= SNARE_EPS_FLOOR and
        supp >= SNARE_SUPP_FLOOR):
        # snare_immutability_check: any standard context sees rope
        # U2 and U3 always see rope, so this ALWAYS passes
        return "snare"

    # 4. Scaffold
    if (chi <= SCAFFOLD_CHI_CEIL and
        bools["has_coordination"] and
        bools["scaffold_temporality"] and
        not (theater > 0.70)):
        return "scaffold"

    # 5. Rope
    if chi <= ROPE_CHI_CEIL:
        eps_ok = (chi <= 0 or eps <= ROPE_EPS_CEIL)
        immut_ok = (immutability == "rope" or bools["emerges_naturally"])
        if eps_ok and immut_ok:
            return "rope"

    # 6. Tangled rope
    if (not nlwb and
        TR_CHI_FLOOR <= chi <= TR_CHI_CEIL and
        eps >= TR_EPS_FLOOR and
        supp >= TR_SUPP_FLOOR and
        bools["requires_enforcement"] and
        bools["has_coordination"] and
        bools["has_asymmetric_extraction"]):
        return "tangled_rope"

    # 7. Piton (fallback)
    if (chi <= PITON_CHI_CEIL and
        eps > PITON_EPS_FLOOR and
        theater >= PITON_THEATER_FLOOR):
        return "piton"

    # 8. Naturalized
    if eps > ROPE_EPS_CEIL and chi < TR_CHI_FLOOR:
        return "naturalized"

    # 9. Unknown
    return "unknown"


def compute_chi(eps, d, scope):
    """χ = ε × f(d) × σ(S)"""
    f_d = sigmoid_f(d, SIG_L, SIG_U, SIG_M, SIG_K)
    return eps * f_d * scope

# =============================================================================
# D GRID — directionality can be overridden per-constraint per-observer to any
# value in [0, 1]. Sweep d independently per observer at threshold-critical points.
# =============================================================================

# f(d) crosses chi thresholds at different d values depending on ε and scope.
# Use a dense d grid covering the full range with extra points near the sigmoid
# midpoint (d=0.5) where f changes fastest.
D_GRID = sorted({
    0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45,
    0.48, 0.49, 0.50, 0.51, 0.52, 0.55, 0.60, 0.65, 0.70, 0.72,
    0.75, 0.80, 0.85, 0.90, 0.95, 1.00
})

# =============================================================================
# GRID SWEEP
# =============================================================================

def sweep():
    """Run the full grid sweep with independent d per observer.

    Since observers are independent (snare_immutability_check always passes because
    U2/U3 always see rope immutability), we can sweep d per observer independently.

    For each (ε, supp, theater, bools): compute the set of achievable types at each
    observer across all d values, then form all 4-tuples from the Cartesian product.
    """
    reachable = set()
    orbit_sources = defaultdict(lambda: {
        "metric_only": False, "signatures": set(),
        "delta_values": set(), "example_input": None,
    })

    total = len(EPS_GRID) * len(SUPP_GRID) * len(THEATER_GRID) * len(BOOL_COMBOS)
    done = 0
    report_every = total // 20 or 1

    for bools in BOOL_COMBOS:
        for eps in EPS_GRID:
            # Pre-compute achievable chi values for each observer across all d
            # chi = eps * f(d) * scope
            chi_by_d = {}
            for obs_idx, obs in enumerate(OBSERVERS):
                chi_by_d[obs_idx] = [compute_chi(eps, d, obs["scope"]) for d in D_GRID]

            for supp in SUPP_GRID:
                for theater in THEATER_GRID:
                    done += 1
                    if done % report_every == 0:
                        pct = done * 100 // total
                        print(f"\r  Sweep: {pct}%", end="", flush=True)

                    # For each observer, compute all achievable types
                    types_per_obs = []
                    for obs_idx, obs in enumerate(OBSERVERS):
                        immutabilities = ["mountain", "rope"] if obs["immutability"] is None else [obs["immutability"]]
                        obs_types = set()
                        for chi in chi_by_d[obs_idx]:
                            for immut in immutabilities:
                                t = classify_at_observer(eps, supp, theater, chi, immut, bools)
                                obs_types.add(t)
                        types_per_obs.append(obs_types)

                    # Form all 4-tuples from Cartesian product
                    for t1 in types_per_obs[0]:
                        for t2 in types_per_obs[1]:
                            for t3 in types_per_obs[2]:
                                for t4 in types_per_obs[3]:
                                    metric_orbit = (t1, t2, t3, t4)
                                    if metric_orbit not in reachable:
                                        orbit_sources[metric_orbit]["metric_only"] = True
                                        orbit_sources[metric_orbit]["example_input"] = {
                                            "eps": eps, "supp": supp, "theater": theater,
                                            "bools": {k: v for k, v in bools.items()
                                                      if k in BOOL_NAMES}
                                        }
                                    reachable.add(metric_orbit)

                                    # Apply signature overrides
                                    for sig in SIGNATURES:
                                        adjusted = tuple(
                                            signature_override(mt, sig) for mt in metric_orbit
                                        )
                                        if adjusted != metric_orbit:
                                            if adjusted not in reachable:
                                                orbit_sources[adjusted]["example_input"] = {
                                                    "eps": eps, "supp": supp, "theater": theater,
                                                    "signature": sig,
                                                    "bools": {k: v for k, v in bools.items()
                                                              if k in BOOL_NAMES}
                                                }
                                            orbit_sources[adjusted]["signatures"].add(sig)
                                            reachable.add(adjusted)

    print("\r  Sweep: 100%")
    return reachable, orbit_sources

# =============================================================================
# OBSERVED PATTERNS (from fingerprint_data.json)
# =============================================================================

def load_observed():
    """Load the 42 observed shift patterns from fingerprint_data.json."""
    fp_data = load_json(OUTPUT_DIR / "fingerprint_data.json", "fingerprint_data")
    observed = {}
    for family in fp_data.get("shift_families", []):
        comp = family["components"]
        pattern = (comp["powerless"], comp["moderate"], comp["institutional"], comp["analytical"])
        observed[pattern] = family["count"]
    return observed

# =============================================================================
# INVARIANT CHECKS
# =============================================================================

def compute_h1(orbit):
    """Count disagreeing observer pairs. H¹ ∈ {0,1,2,3,4,5,6}."""
    pairs = [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]
    return sum(1 for i, j in pairs if orbit[i] != orbit[j])


def check_invariants(reachable, observed):
    """Validate known invariants against the reachable set."""
    results = {}

    # 1. H¹ gap: check which H¹ values are reachable
    h1_reachable = defaultdict(list)
    for orbit in reachable:
        h1 = compute_h1(orbit)
        h1_reachable[h1].append(orbit)

    h1_observed = defaultdict(list)
    for orbit in observed:
        h1 = compute_h1(orbit)
        h1_observed[h1].append(orbit)

    results["h1_distribution_reachable"] = {h: len(v) for h, v in sorted(h1_reachable.items())}
    results["h1_distribution_observed"] = {h: len(v) for h, v in sorted(h1_observed.items())}
    results["h1_gap_values"] = sorted(set(range(7)) - set(h1_reachable.keys()))
    results["h1_gap_confirmed"] = (1 not in h1_reachable and 2 not in h1_reachable)

    # 2. Institutional dissent direction: does U3 ever dissent upward?
    extraction_rank = {"mountain": 0, "rope": 1, "naturalized": 1.5, "scaffold": 1.5,
                       "tangled_rope": 2, "piton": 2.5, "snare": 3, "unknown": -1}
    u3_dissents_up = []
    for orbit in reachable:
        u3_rank = extraction_rank.get(orbit[2], -1)
        other_ranks = [extraction_rank.get(orbit[i], -1) for i in [0, 1, 3]]
        # U3 dissents upward if it sees higher extraction than ALL others
        if u3_rank > 0 and all(u3_rank > r for r in other_ranks if r >= 0):
            u3_dissents_up.append(orbit)

    results["institutional_dissent_upward_count"] = len(u3_dissents_up)
    results["institutional_dissent_upward_examples"] = [
        f"shift({o[0]},{o[1]},{o[2]},{o[3]})" for o in u3_dissents_up[:10]
    ]
    results["institutional_always_suppresses"] = len(u3_dissents_up) == 0

    # 3. Mountain invariance: any orbit with mountain at some positions and non-mountain at others?
    mixed_mountain = []
    for orbit in reachable:
        has_mountain = any(t == "mountain" for t in orbit)
        all_mountain = all(t == "mountain" for t in orbit)
        if has_mountain and not all_mountain:
            mixed_mountain.append(orbit)

    results["mixed_mountain_count"] = len(mixed_mountain)
    results["mixed_mountain_examples"] = [
        f"shift({o[0]},{o[1]},{o[2]},{o[3]})" for o in mixed_mountain[:10]
    ]
    results["mountain_is_global_section"] = len(mixed_mountain) == 0

    return results

# =============================================================================
# ANALYSIS AND REPORTING
# =============================================================================

def classify_impossible_by_mechanism(impossible_orbits):
    """Group axiom-impossible orbits by the cascade mechanism that blocks them."""
    mechanisms = defaultdict(list)

    for orbit in impossible_orbits:
        reasons = []

        # Check if the pattern requires contradictory booleans
        # e.g., tangled_rope requires enforcement+coordination+extraction, mountain requires emerges_naturally
        has_mountain = "mountain" in orbit
        has_tr = "tangled_rope" in orbit
        has_scaffold = "scaffold" in orbit
        has_snare = "snare" in orbit

        if has_mountain and has_tr:
            reasons.append("mountain_requires_emerges_naturally_but_TR_requires_enforcement")
        if has_mountain and has_snare:
            reasons.append("mountain_requires_low_eps_but_snare_requires_high_eps")

        # U3 (institutional) structural constraints
        # f(d=0) ≈ -0.12 means χ_U3 is always negative or near-zero
        # Snare at U3 requires χ ≥ 0.66, impossible with standard d
        if orbit[2] == "snare":
            reasons.append("u3_chi_always_negative_blocks_snare")

        # U3 tangled_rope requires 0.40 ≤ χ ≤ 0.90, impossible with negative χ
        if orbit[2] == "tangled_rope":
            reasons.append("u3_chi_always_negative_blocks_tangled_rope")

        if not reasons:
            reasons.append("priority_ordering_or_threshold_interaction")

        for r in reasons:
            mechanisms[r].append(f"shift({orbit[0]},{orbit[1]},{orbit[2]},{orbit[3]})")

    return dict(mechanisms)


def main():
    print("=" * 70)
    print("AXIOM REACHABILITY ANALYSIS — DR Classification Engine")
    print("=" * 70)

    # Load observed patterns
    print("\n1. Loading observed orbit patterns...")
    observed = load_observed()
    print(f"   Observed: {len(observed)} distinct patterns across "
          f"{sum(observed.values())} constraints")

    # Run grid sweep
    print(f"\n2. Grid sweep: {len(EPS_GRID)} ε × {len(SUPP_GRID)} supp × "
          f"{len(THEATER_GRID)} theater × {len(BOOL_COMBOS)} bool × "
          f"{len(D_GRID)} d (per observer)")
    n_combos = len(EPS_GRID) * len(SUPP_GRID) * len(THEATER_GRID) * len(BOOL_COMBOS)
    print(f"   Metric combos: {n_combos:,}, d sweep: {len(D_GRID)} points × 4 observers")

    reachable, orbit_sources = sweep()

    # Compute sets
    observed_set = set(observed.keys())
    unrealized = reachable - observed_set
    missing_observed = observed_set - reachable  # BUG CHECK: should be empty

    # Full type space
    all_possible = set(product(ALL_TYPES, repeat=4))
    axiom_impossible = all_possible - reachable

    coverage = len(observed_set) / len(reachable) if reachable else 0

    # Classify unrealized orbits: metric-only vs signature-gated
    unrealized_metric_only = []
    unrealized_signature_gated = []
    unrealized_details = []

    for orbit in sorted(unrealized):
        src = orbit_sources[orbit]
        is_metric_only = src["metric_only"]
        sigs = src["signatures"]
        detail = {
            "pattern": f"shift({orbit[0]},{orbit[1]},{orbit[2]},{orbit[3]})",
            "h1": compute_h1(orbit),
            "metric_only": is_metric_only,
            "requires_signatures": sorted(sigs) if sigs and not is_metric_only else [],
            "d_sweep": True,  # all d values in [0,1] via D_GRID
            "example_input": src["example_input"],
        }
        unrealized_details.append(detail)
        if is_metric_only:
            unrealized_metric_only.append(orbit)
        else:
            unrealized_signature_gated.append(orbit)

    # Invariant checks
    print("\n3. Checking invariants...")
    invariants = check_invariants(reachable, observed_set)

    # Classify impossible orbits (sample — full set is huge)
    print("\n4. Classifying axiom-impossible orbits...")
    # Only classify a representative sample (orbits with common types)
    common_types = ["mountain", "rope", "tangled_rope", "snare", "naturalized"]
    sample_impossible = [o for o in axiom_impossible
                         if all(t in common_types for t in o)]
    impossible_mechanisms = classify_impossible_by_mechanism(sample_impossible)

    # =======================================================================
    # REPORT
    # =======================================================================
    print("\n" + "=" * 70)
    print("RESULTS")
    print("=" * 70)

    print(f"\n  Observed patterns:            {len(observed_set)}")
    print(f"  Reachable patterns:           {len(reachable)}")
    print(f"  Axiom-impossible patterns:    {len(axiom_impossible)}")
    print(f"  Unrealized-but-reachable:     {len(unrealized)}")
    print(f"    — Metric-only (no sig):     {len(unrealized_metric_only)}")
    print(f"    — Signature-gated:          {len(unrealized_signature_gated)}")
    print(f"  Coverage ratio:               {coverage:.1%}")

    if missing_observed:
        print(f"\n  *** BUG: {len(missing_observed)} observed patterns NOT in reachable set! ***")
        for o in sorted(missing_observed):
            print(f"      shift({o[0]},{o[1]},{o[2]},{o[3]})")

    print(f"\n  H¹ distribution (reachable):")
    for h, count in sorted(invariants["h1_distribution_reachable"].items()):
        marker = " ← GAP" if h in invariants["h1_gap_values"] else ""
        print(f"    H¹={h}: {count} orbits{marker}")
    print(f"  H¹ gap at {{1,2}} confirmed:   {invariants['h1_gap_confirmed']}")

    print(f"\n  H¹ distribution (observed):")
    for h, count in sorted(invariants["h1_distribution_observed"].items()):
        print(f"    H¹={h}: {count} orbits")

    print(f"\n  Institutional dissent upward:  {invariants['institutional_dissent_upward_count']} orbits")
    if invariants["institutional_dissent_upward_examples"]:
        for ex in invariants["institutional_dissent_upward_examples"][:5]:
            print(f"    {ex}")
    print(f"  Institutional always suppresses: {invariants['institutional_always_suppresses']}")

    print(f"\n  Mixed-mountain orbits:         {invariants['mixed_mountain_count']}")
    if invariants["mixed_mountain_examples"]:
        for ex in invariants["mixed_mountain_examples"][:5]:
            print(f"    {ex}")
    print(f"  Mountain is global section:    {invariants['mountain_is_global_section']}")

    # Unrealized-but-reachable details
    print(f"\n  Unrealized-but-reachable orbits ({len(unrealized)}):")
    print(f"  {'Pattern':<55} H¹  {'Source':<15}")
    print(f"  {'-'*55} {'---'} {'-'*15}")
    for d in sorted(unrealized_details, key=lambda x: (-x["h1"], x["pattern"])):
        src = "metric" if d["metric_only"] else "sig:" + ",".join(d["requires_signatures"][:2])
        print(f"  {d['pattern']:<55} {d['h1']}    {src}")

    # Impossible by mechanism (sample)
    print(f"\n  Axiom-impossible mechanisms (sample of {len(sample_impossible)} common-type orbits):")
    for mech, orbits in sorted(impossible_mechanisms.items(), key=lambda x: -len(x[1])):
        print(f"    {mech}: {len(orbits)} orbits")
        for ex in orbits[:3]:
            print(f"      {ex}")
        if len(orbits) > 3:
            print(f"      ... and {len(orbits)-3} more")

    # Write JSON results
    results = {
        "summary": {
            "observed_count": len(observed_set),
            "reachable_count": len(reachable),
            "axiom_impossible_count": len(axiom_impossible),
            "unrealized_count": len(unrealized),
            "unrealized_metric_only_count": len(unrealized_metric_only),
            "unrealized_signature_gated_count": len(unrealized_signature_gated),
            "coverage_ratio": round(coverage, 4),
            "missing_observed_bug": sorted(
                f"shift({o[0]},{o[1]},{o[2]},{o[3]})" for o in missing_observed
            ),
        },
        "observed": sorted(
            f"shift({o[0]},{o[1]},{o[2]},{o[3]})" for o in observed_set
        ),
        "reachable": sorted(
            f"shift({o[0]},{o[1]},{o[2]},{o[3]})" for o in reachable
        ),
        "unrealized_but_reachable": unrealized_details,
        "invariant_checks": invariants,
        "impossible_by_mechanism": impossible_mechanisms,
        "grid_params": {
            "eps_grid_size": len(EPS_GRID),
            "supp_grid_size": len(SUPP_GRID),
            "theater_grid_size": len(THEATER_GRID),
            "bool_combos": len(BOOL_COMBOS),
            "d_grid_size": len(D_GRID),
            "metric_combos": n_combos,
        }
    }

    results["corpus_hash"] = compute_corpus_hash(Path(__file__).resolve().parent.parent / "prolog" / "testsets")
    out_path = OUTPUT_DIR / "axiom_reachability_results.json"
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2, default=str)
    print(f"\n  Results written to {out_path}")

    return results


if __name__ == "__main__":
    main()
