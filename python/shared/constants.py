"""Shared constants and classification functions for the structural dynamics pipeline.

Canonical definitions of MAXENT_TYPES, BOOLEAN_SPECS, PSI thresholds,
shannon_entropy(), compute_psi(), classify_band(), and classify_coalition(),
previously duplicated across analysis scripts.
"""

import math

# ---------------------------------------------------------------------------
# Type constants
# ---------------------------------------------------------------------------

MAXENT_TYPES = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"]
N_TYPES = len(MAXENT_TYPES)

# ---------------------------------------------------------------------------
# PSI thresholds
# ---------------------------------------------------------------------------

PSI_ROPE_LEANING = 0.3
PSI_SNARE_LEANING = 0.7

# ---------------------------------------------------------------------------
# MaxEnt boolean specs (from maxent_classifier.pl:155-163)
# ---------------------------------------------------------------------------

BOOLEAN_SPECS = [
    ("mountain",     "emerges_naturally",            "required"),
    ("mountain",     "requires_active_enforcement",  "forbidden"),
    ("snare",        "natural_law_without_beneficiary", "forbidden"),
    ("scaffold",     "has_coordination_function",    "required"),
    ("rope",         "emerges_naturally",            "bonus"),
    ("tangled_rope", "natural_law_without_beneficiary", "forbidden"),
    ("tangled_rope", "requires_active_enforcement",  "required"),
    ("tangled_rope", "has_coordination_function",    "required"),
    ("tangled_rope", "has_asymmetric_extraction",    "required"),
]

# ---------------------------------------------------------------------------
# Shannon entropy
# ---------------------------------------------------------------------------

def shannon_entropy(dist):
    """Normalized Shannon entropy of a probability distribution."""
    h = 0.0
    for p in dist.values():
        if p > 1e-15:
            h -= p * math.log(p)
    h_max = math.log(N_TYPES)
    return h / h_max if h_max > 0 else 0.0

# ---------------------------------------------------------------------------
# PSI / band classification
# ---------------------------------------------------------------------------

def compute_psi(dist):
    """psi = P(snare) / (P(rope) + P(snare) + 0.001). Continuous snare-lean in [0,1]."""
    p_rope = dist.get("rope", 0.0)
    p_snare = dist.get("snare", 0.0)
    return p_snare / (p_rope + p_snare + 0.001)


def classify_band(psi):
    """Classify into rope_leaning / genuinely_tangled / snare_leaning."""
    if psi < PSI_ROPE_LEANING:
        return "rope_leaning"
    if psi > PSI_SNARE_LEANING:
        return "snare_leaning"
    return "genuinely_tangled"

# ---------------------------------------------------------------------------
# Coalition classification
# ---------------------------------------------------------------------------

def classify_coalition(orbit_contexts):
    """Classify coalition type from orbit contexts (4 perspectives).

    | Coalition Type | Pattern |
    |---|---|
    | uniform_tangled | All 4 agree on tangled_rope |
    | institutional_dissent | Institutional sees rope/scaffold, majority others see tangled_rope/snare |
    | analytical_dissent | Analytical sees differently from powerless+moderate consensus |
    | split_field | 3+ distinct types across perspectives |
    | other | None of the above |
    """
    if not orbit_contexts or len(orbit_contexts) < 4:
        return "other"

    inst = orbit_contexts.get("institutional")
    anal = orbit_contexts.get("analytical")
    mod = orbit_contexts.get("moderate")
    pwl = orbit_contexts.get("powerless")

    all_types = [inst, anal, mod, pwl]
    distinct = set(all_types)

    # uniform_tangled: all 4 agree on tangled_rope
    if all(t == "tangled_rope" for t in all_types):
        return "uniform_tangled"

    # split_field: 3+ distinct types
    if len(distinct) >= 3:
        return "split_field"

    # institutional_dissent: institutional sees rope/scaffold, majority others see tangled_rope/snare
    others = [anal, mod, pwl]
    if inst in ("rope", "scaffold"):
        tangled_snare_count = sum(1 for t in others if t in ("tangled_rope", "snare"))
        if tangled_snare_count >= 2:
            return "institutional_dissent"

    # analytical_dissent: analytical sees differently from powerless+moderate consensus
    if pwl == mod and anal != pwl:
        return "analytical_dissent"

    return "other"
