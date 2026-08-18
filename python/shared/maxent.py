"""Shared MaxEnt classifier functions for the structural dynamics pipeline.

Extracted from tangled_decomposition.py to eliminate report-to-report imports.
Contains the full MaxEnt shadow classifier replication: boolean feature evaluators,
profile/prior computation, Gaussian and boolean log-likelihoods, log-sum-exp
normalization, signature overrides, and the top-level maxent_classify() entry point.
"""

import math
from collections import Counter

from shared.constants import MAXENT_TYPES, N_TYPES, BOOLEAN_SPECS
from shared.loader import read_config

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

_CFG = read_config()

BOOLEAN_PENALTY = _CFG.get("maxent_boolean_penalty", -4.0)
BOOLEAN_BONUS = _CFG.get("maxent_boolean_bonus", 1.0)
OVERRIDE_STRENGTH = _CFG.get("maxent_signature_override_strength", 0.95)

# ---------------------------------------------------------------------------
# Boolean features (derived from JSON, matching Prolog definitions)
# ---------------------------------------------------------------------------

def has_coordination_function(c):
    """narrative_ontology.pl:251-252: has beneficiaries."""
    return len(c.get("beneficiaries") or []) > 0


def has_asymmetric_extraction(c):
    """narrative_ontology.pl:259-260: has victims."""
    return len(c.get("victims") or []) > 0


def natural_law_without_beneficiary(c):
    """drl_core.pl:249-252: emerges naturally, no enforcement, no beneficiaries."""
    return (
        bool(c.get("emerges_naturally"))
        and not bool(c.get("requires_active_enforcement"))
        and len(c.get("beneficiaries") or []) == 0
    )


def eval_boolean(c, feature):
    """Evaluate a boolean feature for a constraint."""
    if feature == "emerges_naturally":
        return bool(c.get("emerges_naturally"))
    if feature == "requires_active_enforcement":
        return bool(c.get("requires_active_enforcement"))
    if feature == "has_coordination_function":
        return has_coordination_function(c)
    if feature == "has_asymmetric_extraction":
        return has_asymmetric_extraction(c)
    if feature == "natural_law_without_beneficiary":
        return natural_law_without_beneficiary(c)
    return False

# Default profiles (from maxent_classifier.pl:122-139)
DEFAULT_PROFILES = {
    "mountain":     {"extractiveness": (0.09, 0.08), "suppression": (0.03, 0.02), "theater": (0.02, 0.05)},
    "rope":         {"extractiveness": (0.15, 0.12), "suppression": (0.30, 0.20), "theater": (0.25, 0.15)},
    "tangled_rope": {"extractiveness": (0.61, 0.15), "suppression": (0.66, 0.15), "theater": (0.30, 0.15)},
    "snare":        {"extractiveness": (0.67, 0.15), "suppression": (0.73, 0.12), "theater": (0.28, 0.18)},
    "scaffold":     {"extractiveness": (0.20, 0.12), "suppression": (0.38, 0.20), "theater": (0.14, 0.12)},
    "piton":        {"extractiveness": (0.65, 0.15), "suppression": (0.69, 0.15), "theater": (0.85, 0.08)},
}

# ---------------------------------------------------------------------------
# MaxEnt classifier (Python replication)
# ---------------------------------------------------------------------------

def _analytical_type(c):
    """Get the deterministic type from analytical perspective (default context).

    The Prolog MaxEnt uses default_context = context(agent_power(analytical), ...)
    for both profile computation and priors. This corresponds to the analytical
    perspective type in pipeline_output.json.
    """
    return c.get("perspectives", {}).get("analytical") or c.get("claimed_type")


def compute_profiles(constraints):
    """Compute empirical mean/std profiles from corpus, falling back to defaults.

    Groups constraints by analytical-perspective type (matching Prolog dr_type
    in the default context used by maxent_run).
    """
    profiles = {}
    for typ in MAXENT_TYPES:
        profiles[typ] = {}
        type_constraints = [c for c in constraints.values() if _analytical_type(c) == typ]
        for metric_name, json_key in [("extractiveness", "extractiveness"),
                                       ("suppression", "suppression"),
                                       ("theater", "theater_ratio")]:
            values = [c[json_key] for c in type_constraints
                      if c[json_key] is not None]
            if len(values) >= 2:
                n = len(values)
                mu = sum(values) / n
                variance = sum((x - mu) ** 2 for x in values) / n
                sigma = max(0.01, math.sqrt(variance))
                profiles[typ][metric_name] = (mu, sigma)
            else:
                profiles[typ][metric_name] = DEFAULT_PROFILES[typ][metric_name]
    return profiles


def compute_priors(constraints):
    """Compute corpus-frequency priors with floor at 0.001.

    Uses analytical-perspective type (matching Prolog default_context).
    """
    total = len(constraints)
    if total == 0:
        return {t: 1.0 / N_TYPES for t in MAXENT_TYPES}
    counts = Counter(_analytical_type(c) for c in constraints.values())
    return {t: max(0.001, counts.get(t, 0) / total) for t in MAXENT_TYPES}


def gaussian_ll(x, mu, sigma):
    """Gaussian log-likelihood: -0.5*(x-mu)^2/sigma^2 - log(sigma)."""
    if sigma <= 1e-15:
        return -100.0
    diff = x - mu
    return -0.5 * (diff * diff) / (sigma * sigma) - math.log(sigma)


def boolean_ll(c, typ):
    """Compute total boolean log-likelihood contribution for a type."""
    total = 0.0
    for spec_type, feature, spec in BOOLEAN_SPECS:
        if spec_type != typ:
            continue
        val = eval_boolean(c, feature)
        if spec == "required":
            total += 0.0 if val else BOOLEAN_PENALTY
        elif spec == "forbidden":
            total += BOOLEAN_PENALTY if val else 0.0
        elif spec == "bonus":
            total += BOOLEAN_BONUS if val else 0.0
    return total


def log_sum_exp_normalize(type_ll_pairs):
    """Normalize log-likelihoods to probabilities via log-sum-exp."""
    if not type_ll_pairs:
        return {}
    max_ll = max(ll for _, ll in type_ll_pairs)
    exps = []
    for typ, ll in type_ll_pairs:
        shifted = ll - max_ll
        exps.append((typ, math.exp(shifted) if shifted > -500 else 0.0))
    total = sum(e for _, e in exps)
    if total > 1e-30:
        return {typ: e / total for typ, e in exps}
    # Uniform fallback
    return {typ: 1.0 / N_TYPES for typ, _ in type_ll_pairs}


def apply_signature_override(signature, dist):
    """Apply signature-based overrides matching maxent_classifier.pl:297-324."""
    # Unconditional overrides
    # OQ-296 (confirmed 2026-08-18): the "natural_law" key is dead — the engine
    # never emits that signature (live-leg census 0), so this branch has never
    # fired. The other keys are live. Mirrors maxent_classifier.pl's arm.
    unconditional = {
        "natural_law": "mountain",
        "false_natural_law": "tangled_rope",
        "coupling_invariant_rope": "rope",
    }
    # Conditional overrides (boost by 3x)
    conditional = {
        "false_ci_rope": "tangled_rope",
        "coordination_scaffold": "rope",
        "constructed_low_extraction": "rope",
        "constructed_high_extraction": "tangled_rope",
        "constructed_constraint": "tangled_rope",
    }

    if signature in unconditional:
        target = unconditional[signature]
        remainder = (1.0 - OVERRIDE_STRENGTH) / (N_TYPES - 1)
        return {t: (OVERRIDE_STRENGTH if t == target else remainder) for t in MAXENT_TYPES}

    if signature in conditional:
        target = conditional[signature]
        boosted = {}
        for t, p in dist.items():
            boosted[t] = p * 3.0 if t == target else p
        total = sum(boosted.values())
        if total > 1e-15:
            return {t: v / total for t, v in boosted.items()}

    return dist


def maxent_classify(constraints, apply_overrides=True):
    """Run the MaxEnt classifier on all constraints. Returns {cid: {type: prob}}.

    Args:
        constraints: dict of constraint data keyed by id.
        apply_overrides: if True (default), apply signature-based overrides
            matching Prolog behavior. If False, return raw MaxEnt distributions
            without overrides.
    """
    profiles = compute_profiles(constraints)
    priors = compute_priors(constraints)

    distributions = {}
    for cid, c in constraints.items():
        eps = c["extractiveness"] if c["extractiveness"] is not None else 0.0
        supp = c["suppression"] if c["suppression"] is not None else 0.0
        theater = c["theater_ratio"] if c["theater_ratio"] is not None else 0.0

        type_lls = []
        for typ in MAXENT_TYPES:
            # Continuous log-likelihood
            cont_ll = 0.0
            for metric_name, val in [("extractiveness", eps),
                                     ("suppression", supp),
                                     ("theater", theater)]:
                mu, sigma = profiles[typ][metric_name]
                cont_ll += gaussian_ll(val, mu, sigma)

            # Boolean log-likelihood
            bool_ll = boolean_ll(c, typ)

            # Prior
            prior = priors.get(typ, 0.001)
            prior_ll = math.log(prior) if prior > 1e-15 else -10.0

            total_ll = cont_ll + bool_ll + prior_ll
            type_lls.append((typ, total_ll))

        dist = log_sum_exp_normalize(type_lls)
        if apply_overrides:
            dist = apply_signature_override(c.get("signature"), dist)
        distributions[cid] = dist

    return distributions
