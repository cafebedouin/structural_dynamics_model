"""
Sigmoid Directionality Module (v5.0)
====================================
Shared Python implementation of the sigmoid power modifier f(d).

Parameters match prolog/config.pl section 4C exactly.

f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))

where d is directionality in [0.0, 1.0].
"""

import math

# Sigmoid parameters (match config.pl)
SIGMOID_LOWER = -0.20       # L: lower asymptote
SIGMOID_UPPER = 1.50        # U: upper asymptote
SIGMOID_MIDPOINT = 0.50     # d0: inflection point
SIGMOID_STEEPNESS = 6.00    # k: steepness

# Canonical d positions (match config.pl)
# Mid-range atoms match legacy pi exactly; extremes have small residuals
# (institutional: f(0.0)=-0.12 vs -0.20, powerless: f(1.0)=1.42 vs 1.50)
CANONICAL_D = {
    "powerless":     1.00,
    "moderate":      0.6459,
    "powerful":      0.4804,
    "organized":     0.3990,
    "institutional": 0.00,
    "analytical":    0.7250,
}


def sigmoid_f(d, lower=SIGMOID_LOWER, upper=SIGMOID_UPPER,
              midpoint=SIGMOID_MIDPOINT, steepness=SIGMOID_STEEPNESS):
    """Compute the sigmoid power modifier from directionality value d.

    f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))
    """
    range_ = upper - lower
    exponent = -steepness * (d - midpoint)
    return lower + range_ / (1 + math.exp(exponent))


def power_modifier(power_atom):
    """Return the sigmoid-derived power modifier for a power atom.

    Uses the canonical d value for the power atom, then applies sigmoid_f.
    This is the v5.0 replacement for hardcoded POWER_MODIFIERS dicts.
    """
    d = CANONICAL_D[power_atom]
    return sigmoid_f(d)


# Pre-computed power modifiers for convenience (match legacy values closely)
POWER_MODIFIERS = {atom: power_modifier(atom) for atom in CANONICAL_D}
