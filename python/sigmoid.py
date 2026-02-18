"""
Sigmoid Directionality Module (v5.0)
====================================
Shared Python implementation of the sigmoid power modifier f(d).

Parameters match prolog/config.pl section 4C exactly.

f(d) = L + (U - L) / (1 + e^(-k*(d - d0)))

where d is directionality in [0.0, 1.0].
"""

import math
import sys
from pathlib import Path

from shared.loader import read_config

# =============================================================================
# THRESHOLDS — loaded from prolog/config.pl (single source of truth)
# =============================================================================

_CFG = read_config()

# Sigmoid parameters
SIGMOID_LOWER = _CFG.get('sigmoid_lower', -0.20)
SIGMOID_UPPER = _CFG.get('sigmoid_upper', 1.50)
SIGMOID_MIDPOINT = _CFG.get('sigmoid_midpoint', 0.50)
SIGMOID_STEEPNESS = _CFG.get('sigmoid_steepness', 6.00)

# Canonical d positions
CANONICAL_D = {
    "powerless":     _CFG.get('canonical_d_powerless', 1.00),
    "moderate":      _CFG.get('canonical_d_moderate', 0.6459),
    "powerful":      _CFG.get('canonical_d_powerful', 0.4804),
    "organized":     _CFG.get('canonical_d_organized', 0.3990),
    "institutional": _CFG.get('canonical_d_institutional', 0.00),
    "analytical":    _CFG.get('canonical_d_analytical', 0.7250),
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
