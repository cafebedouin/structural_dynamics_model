"""Phase 0: Data loading, extraction, and validation.

Loads enriched_pipeline.json and config.pl parameters.
Extracts chi vectors, weight vectors, types, H1, booleans, and metrics.
Validates chi = epsilon * f_d * scope_mod for every constraint.
"""

import math
import sys
from pathlib import Path

import numpy as np

# Add parent to path for shared imports
ROOT = Path(__file__).resolve().parent.parent
if str(ROOT / "python") not in sys.path:
    sys.path.insert(0, str(ROOT / "python"))

from shared.loader import read_config, load_json, ENRICHED_PIPELINE_JSON

CONTEXTS = ["powerless", "moderate", "institutional", "analytical"]


def sigmoid_f(d, cfg):
    """Compute sigmoid power modifier f(d) matching Prolog constraint_indexing.pl."""
    L = cfg["sigmoid_lower"]
    U = cfg["sigmoid_upper"]
    k = cfg["sigmoid_steepness"]
    d0 = cfg["sigmoid_midpoint"]
    return L + (U - L) / (1.0 + math.exp(-k * (d - d0)))


def load_audit_data(enriched_path=None):
    """Load enriched_pipeline.json and extract structured audit data.

    Returns dict with keys:
        constraints: list of per-constraint dicts (filtered: valid chi)
        n_constraints: int
        config: dict from read_config()
        canonical_sigma: dict mapping context -> f(canonical_d)
        canonical_ratios: dict with r12, r23, r34
        d_patterns: dict mapping d-tuple -> list of constraint indices
    """
    cfg = read_config()
    path = enriched_path or ENRICHED_PIPELINE_JSON
    raw = load_json(path, "enriched_pipeline")
    per_constraint = raw.get("per_constraint", [])

    # Compute canonical sigmoid values
    canonical_d = {
        "powerless": cfg.get("canonical_d_powerless", 1.0),
        "moderate": cfg.get("canonical_d_moderate", 0.6459),
        "institutional": cfg.get("canonical_d_institutional", 0.0),
        "analytical": cfg.get("canonical_d_analytical", 0.725),
    }
    canonical_scope = {
        "powerless": cfg.get("scope_modifier_local", 0.8),
        "moderate": cfg.get("scope_modifier_national", 1.0),
        "institutional": cfg.get("scope_modifier_national", 1.0),
        "analytical": cfg.get("scope_modifier_global", 1.2),
    }
    canonical_sigma = {}
    for ctx in CONTEXTS:
        canonical_sigma[ctx] = sigmoid_f(canonical_d[ctx], cfg)

    # Restriction ratios (framework constants, power-modifier only)
    s = canonical_sigma
    r12 = s["powerless"] / s["moderate"]
    r23 = s["moderate"] / s["institutional"]
    r34 = s["institutional"] / s["analytical"]

    # Extract per-constraint data
    constraints = []
    skipped = []
    for entry in per_constraint:
        cid = entry.get("id")
        pchi = entry.get("perspective_chi", {})

        # Check for null chi
        if not all(ctx in pchi and pchi[ctx].get("chi") is not None for ctx in CONTEXTS):
            skipped.append(cid)
            continue

        chi_vec = [pchi[ctx]["chi"] for ctx in CONTEXTS]
        weight_vec = [pchi[ctx]["f_d"] * pchi[ctx]["scope_mod"] for ctx in CONTEXTS]
        d_vec = tuple(round(pchi[ctx]["d"], 4) for ctx in CONTEXTS)
        eps = entry.get("base_extractiveness", 0.0) or 0.0
        supp = entry.get("suppression", 0.0) or 0.0
        theater = entry.get("theater_ratio", 0.0) or 0.0

        perspectives = entry.get("perspectives", {})
        types_vec = [perspectives.get(ctx, "unknown") for ctx in CONTEXTS]

        constraints.append({
            "id": cid,
            "epsilon": eps,
            "suppression": supp,
            "theater_ratio": theater,
            "chi": chi_vec,
            "weights": weight_vec,
            "d_pattern": d_vec,
            "types": types_vec,
            "h1": entry.get("h1_band", 0),
            "maxent_probs": entry.get("maxent_probs", {}),
            "raw_maxent_probs": entry.get("raw_maxent_probs", {}),
            "signature": entry.get("signature", ""),
            "emerges_naturally": bool(entry.get("emerges_naturally")),
            "requires_active_enforcement": bool(entry.get("requires_active_enforcement")),
            "beneficiaries": entry.get("beneficiaries", []) or [],
            "victims": entry.get("victims", []) or [],
            "claimed_type": entry.get("claimed_type", ""),
            "perspective_chi_raw": pchi,
        })

    # Group by d-pattern
    d_patterns = {}
    for i, c in enumerate(constraints):
        dp = c["d_pattern"]
        d_patterns.setdefault(dp, []).append(i)

    return {
        "constraints": constraints,
        "n_constraints": len(constraints),
        "skipped": skipped,
        "config": cfg,
        "canonical_sigma": canonical_sigma,
        "canonical_scope": canonical_scope,
        "canonical_ratios": {"r12": r12, "r23": r23, "r34": r34},
        "d_patterns": d_patterns,
    }


def validate_chi(data):
    """Validate chi = epsilon * f_d * scope_mod for every constraint.

    Returns (n_valid, n_violations, violations_list).
    """
    violations = []
    n_valid = 0
    for c in data["constraints"]:
        eps = c["epsilon"]
        pchi = c["perspective_chi_raw"]
        for j, ctx in enumerate(CONTEXTS):
            expected = eps * pchi[ctx]["f_d"] * pchi[ctx]["scope_mod"]
            actual = c["chi"][j]
            if abs(expected - actual) > 1e-4:
                violations.append((c["id"], ctx, expected, actual))
            else:
                n_valid += 1
    return n_valid, len(violations), violations


def extract_chi_matrix(constraints):
    """Extract N x 4 matrix of chi values."""
    return np.array([c["chi"] for c in constraints])


def extract_weight_matrix(constraints):
    """Extract N x 4 matrix of stalk weights (f_d * scope_mod)."""
    return np.array([c["weights"] for c in constraints])


def extract_h1_vector(constraints):
    """Extract H1 values as numpy array."""
    return np.array([c["h1"] for c in constraints])


def extract_epsilon_vector(constraints):
    """Extract epsilon values as numpy array."""
    return np.array([c["epsilon"] for c in constraints])
