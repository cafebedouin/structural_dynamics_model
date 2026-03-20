#!/usr/bin/env python3
"""Validate constraint story JSON files against the live Prolog engine.

Checks schema compliance, metric thresholds, compilation, linting,
directionality chain completeness, chi boundary proximity, identity_locked
diagnostics, powerless coverage, Boltzmann coordination, measurement
adequacy, and runs engine-level classification/directionality/H1 verification.

Usage:
    python3 python/validate_constraint_story.py json/burnout_mechanism.json
    python3 python/validate_constraint_story.py json/*.json
    python3 python/validate_constraint_story.py --no-engine json/foo.json
    python3 python/validate_constraint_story.py --summary-only json/*.json
    python3 python/validate_constraint_story.py --json-output results.json json/*.json
"""

import argparse
import json
import math
import os
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field, asdict
from enum import Enum
from pathlib import Path
from typing import Optional

# ---------------------------------------------------------------------------
# Resolve project paths
# ---------------------------------------------------------------------------

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_DIR = SCRIPT_DIR.parent
PROLOG_DIR = PROJECT_DIR / "prolog"

# ---------------------------------------------------------------------------
# Import sibling modules
# ---------------------------------------------------------------------------

sys.path.insert(0, str(SCRIPT_DIR))
from generate_constraint_pl import validate_json, generate_pl  # noqa: E402
from linter import lint_file, get_threshold_values_from_config  # noqa: E402

# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

class Severity(Enum):
    CRITICAL = "CRITICAL"
    WARNING = "WARNING"
    INFO = "INFO"
    PASS = "PASS"


@dataclass
class Finding:
    severity: Severity
    section: str
    code: str
    message: str
    detail: Optional[str] = None


@dataclass
class ValidationResult:
    file_path: str
    constraint_id: str = ""
    claimed_type: str = ""
    extractiveness: float = 0.0
    findings: list = field(default_factory=list)
    engine_classifications: dict = field(default_factory=dict)
    declared_classifications: dict = field(default_factory=dict)
    chi_table: dict = field(default_factory=dict)
    d_table: dict = field(default_factory=dict)
    h1_value: Optional[int] = None
    h0_value: Optional[int] = None


# ---------------------------------------------------------------------------
# Power role heuristic + exit modulation tables
# (mirrors constraint_indexing.pl:325-346)
# ---------------------------------------------------------------------------

# power_role_heuristic(Power, HasBeneficiaries, HasVictims, BaseD)
# Key: (power, has_victims) for most; institutional keys on has_beneficiaries
POWER_ROLE_HEURISTIC = {
    ("powerless", True):   0.85,
    ("powerless", False):  0.90,
    ("moderate", True):    0.70,
    ("moderate", False):   0.65,
    ("powerful", True):    0.50,
    ("powerful", False):   0.46,
    ("organized", True):   0.45,
    ("organized", False):  0.40,
    ("analytical", True):  0.72,
    ("analytical", False): 0.72,
}

# institutional is keyed on has_beneficiaries (3rd arg in Prolog)
INSTITUTIONAL_HEURISTIC = {
    True:  0.15,  # institutional + beneficiary
    False: 0.10,  # institutional + non-beneficiary
}

EXIT_MODULATION = {
    "trapped":         0.05,
    "identity_locked": 0.04,
    "constrained":     0.02,
    "mobile":          0.00,
    "arbitrage":      -0.03,
    "analytical":      0.00,
}

GENERIC_GROUP_NAMES = {
    "affected_parties", "stakeholders", "participants", "actors",
    "beneficiaries", "victims", "agents", "subjects", "targets",
    "users", "members", "people", "groups", "entities",
}

# ---------------------------------------------------------------------------
# Config / threshold loading
# ---------------------------------------------------------------------------

def load_thresholds(quiet: bool = False) -> dict:
    """Parse all param/2 numeric values from config.pl."""
    config_path = str(PROLOG_DIR / "config.pl")
    if quiet:
        import io
        old_stderr = sys.stderr
        sys.stderr = io.StringIO()
        try:
            result = get_threshold_values_from_config(config_path)
        finally:
            sys.stderr = old_stderr
        return result
    return get_threshold_values_from_config(config_path)


# ---------------------------------------------------------------------------
# Sigmoid + chi computation (mirrors constraint_indexing.pl:268-417)
# ---------------------------------------------------------------------------

def sigmoid_f(d: float, L: float, U: float, d0: float, k: float) -> float:
    """f(d) = L + (U-L) / (1 + exp(-k*(d-d0)))"""
    exponent = -k * (d - d0)
    # Guard against overflow
    if exponent > 500:
        return L
    if exponent < -500:
        return L + (U - L)
    return L + (U - L) / (1 + math.exp(exponent))


def compute_directionality(power: str, exit_opts: str,
                           has_beneficiaries: bool, has_victims: bool,
                           overrides: dict, thresholds: dict) -> tuple:
    """Compute d value and source label.

    Returns (d_value, source) where source is 'override', 'structural', or 'canonical'.
    """
    # 1. Explicit override
    if power in overrides:
        return (overrides[power], "override")

    # 2. Structural derivation (needs at least one of beneficiaries/victims)
    if has_beneficiaries or has_victims:
        if power == "institutional":
            base_d = INSTITUTIONAL_HEURISTIC.get(has_beneficiaries, 0.10)
        else:
            base_d = POWER_ROLE_HEURISTIC.get((power, has_victims), 0.65)
        exit_mod = EXIT_MODULATION.get(exit_opts, 0.00)
        d = max(0.0, min(1.0, base_d + exit_mod))
        return (d, "structural")

    # 3. Canonical fallback
    canon_key = f"canonical_d_{power}"
    d = thresholds.get(canon_key, 0.50)
    return (d, "canonical")


def compute_chi(epsilon: float, d: float, scope: str, thresholds: dict) -> float:
    """chi = epsilon * f(d) * sigma(scope)"""
    L = thresholds.get("sigmoid_lower", -0.20)
    U = thresholds.get("sigmoid_upper", 1.50)
    d0 = thresholds.get("sigmoid_midpoint", 0.50)
    k = thresholds.get("sigmoid_steepness", 6.0)
    fd = sigmoid_f(d, L, U, d0, k)
    sigma = thresholds.get(f"scope_modifier_{scope}", 1.0)
    return epsilon * fd * sigma


# ===================================================================
# PHASE 1: Static Validation
# ===================================================================

def check_schema(data: dict) -> list:
    """S2.1: Validate JSON against constraint_story_schema.json."""
    errors = validate_json(data)
    if not errors:
        return [Finding(Severity.PASS, "S2.1", "SCHEMA_OK", "Schema validation passed")]
    findings = []
    for err in errors:
        findings.append(Finding(Severity.CRITICAL, "S2.1", "SCHEMA_INVALID", err))
    return findings


def check_metric_thresholds(data: dict, thresholds: dict) -> list:
    """S2.2: Check declared metrics against engine thresholds."""
    findings = []
    bp = data.get("base_properties", {})
    claimed = bp.get("claimed_type", "")
    eps = bp.get("extractiveness", 0)
    supp = bp.get("suppression", 0)
    theater = bp.get("theater_ratio", 0)

    mt_ext_max = thresholds.get("mountain_extractiveness_max", 0.25)
    mt_supp_max = thresholds.get("mountain_suppression_ceiling", 0.05)
    sn_eps_floor = thresholds.get("snare_epsilon_floor", 0.46)
    tr_eps_floor = thresholds.get("tangled_rope_epsilon_floor", 0.30)
    tr_supp_floor = thresholds.get("tangled_rope_suppression_floor", 0.40)
    piton_theater = thresholds.get("piton_theater_floor", 0.70)

    # Mountain checks
    if claimed == "mountain":
        if eps > mt_ext_max:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "MT_EPS",
                f"Mountain eps={eps:.2f} > max={mt_ext_max:.2f}"))
        else:
            findings.append(Finding(Severity.PASS, "S2.2", "MT_EPS_OK",
                f"Mountain eps={eps:.2f} <= {mt_ext_max:.2f}"))
        if supp > mt_supp_max:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "MT_SUPP",
                f"Mountain suppression={supp:.2f} > max={mt_supp_max:.2f}"))
        if not bp.get("emerges_naturally", False):
            findings.append(Finding(Severity.CRITICAL, "S2.2", "MT_NL",
                "Mountain requires emerges_naturally=true"))
        ac = bp.get("accessibility_collapse")
        res = bp.get("resistance")
        if ac is not None and ac < 0.85:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "MT_AC",
                f"Mountain accessibility_collapse={ac:.2f} < 0.85"))
        if res is not None and res > 0.15:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "MT_RES",
                f"Mountain resistance={res:.2f} > 0.15"))

    # Snare checks
    if claimed == "snare":
        if eps < sn_eps_floor:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "SN_EPS",
                f"Snare eps={eps:.2f} < floor={sn_eps_floor:.2f}"))
        if not bp.get("victims"):
            findings.append(Finding(Severity.CRITICAL, "S2.2", "SN_VICTIMS",
                "Snare requires victims[]"))

    # Tangled rope checks
    if claimed == "tangled_rope":
        if not bp.get("requires_active_enforcement", False):
            findings.append(Finding(Severity.CRITICAL, "S2.2", "TR_ENFORCE",
                "Tangled rope requires requires_active_enforcement=true"))
        if not bp.get("beneficiaries"):
            findings.append(Finding(Severity.CRITICAL, "S2.2", "TR_BENEF",
                "Tangled rope requires beneficiaries[]"))
        if not bp.get("victims"):
            findings.append(Finding(Severity.CRITICAL, "S2.2", "TR_VICTIMS",
                "Tangled rope requires victims[]"))

    # Piton checks
    if claimed == "piton":
        if theater < piton_theater:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "PI_THEATER",
                f"Piton theater_ratio={theater:.2f} < floor={piton_theater:.2f}"))

    # Scaffold + enforcement → sunset
    if claimed == "scaffold" and bp.get("requires_active_enforcement", False):
        if not bp.get("has_sunset_clause", False):
            findings.append(Finding(Severity.WARNING, "S2.2", "SC_SUNSET",
                "Scaffold with enforcement requires has_sunset_clause=true"))

    # High-extraction gates
    if eps > sn_eps_floor:
        measurements = data.get("measurements", [])
        omegas = data.get("omegas", [])
        if len(measurements) < 6:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "HI_MEAS",
                f"eps={eps:.2f} > {sn_eps_floor:.2f} requires >=6 measurements, got {len(measurements)}"))
        if not omegas:
            findings.append(Finding(Severity.CRITICAL, "S2.2", "HI_OMEGA",
                f"eps={eps:.2f} > {sn_eps_floor:.2f} requires omegas[]"))

    # Mandatrophy gate
    if eps > 0.70:
        if not bp.get("mandatrophy_resolved", False):
            findings.append(Finding(Severity.CRITICAL, "S2.2", "MANDATROPHY",
                f"eps={eps:.2f} > 0.70 requires mandatrophy_resolved=true"))

    if not findings:
        findings.append(Finding(Severity.PASS, "S2.2", "THRESHOLDS_OK",
            "All metric thresholds consistent with claimed type"))

    return findings


def check_compilation(data: dict) -> tuple:
    """S2.3: Compile JSON to .pl and verify output structure.

    Returns (findings, pl_content_or_None).
    """
    findings = []
    try:
        pl = generate_pl(data)
    except Exception as e:
        findings.append(Finding(Severity.CRITICAL, "S2.3", "COMPILE_FAIL",
            f"Compilation failed: {e}"))
        return findings, None

    checks = {
        ":- module(": "MODULE_DECL",
        "constraint_metric(": "CONSTRAINT_METRIC",
        "constraint_classification(": "CLASSIFICATION",
        "interval(": "INTERVAL",
    }
    for pattern, code in checks.items():
        if pattern not in pl:
            findings.append(Finding(Severity.CRITICAL, "S2.3", f"MISSING_{code}",
                f"Compiled output missing {pattern}"))

    if not findings:
        findings.append(Finding(Severity.PASS, "S2.3", "COMPILE_OK",
            "Compilation successful, all required predicates present"))

    return findings, pl


def check_lint(pl_content: str) -> list:
    """S2.4: Run linter on compiled .pl output."""
    import io
    findings = []
    # Write to temp file under prolog/testsets/ so linter resolves config.pl
    testsets_dir = PROLOG_DIR / "testsets"
    fd, tmp_path = tempfile.mkstemp(suffix=".pl", prefix="validator_", dir=str(testsets_dir))
    try:
        with os.fdopen(fd, "w") as f:
            f.write(pl_content)
        # Suppress linter's stderr threshold loading message
        old_stderr = sys.stderr
        sys.stderr = io.StringIO()
        try:
            errors = lint_file(tmp_path)
        finally:
            sys.stderr = old_stderr
        if not errors:
            findings.append(Finding(Severity.PASS, "S2.4", "LINT_OK",
                "Linter passed (24 rules)"))
        else:
            for err in errors:
                findings.append(Finding(Severity.WARNING, "S2.4", "LINT", err))
    finally:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass
    return findings


# ===================================================================
# PHASE 2: Structural Checks
# ===================================================================

def check_directionality_chain(data: dict, thresholds: dict) -> list:
    """S3.1: Directionality derivation chain completeness."""
    findings = []
    bp = data.get("base_properties", {})
    claimed = bp.get("claimed_type", "")

    if claimed == "mountain":
        findings.append(Finding(Severity.PASS, "S3.1", "CHAIN_SKIP",
            "Mountain constraint -- directionality derivation not applicable"))
        return findings

    has_benef = bool(bp.get("beneficiaries"))
    has_vict = bool(bp.get("victims"))

    if not has_benef and not has_vict:
        findings.append(Finding(Severity.CRITICAL, "S3.1", "NO_STRUCTURAL_DATA",
            f"No beneficiaries or victims for {claimed}. Engine uses canonical fallback d "
            "for ALL positions -- story is INVISIBLE to delta-band analysis.",
            "93% of delta-band activity concentrates at powerless where effective d ~ 0.50 "
            "for victim-declared constraints, vs canonical d where sigmoid is nearly flat."))
        return findings

    if claimed in ("tangled_rope", "snare") and not has_vict:
        findings.append(Finding(Severity.CRITICAL, "S3.1", "MISSING_VICTIMS",
            f"{claimed} requires victims[] -- missing"))
    if claimed == "tangled_rope" and not has_benef:
        findings.append(Finding(Severity.CRITICAL, "S3.1", "MISSING_BENEF",
            "tangled_rope requires beneficiaries[] -- missing"))

    # Check for generic names
    all_groups = set(bp.get("beneficiaries", []) + bp.get("victims", []))
    generics = all_groups & GENERIC_GROUP_NAMES
    if generics:
        findings.append(Finding(Severity.WARNING, "S3.1", "GENERIC_NAMES",
            f"Generic group names: {sorted(generics)}. Directionality derivation will be imprecise."))

    source = "structural" if (has_benef or has_vict) else "canonical"
    if has_benef and has_vict:
        chain_status = "COMPLETE"
    elif has_benef or has_vict:
        chain_status = "PARTIAL"
    else:
        chain_status = "CANONICAL FALLBACK"

    findings.append(Finding(Severity.INFO, "S3.1", "CHAIN_STATUS",
        f"Derivation chain: {chain_status}",
        f"Beneficiaries: {bp.get('beneficiaries', [])}, Victims: {bp.get('victims', [])}"))

    if not findings or all(f.severity in (Severity.INFO,) for f in findings):
        findings.insert(0, Finding(Severity.PASS, "S3.1", "CHAIN_OK",
            f"Directionality chain {chain_status} with {source} derivation"))

    return findings


def check_chi_boundaries(data: dict, thresholds: dict) -> list:
    """S3.2: Chi proximity to classification boundaries."""
    findings = []
    bp = data.get("base_properties", {})
    eps = bp.get("extractiveness", 0)
    has_benef = bool(bp.get("beneficiaries"))
    has_vict = bool(bp.get("victims"))

    # Build override map
    overrides = {}
    for ov in data.get("directionality_overrides", []):
        overrides[ov["power_atom"]] = ov["d_value"]

    # Boundaries from config
    boundaries = {
        "rope_chi_ceiling": thresholds.get("rope_chi_ceiling", 0.35),
        "tangled_rope_chi_floor": thresholds.get("tangled_rope_chi_floor", 0.40),
        "snare_chi_floor": thresholds.get("snare_chi_floor", 0.66),
        "tangled_rope_chi_ceil": thresholds.get("tangled_rope_chi_ceil", 0.90),
    }
    band_width = 0.10

    delta_band_count = 0
    for p in data.get("perspectives", []):
        power = p["agent_power"]
        scope = p.get("spatial_scope", "national")
        exit_opts = p.get("exit_options", "mobile")

        d, d_source = compute_directionality(power, exit_opts, has_benef, has_vict,
                                             overrides, thresholds)
        chi = compute_chi(eps, d, scope, thresholds)

        # Check boundary proximity
        closest_name, closest_dist = None, float("inf")
        for bname, bval in boundaries.items():
            dist = abs(chi - bval)
            if dist < closest_dist:
                closest_dist = dist
                closest_name = bname

        in_band = closest_dist <= band_width
        label = p.get("label", f"{power}/{p.get('time_horizon')}/{exit_opts}/{scope}")

        detail = (f"d={d:.3f} ({d_source}), f(d)={sigmoid_f(d, thresholds.get('sigmoid_lower', -0.2), thresholds.get('sigmoid_upper', 1.5), thresholds.get('sigmoid_midpoint', 0.5), thresholds.get('sigmoid_steepness', 6.0)):.3f}, "
                  f"chi={chi:.4f}, nearest boundary={closest_name} ({boundaries[closest_name]:.2f}), distance={closest_dist:.4f}")

        if in_band:
            delta_band_count += 1
            findings.append(Finding(Severity.WARNING, "S3.2", "CHI_BOUNDARY",
                f"delta-band ACTIVE: {label} chi={chi:.4f} within {closest_dist:.4f} of {closest_name}",
                detail))
        else:
            findings.append(Finding(Severity.INFO, "S3.2", "CHI_OK",
                f"{label}: chi={chi:.4f}", detail))

        if d_source == "canonical":
            findings.append(Finding(Severity.WARNING, "S3.2", "CANONICAL_D",
                f"{power}: using canonical fallback d={d:.3f}. Engine may derive different d from structural data."))

    if delta_band_count == 0:
        findings.insert(0, Finding(Severity.INFO, "S3.2", "NO_DELTA_BAND",
            "No delta-band activity at any perspective (all chi far from boundaries)"))

    return findings


def check_identity_locked(data: dict) -> list:
    """S3.3: identity_locked diagnostic checks."""
    findings = []
    perspectives = data.get("perspectives", [])
    commentary = data.get("commentary", {})

    il_persp = [p for p in perspectives if p.get("exit_options") == "identity_locked"]
    if not il_persp:
        return [Finding(Severity.PASS, "S3.3", "IL_SKIP", "No identity_locked perspectives")]

    findings.append(Finding(Severity.INFO, "S3.3", "IL_COUNT",
        f"{len(il_persp)} identity_locked perspective(s)"))

    # Check 1: Commentary discusses identity-fusion mechanism
    logic = commentary.get("logic_rationale", "")
    persp_gap = commentary.get("perspectival_gap", "")
    text = (logic + " " + persp_gap).lower()
    identity_terms = ["identity", "fusion", "internalized", "cognitive",
                      "identity_locked", "constituted", "self-concept", "framing"]
    if not any(t in text for t in identity_terms):
        findings.append(Finding(Severity.WARNING, "S3.3", "IL_NO_DISCUSSION",
            "No identity-fusion mechanism discussed in commentary"))

    # Check 2: Paired trapped perspective
    trapped_persp = [p for p in perspectives if p.get("exit_options") == "trapped"]
    il_times = {p["time_horizon"] for p in il_persp}
    trapped_times = {p["time_horizon"] for p in trapped_persp}
    if "biographical" in il_times and "biographical" not in trapped_times:
        findings.append(Finding(Severity.WARNING, "S3.3", "IL_NO_PAIRED",
            "identity_locked at biographical time but no trapped perspective at biographical. "
            "Diagnostic gap (identity_locked -> rope vs trapped -> mountain) cannot fire."))

    # Check 3: Analytical oracle gap
    analytical_il = [p for p in il_persp if p["agent_power"] == "analytical"]
    if analytical_il:
        oracle_terms = ["oracle", "theorem 4", "u4", "meta-cognitive"]
        if not any(t in text for t in oracle_terms):
            findings.append(Finding(Severity.WARNING, "S3.3", "IL_ORACLE",
                "(analytical, identity_locked) present but no oracle gap (Theorem 4) in commentary"))

    # Check 4: Interpersonal decomposition
    domain = data.get("base_properties", {}).get("topic_domain", "")
    network = data.get("network", {})
    interpersonal_terms = ["interpersonal", "relationship", "family", "marriage"]
    if any(t in domain.lower() for t in interpersonal_terms):
        affects = network.get("affects_constraints", [])
        if not affects:
            findings.append(Finding(Severity.WARNING, "S3.3", "IL_DECOMPOSE",
                "Interpersonal constraint with identity_locked but no network.affects_constraints. "
                "Per epsilon-invariance, interpersonal relationships typically decompose into "
                "2-4 structurally distinct stories."))

    return findings


def check_powerless_coverage(data: dict) -> list:
    """S3.4: Powerless-position coverage."""
    findings = []
    bp = data.get("base_properties", {})
    claimed = bp.get("claimed_type", "")
    perspectives = data.get("perspectives", [])

    # Uniform mountains don't need powerless
    if claimed == "mountain":
        all_mt = all(p["classification_type"] == "mountain" for p in perspectives)
        if all_mt:
            return [Finding(Severity.PASS, "S3.4", "PL_SKIP",
                "Uniform mountain -- powerless perspective not required")]

    powerless = [p for p in perspectives if p["agent_power"] == "powerless"]
    if not powerless:
        findings.append(Finding(Severity.CRITICAL, "S3.4", "NO_POWERLESS",
            "No powerless perspective. 93% of delta-band activity concentrates at "
            "powerless position."))
        return findings

    for p in powerless:
        exit_opt = p.get("exit_options", "")
        if exit_opt in ("arbitrage", "analytical"):
            findings.append(Finding(Severity.WARNING, "S3.4", "PL_EXIT",
                f"Powerless with exit_options={exit_opt} is structurally unusual. "
                "Low d values place chi far from sigmoid midpoint."))

    findings.append(Finding(Severity.PASS, "S3.4", "PL_OK",
        f"{len(powerless)} powerless perspective(s) present"))

    return findings


def check_boltzmann(data: dict) -> list:
    """S3.5: Boltzmann coordination type check."""
    findings = []
    bp = data.get("base_properties", {})
    claimed = bp.get("claimed_type", "")
    boltzmann = data.get("boltzmann", {})
    eps = bp.get("extractiveness", 0)
    supp = bp.get("suppression", 0)

    coordination_types = {"rope", "tangled_rope", "scaffold"}
    if claimed in coordination_types and "coordination_type" not in boltzmann:
        findings.append(Finding(Severity.WARNING, "S3.5", "NO_COORD_TYPE",
            f"{claimed} without boltzmann.coordination_type. Engine cannot run "
            "complexity-adjusted Boltzmann threshold analysis."))

    ct = boltzmann.get("coordination_type")
    if ct:
        if ct == "identity_coordination" and eps > 0.40:
            findings.append(Finding(Severity.WARNING, "S3.5", "ID_COORD_RISK",
                f"identity_coordination with eps={eps:.2f}. Identity narratives are common "
                "cover stories. The 0.04 complexity offset gives Boltzmann leeway."))
        if ct == "attachment_coordination" and supp >= 0.40:
            omegas = data.get("omegas", [])
            has_supp_omega = any("suppression" in o.get("id", "") for o in omegas)
            if not has_supp_omega:
                findings.append(Finding(Severity.WARNING, "S3.5", "ATTACH_SUPP",
                    "attachment_coordination with suppression >= 0.40 but no "
                    "suppression_mechanism_ambiguity omega variable."))

    if not findings:
        findings.append(Finding(Severity.PASS, "S3.5", "BOLTZ_OK",
            "Boltzmann coordination checks passed"))

    return findings


def check_measurements(data: dict, thresholds: dict) -> list:
    """S3.6: Measurement adequacy."""
    findings = []
    bp = data.get("base_properties", {})
    eps = bp.get("extractiveness", 0)
    measurements = data.get("measurements", [])
    sn_eps_floor = thresholds.get("snare_epsilon_floor", 0.46)

    if not measurements:
        if eps > sn_eps_floor:
            findings.append(Finding(Severity.CRITICAL, "S3.6", "NO_MEAS",
                f"eps={eps:.2f} > {sn_eps_floor:.2f} requires measurements (min 6)"))
        else:
            findings.append(Finding(Severity.PASS, "S3.6", "MEAS_SKIP",
                "No measurements required"))
        return findings

    # Group by metric
    by_metric = {}
    for m in measurements:
        by_metric.setdefault(m["metric"], []).append((m["time_point"], m["value"]))

    for metric, points in by_metric.items():
        points.sort()
        values = [v for _, v in points]
        n = len(values)

        if n < 2:
            findings.append(Finding(Severity.WARNING, "S3.6", "FEW_POINTS",
                f"{metric}: only {n} point(s)"))
            continue

        increasing = all(values[i] <= values[i + 1] for i in range(n - 1))
        decreasing = all(values[i] >= values[i + 1] for i in range(n - 1))
        is_cyclical = not increasing and not decreasing and n >= 4

        pattern = "cyclical" if is_cyclical else ("monotonic" if (increasing or decreasing) else "irregular")

        if is_cyclical and n < 8:
            findings.append(Finding(Severity.WARNING, "S3.6", "CYCLICAL_FEW",
                f"{metric}: cyclical pattern with only {n} points. Need 8-10 for full cycle."))

        findings.append(Finding(Severity.INFO, "S3.6", "MEAS_DETAIL",
            f"{metric}: {n} points, range [{min(values):.2f}, {max(values):.2f}] ({pattern})"))

    if not any(f.severity in (Severity.CRITICAL, Severity.WARNING) for f in findings):
        findings.insert(0, Finding(Severity.PASS, "S3.6", "MEAS_OK",
            f"Measurement adequacy passed ({len(measurements)} points)"))

    return findings


# ===================================================================
# PHASE 3: Engine Validation (Prolog subprocess)
# ===================================================================

def build_overlay(constraint_id: str, compiled_pl_path: str, perspectives: list) -> str:
    """Build Prolog overlay that loads engine + compiled story and emits tagged output."""
    lines = [
        f"% Auto-generated validator overlay for {constraint_id}",
        ":- [stack].",
        f":- consult('{compiled_pl_path}').",
        "",
    ]

    # Custom context classifications (one per perspective)
    for i, p in enumerate(perspectives):
        pow_ = p["agent_power"]
        time_ = p["time_horizon"]
        exit_ = p["exit_options"]
        scope_ = p["spatial_scope"]
        lines.append(
            f":- (   drl_core:dr_type({constraint_id}, "
            f"context(agent_power({pow_}), time_horizon({time_}), "
            f"exit_options({exit_}), spatial_scope({scope_})), Type{i})"
            f"\n   ->  format('CUSTOM:{constraint_id}:{pow_}:{time_}:{exit_}:{scope_}:~w~n', [Type{i}])"
            f"\n   ;   format('CUSTOM:{constraint_id}:{pow_}:{time_}:{exit_}:{scope_}:unknown~n', [])"
            f"\n)."
        )
    lines.append("")

    # Standard 4-context classifications
    lines.append(
        f":- forall(\n"
        f"    (drl_core:standard_context(Ctx),\n"
        f"     Ctx = context(agent_power(P), _, _, _),\n"
        f"     drl_core:dr_type({constraint_id}, Ctx, T)),\n"
        f"    format('CLASSIFY:{constraint_id}:~w:~w~n', [P, T])\n"
        f")."
    )
    lines.append("")

    # Directionality + chi for standard contexts
    lines.append(
        f":- forall(\n"
        f"    (drl_core:standard_context(Ctx),\n"
        f"     Ctx = context(agent_power(P), _, _, _)),\n"
        f"    (   (constraint_indexing:derive_directionality({constraint_id}, Ctx, D),\n"
        f"         constraint_indexing:extractiveness_for_agent({constraint_id}, Ctx, Chi))\n"
        f"    ->  format('DIRECT:{constraint_id}:~w:~f:~f~n', [P, D, Chi])\n"
        f"    ;   format('DIRECT:{constraint_id}:~w:error:error~n', [P])\n"
        f"    )\n"
        f")."
    )
    lines.append("")

    # H1 computation
    lines.append(
        f":- (   grothendieck_cohomology:cohomological_obstruction({constraint_id}, H0, H1)\n"
        f"   ->  format('H1:{constraint_id}:~w:~w~n', [H0, H1])\n"
        f"   ;   format('H1:{constraint_id}:error:error~n', [])\n"
        f")."
    )
    lines.append("")
    lines.append(":- halt.")

    return "\n".join(lines)


def parse_engine_output(output: str) -> dict:
    """Parse TAG:field:... lines from Prolog stdout+stderr."""
    result = {
        "classify": {},       # {power: type}
        "custom": {},         # {(power,time,exit,scope): type}
        "direct": {},         # {power: (d, chi)}
        "h1": None,           # (h0, h1) or None
        "errors": [],
    }

    for line in output.splitlines():
        line = line.strip()
        if line.startswith("CLASSIFY:"):
            parts = line.split(":")
            if len(parts) == 4:
                _, _cid, power, typ = parts
                result["classify"][power] = typ

        elif line.startswith("CUSTOM:"):
            parts = line.split(":")
            if len(parts) == 7:
                _, _cid, pow_, time_, exit_, scope_, typ = parts
                result["custom"][(pow_, time_, exit_, scope_)] = typ

        elif line.startswith("DIRECT:"):
            parts = line.split(":")
            if len(parts) == 4:
                _, _cid, power, rest = parts[0], parts[1], parts[2], parts[3]
                # rest is "d_val:chi_val" but we split on : already
                # Actually the format is DIRECT:cid:power:d:chi (5 parts)
            if len(parts) == 5:
                _, _cid, power, d_str, chi_str = parts
                try:
                    result["direct"][power] = (float(d_str), float(chi_str))
                except ValueError:
                    result["direct"][power] = (None, None)

        elif line.startswith("H1:"):
            parts = line.split(":")
            if len(parts) == 4:
                _, _cid, h0_str, h1_str = parts
                try:
                    result["h1"] = (int(h0_str), int(h1_str))
                except ValueError:
                    result["h1"] = None

        elif line.startswith("ERROR:"):
            # Capture actual Prolog errors (not warnings)
            result["errors"].append(line)

    return result


def run_engine_validation(constraint_id: str, compiled_pl: str,
                          perspectives: list, timeout_sec: int = 120) -> dict:
    """Run Prolog engine queries for a single constraint.

    Writes compiled .pl and overlay to tempfiles, runs swipl, parses output.
    Returns parsed engine output dict.
    """
    # Write compiled .pl to temp file
    fd_pl, pl_path = tempfile.mkstemp(suffix=".pl", prefix=f"val_{constraint_id}_",
                                       dir=str(PROLOG_DIR / "testsets"))
    # Write overlay
    fd_ov, ov_path = tempfile.mkstemp(suffix=".pl", prefix="val_overlay_",
                                       dir=str(PROLOG_DIR))
    try:
        with os.fdopen(fd_pl, "w") as f:
            f.write(compiled_pl)

        overlay = build_overlay(constraint_id, pl_path, perspectives)
        with os.fdopen(fd_ov, "w") as f:
            f.write(overlay)

        cmd = ["swipl", "-g", f"consult('{ov_path}'), halt(0)."]
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout_sec,
            cwd=str(PROLOG_DIR),
        )
        combined = proc.stdout + "\n" + proc.stderr
        return parse_engine_output(combined)

    except subprocess.TimeoutExpired:
        return {"classify": {}, "custom": {}, "direct": {}, "h1": None,
                "errors": [f"Prolog subprocess timed out after {timeout_sec}s"]}
    except Exception as e:
        return {"classify": {}, "custom": {}, "direct": {}, "h1": None,
                "errors": [str(e)]}
    finally:
        for p in (pl_path, ov_path):
            try:
                os.unlink(p)
            except OSError:
                pass


def check_engine_load(engine_result: dict) -> list:
    """S4.1: Verify the constraint loaded into the engine."""
    findings = []
    if engine_result["errors"]:
        for err in engine_result["errors"]:
            findings.append(Finding(Severity.CRITICAL, "S4.1", "ENGINE_ERROR", err))
    if not engine_result["classify"] and not engine_result["custom"]:
        findings.append(Finding(Severity.CRITICAL, "S4.1", "NO_OUTPUT",
            "Engine produced no classification output"))
    if not findings:
        findings.append(Finding(Severity.PASS, "S4.1", "ENGINE_OK",
            "Engine loaded and produced output"))
    return findings


def check_classification_match(data: dict, engine_result: dict) -> list:
    """S4.2: Compare engine classifications vs declared."""
    findings = []
    perspectives = data.get("perspectives", [])

    for p in perspectives:
        pow_ = p["agent_power"]
        time_ = p["time_horizon"]
        exit_ = p["exit_options"]
        scope_ = p.get("spatial_scope", "national")
        declared = p["classification_type"]
        label = p.get("label", f"{pow_}/{time_}/{exit_}/{scope_}")

        key = (pow_, time_, exit_, scope_)
        engine_type = engine_result["custom"].get(key, "?")

        if engine_type == "?":
            findings.append(Finding(Severity.WARNING, "S4.2", "NO_ENGINE_TYPE",
                f"{label}: no engine result for context {key}"))
        elif engine_type == "unknown":
            findings.append(Finding(Severity.WARNING, "S4.2", "ENGINE_UNKNOWN",
                f"{label}: engine returned 'unknown' (declared={declared})"))
        elif engine_type != declared:
            findings.append(Finding(Severity.CRITICAL, "S4.2", "TYPE_MISMATCH",
                f"{label}: declared={declared}, engine={engine_type}"))
        else:
            findings.append(Finding(Severity.PASS, "S4.2", "TYPE_MATCH",
                f"{label}: {declared} (match)"))

    return findings


def check_directionality_engine(data: dict, engine_result: dict,
                                thresholds: dict) -> list:
    """S4.3: Compare Python-computed d/chi vs engine-computed."""
    findings = []
    bp = data.get("base_properties", {})
    eps = bp.get("extractiveness", 0)
    has_benef = bool(bp.get("beneficiaries"))
    has_vict = bool(bp.get("victims"))
    overrides = {ov["power_atom"]: ov["d_value"]
                 for ov in data.get("directionality_overrides", [])}

    # Standard context exit options for comparison
    std_exit = {
        "powerless": "trapped", "moderate": "mobile",
        "institutional": "arbitrage", "analytical": "analytical",
    }
    std_scope = {
        "powerless": "local", "moderate": "national",
        "institutional": "national", "analytical": "global",
    }

    for power, (eng_d, eng_chi) in engine_result.get("direct", {}).items():
        if eng_d is None or eng_chi is None:
            findings.append(Finding(Severity.WARNING, "S4.3", "DIRECT_ERROR",
                f"{power}: engine could not derive directionality"))
            continue

        exit_ = std_exit.get(power, "mobile")
        py_d, d_source = compute_directionality(power, exit_, has_benef, has_vict,
                                                overrides, thresholds)
        scope = std_scope.get(power, "national")
        py_chi = compute_chi(eps, py_d, scope, thresholds)

        d_diff = abs(py_d - eng_d)
        chi_diff = abs(py_chi - eng_chi)

        if chi_diff > 0.01:
            findings.append(Finding(Severity.WARNING, "S4.3", "CHI_DIVERGE",
                f"{power}: Python chi={py_chi:.4f} vs engine chi={eng_chi:.4f} "
                f"(diff={chi_diff:.4f}). Python d={py_d:.4f} vs engine d={eng_d:.4f}",
                "Divergence may be due to coalition power upgrade or data_repair adjustments."))
        else:
            findings.append(Finding(Severity.PASS, "S4.3", "CHI_AGREE",
                f"{power}: chi={eng_chi:.4f}, d={eng_d:.4f} ({d_source})"))

    if not engine_result.get("direct"):
        findings.append(Finding(Severity.WARNING, "S4.3", "NO_DIRECT",
            "No directionality data from engine"))

    return findings


def check_deltaband_engine(engine_result: dict, thresholds: dict) -> list:
    """S4.4: Delta-band trigger from engine chi values."""
    findings = []
    band_width = 0.10
    boundaries = {
        "rope_chi_ceiling": thresholds.get("rope_chi_ceiling", 0.35),
        "tangled_rope_chi_floor": thresholds.get("tangled_rope_chi_floor", 0.40),
        "snare_chi_floor": thresholds.get("snare_chi_floor", 0.66),
        "tangled_rope_chi_ceil": thresholds.get("tangled_rope_chi_ceil", 0.90),
    }

    active_count = 0
    for power, (d, chi) in engine_result.get("direct", {}).items():
        if chi is None:
            continue
        closest_name, closest_dist = None, float("inf")
        for bname, bval in boundaries.items():
            dist = abs(chi - bval)
            if dist < closest_dist:
                closest_dist = dist
                closest_name = bname

        if closest_dist <= band_width:
            active_count += 1
            findings.append(Finding(Severity.WARNING, "S4.4", "DELTA_ACTIVE",
                f"delta-band ACTIVE: {power} chi={chi:.4f}, "
                f"{closest_name} distance={closest_dist:.4f}"))
        else:
            findings.append(Finding(Severity.INFO, "S4.4", "DELTA_STABLE",
                f"{power}: chi={chi:.4f}, nearest={closest_name} dist={closest_dist:.4f}"))

    if active_count == 0 and engine_result.get("direct"):
        findings.insert(0, Finding(Severity.INFO, "S4.4", "DELTA_NONE",
            "Classification stable: no engine chi within delta-band of any boundary"))

    return findings


def check_h1(engine_result: dict) -> list:
    """S4.5: Report H1 from Grothendieck cohomology."""
    findings = []
    h1_data = engine_result.get("h1")
    if h1_data is None:
        findings.append(Finding(Severity.WARNING, "S4.5", "H1_ERROR",
            "Engine could not compute H1"))
        return findings

    h0, h1 = h1_data
    if h1 == 0:
        sev = Severity.INFO
        msg = f"H0={h0}, H1={h1} -- global section exists, no perspectival fracture"
    elif h1 <= 2:
        sev = Severity.INFO
        msg = f"H0={h0}, H1={h1} -- mild perspectival divergence"
    elif h1 <= 4:
        sev = Severity.INFO
        msg = f"H0={h0}, H1={h1} -- moderate perspectival divergence"
    else:
        sev = Severity.WARNING
        msg = f"H0={h0}, H1={h1} -- extreme perspectival fracture, verify intentional"

    findings.append(Finding(sev, "S4.5", "H1", msg))
    return findings


# ===================================================================
# Report Generation
# ===================================================================

def severity_counts(findings: list) -> dict:
    counts = {"CRITICAL": 0, "WARNING": 0, "INFO": 0, "PASS": 0}
    for f in findings:
        counts[f.severity.value] += 1
    return counts


def generate_report(result: ValidationResult) -> str:
    """Generate structured validation report."""
    lines = []
    w = lines.append
    sep = "=" * 65

    w(sep)
    w(f"CONSTRAINT STORY VALIDATION: {result.constraint_id}")
    w(f"File:  {result.file_path}")
    w(f"Type:  {result.claimed_type}    eps={result.extractiveness:.2f}")
    w(sep)
    w("")

    counts = severity_counts(result.findings)
    w("SUMMARY")
    w("-" * 40)
    w(f"  CRITICAL: {counts['CRITICAL']}  WARNING: {counts['WARNING']}  "
      f"INFO: {counts['INFO']}  PASS: {counts['PASS']}")
    w("")

    # Group findings by section prefix
    sections = {
        "S2": "STATIC VALIDATION",
        "S3": "STRUCTURAL CHECKS",
        "S4": "ENGINE VALIDATION",
    }

    for prefix, title in sections.items():
        section_findings = [f for f in result.findings if f.section.startswith(prefix)]
        if not section_findings:
            continue
        w(f"{title}")
        w("-" * 50)
        for f in section_findings:
            icon = {Severity.CRITICAL: "CRIT", Severity.WARNING: "WARN",
                    Severity.INFO: "INFO", Severity.PASS: "PASS"}[f.severity]
            w(f"  [{icon}] {f.section} {f.message}")
            if f.detail and f.severity in (Severity.CRITICAL, Severity.WARNING):
                for dl in f.detail.split("\n"):
                    w(f"         {dl}")
        w("")

    # Classification table (from engine)
    if result.engine_classifications or result.declared_classifications:
        w("CLASSIFICATION TABLE")
        w("-" * 65)
        w(f"  {'Power':<16} {'Declared':<14} {'Engine':<14} {'Match':<6} {'Chi':>8} {'d':>8} {'Source':>10}")
        w(f"  {'-'*16} {'-'*14} {'-'*14} {'-'*6} {'-'*8} {'-'*8} {'-'*10}")

        all_powers = sorted(set(list(result.declared_classifications.keys()) +
                               list(result.engine_classifications.keys())))
        for pw in all_powers:
            decl = result.declared_classifications.get(pw, "?")
            eng = result.engine_classifications.get(pw, "?")
            match = "YES" if decl == eng else ("?" if "?" in (decl, eng) else "NO")
            chi_val = result.chi_table.get(pw, "")
            d_val = result.d_table.get(pw, "")
            chi_s = f"{chi_val:.4f}" if isinstance(chi_val, float) else str(chi_val)
            d_s = f"{d_val:.4f}" if isinstance(d_val, float) else str(d_val)
            w(f"  {pw:<16} {decl:<14} {eng:<14} {match:<6} {chi_s:>8} {d_s:>8}")
        w("")

    # H1
    if result.h1_value is not None:
        w("COHOMOLOGY")
        w("-" * 30)
        w(f"  H0={result.h0_value}, H1={result.h1_value}")
        w("")

    # Cognitive displacement verdict
    w("COGNITIVE DISPLACEMENT VERDICT")
    w("-" * 40)
    crit_findings = [f for f in result.findings
                     if f.severity == Severity.CRITICAL
                     and f.section.startswith("S3")]
    delta_warnings = [f for f in result.findings if f.code in ("DELTA_ACTIVE", "CHI_BOUNDARY")]
    chain_findings = [f for f in result.findings if f.code == "CHAIN_STATUS"]

    if any(f.code == "NO_STRUCTURAL_DATA" for f in result.findings):
        w("  This story is NOT ready for delta-band analysis.")
        w("  Missing beneficiaries/victims -- engine uses canonical fallback d.")
    elif any(f.code == "NO_POWERLESS" for f in result.findings):
        w("  This story is NOT ready for delta-band analysis.")
        w("  No powerless perspective declared.")
    elif delta_warnings:
        w(f"  This story IS ready for delta-band analysis.")
        w(f"  {len(delta_warnings)} perspective(s) in delta-band (fragile classification).")
    else:
        w("  This story IS ready for delta-band analysis.")
        if result.h1_value is not None and result.h1_value >= 3:
            w("  Deep fracture regime (H1 >= 3, no boundary proximity).")
        else:
            w("  Classification stable across all perspectives.")
    w("")
    w(sep)

    return "\n".join(lines)


def batch_summary(results: list) -> str:
    """Generate batch summary table."""
    lines = []
    w = lines.append
    sep = "=" * 75

    w(sep)
    w(f"BATCH VALIDATION SUMMARY ({len(results)} files)")
    w(sep)
    w("")
    w(f"  {'Constraint ID':<36} {'CRIT':>5} {'WARN':>5} {'INFO':>5} {'PASS':>5}  {'H1':>3}")
    w(f"  {'-'*36} {'-'*5} {'-'*5} {'-'*5} {'-'*5}  {'-'*3}")

    total_c = total_w = total_i = total_p = 0
    crit_files = 0
    for r in results:
        c = severity_counts(r.findings)
        total_c += c["CRITICAL"]
        total_w += c["WARNING"]
        total_i += c["INFO"]
        total_p += c["PASS"]
        if c["CRITICAL"] > 0:
            crit_files += 1
        h1_s = str(r.h1_value) if r.h1_value is not None else "-"
        w(f"  {r.constraint_id:<36} {c['CRITICAL']:>5} {c['WARNING']:>5} "
          f"{c['INFO']:>5} {c['PASS']:>5}  {h1_s:>3}")

    w(f"  {'-'*36} {'-'*5} {'-'*5} {'-'*5} {'-'*5}")
    w(f"  {'TOTALS':<36} {total_c:>5} {total_w:>5} {total_i:>5} {total_p:>5}")
    w("")
    w(f"  Files with CRITICAL findings: {crit_files}")

    # Delta-band readiness count
    delta_ready = sum(1 for r in results
                      if not any(f.code in ("NO_STRUCTURAL_DATA", "NO_POWERLESS")
                                 for f in r.findings))
    w(f"  Delta-band ready: {delta_ready}/{len(results)} ({100*delta_ready/max(len(results),1):.0f}%)")
    w("")

    return "\n".join(lines)


# ===================================================================
# Main orchestration
# ===================================================================

def validate_story(file_path: str, thresholds: dict,
                   no_engine: bool = False, timeout: int = 120) -> ValidationResult:
    """Run full validation pipeline on a single JSON story file."""
    result = ValidationResult(file_path=file_path)

    # Load JSON
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            data = json.load(f)
    except Exception as e:
        result.findings.append(Finding(Severity.CRITICAL, "S2.0", "JSON_PARSE",
            f"Cannot parse JSON: {e}"))
        return result

    bp = data.get("base_properties", {})
    result.constraint_id = data.get("header", {}).get("constraint_id", "unknown")
    result.claimed_type = bp.get("claimed_type", "unknown")
    result.extractiveness = bp.get("extractiveness", 0)

    # Phase 1: Static
    result.findings.extend(check_schema(data))
    result.findings.extend(check_metric_thresholds(data, thresholds))

    compile_findings, pl_content = check_compilation(data)
    result.findings.extend(compile_findings)

    if pl_content:
        result.findings.extend(check_lint(pl_content))

    # Phase 2: Structural
    result.findings.extend(check_directionality_chain(data, thresholds))
    result.findings.extend(check_chi_boundaries(data, thresholds))
    result.findings.extend(check_identity_locked(data))
    result.findings.extend(check_powerless_coverage(data))
    result.findings.extend(check_boltzmann(data))
    result.findings.extend(check_measurements(data, thresholds))

    # Build declared classifications map (first perspective per power atom)
    for p in data.get("perspectives", []):
        pw = p["agent_power"]
        if pw not in result.declared_classifications:
            result.declared_classifications[pw] = p["classification_type"]

    # Phase 3: Engine
    if not no_engine and pl_content:
        perspectives = data.get("perspectives", [])
        engine_result = run_engine_validation(
            result.constraint_id, pl_content, perspectives, timeout)

        result.findings.extend(check_engine_load(engine_result))

        if engine_result["classify"] or engine_result["custom"]:
            result.findings.extend(check_classification_match(data, engine_result))
            result.findings.extend(check_directionality_engine(data, engine_result, thresholds))
            result.findings.extend(check_deltaband_engine(engine_result, thresholds))
            result.findings.extend(check_h1(engine_result))

            # Populate result tables from engine
            # Standard context classifications
            result.engine_classifications = dict(engine_result.get("classify", {}))
            # Add CUSTOM results for power atoms not in standard contexts
            for (pow_, _t, _e, _s), typ in engine_result.get("custom", {}).items():
                if pow_ not in result.engine_classifications:
                    result.engine_classifications[pow_] = typ
            for power, (d, chi) in engine_result.get("direct", {}).items():
                if chi is not None:
                    result.chi_table[power] = chi
                if d is not None:
                    result.d_table[power] = d
            if engine_result.get("h1"):
                result.h0_value, result.h1_value = engine_result["h1"]

    return result


def main():
    parser = argparse.ArgumentParser(
        description="Validate constraint story JSON files against the Prolog engine"
    )
    parser.add_argument("files", nargs="+",
        help="JSON file(s) to validate (supports glob)")
    parser.add_argument("--no-engine", action="store_true",
        help="Skip Prolog engine validation (Phases 1-2 only)")
    parser.add_argument("--timeout", type=int, default=120,
        help="Prolog subprocess timeout in seconds (default: 120)")
    parser.add_argument("--json-output", default=None,
        help="Write structured results to JSON file")
    parser.add_argument("--summary-only", action="store_true",
        help="Print only batch summary table")
    args = parser.parse_args()

    # Resolve file list
    files = []
    for f in args.files:
        p = Path(f)
        if p.is_dir():
            files.extend(sorted(p.glob("*.json")))
        else:
            files.append(p)

    if not files:
        print("No files to validate.", file=sys.stderr)
        sys.exit(1)

    thresholds = load_thresholds()

    results = []
    for fp in files:
        fp_str = str(fp)
        if not args.summary_only:
            print(f"\n{'='*65}", file=sys.stderr)
            print(f"Validating: {fp_str}", file=sys.stderr)
        result = validate_story(fp_str, thresholds,
                                no_engine=args.no_engine, timeout=args.timeout)
        results.append(result)
        if not args.summary_only:
            print(generate_report(result))

    # Batch summary
    if len(results) > 1 or args.summary_only:
        print(batch_summary(results))

    # JSON output
    if args.json_output:
        json_results = []
        for r in results:
            jr = {
                "file": r.file_path,
                "constraint_id": r.constraint_id,
                "claimed_type": r.claimed_type,
                "extractiveness": r.extractiveness,
                "h0": r.h0_value,
                "h1": r.h1_value,
                "engine_classifications": r.engine_classifications,
                "declared_classifications": r.declared_classifications,
                "chi_table": {k: v for k, v in r.chi_table.items()},
                "d_table": {k: v for k, v in r.d_table.items()},
                "findings": [
                    {"severity": f.severity.value, "section": f.section,
                     "code": f.code, "message": f.message, "detail": f.detail}
                    for f in r.findings
                ],
            }
            counts = severity_counts(r.findings)
            jr["counts"] = counts
            json_results.append(jr)

        with open(args.json_output, "w") as f:
            json.dump(json_results, f, indent=2)
        print(f"\nJSON results written to {args.json_output}", file=sys.stderr)

    # Exit code
    total_critical = sum(
        severity_counts(r.findings)["CRITICAL"] for r in results)
    sys.exit(1 if total_critical > 0 else 0)


if __name__ == "__main__":
    main()
