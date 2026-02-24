#!/usr/bin/env python3
"""Chi Variance Decomposition Analysis

Three-part analysis of Chi variability across perspectives for tangled_rope
constraints:
  1. Variance decomposition into f(d) vs scope modifier contributions
  2. Scope modifier sensitivity sweep (stability test)
  3. Dominant divergence pair analysis with counterfactual

Prerequisites:
  - enriched_pipeline.json with perspective_chi fields
  - tangled_gradient_data.json with subtype classifications

Reads:  outputs/enriched_pipeline.json
        outputs/tangled_gradient_data.json
        prolog/config.pl

Writes: outputs/chi_variance_decomposition_data.json
        docs/chi_variance_decomposition.md

Usage:  python3 python/chi_variance_decomposition.py
"""

import json
import math
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, read_config, ENRICHED_PIPELINE_JSON, OUTPUT_DIR
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
REPORT_PATH = DOCS_DIR / "chi_variance_decomposition.md"
DATA_PATH = OUTPUT_DIR / "chi_variance_decomposition_data.json"
TANGLED_GRADIENT_PATH = OUTPUT_DIR / "tangled_gradient_data.json"

PERSPECTIVE_KEYS = ["powerless", "moderate", "institutional", "analytical"]

# ---------------------------------------------------------------------------
# Config thresholds
# ---------------------------------------------------------------------------

_CFG = read_config()

# Gradient boundaries (from config.pl)
ROPE_CHI_CEILING = _CFG.get("rope_chi_ceiling", 0.35)
SNARE_CHI_FLOOR = _CFG.get("snare_chi_floor", 0.66)
CHI_GAP = SNARE_CHI_FLOOR - ROPE_CHI_CEILING

# Classification thresholds for subtype assignment.
# These match tangled_gradient.py:181-186 but are not yet in config.pl.
SUBTYPE_ROPE_THRESHOLD = 0.30
SUBTYPE_SNARE_THRESHOLD = 0.70

# Clamp range for gradient components (tangled_gradient.py:61-63)
CLAMP_MIN = -1.0
CLAMP_MAX = 2.0

# Scope modifier config values for sweep identification
SCOPE_MOD_GLOBAL = _CFG.get("scope_modifier_global", 1.2)
SCOPE_MOD_LOCAL = _CFG.get("scope_modifier_local", 0.8)
SCOPE_MOD_NATIONAL = _CFG.get("scope_modifier_national", 1.0)

# Override detection tolerance — widened beyond naive 0.001 because
# JSON serialization truncates floats, producing rounding artifacts.
CHI_OVERRIDE_TOLERANCE = 0.01


# ---------------------------------------------------------------------------
# Helpers (matching false_ci_rope_audit.py patterns)
# ---------------------------------------------------------------------------

def desc_stats(values):
    """Pure-stdlib descriptive statistics. Returns dict or None if empty."""
    vals = [v for v in values if v is not None]
    n = len(vals)
    if n == 0:
        return None
    s = sorted(vals)
    mean = sum(s) / n
    median = s[n // 2] if n % 2 == 1 else (s[n // 2 - 1] + s[n // 2]) / 2.0
    variance = sum((x - mean) ** 2 for x in s) / n
    std = math.sqrt(variance)
    return {
        "n": n,
        "mean": round(mean, 6),
        "median": round(median, 6),
        "std": round(std, 6),
        "min": round(s[0], 6),
        "max": round(s[-1], 6),
        "q25": round(s[int(n * 0.25)], 6),
        "q75": round(s[int(n * 0.75)], 6),
    }


def pct(num, denom):
    """Safe percentage."""
    if denom == 0:
        return 0.0
    return round(100.0 * num / denom, 1)


def fmt(val, decimals=4):
    """Format a numeric value or return '---' for None."""
    if val is None:
        return "---"
    if isinstance(val, float):
        return f"{val:.{decimals}f}"
    return str(val)


def md_table(headers, rows, alignments=None):
    """Build a markdown table string."""
    if alignments is None:
        alignments = ["l"] * len(headers)
    sep_map = {"l": ":---", "r": "---:", "c": ":---:"}
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join(sep_map.get(a, "---") for a in alignments) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(c) for c in row) + " |")
    return "\n".join(lines)


def pop_variance(values):
    """Population variance of a list of floats."""
    n = len(values)
    if n == 0:
        return 0.0
    mean = sum(values) / n
    return sum((x - mean) ** 2 for x in values) / n


def gradient_component(value, rope_ceiling, snare_floor):
    """Normalized position between rope ceiling and snare floor.

    Returns 0.0 at rope_ceiling, 1.0 at snare_floor.
    NOT clipped — off-scale values are diagnostic.
    """
    gap = snare_floor - rope_ceiling
    if gap == 0:
        return 0.5
    return (value - rope_ceiling) / gap


def clamp(value, lo, hi):
    """Clamp value to [lo, hi]."""
    if value < lo:
        return lo
    if value > hi:
        return hi
    return value


def classify_subtype(chi_gradients):
    """Classify tangled_rope subtype from per-perspective Chi gradients.

    Replicates tangled_gradient.py:166-187.
    """
    valid = [v for v in chi_gradients.values() if v is not None]
    if not valid:
        return "unclassifiable"
    clamped = [clamp(g, CLAMP_MIN, CLAMP_MAX) for g in valid]
    min_g = min(clamped)
    max_g = max(clamped)
    if max_g < SUBTYPE_ROPE_THRESHOLD:
        return "rope_dominant"
    if min_g > SUBTYPE_SNARE_THRESHOLD:
        return "snare_dominant"
    if min_g < SUBTYPE_ROPE_THRESHOLD and max_g > SUBTYPE_SNARE_THRESHOLD:
        return "genuinely_perspectival"
    return "structurally_ambiguous"


# ---------------------------------------------------------------------------
# Scope mapping discovery
# ---------------------------------------------------------------------------

def discover_scope_mapping(population):
    """Discover the perspective -> scope_mod mapping from actual data.

    Returns:
        scope_map: {perspective: scope_mod} if uniform across population
        per_constraint_scopes: {cid: {perspective: scope_mod}} for non-uniform cases
        warnings: list of strings if any perspective has inconsistent scopes
    """
    # Collect all scope values seen per perspective
    per_perspective = defaultdict(set)
    per_constraint_scopes = {}

    for c in population:
        cid = c["id"]
        pchi = c.get("perspective_chi", {})
        cscopes = {}
        for pk in PERSPECTIVE_KEYS:
            sm = pchi.get(pk, {}).get("scope_mod")
            if sm is not None:
                per_perspective[pk].add(round(sm, 4))
                cscopes[pk] = sm
        per_constraint_scopes[cid] = cscopes

    # Check uniformity
    warnings = []
    scope_map = {}
    for pk in PERSPECTIVE_KEYS:
        vals = per_perspective[pk]
        if len(vals) == 1:
            scope_map[pk] = vals.pop()
        elif len(vals) == 0:
            warnings.append(f"No scope_mod data for perspective '{pk}'")
        else:
            warnings.append(
                f"Perspective '{pk}' has multiple scope values: {sorted(vals)}")
            # Use most common value
            scope_map[pk] = sorted(vals)[0]

    return scope_map, per_constraint_scopes, warnings


def identify_sweep_perspectives(scope_map, target_scope_value):
    """Find which perspectives use the given scope modifier value.

    Returns list of perspective keys whose scope_mod matches target_scope_value.
    """
    return [pk for pk, sv in scope_map.items()
            if abs(sv - target_scope_value) < 0.001]


# ---------------------------------------------------------------------------
# Chi override detection
# ---------------------------------------------------------------------------

def detect_chi_overrides(population):
    """Find constraints where chi != epsilon * f_d * scope_mod.

    Returns list of (constraint_id, max_discrepancy) tuples.
    """
    overrides = []
    for c in population:
        pchi = c.get("perspective_chi", {})
        max_disc = 0.0
        for pk in PERSPECTIVE_KEYS:
            pd = pchi.get(pk, {})
            chi_actual = pd.get("chi")
            eps = pd.get("epsilon")
            f_d = pd.get("f_d")
            sm = pd.get("scope_mod")
            if None in (chi_actual, eps, f_d, sm):
                continue
            chi_expected = eps * f_d * sm
            disc = abs(chi_actual - chi_expected)
            if disc > max_disc:
                max_disc = disc
        if max_disc > CHI_OVERRIDE_TOLERANCE:
            overrides.append((c["id"], round(max_disc, 6)))
    return overrides


# ---------------------------------------------------------------------------
# Part 1: Variance Decomposition
# ---------------------------------------------------------------------------

def extract_chi_components(constraint):
    """Extract per-perspective chi, f_d, scope_mod, epsilon.

    Returns dict or None if data is incomplete.
    """
    pchi = constraint.get("perspective_chi", {})
    chi_full = {}
    f_d_vals = {}
    scope_vals = {}
    epsilon = None

    for pk in PERSPECTIVE_KEYS:
        pd = pchi.get(pk, {})
        chi_val = pd.get("chi")
        f_d = pd.get("f_d")
        sm = pd.get("scope_mod")
        eps = pd.get("epsilon")
        if None in (chi_val, f_d, sm, eps):
            return None
        chi_full[pk] = chi_val
        f_d_vals[pk] = f_d
        scope_vals[pk] = sm
        if epsilon is None:
            epsilon = eps

    return {
        "chi_full": chi_full,
        "f_d": f_d_vals,
        "scope_mod": scope_vals,
        "epsilon": epsilon,
    }


def decompose_variance_single(components):
    """Variance decomposition for a single constraint.

    Returns dict with variance components and fractions.
    """
    chi_full = components["chi_full"]
    f_d = components["f_d"]
    scope_mod = components["scope_mod"]
    epsilon = components["epsilon"]

    # Mean f(d) across perspectives
    f_d_values = [f_d[pk] for pk in PERSPECTIVE_KEYS]
    f_d_mean = sum(f_d_values) / len(f_d_values)

    # Counterfactual Chi variants
    chi_fd_only = {}     # scope neutralized to 1.0
    chi_scope_only = {}  # f(d) neutralized to mean

    for pk in PERSPECTIVE_KEYS:
        chi_fd_only[pk] = epsilon * f_d[pk] * 1.0
        chi_scope_only[pk] = epsilon * f_d_mean * scope_mod[pk]

    # Variances across the 4 perspectives
    full_vals = [chi_full[pk] for pk in PERSPECTIVE_KEYS]
    fd_vals = [chi_fd_only[pk] for pk in PERSPECTIVE_KEYS]
    scope_vals = [chi_scope_only[pk] for pk in PERSPECTIVE_KEYS]

    var_total = pop_variance(full_vals)
    var_fd = pop_variance(fd_vals)
    var_scope = pop_variance(scope_vals)
    var_interaction = var_total - var_fd - var_scope

    # Fractions
    if var_total > 0:
        fd_fraction = var_fd / var_total
        scope_fraction = var_scope / var_total
        interaction_fraction = var_interaction / var_total
    else:
        fd_fraction = 0.0
        scope_fraction = 0.0
        interaction_fraction = 0.0

    # Range-based alternative
    range_fd = max(f_d_values) - min(f_d_values)
    scope_values = [scope_mod[pk] for pk in PERSPECTIVE_KEYS]
    range_scope = max(scope_values) - min(scope_values)
    range_chi = max(full_vals) - min(full_vals)

    return {
        "var_total": round(var_total, 8),
        "var_fd": round(var_fd, 8),
        "var_scope": round(var_scope, 8),
        "var_interaction": round(var_interaction, 8),
        "fd_fraction": round(fd_fraction, 6),
        "scope_fraction": round(scope_fraction, 6),
        "interaction_fraction": round(interaction_fraction, 6),
        "range_fd": round(range_fd, 6),
        "range_scope": round(range_scope, 6),
        "range_chi": round(range_chi, 6),
        "f_d_mean": round(f_d_mean, 6),
        "chi_values": {pk: round(chi_full[pk], 6) for pk in PERSPECTIVE_KEYS},
    }


def aggregate_decomposition(per_constraint_decomp):
    """Aggregate decomposition stats across a population subset."""
    if not per_constraint_decomp:
        return None

    fields = ["var_total", "var_fd", "var_scope", "var_interaction",
              "fd_fraction", "scope_fraction"]
    agg = {}
    for f in fields:
        agg[f] = desc_stats([d[f] for d in per_constraint_decomp.values()])

    # Histogram of fd_fraction in 10 bins from 0 to 1
    bins = [{"range": f"{i/10:.1f}-{(i+1)/10:.1f}", "count": 0}
            for i in range(10)]
    # Extra bin for values > 1.0 (from negative interaction)
    overflow_high = 0
    underflow_low = 0
    for d in per_constraint_decomp.values():
        fdf = d["fd_fraction"]
        if fdf < 0.0:
            underflow_low += 1
        elif fdf >= 1.0:
            overflow_high += 1
        else:
            idx = min(int(fdf * 10), 9)
            bins[idx]["count"] += 1

    # Dominance classification (mutually exclusive)
    # Because interaction can be negative, fd_fraction + scope_fraction > 1
    # is common. Classify by which source has the larger fraction.
    n = len(per_constraint_decomp)
    fd_dom = 0
    scope_dom = 0
    balanced = 0
    for d in per_constraint_decomp.values():
        if d["fd_fraction"] > d["scope_fraction"] and d["fd_fraction"] > 0.6:
            fd_dom += 1
        elif d["scope_fraction"] > d["fd_fraction"] and d["scope_fraction"] > 0.6:
            scope_dom += 1
        else:
            balanced += 1

    return {
        "stats": agg,
        "fd_fraction_histogram": {
            "bins": bins,
            "underflow_below_0": underflow_low,
            "overflow_above_1": overflow_high,
        },
        "dominance_counts": {
            "fd_dominated": fd_dom,
            "scope_dominated": scope_dom,
            "balanced": balanced,
            "total": n,
        },
    }


def run_variance_decomposition(population, tangled_gradient):
    """Part 1: Full variance decomposition."""
    per_constraint = {}
    skipped = 0

    for c in population:
        cid = c["id"]
        components = extract_chi_components(c)
        if components is None:
            skipped += 1
            continue
        decomp = decompose_variance_single(components)
        # Attach subtype from tangled_gradient_data
        tg_entry = tangled_gradient.get(cid, {})
        decomp["subtype"] = tg_entry.get("subtype", "unknown")
        per_constraint[cid] = decomp

    # Aggregate over full population
    all_agg = aggregate_decomposition(per_constraint)

    # Aggregate over genuinely_perspectival subset
    gp_subset = {cid: d for cid, d in per_constraint.items()
                 if d["subtype"] == "genuinely_perspectival"}
    gp_agg = aggregate_decomposition(gp_subset)

    # Per-subtype aggregation
    subtypes = ["genuinely_perspectival", "structurally_ambiguous",
                "rope_dominant", "snare_dominant"]
    by_subtype = {}
    for st in subtypes:
        subset = {cid: d for cid, d in per_constraint.items()
                  if d["subtype"] == st}
        by_subtype[st] = aggregate_decomposition(subset)

    return {
        "per_constraint": per_constraint,
        "skipped": skipped,
        "all_population": all_agg,
        "genuinely_perspectival": gp_agg,
        "by_subtype": by_subtype,
    }


# ---------------------------------------------------------------------------
# Part 2: Scope Modifier Sensitivity Sweep
# ---------------------------------------------------------------------------

def sweep_single_value(population, per_constraint_scopes, sweep_perspectives,
                       sweep_value):
    """Reclassify all constraints at a single swept scope value.

    Returns subtype_counts dict.
    """
    counts = Counter()

    for c in population:
        cid = c["id"]
        pchi = c.get("perspective_chi", {})
        cscopes = per_constraint_scopes.get(cid, {})

        chi_gradients = {}
        for pk in PERSPECTIVE_KEYS:
            pd = pchi.get(pk, {})
            eps = pd.get("epsilon")
            f_d = pd.get("f_d")
            if eps is None or f_d is None:
                chi_gradients[pk] = None
                continue

            # Apply swept scope if this perspective matches
            if pk in sweep_perspectives:
                sm = sweep_value
            else:
                sm = cscopes.get(pk, pd.get("scope_mod", 1.0))

            chi_swept = eps * f_d * sm
            g_chi = gradient_component(chi_swept, ROPE_CHI_CEILING,
                                       SNARE_CHI_FLOOR)
            chi_gradients[pk] = g_chi

        subtype = classify_subtype(chi_gradients)
        counts[subtype] += 1

    return dict(counts)


def run_sensitivity_sweep(population, per_constraint_scopes, scope_map):
    """Part 2: Full scope modifier sensitivity analysis."""

    # Identify sweep targets from data
    global_perspectives = identify_sweep_perspectives(scope_map,
                                                      SCOPE_MOD_GLOBAL)
    local_perspectives = identify_sweep_perspectives(scope_map,
                                                     SCOPE_MOD_LOCAL)

    total = len(population)
    subtypes_order = ["rope_dominant", "snare_dominant",
                      "genuinely_perspectival", "structurally_ambiguous"]

    # Global sweep: sigma_global from 1.0 to 1.5
    global_range = [round(1.0 + i * 0.05, 2) for i in range(11)]
    global_sweep = []
    for sv in global_range:
        counts = sweep_single_value(population, per_constraint_scopes,
                                    global_perspectives, sv)
        gp_count = counts.get("genuinely_perspectival", 0)
        global_sweep.append({
            "sigma_value": sv,
            "subtype_counts": {st: counts.get(st, 0) for st in subtypes_order},
            "gp_count": gp_count,
            "gp_fraction": round(gp_count / total, 4) if total else 0,
            "total": total,
        })

    # Local sweep: sigma_local from 0.5 to 1.0
    local_range = [round(0.5 + i * 0.05, 2) for i in range(11)]
    local_sweep = []
    for sv in local_range:
        counts = sweep_single_value(population, per_constraint_scopes,
                                    local_perspectives, sv)
        gp_count = counts.get("genuinely_perspectival", 0)
        local_sweep.append({
            "sigma_value": sv,
            "subtype_counts": {st: counts.get(st, 0) for st in subtypes_order},
            "gp_count": gp_count,
            "gp_fraction": round(gp_count / total, 4) if total else 0,
            "total": total,
        })

    # Phase transitions
    gp_thresholds = [0.80, 0.70, 0.60, 0.50]

    def find_transitions(sweep_results, thresholds):
        transitions = {}
        for th in thresholds:
            found = None
            for row in sweep_results:
                if row["gp_fraction"] < th:
                    found = row["sigma_value"]
                    break
            transitions[str(th)] = found
        return transitions

    global_transitions = find_transitions(global_sweep, gp_thresholds)
    local_transitions = find_transitions(local_sweep, gp_thresholds)

    # Snare growth: when does snare_dominant exceed 5%?
    def find_snare_growth(sweep_results):
        for row in sweep_results:
            snare_frac = row["subtype_counts"].get("snare_dominant", 0) / total
            if snare_frac > 0.05:
                return row["sigma_value"]
        return None

    global_snare_growth = find_snare_growth(global_sweep)
    local_snare_growth = find_snare_growth(local_sweep)

    # "No single subtype exceeds 50%" check
    def find_no_majority(sweep_results):
        for row in sweep_results:
            max_count = max(row["subtype_counts"].values())
            if max_count / total <= 0.50:
                return row["sigma_value"]
        return None

    global_no_majority = find_no_majority(global_sweep)
    local_no_majority = find_no_majority(local_sweep)

    # Rapid shift detection: max delta in GP count between adjacent steps
    def detect_rapid_shift(sweep_results):
        max_delta = 0
        max_delta_range = None
        for i in range(1, len(sweep_results)):
            delta = abs(sweep_results[i]["gp_count"] -
                        sweep_results[i - 1]["gp_count"])
            if delta > max_delta:
                max_delta = delta
                max_delta_range = (sweep_results[i - 1]["sigma_value"],
                                   sweep_results[i]["sigma_value"])
        return {"max_delta_count": max_delta,
                "sigma_range": max_delta_range}

    return {
        "global_sweep": global_sweep,
        "local_sweep": local_sweep,
        "global_perspectives": global_perspectives,
        "local_perspectives": local_perspectives,
        "global_phase_transitions": global_transitions,
        "local_phase_transitions": local_transitions,
        "global_snare_growth_at": global_snare_growth,
        "local_snare_growth_at": local_snare_growth,
        "global_no_majority_at": global_no_majority,
        "local_no_majority_at": local_no_majority,
        "global_rapid_shift": detect_rapid_shift(global_sweep),
        "local_rapid_shift": detect_rapid_shift(local_sweep),
    }


# ---------------------------------------------------------------------------
# Part 3: Dominant Divergence Pair Analysis
# ---------------------------------------------------------------------------

def all_perspective_pairs():
    """Return all 6 ordered perspective pairs."""
    pairs = []
    for i in range(len(PERSPECTIVE_KEYS)):
        for j in range(i + 1, len(PERSPECTIVE_KEYS)):
            pairs.append((PERSPECTIVE_KEYS[i], PERSPECTIVE_KEYS[j]))
    return pairs


def pair_label(a, b):
    """Canonical label for a perspective pair."""
    return f"{a}-{b}"


def compute_pair_divergences(population, tangled_gradient):
    """Compute |delta_chi| for all 6 pairs across genuinely_perspectival.

    Uses actual chi values from enriched_pipeline data.
    """
    pairs = all_perspective_pairs()
    pair_deltas = defaultdict(list)  # pair_label -> list of |delta_chi|

    gp_count = 0
    for c in population:
        cid = c["id"]
        tg = tangled_gradient.get(cid, {})
        if tg.get("subtype") != "genuinely_perspectival":
            continue
        gp_count += 1

        pchi = c.get("perspective_chi", {})
        chi_vals = {}
        for pk in PERSPECTIVE_KEYS:
            chi_vals[pk] = pchi.get(pk, {}).get("chi")

        for a, b in pairs:
            if chi_vals.get(a) is not None and chi_vals.get(b) is not None:
                delta = abs(chi_vals[a] - chi_vals[b])
                pair_deltas[pair_label(a, b)].append(delta)

    # Stats per pair
    pair_stats = {}
    for pl, deltas in pair_deltas.items():
        pair_stats[pl] = desc_stats(deltas)

    # Ranking by mean |delta_chi|
    ranking = sorted(pair_stats.keys(),
                     key=lambda k: pair_stats[k]["mean"] if pair_stats[k] else 0,
                     reverse=True)

    return {"pair_stats": pair_stats, "ranking": ranking, "n": gp_count}


def decompose_pair_delta(constraint, persp_a, persp_b):
    """Decompose delta_chi between two perspectives into f(d) and scope.

    Uses symmetric decomposition:
      delta_due_to_fd    = epsilon * (f_A - f_B) * mean_scope
      delta_due_to_scope = epsilon * mean_f * (s_A - s_B)
      delta_interaction  = delta_chi_actual - fd_term - scope_term
    """
    pchi = constraint.get("perspective_chi", {})
    pa = pchi.get(persp_a, {})
    pb = pchi.get(persp_b, {})

    chi_a = pa.get("chi")
    chi_b = pb.get("chi")
    f_a = pa.get("f_d")
    f_b = pb.get("f_d")
    s_a = pa.get("scope_mod")
    s_b = pb.get("scope_mod")
    eps = pa.get("epsilon")

    if None in (chi_a, chi_b, f_a, f_b, s_a, s_b, eps):
        return None

    delta_chi = chi_a - chi_b
    mean_f = (f_a + f_b) / 2.0
    mean_s = (s_a + s_b) / 2.0
    delta_f = f_a - f_b
    delta_s = s_a - s_b

    delta_due_to_fd = eps * delta_f * mean_s
    delta_due_to_scope = eps * mean_f * delta_s
    delta_interaction = delta_chi - delta_due_to_fd - delta_due_to_scope

    return {
        "delta_chi": round(delta_chi, 6),
        "delta_due_to_fd": round(delta_due_to_fd, 6),
        "delta_due_to_scope": round(delta_due_to_scope, 6),
        "delta_interaction": round(delta_interaction, 6),
        "f_a": round(f_a, 6), "f_b": round(f_b, 6),
        "s_a": round(s_a, 4), "s_b": round(s_b, 4),
        "epsilon": round(eps, 6),
    }


def aggregate_pair_decomposition(population, tangled_gradient,
                                 persp_a, persp_b):
    """Aggregate pair decomposition across genuinely_perspectival."""
    decomps = []
    for c in population:
        cid = c["id"]
        tg = tangled_gradient.get(cid, {})
        if tg.get("subtype") != "genuinely_perspectival":
            continue
        d = decompose_pair_delta(c, persp_a, persp_b)
        if d is not None:
            decomps.append(d)

    if not decomps:
        return None

    fields = ["delta_chi", "delta_due_to_fd", "delta_due_to_scope",
              "delta_interaction"]
    stats = {}
    for f in fields:
        stats[f] = desc_stats([d[f] for d in decomps])

    # How often does |fd| > |scope| ?
    fd_dominates = sum(1 for d in decomps
                       if abs(d["delta_due_to_fd"]) >
                          abs(d["delta_due_to_scope"]))
    n = len(decomps)

    return {
        "n": n,
        "stats": stats,
        "fd_dominates_count": fd_dominates,
        "fd_dominates_fraction": round(fd_dominates / n, 4) if n else 0,
    }


def run_counterfactual(population, tangled_gradient):
    """Counterfactual: if powerless had scope_mod=1.0, which pair dominates?"""
    pairs = all_perspective_pairs()
    pair_deltas = defaultdict(list)

    for c in population:
        cid = c["id"]
        tg = tangled_gradient.get(cid, {})
        if tg.get("subtype") != "genuinely_perspectival":
            continue

        pchi = c.get("perspective_chi", {})
        chi_vals = {}
        for pk in PERSPECTIVE_KEYS:
            pd = pchi.get(pk, {})
            if pk == "powerless":
                # Counterfactual: recompute with scope_mod=1.0
                eps = pd.get("epsilon")
                f_d = pd.get("f_d")
                if eps is not None and f_d is not None:
                    chi_vals[pk] = eps * f_d * 1.0
                else:
                    chi_vals[pk] = None
            else:
                chi_vals[pk] = pd.get("chi")

        for a, b in pairs:
            if chi_vals.get(a) is not None and chi_vals.get(b) is not None:
                delta = abs(chi_vals[a] - chi_vals[b])
                pair_deltas[pair_label(a, b)].append(delta)

    pair_stats = {}
    for pl, deltas in pair_deltas.items():
        pair_stats[pl] = desc_stats(deltas)

    ranking = sorted(pair_stats.keys(),
                     key=lambda k: pair_stats[k]["mean"] if pair_stats[k] else 0,
                     reverse=True)

    return {"pair_stats": pair_stats, "ranking": ranking}


def run_pair_analysis(population, tangled_gradient):
    """Part 3: Full divergence pair analysis."""
    divergences = compute_pair_divergences(population, tangled_gradient)

    # Decompose the two key pairs
    inst_anal = aggregate_pair_decomposition(
        population, tangled_gradient, "institutional", "analytical")
    power_inst = aggregate_pair_decomposition(
        population, tangled_gradient, "powerless", "institutional")

    counterfactual = run_counterfactual(population, tangled_gradient)

    return {
        "all_pairs": divergences,
        "institutional_analytical_decomposition": inst_anal,
        "powerless_institutional_decomposition": power_inst,
        "counterfactual_powerless_scope_1_0": counterfactual,
    }


# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def write_report(results, overrides, scope_map, scope_warnings):
    """Write the markdown analysis report."""
    now = datetime.now().strftime("%Y-%m-%d %H:%M")
    p1 = results["part1_variance_decomposition"]
    p2 = results["part2_sensitivity_sweep"]
    p3 = results["part3_divergence_pairs"]

    lines = []
    w = lines.append

    w("# Chi Variance Decomposition Analysis\n")
    w(f"*Generated {now} by `python/chi_variance_decomposition.py`*\n")
    w("---\n")

    # ------------------------------------------------------------------
    # Executive Summary
    # ------------------------------------------------------------------
    w("## 1. Executive Summary\n")

    all_agg = p1["all_population"]
    gp_agg = p1["genuinely_perspectival"]
    n_total = all_agg["dominance_counts"]["total"] if all_agg else 0
    n_gp = gp_agg["dominance_counts"]["total"] if gp_agg else 0

    # Determine if f(d) or scope dominates
    # Use raw variance means (not fractions, which can exceed 1.0 due to
    # negative interaction) for the summary ratio.
    mean_fd_frac = 0.0
    mean_scope_frac = 0.0
    if all_agg and all_agg["stats"]["var_fd"] and all_agg["stats"]["var_scope"]:
        mean_var_fd = all_agg["stats"]["var_fd"]["mean"]
        mean_var_scope = all_agg["stats"]["var_scope"]["mean"]
        var_sum = mean_var_fd + mean_var_scope
        if var_sum > 0:
            mean_fd_frac = mean_var_fd / var_sum
            mean_scope_frac = mean_var_scope / var_sum

    if mean_fd_frac > 0.8:
        driver = "f(d) (power sigmoid)"
    elif mean_scope_frac > 0.8:
        driver = "scope modifier"
    elif mean_fd_frac > 0.6:
        driver = "primarily f(d), with scope contributing"
    elif mean_scope_frac > 0.6:
        driver = "primarily scope modifier, with f(d) contributing"
    else:
        driver = "both f(d) and scope modifier"

    # Sweep stability
    gt = p2["global_phase_transitions"]
    gp_at_1_0 = None
    for row in p2["global_sweep"]:
        if abs(row["sigma_value"] - 1.0) < 0.001:
            gp_at_1_0 = row["gp_fraction"]
            break

    w(f"Analyzed **{n_total}** tangled_rope constraints "
      f"({n_gp} genuinely perspectival).\n")
    w(f"**Variance driver**: {driver}. "
      f"Of total f(d)+scope variance, f(d) accounts for "
      f"{fmt(mean_fd_frac * 100, 1)}% and scope for "
      f"{fmt(mean_scope_frac * 100, 1)}%. "
      f"(Negative interaction means Var_fd + Var_scope > Var_total.)\n")

    if gp_at_1_0 is not None:
        w(f"**Sweep stability**: At σ(global)=1.0, GP fraction = "
          f"{fmt(gp_at_1_0 * 100, 1)}% "
          f"(baseline at σ=1.2: {pct(n_gp, n_total)}%).\n")

    drop80 = gt.get("0.8")
    if drop80 is None:
        w("GP stays above 80% across the full σ(global) sweep range.\n")
    else:
        w(f"GP drops below 80% at σ(global) = {drop80}.\n")

    w(f"Chi overrides detected: **{len(overrides)}** constraints "
      f"(tolerance = {CHI_OVERRIDE_TOLERANCE}).\n")

    if scope_warnings:
        w("\n**Scope mapping warnings:**\n")
        for warning in scope_warnings:
            w(f"- {warning}\n")

    w(f"\nDiscovered scope mapping: {scope_map}\n")
    w("")

    # ------------------------------------------------------------------
    # Part 1: Variance Decomposition
    # ------------------------------------------------------------------
    w("## 2. Variance Decomposition\n")
    w("### 2.1 Methodology\n")
    w("Chi for each constraint-perspective pair: `χ = ε × f(d) × σ(S)`\n")
    w("Counterfactual variants:\n")
    w("```")
    w("χ_full(C, U)       = ε(C) × f_d(C, U) × scope_mod(U)      -- actual")
    w("χ_fd_only(C, U)    = ε(C) × f_d(C, U) × 1.0               -- scope neutralized")
    w("χ_scope_only(C, U) = ε(C) × mean(f_d) × scope_mod(U)      -- f(d) neutralized")
    w("```\n")
    w("Var_total = Var(χ_full), Var_fd = Var(χ_fd_only), "
      "Var_scope = Var(χ_scope_only), Var_interaction = Var_total - Var_fd - Var_scope\n")
    w("**Note on negative interaction**: When f(d) and scope variations "
      "are anti-correlated across perspectives (high f(d) pairs with low "
      "scope, and vice versa), their product has less variance than the "
      "sum of individual variances. This makes Var_interaction negative "
      "and fd_fraction + scope_fraction > 1.0. This is expected, not an "
      "error — it means f(d) and scope partially cancel each other.\n")

    def write_stats_table(agg, label):
        if agg is None:
            w(f"*No data for {label}*\n")
            return
        st = agg["stats"]
        headers = ["Component", "Mean", "Median", "Std", "Q25", "Q75"]
        rows = []
        for f_name in ["var_total", "var_fd", "var_scope", "var_interaction",
                       "fd_fraction", "scope_fraction"]:
            s = st.get(f_name)
            if s:
                rows.append([f_name,
                             fmt(s["mean"], 6), fmt(s["median"], 6),
                             fmt(s["std"], 6), fmt(s["q25"], 6),
                             fmt(s["q75"], 6)])
        w(md_table(headers, rows, ["l", "r", "r", "r", "r", "r"]))
        w("")

    w(f"### 2.2 Full Population (N={n_total})\n")
    write_stats_table(all_agg, "full population")

    w(f"### 2.3 Genuinely Perspectival Subset (N={n_gp})\n")
    write_stats_table(gp_agg, "genuinely perspectival")

    w("### 2.4 By Subtype\n")
    headers = ["Subtype", "N", "Mean fd_frac", "Mean scope_frac",
               "Mean interaction"]
    rows = []
    for st_name in ["genuinely_perspectival", "structurally_ambiguous",
                    "rope_dominant", "snare_dominant"]:
        st_agg = p1["by_subtype"].get(st_name)
        if st_agg and st_agg["stats"]["fd_fraction"]:
            n_st = st_agg["dominance_counts"]["total"]
            rows.append([
                st_name, str(n_st),
                fmt(st_agg["stats"]["fd_fraction"]["mean"], 4),
                fmt(st_agg["stats"]["scope_fraction"]["mean"], 4),
                fmt(st_agg["stats"]["var_interaction"]["mean"], 6),
            ])
    w(md_table(headers, rows, ["l", "r", "r", "r", "r"]))
    w("")

    w("### 2.5 Variance Share Distribution\n")
    w("Distribution of f(d) share = Var_fd / (Var_fd + Var_scope), which "
      "is always in [0, 1] and avoids the >1.0 artifact from negative "
      "interaction:\n")
    if p1["per_constraint"]:
        # Compute fd_share = Var_fd / (Var_fd + Var_scope) per constraint
        shares = []
        for d in p1["per_constraint"].values():
            vf = d["var_fd"]
            vs = d["var_scope"]
            total_v = vf + vs
            if total_v > 0:
                shares.append(vf / total_v)
        # Histogram in 10 bins
        hist_bins = [0] * 10
        for s in shares:
            idx = min(int(s * 10), 9)
            hist_bins[idx] += 1
        max_count = max(hist_bins) if hist_bins else 1
        w("```")
        for i in range(10):
            lo = i / 10
            hi = (i + 1) / 10
            bar = "#" * max(1, int(40 * hist_bins[i] / max(max_count, 1)))
            w(f"  {lo:.1f}-{hi:.1f} | {bar} ({hist_bins[i]})")
        w("```\n")
        share_stats = desc_stats(shares)
        if share_stats:
            w(f"f(d) share: mean={fmt(share_stats['mean'], 4)}, "
              f"median={fmt(share_stats['median'], 4)}, "
              f"min={fmt(share_stats['min'], 4)}, "
              f"max={fmt(share_stats['max'], 4)}\n")

    w("### 2.6 Dominance Classification\n")
    w("Mutually exclusive: classified by which source has the larger "
      "fraction (must also exceed 0.6).\n")
    if all_agg:
        dc = all_agg["dominance_counts"]
        headers = ["Category", "Count", "%"]
        rows = [
            ["f(d)-dominated",
             str(dc["fd_dominated"]), fmt(pct(dc["fd_dominated"], dc["total"]))],
            ["scope-dominated",
             str(dc["scope_dominated"]),
             fmt(pct(dc["scope_dominated"], dc["total"]))],
            ["balanced",
             str(dc["balanced"]), fmt(pct(dc["balanced"], dc["total"]))],
        ]
        w(md_table(headers, rows, ["l", "r", "r"]))
    w("")

    if overrides:
        w("### 2.7 Chi Overrides\n")
        w(f"{len(overrides)} constraints have Chi values that differ from "
          f"`ε × f(d) × σ(S)` by more than {CHI_OVERRIDE_TOLERANCE}.\n")
        w("These are constraints where manual overrides or rounding effects "
          "produce non-multiplicative Chi. The variance decomposition uses "
          "actual Chi for Var_total but multiplicative formula for "
          "counterfactuals.\n")
        if len(overrides) <= 30:
            w("| Constraint | Max Discrepancy |")
            w("| :--- | ---: |")
            for cid, disc in sorted(overrides, key=lambda x: -x[1]):
                w(f"| {cid} | {fmt(disc, 6)} |")
        else:
            w(f"Top 10 by discrepancy (of {len(overrides)}):\n")
            w("| Constraint | Max Discrepancy |")
            w("| :--- | ---: |")
            for cid, disc in sorted(overrides, key=lambda x: -x[1])[:10]:
                w(f"| {cid} | {fmt(disc, 6)} |")
        w("")

    # ------------------------------------------------------------------
    # Part 2: Sensitivity Sweep
    # ------------------------------------------------------------------
    w("## 3. Scope Modifier Sensitivity Sweep\n")
    w("### 3.1 Methodology\n")
    w("Sweep σ(global) from 1.0 to 1.5 (step 0.05), holding other scopes "
      "at their data values. Parallel sweep σ(local) from 0.5 to 1.0.\n")
    w(f"Global scope perspectives (from data): "
      f"{p2['global_perspectives']}\n")
    w(f"Local scope perspectives (from data): "
      f"{p2['local_perspectives']}\n")
    w(f"At each value: recompute χ = ε × f(d) × σ_swept, compute gradient, "
      f"reclassify subtypes. **{len(overrides)} Chi overrides are replaced** "
      f"by the multiplicative formula during sweep.\n")

    def write_sweep_table(sweep_data, label):
        headers = ["σ", "rope_dom", "snare_dom", "genuinely_persp",
                   "struct_ambig", "GP%"]
        rows = []
        for row in sweep_data:
            sc = row["subtype_counts"]
            rows.append([
                fmt(row["sigma_value"], 2),
                str(sc.get("rope_dominant", 0)),
                str(sc.get("snare_dominant", 0)),
                str(sc.get("genuinely_perspectival", 0)),
                str(sc.get("structurally_ambiguous", 0)),
                fmt(row["gp_fraction"] * 100, 1),
            ])
        w(f"### {label}\n")
        w(md_table(headers, rows, ["r", "r", "r", "r", "r", "r"]))
        w("")

    write_sweep_table(p2["global_sweep"], "3.2 Global Scope Sweep")
    write_sweep_table(p2["local_sweep"], "3.3 Local Scope Sweep")

    w("### 3.4 Phase Transitions\n")
    headers = ["Threshold", "σ(global) at crossing", "σ(local) at crossing"]
    rows = []
    for th in ["0.8", "0.7", "0.6", "0.5"]:
        rows.append([
            f"GP < {fmt(float(th) * 100, 0)}%",
            fmt(p2["global_phase_transitions"].get(th), 2)
            if p2["global_phase_transitions"].get(th) is not None else "never",
            fmt(p2["local_phase_transitions"].get(th), 2)
            if p2["local_phase_transitions"].get(th) is not None else "never",
        ])
    w(md_table(headers, rows, ["l", "r", "r"]))
    w("")

    # Snare growth
    w(f"**Snare growth > 5%**: global sweep = "
      f"{p2['global_snare_growth_at'] or 'never'}, "
      f"local sweep = {p2['local_snare_growth_at'] or 'never'}\n")

    # No majority check
    w(f"**No single subtype > 50%**: global sweep = "
      f"{p2['global_no_majority_at'] or 'never'}, "
      f"local sweep = {p2['local_no_majority_at'] or 'never'}\n")

    # Rapid shift
    gs = p2["global_rapid_shift"]
    ls = p2["local_rapid_shift"]
    w(f"**Max GP count change in one step**: "
      f"global = {gs['max_delta_count']} "
      f"(σ {gs['sigma_range'][0]:.2f}→{gs['sigma_range'][1]:.2f}), "
      f"local = {ls['max_delta_count']} "
      f"(σ {ls['sigma_range'][0]:.2f}→{ls['sigma_range'][1]:.2f})\n")

    w("### 3.5 Stability Assessment\n")
    if gp_at_1_0 is not None:
        gp_pct_at_1 = gp_at_1_0 * 100
        if gp_pct_at_1 >= 70:
            w(f"At σ(global)=1.0 (scope neutralized with national), GP = "
              f"{fmt(gp_pct_at_1, 1)}%. **The 88% genuinely perspectival "
              f"finding is robust** — it is driven primarily by f(d) and "
              f"structural properties, not scope amplification.\n")
        elif gp_pct_at_1 >= 50:
            w(f"At σ(global)=1.0, GP = {fmt(gp_pct_at_1, 1)}%. The finding "
              f"is **moderately scope-dependent** — GP remains a majority "
              f"but scope amplification contributes meaningfully.\n")
        else:
            w(f"At σ(global)=1.0, GP = {fmt(gp_pct_at_1, 1)}%. **The "
              f"finding is scope-dependent** — the 88% genuinely perspectival "
              f"classification relies on scope modifier amplification. "
              f"Calibration investigation needed.\n")
    w("")

    # ------------------------------------------------------------------
    # Part 3: Divergence Pairs
    # ------------------------------------------------------------------
    w("## 4. Dominant Divergence Pair Analysis\n")

    ap = p3["all_pairs"]
    w(f"### 4.1 All Perspective Pairs (N={ap['n']} genuinely perspectival)\n")
    headers = ["Pair", "Mean |Δχ|", "Median |Δχ|", "Std"]
    rows = []
    for pl in ap["ranking"]:
        s = ap["pair_stats"].get(pl)
        if s:
            rows.append([pl, fmt(s["mean"], 6), fmt(s["median"], 6),
                         fmt(s["std"], 6)])
    w(md_table(headers, rows, ["l", "r", "r", "r"]))
    w(f"\n**Dominant pair**: {ap['ranking'][0] if ap['ranking'] else '---'}\n")

    def write_decomp_section(decomp, pair_name, section):
        w(f"### {section} {pair_name} Decomposition\n")
        if decomp is None:
            w("*No data*\n")
            return
        w(f"N = {decomp['n']}\n")
        headers = ["Component", "Mean", "Median", "Std"]
        rows = []
        for comp in ["delta_chi", "delta_due_to_fd", "delta_due_to_scope",
                      "delta_interaction"]:
            s = decomp["stats"].get(comp)
            if s:
                rows.append([comp, fmt(s["mean"], 6), fmt(s["median"], 6),
                             fmt(s["std"], 6)])
        w(md_table(headers, rows, ["l", "r", "r", "r"]))
        w(f"\nf(d) dominates in {decomp['fd_dominates_count']}/{decomp['n']} "
          f"= {fmt(decomp['fd_dominates_fraction'] * 100, 1)}% of "
          f"constraints.\n")
        # Note about interaction term
        w("*Note: Under the symmetric decomposition, the interaction term "
          "is algebraically zero for constraints without Chi overrides. "
          "Non-zero values indicate override effects.*\n")

    write_decomp_section(p3["institutional_analytical_decomposition"],
                         "Institutional↔Analytical", "4.2")
    write_decomp_section(p3["powerless_institutional_decomposition"],
                         "Powerless↔Institutional", "4.3")

    w("### 4.4 Counterfactual: Powerless at scope_mod=1.0\n")
    cf = p3["counterfactual_powerless_scope_1_0"]
    w("If powerless had scope_mod=1.0 instead of 0.8:\n")
    headers = ["Pair", "Original Mean |Δχ|", "Counterfactual Mean |Δχ|"]
    rows = []
    for pl in cf["ranking"]:
        orig = ap["pair_stats"].get(pl)
        cf_s = cf["pair_stats"].get(pl)
        rows.append([
            pl,
            fmt(orig["mean"], 6) if orig else "---",
            fmt(cf_s["mean"], 6) if cf_s else "---",
        ])
    w(md_table(headers, rows, ["l", "r", "r"]))
    w(f"\n**Counterfactual dominant pair**: "
      f"{cf['ranking'][0] if cf['ranking'] else '---'}\n")

    orig_dom = ap["ranking"][0] if ap["ranking"] else None
    cf_dom = cf["ranking"][0] if cf["ranking"] else None
    if orig_dom == cf_dom:
        w(f"The dominant pair **does not change** under the counterfactual. "
          f"{orig_dom} remains dominant even without scope penalty on "
          f"powerless.\n")
    else:
        w(f"The dominant pair **changes** from {orig_dom} to {cf_dom} when "
          f"powerless scope is neutralized.\n")
    w("")

    # ------------------------------------------------------------------
    # Calibration implications
    # ------------------------------------------------------------------
    w("## 5. Calibration Implications\n")
    w("Based on the three analyses above:\n")
    w("1. **Variance driver**: See Section 2 for f(d) vs scope decomposition\n")
    w("2. **Sweep stability**: See Section 3.5 for robustness assessment\n")
    w("3. **Pair structure**: See Section 4 for what drives "
      "institutional↔analytical dominance\n")
    w("")
    w("## 6. Data Sources\n")
    w("- `outputs/enriched_pipeline.json` — perspective_chi components\n")
    w("- `outputs/tangled_gradient_data.json` — subtype classifications\n")
    w("- `prolog/config.pl` — scope modifiers and gradient boundaries\n")
    w("- `docs/tangled_gradient_analysis.md` — gradient analysis background\n")

    # Write to file
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")

    print(f"Report written to {REPORT_PATH}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    # Load data
    enriched = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline",
                         validate_enriched_pipeline)
    if not enriched:
        print("ERROR: Could not load enriched_pipeline.json", file=sys.stderr)
        return 1

    tangled_gradient_raw = load_json(TANGLED_GRADIENT_PATH,
                                     "tangled_gradient_data")
    if not tangled_gradient_raw:
        print("ERROR: Could not load tangled_gradient_data.json",
              file=sys.stderr)
        return 1

    tangled_gradient = tangled_gradient_raw.get("per_constraint", {})

    # Filter tangled_rope population
    per_constraint = enriched.get("per_constraint", [])
    population = [c for c in per_constraint
                  if c.get("claimed_type") == "tangled_rope"]

    print(f"Population: {len(population)} tangled_rope constraints")
    print(f"Tangled gradient entries: {len(tangled_gradient)}")

    # Discover scope mapping
    scope_map, per_constraint_scopes, scope_warnings = \
        discover_scope_mapping(population)
    print(f"Scope mapping: {scope_map}")
    if scope_warnings:
        for w in scope_warnings:
            print(f"  WARNING: {w}", file=sys.stderr)

    # Detect chi overrides
    overrides = detect_chi_overrides(population)
    print(f"Chi overrides (tolerance={CHI_OVERRIDE_TOLERANCE}): "
          f"{len(overrides)}")

    # Part 1: Variance Decomposition
    print("\nPart 1: Variance Decomposition...")
    p1 = run_variance_decomposition(population, tangled_gradient)
    print(f"  Decomposed: {len(p1['per_constraint'])} constraints "
          f"(skipped: {p1['skipped']})")

    # Part 2: Sensitivity Sweep
    print("\nPart 2: Scope Modifier Sensitivity Sweep...")
    p2 = run_sensitivity_sweep(population, per_constraint_scopes, scope_map)
    print(f"  Global sweep: {len(p2['global_sweep'])} values "
          f"(perspectives: {p2['global_perspectives']})")
    print(f"  Local sweep: {len(p2['local_sweep'])} values "
          f"(perspectives: {p2['local_perspectives']})")

    # Part 3: Divergence Pair Analysis
    print("\nPart 3: Dominant Divergence Pair Analysis...")
    p3 = run_pair_analysis(population, tangled_gradient)
    print(f"  Pairs analyzed: {p3['all_pairs']['n']} genuinely perspectival")

    # Assemble results
    results = {
        "generated": datetime.now().isoformat(),
        "population_size": len(population),
        "genuinely_perspectival_size": sum(
            1 for tg in tangled_gradient.values()
            if tg.get("subtype") == "genuinely_perspectival"),
        "config": {
            "scope_modifier_local": SCOPE_MOD_LOCAL,
            "scope_modifier_national": SCOPE_MOD_NATIONAL,
            "scope_modifier_global": SCOPE_MOD_GLOBAL,
            "rope_chi_ceiling": ROPE_CHI_CEILING,
            "snare_chi_floor": SNARE_CHI_FLOOR,
            "subtype_rope_threshold": SUBTYPE_ROPE_THRESHOLD,
            "subtype_snare_threshold": SUBTYPE_SNARE_THRESHOLD,
            "clamp_range": [CLAMP_MIN, CLAMP_MAX],
            "chi_override_tolerance": CHI_OVERRIDE_TOLERANCE,
            "discovered_scope_map": scope_map,
        },
        "chi_overrides": [{"id": cid, "max_discrepancy": disc}
                          for cid, disc in overrides],
        "part1_variance_decomposition": p1,
        "part2_sensitivity_sweep": p2,
        "part3_divergence_pairs": p3,
    }

    # Write JSON
    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2)
    print(f"\nData written to {DATA_PATH}")

    # Write report
    write_report(results, overrides, scope_map, scope_warnings)

    return 0


if __name__ == "__main__":
    sys.exit(main())
