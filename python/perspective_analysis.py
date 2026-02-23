#!/usr/bin/env python3
"""LLM Presheaf Diagnostic — Post-Experiment Analysis.

Reads the experiment log and enhanced reports to compute:
- Epsilon drift (delta_epsilon) per constraint across perspectives
- 4x4 classification matrix per constraint
- Gate flip rates (boolean structural properties)
- Linter failure patterns by perspective/framing
- Missing cell report
- Cross-run consistency

Usage:
    python3 python/perspective_analysis.py
    python3 python/perspective_analysis.py --log path/to/experiment_log.json
"""

import argparse
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent
RESULTS_DIR = PROJECT_ROOT / "results" / "perspective_experiment"
JSON_EXPERIMENT_DIR = PROJECT_ROOT / "json" / "perspective_experiment"
REPORTS_DIR = PROJECT_ROOT / "outputs" / "constraint_reports"

DEFAULT_LOG = RESULTS_DIR / "experiment_log.json"

ALL_PERSPECTIVES = ["u1", "u2", "u3", "u4"]

# Regex to decompose mangled IDs: {original_id}_{perspective}_{framing}_r{run}
_RE_MANGLED = re.compile(r"^(.+)_(u[1-4])_(exp|str)_r(\d+)$")

# DR type extraction from enhanced reports
_RE_DR_TYPE = re.compile(r"Claimed Type:\s+(\w+)")
_RE_VERDICT = re.compile(r"VERDICT:\s*(GREEN|YELLOW|RED)")

# Structural boolean properties to check for gate flips
BOOLEAN_PROPERTIES = [
    "requires_active_enforcement",
    "has_sunset_clause",
    "emerges_naturally",
    "mandatrophy_resolved",
]

BENEFICIARY_PROPERTIES = ["beneficiaries", "victims"]


# ---------------------------------------------------------------------------
# Mangled ID parser
# ---------------------------------------------------------------------------

def parse_mangled_id(mangled_id: str) -> dict | None:
    """Decompose a mangled ID into its components."""
    m = _RE_MANGLED.match(mangled_id)
    if not m:
        return None
    return {
        "original_id": m.group(1),
        "perspective": m.group(2),
        "framing": m.group(3),
        "run": int(m.group(4)),
    }


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_experiment_log(log_path: Path) -> dict:
    """Load experiment log JSON."""
    if not log_path.exists():
        print(f"Error: Experiment log not found at {log_path}", file=sys.stderr)
        sys.exit(1)
    with open(log_path, "r", encoding="utf-8") as f:
        return json.load(f)


def load_story_json(mangled_id: str) -> dict | None:
    """Load a perspective experiment JSON story file."""
    path = JSON_EXPERIMENT_DIR / f"{mangled_id}.json"
    if not path.exists():
        return None
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError):
        return None


def scan_json_dir() -> dict:
    """Build a synthetic experiment log by scanning json/perspective_experiment/.

    This bypasses the experiment_log.json entirely, reconstructing generation
    records from the JSON story files.  Useful when the log was overwritten
    or when combining data from multiple experiment runs.
    """
    generations = []
    for path in sorted(JSON_EXPERIMENT_DIR.glob("*.json")):
        mangled_id = path.stem
        parsed = parse_mangled_id(mangled_id)
        if not parsed:
            continue
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError):
            continue
        bp = data.get("base_properties", {})
        generations.append({
            "constraint_id": parsed["original_id"],
            "perspective": parsed["perspective"],
            "framing": "experiential" if parsed["framing"] == "exp" else "structural",
            "run": parsed["run"],
            "mangled_id": mangled_id,
            "success": True,
            "epsilon": bp.get("extractiveness"),
            "claimed_type": bp.get("claimed_type", ""),
            "lint_passed": True,  # unknown from JSON alone
            "lint_errors": [],
            "error": "",
            "tokens_in": 0,
            "tokens_out": 0,
            "duration_s": 0,
        })

    # Derive config from the data
    constraints = sorted(set(g["constraint_id"] for g in generations))
    perspectives = sorted(set(g["perspective"] for g in generations))
    framings = sorted(set(g["framing"] for g in generations))

    return {
        "timestamp": "reconstructed from JSON scan",
        "config": {
            "constraints": constraints,
            "perspectives": perspectives,
            "framing": framings[0] if len(framings) == 1 else ", ".join(framings),
            "runs": max((g["run"] for g in generations), default=0),
            "model": "unknown (reconstructed)",
        },
        "summary": {
            "total_generations": len(generations),
            "successful": len(generations),
            "failed": 0,
            "lint_failures": 0,
            "reports_generated": 0,
            "total_tokens_in": 0,
            "total_tokens_out": 0,
            "total_duration_s": 0,
        },
        "lint_stats": {},
        "generations": generations,
    }


def load_enhanced_report(mangled_id: str) -> str | None:
    """Load an enhanced report for a mangled constraint ID."""
    path = REPORTS_DIR / f"{mangled_id}_report.md"
    if not path.exists():
        return None
    try:
        return path.read_text(encoding="utf-8")
    except OSError:
        return None


# ---------------------------------------------------------------------------
# Epsilon drift analysis
# ---------------------------------------------------------------------------

def compute_epsilon_drift(log_data: dict) -> dict:
    """Compute epsilon drift (delta_epsilon) per constraint across perspectives.

    Returns a dict mapping original_constraint_id -> {
        "epsilons": {perspective: [eps_r1, eps_r2, ...]},
        "mean_by_perspective": {perspective: mean_eps},
        "delta_epsilon": max - min of means,
        "all_values": [all eps values],
    }
    """
    # Group successful generations by original constraint
    by_constraint = defaultdict(lambda: defaultdict(list))

    for gen in log_data.get("generations", []):
        if not gen.get("success") or gen.get("epsilon") is None:
            continue
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue
        by_constraint[parsed["original_id"]][parsed["perspective"]].append(gen["epsilon"])

    results = {}
    for cid, perspectives in sorted(by_constraint.items()):
        means = {}
        all_values = []
        for persp, eps_list in perspectives.items():
            means[persp] = sum(eps_list) / len(eps_list)
            all_values.extend(eps_list)

        delta = max(means.values()) - min(means.values()) if means else 0.0

        # Variance decomposition (one-way ANOVA components)
        # Within-perspective variance: average variance within each perspective's runs
        # Between-perspective variance: variance of the perspective means
        within_vars = []
        for persp, eps_list in perspectives.items():
            if len(eps_list) >= 2:
                m = sum(eps_list) / len(eps_list)
                v = sum((x - m) ** 2 for x in eps_list) / (len(eps_list) - 1)
                within_vars.append(v)
        within_var = sum(within_vars) / len(within_vars) if within_vars else 0.0

        mean_values = list(means.values())
        if len(mean_values) >= 2:
            grand_mean = sum(mean_values) / len(mean_values)
            between_var = sum((m - grand_mean) ** 2 for m in mean_values) / (len(mean_values) - 1)
        else:
            between_var = 0.0

        # F-ratio: between/within. F > 1 suggests perspective effect exceeds noise.
        f_ratio = between_var / within_var if within_var > 0 else float('inf') if between_var > 0 else 0.0

        results[cid] = {
            "epsilons": dict(perspectives),
            "mean_by_perspective": means,
            "delta_epsilon": round(delta, 4),
            "all_values": all_values,
            "within_var": round(within_var, 6),
            "between_var": round(between_var, 6),
            "f_ratio": round(f_ratio, 3),
        }

    return results


# ---------------------------------------------------------------------------
# Classification matrix
# ---------------------------------------------------------------------------

def extract_classification_from_report(report_text: str) -> str | None:
    """Extract the DR classification type from an enhanced report."""
    m = _RE_DR_TYPE.search(report_text)
    return m.group(1) if m else None


def build_classification_matrices(log_data: dict) -> dict:
    """Build the 4x4 classification matrix per constraint.

    M[i][j] = DR type of Story_Ui evaluated at Context_Uj.

    Since the enhanced report gives us the overall verdict/type per story,
    we extract the claimed_type from each story's JSON (which represents
    the type assigned from that story's own perspective). The full 4x4 matrix
    requires pipeline classification at all 4 contexts, which is in the
    enhanced report.

    For the MVP, we build a partial matrix from available data:
    - Row i = story from perspective i
    - Column data from the story's own claimed_type and enhanced report
    """
    by_constraint = defaultdict(dict)

    for gen in log_data.get("generations", []):
        if not gen.get("success"):
            continue
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue

        cid = parsed["original_id"]
        persp = parsed["perspective"]
        run = parsed["run"]

        # Load the story JSON for classification data
        story = load_story_json(gen["mangled_id"])
        if not story:
            continue

        framing = parsed["framing"]  # "exp" or "str"
        key = (persp, framing, run)
        entry = {
            "mangled_id": gen["mangled_id"],
            "claimed_type": story["base_properties"].get("claimed_type", "unknown"),
            "framing": framing,
            "perspective_types": {},
        }

        # Extract per-perspective classifications from the story's perspectives array.
        # The DR framework defines 6 power atoms; the canonical 4 observer contexts
        # use powerless/moderate/institutional/analytical.  The LLM sometimes uses
        # non-canonical atoms (organized, powerful, individual_moderate).  Map them
        # to the closest canonical context so the matrix isn't full of dashes.
        for p in story.get("perspectives", []):
            ct = p.get("classification_type", "unknown")
            power = p.get("agent_power", "")
            power_to_persp = {
                "powerless": "u1",
                "moderate": "u2",
                "individual_moderate": "u2",
                "powerful": "u2",       # closer to moderate than institutional
                "organized": "u3",      # collective power ≈ institutional
                "institutional": "u3",
                "analytical": "u4",
            }
            mapped = power_to_persp.get(power)
            if mapped:
                # Don't overwrite if we already have this cell from a more
                # canonical atom (prefer exact matches)
                if mapped not in entry["perspective_types"]:
                    entry["perspective_types"][mapped] = ct

        if cid not in by_constraint:
            by_constraint[cid] = {}
        by_constraint[cid][key] = entry

    # Build matrices
    # Detect if multiple framings are present — if so, include framing in row key
    all_framings = set()
    for entries in by_constraint.values():
        for (persp, framing, run) in entries:
            all_framings.add(framing)
    multi_framing = len(all_framings) > 1

    matrices = {}
    for cid, entries in sorted(by_constraint.items()):
        matrix = {}
        for (persp, framing, run), entry in entries.items():
            if multi_framing:
                row_key = f"{persp}_{framing}_r{run}"
            else:
                row_key = f"{persp}_r{run}"
            matrix[row_key] = entry["perspective_types"]
        matrices[cid] = matrix

    return matrices


# ---------------------------------------------------------------------------
# Classification stability
# ---------------------------------------------------------------------------

def compute_classification_stability(matrices: dict) -> dict:
    """Measure how stable discrete type assignments are across stories.

    For each constraint, compute per-column (evaluation context) stability:
    the fraction of non-missing entries that match the modal type.
    A score of 1.0 means all stories got the same type at that context.

    Also flags columns with high type diversity (3+ types = "chaos").
    """
    results = {}
    for cid, matrix in sorted(matrices.items()):
        column_stats = {}
        for col in ALL_PERSPECTIVES:
            types = []
            for row_key, row in matrix.items():
                t = row.get(col)
                if t and t != "-":
                    types.append(t)

            if not types:
                column_stats[col] = {"stability": None, "modal_type": "-", "n": 0, "types": {}}
                continue

            from collections import Counter
            counts = Counter(types)
            modal_type, modal_count = counts.most_common(1)[0]
            stability = modal_count / len(types)
            unique_types = len(counts)

            column_stats[col] = {
                "stability": round(stability, 3),
                "modal_type": modal_type,
                "n": len(types),
                "unique_types": unique_types,
                "types": dict(counts),
                "chaos": unique_types >= 3,
            }

        # Overall stability for this constraint (mean of non-null columns)
        stabilities = [s["stability"] for s in column_stats.values() if s["stability"] is not None]
        overall = sum(stabilities) / len(stabilities) if stabilities else None

        results[cid] = {
            "columns": column_stats,
            "overall_stability": round(overall, 3) if overall is not None else None,
        }

    return results


# ---------------------------------------------------------------------------
# Gate flip analysis
# ---------------------------------------------------------------------------

def compute_gate_flips(log_data: dict) -> dict:
    """Compare boolean structural properties across perspective stories.

    Returns per-constraint gate flip analysis.
    """
    by_constraint = defaultdict(lambda: defaultdict(list))

    for gen in log_data.get("generations", []):
        if not gen.get("success"):
            continue
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue

        story = load_story_json(gen["mangled_id"])
        if not story:
            continue

        cid = parsed["original_id"]
        bp = story.get("base_properties", {})

        props = {}
        for prop in BOOLEAN_PROPERTIES:
            props[prop] = bp.get(prop, False)
        props["has_beneficiaries"] = bool(bp.get("beneficiaries"))
        props["has_victims"] = bool(bp.get("victims"))

        by_constraint[cid][parsed["perspective"]].append(props)

    results = {}
    for cid, perspectives in sorted(by_constraint.items()):
        flips = {}
        all_props = set()
        for persp_props_list in perspectives.values():
            for props in persp_props_list:
                all_props.update(props.keys())

        for prop in sorted(all_props):
            values_by_persp = {}
            for persp, props_list in perspectives.items():
                vals = [p.get(prop) for p in props_list if prop in p]
                if vals:
                    values_by_persp[persp] = vals[0]  # Use first run

            unique_values = set(values_by_persp.values())
            if len(unique_values) > 1:
                flips[prop] = values_by_persp

        results[cid] = flips

    return results


# ---------------------------------------------------------------------------
# Power atom usage analysis
# ---------------------------------------------------------------------------

def compute_power_atom_usage(log_data: dict) -> dict:
    """Analyze which power atoms the LLM actually generates in story perspectives.

    This reveals whether the LLM systematically skips certain observer positions
    (e.g., the moderate/U2 position) when generating from different preambles.
    """
    from collections import Counter

    by_constraint = defaultdict(lambda: defaultdict(list))
    atom_counter = Counter()
    persp_count_counter = Counter()

    for gen in log_data.get("generations", []):
        if not gen.get("success"):
            continue
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue

        story = load_story_json(gen["mangled_id"])
        if not story:
            continue

        cid = parsed["original_id"]
        perspectives = story.get("perspectives", [])
        persp_count_counter[len(perspectives)] += 1

        powers = [p.get("agent_power", "MISSING") for p in perspectives]
        for power in powers:
            atom_counter[power] += 1
        by_constraint[cid][gen["mangled_id"]] = powers

    # Check U2 coverage: how often does 'moderate' appear?
    canonical = {"powerless", "moderate", "institutional", "analytical"}
    missing_moderate = []
    for cid, stories in by_constraint.items():
        for mid, powers in stories.items():
            if "moderate" not in powers:
                missing_moderate.append(mid)

    return {
        "atom_frequency": dict(atom_counter.most_common()),
        "perspective_count_distribution": dict(persp_count_counter),
        "missing_moderate_count": len(missing_moderate),
        "total_stories": sum(persp_count_counter.values()),
        "missing_moderate_ids": missing_moderate[:20],  # cap for readability
    }


# ---------------------------------------------------------------------------
# Framing comparison (experiential vs structural)
# ---------------------------------------------------------------------------

def compute_framing_comparison(log_data: dict) -> dict | None:
    """Compare experiential vs structural framing on key metrics.

    Returns None if only one framing is present.  When both exist, compares:
    - Epsilon values per constraint/perspective
    - Missing-moderate rates (the "missing middle" question)
    - Classification type agreement
    """
    from collections import Counter

    # Group by (constraint, perspective, framing)
    by_key = defaultdict(lambda: {"epsilons": [], "moderate_present": [], "types": []})

    for gen in log_data.get("generations", []):
        if not gen.get("success"):
            continue
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue

        framing = parsed["framing"]
        cid = parsed["original_id"]
        persp = parsed["perspective"]
        key = (cid, persp, framing)

        if gen.get("epsilon") is not None:
            by_key[key]["epsilons"].append(gen["epsilon"])

        story = load_story_json(gen["mangled_id"])
        if story:
            powers = [p.get("agent_power", "") for p in story.get("perspectives", [])]
            by_key[key]["moderate_present"].append("moderate" in powers)
            by_key[key]["types"].append(
                story.get("base_properties", {}).get("claimed_type", "unknown")
            )

    # Check if we have both framings
    framings = set(k[2] for k in by_key)
    if len(framings) < 2:
        return None

    # Build comparison rows for perspectives that have both framings
    comparisons = []
    for cid in sorted(set(k[0] for k in by_key)):
        for persp in ALL_PERSPECTIVES:
            exp_key = (cid, persp, "exp")
            str_key = (cid, persp, "str")
            if exp_key not in by_key or str_key not in by_key:
                continue

            exp = by_key[exp_key]
            stru = by_key[str_key]

            exp_eps = sum(exp["epsilons"]) / len(exp["epsilons"]) if exp["epsilons"] else None
            str_eps = sum(stru["epsilons"]) / len(stru["epsilons"]) if stru["epsilons"] else None

            exp_mod_rate = (sum(exp["moderate_present"]) / len(exp["moderate_present"])
                           if exp["moderate_present"] else None)
            str_mod_rate = (sum(stru["moderate_present"]) / len(stru["moderate_present"])
                           if stru["moderate_present"] else None)

            exp_type = Counter(exp["types"]).most_common(1)[0][0] if exp["types"] else "-"
            str_type = Counter(stru["types"]).most_common(1)[0][0] if stru["types"] else "-"

            comparisons.append({
                "constraint": cid,
                "perspective": persp,
                "exp_eps": exp_eps,
                "str_eps": str_eps,
                "eps_delta": abs(exp_eps - str_eps) if exp_eps is not None and str_eps is not None else None,
                "exp_moderate_rate": exp_mod_rate,
                "str_moderate_rate": str_mod_rate,
                "exp_type": exp_type,
                "str_type": str_type,
                "type_match": exp_type == str_type,
                "exp_n": len(exp["epsilons"]),
                "str_n": len(stru["epsilons"]),
            })

    if not comparisons:
        return None

    # Aggregate moderate rates
    exp_mod_all = [c["exp_moderate_rate"] for c in comparisons if c["exp_moderate_rate"] is not None]
    str_mod_all = [c["str_moderate_rate"] for c in comparisons if c["str_moderate_rate"] is not None]

    return {
        "comparisons": comparisons,
        "exp_moderate_rate": sum(exp_mod_all) / len(exp_mod_all) if exp_mod_all else None,
        "str_moderate_rate": sum(str_mod_all) / len(str_mod_all) if str_mod_all else None,
        "type_match_rate": sum(1 for c in comparisons if c["type_match"]) / len(comparisons),
    }


# ---------------------------------------------------------------------------
# Cross-run consistency
# ---------------------------------------------------------------------------

def compute_cross_run_consistency(log_data: dict, threshold: float = 0.05) -> dict:
    """Flag pairs where Run1 and Run2 epsilon differ by more than threshold."""
    by_key = defaultdict(list)

    for gen in log_data.get("generations", []):
        if not gen.get("success") or gen.get("epsilon") is None:
            continue
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue
        key = (parsed["original_id"], parsed["perspective"], parsed["framing"])
        by_key[key].append((parsed["run"], gen["epsilon"]))

    inconsistent = {}
    total_pairs = 0
    flagged_pairs = 0

    for key, runs in sorted(by_key.items()):
        if len(runs) < 2:
            continue
        runs.sort(key=lambda x: x[0])
        total_pairs += 1
        eps_values = [r[1] for r in runs]
        delta = max(eps_values) - min(eps_values)
        if delta > threshold:
            flagged_pairs += 1
            inconsistent[f"{key[0]}_{key[1]}_{key[2]}"] = {
                "runs": {f"r{r}": e for r, e in runs},
                "delta": round(delta, 4),
            }

    return {
        "total_pairs": total_pairs,
        "flagged_pairs": flagged_pairs,
        "flagged_fraction": round(flagged_pairs / total_pairs, 3) if total_pairs > 0 else 0,
        "threshold": threshold,
        "details": inconsistent,
    }


# ---------------------------------------------------------------------------
# Missing cell report
# ---------------------------------------------------------------------------

def compute_missing_cells(log_data: dict) -> dict:
    """Report which perspective x constraint combinations failed."""
    expected = defaultdict(set)
    actual = defaultdict(set)

    for gen in log_data.get("generations", []):
        parsed = parse_mangled_id(gen["mangled_id"])
        if not parsed:
            continue
        cid = parsed["original_id"]
        persp = parsed["perspective"]
        expected[cid].add(persp)
        if gen.get("success"):
            actual[cid].add(persp)

    missing = {}
    for cid in sorted(expected.keys()):
        gaps = expected[cid] - actual[cid]
        if gaps:
            missing[cid] = sorted(gaps)

    return missing


# ---------------------------------------------------------------------------
# Linter failure patterns
# ---------------------------------------------------------------------------

def compute_linter_patterns(log_data: dict) -> dict:
    """Extract linter failure patterns from experiment log."""
    return log_data.get("lint_stats", {})


# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def generate_analysis_report(
    log_data: dict,
    epsilon_drift: dict,
    matrices: dict,
    gate_flips: dict,
    cross_run: dict,
    missing_cells: dict,
    lint_patterns: dict,
    power_atom_usage: dict | None = None,
    classification_stability: dict | None = None,
    framing_comparison: dict | None = None,
) -> str:
    """Generate the analysis markdown report."""
    lines = []

    def emit(s=""):
        lines.append(s)

    emit("# LLM Presheaf Diagnostic — Analysis Report")
    emit()
    emit(f"**Generated:** {log_data.get('timestamp', 'unknown')}")
    config = log_data.get("config", {})
    summary = log_data.get("summary", {})
    emit(f"**Constraints:** {len(config.get('constraints', []))}")
    emit(f"**Perspectives:** {config.get('perspectives', [])}")
    emit(f"**Framing:** {config.get('framing', 'unknown')}")
    emit(f"**Runs:** {config.get('runs', 0)}")
    emit(f"**Model:** {config.get('model', 'unknown')}")
    emit()
    emit(f"**Results:** {summary.get('successful', 0)}/{summary.get('total_generations', 0)} "
         f"generations succeeded, {summary.get('lint_failures', 0)} lint failures, "
         f"{summary.get('reports_generated', 0)} reports generated")
    emit(f"**Total tokens:** {summary.get('total_tokens_in', 0)} -> {summary.get('total_tokens_out', 0)}")
    emit(f"**Duration:** {summary.get('total_duration_s', 0):.1f}s")
    emit()

    # Section 1: Epsilon Drift
    emit("---")
    emit()
    emit("## 1. Epsilon Drift (delta_epsilon)")
    emit()
    emit("The headline test: does `delta_epsilon = max(eps) - min(eps)` across perspectives")
    emit("exceed the stochastic baseline?")
    emit()

    if epsilon_drift:
        # Summary table
        emit("| Constraint | U1 mean | U2 mean | U3 mean | U4 mean | delta_eps | Status |")
        emit("|---|---|---|---|---|---|---|")
        for cid, data in sorted(epsilon_drift.items()):
            means = data["mean_by_perspective"]
            u1 = means.get("u1", "-")
            u2 = means.get("u2", "-")
            u3 = means.get("u3", "-")
            u4 = means.get("u4", "-")
            delta = data["delta_epsilon"]

            u1_s = f"{u1:.3f}" if isinstance(u1, float) else u1
            u2_s = f"{u2:.3f}" if isinstance(u2, float) else u2
            u3_s = f"{u3:.3f}" if isinstance(u3, float) else u3
            u4_s = f"{u4:.3f}" if isinstance(u4, float) else u4

            status = "DRIFT" if delta > 0.05 else "STABLE"
            emit(f"| {cid} | {u1_s} | {u2_s} | {u3_s} | {u4_s} | {delta:.4f} | {status} |")
        emit()

        # Aggregate statistics
        deltas = [d["delta_epsilon"] for d in epsilon_drift.values()]
        mean_delta = sum(deltas) / len(deltas) if deltas else 0
        max_delta = max(deltas) if deltas else 0
        drifting = sum(1 for d in deltas if d > 0.05)
        emit(f"**Aggregate:** mean delta_epsilon = {mean_delta:.4f}, "
             f"max = {max_delta:.4f}, "
             f"{drifting}/{len(deltas)} constraints show drift > 0.05")
        emit()

        # Directional test: epsilon_U1 > epsilon_U4?
        u1_gt_u4 = 0
        u4_gt_u1 = 0
        for cid, data in epsilon_drift.items():
            means = data["mean_by_perspective"]
            if "u1" in means and "u4" in means:
                if means["u1"] > means["u4"]:
                    u1_gt_u4 += 1
                elif means["u4"] > means["u1"]:
                    u4_gt_u1 += 1
        emit(f"**Directional test (sympathy bias):** epsilon_U1 > epsilon_U4 in "
             f"{u1_gt_u4} constraints, epsilon_U4 > epsilon_U1 in {u4_gt_u1}")
        emit()

        # Variance decomposition table
        has_anova = any(d.get("f_ratio") is not None for d in epsilon_drift.values())
        if has_anova:
            emit("### Variance Decomposition (ANOVA-style)")
            emit()
            emit("Tests whether between-perspective variance exceeds within-perspective "
                 "(stochastic) variance. F > 1 suggests a perspective effect beyond noise.")
            emit()
            emit("| Constraint | Within-var | Between-var | F-ratio | Signal? |")
            emit("|---|---|---|---|---|")
            for cid, data in sorted(epsilon_drift.items()):
                wv = data.get("within_var", 0)
                bv = data.get("between_var", 0)
                fr = data.get("f_ratio", 0)
                signal = "YES" if fr > 1.0 else "NO"
                if fr == float('inf'):
                    fr_s = "inf"
                else:
                    fr_s = f"{fr:.3f}"
                emit(f"| {cid} | {wv:.6f} | {bv:.6f} | {fr_s} | {signal} |")
            emit()

            f_above_1 = sum(1 for d in epsilon_drift.values()
                            if d.get("f_ratio", 0) > 1.0 and d.get("f_ratio") != float('inf'))
            f_total = len(epsilon_drift)
            emit(f"**{f_above_1}/{f_total} constraints show F > 1** (between-perspective "
                 f"variance exceeds within-perspective noise).")
            emit()
            emit("Note: With k=4 groups and n=2-5 per group, critical F(3, ~16) at p=0.05 "
                 "is approximately 3.24. F-ratios below this are not statistically significant.")
            emit()
    else:
        emit("*No epsilon data available.*")
        emit()

    # Section 2: Classification Matrices
    emit("---")
    emit()
    emit("## 2. Classification Matrices (4x4)")
    emit()

    if matrices:
        for cid, matrix in sorted(matrices.items()):
            emit(f"### {cid}")
            emit()
            emit("| Story \\ Eval | U1 | U2 | U3 | U4 |")
            emit("|---|---|---|---|---|")
            for row_key in sorted(matrix.keys()):
                types = matrix[row_key]
                u1 = types.get("u1", "-")
                u2 = types.get("u2", "-")
                u3 = types.get("u3", "-")
                u4 = types.get("u4", "-")
                emit(f"| {row_key} | {u1} | {u2} | {u3} | {u4} |")
            emit()
    else:
        emit("*No classification data available.*")
        emit()

    # Section 2b: Classification Stability
    if classification_stability:
        emit("---")
        emit()
        emit("### Classification Stability (per evaluation context)")
        emit()
        emit("Measures what fraction of stories receive the same type at each evaluation "
             "context. Stability = 1.0 means all stories agree. 'Chaos' flags columns with "
             "3+ distinct types.")
        emit()

        emit("| Constraint | U1 | U2 | U3 | U4 | Overall |")
        emit("|---|---|---|---|---|---|")
        for cid, data in sorted(classification_stability.items()):
            cols = data["columns"]
            cells = []
            for col in ALL_PERSPECTIVES:
                cs = cols.get(col, {})
                stab = cs.get("stability")
                if stab is None:
                    cells.append("-")
                else:
                    modal = cs.get("modal_type", "?")
                    chaos = " **CHAOS**" if cs.get("chaos") else ""
                    cells.append(f"{stab:.2f} ({modal}){chaos}")
            overall = data.get("overall_stability")
            overall_s = f"{overall:.3f}" if overall is not None else "-"
            emit(f"| {cid} | {cells[0]} | {cells[1]} | {cells[2]} | {cells[3]} | {overall_s} |")
        emit()

        # Flag any chaos columns
        chaos_found = []
        for cid, data in classification_stability.items():
            for col, cs in data["columns"].items():
                if cs.get("chaos"):
                    types_str = ", ".join(f"{t}:{n}" for t, n in cs["types"].items())
                    chaos_found.append(f"- **{cid}** at **{col.upper()}**: {types_str}")
        if chaos_found:
            emit("**Chaotic columns** (3+ distinct types — model doesn't know what this constraint is here):")
            emit()
            for line in chaos_found:
                emit(line)
            emit()

    # Section 3: Gate Flip Rates
    emit("---")
    emit()
    emit("## 3. Gate Flip Rates (Boolean Structural Properties)")
    emit()

    if gate_flips:
        any_flips = False
        for cid, flips in sorted(gate_flips.items()):
            if flips:
                any_flips = True
                emit(f"### {cid}")
                emit()
                for prop, values in sorted(flips.items()):
                    emit(f"- **{prop}**: {values}")
                emit()
        if not any_flips:
            emit("*No gate flips detected across perspectives.*")
            emit()
    else:
        emit("*No gate flip data available.*")
        emit()

    # Section 4: Cross-Run Consistency
    emit("---")
    emit()
    emit("## 4. Cross-Run Consistency")
    emit()
    emit(f"**Threshold:** delta > {cross_run['threshold']}")
    emit(f"**Total pairs:** {cross_run['total_pairs']}")
    emit(f"**Flagged:** {cross_run['flagged_pairs']} "
         f"({cross_run['flagged_fraction']*100:.1f}%)")
    emit()

    if cross_run["details"]:
        emit("| Key | Runs | Delta |")
        emit("|---|---|---|")
        for key, detail in sorted(cross_run["details"].items()):
            runs_str = ", ".join(f"{k}={v:.3f}" for k, v in detail["runs"].items())
            emit(f"| {key} | {runs_str} | {detail['delta']:.4f} |")
        emit()

        if cross_run["flagged_fraction"] > 0.3:
            emit("**WARNING:** More than 30% of condition-pairs exceed the consistency threshold.")
            emit("The stochastic noise floor may be too high for this experiment as designed.")
            emit("Consider additional runs.")
            emit()
    else:
        emit("*All cross-run pairs within consistency threshold.*")
        emit()

    # Section 5: Missing Cells
    emit("---")
    emit()
    emit("## 5. Missing Cells")
    emit()

    if missing_cells:
        emit("| Constraint | Missing Perspectives |")
        emit("|---|---|")
        for cid, gaps in sorted(missing_cells.items()):
            emit(f"| {cid} | {', '.join(gaps)} |")
        emit()
    else:
        emit("*All cells present — no missing data.*")
        emit()

    # Section 6: Power Atom Usage (U2 Skip Pattern)
    emit("---")
    emit()
    emit("## 6. Power Atom Usage (Perspective Coverage)")
    emit()

    if power_atom_usage:
        total = power_atom_usage["total_stories"]
        missing = power_atom_usage["missing_moderate_count"]
        emit(f"**Stories analyzed:** {total}")
        emit(f"**Stories missing `moderate` power atom:** {missing}/{total} "
             f"({missing/total*100:.0f}%)")
        emit()

        emit("**Power atom frequency across all story perspectives:**")
        emit()
        emit("| Power Atom | Count |")
        emit("|---|---|")
        for atom, count in power_atom_usage["atom_frequency"].items():
            canonical_mark = " (canonical)" if atom in {"powerless", "moderate", "institutional", "analytical"} else ""
            emit(f"| {atom}{canonical_mark} | {count} |")
        emit()

        emit("**Perspective count per story:**")
        emit()
        for k, v in sorted(power_atom_usage["perspective_count_distribution"].items()):
            emit(f"- {k} perspectives: {v} stories")
        emit()

        if missing > total * 0.5:
            emit("**FINDING:** The LLM systematically skips the `moderate` (U2) observer position. "
                 "It prefers the three structurally extreme positions: powerless, institutional, "
                 "analytical. The U2 column dashes in classification matrices above reflect "
                 "genuine missing perspectives in the generated stories, not analysis artifacts.")
            emit()
    else:
        emit("*No power atom data available.*")
        emit()

    # Section 7: Framing Comparison (experiential vs structural)
    if framing_comparison:
        emit("---")
        emit()
        emit("## 7. Framing Comparison (Experiential vs Structural)")
        emit()
        emit("Direct comparison of experiential vs structural framing for overlapping "
             "perspective × constraint cells. The key question: does structural framing "
             "produce the `moderate` power atom more reliably (is the missing middle a "
             "framing effect or a capacity limitation)?")
        emit()

        comps = framing_comparison["comparisons"]
        emit("| Constraint | Persp | Exp ε (n) | Str ε (n) | Δε | Exp mod% | Str mod% | Exp type | Str type | Match? |")
        emit("|---|---|---|---|---|---|---|---|---|---|")
        for c in comps:
            exp_e = f"{c['exp_eps']:.3f}" if c['exp_eps'] is not None else "-"
            str_e = f"{c['str_eps']:.3f}" if c['str_eps'] is not None else "-"
            delta = f"{c['eps_delta']:.3f}" if c['eps_delta'] is not None else "-"
            exp_m = f"{c['exp_moderate_rate']*100:.0f}%" if c['exp_moderate_rate'] is not None else "-"
            str_m = f"{c['str_moderate_rate']*100:.0f}%" if c['str_moderate_rate'] is not None else "-"
            match = "YES" if c['type_match'] else "**NO**"
            emit(f"| {c['constraint']} | {c['perspective']} | {exp_e} ({c['exp_n']}) | "
                 f"{str_e} ({c['str_n']}) | {delta} | {exp_m} | {str_m} | "
                 f"{c['exp_type']} | {c['str_type']} | {match} |")
        emit()

        exp_mod = framing_comparison.get("exp_moderate_rate")
        str_mod = framing_comparison.get("str_moderate_rate")
        match_rate = framing_comparison.get("type_match_rate", 0)
        emit(f"**Aggregate moderate rate:** experiential = "
             f"{exp_mod*100:.0f}%, structural = {str_mod*100:.0f}%"
             if exp_mod is not None and str_mod is not None
             else "**Aggregate moderate rate:** insufficient data")
        emit(f"**Type agreement:** {match_rate*100:.0f}% of cells match across framings")
        emit()

        if exp_mod is not None and str_mod is not None:
            if str_mod > exp_mod + 0.15:
                emit("**FINDING:** Structural framing significantly increases moderate atom production. "
                     "The missing middle is at least partially a **framing effect** — the experiential "
                     "preamble's phenomenological language steers the LLM away from the moderate position.")
            elif abs(str_mod - exp_mod) <= 0.15:
                emit("**FINDING:** Structural framing does NOT significantly change moderate atom production. "
                     "The missing middle appears to be a **capacity limitation** — the LLM struggles to "
                     "conceptualize a moderate observer position regardless of how it's framed.")
            else:
                emit("**FINDING:** Experiential framing produces more moderate atoms than structural. "
                     "This is unexpected and may warrant further investigation.")
            emit()

    # Section 8: Linter Failure Patterns
    emit("---")
    emit()
    emit("## 8. Linter Failure Patterns")
    emit()

    if lint_patterns:
        emit("| Perspective_Framing | Total | Passed | Failed | Fail Rate |")
        emit("|---|---|---|---|---|")
        for key, stats in sorted(lint_patterns.items()):
            total = stats.get("total", 0)
            passed = stats.get("passed", 0)
            failed = stats.get("failed", 0)
            rate = f"{failed/total*100:.1f}%" if total > 0 else "N/A"
            emit(f"| {key} | {total} | {passed} | {failed} | {rate} |")
        emit()
    else:
        emit("*No linter pattern data available.*")
        emit()

    emit("---")
    emit()
    emit("*Analysis generated by `python/perspective_analysis.py`*")
    emit()

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="LLM Presheaf Diagnostic — Post-Experiment Analysis"
    )
    parser.add_argument(
        "--log", "-l",
        type=Path,
        default=DEFAULT_LOG,
        help=f"Path to experiment_log.json (default: {DEFAULT_LOG})"
    )
    parser.add_argument(
        "--output", "-o",
        type=Path,
        default=RESULTS_DIR / "analysis.md",
        help="Output path for analysis report"
    )
    parser.add_argument(
        "--scan-json", action="store_true",
        help="Bypass experiment_log.json and reconstruct data from json/perspective_experiment/ files"
    )
    args = parser.parse_args()

    # Load experiment data
    if args.scan_json:
        print(f"Scanning JSON files from {JSON_EXPERIMENT_DIR}...")
        log_data = scan_json_dir()
        print(f"  Found {log_data['summary']['total_generations']} stories "
              f"({len(log_data['config']['constraints'])} constraints, "
              f"framing: {log_data['config']['framing']})")
    else:
        print(f"Loading experiment log from {args.log}...")
        log_data = load_experiment_log(args.log)

    # Compute analyses
    print("Computing epsilon drift...")
    epsilon_drift = compute_epsilon_drift(log_data)

    print("Building classification matrices...")
    matrices = build_classification_matrices(log_data)

    print("Computing classification stability...")
    classification_stability = compute_classification_stability(matrices)

    print("Computing gate flip rates...")
    gate_flips = compute_gate_flips(log_data)

    print("Checking cross-run consistency...")
    cross_run = compute_cross_run_consistency(log_data)

    print("Computing missing cells...")
    missing_cells = compute_missing_cells(log_data)

    print("Analyzing power atom usage...")
    power_atom_usage = compute_power_atom_usage(log_data)

    print("Extracting linter patterns...")
    lint_patterns = compute_linter_patterns(log_data)

    print("Computing framing comparison...")
    framing_comparison = compute_framing_comparison(log_data)

    # Generate report
    print("Generating analysis report...")
    report = generate_analysis_report(
        log_data, epsilon_drift, matrices, gate_flips,
        cross_run, missing_cells, lint_patterns, power_atom_usage,
        classification_stability, framing_comparison,
    )

    # Write output
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(report, encoding="utf-8")
    print(f"Analysis report written to {args.output}")


if __name__ == "__main__":
    main()
