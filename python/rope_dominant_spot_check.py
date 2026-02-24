#!/usr/bin/env python3
"""Rope-Dominant Spot Check

Spot-checks the 28 rope-dominant tangled_rope constraints — those where
max(g_chi) < 0.30 across all 4 perspectives.  Chi never reaches snare
territory regardless of observer position.  The question: does the
tangled_rope label add real signal for these 28, or is it labeling noise?

Key structural finding: d, f(d), and scope_mod are per-perspective
constants (identical across all constraints).  Rope-dominance is driven
entirely by epsilon.  The f(d) spread diagnostic will be trivially 0/28.

Reads:  outputs/enriched_pipeline.json
        outputs/chi_variance_decomposition_data.json
        outputs/tangled_gradient_data.json
        prolog/testsets/*.pl  (28 files only)

Writes: outputs/rope_dominant_spot_check_data.json
        docs/rope_dominant_spot_check.md

Usage:  python3 python/rope_dominant_spot_check.py
"""

import json
import math
import re
import sys
from collections import Counter
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import (load_json, read_config, ENRICHED_PIPELINE_JSON,
                           OUTPUT_DIR, PROLOG_DIR)
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
TESTSET_DIR = PROLOG_DIR / "testsets"
REPORT_PATH = DOCS_DIR / "rope_dominant_spot_check.md"
DATA_PATH = OUTPUT_DIR / "rope_dominant_spot_check_data.json"
TANGLED_GRADIENT_PATH = OUTPUT_DIR / "tangled_gradient_data.json"
CHI_VARIANCE_DATA_PATH = OUTPUT_DIR / "chi_variance_decomposition_data.json"

PERSPECTIVE_KEYS = ["powerless", "moderate", "institutional", "analytical"]

# ---------------------------------------------------------------------------
# Config thresholds
# ---------------------------------------------------------------------------

_CFG = read_config()

ROPE_CHI_CEILING = _CFG.get("rope_chi_ceiling", 0.35)
SNARE_CHI_FLOOR = _CFG.get("snare_chi_floor", 0.66)

# Subtype classification threshold (matches tangled_gradient.py / chi_variance_decomposition.py)
SUBTYPE_ROPE_THRESHOLD = 0.30

# Diagnostic thresholds
LOW_EPSILON_THRESHOLD = 0.15
SIGMOID_COMPRESSION_THRESHOLD = 0.5
FD_SPREAD_THRESHOLD = 0.1

# Recommendation tier boundaries
RECLASSIFY_EPSILON_CEILING = 0.10
KEEP_EPSILON_FLOOR = 0.20


# ---------------------------------------------------------------------------
# Helpers (matching chi_variance_decomposition.py patterns)
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


# ---------------------------------------------------------------------------
# Prolog spec parser (following coordination_vitality_diagnostic.py:305-375)
# ---------------------------------------------------------------------------

RE_BENEFICIARY = re.compile(
    r"^[^%]*constraint_beneficiary\(\s*[\w']+\s*,\s*(\w+)\s*\)")
RE_VICTIM = re.compile(
    r"^[^%]*constraint_victim\(\s*[\w']+\s*,\s*(\w+)\s*\)")
RE_OMEGA = re.compile(
    r'omega_variable\(\s*\w+\s*,\s*\w+\s*,\s*[\'"]([^\'"]+)[\'"]')
RE_DOC_BLOCK = re.compile(r'/\*\*(.+?)\*/', re.DOTALL)


def _extract_section(block_text, header):
    """Extract a named section from a /** ... */ doc comment block.

    Looks for lines like ' * SUMMARY:' and collects subsequent ' * ...' lines
    until the next section header or end of block.
    """
    lines = block_text.splitlines()
    capture = []
    capturing = False
    for line in lines:
        stripped = line.strip().lstrip("* ").strip()
        # Check for section header (e.g. "SUMMARY:", "KEY AGENTS ...")
        if stripped.upper().startswith(header.upper()):
            # Skip the header line itself, capture what follows
            remainder = stripped[len(header):].lstrip(":").strip()
            if remainder:
                capture.append(remainder)
            capturing = True
            continue
        if capturing:
            # Stop at next section header (all-caps word followed by colon)
            if (stripped and stripped[0].isupper() and ":" in stripped
                    and stripped.split(":")[0].replace(" ", "").isupper()
                    and len(stripped.split(":")[0]) > 2):
                break
            if stripped:
                capture.append(stripped)
    return " ".join(capture).strip() if capture else None


def parse_prolog_spec(pl_path):
    """Parse a single Prolog testset file for spot-check context.

    Returns dict with summary, key_agents, beneficiaries, victims,
    omega_questions.  Returns empty-field dict if file cannot be read.
    """
    empty = {
        "summary": None,
        "key_agents": None,
        "beneficiaries": [],
        "victims": [],
        "omega_questions": [],
        "file_found": False,
    }
    if not pl_path.exists():
        return empty

    try:
        content = pl_path.read_text(encoding="utf-8", errors="replace")
    except Exception:
        return empty

    result = dict(empty)
    result["file_found"] = True

    # Extract doc blocks
    for match in RE_DOC_BLOCK.finditer(content):
        block = match.group(1)
        if result["summary"] is None:
            result["summary"] = _extract_section(block, "SUMMARY")
        if result["key_agents"] is None:
            result["key_agents"] = _extract_section(block, "KEY AGENTS")

    # Beneficiaries / victims
    for line in content.splitlines():
        m = RE_BENEFICIARY.search(line)
        if m:
            atom = m.group(1)
            if atom not in result["beneficiaries"]:
                result["beneficiaries"].append(atom)
        m = RE_VICTIM.search(line)
        if m:
            atom = m.group(1)
            if atom not in result["victims"]:
                result["victims"].append(atom)

    # Omega questions
    for m in RE_OMEGA.finditer(content):
        result["omega_questions"].append(m.group(1))

    return result


# ---------------------------------------------------------------------------
# Per-constraint analysis
# ---------------------------------------------------------------------------

def analyze_constraint(c, chi_var_entry, gradient_entry, chi_override_ids,
                       prolog_spec):
    """Full analysis of one rope-dominant constraint."""
    cid = c["id"]
    pchi = c.get("perspective_chi", {})

    # --- Core identity ---
    epsilon = c.get("base_extractiveness")
    suppression = c.get("suppression")
    theater_ratio = c.get("theater_ratio")
    human_readable = c.get("human_readable")
    topic_domain = c.get("topic_domain")
    claimed_type = c.get("claimed_type")

    # --- Per-perspective data ---
    per_perspective = {}
    fd_values = []
    chi_values = []
    for pk in PERSPECTIVE_KEYS:
        pd = pchi.get(pk, {})
        d = pd.get("d")
        f_d = pd.get("f_d")
        scope_mod = pd.get("scope_mod")
        chi = pd.get("chi")
        gradient = None
        if gradient_entry:
            gradient = (gradient_entry.get("gradient", {})
                        .get("chi_gradients", {}).get(pk))
        per_perspective[pk] = {
            "d": d,
            "f_d": f_d,
            "scope_mod": scope_mod,
            "chi": chi,
            "gradient": gradient,
        }
        if f_d is not None:
            fd_values.append(f_d)
        if chi is not None:
            chi_values.append(chi)

    # Max gradient
    all_grads = [per_perspective[pk]["gradient"] for pk in PERSPECTIVE_KEYS
                 if per_perspective[pk]["gradient"] is not None]
    max_gradient = max(all_grads) if all_grads else None
    max_chi = max(chi_values) if chi_values else None

    # --- Perspective type divergence ---
    perspectives = c.get("perspectives", {})
    perspective_types = dict(Counter(perspectives.values()))
    unique_types = set(perspectives.values())
    has_perspective_divergence = len(unique_types) > 1
    has_tangled_or_snare = bool(
        unique_types & {"tangled_rope", "snare"})

    # --- Chi override ---
    is_chi_override = cid in chi_override_ids

    # --- Diagnostics ---
    low_epsilon_trivial = epsilon is not None and epsilon < LOW_EPSILON_THRESHOLD
    sigmoid_compressed = (
        bool(fd_values) and all(v < SIGMOID_COMPRESSION_THRESHOLD
                                for v in fd_values))
    fd_spread = (max(fd_values) - min(fd_values)) if fd_values else 0.0
    low_fd_spread = fd_spread < FD_SPREAD_THRESHOLD

    # --- Tangled gradient cross-references ---
    cross_refs = {}
    perspectival_data = {}
    if gradient_entry:
        cr = gradient_entry.get("cross_references", {})
        cross_refs = {
            "tangled_psi": cr.get("tangled_psi"),
            "tangled_band": cr.get("tangled_band"),
            "signature": cr.get("signature"),
            "coalition_type": cr.get("coalition_type"),
        }
        pd_raw = gradient_entry.get("perspectival", {})
        perspectival_data = {
            "chi_variance": pd_raw.get("chi_variance"),
            "gradient_flip": pd_raw.get("gradient_flip"),
            "max_divergence_pair": pd_raw.get("max_divergence_pair"),
        }

    # --- Variance decomposition data ---
    var_data = {}
    if chi_var_entry:
        var_data = {
            "var_total": chi_var_entry.get("var_total"),
            "fd_fraction": chi_var_entry.get("fd_fraction"),
            "range_chi": chi_var_entry.get("range_chi"),
            "f_d_mean": chi_var_entry.get("f_d_mean"),
        }

    # --- Recommendation ---
    analysis = {
        "id": cid,
        "human_readable": human_readable,
        "topic_domain": topic_domain,
        "claimed_type": claimed_type,
        "epsilon": epsilon,
        "suppression": suppression,
        "theater_ratio": theater_ratio,
        "per_perspective": per_perspective,
        "max_gradient": max_gradient,
        "max_chi": max_chi,
        "perspectives_raw": dict(perspectives),
        "perspective_type_counts": perspective_types,
        "has_perspective_divergence": has_perspective_divergence,
        "has_tangled_or_snare": has_tangled_or_snare,
        "is_chi_override": is_chi_override,
        "diagnostics": {
            "low_epsilon_trivial": low_epsilon_trivial,
            "sigmoid_compressed": sigmoid_compressed,
            "low_fd_spread": low_fd_spread,
            "fd_spread": round(fd_spread, 6),
        },
        "cross_references": cross_refs,
        "perspectival": perspectival_data,
        "variance_decomposition": var_data,
        "prolog_spec": prolog_spec,
    }

    analysis["recommendation"], analysis["recommendation_reason"] = \
        classify_recommendation(analysis)

    return analysis


def classify_recommendation(analysis):
    """Classify recommendation tier: Keep / Reclassify / Investigate.

    Automated triage heuristic — prioritizes where human spec review is most
    needed.  Not a final judgment.

    'Perspective sees tangled_rope/snare' means the enriched pipeline type
    LABEL, not the gradient value.  A constraint can be gradient-level
    rope-dominant while still having perspectives classified as tangled_rope.
    """
    eps = analysis.get("epsilon")
    has_ts = analysis.get("has_tangled_or_snare")

    # Reclassify: epsilon < 0.10 — trivially low extraction
    if eps is not None and eps < RECLASSIFY_EPSILON_CEILING:
        return ("reclassify",
                f"Trivially low epsilon ({fmt(eps, 2)}); "
                f"rope-dominant is obviously correct")

    # Investigate: 0.10 <= epsilon < 0.20 — moderate zone
    if eps is not None and eps < KEEP_EPSILON_FLOOR:
        return ("investigate",
                f"Moderate epsilon ({fmt(eps, 2)}); "
                f"tangled_rope label may or may not add signal — "
                f"needs spec review")

    # Keep: epsilon >= 0.20 AND at least one tangled_rope/snare label
    if eps is not None and eps >= KEEP_EPSILON_FLOOR and has_ts:
        return ("keep",
                f"Meaningful epsilon ({fmt(eps, 2)}) with "
                f"tangled_rope/snare perspective labels; "
                f"classification is doing structural work")

    # Edge case: epsilon >= 0.20 but no tangled/snare label
    if eps is not None and eps >= KEEP_EPSILON_FLOOR and not has_ts:
        return ("investigate",
                f"Epsilon ({fmt(eps, 2)}) is meaningful but "
                f"no perspective sees tangled_rope/snare — "
                f"possible mislabel")

    return ("investigate", "Insufficient data for automated classification")


# ---------------------------------------------------------------------------
# Population analysis
# ---------------------------------------------------------------------------

def population_analysis(analyses):
    """Aggregate statistics across all rope_dominant constraints."""
    n = len(analyses)

    # Diagnostic counts
    low_eps_count = sum(1 for a in analyses
                        if a["diagnostics"]["low_epsilon_trivial"])
    sigmoid_comp_count = sum(1 for a in analyses
                             if a["diagnostics"]["sigmoid_compressed"])
    low_fd_count = sum(1 for a in analyses
                       if a["diagnostics"]["low_fd_spread"])
    chi_override_count = sum(1 for a in analyses if a["is_chi_override"])
    divergent_count = sum(1 for a in analyses
                          if a["has_perspective_divergence"])
    tangled_snare_count = sum(1 for a in analyses
                              if a["has_tangled_or_snare"])

    # Epsilon distribution
    epsilons = [a["epsilon"] for a in analyses if a["epsilon"] is not None]
    epsilon_stats = desc_stats(epsilons)

    # Max-chi distribution
    max_chis = [a["max_chi"] for a in analyses if a["max_chi"] is not None]
    max_chi_stats = desc_stats(max_chis)

    # Domain distribution
    domain_counts = Counter(
        a["topic_domain"] for a in analyses if a["topic_domain"])

    # Recommendation tier counts
    tier_counts = Counter(a["recommendation"] for a in analyses)

    return {
        "n": n,
        "diagnostic_counts": {
            "low_epsilon_trivial": low_eps_count,
            "sigmoid_compressed": sigmoid_comp_count,
            "low_fd_spread": low_fd_count,
            "chi_override_overlap": chi_override_count,
            "perspective_divergent": divergent_count,
            "has_tangled_or_snare_label": tangled_snare_count,
        },
        "epsilon_stats": epsilon_stats,
        "max_chi_stats": max_chi_stats,
        "domain_distribution": dict(domain_counts.most_common()),
        "recommendation_tiers": dict(tier_counts),
    }


# ---------------------------------------------------------------------------
# Report generation
# ---------------------------------------------------------------------------

def write_report(analyses, pop_stats):
    """Write the markdown analysis report."""
    now = datetime.now().strftime("%Y-%m-%d %H:%M")
    n = pop_stats["n"]
    dc = pop_stats["diagnostic_counts"]
    tiers = pop_stats["recommendation_tiers"]

    lines = []
    w = lines.append

    w("# Rope-Dominant Spot Check\n")
    w(f"*Generated {now} by `python/rope_dominant_spot_check.py`*\n")
    w("---\n")

    # ------------------------------------------------------------------
    # 1. Executive Summary
    # ------------------------------------------------------------------
    w("## 1. Executive Summary\n")
    w(f"Analyzed **{n}** rope-dominant tangled_rope constraints "
      f"(max g_chi < {SUBTYPE_ROPE_THRESHOLD} across all 4 perspectives).\n")

    w("### Diagnostic Counts\n")
    w(md_table(
        ["Diagnostic", "Count", "%"],
        [
            [f"Low-epsilon trivial (eps < {LOW_EPSILON_THRESHOLD})",
             dc["low_epsilon_trivial"], f"{pct(dc['low_epsilon_trivial'], n)}%"],
            [f"Sigmoid-compressed (all f(d) < {SIGMOID_COMPRESSION_THRESHOLD})",
             dc["sigmoid_compressed"], f"{pct(dc['sigmoid_compressed'], n)}%"],
            [f"Low f(d) spread (< {FD_SPREAD_THRESHOLD})",
             dc["low_fd_spread"], f"{pct(dc['low_fd_spread'], n)}%"],
            ["Chi override overlap",
             dc["chi_override_overlap"],
             f"{pct(dc['chi_override_overlap'], n)}%"],
            ["Perspective divergent (>1 type label)",
             dc["perspective_divergent"],
             f"{pct(dc['perspective_divergent'], n)}%"],
            ["Has tangled_rope/snare label",
             dc["has_tangled_or_snare_label"],
             f"{pct(dc['has_tangled_or_snare_label'], n)}%"],
        ],
        ["l", "r", "r"],
    ))
    w("")

    w("### Recommendation Tiers\n")
    tier_rows = []
    for tier in ["keep", "investigate", "reclassify"]:
        cnt = tiers.get(tier, 0)
        tier_rows.append([tier.capitalize(), cnt, f"{pct(cnt, n)}%"])
    w(md_table(["Tier", "Count", "%"], tier_rows, ["l", "r", "r"]))
    w("")

    # ------------------------------------------------------------------
    # 2. Structural Observation
    # ------------------------------------------------------------------
    w("## 2. Structural Observation\n")
    w("All 28 constraints share **identical d, f(d), and scope_mod values** "
      "per perspective:\n")
    # Show the invariant values from the first constraint
    if analyses:
        first = analyses[0]
        inv_rows = []
        for pk in PERSPECTIVE_KEYS:
            pp = first["per_perspective"].get(pk, {})
            inv_rows.append([
                pk,
                fmt(pp.get("d")),
                fmt(pp.get("f_d"), 6),
                fmt(pp.get("scope_mod"), 1),
            ])
        w(md_table(["Perspective", "d", "f(d)", "scope_mod"],
                   inv_rows, ["l", "r", "r", "r"]))
    w("")
    w("Since chi = epsilon × f(d) × scope_mod, and f(d) and scope_mod are "
      "per-perspective constants, **rope-dominance is driven entirely by "
      "epsilon**.  A constraint with low epsilon produces low chi across "
      "all perspectives, guaranteeing max(g_chi) < 0.30.\n")
    w("Consequently:\n")
    w(f"- **f(d) spread** = {fmt(analyses[0]['diagnostics']['fd_spread'], 6) if analyses else '---'} "
      f"for all {n} (structural invariant)\n"
      f"- **Sigmoid-compressed count** = {dc['sigmoid_compressed']} "
      f"(powerless f(d) = "
      f"{fmt(analyses[0]['per_perspective']['powerless'].get('f_d'), 6) if analyses else '---'} "
      f"> {SIGMOID_COMPRESSION_THRESHOLD})\n")

    # ------------------------------------------------------------------
    # 3. Population Statistics
    # ------------------------------------------------------------------
    w("## 3. Population Statistics\n")

    # 3.1 Epsilon distribution
    w("### 3.1 Epsilon Distribution\n")
    es = pop_stats["epsilon_stats"]
    if es:
        w(md_table(
            ["Stat", "Value"],
            [[k, fmt(v, 4)] for k, v in es.items()],
            ["l", "r"],
        ))
    w("")

    # Epsilon histogram (text-based bucket counts)
    epsilons = sorted(a["epsilon"] for a in analyses
                      if a["epsilon"] is not None)
    buckets = [
        ("0.00–0.09", lambda e: e < 0.10),
        ("0.10–0.14", lambda e: 0.10 <= e < 0.15),
        ("0.15–0.19", lambda e: 0.15 <= e < 0.20),
        ("0.20–0.24", lambda e: 0.20 <= e < 0.25),
        ("0.25–0.29", lambda e: 0.25 <= e < 0.30),
        ("0.30–0.35", lambda e: 0.30 <= e <= 0.35),
    ]
    bucket_rows = []
    for label, pred in buckets:
        cnt = sum(1 for e in epsilons if pred(e))
        bar = "█" * cnt
        bucket_rows.append([label, cnt, bar])
    w(md_table(["Range", "Count", ""], bucket_rows, ["l", "r", "l"]))
    w("")

    # 3.2 Domain distribution
    w("### 3.2 Domain Distribution\n")
    dom = pop_stats["domain_distribution"]
    dom_rows = [[d, c, f"{pct(c, n)}%"] for d, c in dom.items()]
    w(md_table(["Domain", "Count", "%"], dom_rows, ["l", "r", "r"]))
    w("")

    # 3.3 Perspective divergence
    w("### 3.3 Perspective Type Labels\n")
    w(f"- **Divergent** (>1 unique type across 4 perspectives): "
      f"**{dc['perspective_divergent']}** / {n}\n")
    w(f"- **Uniform** (all 4 perspectives agree): "
      f"**{n - dc['perspective_divergent']}** / {n}\n")
    w(f"- **Has tangled_rope or snare label**: "
      f"**{dc['has_tangled_or_snare_label']}** / {n}\n")

    # 3.4 Chi override overlap
    w("### 3.4 Chi Override Overlap\n")
    override_ids = [a["id"] for a in analyses if a["is_chi_override"]]
    if override_ids:
        w(f"**{dc['chi_override_overlap']}** of the {n} rope-dominant "
          f"constraints overlap with the 19 chi override set:\n")
        for oid in override_ids:
            w(f"- `{oid}`")
    else:
        w(f"**0** of the {n} rope-dominant constraints overlap with "
          f"the 19 chi override set.\n")
    w("")

    # ------------------------------------------------------------------
    # 4. Recommendations by Tier
    # ------------------------------------------------------------------
    w("## 4. Recommendations by Tier\n")

    for tier, tier_label in [("reclassify", "Reclassify"),
                              ("investigate", "Investigate"),
                              ("keep", "Keep")]:
        tier_analyses = [a for a in analyses if a["recommendation"] == tier]
        cnt = len(tier_analyses)
        w(f"### 4.{['reclassify', 'investigate', 'keep'].index(tier) + 1} "
          f"{tier_label} ({cnt} constraint{'s' if cnt != 1 else ''})\n")

        if not tier_analyses:
            w("*(none)*\n")
            continue

        if tier == "reclassify":
            w("These constraints have epsilon < 0.10.  Extraction potential "
              "is trivially low; rope-dominant is obviously correct.  "
              "Recommend reclassifying from tangled_rope to rope.\n")
        elif tier == "investigate":
            w("These constraints have moderate epsilon (0.10–0.19) or "
              "ambiguous perspective labels.  The tangled_rope label may or "
              "may not add signal.  Human review of the Prolog spec is "
              "recommended.\n")
        else:
            w("These constraints have epsilon >= 0.20 and at least one "
              "perspective labels them tangled_rope or snare.  The "
              "classification appears to be doing structural work, though "
              "qualitative confirmation from spec review is still valuable.\n")

        tier_rows = []
        for a in sorted(tier_analyses,
                        key=lambda x: x["epsilon"] or 0, reverse=True):
            tier_rows.append([
                f"`{a['id']}`",
                fmt(a["epsilon"], 2),
                a["topic_domain"] or "---",
                ", ".join(sorted(set(a["perspectives_raw"].values()))),
                a["recommendation_reason"],
            ])
        w(md_table(
            ["Constraint", "ε", "Domain", "Perspective Types", "Reason"],
            tier_rows,
            ["l", "r", "l", "l", "l"],
        ))
        w("")

    # ------------------------------------------------------------------
    # 5. Calibration Implications
    # ------------------------------------------------------------------
    w("## 5. Calibration Implications\n")

    reclassify_n = tiers.get("reclassify", 0)
    investigate_n = tiers.get("investigate", 0)
    keep_n = tiers.get("keep", 0)

    w(f"- **{reclassify_n}** constraints recommended for reclassification "
      f"to plain rope (epsilon < {RECLASSIFY_EPSILON_CEILING})\n")
    w(f"- **{investigate_n}** constraints need human review "
      f"(epsilon {RECLASSIFY_EPSILON_CEILING}–{KEEP_EPSILON_FLOOR})\n")
    w(f"- **{keep_n}** constraints appear correctly labeled "
      f"(epsilon >= {KEEP_EPSILON_FLOOR} with extraction signals)\n")
    w("")

    if reclassify_n > 0:
        w("**If reclassifying**: consider tightening the tangled_rope "
          "binary gate to require epsilon >= 0.10 before the tangled_rope "
          "label can apply.  This would prevent very-low-extraction "
          "coordination mechanisms from receiving the tangled_rope label.\n")

    w("**Structural note**: Since rope-dominance is entirely "
      "epsilon-driven (f(d) and scope_mod are perspective constants), "
      "the epsilon threshold is the only lever.  The question for the "
      "Investigate tier is whether the Prolog specs describe genuine "
      "rope-snare entanglement at moderate epsilon levels, or merely "
      "incidental extraction.\n")

    # ------------------------------------------------------------------
    # 6. Per-Constraint Detail (Appendix)
    # ------------------------------------------------------------------
    w("## 6. Per-Constraint Detail\n")
    w("Sorted by epsilon (descending).\n")

    sorted_analyses = sorted(analyses,
                             key=lambda x: x["epsilon"] or 0, reverse=True)

    for i, a in enumerate(sorted_analyses, 1):
        w(f"### 6.{i}. `{a['id']}` — **{a['recommendation'].upper()}**\n")
        w(f"**{a['human_readable']}** | Domain: {a['topic_domain'] or '---'}\n")

        # Core metrics
        w(md_table(
            ["Metric", "Value"],
            [
                ["epsilon (base_extractiveness)", fmt(a["epsilon"])],
                ["suppression", fmt(a["suppression"])],
                ["theater_ratio", fmt(a["theater_ratio"])],
                ["max gradient (g_chi)", fmt(a["max_gradient"])],
                ["max Chi", fmt(a["max_chi"])],
                ["claimed_type", a["claimed_type"] or "---"],
                ["chi override", "YES" if a["is_chi_override"] else "no"],
                ["perspective divergence",
                 "YES" if a["has_perspective_divergence"] else "no"],
                ["has tangled/snare label",
                 "YES" if a["has_tangled_or_snare"] else "no"],
            ],
            ["l", "r"],
        ))
        w("")

        # Perspective labels
        w("**Perspective type labels**: "
          + ", ".join(f"{pk}={a['perspectives_raw'].get(pk, '---')}"
                      for pk in PERSPECTIVE_KEYS)
          + "\n")

        # Per-perspective table
        pp_rows = []
        for pk in PERSPECTIVE_KEYS:
            pp = a["per_perspective"].get(pk, {})
            pp_rows.append([
                pk,
                fmt(pp.get("d")),
                fmt(pp.get("f_d"), 6),
                fmt(pp.get("scope_mod"), 1),
                fmt(pp.get("chi"), 6),
                fmt(pp.get("gradient"), 6),
            ])
        w(md_table(
            ["Perspective", "d", "f(d)", "scope", "Chi", "g_chi"],
            pp_rows,
            ["l", "r", "r", "r", "r", "r"],
        ))
        w("")

        # Diagnostics
        diag = a["diagnostics"]
        w(f"**Diagnostics**: "
          f"low-epsilon={'YES' if diag['low_epsilon_trivial'] else 'no'}, "
          f"sigmoid-compressed={'YES' if diag['sigmoid_compressed'] else 'no'}, "
          f"low-f(d)-spread={'YES' if diag['low_fd_spread'] else 'no'} "
          f"(spread={fmt(diag['fd_spread'], 4)})\n")

        # Cross-references
        cr = a.get("cross_references", {})
        if cr:
            w(f"**Cross-refs**: "
              f"tangled_psi={fmt(cr.get('tangled_psi'))}, "
              f"band={cr.get('tangled_band', '---')}, "
              f"signature={cr.get('signature', '---')}, "
              f"coalition={cr.get('coalition_type', '---')}\n")

        # Prolog spec excerpt
        spec = a.get("prolog_spec", {})
        if spec.get("file_found"):
            if spec.get("summary"):
                w(f"**Spec summary**: {spec['summary']}\n")
            if spec.get("key_agents"):
                w(f"**Key agents**: {spec['key_agents']}\n")
            if spec.get("beneficiaries"):
                w(f"**Beneficiaries**: "
                  + ", ".join(spec["beneficiaries"]) + "\n")
            if spec.get("victims"):
                w(f"**Victims**: "
                  + ", ".join(spec["victims"]) + "\n")
            if spec.get("omega_questions"):
                w("**Omega questions**:\n")
                for oq in spec["omega_questions"]:
                    w(f"- {oq}")
                w("")
        else:
            w("*Prolog spec file not found.*\n")

        w(f"**Recommendation**: {a['recommendation'].upper()} — "
          f"{a['recommendation_reason']}\n")
        w("---\n")

    # Write file
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

    chi_var_raw = load_json(CHI_VARIANCE_DATA_PATH,
                            "chi_variance_decomposition_data")
    if not chi_var_raw:
        print("ERROR: Could not load chi_variance_decomposition_data.json",
              file=sys.stderr)
        return 1

    tangled_gradient_raw = load_json(TANGLED_GRADIENT_PATH,
                                     "tangled_gradient_data")
    if not tangled_gradient_raw:
        print("ERROR: Could not load tangled_gradient_data.json",
              file=sys.stderr)
        return 1

    # Extract per-constraint lookups
    all_constraints = enriched.get("per_constraint", [])
    tangled_gradient = tangled_gradient_raw.get("per_constraint", {})
    chi_var_per = chi_var_raw.get("part1_variance_decomposition", {}).get(
        "per_constraint", {})

    # Build chi override set
    chi_overrides_list = chi_var_raw.get("chi_overrides", [])
    chi_override_ids = set(o["id"] for o in chi_overrides_list)

    # Identify 28 rope_dominant IDs
    rope_dominant_ids = set(
        cid for cid, v in tangled_gradient.items()
        if v.get("subtype") == "rope_dominant")

    # Filter enriched pipeline to those 28
    population = [c for c in all_constraints if c["id"] in rope_dominant_ids]

    print(f"Population: {len(population)} rope-dominant tangled_rope constraints")
    print(f"Chi overrides: {len(chi_override_ids)}")
    print(f"Rope-dominant IDs: {len(rope_dominant_ids)}")

    if not population:
        print("ERROR: No rope-dominant constraints found", file=sys.stderr)
        return 1

    # Parse Prolog specs for the 28
    print("\nParsing Prolog testset files...")
    prolog_specs = {}
    found = 0
    for cid in rope_dominant_ids:
        pl_path = TESTSET_DIR / f"{cid}.pl"
        spec = parse_prolog_spec(pl_path)
        prolog_specs[cid] = spec
        if spec["file_found"]:
            found += 1
    print(f"  Found: {found} / {len(rope_dominant_ids)} files")

    # Per-constraint analysis
    print("\nAnalyzing constraints...")
    analyses = []
    for c in population:
        cid = c["id"]
        chi_var_entry = chi_var_per.get(cid)
        gradient_entry = tangled_gradient.get(cid)
        spec = prolog_specs.get(cid, {})
        analysis = analyze_constraint(
            c, chi_var_entry, gradient_entry, chi_override_ids, spec)
        analyses.append(analysis)

    # Sort by epsilon descending for consistent ordering
    analyses.sort(key=lambda x: x["epsilon"] or 0, reverse=True)

    # Population analysis
    pop_stats = population_analysis(analyses)

    print(f"\n--- Results ---")
    print(f"  Low-epsilon trivial: {pop_stats['diagnostic_counts']['low_epsilon_trivial']}")
    print(f"  Sigmoid-compressed: {pop_stats['diagnostic_counts']['sigmoid_compressed']}")
    print(f"  Low f(d) spread: {pop_stats['diagnostic_counts']['low_fd_spread']}")
    print(f"  Chi override overlap: {pop_stats['diagnostic_counts']['chi_override_overlap']}")
    print(f"  Perspective divergent: {pop_stats['diagnostic_counts']['perspective_divergent']}")
    print(f"  Has tangled/snare label: {pop_stats['diagnostic_counts']['has_tangled_or_snare_label']}")
    print(f"  Tiers: {pop_stats['recommendation_tiers']}")

    # Assemble JSON data
    results = {
        "generated": datetime.now().isoformat(),
        "population_size": len(analyses),
        "population_definition":
            "subtype == 'rope_dominant' in tangled_gradient_data "
            "(max g_chi < 0.30 across all 4 perspectives)",
        "config": {
            "rope_chi_ceiling": ROPE_CHI_CEILING,
            "snare_chi_floor": SNARE_CHI_FLOOR,
            "subtype_rope_threshold": SUBTYPE_ROPE_THRESHOLD,
            "low_epsilon_threshold": LOW_EPSILON_THRESHOLD,
            "sigmoid_compression_threshold": SIGMOID_COMPRESSION_THRESHOLD,
            "fd_spread_threshold": FD_SPREAD_THRESHOLD,
            "reclassify_epsilon_ceiling": RECLASSIFY_EPSILON_CEILING,
            "keep_epsilon_floor": KEEP_EPSILON_FLOOR,
        },
        "summary": pop_stats,
        "per_constraint": {a["id"]: a for a in analyses},
    }

    # Write JSON
    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2)
    print(f"\nData written to {DATA_PATH}")

    # Write report
    write_report(analyses, pop_stats)

    return 0


if __name__ == "__main__":
    sys.exit(main())
