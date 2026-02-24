#!/usr/bin/env python3
"""Coordination Vitality Diagnostic

Analyzes whether coordination structures (scaffolds and pitons) serve
genuine functional purposes or merely persist as institutional inertia.
The binary gate has_coordination_function fires T for all 116 because
it only checks existence of constraint_beneficiary/2.  This diagnostic
distinguishes active coordination (beneficiaries benefit from the
constraint's function) from residual coordination (beneficiaries benefit
from the constraint's persistence).

Seven analysis steps:
  1. Beneficiary actor-type classification
  2. Victim actor-type classification
  3. Theater-extraction-coordination cross-reference (3 proxies)
  4. Scaffold-specific vitality assessment
  5. Piton degradation path assessment
  6. Scaffold lifecycle position
  7. Verdict and implications

Reads:  outputs/enriched_pipeline.json
        prolog/testsets/*.pl
Writes: docs/coordination_vitality_diagnostic.md
        outputs/coordination_vitality_diagnostic_data.json

Usage:  python3 python/coordination_vitality_diagnostic.py
"""

import json
import math
import re
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from shared.loader import load_json, ENRICHED_PIPELINE_JSON, OUTPUT_DIR, PROLOG_DIR
from shared.schemas import validate_enriched_pipeline

# ---------------------------------------------------------------------------
# Path constants
# ---------------------------------------------------------------------------

ROOT_DIR = Path(__file__).resolve().parent.parent
DOCS_DIR = ROOT_DIR / "docs"
TESTSET_DIR = PROLOG_DIR / "testsets"
REPORT_PATH = DOCS_DIR / "coordination_vitality_diagnostic.md"
DATA_PATH = OUTPUT_DIR / "coordination_vitality_diagnostic_data.json"

TARGET_TYPES = {"scaffold", "piton"}

# ---------------------------------------------------------------------------
# Prolog gate thresholds (from prolog/config.pl)
# ---------------------------------------------------------------------------

PITON_EXTRACTION_CEILING = 0.25
SCAFFOLD_EXTRACTION_CEIL = 0.30
PITON_THEATER_FLOOR = 0.70
SNARE_EPSILON_FLOOR = 0.46
TANGLED_ROPE_EPSILON_FLOOR = 0.30

# ---------------------------------------------------------------------------
# Actor-type classification keyword sets
#
# Applied to beneficiary/victim atom labels (snake_case identifiers).
# The atom is tokenized on underscores and scored by token overlap.
# ---------------------------------------------------------------------------

INSTITUTIONAL_TOKENS = {
    # State / government
    "state", "government", "governments", "agency", "agencies",
    "administration", "bureaucracy", "bureaucrat", "bureaucrats",
    "officials", "regulators", "regulatory", "authorities", "authority",
    "ministry", "department", "treasury", "judiciary", "judicial",
    "executive", "legislature", "parliament", "congressional",
    "federal", "national", "municipal", "policymakers",
    # Institutional / establishment
    "institution", "institutions", "institutional", "establishment",
    "incumbents", "legacy", "apparatus", "regime", "hegemony",
    "elite", "elites", "oligarchy", "dominance",
    "administrators", "formalists",
    # Corporate / organizational
    "corporation", "corporations", "corporate", "conglomerate",
    "management", "leadership", "board",
    "industry", "industrial", "manufacturer", "manufacturers",
    "contractor", "contractors", "firms", "labs",
    # Platform / infrastructure operators
    "platform", "platforms", "operators", "provider", "providers",
    # Military / security / enforcement
    "church", "military", "command", "intelligence", "security",
    "police", "enforcement", "colonial", "navy", "empire",
    # Specific role patterns
    "architects", "monopolist", "capture",
    "brands", "officers", "states", "aggregators", "algorithms",
    "adversary",
}

INDIVIDUAL_TOKENS = {
    # People / persons
    "individual", "individuals", "person", "persons", "citizen",
    "citizens", "civilian", "civilians", "populace", "public",
    "people", "residents", "resident",
    # Workers / role-specific people
    "workers", "worker", "laborers", "laborer", "employees",
    "taxpayers", "voters", "patients", "students", "applicants",
    "seekers", "borrowers", "tenants", "consumers", "users",
    "creators", "freelancers", "artisans", "homeowners",
    "savers", "graduates", "payers",
    # Vulnerable / low-power
    "low", "small", "independent", "marginalized",
    "disadvantaged", "vulnerable", "poor", "minority",
    "immigrant", "refugees", "displaced", "carers",
    "unpaid", "informal", "indebted", "unemployed",
    "excluded", "isolated", "youth", "juvenile",
    # Singular forms for corpus coverage
    "user", "graduate", "artist", "artists", "litigants",
    "operator", "speculators", "crews", "navigator", "navigators",
    "engineer",
    # Literary characters (Ulysses corpus artifacts)
    "bloom", "dedalus", "cinderella", "dignam",
}

COLLECTIVE_TOKENS = {
    # Communities / groups
    "community", "communities", "collective", "network",
    "movement", "coalition", "alliance", "federation",
    "union", "cooperative", "association", "society", "societies",
    # Professional collectives
    "researchers", "scientists", "engineers", "developers",
    "designers", "practitioners", "professionals", "experts",
    "scholars", "investors", "innovators",
    "entrepreneurs", "ranchers",
    # Sector / organized groups
    "sector", "participants", "factions", "mediators",
    "signatories", "influencers", "contributors",
    "insiders", "observers", "holders", "merchants",
    "orators",
    # Group labels
    "parties", "constituencies",
}

ABSTRACT_TOKENS = {
    # Outcomes / qualities
    "outcomes", "stability", "resilience", "continuity",
    "continuance", "affirmation", "determinism",
    "reality", "autonomy", "sovereignty", "infrastructure",
    "morale", "cohesion", "nuance", "transparency", "privacy",
    "viability", "margins",
    # Process / system concepts
    "consolidation", "optimization", "efficiency", "capacity",
    "coordination", "economy", "dogma", "narrative", "limitations",
    # Non-agent
    "future", "generations", "nature", "ecosystem",
    "climate", "environment",
}

ACTOR_TYPES = [
    "institutional_actor", "individual_actor",
    "collective_actor", "abstract_entity", "ambiguous",
]

# ---------------------------------------------------------------------------
# Helpers (same pattern as scaffold_piton_gate_audit.py)
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


def compute_gates(r):
    """Return 4-tuple of boolean gates for a record."""
    en = bool(r.get("emerges_naturally"))
    rae = bool(r.get("requires_active_enforcement"))
    hcf = len(r.get("beneficiaries") or []) > 0
    hae = len(r.get("victims") or []) > 0
    return (en, rae, hcf, hae)


# ---------------------------------------------------------------------------
# Actor-type classification
# ---------------------------------------------------------------------------


def classify_actor_type(atom):
    """Classify a beneficiary/victim atom label into actor type.

    Returns (actor_type, matched_tokens) where actor_type is one of:
    institutional_actor, individual_actor, collective_actor,
    abstract_entity, ambiguous.
    """
    # Normalize: handle bracket-list atoms, spaces, commas
    clean = atom.strip("[]").replace(",", "_").replace(" ", "_").replace("-", "_")
    tokens = set(clean.lower().split("_"))
    tokens.discard("")

    scores = {
        "institutional_actor": 0,
        "individual_actor": 0,
        "collective_actor": 0,
        "abstract_entity": 0,
    }
    matched = {k: [] for k in scores}

    for tok in tokens:
        for cat, kwset in [
            ("institutional_actor", INSTITUTIONAL_TOKENS),
            ("individual_actor", INDIVIDUAL_TOKENS),
            ("collective_actor", COLLECTIVE_TOKENS),
            ("abstract_entity", ABSTRACT_TOKENS),
        ]:
            if tok in kwset:
                scores[cat] += 1
                matched[cat].append(tok)

    max_score = max(scores.values())
    if max_score == 0:
        return ("ambiguous", [])

    # Priority tie-breaking: institutional > individual > collective > abstract
    for priority in ["institutional_actor", "individual_actor",
                     "collective_actor", "abstract_entity"]:
        if scores[priority] == max_score:
            return (priority, matched[priority])
    return ("ambiguous", [])


def classify_atoms(atoms):
    """Classify a list of atoms, return per-atom results and summary."""
    results = []
    for atom in atoms:
        atype, matched = classify_actor_type(atom)
        results.append({"atom": atom, "actor_type": atype, "matched_tokens": matched})
    type_counts = Counter(r["actor_type"] for r in results)
    return {"atoms": results, "type_counts": dict(type_counts)}


def dominant_type(classification):
    """Return the most common actor type from a classification, or 'ambiguous'."""
    counts = classification.get("type_counts", {})
    if not counts:
        return "ambiguous"
    # Exclude ambiguous from dominant calculation unless it's all we have
    non_ambig = {k: v for k, v in counts.items() if k != "ambiguous"}
    if non_ambig:
        return max(non_ambig, key=non_ambig.get)
    return "ambiguous"


# ---------------------------------------------------------------------------
# Prolog file parsing
# ---------------------------------------------------------------------------

RE_BENEFICIARY = re.compile(
    r"^[^%]*constraint_beneficiary\(\s*(\w+)\s*,\s*(\w+)\s*\)"
)
RE_VICTIM = re.compile(
    r"^[^%]*constraint_victim\(\s*(\w+)\s*,\s*(\w+)\s*\)"
)
RE_COORDINATION_TYPE = re.compile(
    r"^[^%]*coordination_type\(\s*(\w+)\s*,\s*(\w+)\s*\)"
)
RE_SUNSET = re.compile(
    r"^[^%]*has_sunset_clause\(\s*(\w+)\s*\)"
)
RE_RAE = re.compile(
    r"^[^%]*requires_active_enforcement\(\s*(\w+)\s*\)"
)


def parse_prolog_testsets(testset_dir):
    """Parse structural declarations from all .pl testset files.

    Returns dict with:
        beneficiaries: {constraint_id: [atoms]}
        victims: {constraint_id: [atoms]}
        coordination_types: {constraint_id: type_atom}
        sunset_clauses: set of constraint_ids
        rae_flags: set of constraint_ids
        files_parsed: int
    """
    beneficiaries = defaultdict(list)
    victims = defaultdict(list)
    coordination_types = {}
    sunset_clauses = set()
    rae_flags = set()
    files_parsed = 0

    pl_files = sorted(testset_dir.glob("*.pl"))
    for pl_file in pl_files:
        try:
            content = pl_file.read_text(encoding="utf-8", errors="replace")
        except Exception:
            continue
        files_parsed += 1
        for line in content.splitlines():
            m = RE_BENEFICIARY.search(line)
            if m:
                cid, atom = m.group(1), m.group(2)
                if atom not in beneficiaries[cid]:
                    beneficiaries[cid].append(atom)
            m = RE_VICTIM.search(line)
            if m:
                cid, atom = m.group(1), m.group(2)
                if atom not in victims[cid]:
                    victims[cid].append(atom)
            m = RE_COORDINATION_TYPE.search(line)
            if m:
                coordination_types[m.group(1)] = m.group(2)
            m = RE_SUNSET.search(line)
            if m:
                sunset_clauses.add(m.group(1))
            m = RE_RAE.search(line)
            if m:
                rae_flags.add(m.group(1))

    return {
        "beneficiaries": dict(beneficiaries),
        "victims": dict(victims),
        "coordination_types": coordination_types,
        "sunset_clauses": sunset_clauses,
        "rae_flags": rae_flags,
        "files_parsed": files_parsed,
    }


# ---------------------------------------------------------------------------
# Gate trace simulation (simplified from scaffold_piton_gate_audit.py)
# ---------------------------------------------------------------------------


def simulate_gate(r):
    """Simulate Prolog gate priority chain. Returns intercepting gate or claimed_type."""
    eps = r.get("base_extractiveness", 0)
    sigma = r.get("suppression", 0)
    tau = r.get("theater_ratio", 0)
    en, rae, hcf, hae = compute_gates(r)

    # Mountain
    if sigma <= 0.05 and eps <= 0.25 and en:
        return "mountain"
    # Snare
    if eps >= SNARE_EPSILON_FLOOR and sigma >= 0.60 and eps >= 0.66:
        return "snare"
    # Tangled rope
    if (eps >= TANGLED_ROPE_EPSILON_FLOOR and eps <= 0.90
            and sigma >= 0.40 and rae and hcf and hae):
        return "tangled_rope"
    # Scaffold
    if eps <= SCAFFOLD_EXTRACTION_CEIL and hcf:
        return "scaffold"
    # Rope
    if eps <= 0.45 and eps <= 0.35:
        return "rope"
    # Piton
    if eps >= 0.10 and eps <= PITON_EXTRACTION_CEILING and tau >= PITON_THEATER_FLOOR:
        return "piton"
    # Fallback: highest-priority gate that would fire
    return "unknown"


def is_intercepted(r):
    """Check if a constraint is intercepted by a higher-priority gate."""
    claimed = r.get("claimed_type")
    result = simulate_gate(r)
    return result != claimed


# ---------------------------------------------------------------------------
# Step 1: Beneficiary Text Analysis
# ---------------------------------------------------------------------------


def step1_beneficiary_analysis(pop, prolog_meta):
    """Classify beneficiary atoms by actor type for all target constraints."""
    per_constraint = {}
    all_atom_types = []

    for r in pop:
        cid = r["id"]
        bens = r.get("beneficiaries") or []
        # Cross-reference with prolog parse
        prolog_bens = prolog_meta["beneficiaries"].get(cid, [])
        # Use enriched pipeline as primary, note prolog coverage
        classification = classify_atoms(bens)
        dom = dominant_type(classification)
        per_constraint[cid] = {
            "beneficiaries": bens,
            "prolog_beneficiaries": prolog_bens,
            "classification": classification,
            "dominant_type": dom,
            "prolog_coverage": len(prolog_bens) > 0,
        }
        all_atom_types.extend(a["actor_type"] for a in classification["atoms"])

    # Population statistics
    total_atoms = len(all_atom_types)
    type_dist = dict(Counter(all_atom_types).most_common())

    # By claimed type
    by_claimed = {}
    for ctype in TARGET_TYPES:
        group = [r for r in pop if r.get("claimed_type") == ctype]
        group_types = []
        for r in group:
            cls = per_constraint[r["id"]]["classification"]
            group_types.extend(a["actor_type"] for a in cls["atoms"])
        by_claimed[ctype] = dict(Counter(group_types).most_common())

    # Dominant type distribution by claimed type
    dom_by_claimed = {}
    for ctype in TARGET_TYPES:
        group = [r for r in pop if r.get("claimed_type") == ctype]
        doms = [per_constraint[r["id"]]["dominant_type"] for r in group]
        dom_by_claimed[ctype] = dict(Counter(doms).most_common())

    # Intercepted vs passing
    intercepted_doms = []
    passing_doms = []
    for r in pop:
        dom = per_constraint[r["id"]]["dominant_type"]
        if is_intercepted(r):
            intercepted_doms.append(dom)
        else:
            passing_doms.append(dom)

    # Ambiguous atoms for review
    ambiguous_atoms = []
    for cid, data in per_constraint.items():
        for a in data["classification"]["atoms"]:
            if a["actor_type"] == "ambiguous":
                ambiguous_atoms.append({"constraint": cid, "atom": a["atom"]})

    # Prolog coverage stats
    prolog_covered = sum(1 for d in per_constraint.values() if d["prolog_coverage"])

    return {
        "per_constraint": per_constraint,
        "population_stats": {
            "total_atoms": total_atoms,
            "type_distribution": type_dist,
            "by_claimed_type": by_claimed,
            "dominant_by_claimed_type": dom_by_claimed,
            "intercepted_dominant_dist": dict(Counter(intercepted_doms).most_common()),
            "passing_dominant_dist": dict(Counter(passing_doms).most_common()),
            "intercepted_count": len(intercepted_doms),
            "passing_count": len(passing_doms),
        },
        "ambiguous_atoms": ambiguous_atoms,
        "prolog_coverage": {
            "covered": prolog_covered,
            "total": len(per_constraint),
            "pct": pct(prolog_covered, len(per_constraint)),
        },
    }


# ---------------------------------------------------------------------------
# Step 2: Victim Text Analysis
# ---------------------------------------------------------------------------


def step2_victim_analysis(pop, prolog_meta):
    """Classify victim atoms by actor type for all target constraints."""
    per_constraint = {}
    all_atom_types = []

    for r in pop:
        cid = r["id"]
        vics = r.get("victims") or []
        prolog_vics = prolog_meta["victims"].get(cid, [])
        classification = classify_atoms(vics)
        dom = dominant_type(classification)
        per_constraint[cid] = {
            "victims": vics,
            "prolog_victims": prolog_vics,
            "classification": classification,
            "dominant_type": dom,
            "prolog_coverage": len(prolog_vics) > 0,
        }
        all_atom_types.extend(a["actor_type"] for a in classification["atoms"])

    total_atoms = len(all_atom_types)
    type_dist = dict(Counter(all_atom_types).most_common())

    by_claimed = {}
    for ctype in TARGET_TYPES:
        group = [r for r in pop if r.get("claimed_type") == ctype]
        group_types = []
        for r in group:
            cls = per_constraint[r["id"]]["classification"]
            group_types.extend(a["actor_type"] for a in cls["atoms"])
        by_claimed[ctype] = dict(Counter(group_types).most_common())

    dom_by_claimed = {}
    for ctype in TARGET_TYPES:
        group = [r for r in pop if r.get("claimed_type") == ctype]
        doms = [per_constraint[r["id"]]["dominant_type"] for r in group]
        dom_by_claimed[ctype] = dict(Counter(doms).most_common())

    ambiguous_atoms = []
    for cid, data in per_constraint.items():
        for a in data["classification"]["atoms"]:
            if a["actor_type"] == "ambiguous":
                ambiguous_atoms.append({"constraint": cid, "atom": a["atom"]})

    prolog_covered = sum(1 for d in per_constraint.values() if d["prolog_coverage"])

    return {
        "per_constraint": per_constraint,
        "population_stats": {
            "total_atoms": total_atoms,
            "type_distribution": type_dist,
            "by_claimed_type": by_claimed,
            "dominant_by_claimed_type": dom_by_claimed,
        },
        "ambiguous_atoms": ambiguous_atoms,
        "prolog_coverage": {
            "covered": prolog_covered,
            "total": len(per_constraint),
            "pct": pct(prolog_covered, len(per_constraint)),
        },
    }


# ---------------------------------------------------------------------------
# Step 3: Theater-Extraction-Coordination Cross-Reference
# ---------------------------------------------------------------------------


def step3_cross_reference(pop, s1, s2, prolog_meta):
    """Cross-reference actor types with metric profiles via three proxies."""

    # --- Proxy 1: Beneficiary-Victim Asymmetry Score ---
    asymmetry_scores = {}
    for r in pop:
        cid = r["id"]
        ben_cls = s1["per_constraint"].get(cid, {}).get("classification", {})
        vic_cls = s2["per_constraint"].get(cid, {}).get("classification", {})
        ben_types = ben_cls.get("type_counts", {})
        vic_types = vic_cls.get("type_counts", {})
        ben_total = sum(ben_types.values()) or 1
        vic_total = sum(vic_types.values()) or 1
        ben_inst_frac = ben_types.get("institutional_actor", 0) / ben_total
        vic_inst_frac = vic_types.get("institutional_actor", 0) / vic_total
        asymmetry = round(ben_inst_frac - vic_inst_frac, 4)
        asymmetry_scores[cid] = {
            "asymmetry": asymmetry,
            "ben_inst_frac": round(ben_inst_frac, 4),
            "vic_inst_frac": round(vic_inst_frac, 4),
        }

    # Asymmetry by claimed type
    asym_by_type = {}
    for ctype in TARGET_TYPES:
        group = [r for r in pop if r.get("claimed_type") == ctype]
        vals = [asymmetry_scores[r["id"]]["asymmetry"] for r in group]
        asym_by_type[ctype] = desc_stats(vals)

    # Persistence vs functional pattern counts
    persistence_count = sum(1 for v in asymmetry_scores.values() if v["asymmetry"] > 0)
    functional_count = sum(1 for v in asymmetry_scores.values() if v["asymmetry"] < 0)
    neutral_count = sum(1 for v in asymmetry_scores.values() if v["asymmetry"] == 0)

    proxy1 = {
        "by_claimed_type": asym_by_type,
        "persistence_pattern_count": persistence_count,
        "functional_pattern_count": functional_count,
        "neutral_count": neutral_count,
        "total": len(asymmetry_scores),
    }

    # --- Proxy 2: TR x Epsilon x Beneficiary-Type ---
    cells = {
        "theatrical_persistence": [],   # high-TR + institutional
        "theatrical_function": [],      # high-TR + non-institutional
        "active_persistence": [],       # low-TR + institutional
        "active_function": [],          # low-TR + non-institutional
    }
    for r in pop:
        cid = r["id"]
        tau = r.get("theater_ratio", 0)
        dom = s1["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        high_tr = tau >= PITON_THEATER_FLOOR
        institutional = dom == "institutional_actor"
        if high_tr and institutional:
            cells["theatrical_persistence"].append(r)
        elif high_tr and not institutional:
            cells["theatrical_function"].append(r)
        elif not high_tr and institutional:
            cells["active_persistence"].append(r)
        else:
            cells["active_function"].append(r)

    proxy2 = {}
    for cell_name, group in cells.items():
        eps_vals = [r.get("base_extractiveness", 0) for r in group]
        proxy2[cell_name] = {
            "count": len(group),
            "pct": pct(len(group), len(pop)),
            "epsilon_stats": desc_stats(eps_vals),
            "ids": [r["id"] for r in group[:5]],  # sample
        }

    # --- Proxy 3: RAE x Beneficiary-Type ---
    rae_cells = {
        "enforced_persistence": [],    # RAE=T + institutional
        "enforced_function": [],       # RAE=T + non-institutional
        "emergent_persistence": [],    # RAE=F + institutional
        "emergent_function": [],       # RAE=F + non-institutional
    }
    for r in pop:
        cid = r["id"]
        rae = bool(r.get("requires_active_enforcement"))
        dom = s1["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        institutional = dom == "institutional_actor"
        if rae and institutional:
            rae_cells["enforced_persistence"].append(r)
        elif rae and not institutional:
            rae_cells["enforced_function"].append(r)
        elif not rae and institutional:
            rae_cells["emergent_persistence"].append(r)
        else:
            rae_cells["emergent_function"].append(r)

    proxy3 = {}
    for cell_name, group in rae_cells.items():
        eps_vals = [r.get("base_extractiveness", 0) for r in group]
        proxy3[cell_name] = {
            "count": len(group),
            "pct": pct(len(group), len(pop)),
            "epsilon_stats": desc_stats(eps_vals),
        }

    return {
        "proxy1_asymmetry": proxy1,
        "proxy2_tr_epsilon": proxy2,
        "proxy3_rae_beneficiary": proxy3,
        "per_constraint_asymmetry": asymmetry_scores,
    }


# ---------------------------------------------------------------------------
# Step 4: Scaffold-Specific Assessment
# ---------------------------------------------------------------------------


def step4_scaffold_assessment(pop, s1, s2, prolog_meta):
    """Assess calcification risk for each scaffold."""
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]
    per_scaffold = []

    for r in scaffolds:
        cid = r["id"]
        eps = r.get("base_extractiveness", 0)
        dom_ben = s1["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        dom_vic = s2["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        has_sunset = cid in prolog_meta["sunset_clauses"]
        coord_type = prolog_meta["coordination_types"].get(cid)
        rae = bool(r.get("requires_active_enforcement"))

        # Calcification risk scoring
        risk_score = 0
        signals = []

        # Institutional beneficiary → +1 risk
        if dom_ben == "institutional_actor":
            risk_score += 1
            signals.append("institutional_beneficiary")

        # No sunset clause → +1 risk
        if not has_sunset:
            risk_score += 1
            signals.append("no_sunset_clause")

        # enforcement_mechanism coordination → +1 risk
        if coord_type == "enforcement_mechanism":
            risk_score += 1
            signals.append("enforcement_mechanism_coord")

        # Epsilon exceeds scaffold ceiling → +1 risk
        if eps > SCAFFOLD_EXTRACTION_CEIL:
            risk_score += 1
            signals.append("epsilon_exceeds_ceiling")

        # Risk band
        if risk_score >= 3:
            risk_band = "high"
        elif risk_score == 2:
            risk_band = "medium"
        else:
            risk_band = "low"

        per_scaffold.append({
            "id": cid,
            "human_readable": r.get("human_readable", ""),
            "epsilon": eps,
            "suppression": r.get("suppression", 0),
            "theater_ratio": r.get("theater_ratio", 0),
            "dominant_beneficiary_type": dom_ben,
            "dominant_victim_type": dom_vic,
            "has_sunset_clause": has_sunset,
            "coordination_type": coord_type,
            "requires_active_enforcement": rae,
            "intercepted": is_intercepted(r),
            "risk_score": risk_score,
            "risk_band": risk_band,
            "risk_signals": signals,
        })

    risk_dist = Counter(s["risk_band"] for s in per_scaffold)
    return {
        "per_scaffold": per_scaffold,
        "summary": {
            "total": len(per_scaffold),
            "high_risk": risk_dist.get("high", 0),
            "medium_risk": risk_dist.get("medium", 0),
            "low_risk": risk_dist.get("low", 0),
            "intercepted_count": sum(1 for s in per_scaffold if s["intercepted"]),
            "sunset_clause_count": sum(1 for s in per_scaffold if s["has_sunset_clause"]),
            "coord_type_coverage": sum(
                1 for s in per_scaffold if s["coordination_type"] is not None
            ),
        },
    }


# ---------------------------------------------------------------------------
# Step 5: Piton Degradation Path Assessment
# ---------------------------------------------------------------------------


def step5_degradation_assessment(pop, s1, s2, prolog_meta):
    """Classify pitons as terminal, degrading, or transitional."""
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]
    per_piton = []

    for r in pitons:
        cid = r["id"]
        eps = r.get("base_extractiveness", 0)
        tau = r.get("theater_ratio", 0)
        dom_ben = s1["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        dom_vic = s2["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        rae = bool(r.get("requires_active_enforcement"))

        # Terminal signals (dead coordination, persistence only)
        terminal_score = 0
        signals = []

        if dom_ben == "institutional_actor":
            terminal_score += 1
            signals.append("institutional_beneficiary")

        if dom_vic == "individual_actor":
            terminal_score += 1
            signals.append("individual_victim")

        if tau >= PITON_THEATER_FLOOR:
            terminal_score += 1
            signals.append("high_theater_ratio")

        if rae:
            terminal_score += 1
            signals.append("requires_active_enforcement")

        # Classification
        if terminal_score >= 3:
            classification = "terminal"
        elif terminal_score == 2:
            classification = "degrading"
        else:
            classification = "transitional"

        per_piton.append({
            "id": cid,
            "human_readable": r.get("human_readable", ""),
            "epsilon": eps,
            "suppression": r.get("suppression", 0),
            "theater_ratio": tau,
            "dominant_beneficiary_type": dom_ben,
            "dominant_victim_type": dom_vic,
            "requires_active_enforcement": rae,
            "intercepted": is_intercepted(r),
            "terminal_score": terminal_score,
            "classification": classification,
            "signals": signals,
        })

    class_dist = Counter(p["classification"] for p in per_piton)

    # Signal frequency
    signal_freq = Counter()
    for p in per_piton:
        for sig in p["signals"]:
            signal_freq[sig] += 1

    # Metric stats by classification
    metric_stats = {}
    for cls_name in ["terminal", "degrading", "transitional"]:
        group = [p for p in per_piton if p["classification"] == cls_name]
        if group:
            metric_stats[cls_name] = {
                "count": len(group),
                "epsilon": desc_stats([p["epsilon"] for p in group]),
                "suppression": desc_stats([p["suppression"] for p in group]),
                "theater_ratio": desc_stats([p["theater_ratio"] for p in group]),
                "intercepted_count": sum(1 for p in group if p["intercepted"]),
            }

    # Intercepted vs passing by classification
    intercepted_class = Counter(
        p["classification"] for p in per_piton if p["intercepted"]
    )
    passing_class = Counter(
        p["classification"] for p in per_piton if not p["intercepted"]
    )

    return {
        "per_piton": per_piton,
        "summary": {
            "total": len(per_piton),
            "terminal": class_dist.get("terminal", 0),
            "degrading": class_dist.get("degrading", 0),
            "transitional": class_dist.get("transitional", 0),
        },
        "signal_frequency": dict(signal_freq.most_common()),
        "metric_stats_by_classification": metric_stats,
        "intercepted_by_classification": dict(intercepted_class),
        "passing_by_classification": dict(passing_class),
    }


# ---------------------------------------------------------------------------
# Step 6: Scaffold Lifecycle Position
# ---------------------------------------------------------------------------


def step6_scaffold_lifecycle(pop, s1, prolog_meta, s4):
    """Classify scaffolds as genuinely temporary, calcified, or ambiguous."""
    per_scaffold = []

    for scaffold in s4["per_scaffold"]:
        cid = scaffold["id"]
        eps = scaffold["epsilon"]
        dom_ben = scaffold["dominant_beneficiary_type"]
        has_sunset = scaffold["has_sunset_clause"]
        coord_type = scaffold["coordination_type"]

        # Temporary signals
        temp_score = 0
        temp_signals = []
        if has_sunset:
            temp_score += 1
            temp_signals.append("has_sunset_clause")
        if dom_ben != "institutional_actor" and dom_ben != "ambiguous":
            temp_score += 1
            temp_signals.append("non_institutional_beneficiary")
        if eps <= SCAFFOLD_EXTRACTION_CEIL:
            temp_score += 1
            temp_signals.append("epsilon_within_ceiling")
        if coord_type in ("resource_allocation", "global_infrastructure"):
            temp_score += 1
            temp_signals.append(f"coord_type_{coord_type}")

        # Calcified signals
        calc_score = 0
        calc_signals = []
        if not has_sunset:
            calc_score += 1
            calc_signals.append("no_sunset_clause")
        if dom_ben == "institutional_actor":
            calc_score += 1
            calc_signals.append("institutional_beneficiary")
        if eps > SCAFFOLD_EXTRACTION_CEIL:
            calc_score += 1
            calc_signals.append("epsilon_exceeds_ceiling")
        if coord_type == "enforcement_mechanism":
            calc_score += 1
            calc_signals.append("enforcement_mechanism_coord")

        # Classification
        if temp_score >= 3 and temp_score > calc_score:
            lifecycle = "genuinely_temporary"
        elif calc_score >= 3 and calc_score > temp_score:
            lifecycle = "calcified"
        elif calc_score > temp_score:
            lifecycle = "calcifying"
        elif temp_score > calc_score:
            lifecycle = "likely_temporary"
        else:
            lifecycle = "ambiguous_lifecycle"

        per_scaffold.append({
            "id": cid,
            "human_readable": scaffold["human_readable"],
            "epsilon": eps,
            "lifecycle": lifecycle,
            "temporary_score": temp_score,
            "calcified_score": calc_score,
            "temporary_signals": temp_signals,
            "calcified_signals": calc_signals,
            "intercepted": scaffold["intercepted"],
        })

    lifecycle_dist = Counter(s["lifecycle"] for s in per_scaffold)

    return {
        "per_scaffold": per_scaffold,
        "summary": {
            "total": len(per_scaffold),
            "genuinely_temporary": lifecycle_dist.get("genuinely_temporary", 0),
            "likely_temporary": lifecycle_dist.get("likely_temporary", 0),
            "ambiguous_lifecycle": lifecycle_dist.get("ambiguous_lifecycle", 0),
            "calcifying": lifecycle_dist.get("calcifying", 0),
            "calcified": lifecycle_dist.get("calcified", 0),
        },
    }


# ---------------------------------------------------------------------------
# Step 7: Verdict and Implications
# ---------------------------------------------------------------------------


def step7_verdict(pop, s1, s2, s3, s4, s5, s6):
    """Aggregate findings and answer the core diagnostic questions."""
    pitons = [r for r in pop if r.get("claimed_type") == "piton"]
    scaffolds = [r for r in pop if r.get("claimed_type") == "scaffold"]

    # Piton verdict
    total_pitons = len(pitons)
    intercepted_pitons = sum(1 for r in pitons if is_intercepted(r))
    passing_pitons = total_pitons - intercepted_pitons

    terminal_count = s5["summary"]["terminal"]
    degrading_count = s5["summary"]["degrading"]
    transitional_count = s5["summary"]["transitional"]
    dead_coordination = terminal_count + degrading_count
    active_coordination = transitional_count

    piton_dead_frac = dead_coordination / max(total_pitons, 1)
    piton_active_frac = active_coordination / max(total_pitons, 1)

    # Scaffold verdict
    total_scaffolds = len(scaffolds)
    intercepted_scaffolds = sum(1 for r in scaffolds if is_intercepted(r))

    genuinely_temp = (s6["summary"].get("genuinely_temporary", 0)
                      + s6["summary"].get("likely_temporary", 0))
    calcified_total = (s6["summary"].get("calcified", 0)
                       + s6["summary"].get("calcifying", 0))

    scaffold_temp_frac = genuinely_temp / max(total_scaffolds, 1)
    scaffold_calc_frac = calcified_total / max(total_scaffolds, 1)

    # Cross-type: does beneficiary type predict gate outcome?
    ben_dom_by_intercept = {"intercepted": Counter(), "passing": Counter()}
    for r in pop:
        cid = r["id"]
        dom = s1["per_constraint"].get(cid, {}).get("dominant_type", "ambiguous")
        bucket = "intercepted" if is_intercepted(r) else "passing"
        ben_dom_by_intercept[bucket][dom] += 1

    # Asymmetry correlation proxy: does asymmetry differ for intercepted vs passing?
    asym_data = s3.get("per_constraint_asymmetry", {})
    intercepted_asym = [
        asym_data[r["id"]]["asymmetry"] for r in pop
        if is_intercepted(r) and r["id"] in asym_data
    ]
    passing_asym = [
        asym_data[r["id"]]["asymmetry"] for r in pop
        if not is_intercepted(r) and r["id"] in asym_data
    ]

    # Key findings
    findings = []
    if piton_dead_frac > 0.5:
        findings.append(
            f"{pct(dead_coordination, total_pitons)}% of pitons show dead/degrading "
            f"coordination (terminal+degrading). The ontology may need a wider piton "
            f"definition or an 'extractive piton' subtype."
        )
    else:
        findings.append(
            f"Only {pct(dead_coordination, total_pitons)}% of pitons show dead/degrading "
            f"coordination. {pct(active_coordination, total_pitons)}% show transitional "
            f"signals — the LLM may be over-calling piton where tangled_rope is correct."
        )

    if scaffold_calc_frac > 0.5:
        findings.append(
            f"{pct(calcified_total, total_scaffolds)}% of scaffolds show calcification "
            f"signals. The LLM may be labeling tangled_ropes as scaffolds."
        )
    else:
        findings.append(
            f"{pct(genuinely_temp, total_scaffolds)}% of scaffolds appear genuinely "
            f"temporary. The scaffold extraction ceiling may be too low for legitimate "
            f"transition costs."
        )

    # Proxy 2 breakdown
    p2 = s3["proxy2_tr_epsilon"]
    theatrical_persistence_n = p2["theatrical_persistence"]["count"]
    theatrical_function_n = p2["theatrical_function"]["count"]
    if theatrical_persistence_n > theatrical_function_n:
        findings.append(
            f"Proxy 2 (TR×beneficiary-type): {theatrical_persistence_n} constraints show "
            f"'theatrical persistence' (high-TR + institutional beneficiary) vs "
            f"{theatrical_function_n} showing 'theatrical function'. The theatrical "
            f"persistence pattern dominates."
        )
    else:
        findings.append(
            f"Proxy 2 (TR×beneficiary-type): {theatrical_function_n} constraints show "
            f"'theatrical function' (high-TR + non-institutional beneficiary) vs "
            f"{theatrical_persistence_n} showing 'theatrical persistence'. Active "
            f"coordination masked by theater is the more common pattern."
        )

    # Recommendations
    recommendations = []
    if piton_active_frac > 0.3:
        recommendations.append(
            "Review transitional pitons as tangled_rope candidates — they may have "
            "active coordination that the piton label misses."
        )
    if piton_dead_frac > 0.5:
        recommendations.append(
            "Consider an 'extractive piton' subtype for high-epsilon pitons with "
            "dead coordination — these differ from classic low-extraction pitons."
        )
    if scaffold_calc_frac > 0.3:
        recommendations.append(
            "Audit calcified scaffolds for reclassification as tangled_rope — "
            "they have outlived their temporary purpose."
        )
    recommendations.append(
        "Consider adding 'coordination vitality' as a formal axis in the taxonomy. "
        "The binary has_coordination_function gate misses the functional/persistence "
        "distinction that this diagnostic reveals."
    )

    return {
        "piton_verdict": {
            "total": total_pitons,
            "intercepted": intercepted_pitons,
            "passing": passing_pitons,
            "terminal": terminal_count,
            "degrading": degrading_count,
            "transitional": transitional_count,
            "dead_coordination_frac": round(piton_dead_frac, 4),
            "active_coordination_frac": round(piton_active_frac, 4),
        },
        "scaffold_verdict": {
            "total": total_scaffolds,
            "intercepted": intercepted_scaffolds,
            "genuinely_temporary": genuinely_temp,
            "calcified": calcified_total,
            "temporary_frac": round(scaffold_temp_frac, 4),
            "calcification_frac": round(scaffold_calc_frac, 4),
        },
        "cross_type": {
            "beneficiary_type_by_interception": {
                k: dict(v) for k, v in ben_dom_by_intercept.items()
            },
            "intercepted_asymmetry_stats": desc_stats(intercepted_asym),
            "passing_asymmetry_stats": desc_stats(passing_asym),
        },
        "key_findings": findings,
        "recommendations": recommendations,
    }


# ---------------------------------------------------------------------------
# Report Generation
# ---------------------------------------------------------------------------


def generate_report(s1, s2, s3, s4, s5, s6, s7, prolog_meta, timestamp):
    """Build the complete markdown diagnostic report."""
    L = []

    def section(title, level=2):
        L.append("")
        L.append(f"{'#' * level} {title}")
        L.append("")

    def para(text):
        L.append(text)
        L.append("")

    # ---- Title ----
    L.append("# Coordination Vitality Diagnostic")
    L.append("")
    L.append(f"*Generated {timestamp} by "
             f"`python/coordination_vitality_diagnostic.py`*")

    # ---- Executive Summary ----
    section("Executive Summary")
    pv = s7["piton_verdict"]
    sv = s7["scaffold_verdict"]
    para(
        f"This diagnostic examines whether the coordination function in "
        f"**{pv['total']} pitons** and **{sv['total']} scaffolds** is "
        f"genuinely active or merely residual. The binary gate "
        f"`has_coordination_function` fires T for all {pv['total'] + sv['total']} "
        f"because it only checks `constraint_beneficiary(C, _)` existence. "
        f"This diagnostic distinguishes active coordination (beneficiaries "
        f"benefit from the constraint's function) from residual coordination "
        f"(beneficiaries benefit from the constraint's persistence)."
    )

    L.append("Key findings:")
    L.append("")
    for finding in s7["key_findings"]:
        L.append(f"- {finding}")
    L.append("")

    L.append("Recommendations:")
    L.append("")
    for rec in s7["recommendations"]:
        L.append(f"- {rec}")
    L.append("")

    # ---- Heuristic Note ----
    section("Methodological Note")
    para(
        "Beneficiary/victim atoms are structural role labels (e.g., "
        "`executive_state_body`, `medical_applicants`), not descriptive text. "
        "Classification uses keyword matching on underscore-tokenized atoms "
        "against four actor-type categories: `institutional_actor`, "
        "`individual_actor`, `collective_actor`, `abstract_entity`. "
        "Unmatched atoms are labeled `ambiguous`. This is a heuristic — "
        "it classifies WHO benefits, not HOW they benefit. The cross-reference "
        "with metric profiles (Step 3) provides the complementary signal."
    )

    # ---- Section 1: Beneficiary Analysis ----
    section("1. Beneficiary Actor-Type Analysis")

    stats = s1["population_stats"]
    section("1.1 Population Distribution", 3)
    para(
        f"Total beneficiary atoms classified: **{stats['total_atoms']}** "
        f"across {pv['total'] + sv['total']} constraints."
    )
    rows = [
        (atype, stats["type_distribution"].get(atype, 0),
         f"{pct(stats['type_distribution'].get(atype, 0), stats['total_atoms'])}%")
        for atype in ACTOR_TYPES
    ]
    L.append(md_table(["Actor Type", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")
    para(
        f"Prolog .pl file cross-reference coverage: "
        f"{s1['prolog_coverage']['covered']}/{s1['prolog_coverage']['total']} "
        f"({s1['prolog_coverage']['pct']}%)."
    )

    section("1.2 By Claimed Type (atom-level)", 3)
    for ctype in sorted(TARGET_TYPES):
        dist = stats["by_claimed_type"].get(ctype, {})
        total = sum(dist.values()) or 1
        L.append(f"**{ctype}** (N={total} atoms):")
        L.append("")
        rows = [
            (atype, dist.get(atype, 0), f"{pct(dist.get(atype, 0), total)}%")
            for atype in ACTOR_TYPES
        ]
        L.append(md_table(["Actor Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

    section("1.3 Dominant Beneficiary Type by Claimed Type (constraint-level)", 3)
    for ctype in sorted(TARGET_TYPES):
        dist = stats["dominant_by_claimed_type"].get(ctype, {})
        total = sum(dist.values()) or 1
        L.append(f"**{ctype}** (N={total} constraints):")
        L.append("")
        rows = [
            (atype, dist.get(atype, 0), f"{pct(dist.get(atype, 0), total)}%")
            for atype in ACTOR_TYPES
        ]
        L.append(md_table(["Dominant Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

    section("1.4 Intercepted vs Passing", 3)
    for label, dist in [("Intercepted", stats.get("intercepted_dominant_dist", {})),
                        ("Passing own gate", stats.get("passing_dominant_dist", {}))]:
        total = sum(dist.values()) or 1
        L.append(f"**{label}** (N={total}):")
        L.append("")
        rows = [
            (atype, dist.get(atype, 0), f"{pct(dist.get(atype, 0), total)}%")
            for atype in ACTOR_TYPES
        ]
        L.append(md_table(["Dominant Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

    # ---- Section 2: Victim Analysis ----
    section("2. Victim Actor-Type Analysis")

    vstats = s2["population_stats"]
    section("2.1 Population Distribution", 3)
    para(
        f"Total victim atoms classified: **{vstats['total_atoms']}** "
        f"across {pv['total'] + sv['total']} constraints."
    )
    rows = [
        (atype, vstats["type_distribution"].get(atype, 0),
         f"{pct(vstats['type_distribution'].get(atype, 0), vstats['total_atoms'])}%")
        for atype in ACTOR_TYPES
    ]
    L.append(md_table(["Actor Type", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    section("2.2 By Claimed Type (atom-level)", 3)
    for ctype in sorted(TARGET_TYPES):
        dist = vstats["by_claimed_type"].get(ctype, {})
        total = sum(dist.values()) or 1
        L.append(f"**{ctype}** (N={total} atoms):")
        L.append("")
        rows = [
            (atype, dist.get(atype, 0), f"{pct(dist.get(atype, 0), total)}%")
            for atype in ACTOR_TYPES
        ]
        L.append(md_table(["Actor Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

    section("2.3 Dominant Victim Type by Claimed Type", 3)
    for ctype in sorted(TARGET_TYPES):
        dist = vstats["dominant_by_claimed_type"].get(ctype, {})
        total = sum(dist.values()) or 1
        L.append(f"**{ctype}** (N={total} constraints):")
        L.append("")
        rows = [
            (atype, dist.get(atype, 0), f"{pct(dist.get(atype, 0), total)}%")
            for atype in ACTOR_TYPES
        ]
        L.append(md_table(["Dominant Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

    # ---- Section 3: Cross-Reference ----
    section("3. Theater-Extraction-Coordination Cross-Reference")

    section("3.1 Proxy 1: Beneficiary-Victim Asymmetry Score", 3)
    para(
        "Asymmetry = (institutional beneficiary fraction) − (institutional victim "
        "fraction). Positive = persistence pattern (institutional beneficiaries, "
        "individual victims). Negative = functional pattern (individual beneficiaries, "
        "institutional victims)."
    )
    p1 = s3["proxy1_asymmetry"]
    para(
        f"Overall: **{p1['persistence_pattern_count']}** persistence pattern (>0), "
        f"**{p1['functional_pattern_count']}** functional pattern (<0), "
        f"**{p1['neutral_count']}** neutral (=0) out of {p1['total']}."
    )
    rows = []
    for ctype in sorted(TARGET_TYPES):
        st = p1["by_claimed_type"].get(ctype)
        if st:
            rows.append((ctype, st["n"], fmt(st["mean"]), fmt(st["median"]),
                         fmt(st["min"]), fmt(st["max"])))
        else:
            rows.append((ctype, 0, "---", "---", "---", "---"))
    L.append(md_table(
        ["Type", "N", "Mean", "Median", "Min", "Max"],
        rows, ["l", "r", "r", "r", "r", "r"],
    ))
    L.append("")

    section("3.2 Proxy 2: Theater Ratio × Beneficiary Type", 3)
    para(
        "Partitions constraints by theater ratio threshold (TR ≥ 0.70) and "
        "dominant beneficiary actor type (institutional vs non-institutional)."
    )
    p2 = s3["proxy2_tr_epsilon"]
    interpretation = {
        "theatrical_persistence": "High-TR + institutional → dead coordination, theater persists",
        "theatrical_function": "High-TR + non-institutional → active coordination masked by theater",
        "active_persistence": "Low-TR + institutional → active institutional extraction",
        "active_function": "Low-TR + non-institutional → genuine functional coordination",
    }
    rows = []
    for cell in ["theatrical_persistence", "theatrical_function",
                 "active_persistence", "active_function"]:
        d = p2[cell]
        eps_st = d.get("epsilon_stats")
        rows.append((
            cell.replace("_", " ").title(),
            d["count"], f"{d['pct']}%",
            fmt(eps_st["mean"] if eps_st else None),
            fmt(eps_st["median"] if eps_st else None),
        ))
    L.append(md_table(
        ["Cell", "N", "%", "Mean ε", "Median ε"],
        rows, ["l", "r", "r", "r", "r"],
    ))
    L.append("")
    for cell, interp in interpretation.items():
        L.append(f"- **{cell.replace('_', ' ').title()}**: {interp}")
    L.append("")

    section("3.3 Proxy 3: RAE × Beneficiary Type", 3)
    para(
        "Partitions by requires_active_enforcement and dominant beneficiary type."
    )
    p3 = s3["proxy3_rae_beneficiary"]
    rae_interp = {
        "enforced_persistence": "RAE=T + institutional → enforced theater (extractive piton)",
        "enforced_function": "RAE=T + non-institutional → enforced coordination (tangled_rope)",
        "emergent_persistence": "RAE=F + institutional → emergent institutional inertia",
        "emergent_function": "RAE=F + non-institutional → emergent functional coordination",
    }
    rows = []
    for cell in ["enforced_persistence", "enforced_function",
                 "emergent_persistence", "emergent_function"]:
        d = p3[cell]
        rows.append((
            cell.replace("_", " ").title(),
            d["count"], f"{d['pct']}%",
        ))
    L.append(md_table(["Cell", "N", "%"], rows, ["l", "r", "r"]))
    L.append("")
    for cell, interp in rae_interp.items():
        L.append(f"- **{cell.replace('_', ' ').title()}**: {interp}")
    L.append("")

    # ---- Section 4: Scaffold Assessment ----
    section("4. Scaffold Vitality Assessment")

    s4_summary = s4["summary"]
    para(
        f"N={s4_summary['total']} scaffolds. "
        f"Sunset clause detected in .pl files: {s4_summary['sunset_clause_count']}. "
        f"Coordination type coverage: {s4_summary['coord_type_coverage']}/{s4_summary['total']}. "
        f"Intercepted by higher-priority gate: {s4_summary['intercepted_count']}."
    )

    rows = []
    for sc in s4["per_scaffold"]:
        rows.append((
            f"`{sc['id']}`",
            sc["dominant_beneficiary_type"][:12],
            "Y" if sc["has_sunset_clause"] else "N",
            sc["coordination_type"] or "---",
            fmt(sc["epsilon"]),
            sc["risk_band"],
            ", ".join(sc["risk_signals"]) or "---",
        ))
    L.append(md_table(
        ["Constraint", "Ben. Type", "Sunset", "Coord Type", "ε", "Risk", "Signals"],
        rows, ["l", "l", "c", "l", "r", "c", "l"],
    ))
    L.append("")

    para(
        f"Risk distribution: **{s4_summary['high_risk']}** high, "
        f"**{s4_summary['medium_risk']}** medium, "
        f"**{s4_summary['low_risk']}** low."
    )

    # ---- Section 5: Piton Degradation Assessment ----
    section("5. Piton Degradation Path Assessment")

    s5_summary = s5["summary"]
    para(
        f"N={s5_summary['total']} pitons classified by degradation state."
    )

    section("5.1 Classification Distribution", 3)
    rows = [
        (cls, s5_summary.get(cls, 0),
         f"{pct(s5_summary.get(cls, 0), s5_summary['total'])}%")
        for cls in ["terminal", "degrading", "transitional"]
    ]
    L.append(md_table(["State", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    section("5.2 Signal Frequency", 3)
    rows = [
        (sig, n, f"{pct(n, s5_summary['total'])}%")
        for sig, n in s5["signal_frequency"].items()
    ]
    L.append(md_table(["Signal", "Count", "% of pitons"], rows, ["l", "r", "r"]))
    L.append("")

    section("5.3 Metric Profiles by Classification", 3)
    for cls_name in ["terminal", "degrading", "transitional"]:
        mdata = s5["metric_stats_by_classification"].get(cls_name)
        if not mdata:
            continue
        L.append(f"**{cls_name.title()}** (N={mdata['count']}, "
                 f"intercepted: {mdata['intercepted_count']}):")
        L.append("")
        rows = []
        for metric in ["epsilon", "suppression", "theater_ratio"]:
            st = mdata.get(metric)
            if st:
                rows.append((metric, fmt(st["mean"]), fmt(st["median"]),
                             fmt(st["std"]), fmt(st["min"]), fmt(st["max"])))
        if rows:
            L.append(md_table(
                ["Metric", "Mean", "Median", "Std", "Min", "Max"],
                rows, ["l", "r", "r", "r", "r", "r"],
            ))
            L.append("")

    section("5.4 Gate Interception by Classification", 3)
    para(
        f"Intercepted by classification: "
        f"{dict(s5.get('intercepted_by_classification', {}))}. "
        f"Passing own gate by classification: "
        f"{dict(s5.get('passing_by_classification', {}))}."
    )

    # ---- Section 6: Scaffold Lifecycle ----
    section("6. Scaffold Lifecycle Position")

    s6_summary = s6["summary"]
    para(f"N={s6_summary['total']} scaffolds classified by lifecycle position.")

    rows = []
    for lc in ["genuinely_temporary", "likely_temporary", "ambiguous_lifecycle",
                "calcifying", "calcified"]:
        n = s6_summary.get(lc, 0)
        if n > 0 or lc in ("genuinely_temporary", "calcified"):
            rows.append((lc, n, f"{pct(n, s6_summary['total'])}%"))
    L.append(md_table(["Lifecycle", "Count", "%"], rows, ["l", "r", "r"]))
    L.append("")

    rows = []
    for sc in s6["per_scaffold"]:
        rows.append((
            f"`{sc['id']}`",
            sc["lifecycle"],
            sc["temporary_score"],
            sc["calcified_score"],
            fmt(sc["epsilon"]),
            "Y" if sc["intercepted"] else "N",
        ))
    L.append(md_table(
        ["Constraint", "Lifecycle", "Temp Score", "Calc Score", "ε", "Intercepted"],
        rows, ["l", "l", "r", "r", "r", "c"],
    ))
    L.append("")

    # ---- Section 7: Verdict ----
    section("7. Verdict and Implications")

    section("7.1 Piton Coordination Vitality", 3)
    pv = s7["piton_verdict"]
    para(
        f"Of {pv['total']} pitons: **{pv['terminal']}** terminal "
        f"({pct(pv['terminal'], pv['total'])}%), "
        f"**{pv['degrading']}** degrading "
        f"({pct(pv['degrading'], pv['total'])}%), "
        f"**{pv['transitional']}** transitional "
        f"({pct(pv['transitional'], pv['total'])}%)."
    )
    para(
        f"Dead/degrading coordination fraction: "
        f"**{pv['dead_coordination_frac']:.1%}**. "
        f"Active coordination fraction: "
        f"**{pv['active_coordination_frac']:.1%}**."
    )
    if pv["dead_coordination_frac"] > 0.5:
        para(
            "**Verdict**: Majority of pitons have dead or degrading coordination. "
            "The ontology's piton definition may be too narrow (epsilon ceiling 0.25 "
            "excludes high-extraction dead constraints), or an 'extractive piton' "
            "subtype is needed."
        )
    else:
        para(
            "**Verdict**: Majority of pitons still show active coordination signals. "
            "The LLM may be over-calling piton where tangled_rope is the correct "
            "classification — these constraints still solve collective action problems "
            "but with high theater."
        )

    section("7.2 Scaffold Coordination Vitality", 3)
    sv = s7["scaffold_verdict"]
    para(
        f"Of {sv['total']} scaffolds: **{sv['genuinely_temporary']}** genuinely/"
        f"likely temporary ({pct(sv['genuinely_temporary'], sv['total'])}%), "
        f"**{sv['calcified']}** calcifying/calcified "
        f"({pct(sv['calcified'], sv['total'])}%)."
    )
    if sv["calcification_frac"] > 0.5:
        para(
            "**Verdict**: Majority of scaffolds show calcification signals. "
            "The LLM may be labeling tangled_ropes as scaffolds."
        )
    else:
        para(
            "**Verdict**: Most scaffolds appear genuinely temporary. The scaffold "
            "extraction ceiling (0.30) may be too low for legitimate high-cost "
            "transition support."
        )

    section("7.3 Cross-Type: Does Beneficiary Type Predict Gate Outcome?", 3)
    ct = s7["cross_type"]
    L.append("Beneficiary dominant type distribution by gate interception status:")
    L.append("")
    for status in ["intercepted", "passing"]:
        dist = ct["beneficiary_type_by_interception"].get(status, {})
        total = sum(dist.values()) or 1
        L.append(f"**{status.title()}** (N={total}):")
        L.append("")
        rows = [
            (atype, dist.get(atype, 0), f"{pct(dist.get(atype, 0), total)}%")
            for atype in ACTOR_TYPES if dist.get(atype, 0) > 0
        ]
        L.append(md_table(["Type", "Count", "%"], rows, ["l", "r", "r"]))
        L.append("")

    int_st = ct.get("intercepted_asymmetry_stats")
    pass_st = ct.get("passing_asymmetry_stats")
    if int_st and pass_st:
        para(
            f"Mean asymmetry score — intercepted: {fmt(int_st['mean'])}, "
            f"passing: {fmt(pass_st['mean'])}. "
            + ("The difference suggests beneficiary type carries predictive signal."
               if abs((int_st["mean"] or 0) - (pass_st["mean"] or 0)) > 0.1
               else "The difference is small — beneficiary type alone is not strongly predictive.")
        )

    section("7.4 Recommendations", 3)
    for i, rec in enumerate(s7["recommendations"], 1):
        L.append(f"{i}. {rec}")
    L.append("")

    return "\n".join(L)


# ---------------------------------------------------------------------------
# JSON Output
# ---------------------------------------------------------------------------


def generate_json(s1, s2, s3, s4, s5, s6, s7, prolog_meta, timestamp):
    """Build the JSON sidecar structure."""
    # Trim per-constraint details for JSON (keep summaries + sample)
    s1_json = {k: v for k, v in s1.items() if k != "per_constraint"}
    s1_json["sample_classifications"] = [
        {"constraint": cid, **data["classification"]["atoms"][0]}
        for cid, data in list(s1["per_constraint"].items())[:10]
        if data["classification"]["atoms"]
    ]

    s2_json = {k: v for k, v in s2.items() if k != "per_constraint"}
    s2_json["sample_classifications"] = [
        {"constraint": cid, **data["classification"]["atoms"][0]}
        for cid, data in list(s2["per_constraint"].items())[:10]
        if data["classification"]["atoms"]
    ]

    s3_json = {k: v for k, v in s3.items() if k != "per_constraint_asymmetry"}
    # Include per-constraint asymmetry as sample
    s3_json["sample_asymmetry"] = dict(
        list(s3.get("per_constraint_asymmetry", {}).items())[:10]
    )

    return {
        "metadata": {
            "generated": timestamp,
            "script": "python/coordination_vitality_diagnostic.py",
            "sources": [
                "outputs/enriched_pipeline.json",
                "prolog/testsets/*.pl",
            ],
            "population": {
                "scaffold_count": s7["scaffold_verdict"]["total"],
                "piton_count": s7["piton_verdict"]["total"],
            },
            "prolog_coverage": {
                "files_parsed": prolog_meta["files_parsed"],
                "coordination_type_count": len(prolog_meta["coordination_types"]),
                "sunset_clause_count": len(prolog_meta["sunset_clauses"]),
            },
            "keyword_sets": {
                "institutional_tokens": len(INSTITUTIONAL_TOKENS),
                "individual_tokens": len(INDIVIDUAL_TOKENS),
                "collective_tokens": len(COLLECTIVE_TOKENS),
                "abstract_tokens": len(ABSTRACT_TOKENS),
            },
        },
        "step1_beneficiary_analysis": s1_json,
        "step2_victim_analysis": s2_json,
        "step3_cross_reference": s3_json,
        "step4_scaffold_assessment": s4,
        "step5_degradation_assessment": {
            k: v for k, v in s5.items() if k != "per_piton"
        } | {"sample_pitons": [
            {k: v for k, v in p.items() if k != "human_readable"}
            for p in s5["per_piton"][:15]
        ]},
        "step6_scaffold_lifecycle": s6,
        "step7_verdict": s7,
    }


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    tag = "[CVD]"
    print(f"{tag} Loading enriched_pipeline.json...", file=sys.stderr)
    data = load_json(ENRICHED_PIPELINE_JSON, "enriched_pipeline",
                     schema=validate_enriched_pipeline)
    per_constraint = data.get("per_constraint", [])
    total = len(per_constraint)

    pop = [r for r in per_constraint if r.get("claimed_type") in TARGET_TYPES]
    print(f"{tag} Found {len(pop)} scaffold/piton constraints "
          f"(of {total} total)", file=sys.stderr)

    if not pop:
        print(f"{tag} ERROR: No scaffold/piton constraints found",
              file=sys.stderr)
        sys.exit(1)

    print(f"{tag} Parsing Prolog testset files...", file=sys.stderr)
    prolog_meta = parse_prolog_testsets(TESTSET_DIR)
    print(f"{tag} Parsed {prolog_meta['files_parsed']} .pl files: "
          f"{len(prolog_meta['beneficiaries'])} with beneficiaries, "
          f"{len(prolog_meta['coordination_types'])} with coordination_type, "
          f"{len(prolog_meta['sunset_clauses'])} with sunset clause",
          file=sys.stderr)

    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    print(f"{tag} Step 1: Beneficiary text analysis...", file=sys.stderr)
    s1 = step1_beneficiary_analysis(pop, prolog_meta)

    print(f"{tag} Step 2: Victim text analysis...", file=sys.stderr)
    s2 = step2_victim_analysis(pop, prolog_meta)

    print(f"{tag} Step 3: Theater-extraction-coordination cross-reference...",
          file=sys.stderr)
    s3 = step3_cross_reference(pop, s1, s2, prolog_meta)

    print(f"{tag} Step 4: Scaffold-specific assessment...", file=sys.stderr)
    s4 = step4_scaffold_assessment(pop, s1, s2, prolog_meta)

    print(f"{tag} Step 5: Piton degradation path assessment...", file=sys.stderr)
    s5 = step5_degradation_assessment(pop, s1, s2, prolog_meta)

    print(f"{tag} Step 6: Scaffold lifecycle position...", file=sys.stderr)
    s6 = step6_scaffold_lifecycle(pop, s1, prolog_meta, s4)

    print(f"{tag} Step 7: Verdict and implications...", file=sys.stderr)
    s7 = step7_verdict(pop, s1, s2, s3, s4, s5, s6)

    # Generate outputs
    print(f"{tag} Generating report...", file=sys.stderr)
    report_md = generate_report(s1, s2, s3, s4, s5, s6, s7, prolog_meta, timestamp)

    DOCS_DIR.mkdir(parents=True, exist_ok=True)
    with open(REPORT_PATH, "w", encoding="utf-8") as f:
        f.write(report_md)
    print(f"{tag} Wrote {REPORT_PATH}", file=sys.stderr)

    json_data = generate_json(s1, s2, s3, s4, s5, s6, s7, prolog_meta, timestamp)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    with open(DATA_PATH, "w", encoding="utf-8") as f:
        json.dump(json_data, f, indent=2)
    print(f"{tag} Wrote {DATA_PATH}", file=sys.stderr)

    print(f"{tag} Done.", file=sys.stderr)


if __name__ == "__main__":
    main()
