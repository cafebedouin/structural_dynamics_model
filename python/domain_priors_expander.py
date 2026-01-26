#!/usr/bin/env python3
"""
Domain Priors Expander - v1.0

Analyzes all testset files in prolog/testsets/*.pl to compute corpus averages
and expands domain_priors.pl with reasonable defaults for inputted domains.

Usage:
    python domain_priors_expander.py

Output:
    - Prints corpus statistics to console
    - Generates prolog/domain_priors_expanded.pl with new category profiles
"""

import os
import re
import json
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple
from pathlib import Path

# Configuration
TESTSETS_DIR = Path(__file__).parent.parent / 'prolog' / 'testsets'
DOMAIN_REGISTRY = Path(__file__).parent.parent / 'prolog' / 'domain_registry.pl'
DOMAIN_PRIORS = Path(__file__).parent.parent / 'prolog' / 'domain_priors.pl'
OUTPUT_FILE = Path(__file__).parent.parent / 'prolog' / 'domain_priors_expanded.pl'
STATS_OUTPUT = Path(__file__).parent.parent / 'outputs' / 'domain_corpus_stats.json'

# Regex patterns for extracting Prolog facts
EXTRACTIVENESS_REGEX = re.compile(
    r"(?:domain_priors:)?base_extractiveness\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([0-9.]+)\s*\)"
)
SUPPRESSION_REGEX = re.compile(
    r"(?:domain_priors:)?suppression_score\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([0-9.]+)\s*\)"
)
CONSTRAINT_CLAIM_REGEX = re.compile(
    r"(?:narrative_ontology:)?constraint_claim\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([a-zA-Z_]+)\s*\)"
)
REQUIRES_ENFORCEMENT_REGEX = re.compile(
    r"(?:domain_priors:)?requires_active_enforcement\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*\)"
)
EMERGES_NATURALLY_REGEX = re.compile(
    r"(?:domain_priors:)?emerges_naturally\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*\)"
)
DOMAIN_CATEGORY_REGEX = re.compile(
    r"domain_category\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([a-zA-Z_]+)\s*\)"
)
INTERVAL_REGEX = re.compile(
    r"(?:narrative_ontology:)?interval\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*(-?[0-9]+)\s*,\s*(-?[0-9]+)\s*\)"
)
CONSTRAINT_METRIC_REGEX = re.compile(
    r"constraint_metric\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([a-zA-Z_]+)\s*,\s*([0-9.]+)\s*\)"
)


@dataclass
class ConstraintData:
    """Data extracted from a single testset file."""
    constraint_id: str
    file_path: str
    base_extractiveness: Optional[float] = None
    suppression_score: Optional[float] = None
    constraint_type: Optional[str] = None  # mountain, rope, noose, tangled_rope
    requires_enforcement: bool = False
    emerges_naturally: bool = False
    category: Optional[str] = None
    interval_start: Optional[int] = None
    interval_end: Optional[int] = None
    metrics: Dict[str, float] = field(default_factory=dict)


@dataclass
class CategoryStats:
    """Aggregated statistics for a category."""
    category: str
    count: int = 0
    extractiveness_values: List[float] = field(default_factory=list)
    suppression_values: List[float] = field(default_factory=list)
    constraint_types: Dict[str, int] = field(default_factory=lambda: defaultdict(int))
    enforcement_count: int = 0
    natural_count: int = 0

    @property
    def avg_extractiveness(self) -> float:
        return sum(self.extractiveness_values) / len(self.extractiveness_values) if self.extractiveness_values else 0.5

    @property
    def avg_suppression(self) -> float:
        return sum(self.suppression_values) / len(self.suppression_values) if self.suppression_values else 0.5

    @property
    def std_extractiveness(self) -> float:
        if len(self.extractiveness_values) < 2:
            return 0.0
        mean = self.avg_extractiveness
        variance = sum((x - mean) ** 2 for x in self.extractiveness_values) / len(self.extractiveness_values)
        return variance ** 0.5

    @property
    def std_suppression(self) -> float:
        if len(self.suppression_values) < 2:
            return 0.0
        mean = self.avg_suppression
        variance = sum((x - mean) ** 2 for x in self.suppression_values) / len(self.suppression_values)
        return variance ** 0.5

    @property
    def dominant_type(self) -> str:
        if not self.constraint_types:
            return 'unknown'
        return max(self.constraint_types.items(), key=lambda x: x[1])[0]

    @property
    def enforcement_ratio(self) -> float:
        total = self.enforcement_count + self.natural_count
        return self.enforcement_count / total if total > 0 else 0.5


def load_domain_registry() -> Dict[str, str]:
    """Load domain_category mappings from domain_registry.pl."""
    registry = {}
    if not DOMAIN_REGISTRY.exists():
        print(f"Warning: {DOMAIN_REGISTRY} not found")
        return registry

    with open(DOMAIN_REGISTRY, 'r', encoding='utf-8') as f:
        content = f.read()
        for match in DOMAIN_CATEGORY_REGEX.finditer(content):
            constraint_id, category = match.groups()
            registry[constraint_id] = category

    print(f"Loaded {len(registry)} domain mappings from registry")
    return registry


def parse_testset_file(filepath: Path, registry: Dict[str, str]) -> Optional[ConstraintData]:
    """Parse a single testset file and extract constraint data."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        print(f"Error reading {filepath}: {e}")
        return None

    # Extract constraint ID from interval or filename
    interval_match = INTERVAL_REGEX.search(content)
    if interval_match:
        constraint_id = interval_match.group(1)
        interval_start = int(interval_match.group(2))
        interval_end = int(interval_match.group(3))
    else:
        constraint_id = filepath.stem
        interval_start = None
        interval_end = None

    data = ConstraintData(
        constraint_id=constraint_id,
        file_path=str(filepath),
        interval_start=interval_start,
        interval_end=interval_end
    )

    # Extract base_extractiveness
    ext_match = EXTRACTIVENESS_REGEX.search(content)
    if ext_match:
        data.base_extractiveness = float(ext_match.group(2))

    # Extract suppression_score
    sup_match = SUPPRESSION_REGEX.search(content)
    if sup_match:
        data.suppression_score = float(sup_match.group(2))

    # Extract constraint_claim type
    claim_match = CONSTRAINT_CLAIM_REGEX.search(content)
    if claim_match:
        data.constraint_type = claim_match.group(2)

    # Check enforcement/natural emergence
    if REQUIRES_ENFORCEMENT_REGEX.search(content):
        data.requires_enforcement = True
    if EMERGES_NATURALLY_REGEX.search(content):
        data.emerges_naturally = True

    # Get category from registry
    data.category = registry.get(constraint_id, registry.get(filepath.stem, 'unknown_novel'))

    # Extract additional metrics
    for match in CONSTRAINT_METRIC_REGEX.finditer(content):
        metric_name = match.group(2)
        metric_value = float(match.group(3))
        data.metrics[metric_name] = metric_value

    return data


def analyze_corpus(testsets_dir: Path, registry: Dict[str, str]) -> Tuple[List[ConstraintData], Dict[str, CategoryStats]]:
    """Analyze all testset files and compute category statistics."""
    constraints = []
    category_stats: Dict[str, CategoryStats] = defaultdict(lambda: CategoryStats(category='unknown'))

    pl_files = sorted(testsets_dir.glob('*.pl'))
    print(f"Found {len(pl_files)} testset files")

    for filepath in pl_files:
        data = parse_testset_file(filepath, registry)
        if data is None:
            continue

        constraints.append(data)

        # Update category stats
        cat = data.category or 'unknown_novel'
        if cat not in category_stats:
            category_stats[cat] = CategoryStats(category=cat)

        stats = category_stats[cat]
        stats.count += 1

        if data.base_extractiveness is not None:
            stats.extractiveness_values.append(data.base_extractiveness)
        if data.suppression_score is not None:
            stats.suppression_values.append(data.suppression_score)
        if data.constraint_type:
            stats.constraint_types[data.constraint_type] += 1
        if data.requires_enforcement:
            stats.enforcement_count += 1
        if data.emerges_naturally:
            stats.natural_count += 1

    print(f"Parsed {len(constraints)} constraints across {len(category_stats)} categories")
    return constraints, dict(category_stats)


def compute_type_profiles(constraints: List[ConstraintData]) -> Dict[str, Dict[str, float]]:
    """Compute average profiles by constraint type (mountain/rope/noose/tangled_rope)."""
    type_stats = defaultdict(lambda: {'extractiveness': [], 'suppression': [], 'count': 0})

    for c in constraints:
        if c.constraint_type:
            type_stats[c.constraint_type]['count'] += 1
            if c.base_extractiveness is not None:
                type_stats[c.constraint_type]['extractiveness'].append(c.base_extractiveness)
            if c.suppression_score is not None:
                type_stats[c.constraint_type]['suppression'].append(c.suppression_score)

    profiles = {}
    for ctype, data in type_stats.items():
        ext_vals = data['extractiveness']
        sup_vals = data['suppression']
        profiles[ctype] = {
            'count': data['count'],
            'avg_extractiveness': sum(ext_vals) / len(ext_vals) if ext_vals else 0.5,
            'avg_suppression': sum(sup_vals) / len(sup_vals) if sup_vals else 0.5,
            'std_extractiveness': (sum((x - sum(ext_vals)/len(ext_vals))**2 for x in ext_vals) / len(ext_vals))**0.5 if len(ext_vals) > 1 else 0.0,
            'std_suppression': (sum((x - sum(sup_vals)/len(sup_vals))**2 for x in sup_vals) / len(sup_vals))**0.5 if len(sup_vals) > 1 else 0.0
        }

    return profiles


def infer_subcategories(constraints: List[ConstraintData], category_stats: Dict[str, CategoryStats]) -> Dict[str, Dict[str, float]]:
    """Infer subcategories based on clustering within categories."""
    subcategories = {}

    for cat, stats in category_stats.items():
        if stats.count < 5:
            continue

        # Split by constraint type within category
        type_groups = defaultdict(list)
        for c in constraints:
            if c.category == cat and c.constraint_type:
                type_groups[c.constraint_type].append(c)

        for ctype, group in type_groups.items():
            if len(group) >= 3:
                ext_vals = [c.base_extractiveness for c in group if c.base_extractiveness is not None]
                sup_vals = [c.suppression_score for c in group if c.suppression_score is not None]

                subcat_name = f"{cat}_{ctype}"
                subcategories[subcat_name] = {
                    'parent': cat,
                    'constraint_type': ctype,
                    'count': len(group),
                    'avg_extractiveness': sum(ext_vals) / len(ext_vals) if ext_vals else 0.5,
                    'avg_suppression': sum(sup_vals) / len(sup_vals) if sup_vals else 0.5
                }

    return subcategories


def generate_expanded_priors(
    category_stats: Dict[str, CategoryStats],
    type_profiles: Dict[str, Dict[str, float]],
    subcategories: Dict[str, Dict[str, float]]
) -> str:
    """Generate expanded domain_priors.pl content."""

    lines = []
    lines.append(":- module(domain_priors_expanded, [")
    lines.append("    get_corpus_prior/3,")
    lines.append("    category_corpus_profile/2,")
    lines.append("    type_corpus_profile/2,")
    lines.append("    subcategory_profile/2,")
    lines.append("    default_extractiveness/2,")
    lines.append("    default_suppression/2,")
    lines.append("    default_resistance/2,")
    lines.append("    infer_category_defaults/4")
    lines.append("]).")
    lines.append("")
    lines.append("/**")
    lines.append(" * DOMAIN PRIORS EXPANSION - Auto-generated from corpus analysis")
    lines.append(f" * Generated from {sum(s.count for s in category_stats.values())} constraints")
    lines.append(f" * Categories analyzed: {len(category_stats)}")
    lines.append(" *")
    lines.append(" * This module provides corpus-derived defaults for domain priors.")
    lines.append(" * Use these when a new domain lacks explicit priors.")
    lines.append(" */")
    lines.append("")

    # Category profiles derived from corpus
    lines.append("%% ============================================================================")
    lines.append("%% 1. CATEGORY CORPUS PROFILES (From Corpus Averages)")
    lines.append("%% ============================================================================")
    lines.append("%% Format: category_corpus_profile(Category, [AvgExtract, AvgSuppress, StdExtract, StdSuppress, Count]).")
    lines.append("")

    for cat, stats in sorted(category_stats.items(), key=lambda x: -x[1].count):
        profile = [
            round(stats.avg_extractiveness, 3),
            round(stats.avg_suppression, 3),
            round(stats.std_extractiveness, 3),
            round(stats.std_suppression, 3),
            stats.count
        ]
        dominant = stats.dominant_type
        enf_ratio = round(stats.enforcement_ratio, 2)
        lines.append(f"category_corpus_profile({cat}, {profile}).  % dominant: {dominant}, enforcement_ratio: {enf_ratio}")

    lines.append("")

    # Type profiles (mountain, rope, noose, tangled_rope)
    lines.append("%% ============================================================================")
    lines.append("%% 2. CONSTRAINT TYPE PROFILES (Mountain/Rope/Noose/Tangled Rope)")
    lines.append("%% ============================================================================")
    lines.append("%% Format: type_corpus_profile(Type, [AvgExtract, AvgSuppress, StdExtract, StdSuppress, Count]).")
    lines.append("")

    for ctype, profile in sorted(type_profiles.items(), key=lambda x: -x[1]['count']):
        vec = [
            round(profile['avg_extractiveness'], 3),
            round(profile['avg_suppression'], 3),
            round(profile['std_extractiveness'], 3),
            round(profile['std_suppression'], 3),
            profile['count']
        ]
        lines.append(f"type_corpus_profile({ctype}, {vec}).")

    lines.append("")

    # Subcategory profiles
    if subcategories:
        lines.append("%% ============================================================================")
        lines.append("%% 3. SUBCATEGORY PROFILES (Category + Type combinations)")
        lines.append("%% ============================================================================")
        lines.append("%% Format: subcategory_profile(SubcatName, [Parent, Type, AvgExtract, AvgSuppress, Count]).")
        lines.append("")

        for subcat, profile in sorted(subcategories.items(), key=lambda x: -x[1]['count']):
            vec = [
                profile['parent'],
                profile['constraint_type'],
                round(profile['avg_extractiveness'], 3),
                round(profile['avg_suppression'], 3),
                profile['count']
            ]
            lines.append(f"subcategory_profile({subcat}, {vec}).")

        lines.append("")

    # Default value inference predicates
    lines.append("%% ============================================================================")
    lines.append("%% 4. DEFAULT VALUE INFERENCE PREDICATES")
    lines.append("%% ============================================================================")
    lines.append("")
    lines.append("%% default_extractiveness(+Category, -Value)")
    lines.append("%% Returns corpus-derived default extractiveness for a category.")
    lines.append("default_extractiveness(Category, Value) :-")
    lines.append("    category_corpus_profile(Category, [Value|_]), !.")
    lines.append("default_extractiveness(_, 0.5).  % Neutral fallback")
    lines.append("")
    lines.append("%% default_suppression(+Category, -Value)")
    lines.append("%% Returns corpus-derived default suppression for a category.")
    lines.append("default_suppression(Category, Value) :-")
    lines.append("    category_corpus_profile(Category, [_, Value|_]), !.")
    lines.append("default_suppression(_, 0.5).  % Neutral fallback")
    lines.append("")
    lines.append("%% default_resistance(+Category, -Value)")
    lines.append("%% Infers resistance from extractiveness (inverse correlation).")
    lines.append("default_resistance(Category, Value) :-")
    lines.append("    default_extractiveness(Category, Ext),")
    lines.append("    Value is max(0.1, min(0.9, 1.0 - Ext * 0.5)), !.")
    lines.append("default_resistance(_, 0.5).")
    lines.append("")

    # Main inference predicate
    lines.append("%% infer_category_defaults(+Category, -Extractiveness, -Suppression, -Resistance)")
    lines.append("%% Unified predicate to get all defaults for a category.")
    lines.append("infer_category_defaults(Category, Ext, Sup, Res) :-")
    lines.append("    default_extractiveness(Category, Ext),")
    lines.append("    default_suppression(Category, Sup),")
    lines.append("    default_resistance(Category, Res).")
    lines.append("")

    # Type-based inference
    lines.append("%% get_corpus_prior(+ID, +Metric, -Value)")
    lines.append("%% Retrieves corpus-derived prior by constraint type.")
    lines.append("get_corpus_prior(ID, extractiveness, Value) :-")
    lines.append("    narrative_ontology:constraint_claim(ID, Type),")
    lines.append("    type_corpus_profile(Type, [Value|_]), !.")
    lines.append("get_corpus_prior(ID, suppression, Value) :-")
    lines.append("    narrative_ontology:constraint_claim(ID, Type),")
    lines.append("    type_corpus_profile(Type, [_, Value|_]), !.")
    lines.append("get_corpus_prior(_, _, 0.5).  % Neutral fallback")
    lines.append("")

    # Recommended category profile vectors (4-element format matching original domain_priors)
    lines.append("%% ============================================================================")
    lines.append("%% 5. RECOMMENDED CATEGORY PROFILE VECTORS")
    lines.append("%% ============================================================================")
    lines.append("%% These are corpus-calibrated replacements for the original category_profile/2.")
    lines.append("%% Format: [accessibility_collapse, stakes_inflation, suppression, resistance]")
    lines.append("")

    for cat, stats in sorted(category_stats.items(), key=lambda x: -x[1].count):
        if stats.count >= 5:  # Only output for categories with sufficient data
            # Map corpus stats to 4-vector format:
            # accessibility_collapse ~ inverse of enforcement_ratio (natural = high accessibility)
            # stakes_inflation ~ extractiveness (high extract = high stakes)
            # suppression ~ suppression_score
            # resistance ~ inverse correlation with extractiveness
            acc_collapse = round(1.0 - stats.enforcement_ratio * 0.5, 2)
            stakes = round(stats.avg_extractiveness * 1.2, 2)  # Slight inflation
            stakes = min(1.0, stakes)
            suppress = round(stats.avg_suppression, 2)
            resist = round(max(0.1, 1.0 - stats.avg_extractiveness * 0.6), 2)

            lines.append(f"% recommended_profile({cat}, [{acc_collapse}, {stakes}, {suppress}, {resist}]).  % N={stats.count}, dominant={stats.dominant_type}")

    lines.append("")
    lines.append("%% ============================================================================")
    lines.append("%% END OF AUTO-GENERATED PRIORS")
    lines.append("%% ============================================================================")

    return '\n'.join(lines)


def print_corpus_report(
    constraints: List[ConstraintData],
    category_stats: Dict[str, CategoryStats],
    type_profiles: Dict[str, Dict[str, float]]
):
    """Print a detailed corpus analysis report."""
    print("\n" + "=" * 70)
    print("   DOMAIN PRIORS CORPUS ANALYSIS REPORT")
    print("=" * 70)

    print(f"\nTotal constraints analyzed: {len(constraints)}")
    print(f"Categories found: {len(category_stats)}")

    # Category breakdown
    print("\n--- CATEGORY BREAKDOWN ---")
    print(f"{'Category':<30} {'Count':>6} {'Avg Ext':>8} {'Avg Sup':>8} {'Dominant':>12}")
    print("-" * 70)

    for cat, stats in sorted(category_stats.items(), key=lambda x: -x[1].count):
        print(f"{cat:<30} {stats.count:>6} {stats.avg_extractiveness:>8.3f} {stats.avg_suppression:>8.3f} {stats.dominant_type:>12}")

    # Type breakdown
    print("\n--- CONSTRAINT TYPE BREAKDOWN ---")
    print(f"{'Type':<15} {'Count':>6} {'Avg Ext':>8} {'Avg Sup':>8} {'Std Ext':>8} {'Std Sup':>8}")
    print("-" * 60)

    for ctype, profile in sorted(type_profiles.items(), key=lambda x: -x[1]['count']):
        print(f"{ctype:<15} {profile['count']:>6} {profile['avg_extractiveness']:>8.3f} {profile['avg_suppression']:>8.3f} {profile['std_extractiveness']:>8.3f} {profile['std_suppression']:>8.3f}")

    # Recommended defaults
    print("\n--- RECOMMENDED DEFAULTS FOR NEW DOMAINS ---")
    print("\nWhen inputting a new domain without explicit priors, use these corpus-derived defaults:")
    print()

    for cat, stats in sorted(category_stats.items(), key=lambda x: -x[1].count)[:5]:
        print(f"  {cat}:")
        print(f"    base_extractiveness: {stats.avg_extractiveness:.2f} (+/- {stats.std_extractiveness:.2f})")
        print(f"    suppression_score:   {stats.avg_suppression:.2f} (+/- {stats.std_suppression:.2f})")
        print(f"    dominant_type:       {stats.dominant_type}")
        print()

    print("=" * 70)


def save_stats_json(
    constraints: List[ConstraintData],
    category_stats: Dict[str, CategoryStats],
    type_profiles: Dict[str, Dict[str, float]],
    subcategories: Dict[str, Dict[str, float]]
):
    """Save corpus statistics to JSON for external analysis."""
    stats = {
        'summary': {
            'total_constraints': len(constraints),
            'categories': len(category_stats),
            'constraint_types': len(type_profiles)
        },
        'categories': {
            cat: {
                'count': s.count,
                'avg_extractiveness': round(s.avg_extractiveness, 4),
                'avg_suppression': round(s.avg_suppression, 4),
                'std_extractiveness': round(s.std_extractiveness, 4),
                'std_suppression': round(s.std_suppression, 4),
                'dominant_type': s.dominant_type,
                'enforcement_ratio': round(s.enforcement_ratio, 4),
                'type_distribution': dict(s.constraint_types)
            }
            for cat, s in category_stats.items()
        },
        'type_profiles': type_profiles,
        'subcategories': subcategories,
        'constraints': [
            {
                'id': c.constraint_id,
                'category': c.category,
                'type': c.constraint_type,
                'extractiveness': c.base_extractiveness,
                'suppression': c.suppression_score,
                'requires_enforcement': c.requires_enforcement,
                'emerges_naturally': c.emerges_naturally
            }
            for c in constraints
        ]
    }

    STATS_OUTPUT.parent.mkdir(exist_ok=True)
    with open(STATS_OUTPUT, 'w', encoding='utf-8') as f:
        json.dump(stats, f, indent=2)

    print(f"\nSaved detailed statistics to {STATS_OUTPUT}")


def main():
    print("Domain Priors Expander v1.0")
    print("-" * 40)

    # Load domain registry
    registry = load_domain_registry()

    # Analyze corpus
    constraints, category_stats = analyze_corpus(TESTSETS_DIR, registry)

    if not constraints:
        print("Error: No constraints found. Check TESTSETS_DIR path.")
        return

    # Compute type profiles
    type_profiles = compute_type_profiles(constraints)

    # Infer subcategories
    subcategories = infer_subcategories(constraints, category_stats)

    # Print report
    print_corpus_report(constraints, category_stats, type_profiles)

    # Generate expanded priors
    expanded_content = generate_expanded_priors(category_stats, type_profiles, subcategories)

    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        f.write(expanded_content)

    print(f"\nGenerated expanded priors: {OUTPUT_FILE}")

    # Save JSON stats
    save_stats_json(constraints, category_stats, type_profiles, subcategories)

    print("\nDone!")


if __name__ == "__main__":
    main()
