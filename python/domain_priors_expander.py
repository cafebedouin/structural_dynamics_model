#!/usr/bin/env python3
"""
Domain Priors Expander
Analyzes the testset corpus and generates:
  1. prolog/domain_priors_expanded.pl - Corpus-derived default priors
  2. outputs/domain_corpus_stats.json  - Raw statistics for meta-reporting

Thresholds aligned with config.pl / core.md.
"""

import os
import re
import json
import math
from collections import defaultdict
from pathlib import Path

BASE_DIR = Path("/home/scott/bin/structural_dynamics_model/prolog")
TESTSETS_DIR = BASE_DIR / 'testsets'
DOMAIN_REGISTRY = BASE_DIR / 'domain_registry.pl'
OUTPUT_FILE = BASE_DIR / 'domain_priors_expanded.pl'
STATS_OUTPUT = Path("/home/scott/bin/structural_dynamics_model/outputs/domain_corpus_stats.json")

# Thresholds aligned with config.pl
ROPE_EXTRACTION_CEILING = 0.15
SNARE_EXTRACTION_FLOOR = 0.46

# Regex patterns
EXTRACTIVENESS_REGEX = re.compile(
    r"base_extractiveness\s*\(\s*(?:\s*)?['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([0-9.]+)\s*\)"
)
SUPPRESSION_REGEX = re.compile(
    r"suppression_score\s*\(\s*(?:\s*)?['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([0-9.]+)\s*\)"
)
CLAIM_REGEX = re.compile(
    r"(?:\w+:)?constraint_claim\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,\s*([a-zA-Z0-9_]+)"
)


def load_registry():
    """Load domain_category/2 facts from domain_registry.pl."""
    registry = {}
    if not DOMAIN_REGISTRY.exists():
        return registry
    pattern = re.compile(r"domain_category\(([a-zA-Z0-9_]+),\s*([a-zA-Z0-9_]+)\)\.")
    for line in DOMAIN_REGISTRY.read_text(encoding='utf-8').splitlines():
        m = pattern.match(line)
        if m:
            registry[m.group(1)] = m.group(2)
    return registry


def analyze_corpus():
    """Extract extractiveness and suppression values from all testset .pl files."""
    stats = defaultdict(lambda: {"extractiveness": [], "suppression": [], "claims": []})

    for pl_file in TESTSETS_DIR.glob("*.pl"):
        content = pl_file.read_text(encoding='utf-8')

        e_matches = EXTRACTIVENESS_REGEX.findall(content)
        s_matches = SUPPRESSION_REGEX.findall(content)
        c_matches = CLAIM_REGEX.findall(content)

        for name, val in e_matches:
            stats[name]["extractiveness"].append(float(val))
        for name, val in s_matches:
            stats[name]["suppression"].append(float(val))
        for name, ctype in c_matches:
            stats[name]["claims"].append(ctype)

    return stats


def compute_profiles(corpus_data, registry):
    """Compute category and type profiles from corpus data."""

    # Group by registry category
    category_data = defaultdict(lambda: {"ext": [], "sup": []})
    type_data = defaultdict(lambda: {"ext": [], "sup": []})
    subcat_data = defaultdict(lambda: {"ext": [], "sup": [], "parent": None, "type": None})

    for name, data in corpus_data.items():
        ext_vals = data["extractiveness"]
        sup_vals = data["suppression"]
        if not ext_vals:
            continue

        avg_ext = sum(ext_vals) / len(ext_vals)
        avg_sup = sum(sup_vals) / len(sup_vals) if sup_vals else 0.5

        # Category from registry
        cat = registry.get(name, "unknown_novel")
        category_data[cat]["ext"].append(avg_ext)
        category_data[cat]["sup"].append(avg_sup)

        # Type from claims or inferred from score
        claims = data["claims"]
        if claims:
            ctype = claims[0]
        elif avg_ext <= ROPE_EXTRACTION_CEILING:
            ctype = "rope"
        elif avg_ext < SNARE_EXTRACTION_FLOOR:
            ctype = "tangled_rope"
        else:
            ctype = "snare"

        type_data[ctype]["ext"].append(avg_ext)
        type_data[ctype]["sup"].append(avg_sup)

        subcat_key = f"{cat}_{ctype}"
        subcat_data[subcat_key]["ext"].append(avg_ext)
        subcat_data[subcat_key]["sup"].append(avg_sup)
        subcat_data[subcat_key]["parent"] = cat
        subcat_data[subcat_key]["type"] = ctype

    return category_data, type_data, subcat_data


def stddev(values):
    if len(values) < 2:
        return 0.0
    mean = sum(values) / len(values)
    variance = sum((v - mean) ** 2 for v in values) / (len(values) - 1)
    return math.sqrt(variance)


def dominant_type(ext_avg):
    if ext_avg <= ROPE_EXTRACTION_CEILING:
        return "rope"
    elif ext_avg < SNARE_EXTRACTION_FLOOR:
        return "tangled_rope"
    else:
        return "snare"


def generate_prolog(category_data, type_data, subcat_data, total_constraints):
    """Generate domain_priors_expanded.pl from computed profiles."""
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
    lines.append(f" * Generated from {total_constraints} constraints")
    lines.append(f" * Categories analyzed: {len(category_data)}")
    lines.append(f" * Thresholds: Rope <= {ROPE_EXTRACTION_CEILING}, Snare >= {SNARE_EXTRACTION_FLOOR}")
    lines.append(" */")
    lines.append("")

    # 1. Category corpus profiles
    lines.append("%% ============================================================================")
    lines.append("%% 1. CATEGORY CORPUS PROFILES (From Corpus Averages)")
    lines.append("%% ============================================================================")
    lines.append("%% Format: category_corpus_profile(Category, [AvgExtract, AvgSuppress, StdExtract, StdSuppress, Count]).")
    lines.append("")

    for cat in sorted(category_data.keys()):
        d = category_data[cat]
        ext = d["ext"]
        sup = d["sup"]
        n = len(ext)
        avg_e = round(sum(ext) / n, 3) if n else 0
        avg_s = round(sum(sup) / n, 3) if n else 0
        std_e = round(stddev(ext), 3)
        std_s = round(stddev(sup), 3)
        dom = dominant_type(avg_e)
        enf_ratio = round(avg_s / avg_e, 2) if avg_e > 0 else 0
        lines.append(f"category_corpus_profile({cat}, [{avg_e}, {avg_s}, {std_e}, {std_s}, {n}]).  % dominant: {dom}, enforcement_ratio: {enf_ratio}")

    lines.append("")

    # 2. Type corpus profiles
    lines.append("%% ============================================================================")
    lines.append("%% 2. CONSTRAINT TYPE PROFILES")
    lines.append("%% ============================================================================")
    lines.append("%% Format: type_corpus_profile(Type, [AvgExtract, AvgSuppress, StdExtract, StdSuppress, Count]).")
    lines.append("")

    for tp in sorted(type_data.keys()):
        d = type_data[tp]
        ext = d["ext"]
        sup = d["sup"]
        n = len(ext)
        avg_e = round(sum(ext) / n, 3) if n else 0
        avg_s = round(sum(sup) / n, 3) if n else 0
        std_e = round(stddev(ext), 3)
        std_s = round(stddev(sup), 3)
        lines.append(f"type_corpus_profile({tp}, [{avg_e}, {avg_s}, {std_e}, {std_s}, {n}]).")

    lines.append("")

    # 3. Subcategory profiles
    lines.append("%% ============================================================================")
    lines.append("%% 3. SUBCATEGORY PROFILES (Category + Type combinations)")
    lines.append("%% ============================================================================")
    lines.append("%% Format: subcategory_profile(SubcatName, [Parent, Type, AvgExtract, AvgSuppress, Count]).")
    lines.append("")

    for sk in sorted(subcat_data.keys(), key=lambda k: -len(subcat_data[k]["ext"])):
        d = subcat_data[sk]
        ext = d["ext"]
        sup = d["sup"]
        n = len(ext)
        if n == 0:
            continue
        avg_e = round(sum(ext) / n, 3)
        avg_s = round(sum(sup) / n, 3)
        lines.append(f"subcategory_profile({sk}, ['{d['parent']}', '{d['type']}', {avg_e}, {avg_s}, {n}]).")

    lines.append("")

    # 4. Default value inference predicates
    lines.append("%% ============================================================================")
    lines.append("%% 4. DEFAULT VALUE INFERENCE PREDICATES")
    lines.append("%% ============================================================================")
    lines.append("")
    lines.append("%% default_extractiveness(+Category, -Value)")
    lines.append("default_extractiveness(Category, Value) :-")
    lines.append("    category_corpus_profile(Category, [Value|_]), !.")
    lines.append("default_extractiveness(_, 0.5).")
    lines.append("")
    lines.append("%% default_suppression(+Category, -Value)")
    lines.append("default_suppression(Category, Value) :-")
    lines.append("    category_corpus_profile(Category, [_, Value|_]), !.")
    lines.append("default_suppression(_, 0.5).")
    lines.append("")
    lines.append("%% default_resistance(+Category, -Value)")
    lines.append("default_resistance(Category, Value) :-")
    lines.append("    default_extractiveness(Category, Ext),")
    lines.append("    Value is max(0.1, min(0.9, 1.0 - Ext * 0.5)), !.")
    lines.append("default_resistance(_, 0.5).")
    lines.append("")
    lines.append("%% infer_category_defaults(+Category, -Extractiveness, -Suppression, -Resistance)")
    lines.append("infer_category_defaults(Category, Ext, Sup, Res) :-")
    lines.append("    default_extractiveness(Category, Ext),")
    lines.append("    default_suppression(Category, Sup),")
    lines.append("    default_resistance(Category, Res).")
    lines.append("")
    lines.append("%% get_corpus_prior(+ID, +Metric, -Value)")
    lines.append("get_corpus_prior(ID, extractiveness, Value) :-")
    lines.append("    narrative_ontology:constraint_claim(ID, Type),")
    lines.append("    type_corpus_profile(Type, [Value|_]), !.")
    lines.append("get_corpus_prior(ID, suppression, Value) :-")
    lines.append("    narrative_ontology:constraint_claim(ID, Type),")
    lines.append("    type_corpus_profile(Type, [_, Value|_]), !.")
    lines.append("get_corpus_prior(_, _, 0.5).")
    lines.append("")
    lines.append("%% ============================================================================")
    lines.append("%% END OF AUTO-GENERATED PRIORS")
    lines.append("%% ============================================================================")

    return "\n".join(lines) + "\n"


def main():
    print("Domain Priors Expander")
    print("-" * 45)

    if not TESTSETS_DIR.exists():
        print(f"Error: Could not find testsets at {TESTSETS_DIR}")
        return

    # Load registry for category grouping
    registry = load_registry()
    print(f"Registry loaded: {len(registry)} entries")

    # Analyze corpus
    corpus_data = analyze_corpus()
    total_constraints = len(corpus_data)
    print(f"Corpus analyzed: {total_constraints} constraints")

    # Save raw stats as JSON
    STATS_OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    # Convert defaultdict to regular dict for JSON serialization
    json_data = {k: {"extractiveness": v["extractiveness"], "suppression": v["suppression"]}
                 for k, v in corpus_data.items()}
    with open(STATS_OUTPUT, 'w') as f:
        json.dump(json_data, f, indent=2)
    print(f"Stats saved to {STATS_OUTPUT}")

    # Compute profiles
    category_data, type_data, subcat_data = compute_profiles(corpus_data, registry)

    # Generate Prolog output
    prolog_content = generate_prolog(category_data, type_data, subcat_data, total_constraints)
    OUTPUT_FILE.write_text(prolog_content, encoding='utf-8')
    print(f"Expanded priors generated at {OUTPUT_FILE}")
    print(f"  Categories: {len(category_data)}")
    print(f"  Types: {len(type_data)}")
    print(f"  Subcategories: {len(subcat_data)}")


if __name__ == "__main__":
    main()
