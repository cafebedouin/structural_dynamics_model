#!/usr/bin/env python3
"""
Omega Enricher — Cross-references omega data with corpus metrics,
orbit signatures, and conflict map classifications to produce
triaged, actionable omega reports.

Inputs:
  outputs/omega_data.json    — parsed omegas from omega_reporter.py
  outputs/corpus_data.json   — constraint metrics, domain, classifications
  outputs/orbit_data.json    — orbit signatures and per-context types

Outputs:
  outputs/enriched_omega_report.md   — enriched markdown report
  outputs/enriched_omega_data.json   — machine-readable enriched data

Usage: python3 python/omega_enricher.py
"""

import json
import sys
from collections import defaultdict
from pathlib import Path

# Import shared utilities
sys.path.insert(0, str(Path(__file__).parent))
from orbit_utils import load_orbit_data, get_orbit_signature
# --- Severity ordering (inlined from conflict_map.py) ---

SEVERITY = {
    'mountain': 0,
    'rope': 1,
    'scaffold': 2,
    'piton': 3,
    'tangled_rope': 4,
    'snare': 5,
    'unknown': -1,  # Treated specially
}


def classify_shift(type_analytical, type_powerless):
    """Classify the direction of perspectival gap."""
    sa = SEVERITY.get(type_analytical, -1)
    sp = SEVERITY.get(type_powerless, -1)

    if type_analytical == type_powerless:
        return 'consensus'

    # Unknown from either side is a classification gap, not a shift
    if sa == -1 or sp == -1:
        if sa == -1 and sp == -1:
            return 'both_unknown'
        elif sa == -1:
            return 'analytical_blind'  # Analyst can't classify, powerless can
        else:
            return 'powerless_blind'   # Powerless can't classify, analyst can

    # Both have real types
    if sa < sp:
        # Analyst sees lighter, powerless sees heavier
        if sa <= 2:  # rope, scaffold, mountain
            return 'coordination_washing'  # Benign->extractive
        else:
            return 'severity_amplification'  # Both extractive, powerless worse
    else:
        return 'protective_framing'  # Analyst sees worse (rare)


# --- Configuration ---

# Composite severity score weights
W_EPSILON = 0.5      # extraction intensity
W_SUPPRESSION = 0.3  # enforcement intensity
W_ORBIT_SPAN = 0.2   # classification instability

# Severity bucket thresholds (calibrate after first run)
SEVERITY_THRESHOLDS = {
    'critical': 0.70,
    'high':     0.45,
    'medium':   0.25,
    # below 0.25 -> low
}

# Omega name prefix -> gap pattern mapping
PREFIX_TO_GAP = {
    'omega_extraction_blindness_': 'snare_masked_as_rope',
    'omega_cut_safety_': 'mountain_coordination_confusion',
    'omega_learned_helplessness_': 'snare_mountain_confusion',
    'omega_perspectival_': 'general_type_mismatch',
}

# Resolution strategies: gap_class -> domain -> strategy text
RESOLUTION_STRATEGIES = {
    'coordination_washing': {
        'default': 'Map beneficiary flows. Interview affected populations to verify coordination claim. Test: who controls change mechanisms? If control is asymmetric, extraction is likely.',
        'political': 'Trace legislative intent vs. distributive outcome. Compare who advocated for the constraint with who benefits.',
        'economic': 'Audit extraction pathways through financial instruments. Calculate rent vs coordination value.',
        'legal': 'Compare statutory purpose with enforcement pattern. Test selective enforcement hypothesis.',
        'technological': 'Examine platform architecture for asymmetric control surfaces. Test whether coordination features mask rent extraction.',
    },
    'severity_amplification': {
        'default': 'Quantify extraction differential across power levels. Both perspectives agree extraction exists — the question is degree. Collect suppression and exit metrics for validation.',
        'political': 'Measure policy capture indicators. Compare formal vs effective access to reform mechanisms.',
        'economic': 'Calculate rent extraction vs coordination value. Map who bears enforcement costs vs who captures surplus.',
    },
    'protective_framing': {
        'default': 'Investigate why analyst sees worse than affected population. Possible causes: information asymmetry, normalization of extraction, or genuine coordination value invisible to outsiders.',
    },
    'analytical_blind': {
        'default': 'Analyst cannot classify but affected population can. Collect ground-truth from affected populations. Test whether analytical abstraction obscures lived experience.',
    },
    'powerless_blind': {
        'default': 'Affected population cannot classify. Test whether opacity serves extraction (deliberate complexity) or coordination (legitimate technical barrier). Measure information asymmetry.',
    },
    'consensus': {
        'default': 'Both perspectives agree on classification. Verify via independent measurement if the omega was generated from a different perspective pair.',
    },
    'both_unknown': {
        'default': 'Neither perspective can classify. Collect baseline metrics: suppression_requirement, resistance_to_change, base_extractiveness. Re-run classification after data collection.',
    },
}

# --- Paths ---

BASE = Path(__file__).resolve().parent.parent
OMEGA_JSON = BASE / 'outputs' / 'omega_data.json'
CORPUS_JSON = BASE / 'outputs' / 'corpus_data.json'
ORBIT_JSON = BASE / 'outputs' / 'orbit_data.json'
OUTPUT_MD = BASE / 'outputs' / 'enriched_omega_report.md'
OUTPUT_JSON = BASE / 'outputs' / 'enriched_omega_data.json'


from shared.loader import load_json


def infer_gap_pattern(omega_name):
    """Infer gap pattern from omega name prefix."""
    for prefix, pattern in PREFIX_TO_GAP.items():
        if omega_name.startswith(prefix):
            return pattern
    return 'unknown'


def extract_constraint_id(omega_name):
    """Extract constraint ID by stripping known omega name prefix."""
    for prefix in PREFIX_TO_GAP:
        if omega_name.startswith(prefix):
            return omega_name[len(prefix):]
    return omega_name


def resolve_constraint(omega, corpus):
    """Resolve constraint ID using associated_constraint field, then fallback to name prefix.

    Returns (constraint_id, constraint_data_dict_or_None).
    """
    # Primary: use associated_constraint from omega_reporter
    cid = omega.get('associated_constraint', 'N/A')
    if cid != 'N/A' and cid in corpus:
        return cid, corpus[cid]

    # Fallback: strip prefix from omega name
    cid_from_name = extract_constraint_id(omega.get('name', ''))
    if cid_from_name in corpus:
        return cid_from_name, corpus[cid_from_name]

    # Second fallback: associated_constraint may be filename, try it even if not in corpus
    if cid != 'N/A':
        return cid, None

    return cid_from_name, None


def compute_orbit_span(orbit_signature):
    """Compute ordinal distance between min and max types in orbit signature."""
    if not orbit_signature:
        return 0
    vals = [SEVERITY.get(t, -1) for t in orbit_signature]
    vals = [v for v in vals if v >= 0]
    if len(vals) < 2:
        return 0
    return max(vals) - min(vals)


def compute_severity_score(epsilon, suppression, orbit_span):
    """Composite severity score from 0.0 to 1.0.

    These weights are initial heuristics. Calibrate after first run using
    the distribution summary printed to stdout.
    """
    eps = epsilon if epsilon is not None else 0.0
    span = orbit_span if orbit_span is not None else 0
    if suppression is None:
        # Renormalized two-factor: drop suppression, reweight epsilon + orbit_span
        return (0.714 * eps) + (0.286 * (span / 5.0))
    else:
        return (W_EPSILON * eps) + (W_SUPPRESSION * suppression) + (W_ORBIT_SPAN * (span / 5.0))


def score_to_severity(score):
    """Map composite score to severity label."""
    if score >= SEVERITY_THRESHOLDS['critical']:
        return 'critical'
    elif score >= SEVERITY_THRESHOLDS['high']:
        return 'high'
    elif score >= SEVERITY_THRESHOLDS['medium']:
        return 'medium'
    else:
        return 'low'


def get_resolution_strategy(gap_class, domain):
    """Look up resolution strategy by gap_class and domain."""
    strategies = RESOLUTION_STRATEGIES.get(gap_class, RESOLUTION_STRATEGIES.get('consensus', {}))
    return strategies.get(domain, strategies.get('default', 'No resolution strategy defined for this gap class.'))


def enrich_omegas(omega_list, corpus, orbit_data):
    """Enrich each omega with metrics, orbit data, severity, gap class, and family."""
    enriched = []
    unresolved = []

    for omega in omega_list:
        name = omega.get('name', 'N/A')
        cid, cdata = resolve_constraint(omega, corpus)

        # Metrics from corpus
        metrics = cdata.get('metrics', {}) if cdata else {}
        epsilon = metrics.get('extractiveness')
        suppression = metrics.get('suppression')
        domain = (cdata.get('domain') or 'unknown') if cdata else 'unknown'

        # Orbit data
        orbit_sig = get_orbit_signature(orbit_data, cid)
        orbit_span = compute_orbit_span(orbit_sig)

        # Get per-context types for gap_class computation
        # Omegas are generated from powerless vs institutional gaps
        # (see report_generator.pl detect_gap_pattern/2), so use those
        # perspectives for gap_class, not analytical vs powerless.
        orbit_entry = orbit_data.get(cid, {})
        contexts = orbit_entry.get('contexts', {})
        type_institutional = contexts.get('institutional')
        type_powerless = contexts.get('powerless')

        # Fallback: extract from corpus_data classifications if orbit lookup failed
        if (type_institutional is None or type_powerless is None) and cdata:
            for cl in cdata.get('classifications', []):
                ctx = cl.get('context', '')
                if isinstance(ctx, str):
                    if 'institutional' in ctx and type_institutional is None:
                        type_institutional = cl.get('type')
                    elif 'powerless' in ctx and type_powerless is None:
                        type_powerless = cl.get('type')

        # Compute gap_class using institutional perspective as the
        # "lighter" view (replaces analytical in classify_shift's logic)
        if type_institutional and type_powerless:
            gap_class = classify_shift(type_institutional, type_powerless)
        else:
            gap_class = 'unknown'

        # Gap pattern from name
        gap_pattern = infer_gap_pattern(name)

        # Severity
        score = compute_severity_score(epsilon, suppression, orbit_span)
        severity = score_to_severity(score)

        entry = {
            'name': name,
            'severity': severity,
            'severity_score': round(score, 4),
            'associated_constraint': cid,
            'domain': domain,
            'orbit_signature': orbit_sig,
            'orbit_span': orbit_span,
            'gap_class': gap_class,
            'gap_pattern': gap_pattern,
            'source_gap': omega.get('source_gap', 'N/A'),
            'epsilon': epsilon,
            'suppression': suppression,
            'question': omega.get('question', 'N/A'),
            'resolution_strategy': get_resolution_strategy(gap_class, domain),
            'family': None,  # assigned below
        }
        enriched.append(entry)

        if cdata is None:
            unresolved.append(cid)

    if unresolved:
        unique_unresolved = sorted(set(unresolved))
        print(f"Warning: {len(unique_unresolved)} constraints not found in corpus_data.json", file=sys.stderr)
        if len(unique_unresolved) <= 10:
            for u in unique_unresolved:
                print(f"  - {u}", file=sys.stderr)

    return enriched


def assign_families(enriched):
    """Assign family IDs based on (orbit_signature, gap_class, domain) triples.

    Families sorted by size descending, IDs assigned F001, F002, ...
    """
    family_map = defaultdict(list)
    for entry in enriched:
        sig = tuple(entry['orbit_signature']) if entry['orbit_signature'] else ('unknown',)
        key = (sig, entry['gap_class'], entry['domain'])
        family_map[key].append(entry)

    # Sort families by size descending
    sorted_families = sorted(family_map.items(), key=lambda x: -len(x[1]))

    family_index = {}
    for i, (key, members) in enumerate(sorted_families, 1):
        fid = f"F{i:03d}"
        family_index[fid] = {
            'orbit_signature': list(key[0]),
            'gap_class': key[1],
            'domain': key[2],
            'count': len(members),
            'members': [m['associated_constraint'] for m in members],
        }
        for m in members:
            m['family'] = fid

    return family_index


def print_distribution(enriched):
    """Print severity distribution and score percentiles for calibration."""
    scores = sorted(e['severity_score'] for e in enriched)
    n = len(scores)
    if n == 0:
        print("No omegas to analyze.")
        return

    counts = defaultdict(int)
    for e in enriched:
        counts[e['severity']] += 1

    print(f"\nSeverity Distribution ({n} omegas):")
    for level in ['critical', 'high', 'medium', 'low']:
        c = counts.get(level, 0)
        pct = 100 * c / n
        print(f"  {level:8s}: {c:4d} ({pct:5.1f}%)")

    def percentile(data, p):
        idx = int(len(data) * p / 100)
        return data[min(idx, len(data) - 1)]

    print(f"\n  Score percentiles: "
          f"p25={percentile(scores, 25):.3f}, "
          f"p50={percentile(scores, 50):.3f}, "
          f"p75={percentile(scores, 75):.3f}, "
          f"p90={percentile(scores, 90):.3f}")
    print(f"  Score range: [{scores[0]:.3f}, {scores[-1]:.3f}]")


def generate_enriched_report(enriched, family_index, output_path):
    """Generate the enriched omega markdown report."""
    # Sort by severity score descending
    sorted_omegas = sorted(enriched, key=lambda x: -x['severity_score'])

    # Gather domain stats
    domain_counts = defaultdict(lambda: {'count': 0, 'total_score': 0.0})
    for e in enriched:
        d = e['domain']
        domain_counts[d]['count'] += 1
        domain_counts[d]['total_score'] += e['severity_score']

    with open(output_path, 'w', encoding='utf-8') as f:
        # --- Executive Summary ---
        f.write("# Enriched Omega Epistemological Gap Report\n\n")
        f.write(f"**Total Omegas:** {len(enriched)}\n\n")

        # Severity distribution
        counts = defaultdict(int)
        for e in enriched:
            counts[e['severity']] += 1
        f.write("## Severity Distribution\n\n")
        f.write("| Severity | Count | % |\n")
        f.write("|----------|------:|--:|\n")
        for level in ['critical', 'high', 'medium', 'low']:
            c = counts.get(level, 0)
            pct = 100 * c / len(enriched) if enriched else 0
            f.write(f"| {level} | {c} | {pct:.1f}% |\n")
        f.write("\n")

        # Top 5 families
        top_families = sorted(family_index.items(), key=lambda x: -x[1]['count'])[:5]
        f.write("## Top 5 Families\n\n")
        f.write("| Family | Orbit Signature | Gap Class | Domain | Count |\n")
        f.write("|--------|----------------|-----------|--------|------:|\n")
        for fid, finfo in top_families:
            sig = ', '.join(finfo['orbit_signature'])
            dom = finfo.get('domain', 'unknown')
            f.write(f"| {fid} | [{sig}] | {finfo['gap_class']} | {dom} | {finfo['count']} |\n")
        f.write("\n")

        # Top 5 domains
        top_domains = sorted(domain_counts.items(), key=lambda x: -x[1]['count'])[:5]
        f.write("## Top 5 Domains by Omega Count\n\n")
        f.write("| Domain | Count | Mean Score |\n")
        f.write("|--------|------:|-----------:|\n")
        for d, info in top_domains:
            mean = info['total_score'] / info['count'] if info['count'] > 0 else 0
            f.write(f"| {d} | {info['count']} | {mean:.3f} |\n")
        f.write("\n---\n\n")

        # --- Omega entries by severity ---
        for level in ['critical', 'high', 'medium', 'low']:
            level_omegas = [e for e in sorted_omegas if e['severity'] == level]
            if not level_omegas:
                continue
            f.write(f"## {level.upper()} ({len(level_omegas)})\n\n")
            for i, e in enumerate(level_omegas, 1):
                sig_str = ', '.join(e['orbit_signature']) if e['orbit_signature'] else 'N/A'
                eps_str = f"{e['epsilon']:.2f}" if e['epsilon'] is not None else 'N/A'
                supp_str = f"{e['suppression']:.2f}" if e['suppression'] is not None else 'N/A'
                fam = e.get('family', 'N/A')
                fam_count = family_index.get(fam, {}).get('count', '?')

                f.write(f"### `{e['name']}` [{level.upper()}]\n\n")
                f.write(f"- **Severity:** `{e['severity']}` (score: {e['severity_score']:.3f})\n")
                f.write(f"- **Constraint:** `{e['associated_constraint']}`\n")
                f.write(f"- **Domain:** `{e['domain']}`\n")
                f.write(f"- **Orbit Signature:** `[{sig_str}]`\n")
                f.write(f"- **Orbit Span:** `{e['orbit_span']}`\n")
                f.write(f"- **Gap Class:** `{e['gap_class']}`\n")
                f.write(f"- **Gap Pattern:** `{e['gap_pattern']}`\n")
                f.write(f"- **Source Gap:** `{e['source_gap']}`\n")
                f.write(f"- **Epsilon:** `{eps_str}`\n")
                f.write(f"- **Suppression:** `{supp_str}`\n")
                f.write(f"- **Question:** {e['question']}\n")
                f.write(f"- **Resolution Strategy:** {e['resolution_strategy']}\n")
                f.write(f"- **Family:** `{fam}` ({fam_count} members)\n\n")
                f.write("---\n\n")

        # --- Family Index (≥5 members) ---
        FAMILY_DISPLAY_THRESHOLD = 5
        sorted_fams = sorted(family_index.items(), key=lambda x: -x[1]['count'])
        display_fams = [(fid, fi) for fid, fi in sorted_fams if fi['count'] >= FAMILY_DISPLAY_THRESHOLD]
        small_fams = [(fid, fi) for fid, fi in sorted_fams if fi['count'] < FAMILY_DISPLAY_THRESHOLD]

        f.write(f"## Family Index ({len(display_fams)} families with {FAMILY_DISPLAY_THRESHOLD}+ members)\n\n")
        for fid, finfo in display_fams:
            sig_str = ', '.join(finfo['orbit_signature'])
            dom = finfo.get('domain', 'unknown')
            f.write(f"### {fid}: [{sig_str}] / {finfo['gap_class']} / {dom} ({finfo['count']} members)\n\n")
            for m in sorted(finfo['members']):
                f.write(f"- `{m}`\n")
            f.write("\n")

        # --- Small families appendix (collapsed summary) ---
        if small_fams:
            small_total = sum(fi['count'] for _, fi in small_fams)
            f.write(f"## Appendix: Small Families ({len(small_fams)} families, {small_total} omegas, <{FAMILY_DISPLAY_THRESHOLD} members each)\n\n")
            f.write("| Family | Orbit Signature | Gap Class | Domain | Count |\n")
            f.write("|--------|----------------|-----------|--------|------:|\n")
            for fid, finfo in small_fams:
                sig = ', '.join(finfo['orbit_signature'])
                dom = finfo.get('domain', 'unknown')
                f.write(f"| {fid} | [{sig}] | {finfo['gap_class']} | {dom} | {finfo['count']} |\n")
            f.write("\n*Full member lists for small families available in `enriched_omega_data.json`.*\n\n")

        # --- Domain Summary ---
        f.write("## Domain Summary\n\n")
        f.write("| Domain | Count | Mean Score |\n")
        f.write("|--------|------:|-----------:|\n")
        for d, info in sorted(domain_counts.items(), key=lambda x: -x[1]['total_score'] / max(x[1]['count'], 1)):
            mean = info['total_score'] / info['count'] if info['count'] > 0 else 0
            f.write(f"| {d} | {info['count']} | {mean:.3f} |\n")
        f.write("\n")


def main():
    print("Loading omega data...")
    omega_list = load_json(OMEGA_JSON)
    if not omega_list:
        print("No omega data found. Run omega_reporter.py first.", file=sys.stderr)
        sys.exit(1)
    if isinstance(omega_list, dict):
        omega_list = list(omega_list.values())

    print(f"Loaded {len(omega_list)} omegas.")

    print("Loading corpus data...")
    corpus_raw = load_json(CORPUS_JSON)
    corpus = corpus_raw.get('constraints', corpus_raw)
    print(f"Loaded {len(corpus)} constraints from corpus.")

    print("Loading orbit data...")
    orbit_data = load_orbit_data(str(ORBIT_JSON))
    print(f"Loaded {len(orbit_data)} orbit entries.")

    print("Enriching omegas...")
    enriched = enrich_omegas(omega_list, corpus, orbit_data)

    print("Assigning families...")
    family_index = assign_families(enriched)
    print(f"Created {len(family_index)} families.")

    # Print calibration summary
    print_distribution(enriched)

    # Check for large families
    large_families = [(fid, f) for fid, f in family_index.items() if f['count'] > 50]
    if large_families:
        print(f"\nNote: {len(large_families)} families have >50 members. "
              "Consider domain subdivision if triage granularity is insufficient:")
        for fid, f in sorted(large_families, key=lambda x: -x[1]['count']):
            sig = ', '.join(f['orbit_signature'])
            dom = f.get('domain', 'unknown')
            print(f"  {fid}: [{sig}] / {f['gap_class']} / {dom} — {f['count']} members")

    # Write outputs
    print(f"\nWriting enriched report to {OUTPUT_MD}...")
    generate_enriched_report(enriched, family_index, OUTPUT_MD)

    print(f"Writing enriched JSON to {OUTPUT_JSON}...")
    output_data = {
        'omegas': enriched,
        'families': family_index,
        'config': {
            'weights': {'epsilon': W_EPSILON, 'suppression': W_SUPPRESSION, 'orbit_span': W_ORBIT_SPAN},
            'thresholds': SEVERITY_THRESHOLDS,
        },
    }
    with open(OUTPUT_JSON, 'w', encoding='utf-8') as f:
        json.dump(output_data, f, indent=2)

    print("Done.")


if __name__ == '__main__':
    main()
