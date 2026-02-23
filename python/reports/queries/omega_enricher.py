"""Omega enricher — query and JSON functions.

Cross-references omega data with corpus metrics, orbit signatures, and
conflict map classifications to produce triaged, actionable omega reports.
"""

import sys
from collections import defaultdict

from orbit_utils import get_orbit_signature

# --- Severity ordering (inlined from conflict_map.py) ---

SEVERITY = {
    'mountain': 0,
    'rope': 1,
    'scaffold': 2,
    'piton': 3,
    'tangled_rope': 4,
    'snare': 5,
    'unknown': -1,
}


def classify_shift(type_analytical, type_powerless):
    """Classify the direction of perspectival gap."""
    sa = SEVERITY.get(type_analytical, -1)
    sp = SEVERITY.get(type_powerless, -1)

    if type_analytical == type_powerless:
        return 'consensus'

    if sa == -1 or sp == -1:
        if sa == -1 and sp == -1:
            return 'both_unknown'
        elif sa == -1:
            return 'analytical_blind'
        else:
            return 'powerless_blind'

    if sa < sp:
        if sa <= 2:
            return 'coordination_washing'
        else:
            return 'severity_amplification'
    else:
        return 'protective_framing'


# --- Configuration ---

W_EPSILON = 0.5
W_SUPPRESSION = 0.3
W_ORBIT_SPAN = 0.2

SEVERITY_THRESHOLDS = {
    'critical': 0.70,
    'high':     0.45,
    'medium':   0.25,
}

PREFIX_TO_GAP = {
    'omega_extraction_blindness_': 'snare_masked_as_rope',
    'omega_cut_safety_': 'mountain_coordination_confusion',
    'omega_learned_helplessness_': 'snare_mountain_confusion',
    'omega_perspectival_': 'general_type_mismatch',
}

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

FAMILY_DISPLAY_THRESHOLD = 5


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
    cid = omega.get('associated_constraint', 'N/A')
    if cid != 'N/A' and cid in corpus:
        return cid, corpus[cid]

    cid_from_name = extract_constraint_id(omega.get('name', ''))
    if cid_from_name in corpus:
        return cid_from_name, corpus[cid_from_name]

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
    """Composite severity score from 0.0 to 1.0."""
    eps = epsilon if epsilon is not None else 0.0
    span = orbit_span if orbit_span is not None else 0
    if suppression is None:
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

        metrics = cdata.get('metrics', {}) if cdata else {}
        epsilon = metrics.get('extractiveness')
        suppression = metrics.get('suppression')
        domain = (cdata.get('domain') or 'unknown') if cdata else 'unknown'

        orbit_sig = get_orbit_signature(orbit_data, cid)
        orbit_span = compute_orbit_span(orbit_sig)

        orbit_entry = orbit_data.get(cid, {})
        contexts = orbit_entry.get('contexts', {})
        type_institutional = contexts.get('institutional')
        type_powerless = contexts.get('powerless')

        if (type_institutional is None or type_powerless is None) and cdata:
            for cl in cdata.get('classifications', []):
                ctx = cl.get('context', '')
                if isinstance(ctx, str):
                    if 'institutional' in ctx and type_institutional is None:
                        type_institutional = cl.get('type')
                    elif 'powerless' in ctx and type_powerless is None:
                        type_powerless = cl.get('type')

        if type_institutional and type_powerless:
            gap_class = classify_shift(type_institutional, type_powerless)
        else:
            gap_class = 'unknown'

        gap_pattern = infer_gap_pattern(name)

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
            'family': None,
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


def _build_context(omega_list, corpus, orbit_data):
    """Shared computation for query and json_fn."""
    enriched = enrich_omegas(omega_list, corpus, orbit_data)
    family_index = assign_families(enriched)

    sorted_omegas = sorted(enriched, key=lambda x: -x['severity_score'])

    # Severity counts
    severity_counts = defaultdict(int)
    for e in enriched:
        severity_counts[e['severity']] += 1

    # Domain stats
    domain_counts = defaultdict(lambda: {'count': 0, 'total_score': 0.0})
    for e in enriched:
        d = e['domain']
        domain_counts[d]['count'] += 1
        domain_counts[d]['total_score'] += e['severity_score']

    # Top 5 families
    top_families = sorted(family_index.items(), key=lambda x: -x[1]['count'])[:5]

    # Top 5 domains by count
    top_domains = sorted(domain_counts.items(), key=lambda x: -x[1]['count'])[:5]

    # Domain summary sorted by mean score descending
    domain_summary = sorted(
        domain_counts.items(),
        key=lambda x: -x[1]['total_score'] / max(x[1]['count'], 1)
    )

    # Family index partitioned
    sorted_fams = sorted(family_index.items(), key=lambda x: -x[1]['count'])
    large_families = [(fid, fi) for fid, fi in sorted_fams if fi['count'] >= FAMILY_DISPLAY_THRESHOLD]
    small_families = [(fid, fi) for fid, fi in sorted_fams if fi['count'] < FAMILY_DISPLAY_THRESHOLD]

    # Omegas by severity level
    omegas_by_severity = {}
    for level in ['critical', 'high', 'medium', 'low']:
        level_omegas = [e for e in sorted_omegas if e['severity'] == level]
        if level_omegas:
            omegas_by_severity[level] = level_omegas

    return {
        'enriched': enriched,
        'family_index': family_index,
        'sorted_omegas': sorted_omegas,
        'severity_counts': dict(severity_counts),
        'top_families': top_families,
        'top_domains': top_domains,
        'domain_summary': domain_summary,
        'large_families': large_families,
        'small_families': small_families,
        'omegas_by_severity': omegas_by_severity,
        'total': len(enriched),
        'family_display_threshold': FAMILY_DISPLAY_THRESHOLD,
    }


def _load_sources(data):
    """Extract omega list, corpus dict, and orbit data from registry data sources."""
    omega_raw = data["omega"]
    if isinstance(omega_raw, dict):
        omega_list = list(omega_raw.values())
    else:
        omega_list = omega_raw

    corpus_raw = data["corpus"]
    corpus = corpus_raw.get('constraints', corpus_raw)

    from orbit_utils import load_orbit_data as _load_orbit
    orbit_data = data["orbit"]

    return omega_list, corpus, orbit_data


def query(data: dict) -> dict:
    """Omega/corpus/orbit data -> template context for enriched omega report."""
    omega_list, corpus, orbit_data = _load_sources(data)
    return _build_context(omega_list, corpus, orbit_data)


def json_fn(data: dict):
    """Omega/corpus/orbit data -> JSON-serializable enriched structure."""
    omega_list, corpus, orbit_data = _load_sources(data)

    enriched = enrich_omegas(omega_list, corpus, orbit_data)
    family_index = assign_families(enriched)

    return {
        'omegas': enriched,
        'families': family_index,
        'config': {
            'weights': {'epsilon': W_EPSILON, 'suppression': W_SUPPRESSION, 'orbit_span': W_ORBIT_SPAN},
            'thresholds': SEVERITY_THRESHOLDS,
        },
    }
