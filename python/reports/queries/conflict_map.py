"""Conflict map — query function.

Reads fingerprint JSON + corpus data to produce perspectival gap analysis
by domain. Replaces direct markdown parsing with JSON sidecar consumption.
"""

from collections import defaultdict, Counter

# Severity ordering: lower = less extractive
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


def severity_delta(t1, t2):
    """Numeric gap magnitude between two types."""
    s1 = SEVERITY.get(t1, -1)
    s2 = SEVERITY.get(t2, -1)
    if s1 == -1 or s2 == -1:
        return 0
    return abs(s2 - s1)


def extract_perspective_types(constraint_data, shift_patterns, cid):
    """Get analytical and powerless types from shift pattern or classifications.

    Returns (analytical_type, powerless_type, source) where source is:
      'fingerprint' -- from engine-computed shift patterns (respects metric gates)
      'corpus_static' -- from hardcoded constraint_classification/3 facts in testsets
    """
    if cid in shift_patterns:
        sp = shift_patterns[cid]
        return sp['analytical'], sp['powerless'], 'fingerprint'

    analytical_type = None
    powerless_type = None
    for cl in constraint_data.get('classifications', []):
        ctx = cl.get('context', '')
        if not isinstance(ctx, str):
            continue
        if 'analytical' in ctx:
            analytical_type = cl['type']
        elif 'powerless' in ctx:
            powerless_type = cl['type']
    return analytical_type, powerless_type, 'corpus_static'


def query(data: dict) -> dict:
    """Loaded data -> template context for conflict map."""
    fingerprint = data["fingerprint"]
    corpus_raw = data["corpus"]

    # Build shift_patterns: cid -> {analytical, powerless, moderate, institutional}
    shift_patterns = {}
    for fam in fingerprint["shift_families"]:
        comp = fam["components"]
        if not comp:
            continue
        for cid in fam["members"]:
            shift_patterns[cid] = comp

    constraints = corpus_raw.get('constraints', {})

    # Collect per-constraint analysis
    results = []
    for cid, cdata in constraints.items():
        domain = cdata.get('domain', 'unknown') or 'unknown'
        eps = cdata.get('metrics', {}).get('extractiveness', 0) or 0

        type_a, type_p, source = extract_perspective_types(cdata, shift_patterns, cid)
        if type_a is None and type_p is None:
            continue

        type_a = type_a or 'unknown'
        type_p = type_p or 'unknown'

        shift_class = classify_shift(type_a, type_p)
        delta = severity_delta(type_a, type_p)

        results.append({
            'id': cid,
            'domain': domain,
            'type_analytical': type_a,
            'type_powerless': type_p,
            'shift_class': shift_class,
            'delta': delta,
            'epsilon': eps,
            'shift_pair': f'{type_a} -> {type_p}',
            'source': source,
        })

    # === DOMAIN AGGREGATION ===
    domain_stats = defaultdict(lambda: {
        'count': 0,
        'shifts': Counter(),
        'shift_classes': Counter(),
        'total_delta': 0,
        'shift_pairs': Counter(),
        'constraints': [],
        'sources': Counter(),
    })

    for r in results:
        d = domain_stats[r['domain']]
        d['count'] += 1
        d['shift_classes'][r['shift_class']] += 1
        d['total_delta'] += r['delta']
        d['sources'][r['source']] += 1
        if r['shift_class'] != 'consensus':
            d['shift_pairs'][r['shift_pair']] += 1
            d['constraints'].append(r)

    source_counts = Counter(r['source'] for r in results)
    class_counts = Counter(r['shift_class'] for r in results)
    pair_counts = Counter(r['shift_pair'] for r in results if r['shift_class'] != 'consensus')

    # Precompute pair table rows (avoids passing classify_shift to template)
    pair_table = []
    for pair, count in pair_counts.most_common(15):
        ta, tp = pair.split(' -> ')
        sc = classify_shift(ta, tp)
        pair_table.append({'pair': pair, 'count': count, 'shift_class': sc})

    ranked = sorted(domain_stats.items(),
                    key=lambda x: x[1]['total_delta'] / max(x[1]['count'], 1),
                    reverse=True)

    cw_constraints = sorted(
        [r for r in results if r['shift_class'] == 'coordination_washing'],
        key=lambda x: x['delta'], reverse=True)
    sa_constraints = sorted(
        [r for r in results if r['shift_class'] == 'severity_amplification'],
        key=lambda x: x['delta'], reverse=True)

    # === ENGINE-ONLY STATS ===
    engine_results = [r for r in results if r['source'] == 'fingerprint']
    engine_class_counts = Counter(r['shift_class'] for r in engine_results)

    engine_domain_stats = defaultdict(lambda: {
        'count': 0,
        'shift_classes': Counter(),
        'total_delta': 0,
        'constraints': [],
    })
    for r in engine_results:
        d = engine_domain_stats[r['domain']]
        d['count'] += 1
        d['shift_classes'][r['shift_class']] += 1
        d['total_delta'] += r['delta']
        if r['shift_class'] != 'consensus':
            d['constraints'].append(r)

    engine_ranked = sorted(engine_domain_stats.items(),
                           key=lambda x: x[1]['total_delta'] / max(x[1]['count'], 1),
                           reverse=True)

    engine_cw = sorted(
        [r for r in engine_results if r['shift_class'] == 'coordination_washing'],
        key=lambda x: x['delta'], reverse=True)
    engine_sa = sorted(
        [r for r in engine_results if r['shift_class'] == 'severity_amplification'],
        key=lambda x: x['delta'], reverse=True)

    return {
        'results': results,
        'domain_stats': dict(domain_stats),
        'ranked': ranked,
        'class_counts': class_counts,
        'source_counts': source_counts,
        'pair_counts': pair_counts,
        'pair_table': pair_table,
        'engine_results': engine_results,
        'engine_class_counts': engine_class_counts,
        'engine_ranked': engine_ranked,
        'cw_constraints': cw_constraints,
        'sa_constraints': sa_constraints,
        'engine_cw': engine_cw,
        'engine_sa': engine_sa,
    }
