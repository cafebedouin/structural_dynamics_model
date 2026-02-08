#!/usr/bin/env python3
"""
Conflict Map Generator — Perspectival Gap Analysis by Domain

Reads corpus_data.json and fingerprint_report.md to produce a conflict map
showing where analytical and powerless perspectives diverge most, grouped
by domain and shift direction.

Distinguishes:
  - Coordination-washing: analyst sees benign (rope/scaffold), powerless sees extractive
  - Severity amplification: both see extraction, disagree on degree
  - Protective framing: analyst sees worse than powerless (rare)
  - Consensus: both perspectives agree

Usage: python3 python/conflict_map.py
"""

import json
import re
import sys
from collections import defaultdict, Counter
from pathlib import Path

# Severity ordering: lower = less extractive
SEVERITY = {
    'mountain': 0,
    'rope': 1,
    'scaffold': 2,
    'piton': 3,
    'tangled_rope': 4,
    'snare': 5,
    'unknown': -1,  # Treated specially
}

# Shift classification
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
            return 'coordination_washing'  # Benign→extractive
        else:
            return 'severity_amplification'  # Both extractive, powerless worse
    else:
        return 'protective_framing'  # Analyst sees worse (rare)


def parse_shift_patterns(fingerprint_path):
    """Parse shift(powerless, moderate, institutional, analytical) from fingerprint report."""
    patterns = {}
    current_pattern = None
    with open(fingerprint_path) as f:
        for line in f:
            m = re.match(r'### `shift\((\w+), (\w+), (\w+), (\w+)\)`', line)
            if m:
                current_pattern = {
                    'powerless': m.group(1),
                    'moderate': m.group(2),
                    'institutional': m.group(3),
                    'analytical': m.group(4),
                }
                continue
            if current_pattern and line.startswith('- `'):
                cid = line.strip().lstrip('- `').rstrip('`')
                patterns[cid] = current_pattern
    return patterns


def load_corpus(corpus_path):
    """Load corpus data with domain and classification info."""
    with open(corpus_path) as f:
        data = json.load(f)
    return data['constraints']


def extract_perspective_types(constraint_data, shift_patterns, cid):
    """Get analytical and powerless types from shift pattern or classifications.

    Returns (analytical_type, powerless_type, source) where source is:
      'fingerprint' — from engine-computed shift patterns (respects metric gates)
      'corpus_static' — from hardcoded constraint_classification/3 facts in testsets
    """
    # Prefer shift pattern (canonical 4-perspective fingerprint)
    if cid in shift_patterns:
        sp = shift_patterns[cid]
        return sp['analytical'], sp['powerless'], 'fingerprint'

    # Fallback: parse from classifications array
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


def severity_delta(t1, t2):
    """Numeric gap magnitude between two types."""
    s1 = SEVERITY.get(t1, -1)
    s2 = SEVERITY.get(t2, -1)
    if s1 == -1 or s2 == -1:
        return 0
    return abs(s2 - s1)


def main():
    base = Path(__file__).resolve().parent.parent
    corpus_path = base / 'outputs' / 'corpus_data.json'
    fingerprint_path = base / 'outputs' / 'fingerprint_report.md'
    output_path = base / 'outputs' / 'conflict_map.md'

    constraints = load_corpus(corpus_path)
    shift_patterns = parse_shift_patterns(fingerprint_path)

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
    })

    for r in results:
        d = domain_stats[r['domain']]
        d['count'] += 1
        d['shift_classes'][r['shift_class']] += 1
        d['total_delta'] += r['delta']
        d['sources'] = d.get('sources', Counter())
        d['sources'][r['source']] += 1
        if r['shift_class'] != 'consensus':
            d['shift_pairs'][r['shift_pair']] += 1
            d['constraints'].append(r)

    # === GENERATE REPORT ===
    lines = []
    lines.append('# Conflict Map: Perspectival Gap Analysis by Domain')
    lines.append('')
    lines.append('*Where do analytical and powerless perspectives diverge most?*')
    lines.append('')

    # Source tracking counts
    source_counts = Counter(r['source'] for r in results)
    n_fp = source_counts.get('fingerprint', 0)
    n_cs = source_counts.get('corpus_static', 0)

    # Global summary
    total = len(results)
    class_counts = Counter(r['shift_class'] for r in results)
    lines.append('## Global Summary')
    lines.append('')
    lines.append(f'- **Total constraints**: {total}')
    lines.append(f'- **Consensus** (same type both perspectives): {class_counts["consensus"]}')
    lines.append(f'- **Coordination-washing** (analyst sees benign, powerless sees extractive): {class_counts["coordination_washing"]}')
    lines.append(f'- **Severity amplification** (both see extraction, powerless sees worse): {class_counts["severity_amplification"]}')
    lines.append(f'- **Protective framing** (analyst sees worse than powerless): {class_counts["protective_framing"]}')
    lines.append(f'- **Analytical blind** (analyst can\'t classify, powerless can): {class_counts["analytical_blind"]}')
    lines.append(f'- **Powerless blind** (powerless can\'t classify, analyst can): {class_counts["powerless_blind"]}')
    lines.append(f'- **Both unknown**: {class_counts["both_unknown"]}')
    lines.append('')

    # Data source summary
    lines.append('## Data Source Summary')
    lines.append('')
    lines.append(f'- **Fingerprint (engine-computed)**: {n_fp} constraints — from shift patterns in fingerprint report, respects all metric gates')
    lines.append(f'- **Corpus static (testset annotations)**: {n_cs} constraints — from hardcoded `constraint_classification/3` facts in testset `.pl` files')
    lines.append('')
    lines.append('> **Note:** Corpus static entries are hand-authored narrative annotations encoding how a constraint')
    lines.append('> is *experienced* from a given perspective. They do not pass through the metric engine\'s gate logic')
    lines.append('> (e.g., snare requires epsilon >= 0.46). Low-epsilon snare classifications from this source reflect')
    lines.append('> authorial perspectival analysis, not engine-computed measurement.')
    lines.append('')

    # Shift pair frequency
    pair_counts = Counter(r['shift_pair'] for r in results if r['shift_class'] != 'consensus')
    lines.append('## Most Common Shift Pairs (excluding consensus)')
    lines.append('')
    lines.append('| Shift | Count | Class |')
    lines.append('|-------|-------|-------|')
    for pair, count in pair_counts.most_common(15):
        ta, tp = pair.split(' -> ')
        sc = classify_shift(ta, tp)
        lines.append(f'| {pair} | {count} | {sc} |')
    lines.append('')

    # Domain ranking by gap magnitude
    ranked = sorted(domain_stats.items(),
                    key=lambda x: x[1]['total_delta'] / max(x[1]['count'], 1),
                    reverse=True)

    lines.append('## Domain Rankings by Perspectival Gap')
    lines.append('')
    lines.append('Domains ranked by mean severity delta (higher = more divergent perspectives).')
    lines.append('')
    lines.append('| Rank | Domain | Constraints | Mean Gap | Coord-Wash | Sev-Amp | Consensus |')
    lines.append('|------|--------|-------------|----------|------------|---------|-----------|')
    for rank, (domain, stats) in enumerate(ranked, 1):
        n = stats['count']
        mean_gap = stats['total_delta'] / n if n > 0 else 0
        cw = stats['shift_classes']['coordination_washing']
        sa = stats['shift_classes']['severity_amplification']
        cons = stats['shift_classes']['consensus']
        lines.append(f'| {rank} | {domain} | {n} | {mean_gap:.2f} | {cw} | {sa} | {cons} |')
    lines.append('')

    # Detailed domain profiles (top 10 by gap)
    lines.append('## Domain Profiles (Top 10 by Gap)')
    lines.append('')
    for domain, stats in ranked[:10]:
        n = stats['count']
        mean_gap = stats['total_delta'] / n if n > 0 else 0
        lines.append(f'### {domain} (n={n}, mean gap={mean_gap:.2f})')
        lines.append('')

        # Shift class breakdown
        lines.append('**Shift classes:**')
        for sc, cnt in stats['shift_classes'].most_common():
            pct = cnt / n * 100
            lines.append(f'- {sc}: {cnt} ({pct:.0f}%)')
        lines.append('')

        # Top shift pairs
        if stats['shift_pairs']:
            lines.append('**Characteristic shifts:**')
            for pair, cnt in stats['shift_pairs'].most_common(5):
                lines.append(f'- {pair}: {cnt}')
            lines.append('')

        # Example constraints (up to 5 most gapped)
        gapped = sorted(stats['constraints'], key=lambda x: x['delta'], reverse=True)
        if gapped:
            lines.append('**Most divergent constraints:**')
            for c in gapped[:5]:
                src_tag = ' *' if c['source'] == 'corpus_static' else ''
                lines.append(f'- `{c["id"]}`: {c["shift_pair"]} (delta={c["delta"]}, eps={c["epsilon"]:.2f}){src_tag}')
            if any(c['source'] == 'corpus_static' for c in gapped[:5]):
                lines.append('')
                lines.append('*\\* = corpus_static source (testset annotation, not engine-computed)*')
            lines.append('')

    # Coordination-washing hotlist
    cw_constraints = [r for r in results if r['shift_class'] == 'coordination_washing']
    cw_constraints.sort(key=lambda x: x['delta'], reverse=True)
    lines.append('## Coordination-Washing Hotlist')
    lines.append('')
    lines.append('Constraints where analytical perspective sees rope/scaffold but powerless sees tangled_rope/snare.')
    lines.append('These are the strongest candidates for "extraction narrated as coordination."')
    lines.append('')
    if cw_constraints:
        lines.append('| Constraint | Domain | Analytical | Powerless | Delta | Epsilon | Source |')
        lines.append('|------------|--------|------------|-----------|-------|---------|--------|')
        for c in cw_constraints[:30]:
            lines.append(f'| {c["id"]} | {c["domain"]} | {c["type_analytical"]} | {c["type_powerless"]} | {c["delta"]} | {c["epsilon"]:.2f} | {c["source"]} |')
        n_static = sum(1 for c in cw_constraints[:30] if c['source'] == 'corpus_static')
        if n_static > 0:
            lines.append('')
            lines.append(f'> **Caveat:** {n_static} of the above entries are from `corpus_static` (testset annotations).')
            lines.append('> These encode the author\'s perspectival analysis of how a constraint is *experienced*,')
            lines.append('> not engine-computed gaps. Low-epsilon snare classifications from this source bypass metric gate logic.')
    else:
        lines.append('*None detected.*')
    lines.append('')

    # Severity amplification hotlist
    sa_constraints = [r for r in results if r['shift_class'] == 'severity_amplification']
    sa_constraints.sort(key=lambda x: x['delta'], reverse=True)
    lines.append('## Severity Amplification Hotlist')
    lines.append('')
    lines.append('Constraints where both perspectives see extraction but powerless sees it as worse.')
    lines.append('These are domains where enforcement asymmetry is highest.')
    lines.append('')
    if sa_constraints:
        lines.append('| Constraint | Domain | Analytical | Powerless | Delta | Epsilon | Source |')
        lines.append('|------------|--------|------------|-----------|-------|---------|--------|')
        for c in sa_constraints[:30]:
            lines.append(f'| {c["id"]} | {c["domain"]} | {c["type_analytical"]} | {c["type_powerless"]} | {c["delta"]} | {c["epsilon"]:.2f} | {c["source"]} |')
        n_static = sum(1 for c in sa_constraints[:30] if c['source'] == 'corpus_static')
        if n_static > 0:
            lines.append('')
            lines.append(f'> **Caveat:** {n_static} of the above entries are from `corpus_static` (testset annotations).')
    else:
        lines.append('*None detected.*')
    lines.append('')

    # === ENGINE-COMPUTED GAPS ONLY ===
    engine_results = [r for r in results if r['source'] == 'fingerprint']
    engine_total = len(engine_results)
    engine_class_counts = Counter(r['shift_class'] for r in engine_results)

    lines.append('## Engine-Computed Gaps Only')
    lines.append('')
    lines.append('This section filters to fingerprint-sourced data only (engine-computed shift patterns')
    lines.append('that respect all metric gates). Compare with the global summary above to see what')
    lines.append('testset annotations add.')
    lines.append('')

    # Engine-only global shift class counts
    lines.append('### Engine-Only Shift Class Counts')
    lines.append('')
    lines.append(f'- **Total constraints**: {engine_total}')
    lines.append(f'- **Consensus**: {engine_class_counts["consensus"]}')
    lines.append(f'- **Coordination-washing**: {engine_class_counts["coordination_washing"]}')
    lines.append(f'- **Severity amplification**: {engine_class_counts["severity_amplification"]}')
    lines.append(f'- **Protective framing**: {engine_class_counts["protective_framing"]}')
    lines.append(f'- **Analytical blind**: {engine_class_counts["analytical_blind"]}')
    lines.append(f'- **Powerless blind**: {engine_class_counts["powerless_blind"]}')
    lines.append(f'- **Both unknown**: {engine_class_counts["both_unknown"]}')
    lines.append('')

    # Engine-only domain rankings
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

    lines.append('### Engine-Only Domain Rankings')
    lines.append('')
    lines.append('| Rank | Domain | Constraints | Mean Gap | Coord-Wash | Sev-Amp | Consensus |')
    lines.append('|------|--------|-------------|----------|------------|---------|-----------|')
    for rank, (domain, stats) in enumerate(engine_ranked, 1):
        n = stats['count']
        mean_gap = stats['total_delta'] / n if n > 0 else 0
        cw = stats['shift_classes']['coordination_washing']
        sa = stats['shift_classes']['severity_amplification']
        cons = stats['shift_classes']['consensus']
        lines.append(f'| {rank} | {domain} | {n} | {mean_gap:.2f} | {cw} | {sa} | {cons} |')
    lines.append('')

    # Engine-only coordination-washing hotlist
    engine_cw = [r for r in engine_results if r['shift_class'] == 'coordination_washing']
    engine_cw.sort(key=lambda x: x['delta'], reverse=True)
    lines.append('### Engine-Only Coordination-Washing Hotlist')
    lines.append('')
    if engine_cw:
        lines.append('| Constraint | Domain | Analytical | Powerless | Delta | Epsilon |')
        lines.append('|------------|--------|------------|-----------|-------|---------|')
        for c in engine_cw[:30]:
            lines.append(f'| {c["id"]} | {c["domain"]} | {c["type_analytical"]} | {c["type_powerless"]} | {c["delta"]} | {c["epsilon"]:.2f} |')
    else:
        lines.append('*None detected.*')
    lines.append('')

    # Engine-only severity amplification hotlist
    engine_sa = [r for r in engine_results if r['shift_class'] == 'severity_amplification']
    engine_sa.sort(key=lambda x: x['delta'], reverse=True)
    lines.append('### Engine-Only Severity Amplification Hotlist')
    lines.append('')
    if engine_sa:
        lines.append('| Constraint | Domain | Analytical | Powerless | Delta | Epsilon |')
        lines.append('|------------|--------|------------|-----------|-------|---------|')
        for c in engine_sa[:30]:
            lines.append(f'| {c["id"]} | {c["domain"]} | {c["type_analytical"]} | {c["type_powerless"]} | {c["delta"]} | {c["epsilon"]:.2f} |')
    else:
        lines.append('*None detected.*')
    lines.append('')

    # Write report
    report = '\n'.join(lines)
    output_path.write_text(report)
    print(f'Conflict map generated: {output_path}')
    print(f'  Total constraints: {total}')
    print(f'    Fingerprint (engine): {n_fp}')
    print(f'    Corpus static (annotations): {n_cs}')
    print(f'  Coordination-washing: {class_counts["coordination_washing"]} (engine-only: {engine_class_counts["coordination_washing"]})')
    print(f'  Severity amplification: {class_counts["severity_amplification"]} (engine-only: {engine_class_counts["severity_amplification"]})')
    print(f'  Consensus: {class_counts["consensus"]} (engine-only: {engine_class_counts["consensus"]})')
    print(f'  Domains analyzed: {len(domain_stats)} (engine-only: {len(engine_domain_stats)})')


if __name__ == '__main__':
    main()
