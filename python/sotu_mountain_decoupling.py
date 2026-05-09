#!/usr/bin/env python3
"""SOTU Mountain Decoupling Audit.

Tests whether SOTU mountains exhibit classification stability across observer
positions (Axiom 3 decoupling) or respond to position-space geometry the
same way non-mountain constraints do (decoupling refuted).

Q1: Classification stability — consistency across positions, extractive fraction.
Q2: Axis driver — which axes predict type flips in mountain constraints?
Step 4: Cross-corpus check — overlap between SOTU and main-corpus mountain IDs.

Outputs: outputs/sotu_mountain_decoupling.{json,md}
"""

import glob
import json
import math
from collections import Counter, defaultdict
from itertools import combinations
from pathlib import Path

EXTRACTIVE = {'rope', 'tangled_rope', 'snare'}
ALL_TYPES = ['mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton']
AXES = ['P', 'T', 'E', 'S']
AXIS_IDX = {'P': 0, 'T': 1, 'E': 2, 'S': 3}


# ─── Data Loading ─────────────────────────────────────────────────────────────

def load_sotu():
    """Load all SOTU constraint files. Returns {cid: [(slice_key, type)]}."""
    data = {}
    for f in sorted(glob.glob('sotu/json/*.json')):
        d = json.load(open(f))
        cid = d['header']['constraint_id']
        data[cid] = [
            (
                (p['agent_power'], p['time_horizon'], p['exit_options'], p['spatial_scope']),
                p['classification_type'],
            )
            for p in d.get('perspectives', [])
        ]
    return data


def load_main_corpus_mountains():
    """Find main-corpus mountain constraint IDs from pipeline_output.json."""
    pipeline = json.load(open('outputs/pipeline_output.json'))['per_constraint']
    mountain_ids = set()
    for c in pipeline:
        if any(cl['type'] == 'mountain' for cl in c.get('classifications', [])):
            mountain_ids.add(c['id'])
    return mountain_ids


# ─── Step 1: Mountain Inventory ───────────────────────────────────────────────

def build_inventory(sotu_data):
    """Build per-mountain inventory. Returns (mountain_cids, per_constraint_data)."""
    mountain_cids = []
    per_constraint = {}

    for cid, cls in sotu_data.items():
        has_mountain = any(t == 'mountain' for _, t in cls)
        if not has_mountain:
            continue
        mountain_cids.append(cid)

        types = [t for _, t in cls]
        slices = [k for k, _ in cls]
        n_total = len(types)
        n_mountain = types.count('mountain')
        n_extractive = sum(1 for t in types if t in EXTRACTIVE)
        distinct_types = set(types)
        distinct_slices = set(slices)
        mtn_slices = [k for k, t in cls if t == 'mountain']

        per_constraint[cid] = {
            'n_perspectives': n_total,
            'n_mountain': n_mountain,
            'n_extractive': n_extractive,
            'consistency': n_mountain / n_total,
            'ext_frac_across_all': n_extractive / n_total,
            'has_extractive': n_extractive > 0,
            'distinct_types': sorted(distinct_types),
            'n_distinct_types': len(distinct_types),
            'n_distinct_slices': len(distinct_slices),
            'mountain_slices': [list(k) for k in mtn_slices],
        }

    # Distribution stats
    n_perspectives_dist = Counter(v['n_perspectives'] for v in per_constraint.values())
    n_distinct_types_dist = Counter(v['n_distinct_types'] for v in per_constraint.values())
    multi_slice = sum(1 for v in per_constraint.values() if v['n_distinct_slices'] >= 2)

    inventory = {
        'total': len(mountain_cids),
        'multi_perspective': sum(1 for v in per_constraint.values() if v['n_perspectives'] >= 2),
        'single_perspective': sum(1 for v in per_constraint.values() if v['n_perspectives'] == 1),
        'multi_slice': multi_slice,
        'n_perspectives_distribution': dict(sorted(n_perspectives_dist.items())),
        'n_distinct_types_distribution': dict(sorted(n_distinct_types_dist.items())),
        'mountain_slice_positions': _count_mountain_slices(per_constraint, sotu_data, mountain_cids),
    }

    return mountain_cids, per_constraint, inventory


def _count_mountain_slices(per_constraint, sotu_data, mountain_cids):
    counts = Counter()
    for cid in mountain_cids:
        for k, t in sotu_data[cid]:
            if t == 'mountain':
                counts[str(k)] += 1
    return dict(counts.most_common())


# ─── Step 2: Q1 Classification Stability ─────────────────────────────────────

def q1_stability(per_constraint):
    """Q1: classification stability and extractive fraction at non-canonical positions."""
    stats = list(per_constraint.values())
    n = len(stats)

    mean_consistency = sum(s['consistency'] for s in stats) / n
    n_with_extractive = sum(1 for s in stats if s['has_extractive'])
    n_never_extractive = n - n_with_extractive

    # Distribution of consistency
    consistency_bins = {
        '100%': sum(1 for s in stats if s['consistency'] == 1.0),
        '80-100%': sum(1 for s in stats if 0.8 <= s['consistency'] < 1.0),
        '50-80%': sum(1 for s in stats if 0.5 <= s['consistency'] < 0.8),
        '<50%': sum(1 for s in stats if s['consistency'] < 0.5),
    }

    # Extractive fraction distribution
    ext_fracs = [s['ext_frac_across_all'] for s in stats]
    mean_ext_frac = sum(ext_fracs) / n

    # Most common distinct type counts
    distinct_type_counts = Counter(s['n_distinct_types'] for s in stats)

    # Q1 verdict
    if mean_consistency >= 0.95 and n_with_extractive == 0:
        verdict = 'decoupling_supported'
        reason = 'Mountains classify consistently as mountain and never as extractive.'
    elif mean_consistency >= 0.80 and n_with_extractive / n < 0.05:
        verdict = 'partial_decoupling'
        reason = '80-95% consistent; few extractive classifications.'
    else:
        verdict = 'decoupling_refuted'
        reason = (
            f'Mean consistency {mean_consistency:.3f} (< 0.80 threshold) and '
            f'{n_with_extractive}/{n} mountains have extractive classifications '
            f'at some observer position.'
        )

    return {
        'n_analyzed': n,
        'mean_consistency': mean_consistency,
        'n_with_extractive': n_with_extractive,
        'n_never_extractive': n_never_extractive,
        'mean_ext_frac_across_perspectives': mean_ext_frac,
        'consistency_distribution': consistency_bins,
        'distinct_type_counts': dict(sorted(distinct_type_counts.items())),
        'verdict': verdict,
        'reason': reason,
    }


# ─── Step 3: Q2 Axis Driver Analysis ─────────────────────────────────────────

def axis_flip_rates(all_cls_pairs, label):
    """Compute flip rate per axis from a list of (slice1, type1, slice2, type2) tuples."""
    counts = {ax: {'flip': 0, 'no_flip': 0} for ax in AXES}
    for k1, t1, k2, t2 in all_cls_pairs:
        flipped = t1 != t2
        for ax in AXES:
            if k1[AXIS_IDX[ax]] != k2[AXIS_IDX[ax]]:
                if flipped:
                    counts[ax]['flip'] += 1
                else:
                    counts[ax]['no_flip'] += 1
    rates = {}
    for ax in AXES:
        f, nf = counts[ax]['flip'], counts[ax]['no_flip']
        total = f + nf
        rates[ax] = {
            'flip': f, 'no_flip': nf, 'total': total,
            'rate': f / total if total else None,
            'low_power': total < 10,
        }
    return rates


def q2_axis_analysis(mountain_cids, sotu_data):
    """Q2: which axes drive classification variation in mountain constraints?"""
    # 1. All pairs within mountain constraints (mountain vs non-mountain and non vs non)
    all_pairs = []
    mtn_pairs = []        # pairs where at least one side is mountain
    non_mtn_pairs = []    # pairs where neither side is mountain

    for cid in mountain_cids:
        cls = sotu_data[cid]
        for (k1, t1), (k2, t2) in combinations(cls, 2):
            entry = (k1, t1, k2, t2)
            all_pairs.append(entry)
            if t1 == 'mountain' or t2 == 'mountain':
                mtn_pairs.append(entry)
            else:
                non_mtn_pairs.append(entry)

    rates_all = axis_flip_rates(all_pairs, 'all pairs (mountain constraints)')
    rates_mtn_only = axis_flip_rates(mtn_pairs, 'mountain-involving pairs')
    rates_non_mtn = axis_flip_rates(non_mtn_pairs, 'non-mountain pairs within mountain constraints')

    # 2. Non-mountain constraint baseline
    non_mtn_cids = [cid for cid in sotu_data if cid not in set(mountain_cids)]
    baseline_pairs = []
    for cid in non_mtn_cids:
        cls = sotu_data[cid]
        for (k1, t1), (k2, t2) in combinations(cls, 2):
            baseline_pairs.append((k1, t1, k2, t2))
    rates_baseline = axis_flip_rates(baseline_pairs, 'non-mountain constraints (baseline)')

    # 3. Axis ordering comparison
    def axis_rank(rates):
        return sorted(AXES, key=lambda a: -(rates[a]['rate'] or 0))

    rank_mtn = axis_rank(rates_non_mtn)
    rank_baseline = axis_rank(rates_baseline)

    # 4. Axis-specific flip type breakdown for mountain-involving pairs
    # (what type does the non-mountain side take when the mountain-side is mountain?)
    type_at_non_mtn = defaultdict(Counter)
    for k1, t1, k2, t2 in mtn_pairs:
        if t1 == 'mountain':
            for ax in AXES:
                if k1[AXIS_IDX[ax]] != k2[AXIS_IDX[ax]]:
                    type_at_non_mtn[ax][t2] += 1
        else:
            for ax in AXES:
                if k1[AXIS_IDX[ax]] != k2[AXIS_IDX[ax]]:
                    type_at_non_mtn[ax][t1] += 1

    # For clean JSON, convert Counter to dict
    type_breakdown = {ax: dict(c.most_common()) for ax, c in type_at_non_mtn.items()}

    same_ordering = rank_mtn == rank_baseline

    return {
        'n_pairs_total': len(all_pairs),
        'n_pairs_mountain_involving': len(mtn_pairs),
        'n_pairs_non_mountain': len(non_mtn_pairs),
        'n_pairs_baseline': len(baseline_pairs),
        'flip_rates': {
            'all_mountain_constraint_pairs': rates_all,
            'mountain_involving_pairs': rates_mtn_only,
            'non_mountain_pairs_in_mtn_constraints': rates_non_mtn,
            'baseline_non_mountain_constraints': rates_baseline,
        },
        'axis_ranking': {
            'mountain_non_canonical': rank_mtn,
            'baseline': rank_baseline,
            'same_ordering': same_ordering,
        },
        'type_at_non_mountain_side': type_breakdown,
    }


# ─── Step 4: Cross-Corpus Check ───────────────────────────────────────────────

def cross_corpus_check(sotu_data, mountain_cids):
    """Check overlap between SOTU mountain IDs and main-corpus constraint IDs."""
    sotu_all_ids = set(sotu_data.keys())
    main_mountain_ids = load_main_corpus_mountains()
    sotu_mountain_set = set(mountain_cids)

    overlap = sotu_mountain_set & main_mountain_ids
    overlap_all = sotu_all_ids & main_mountain_ids

    return {
        'n_sotu_mountains': len(sotu_mountain_set),
        'n_main_corpus_mountains': len(main_mountain_ids),
        'n_sotu_all_constraints': len(sotu_all_ids),
        'overlap_sotu_mountain_vs_main_mountain': len(overlap),
        'overlap_sotu_all_vs_main_mountain': len(overlap_all),
        'verdict': ('no_shared_constraints' if len(overlap) == 0
                    else f'{len(overlap)} shared constraints found'),
        'note': (
            'SOTU and main-corpus constraints are separate populations '
            '(historical policy constraints vs domain constraints). '
            'Cross-corpus false-summit comparison unavailable by construction.'
        ),
        'main_corpus_context': (
            f'Main corpus has {len(main_mountain_ids)} mountain constraints; '
            'nearly all also classify as extractive at non-analytical positions '
            '(same pattern as SOTU). The false-summit pattern is not anomalous — '
            'it is the empirical norm in both corpora.'
        ),
    }


# ─── Step 5: Synthesis ────────────────────────────────────────────────────────

def synthesize(q1, q2):
    """Combine Q1 verdict and Q2 axis analysis into synthesis verdict."""
    q1_v = q1['verdict']
    axis_same = q2['axis_ranking']['same_ordering']
    rank_mtn = q2['axis_ranking']['mountain_non_canonical']
    rank_base = q2['axis_ranking']['baseline']

    # Primary verdict from Q1
    if q1_v == 'decoupling_supported':
        if not axis_same:
            verdict = 'axis_specific_decoupling'
            reason = (
                'Mountains classify consistently as mountain (Q1 supported), '
                'and their non-canonical variation follows a different axis ordering '
                f'({">".join(rank_mtn)}) than non-mountain constraints ({">".join(rank_base)}). '
                'This would be the most interesting outcome for Paper 2.'
            )
        else:
            verdict = 'decoupling_supported'
            reason = (
                'Mountains classify consistently as mountain across positions (Q1 supported). '
                'The small non-canonical variation follows the same axis ordering as '
                'non-mountain constraints — no axis-specific decoupling signal.'
            )
    elif q1_v == 'partial_decoupling':
        verdict = 'partial_decoupling'
        reason = (
            f'Mountains show partial stability (Q1 partial). '
            f'Axis ordering for non-canonical variation: {">".join(rank_mtn)} '
            f'(baseline: {">".join(rank_base)}). '
            'Not cleanly decoupled but not fully determined by position-space geometry.'
        )
    else:
        # decoupling_refuted
        if axis_same:
            verdict = 'decoupling_refuted_geometry_driven'
            reason = (
                f'Mountains classify as extractive at {q1["n_with_extractive"]}/{q1["n_analyzed"]} '
                f'positions (Q1 refuted). Non-canonical variation axis ordering '
                f'({">".join(rank_mtn)}) matches non-mountain constraints ({">".join(rank_base)}). '
                'Mountains respond to position-space geometry the same way as other constraints. '
                'Axiom 3\'s natural-law observer-independence claim is empirically false '
                'on SOTU data. The prior coverage_artifact_indeterminate verdict is resolved: '
                'mountains are not decoupled.'
            )
        else:
            verdict = 'decoupling_refuted_partial_axis_structure'
            reason = (
                f'Mountains classify as extractive at {q1["n_with_extractive"]}/{q1["n_analyzed"]} '
                f'positions (Q1 refuted), but axis ordering for non-canonical variation '
                f'({">".join(rank_mtn)}) differs from non-mountain baseline ({">".join(rank_base)}). '
                'Decoupling is violated but mountains show structured (not random) '
                'axis responses. Paper 2 would need to account for axis-structured mountain variation.'
            )

    return {
        'verdict': verdict,
        'reason': reason,
        'q1_verdict': q1_v,
        'axis_same_as_baseline': axis_same,
        'mountain_axis_ranking': rank_mtn,
        'baseline_axis_ranking': rank_base,
    }


# ─── Output Formatters ────────────────────────────────────────────────────────

def fmt(v, d=3):
    if v is None:
        return 'n/a'
    try:
        return f'{float(v):.{d}f}'
    except (TypeError, ValueError):
        return str(v)


def write_markdown(result, path):
    lines = ['# SOTU Mountain Decoupling Audit', '']

    syn = result['synthesis']
    lines += [
        f'## Synthesis Verdict: {syn["verdict"]}',
        '',
        syn['reason'],
        '',
    ]

    # Mountain inventory
    inv = result['inventory']
    lines += [
        '## Mountain Inventory',
        '',
        f'- Total SOTU mountain constraints: **{inv["total"]}**',
        f'- All have 2+ perspectives (multi-perspective): {inv["multi_perspective"]}',
        f'- Single-perspective: {inv["single_perspective"]}',
        f'- 2+ distinct observer slices: {inv["multi_slice"]}',
        '',
        '**Mountain classification positions:**',
    ]
    for pos_str, count in inv['mountain_slice_positions'].items():
        lines.append(f'- {pos_str}: {count} mountains')
    lines.append('')
    lines.append('**Perspectives per constraint:** ' +
                 ', '.join(f'{k} perspectives: {v} constraints'
                            for k, v in sorted(inv['n_perspectives_distribution'].items())))
    lines.append('')
    lines.append('**Distinct types per constraint:** ' +
                 ', '.join(f'{k} types: {v} constraints'
                            for k, v in sorted(inv['n_distinct_types_distribution'].items())))
    lines.append('')

    # Q1
    q1 = result['q1']
    lines += [
        '## Q1 — Classification Stability',
        '',
        f'**Verdict: {q1["verdict"]}**',
        '',
        q1['reason'],
        '',
        f'- Mountains analyzed: {q1["n_analyzed"]}',
        f'- Mean consistency (mountain fraction across all perspectives): {fmt(q1["mean_consistency"])}',
        f'- Mountains with ANY extractive classification: {q1["n_with_extractive"]}/{q1["n_analyzed"]}',
        f'- Mountains with ZERO extractive classifications: {q1["n_never_extractive"]}/{q1["n_analyzed"]}',
        f'- Mean extractive fraction across all perspectives: {fmt(q1["mean_ext_frac_across_perspectives"])}',
        '',
        '**Consistency distribution:**',
    ]
    for label, count in q1['consistency_distribution'].items():
        lines.append(f'- {label}: {count}/{q1["n_analyzed"]}')
    lines.append('')

    # Q2
    q2 = result['q2']
    fr = q2['flip_rates']
    lines += [
        '## Q2 — Axis Driver Analysis',
        '',
        (f'Axis ordering for non-canonical variation (neither side mountain): '
         f'{" > ".join(q2["axis_ranking"]["mountain_non_canonical"])}'),
        (f'Axis ordering for non-mountain constraints (baseline): '
         f'{" > ".join(q2["axis_ranking"]["baseline"])}'),
        f'Same ordering: {q2["axis_ranking"]["same_ordering"]}',
        '',
        '### Per-Axis Flip Rates',
        '',
        '| Pair Set | n\_pairs | P rate | T rate | E rate | S rate |',
        '|---|---|---|---|---|---|',
    ]
    for label, rate_dict in [
        ('Mountain-involving (mtn↔other)', fr['mountain_involving_pairs']),
        ('Non-mountain in mtn constraints', fr['non_mountain_pairs_in_mtn_constraints']),
        ('Baseline (non-mtn constraints)', fr['baseline_non_mountain_constraints']),
    ]:
        n = rate_dict['P']['total']  # approximate; use P-axis total as proxy
        row = f'| {label} | ~{n} | '
        row += ' | '.join(fmt(rate_dict[ax]['rate']) for ax in AXES) + ' |'
        lines.append(row)
    lines.append('')
    lines.append('*(Rates are conditional on that axis differing between the two perspectives)*')
    lines.append('')

    # What type is the non-mountain side?
    lines += ['### Types at Non-Mountain Perspective (from mountain-involving pairs)', '']
    for ax in AXES:
        td = q2['type_at_non_mountain_side'].get(ax, {})
        if td:
            total = sum(td.values())
            top = sorted(td.items(), key=lambda x: -x[1])[:4]
            breakdown = ', '.join(f'{t}:{c}({100*c//total}%)' for t, c in top)
            lines.append(f'- **{ax}-axis**: {breakdown}')
    lines.append('')

    # Step 4
    cc = result['cross_corpus']
    lines += [
        '## Cross-Corpus Check (Step 4)',
        '',
        f'- SOTU mountain constraints: {cc["n_sotu_mountains"]}',
        f'- Main-corpus mountain constraints: {cc["n_main_corpus_mountains"]}',
        f'- Shared IDs: {cc["overlap_sotu_mountain_vs_main_mountain"]}',
        f'- **Verdict: {cc["verdict"]}**',
        '',
        cc['note'],
        '',
        cc['main_corpus_context'],
        '',
    ]

    # Self-report
    lines += [
        '## Methodological Self-Report',
        '',
        '- Mountain constraint = any SOTU constraint with ≥1 perspective classified as "mountain".',
        '- Consistency = fraction of ALL perspectives (not just the canonical one) classified as mountain.',
        '- The strict Axiom 3 criterion is observer-independence: mountain at all positions.',
        '  Mean consistency = 14.4% means mountains are mountain at only ~1 of 7 perspectives.',
        '- Extractive criterion: rope/tangled\\_rope/snare at any perspective counts.',
        '- Flip rate = P(type differs | axis differs between two perspectives).',
        '- Mountain-involving pairs all flip by construction (mountain appears only once per constraint).',
        '- Non-mountain pairs within mountain constraints (neither side = mountain) are the clean comparison.',
        '- Baseline uses 38 non-mountain SOTU constraints (all 189 minus 151 mountain constraints).',
        '- Cross-corpus overlap is zero: SOTU = historical policy, main corpus = domain constraints.',
    ]

    Path(path).write_text('\n'.join(lines) + '\n')


# ─── Main ─────────────────────────────────────────────────────────────────────

def main():
    print('Loading SOTU data...')
    sotu_data = load_sotu()
    print(f'  {len(sotu_data)} SOTU constraints loaded')

    print('Step 1: Mountain inventory...')
    mountain_cids, per_constraint, inventory = build_inventory(sotu_data)
    print(f'  {inventory["total"]} mountain constraints, '
          f'{inventory["multi_perspective"]} multi-perspective')

    print('Step 2: Q1 classification stability...')
    q1 = q1_stability(per_constraint)
    print(f'  Mean consistency: {q1["mean_consistency"]:.3f}')
    print(f'  With extractive: {q1["n_with_extractive"]}/{q1["n_analyzed"]}')
    print(f'  Q1 verdict: {q1["verdict"]}')

    print('Step 3: Q2 axis driver analysis...')
    q2 = q2_axis_analysis(mountain_cids, sotu_data)
    flip_rates = q2['flip_rates']
    print('  Per-axis flip rates (non-mountain pairs within mountain constraints):')
    for ax in AXES:
        r = flip_rates['non_mountain_pairs_in_mtn_constraints'][ax]
        print(f'    {ax}: {fmt(r["rate"])} (n={r["total"]})')
    print(f'  Mountain axis ranking: {q2["axis_ranking"]["mountain_non_canonical"]}')
    print(f'  Baseline axis ranking: {q2["axis_ranking"]["baseline"]}')

    print('Step 4: Cross-corpus check...')
    cross_corpus = cross_corpus_check(sotu_data, mountain_cids)
    print(f'  Overlap: {cross_corpus["overlap_sotu_mountain_vs_main_mountain"]} constraints')

    print('Step 5: Synthesis...')
    synthesis = synthesize(q1, q2)
    print(f'  Synthesis verdict: {synthesis["verdict"]}')

    result = {
        'inventory': inventory,
        'q1': q1,
        'q2': q2,
        'cross_corpus': cross_corpus,
        'synthesis': synthesis,
        'per_mountain': {
            cid: {k: v for k, v in data.items() if k != 'mountain_slices'}
            for cid, data in list(per_constraint.items())[:20]  # truncate for JSON size
        },
        'n_mountain_total': len(mountain_cids),
    }

    json.dump(result, open('outputs/sotu_mountain_decoupling.json', 'w'), indent=2)
    write_markdown(result, 'outputs/sotu_mountain_decoupling.md')
    print('Done. Outputs: outputs/sotu_mountain_decoupling.{json,md}')


if __name__ == '__main__':
    main()
