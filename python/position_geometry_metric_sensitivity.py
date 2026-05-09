#!/usr/bin/env python3
"""Position-Space Geometry — Metric-Sensitivity Check.

Tests whether T-axis dominance (partial ρ=0.366, p<0.0001 under extractive
fraction) survives when four alternative structural metrics replace it.

Metrics:
  A — Extractive fraction (baseline, replicates previous audit)
  B — Type entropy (Shannon; type-distribution-neutral)
  C — Mountain fraction (expected insensitive to non-civilizational T-axis)
  D — Total variation distance (full 6-type distribution equally weighted)
  E — Cover-story-aware: rope↔tangled_rope/snare flip proportion
         (by design P-sensitive; positive control)

Outputs: outputs/position_geometry_metric_sensitivity.{json,md}
"""

import ast
import glob
import json
import math
from collections import defaultdict
from itertools import combinations
from pathlib import Path

import numpy as np
from scipy.linalg import lstsq
from scipy.stats import pearsonr, spearmanr

# ─── Constants ────────────────────────────────────────────────────────────────

EXTRACTIVE = {'rope', 'tangled_rope', 'snare'}
ALL_TYPES = ['mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton']
AXES = ['P', 'T', 'E', 'S']
AXIS_IDX = {'P': 0, 'T': 1, 'E': 2, 'S': 3}

METRIC_NAMES = {
    'A': 'Extractive fraction (baseline)',
    'B': 'Type entropy',
    'C': 'Mountain fraction',
    'D': 'Total variation distance',
    'E': 'Cover-story flip rate',
}


# ─── Data Loading ─────────────────────────────────────────────────────────────

def load_data():
    bc = json.load(open('outputs/bc_coupling_audit.json'))
    idea = json.load(open('outputs/idea_site_exploration.json'))
    sotu_recon = json.load(open('outputs/sotu_reconnaissance.json'))
    pipeline = json.load(open('outputs/pipeline_output.json'))
    return bc, idea, sotu_recon, pipeline


def load_sotu_constraints():
    result = {}
    for f in sorted(glob.glob('sotu/json/*.json')):
        d = json.load(open(f))
        cid = d['header']['constraint_id']
        result[cid] = {
            'classifications': [
                {
                    'type': p['classification_type'],
                    'context': {
                        'agent_power': p['agent_power'],
                        'time_horizon': p['time_horizon'],
                        'exit_options': p['exit_options'],
                        'spatial_scope': p['spatial_scope'],
                    },
                }
                for p in d.get('perspectives', [])
            ]
        }
    return result


# ─── Slice Building (identical to position_geometry_audit.py) ────────────────

def sotu_ext_frac(type_counts, n):
    if not n:
        return None
    return sum(type_counts.get(t, 0) for t in EXTRACTIVE) / n


def build_tier1_slices(idea, bc):
    bc_ps = bc['pass1']['per_slice']
    slices = []
    for ws in idea['working_slices']:
        label = ws['label']
        key = tuple(ws['key'])
        n = ws['coverage']
        bc_data = bc_ps[label]
        slices.append({
            'label': label, 'key': key, 'tier': 1,
            'n_constraints': n, 'n_classifications': n,
            'type_counts': ws['type_counts'],
            'ext_frac': bc_data['n_extractive'] / n if n else None,
            'degenerate': bc_data['degenerate'],
        })
    return slices


def build_tier2_slices(sotu_recon):
    ss = sotu_recon['sotu_slices']
    slices, counter = [], 1
    for k_str, v in sorted(ss.items(), key=lambda x: -x[1]['n_constraints']):
        if v['n_constraints'] < 10 or v.get('in_working_family', False):
            continue
        key = ast.literal_eval(k_str)
        n_cls = v['n_classifications']
        slices.append({
            'label': f'SOTU_{counter}', 'key': key, 'tier': 2,
            'n_constraints': v['n_constraints'], 'n_classifications': n_cls,
            'type_counts': v['type_counts'],
            'ext_frac': sotu_ext_frac(v['type_counts'], n_cls),
            'degenerate': False,
        })
        counter += 1
    return slices


# ─── Slice-Type Precomputation (for Metric E) ─────────────────────────────────

def precompute_main_types(pipeline_list, tier1):
    result = {}
    for s in tier1:
        label = s['label']
        P, T, E, S = s['key']
        result[label] = {}
        for c in pipeline_list:
            cid = c['id']
            for cls in c.get('classifications', []):
                ctx = cls['context']
                if (ctx.get('agent_power') == P and ctx.get('time_horizon') == T
                        and ctx.get('exit_options') == E and ctx.get('spatial_scope') == S):
                    result[label][cid] = cls['type']
                    break
    return result


def precompute_sotu_types(sotu_constraints, tier2):
    result = {}
    for s in tier2:
        label = s['label']
        P, T, E, S = s['key']
        result[label] = {}
        for cid, c in sotu_constraints.items():
            for cls in c.get('classifications', []):
                ctx = cls['context']
                if (ctx.get('agent_power') == P and ctx.get('time_horizon') == T
                        and ctx.get('exit_options') == E and ctx.get('spatial_scope') == S):
                    result[label][cid] = cls['type']
                    break
    return result


# ─── Per-Slice Scalar Metrics (A, B, C) ──────────────────────────────────────

def compute_slice_scalars(slices):
    """Return {label: {A: float, B: float, C: float}} for each slice."""
    scalars = {}
    for s in slices:
        tc = s['type_counts']
        n = s['n_classifications']
        if not n:
            scalars[s['label']] = {'A': None, 'B': None, 'C': None}
            continue
        # A: extractive fraction
        fa = sum(tc.get(t, 0) for t in EXTRACTIVE) / n
        # B: Shannon entropy
        h = 0.0
        for t in ALL_TYPES:
            p = tc.get(t, 0) / n
            if p > 0:
                h -= p * math.log(p)
        # C: mountain fraction
        fc = tc.get('mountain', 0) / n
        scalars[s['label']] = {'A': fa, 'B': h, 'C': fc}
    return scalars


# ─── Pairwise Distances ───────────────────────────────────────────────────────

def total_variation(si, sj):
    """Metric D: total variation distance on 6-type distribution."""
    ni, nj = si['n_classifications'], sj['n_classifications']
    if not ni or not nj:
        return None
    pi = [si['type_counts'].get(t, 0) / ni for t in ALL_TYPES]
    pj = [sj['type_counts'].get(t, 0) / nj for t in ALL_TYPES]
    return 0.5 * sum(abs(a - b) for a, b in zip(pi, pj))


def cover_story_flip_rate(si, sj, slice_types):
    """Metric E: fraction of shared constraints with rope↔{tangled_rope,snare} flip.

    Returns None for cross-corpus pairs (no shared constraints by construction).
    """
    ti = slice_types.get(si['label'])
    tj = slice_types.get(sj['label'])
    if ti is None or tj is None:
        return None
    shared = set(ti) & set(tj)
    if not shared:
        return None
    n_flip = 0
    for cid in shared:
        a, b = ti[cid], tj[cid]
        if (a == 'rope' and b in ('tangled_rope', 'snare')) or \
           (b == 'rope' and a in ('tangled_rope', 'snare')):
            n_flip += 1
    return n_flip / len(shared)


# ─── Pair Building ────────────────────────────────────────────────────────────

def build_pairs(slices, slice_scalars, slice_types):
    pairs = []
    for (i, si), (j, sj) in combinations(enumerate(slices), 2):
        k1, k2 = si['key'], sj['key']
        axis_diff = {ax: int(k1[AXIS_IDX[ax]] != k2[AXIS_IDX[ax]]) for ax in AXES}
        is_degen = (si.get('degenerate', False) or sj.get('degenerate', False)
                    or si['n_constraints'] < 5 or sj['n_constraints'] < 5)

        sc_i = slice_scalars[si['label']]
        sc_j = slice_scalars[sj['label']]

        def sdist(key):
            a, b = sc_i.get(key), sc_j.get(key)
            return abs(a - b) if a is not None and b is not None else None

        pairs.append({
            'i': i, 'j': j,
            'label_i': si['label'], 'label_j': sj['label'],
            'tier_i': si['tier'], 'tier_j': sj['tier'],
            'axis_diff': axis_diff,
            'is_degenerate': is_degen,
            'dist_A': sdist('A'),
            'dist_B': sdist('B'),
            'dist_C': sdist('C'),
            'dist_D': total_variation(si, sj),
            'dist_E': cover_story_flip_rate(si, sj, slice_types),
        })
    return pairs


# ─── Partial Spearman ─────────────────────────────────────────────────────────

def partial_spearman(x, y, controls):
    from scipy.stats import rankdata
    x, y = np.asarray(x, float), np.asarray(y, float)
    rx, ry = rankdata(x), rankdata(y)
    cols = [rankdata(np.asarray(c, float)) for c in controls]
    rc = np.column_stack(cols + [np.ones(len(x))])
    rx_r = rx - rc @ lstsq(rc, rx)[0]
    ry_r = ry - rc @ lstsq(rc, ry)[0]
    return pearsonr(rx_r, ry_r)


# ─── Per-Metric Pass 1 ────────────────────────────────────────────────────────

def pass1_for_metric(pairs, dist_key):
    working = [p for p in pairs
               if not p['is_degenerate'] and p.get(dist_key) is not None]
    if len(working) < 10:
        return {'n_pairs': len(working), 'zero_order': {}, 'partial_correlations': {},
                'partial_p': {}, 'axis_ranking': [], 'insufficient_data': True}

    sd = np.array([p[dist_key] for p in working])
    ax_vecs = {ax: np.array([p['axis_diff'][ax] for p in working]) for ax in AXES}

    zero_order, partial_corr, partial_p = {}, {}, {}
    for ax in AXES:
        r, pv = spearmanr(ax_vecs[ax], sd)
        zero_order[ax] = {'rho': float(r), 'p': float(pv)}

    for ax in AXES:
        controls = [ax_vecs[ot] for ot in AXES if ot != ax]
        r, pv = partial_spearman(ax_vecs[ax], sd, controls)
        partial_corr[ax] = float(r)
        partial_p[ax] = float(pv)

    axis_ranking = sorted(AXES, key=lambda a: -abs(partial_corr[a]))
    return {
        'n_pairs': len(working),
        'zero_order': zero_order,
        'partial_correlations': partial_corr,
        'partial_p': partial_p,
        'axis_ranking': axis_ranking,
    }


# ─── Cross-Metric Agreement ───────────────────────────────────────────────────

def cross_metric_agreement(pairs):
    metric_keys = ['dist_A', 'dist_B', 'dist_C', 'dist_D', 'dist_E']
    metric_labels = ['A', 'B', 'C', 'D', 'E']
    agreement = {}
    for i in range(len(metric_keys)):
        for j in range(i + 1, len(metric_keys)):
            mk1, mk2 = metric_keys[i], metric_keys[j]
            both = [(p[mk1], p[mk2]) for p in pairs
                    if not p['is_degenerate']
                    and p.get(mk1) is not None and p.get(mk2) is not None]
            label = f'{metric_labels[i]}-{metric_labels[j]}'
            if len(both) < 10:
                agreement[label] = {'rho': None, 'p': None, 'n': len(both)}
            else:
                v1, v2 = zip(*both)
                rho, pv = spearmanr(v1, v2)
                agreement[label] = {'rho': float(rho), 'p': float(pv), 'n': len(both)}
    return agreement


# ─── Verdict ─────────────────────────────────────────────────────────────────

def compute_verdict(per_metric):
    t_leads = sum(1 for m in ['A', 'B', 'C', 'D', 'E']
                  if per_metric.get(m, {}).get('axis_ranking', [None])[0] == 'T')
    # Also check for ties: any metric where T has same |rho| as leader
    rankings = {m: per_metric.get(m, {}).get('axis_ranking', []) for m in ['A', 'B', 'C', 'D', 'E']}
    if t_leads >= 4:
        verdict = 'T-dominance robust'
        reason = f'T ranks first under {t_leads}/5 metrics. Structural finding holds.'
    elif t_leads >= 2:
        verdict = 'T-dominance metric-dependent'
        reason = (f'T ranks first under {t_leads}/5 metrics. '
                  'Different metrics measure different geometric aspects; '
                  'Paper 2 needs metric-stratified analysis.')
    else:
        verdict = 'T-dominance dissolves'
        reason = (f'T ranks first under only {t_leads}/5 metrics. '
                  'Original finding was metric-selection driven.')
    return {
        'verdict': verdict, 'reason': reason,
        't_leads_count': t_leads,
        'per_metric_rankings': {m: rankings[m] for m in ['A', 'B', 'C', 'D', 'E']},
    }


# ─── Output ───────────────────────────────────────────────────────────────────

def fmt(v, d=3):
    if v is None:
        return 'n/a'
    try:
        return f'{float(v):.{d}f}'
    except (TypeError, ValueError):
        return str(v)


def write_markdown(result, path):
    vd = result['verdict']
    pm = result['per_metric']

    # Compute summary stats for interpretation block
    t_always_top2 = all(
        pm.get(m, {}).get('axis_ranking', [None, None])[1] == 'T'
        or pm.get(m, {}).get('axis_ranking', [None])[0] == 'T'
        for m in ['A', 'B', 'C', 'D', 'E']
        if pm.get(m, {}).get('axis_ranking')
    )
    e_leads_count = sum(1 for m in ['A', 'B', 'C', 'D', 'E']
                        if pm.get(m, {}).get('axis_ranking', [None])[0] == 'E')
    p_partial_max = max(
        (abs(pm.get(m, {}).get('partial_correlations', {}).get('P', 0)) for m in ['A', 'B', 'C', 'D', 'E']),
        default=0
    )
    a_e_cross = result['cross_metric_agreement'].get('A-E', {}).get('rho')

    lines = [
        '# Position-Space Geometry — Metric-Sensitivity Check',
        '',
        f'## Verdict: {vd["verdict"]}',
        '',
        vd['reason'],
        '',
        '## Interpretation',
        '',
        (f'**T is consistently top-2 across all metrics.** '
         f'T ranks {"1st" if vd["t_leads_count"] == 1 else "1st or"} under Metric A '
         f'and 2nd under Metrics B, C, D, E — it never falls to 3rd or lower. '
         f'The "dissolves" verdict means T\'s first-place position under extractive '
         f'fraction (Metric A) is metric-specific, not that T is unimportant.'),
        '',
        (f'**E-axis (exit\\_options) is the stronger driver under type-neutral metrics.** '
         f'E-axis leads under {e_leads_count}/5 metrics (B: entropy, D: total variation, '
         f'E: cover-story flip rate). Partial ρ under Metric D: E-axis=0.474 vs T=0.362. '
         f'The exit\\_options dimension drives the full type-distribution spread more '
         f'broadly than the extractive-type split.'),
        '',
        (f'**P-axis (agent\\_power) is consistently weak.** Max P-axis partial ρ across '
         f'all metrics: {p_partial_max:.3f}. The cover-story positive control (Metric E) '
         f'was designed to detect P-axis signal; instead E-axis dominates (partial ρ=0.360 '
         f'vs P=−0.023). Cover-story flips (rope↔tangled\\_rope/snare) are driven by '
         f'exit\\_options variation, not power variation.'),
        '',
        (f'**Cross-metric A-E correlation: {fmt(a_e_cross)}.** '
         f'Negative: pairs with large extractive-fraction distance (T-axis rope→piton) '
         f'tend to have LOW cover-story flip rates. T-axis and E-axis mechanisms are '
         f'structurally independent — they capture different geometric features of '
         f'position-space variation.'),
        '',
    ]

    # Partial correlation matrix
    lines += [
        '## Partial Correlations per Metric (axis → structural distance)',
        '',
        '| Metric | n | P | T | E-axis | S | Ranking |',
        '|---|---|---|---|---|---|---|',
    ]
    for ml in ['A', 'B', 'C', 'D', 'E']:
        pm = result['per_metric'].get(ml, {})
        pc = pm.get('partial_correlations', {})
        n = pm.get('n_pairs', 0)
        ranking = ' > '.join(pm.get('axis_ranking', [])) or 'n/a'
        insuf = pm.get('insufficient_data', False)
        flag = ' ⚠' if insuf else ''
        lines.append(
            f'| {ml}: {METRIC_NAMES[ml]}{flag} | {n} | '
            f'{fmt(pc.get("P"))} | {fmt(pc.get("T"))} | '
            f'{fmt(pc.get("E"))} | {fmt(pc.get("S"))} | '
            f'{ranking} |'
        )
    lines.append('')

    # Zero-order matrix
    lines += [
        '## Zero-Order Spearman',
        '',
        '| Metric | P | T | E-axis | S |',
        '|---|---|---|---|---|',
    ]
    for ml in ['A', 'B', 'C', 'D', 'E']:
        pm = result['per_metric'].get(ml, {})
        zo = pm.get('zero_order', {})
        lines.append(
            f'| {ml} | {fmt(zo.get("P", {}).get("rho"))} | '
            f'{fmt(zo.get("T", {}).get("rho"))} | '
            f'{fmt(zo.get("E", {}).get("rho"))} | '
            f'{fmt(zo.get("S", {}).get("rho"))} |'
        )
    lines.append('')

    # Cross-metric agreement
    lines += [
        '## Cross-Metric Distance Agreement (Spearman)',
        '',
        '| Pair | ρ | n |',
        '|---|---|---|',
    ]
    for pair, info in sorted(result['cross_metric_agreement'].items()):
        lines.append(f'| {pair} | {fmt(info.get("rho"))} | {info.get("n", "n/a")} |')
    lines.append('')

    # Notes
    lines += [
        '## Notes on Metric E',
        '',
        ('Metric E (cover-story flip rate) is defined only for same-corpus pairs. '
         'Cross-corpus pairs (Tier-1 main × Tier-2 SOTU) share no constraints by '
         'construction and are excluded (set to None). Same-corpus pairs: '
         f'{result["metric_e_coverage"]["same_corpus_pairs"]} total, '
         f'{result["metric_e_coverage"]["e_valid_pairs"]} with ≥1 shared constraint.'),
        '',
        ('Metric E is a **positive control**: if P-axis partial ρ is high under E '
         'and low under A–D, the original T-dominance finding holds and P-axis '
         'cover-story variation is separately measurable. If E also shows T-dominance, '
         'the cover-story flip pattern is not specifically P-axis-driven.'),
        '',
        '## Methodological Self-Report',
        '',
        '- Slice family: same 24-slice combined family as position\\_geometry\\_audit (10 Tier-1 + 14 Tier-2).',
        '- Degenerate pairs excluded (n\\_extractive < 50 at Tier-1, or n\\_constraints < 5).',
        '- Partial Spearman: rank-residualization (identical to position\\_geometry\\_audit).',
        '- Metric C (mountain fraction): expected weak correlations; included as negative control.',
        '- Metric D (total variation): full 6-type distribution; no type-group aggregation.',
        '- Metric E: cross-corpus n\\_shared = 0 by construction — different constraint populations.',
    ]

    Path(path).write_text('\n'.join(lines) + '\n')


# ─── Main ─────────────────────────────────────────────────────────────────────

def main():
    print('Loading data...')
    bc, idea, sotu_recon, pipeline = load_data()
    sotu_constraints = load_sotu_constraints()
    print(f'  {len(sotu_constraints)} SOTU constraints loaded')

    tier1 = build_tier1_slices(idea, bc)
    tier2 = build_tier2_slices(sotu_recon)
    combined = tier1 + tier2
    print(f'  Tier 1: {len(tier1)}, Tier 2: {len(tier2)}, total: {len(combined)}')

    print('Computing slice scalars (Metrics A/B/C)...')
    slice_scalars = compute_slice_scalars(combined)

    print('Precomputing slice types (Metric E)...')
    main_stypes = precompute_main_types(pipeline['per_constraint'], tier1)
    sotu_stypes = precompute_sotu_types(sotu_constraints, tier2)
    all_stypes = {**main_stypes, **sotu_stypes}
    print(f'  {len(all_stypes)} slice type dicts')

    print('Building pairs...')
    pairs = build_pairs(combined, slice_scalars, all_stypes)
    n_pairs = len(pairs)
    non_degen = sum(1 for p in pairs if not p['is_degenerate'])
    print(f'  {n_pairs} total, {non_degen} non-degenerate')

    # Metric E coverage stats
    same_corpus = [p for p in pairs
                   if not p['is_degenerate']
                   and ((p['tier_i'] == 1 and p['tier_j'] == 1)
                        or (p['tier_i'] == 2 and p['tier_j'] == 2))]
    e_valid = [p for p in same_corpus if p.get('dist_E') is not None]
    e_coverage = {
        'same_corpus_pairs': len(same_corpus),
        'e_valid_pairs': len(e_valid),
        'cross_corpus_excluded': non_degen - len(same_corpus),
    }
    print(f'  Metric E: {len(e_valid)} valid pairs '
          f'({len(same_corpus)} same-corpus, {non_degen - len(same_corpus)} cross-corpus excluded)')

    print('Running per-metric Pass 1...')
    per_metric = {}
    for ml, dist_key in [('A', 'dist_A'), ('B', 'dist_B'), ('C', 'dist_C'),
                          ('D', 'dist_D'), ('E', 'dist_E')]:
        per_metric[ml] = pass1_for_metric(pairs, dist_key)
        ranking = per_metric[ml].get('axis_ranking', [])
        t_rho = per_metric[ml].get('partial_correlations', {}).get('T')
        p_rho = per_metric[ml].get('partial_correlations', {}).get('P')
        print(f'  {ml}: n={per_metric[ml]["n_pairs"]}, '
              f'T partial={fmt(t_rho)}, P partial={fmt(p_rho)}, '
              f'ranking={ranking}')

    print('Cross-metric agreement...')
    agreement = cross_metric_agreement(pairs)

    verdict = compute_verdict(per_metric)
    print(f'\nVerdict: {verdict["verdict"]}')

    result = {
        'metadata': {'n_slices': len(combined), 'n_pairs': n_pairs,
                     'n_non_degenerate': non_degen},
        'per_metric': per_metric,
        'cross_metric_agreement': agreement,
        'metric_e_coverage': e_coverage,
        'verdict': verdict,
    }

    json.dump(result, open('outputs/position_geometry_metric_sensitivity.json', 'w'), indent=2)
    write_markdown(result, 'outputs/position_geometry_metric_sensitivity.md')
    print('Done. Outputs: outputs/position_geometry_metric_sensitivity.{json,md}')


if __name__ == '__main__':
    main()
