#!/usr/bin/env python3
"""Implementation-Derived Positional Structure Audit.

Three audits:
  1. Metric F (E-weighted Hamming [1,1,2,1]) vs. unweighted Hamming as
     positional-distance predictor of structural metrics A-E. Analogue of
     bc_coupling_audit.py's Pass 2 methodology applied to all five structural
     metrics.

  2. Hub-separated predictors: hub1_diff (P or S differ) vs hub2_diff (T or E
     differ) as independent partial Spearman predictors of structural metrics
     A-E. Tests whether Hub 1 and Hub 2 variation capture statistically
     independent structural variance.

  3. Hub-2-spanning partition: split pairs into Hub-2-spanning (different
     effective_immutability output) vs Hub-2-internal (same output). Compare
     per-axis partial Spearman within each subset. Key question: is T-axis
     dominance under Metric A concentrated in Hub-2-spanning pairs?

Outputs: outputs/metric_audit_results.{json,md}
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

# Effective immutability table (from constraint_indexing.pl lines 191-223).
# Key: (T, E). Value: 'mountain', 'rope', or 'dual' (civilizational/analytical).
EFFECTIVE_IMMUTABILITY = {
    # immediate
    ('immediate', 'trapped'):         'mountain',
    ('immediate', 'identity_locked'): 'mountain',
    ('immediate', 'constrained'):     'mountain',
    ('immediate', 'mobile'):          'rope',
    ('immediate', 'arbitrage'):       'rope',
    ('immediate', 'analytical'):      'rope',
    # biographical
    ('biographical', 'trapped'):         'mountain',
    ('biographical', 'identity_locked'): 'rope',
    ('biographical', 'constrained'):     'mountain',
    ('biographical', 'mobile'):          'rope',
    ('biographical', 'arbitrage'):       'rope',
    ('biographical', 'analytical'):      'rope',
    # generational
    ('generational', 'trapped'):         'mountain',
    ('generational', 'identity_locked'): 'rope',
    ('generational', 'constrained'):     'rope',
    ('generational', 'mobile'):          'rope',
    ('generational', 'arbitrage'):       'rope',
    ('generational', 'analytical'):      'rope',
    # historical (all rope)
    ('historical', 'trapped'):         'rope',
    ('historical', 'identity_locked'): 'rope',
    ('historical', 'constrained'):     'rope',
    ('historical', 'mobile'):          'rope',
    ('historical', 'arbitrage'):       'rope',
    ('historical', 'analytical'):      'rope',
    # civilizational
    ('civilizational', 'analytical'):      'dual',   # both mountain and rope by design
    ('civilizational', 'trapped'):         'rope',
    ('civilizational', 'identity_locked'): 'rope',
    ('civilizational', 'constrained'):     'rope',
    ('civilizational', 'mobile'):          'rope',
    ('civilizational', 'arbitrage'):       'rope',
}


def hub2_output(t, e):
    """Return effective immutability output for (T, E) pair."""
    return EFFECTIVE_IMMUTABILITY.get((t, e), 'rope')  # default rope if missing


def is_spanning(t1, e1, t2, e2):
    """True if pair spans the Hub-2 mountain/rope boundary.

    Dual slices (civilizational/analytical) count as spanning in all pairs,
    because they can produce either mountain or rope.
    """
    o1 = hub2_output(t1, e1)
    o2 = hub2_output(t2, e2)
    if o1 == 'dual' or o2 == 'dual':
        return True
    return o1 != o2


# ─── Data Loading (identical to position_geometry_metric_sensitivity.py) ──────

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


# ─── Slice Building (identical to position_geometry_metric_sensitivity.py) ────

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


# ─── Slice Types for Metric E ─────────────────────────────────────────────────

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
    scalars = {}
    for s in slices:
        tc = s['type_counts']
        n = s['n_classifications']
        if not n:
            scalars[s['label']] = {'A': None, 'B': None, 'C': None}
            continue
        fa = sum(tc.get(t, 0) for t in EXTRACTIVE) / n
        h = 0.0
        for t in ALL_TYPES:
            p = tc.get(t, 0) / n
            if p > 0:
                h -= p * math.log(p)
        fc = tc.get('mountain', 0) / n
        scalars[s['label']] = {'A': fa, 'B': h, 'C': fc}
    return scalars


# ─── Pairwise Distances ───────────────────────────────────────────────────────

def total_variation(si, sj):
    ni, nj = si['n_classifications'], sj['n_classifications']
    if not ni or not nj:
        return None
    pi = [si['type_counts'].get(t, 0) / ni for t in ALL_TYPES]
    pj = [sj['type_counts'].get(t, 0) / nj for t in ALL_TYPES]
    return 0.5 * sum(abs(a - b) for a, b in zip(pi, pj))


def cover_story_flip_rate(si, sj, slice_types):
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
        P1, T1, E1, S1 = k1
        P2, T2, E2, S2 = k2
        axis_diff = {
            'P': int(P1 != P2),
            'T': int(T1 != T2),
            'E': int(E1 != E2),
            'S': int(S1 != S2),
        }
        is_degen = (si.get('degenerate', False) or sj.get('degenerate', False)
                    or si['n_constraints'] < 5 or sj['n_constraints'] < 5)

        hamming = sum(axis_diff.values())
        metric_f = (axis_diff['P'] + axis_diff['T']
                    + 2 * axis_diff['E'] + axis_diff['S'])
        hub1_diff = int(bool(axis_diff['P'] or axis_diff['S']))
        hub2_diff = int(bool(axis_diff['T'] or axis_diff['E']))
        spanning = is_spanning(T1, E1, T2, E2)

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
            'hamming': hamming,
            'metric_f': metric_f,
            'hub1_diff': hub1_diff,
            'hub2_diff': hub2_diff,
            'hub2_spanning': spanning,
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


# ─── Audit 1: Metric F vs Hamming ────────────────────────────────────────────

def audit1_metric_f(pairs):
    """Compare zero-order Spearman(Hamming, structural_dist) vs
    Spearman(metric_F, structural_dist) for each structural metric A-E.
    Also checks collinearity of Hamming and metric_F.
    """
    non_degen = [p for p in pairs if not p['is_degenerate']]

    # Collinearity check
    ham = [p['hamming'] for p in non_degen]
    mf = [p['metric_f'] for p in non_degen]
    col_rho, col_p = spearmanr(ham, mf)

    results = {'hamming_f_collinearity': {'rho': float(col_rho), 'p': float(col_p),
                                           'n': len(non_degen)}}

    for ml in ['A', 'B', 'C', 'D', 'E']:
        dist_key = f'dist_{ml}'
        working = [p for p in non_degen if p.get(dist_key) is not None]
        if len(working) < 10:
            results[ml] = {'n': len(working), 'insufficient': True}
            continue
        sd = [p[dist_key] for p in working]
        ham_w = [p['hamming'] for p in working]
        mf_w = [p['metric_f'] for p in working]
        ham_rho, ham_p = spearmanr(ham_w, sd)
        f_rho, f_p = spearmanr(mf_w, sd)
        results[ml] = {
            'n': len(working),
            'hamming_rho': float(ham_rho),
            'hamming_p': float(ham_p),
            'f_rho': float(f_rho),
            'f_p': float(f_p),
            'diff': float(f_rho - ham_rho),
        }
    return results


# ─── Audit 2: Hub-Separated Predictors ───────────────────────────────────────

def audit2_hub_separation(pairs):
    """Partial Spearman of hub1_diff and hub2_diff against each structural metric,
    controlling for each other.
    """
    non_degen = [p for p in pairs if not p['is_degenerate']]
    h1 = [p['hub1_diff'] for p in non_degen]
    h2 = [p['hub2_diff'] for p in non_degen]
    col_rho, col_p = spearmanr(h1, h2)
    results = {
        'hub_collinearity': {'rho': float(col_rho), 'p': float(col_p),
                             'n': len(non_degen),
                             'uninterpretable': abs(col_rho) > 0.7},
    }

    for ml in ['A', 'B', 'C', 'D', 'E']:
        dist_key = f'dist_{ml}'
        working = [p for p in non_degen if p.get(dist_key) is not None]
        if len(working) < 10:
            results[ml] = {'n': len(working), 'insufficient': True}
            continue
        sd = np.array([p[dist_key] for p in working])
        h1_w = np.array([p['hub1_diff'] for p in working], float)
        h2_w = np.array([p['hub2_diff'] for p in working], float)

        # Zero-order
        zo_h1, zo_h1_p = spearmanr(h1_w, sd)
        zo_h2, zo_h2_p = spearmanr(h2_w, sd)

        # Partial: hub1 controlling for hub2, and vice versa
        p_h1, p_h1_pv = partial_spearman(h1_w, sd, [h2_w])
        p_h2, p_h2_pv = partial_spearman(h2_w, sd, [h1_w])

        results[ml] = {
            'n': len(working),
            'hub1_zero_rho': float(zo_h1), 'hub1_zero_p': float(zo_h1_p),
            'hub2_zero_rho': float(zo_h2), 'hub2_zero_p': float(zo_h2_p),
            'hub1_partial_rho': float(p_h1), 'hub1_partial_p': float(p_h1_pv),
            'hub2_partial_rho': float(p_h2), 'hub2_partial_p': float(p_h2_pv),
            'hub_diff': float(abs(p_h2) - abs(p_h1)),  # positive = hub2 stronger
        }
    return results


# ─── Audit 3: Hub-2-Spanning Partition ───────────────────────────────────────

def audit3_spanning_partition(pairs):
    """Partition non-degenerate pairs into Hub-2-spanning vs Hub-2-internal.
    Within each subset, compute per-axis partial Spearman for each structural metric.
    """
    non_degen = [p for p in pairs if not p['is_degenerate']]
    spanning = [p for p in non_degen if p['hub2_spanning']]
    internal = [p for p in non_degen if not p['hub2_spanning']]

    def per_axis_partials(subset, ml):
        dist_key = f'dist_{ml}'
        working = [p for p in subset if p.get(dist_key) is not None]
        if len(working) < 6:
            return {'n': len(working), 'insufficient': True}
        sd = np.array([p[dist_key] for p in working])
        ax_vecs = {ax: np.array([p['axis_diff'][ax] for p in working], float)
                   for ax in AXES}
        zero_order, partial_corr, partial_p = {}, {}, {}
        for ax in AXES:
            r, pv = spearmanr(ax_vecs[ax], sd)
            zero_order[ax] = float(r)
        for ax in AXES:
            controls = [ax_vecs[ot] for ot in AXES if ot != ax]
            r, pv = partial_spearman(ax_vecs[ax], sd, controls)
            partial_corr[ax] = float(r)
            partial_p[ax] = float(pv)
        axis_ranking = sorted(AXES, key=lambda a: -abs(partial_corr[a]))
        return {
            'n': len(working),
            'zero_order': zero_order,
            'partial_corr': partial_corr,
            'partial_p': partial_p,
            'axis_ranking': axis_ranking,
        }

    results = {
        'n_spanning': len(spanning),
        'n_internal': len(internal),
        'n_total_non_degen': len(non_degen),
        'spanning_fraction': len(spanning) / len(non_degen) if non_degen else 0,
    }

    for ml in ['A', 'B', 'C', 'D', 'E']:
        results[f'spanning_{ml}'] = per_axis_partials(spanning, ml)
        results[f'internal_{ml}'] = per_axis_partials(internal, ml)

    # Key comparison: T partial rho under Metric A, spanning vs internal
    sp_a = results.get('spanning_A', {}).get('partial_corr', {}).get('T')
    in_a = results.get('internal_A', {}).get('partial_corr', {}).get('T')
    if sp_a is not None and in_a is not None:
        diff = sp_a - in_a
        if abs(diff) >= 0.15:
            verdict = 'recast_warranted'
        elif abs(diff) >= 0.07:
            verdict = 'suggestive'
        else:
            verdict = 'negative'
        results['t_dominance_metric_a'] = {
            'spanning_T_rho': float(sp_a),
            'internal_T_rho': float(in_a),
            'diff': float(diff),
            'verdict': verdict,
        }
    else:
        results['t_dominance_metric_a'] = {'verdict': 'insufficient_data'}

    return results


# ─── Verdict ─────────────────────────────────────────────────────────────────

def compute_verdict(a1, a2, a3):
    # Audit 1 verdict
    metrics = ['A', 'B', 'C', 'D', 'E']
    a1_diffs = [a1[m]['diff'] for m in metrics if m in a1 and not a1[m].get('insufficient')]
    consistent_positive = sum(1 for d in a1_diffs if d > 0.05)
    consistent_negative = sum(1 for d in a1_diffs if d < -0.05)
    a1_verdict = (consistent_positive >= 3 or consistent_negative >= 3)

    # Audit 2 verdict
    col = a2['hub_collinearity']['rho']
    if abs(col) > 0.7:
        a2_verdict = False
        a2_reason = f'hub1/hub2 collinear (ρ={col:.3f}); results uninterpretable'
    else:
        hub_diffs = [a2[m]['hub_diff'] for m in metrics
                     if m in a2 and not a2[m].get('insufficient')]
        a2_verdict = sum(1 for d in hub_diffs if d >= 0.10) >= 3
        a2_reason = f'{sum(1 for d in hub_diffs if d >= 0.10)}/5 metrics show hub2>hub1 by >=0.10'

    # Audit 3 verdict
    a3_verdict_str = a3.get('t_dominance_metric_a', {}).get('verdict', 'insufficient_data')
    a3_recast = a3_verdict_str == 'recast_warranted'

    # Overall
    positional_structure_found = a1_verdict or a2_verdict

    reasons = []
    if a1_verdict:
        reasons.append(f'Audit 1: Metric F differs from Hamming consistently ({consistent_positive} metrics > 0.05)')
    if a2_verdict:
        reasons.append(f'Audit 2: {a2_reason}')
    if not reasons:
        reasons.append('No consistent positional structure detected')

    return {
        'positional_structure_found': positional_structure_found,
        'audit1_metric_f_differs': a1_verdict,
        'audit2_hub_decomp_informative': a2_verdict,
        'audit2_collinearity_issue': abs(col) > 0.7,
        'audit3_t_dominance_verdict': a3_verdict_str,
        'audit3_recast_warranted': a3_recast,
        'reason': '; '.join(reasons),
    }


# ─── Formatting ──────────────────────────────────────────────────────────────

def fmt(v, d=3):
    if v is None:
        return 'n/a'
    try:
        return f'{float(v):.{d}f}'
    except (TypeError, ValueError):
        return str(v)


# ─── Markdown Output ──────────────────────────────────────────────────────────

def write_markdown(result, path):
    a1 = result['audit1_metric_f']
    a2 = result['audit2_hub_separation']
    a3 = result['audit3_hub2_partition']
    vd = result['verdict']
    meta = result['metadata']

    lines = ['# Metric Audit — Results', '']
    lines += [f'## Verdict: {vd["reason"]}', '']
    lines += [
        f'Positional structure found: **{vd["positional_structure_found"]}**  ',
        f'Metric F differs from Hamming: **{vd["audit1_metric_f_differs"]}**  ',
        f'Hub decomposition informative: **{vd["audit2_hub_decomp_informative"]}**  ',
        f'T-dominance recast verdict: **{vd["audit3_t_dominance_verdict"]}**  ',
        '',
        f'Slice family: {meta["n_slices"]} slices, '
        f'{meta["n_pairs"]} total pairs, '
        f'{meta["n_non_degenerate"]} non-degenerate.',
        '',
    ]

    # Audit 1
    lines += [
        '## Audit 1: E-Weighted Hamming (Metric F) vs. Unweighted Hamming',
        '',
        f'Hamming–F collinearity: ρ = {fmt(a1["hamming_f_collinearity"]["rho"])} '
        f'(n={a1["hamming_f_collinearity"]["n"]}). '
        'High collinearity expected (F = Hamming + E_diff).',
        '',
        '| Metric | n | Hamming ρ | Metric F ρ | Difference (F − Hamm) |',
        '|---|---|---|---|---|',
    ]
    for ml in ['A', 'B', 'C', 'D', 'E']:
        r = a1.get(ml, {})
        if r.get('insufficient'):
            lines.append(f'| {ml}: {METRIC_NAMES[ml]} | {r.get("n", 0)} | n/a | n/a | n/a |')
        else:
            lines.append(
                f'| {ml}: {METRIC_NAMES[ml]} | {r["n"]} | '
                f'{fmt(r["hamming_rho"])} | {fmt(r["f_rho"])} | '
                f'{fmt(r["diff"])} |'
            )
    lines.append('')
    lines += [
        'Positive finding criterion: |difference| ≥ 0.05 in consistent direction across ≥ 3/5 metrics.',
        '',
    ]

    # Audit 2
    col = a2['hub_collinearity']
    lines += [
        '## Audit 2: Hub-Separated Predictors',
        '',
        f'Hub 1 vs Hub 2 collinearity: ρ = {fmt(col["rho"])} (n={col["n"]}). '
        + ('**UNINTERPRETABLE** (|ρ| > 0.7).' if col['uninterpretable']
           else 'Interpretable.'),
        '',
        '| Metric | n | Hub 1 zero-ρ | Hub 2 zero-ρ | Hub 1 partial ρ | Hub 2 partial ρ | Hub2 − Hub1 |',
        '|---|---|---|---|---|---|---|',
    ]
    for ml in ['A', 'B', 'C', 'D', 'E']:
        r = a2.get(ml, {})
        if r.get('insufficient'):
            lines.append(f'| {ml}: {METRIC_NAMES[ml]} | {r.get("n", 0)} | n/a | n/a | n/a | n/a | n/a |')
        else:
            lines.append(
                f'| {ml}: {METRIC_NAMES[ml]} | {r["n"]} | '
                f'{fmt(r["hub1_zero_rho"])} | {fmt(r["hub2_zero_rho"])} | '
                f'{fmt(r["hub1_partial_rho"])} | {fmt(r["hub2_partial_rho"])} | '
                f'{fmt(r["hub_diff"])} |'
            )
    lines.append('')

    # Audit 3
    td = a3.get('t_dominance_metric_a', {})
    lines += [
        '## Audit 3: Hub-2-Spanning Partition',
        '',
        f'n_spanning: {a3["n_spanning"]}, n_internal: {a3["n_internal"]} '
        f'({100*a3["spanning_fraction"]:.0f}% spanning).',
        '',
    ]
    if 'spanning_T_rho' in td:
        lines += [
            f'T-axis partial ρ under Metric A:',
            f'  Spanning pairs: {fmt(td["spanning_T_rho"])}',
            f'  Internal pairs: {fmt(td["internal_T_rho"])}',
            f'  Difference: {fmt(td["diff"])} → **{td["verdict"]}**',
            '',
        ]

    # Full per-axis table for spanning and internal subsets, Metric A
    lines += ['### Per-Axis Partial ρ: Spanning vs Internal (all metrics)', '']
    lines += [
        '| Subset | Metric | n | P | T | E | S | Top-1 |',
        '|---|---|---|---|---|---|---|---|',
    ]
    for subset_key, label in [('spanning', 'Spanning'), ('internal', 'Internal')]:
        for ml in ['A', 'B', 'C', 'D', 'E']:
            r = a3.get(f'{subset_key}_{ml}', {})
            if r.get('insufficient'):
                lines.append(f'| {label} | {ml} | {r.get("n", 0)} | n/a | n/a | n/a | n/a | n/a |')
            else:
                pc = r.get('partial_corr', {})
                ranking = r.get('axis_ranking', [])
                top1 = ranking[0] if ranking else 'n/a'
                lines.append(
                    f'| {label} | {ml}: {METRIC_NAMES[ml]} | {r["n"]} | '
                    f'{fmt(pc.get("P"))} | {fmt(pc.get("T"))} | '
                    f'{fmt(pc.get("E"))} | {fmt(pc.get("S"))} | {top1} |'
                )
    lines.append('')

    # Methodological self-report
    lines += [
        '## Methodological Self-Report',
        '',
        '- Slice family: same 24-slice combined family as position_geometry_metric_sensitivity (10 Tier-1 + 14 Tier-2).',
        '- Degenerate pairs excluded (n_extractive < 50 at Tier-1 or n_constraints < 5).',
        '- Audit 1: zero-order Spearman(positional_dist, structural_dist); no partial-correlation control (metric_F is a linear combination of axis diffs and would be collinear with them as controls).',
        '- Audit 2: partial Spearman with hub1_diff controlling for hub2_diff, and vice versa.',
        '- Audit 3: effective_immutability encoded from constraint_indexing.pl lines 191-223; (civilizational, analytical) treated as dual (counts as spanning in all pairs).',
        '- Audit 3 per-axis partial Spearman uses rank-residualization controlling for all other three axes within each subset.',
        '- Hub-2 spanning classification: a pair is spanning if either slice has a dual output OR the two slices have different outputs (mountain vs rope).',
        '',
        '## What This Evidence Does and Does Not Support',
        '',
        '**Supports:**',
        '- [from Audit 1] Whether E-weighted Hamming is a better positional-distance predictor of structural metrics than unweighted Hamming.',
        '- [from Audit 2] Whether Hub 1 (P, S axes) and Hub 2 (T, E axes) variation capture statistically independent structural variance.',
        '- [from Audit 3] Whether T-axis dominance under Metric A (extractive fraction) is concentrated in pairs that span Hub 2\'s mountain/rope boundary.',
        '- [from code inspection] That v6.11\'s Axiom 2 notation d(P) understates E\'s role (d = g(P, E) in structural path); and σ(S(P)) is incorrect notation (S is independent of P).',
        '',
        '**Does not support:**',
        '- A claim that the implementation "privileges" any metric — it encodes no distance computation at all.',
        '- An explanation of why P-axis partial ρ is empirically weak (no pure P-axis pairs in working family; untestable in this audit).',
        '- A test of the framework\'s P-primacy claim about the binary sheaf/presheaf boundary (H¹ = 0 vs H¹ > 0); these audits do not compute H¹.',
        '- A determination of which structural metric A-E operationalizes the framework\'s cover-story mechanism in its native terms.',
    ]
    lines.append('')

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

    print('Building pairs...')
    pairs = build_pairs(combined, slice_scalars, all_stypes)
    n_pairs = len(pairs)
    non_degen = sum(1 for p in pairs if not p['is_degenerate'])
    print(f'  {n_pairs} total, {non_degen} non-degenerate')

    # Hub-2 partition summary
    nd_pairs = [p for p in pairs if not p['is_degenerate']]
    n_spanning = sum(1 for p in nd_pairs if p['hub2_spanning'])
    print(f'  Hub-2 spanning: {n_spanning}/{non_degen} ({100*n_spanning/non_degen:.0f}%)')

    print('Running Audit 1 (Metric F vs Hamming)...')
    a1 = audit1_metric_f(pairs)
    for ml in ['A', 'B', 'C', 'D', 'E']:
        r = a1.get(ml, {})
        if not r.get('insufficient'):
            print(f'  {ml}: Hamming ρ={fmt(a1[ml]["hamming_rho"])}, '
                  f'F ρ={fmt(a1[ml]["f_rho"])}, diff={fmt(a1[ml]["diff"])}')

    print('Running Audit 2 (Hub-separated predictors)...')
    a2 = audit2_hub_separation(pairs)
    col = a2['hub_collinearity']
    print(f'  Hub collinearity: ρ={fmt(col["rho"])} '
          f'({"UNINTERPRETABLE" if col["uninterpretable"] else "interpretable"})')
    for ml in ['A', 'B', 'C', 'D', 'E']:
        r = a2.get(ml, {})
        if not r.get('insufficient'):
            print(f'  {ml}: hub1 partial={fmt(r["hub1_partial_rho"])}, '
                  f'hub2 partial={fmt(r["hub2_partial_rho"])}, '
                  f'diff={fmt(r["hub_diff"])}')

    print('Running Audit 3 (Hub-2-spanning partition)...')
    a3 = audit3_spanning_partition(pairs)
    print(f'  n_spanning={a3["n_spanning"]}, n_internal={a3["n_internal"]}')
    td = a3.get('t_dominance_metric_a', {})
    if 'spanning_T_rho' in td:
        print(f'  T under Metric A: spanning={fmt(td["spanning_T_rho"])}, '
              f'internal={fmt(td["internal_T_rho"])}, '
              f'diff={fmt(td["diff"])}, verdict={td["verdict"]}')

    verdict = compute_verdict(a1, a2, a3)
    print(f'\nVerdict: {verdict["reason"]}')

    result = {
        'metadata': {
            'n_slices': len(combined),
            'n_pairs': n_pairs,
            'n_non_degenerate': non_degen,
        },
        'audit1_metric_f': a1,
        'audit2_hub_separation': a2,
        'audit3_hub2_partition': a3,
        'verdict': verdict,
    }

    class _Enc(json.JSONEncoder):
        def default(self, o):
            if isinstance(o, (np.bool_,)):
                return bool(o)
            if isinstance(o, (np.integer,)):
                return int(o)
            if isinstance(o, (np.floating,)):
                return float(o)
            return super().default(o)

    json.dump(result, open('outputs/metric_audit_results.json', 'w'), indent=2, cls=_Enc)
    write_markdown(result, 'outputs/metric_audit_results.md')
    print('Done. Outputs: outputs/metric_audit_results.{json,md}')


if __name__ == '__main__':
    main()
