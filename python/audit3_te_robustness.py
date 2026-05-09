#!/usr/bin/env python3
"""Audit 3 T-E Covariation Robustness Check.

Within Hub-2-internal pairs (n=112 from metric_audit.py), partition by E_diff
(same vs different exit_options). In each cell, compute partial Spearman of
T_diff against structural metrics A, B, D, controlling for P_diff and S_diff.

Tests whether the T partial ρ = 0.577 under Metric A is driven by genuine
within-rope-group T-axis variation (robust) or by T-E joint variation in
Tier-1 rope-rope pairs (hypothesis b from the audit prompt).

Outputs: outputs/audit3_te_robustness.{json,md}
"""

import ast
import glob
import json
import math
from itertools import combinations
from pathlib import Path

import numpy as np
from scipy.linalg import lstsq
from scipy.stats import pearsonr, spearmanr

# ─── Constants (must match metric_audit.py exactly) ───────────────────────────

EXTRACTIVE = {'rope', 'tangled_rope', 'snare'}
ALL_TYPES = ['mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton']
AXES = ['P', 'T', 'E', 'S']

EFFECTIVE_IMMUTABILITY = {
    ('immediate', 'trapped'):         'mountain',
    ('immediate', 'identity_locked'): 'mountain',
    ('immediate', 'constrained'):     'mountain',
    ('immediate', 'mobile'):          'rope',
    ('immediate', 'arbitrage'):       'rope',
    ('immediate', 'analytical'):      'rope',
    ('biographical', 'trapped'):         'mountain',
    ('biographical', 'identity_locked'): 'rope',
    ('biographical', 'constrained'):     'mountain',
    ('biographical', 'mobile'):          'rope',
    ('biographical', 'arbitrage'):       'rope',
    ('biographical', 'analytical'):      'rope',
    ('generational', 'trapped'):         'mountain',
    ('generational', 'identity_locked'): 'rope',
    ('generational', 'constrained'):     'rope',
    ('generational', 'mobile'):          'rope',
    ('generational', 'arbitrage'):       'rope',
    ('generational', 'analytical'):      'rope',
    ('historical', 'trapped'):         'rope',
    ('historical', 'identity_locked'): 'rope',
    ('historical', 'constrained'):     'rope',
    ('historical', 'mobile'):          'rope',
    ('historical', 'arbitrage'):       'rope',
    ('historical', 'analytical'):      'rope',
    ('civilizational', 'analytical'):      'dual',
    ('civilizational', 'trapped'):         'rope',
    ('civilizational', 'identity_locked'): 'rope',
    ('civilizational', 'constrained'):     'rope',
    ('civilizational', 'mobile'):          'rope',
    ('civilizational', 'arbitrage'):       'rope',
}


def hub2_output(t, e):
    return EFFECTIVE_IMMUTABILITY.get((t, e), 'rope')


def is_spanning(t1, e1, t2, e2):
    o1 = hub2_output(t1, e1)
    o2 = hub2_output(t2, e2)
    if o1 == 'dual' or o2 == 'dual':
        return True
    return o1 != o2


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


# ─── Slice Building (identical to metric_audit.py) ────────────────────────────

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


# ─── Slice Scalars ────────────────────────────────────────────────────────────

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


# ─── Total Variation ──────────────────────────────────────────────────────────

def total_variation(si, sj):
    ni, nj = si['n_classifications'], sj['n_classifications']
    if not ni or not nj:
        return None
    pi = [si['type_counts'].get(t, 0) / ni for t in ALL_TYPES]
    pj = [sj['type_counts'].get(t, 0) / nj for t in ALL_TYPES]
    return 0.5 * sum(abs(a - b) for a, b in zip(pi, pj))


# ─── Pair Building ────────────────────────────────────────────────────────────

def build_pairs(slices, slice_scalars):
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
        spanning = is_spanning(T1, E1, T2, E2)

        sc_i = slice_scalars[si['label']]
        sc_j = slice_scalars[sj['label']]

        def sdist(key):
            a, b = sc_i.get(key), sc_j.get(key)
            return abs(a - b) if a is not None and b is not None else None

        pairs.append({
            'label_i': si['label'], 'label_j': sj['label'],
            'tier_i': si['tier'], 'tier_j': sj['tier'],
            'axis_diff': axis_diff,
            'hub2_spanning': spanning,
            'is_degenerate': is_degen,
            'dist_A': sdist('A'),
            'dist_B': sdist('B'),
            'dist_D': total_variation(si, sj),
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


# ─── Cell Analysis ────────────────────────────────────────────────────────────

def analyze_cell(pairs, metric_key):
    """Compute T partial ρ (controlling for P, S) and zero-order ρ for a subset."""
    working = [p for p in pairs if p.get(metric_key) is not None]
    n = len(working)
    if n < 10:
        return {'n': n, 'insufficient': True, 'reason': f'n={n} < 10'}

    sd = np.array([p[metric_key] for p in working])
    t_vec = np.array([p['axis_diff']['T'] for p in working], float)
    p_vec = np.array([p['axis_diff']['P'] for p in working], float)
    s_vec = np.array([p['axis_diff']['S'] for p in working], float)

    # Zero-order
    zo_rho, zo_p = spearmanr(t_vec, sd)

    # Partial: T controlling for P and S only (E_diff is constant within cell)
    try:
        partial_rho, partial_p = partial_spearman(t_vec, sd, [p_vec, s_vec])
        partial_ok = True
    except Exception as e:
        partial_rho, partial_p, partial_ok = float('nan'), float('nan'), False

    result = {
        'n': n,
        't_partial_rho': float(partial_rho),
        't_partial_p': float(partial_p),
        't_zero_rho': float(zo_rho),
        't_zero_p': float(zo_p),
    }
    if not partial_ok:
        result['partial_failed'] = True
    return result


# ─── Sanity Check ─────────────────────────────────────────────────────────────

def sanity_check_internal(internal_pairs):
    """Verify n_internal matches original audit (should be 112)."""
    n = len([p for p in internal_pairs if p.get('dist_A') is not None])
    return n


# ─── Formatting ───────────────────────────────────────────────────────────────

def fmt(v, d=3):
    if v is None or (isinstance(v, float) and math.isnan(v)):
        return 'n/a'
    return f'{float(v):.{d}f}'


def verdict_label(e0_rho, e1_rho, n_e0):
    if n_e0 < 15:
        return 'sample-limited'
    if e0_rho is None or math.isnan(e0_rho):
        return 'sample-limited'
    if abs(e0_rho) >= 0.40:
        return 'robust'
    if abs(e0_rho) < 0.20:
        return 'collapsed'
    return 'attenuated'


# ─── Markdown ─────────────────────────────────────────────────────────────────

def write_markdown(result, path):
    lines = ['# Audit 3 T-E Covariation Robustness Check', '']
    lines += [
        '## Summary',
        '',
        f'n_internal (total): {result["n_internal"]}',
        f'n_e_diff_0 (exit options match): {result["n_e_diff_0"]}',
        f'n_e_diff_1 (exit options differ): {result["n_e_diff_1"]}',
        '',
    ]

    for ml, mname in [('A', 'Extractive fraction'), ('B', 'Type entropy'), ('D', 'Total variation distance')]:
        r = result.get(f'metric_{ml}', {})
        e0 = r.get('e_diff_0', {})
        e1 = r.get('e_diff_1', {})
        vd = r.get('verdict', 'n/a')

        lines += [f'## Metric {ml}: {mname}', '']
        lines += [
            '| Cell | n | T partial ρ | T partial p | T zero-order ρ |',
            '|---|---|---|---|---|',
        ]

        def row(label, cell):
            if cell.get('insufficient'):
                return f'| {label} | {cell["n"]} | n/a | n/a | n/a |'
            return (f'| {label} | {cell["n"]} | {fmt(cell.get("t_partial_rho"))} | '
                    f'{fmt(cell.get("t_partial_p"))} | {fmt(cell.get("t_zero_rho"))} |')

        lines.append(row('E_diff=0 (E fixed)', e0))
        lines.append(row('E_diff=1 (E varies)', e1))
        lines += ['', f'**Verdict: {vd}**', '']

    lines += ['## Interpretation', '']
    lines.append(result.get('interpretation', ''))
    lines += ['', f'*Robustness check conducted 2026-05-08. Script: python/audit3_te_robustness.py.*', '']

    Path(path).write_text('\n'.join(lines) + '\n')


# ─── Main ─────────────────────────────────────────────────────────────────────

def main():
    print('Loading data...')
    bc, idea, sotu_recon, pipeline = load_data()
    sotu_constraints = load_sotu_constraints()

    tier1 = build_tier1_slices(idea, bc)
    tier2 = build_tier2_slices(sotu_recon)
    combined = tier1 + tier2
    print(f'  Slices: {len(tier1)} Tier-1 + {len(tier2)} Tier-2 = {len(combined)} total')

    print('Computing slice scalars...')
    slice_scalars = compute_slice_scalars(combined)

    print('Building pairs...')
    all_pairs = build_pairs(combined, slice_scalars)
    non_degen = [p for p in all_pairs if not p['is_degenerate']]
    internal = [p for p in non_degen if not p['hub2_spanning']]
    print(f'  All pairs: {len(all_pairs)}, non-degenerate: {len(non_degen)}, internal: {len(internal)}')

    # Sanity check: dist_A available count should match original n_internal=112
    n_check = sanity_check_internal(internal)
    print(f'  Sanity check: n_internal with dist_A = {n_check} (expected 112)')

    # Partition by E_diff
    e_diff_0 = [p for p in internal if p['axis_diff']['E'] == 0]
    e_diff_1 = [p for p in internal if p['axis_diff']['E'] == 1]
    n_e0 = len(e_diff_0)
    n_e1 = len(e_diff_1)
    print(f'  E_diff=0: {n_e0}, E_diff=1: {n_e1}')

    # T variation breakdown within E_diff=0
    t_varies_in_e0 = sum(1 for p in e_diff_0 if p['axis_diff']['T'] == 1)
    print(f'  T_diff=1 within E_diff=0: {t_varies_in_e0}/{n_e0}')

    result = {
        'n_internal': len(internal),
        'n_e_diff_0': n_e0,
        'n_e_diff_1': n_e1,
        'n_t_varies_in_e_diff_0': t_varies_in_e0,
        'sanity_n_internal_dist_a': n_check,
    }

    # Per-metric analysis
    verdicts = {}
    for ml in ['A', 'B', 'D']:
        dist_key = f'dist_{ml}'
        e0_result = analyze_cell(e_diff_0, dist_key)
        e1_result = analyze_cell(e_diff_1, dist_key)
        e0_rho = e0_result.get('t_partial_rho')
        e1_rho = e1_result.get('t_partial_rho')
        n_e0_working = e0_result.get('n', 0)
        vd = verdict_label(e0_rho, e1_rho, n_e0_working)
        verdicts[ml] = vd

        print(f'\nMetric {ml}:')
        print(f'  E_diff=0: n={e0_result.get("n")}, T partial ρ={fmt(e0_rho)}, '
              f'p={fmt(e0_result.get("t_partial_p"))}, zero-order ρ={fmt(e0_result.get("t_zero_rho"))}')
        print(f'  E_diff=1: n={e1_result.get("n")}, T partial ρ={fmt(e1_rho)}, '
              f'p={fmt(e1_result.get("t_partial_p"))}, zero-order ρ={fmt(e1_result.get("t_zero_rho"))}')
        print(f'  Verdict: {vd}')

        result[f'metric_{ml}'] = {
            'e_diff_0': e0_result,
            'e_diff_1': e1_result,
            'verdict': vd,
        }

    # Overall interpretation string (for markdown)
    a_rho_e0 = result['metric_A']['e_diff_0'].get('t_partial_rho')
    a_rho_e1 = result['metric_A']['e_diff_1'].get('t_partial_rho')
    overall_verdict = verdicts.get('A', 'n/a')

    if overall_verdict == 'robust':
        interp = (
            f"T partial ρ under Metric A in the E_diff=0 cell is {fmt(a_rho_e0)} "
            f"(n={result['metric_A']['e_diff_0']['n']}), remaining large after isolating "
            f"pairs where exit options are held constant. This rules out hypothesis (b): "
            f"the 0.577 was not inflated by T-E joint variation in Tier-1 rope-rope pairs. "
            f"T-axis variation within the rope immutability group produces extractive-fraction "
            f"differences independently of E. The original §5.3 mechanistic claim stands."
        )
    elif overall_verdict == 'collapsed':
        interp = (
            f"T partial ρ under Metric A in the E_diff=0 cell is {fmt(a_rho_e0)} "
            f"(n={result['metric_A']['e_diff_0']['n']}), substantially below the full-internal "
            f"value of 0.577. T partial ρ in the E_diff=1 cell is {fmt(a_rho_e1)}, "
            f"indicating the signal concentrates where T and E co-vary. This supports "
            f"hypothesis (b): the 0.577 was driven by T-E joint variation (Tier-1 collinearity) "
            f"rather than an independent T-axis effect. §5.3's claim of a 'within-rope-group T effect "
            f"driven by the organizational/institutional divide' should be revised to "
            f"'T-E joint variation in Hub-2-internal pairs.'"
        )
    elif overall_verdict == 'attenuated':
        interp = (
            f"T partial ρ under Metric A in the E_diff=0 cell is {fmt(a_rho_e0)} "
            f"(n={result['metric_A']['e_diff_0']['n']}), below the full-internal 0.577 but "
            f"not negligible. The signal attenuates when T-E collinearity is removed, "
            f"suggesting partial contribution from T-E joint variation. §5.3's mechanism "
            f"claim warrants qualification: the T-axis effect is partially independent of E "
            f"and partially a joint T-E effect; the organizational/institutional divide drives "
            f"some but not all of the 0.577."
        )
    else:  # sample-limited
        n_e0_val = result['metric_A']['e_diff_0'].get('n', 0)
        interp = (
            f"The E_diff=0 cell has n={n_e0_val} pairs with non-null dist_A, below the "
            f"threshold for reliable partial correlation. The robustness check is sample-limited: "
            f"the T-E collinearity concern cannot be resolved at this sample size. §5.3's "
            f"mechanistic claim should be flagged as unverified pending a larger slice family."
        )

    result['interpretation'] = interp
    result['overall_verdict_metric_a'] = overall_verdict

    # Write outputs
    class _Enc(json.JSONEncoder):
        def default(self, o):
            import numpy as np
            if isinstance(o, (np.bool_,)):
                return bool(o)
            if isinstance(o, (np.integer,)):
                return int(o)
            if isinstance(o, (np.floating,)):
                return float(o)
            return super().default(o)

    json.dump(result, open('outputs/audit3_te_robustness.json', 'w'), indent=2, cls=_Enc)
    write_markdown(result, 'outputs/audit3_te_robustness.md')
    print('\nOutputs: outputs/audit3_te_robustness.{json,md}')


if __name__ == '__main__':
    main()
