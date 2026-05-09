#!/usr/bin/env python3
"""Position-Space Geometry Audit.

Characterizes how PTES position-space geometry shapes classification across
the combined 24-slice family (10 Tier-1 main-corpus + 14 Tier-2 SOTU).

Four passes:
  1. Per-axis contribution (zero-order + partial Spearman)
  2. Linearity (Hamming distance → structural distance)
  3. Anisotropy (per-axis structural strength, per-value-pair breakdown)
  4. Axis-specific classification flow matrices (main + SOTU corpora)

Cross-validation gate: Spearman(ext_frac_dist, homophily_dist) on 45
main-corpus pairs; halt if ρ < 0.85.

Outputs: outputs/position_geometry_audit.{json,md}
"""

import ast
import glob
import json
import math
import sys
from collections import defaultdict
from itertools import combinations
from pathlib import Path

import numpy as np
from scipy.linalg import lstsq
from scipy.stats import linregress, pearsonr, spearmanr

# ─── Constants ────────────────────────────────────────────────────────────────

EXTRACTIVE = {'rope', 'tangled_rope', 'snare'}
ALL_TYPES = ['mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton']
AXES = ['P', 'T', 'E', 'S']
AXIS_IDX = {'P': 0, 'T': 1, 'E': 2, 'S': 3}
HAMMING_WEIGHTS = {'P': 2, 'T': 1, 'E': 2, 'S': 1}


# ─── Data Loading ─────────────────────────────────────────────────────────────

def load_data():
    bc = json.load(open('outputs/bc_coupling_audit.json'))
    idea = json.load(open('outputs/idea_site_exploration.json'))
    sotu_recon = json.load(open('outputs/sotu_reconnaissance.json'))
    pipeline = json.load(open('outputs/pipeline_output.json'))
    return bc, idea, sotu_recon, pipeline


def load_sotu_constraints():
    """Load all 189 SOTU constraint files. Returns {cid: {classifications: [...]}}."""
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


# ─── Slice Building ───────────────────────────────────────────────────────────

def sotu_ext_frac(type_counts, n_classifications):
    if not n_classifications:
        return None
    ext = sum(type_counts.get(t, 0) for t in EXTRACTIVE)
    return ext / n_classifications


def build_tier1_slices(idea, bc):
    """10 main-corpus slices from idea_site_exploration working_slices."""
    slices = []
    bc_ps = bc['pass1']['per_slice']
    for ws in idea['working_slices']:
        label = ws['label']
        key = tuple(ws['key'])
        coverage = ws['coverage']
        bc_data = bc_ps[label]
        n_ext = bc_data['n_extractive']
        slices.append({
            'label': label,
            'key': key,
            'ext_frac': n_ext / coverage if coverage else None,
            'homophily': bc_data['extractive_homophily'],
            'degenerate': bc_data['degenerate'],
            'n_constraints': coverage,
            'n_classifications': coverage,
            'type_counts': ws['type_counts'],
            'tier': 1,
        })
    return slices


def build_tier2_slices(sotu_recon):
    """14 SOTU slices with n_constraints ≥ 10 and not in main working family."""
    ss = sotu_recon['sotu_slices']
    slices = []
    counter = 1
    for k_str, v in sorted(ss.items(), key=lambda x: -x[1]['n_constraints']):
        if v['n_constraints'] < 10 or v.get('in_working_family', False):
            continue
        key = ast.literal_eval(k_str)
        n_cls = v['n_classifications']
        slices.append({
            'label': f'SOTU_{counter}',
            'key': key,
            'ext_frac': sotu_ext_frac(v['type_counts'], n_cls),
            'homophily': None,
            'degenerate': False,
            'n_constraints': v['n_constraints'],
            'n_classifications': n_cls,
            'type_counts': v['type_counts'],
            'tier': 2,
        })
        counter += 1
    return slices


# ─── Pair Building ────────────────────────────────────────────────────────────

def compute_pairs(slices):
    """C(n,2) pairs with axis diffs, Hamming, weighted Hamming, and struct_dist."""
    pairs = []
    for (i, s1), (j, s2) in combinations(enumerate(slices), 2):
        k1, k2 = s1['key'], s2['key']
        axis_diff = {ax: int(k1[AXIS_IDX[ax]] != k2[AXIS_IDX[ax]]) for ax in AXES}
        hamming = sum(axis_diff.values())
        weighted_hamming = sum(HAMMING_WEIGHTS[ax] * v for ax, v in axis_diff.items())
        ef1, ef2 = s1['ext_frac'], s2['ext_frac']
        struct_dist = abs(ef1 - ef2) if ef1 is not None and ef2 is not None else None
        is_degen = (s1.get('degenerate', False) or s2.get('degenerate', False)
                    or s1['n_constraints'] < 5 or s2['n_constraints'] < 5)
        pairs.append({
            'i': i, 'j': j,
            'label_i': s1['label'], 'label_j': s2['label'],
            'tier_i': s1['tier'], 'tier_j': s2['tier'],
            'axis_diff': axis_diff,
            'hamming': hamming,
            'weighted_hamming': weighted_hamming,
            'struct_dist': struct_dist,
            'is_degenerate': is_degen,
        })
    return pairs


# ─── Math helpers ─────────────────────────────────────────────────────────────

def partial_spearman(x, y, controls):
    """Rank-residualization approximation of partial Spearman."""
    from scipy.stats import rankdata
    x, y = np.asarray(x, float), np.asarray(y, float)
    rx, ry = rankdata(x), rankdata(y)
    cols = [rankdata(np.asarray(c, float)) for c in controls]
    rc = np.column_stack(cols + [np.ones(len(x))])
    rx_r = rx - rc @ lstsq(rc, rx)[0]
    ry_r = ry - rc @ lstsq(rc, ry)[0]
    return pearsonr(rx_r, ry_r)


def hellinger(tc1, n1, tc2, n2):
    """Hellinger distance on 6-type distribution."""
    if not n1 or not n2:
        return None
    p = [tc1.get(t, 0) / n1 for t in ALL_TYPES]
    q = [tc2.get(t, 0) / n2 for t in ALL_TYPES]
    return math.sqrt(0.5 * sum((math.sqrt(pi) - math.sqrt(qi)) ** 2 for pi, qi in zip(p, q)))


# ─── Cross-Validation Gate ────────────────────────────────────────────────────

def cross_validate(tier1_slices):
    ef_dists, hom_dists = [], []
    for s1, s2 in combinations(tier1_slices, 2):
        ef1, ef2 = s1['ext_frac'], s2['ext_frac']
        h1, h2 = s1['homophily'], s2['homophily']
        if None in (ef1, ef2, h1, h2):
            continue
        ef_dists.append(abs(ef1 - ef2))
        hom_dists.append(abs(h1 - h2))
    rho, pval = spearmanr(ef_dists, hom_dists)
    return float(rho), float(pval), len(ef_dists)


# ─── Pass 1: Per-Axis Contribution ────────────────────────────────────────────

def run_pass1_core(working_pairs):
    if not working_pairs:
        return None
    struct_dists = np.array([p['struct_dist'] for p in working_pairs])
    axis_vecs = {ax: np.array([p['axis_diff'][ax] for p in working_pairs]) for ax in AXES}

    zero_order = {}
    for ax in AXES:
        r, pv = spearmanr(axis_vecs[ax], struct_dists)
        zero_order[ax] = {'rho': float(r), 'p': float(pv)}

    partial = {}
    for ax in AXES:
        controls = [axis_vecs[ot] for ot in AXES if ot != ax]
        r, pv = partial_spearman(axis_vecs[ax], struct_dists, controls)
        partial[ax] = {'rho': float(r), 'p': float(pv)}

    axis_ranking = sorted(AXES, key=lambda a: -abs(partial[a]['rho']))
    return {'n_pairs': len(working_pairs), 'zero_order': zero_order,
            'partial': partial, 'axis_ranking': axis_ranking}


def run_pass1(pairs, slices):
    tier1_labels = {s['label'] for s in slices if s['tier'] == 1}
    non_degen = [p for p in pairs if not p['is_degenerate'] and p['struct_dist'] is not None]

    combined_result = run_pass1_core(non_degen)
    tier1_only = run_pass1_core([p for p in non_degen
                                 if p['label_i'] in tier1_labels and p['label_j'] in tier1_labels])

    # Sensitivity checks
    label_map = {s['label']: s for s in slices}
    wh_vals = np.array([p['weighted_hamming'] for p in non_degen])
    struct_arr = np.array([p['struct_dist'] for p in non_degen])
    rho_wh, p_wh = spearmanr(wh_vals, struct_arr)

    hell_pairs = []
    for p in non_degen:
        si, sj = label_map[p['label_i']], label_map[p['label_j']]
        tci, tcj = si.get('type_counts'), sj.get('type_counts')
        ni, nj = si['n_classifications'], sj['n_classifications']
        h = hellinger(tci, ni, tcj, nj) if tci and tcj else None
        if h is not None:
            hell_pairs.append((h, p['struct_dist']))

    if hell_pairs:
        h_vals, sd_vals = zip(*hell_pairs)
        rho_h, p_h = spearmanr(h_vals, sd_vals)
        hell_result = {'rho': float(rho_h), 'p': float(p_h), 'n': len(hell_pairs)}
    else:
        hell_result = {}

    sensitivity = {
        'weighted_hamming_vs_struct': {'rho': float(rho_wh), 'p': float(p_wh)},
        'hellinger_vs_extfrac': hell_result,
    }

    combined_result['check_a'] = tier1_only
    combined_result['sensitivity'] = sensitivity
    return combined_result


# ─── Pass 2: Linearity ────────────────────────────────────────────────────────

def run_pass2(pairs):
    non_degen = [p for p in pairs if not p['is_degenerate'] and p['struct_dist'] is not None]
    if not non_degen:
        return {}

    hamming_arr = np.array([p['hamming'] for p in non_degen], dtype=float)
    struct_arr = np.array([p['struct_dist'] for p in non_degen])
    slope, intercept, r, pv, se = linregress(hamming_arr, struct_arr)

    groups = {}
    for d_val in sorted(set(int(h) for h in hamming_arr)):
        mask = hamming_arr == d_val
        gd = struct_arr[mask]
        groups[d_val] = {'count': int(mask.sum()), 'mean': float(gd.mean()),
                         'std': float(gd.std()), 'min': float(gd.min()), 'max': float(gd.max())}

    sorted_ds = sorted(groups)
    steps = [{'from': d1, 'to': d2,
               'step': abs(groups[d2]['mean'] - groups[d1]['mean'])}
             for d1, d2 in zip(sorted_ds, sorted_ds[1:])]
    step_vals = [s['step'] for s in steps]
    median_step = sorted(step_vals)[len(step_vals) // 2] if step_vals else 0
    discontinuous = [s for s in steps if median_step > 0 and s['step'] > 2 * median_step]

    pure_by_axis = defaultdict(list)
    for p in non_degen:
        if p['hamming'] == 1:
            for ax in AXES:
                if p['axis_diff'][ax] == 1:
                    pure_by_axis[ax].append(p['struct_dist'])

    per_axis_pure = {}
    for ax in AXES:
        dists = pure_by_axis[ax]
        per_axis_pure[ax] = ({'n': len(dists), 'mean': float(np.mean(dists)),
                               'std': float(np.std(dists)),
                               'min': float(min(dists)), 'max': float(max(dists))}
                              if dists else {'n': 0})

    linearity = 'linear' if r ** 2 >= 0.7 else ('moderate' if r ** 2 >= 0.4 else 'weak')
    return {
        'n_pairs': len(non_degen),
        'linear_fit': {'r_squared': float(r ** 2), 'slope': float(slope),
                        'intercept': float(intercept), 'p': float(pv)},
        'per_group': groups,
        'discontinuity': {'steps': steps, 'discontinuous_steps': discontinuous},
        'per_axis_pure_pairs': per_axis_pure,
        'linearity_verdict': linearity,
    }


# ─── Pass 3: Anisotropy ───────────────────────────────────────────────────────

def run_pass3(sotu_recon, tier1_slices, tier2_slices):
    ss = sotu_recon['sotu_slices']
    pap = sotu_recon['pure_axis_pairs']

    # Build ext_frac + n lookup for every SOTU slice
    all_ef = {}
    for k_str, v in ss.items():
        k = ast.literal_eval(k_str)
        n_cls = v['n_classifications']
        all_ef[k] = {'ext_frac': sotu_ext_frac(v['type_counts'], n_cls),
                      'n': v['n_constraints'], 'type_counts': v['type_counts'],
                      'n_cls': n_cls}

    # Override with Tier 1 (main-corpus values are authoritative for shared slices)
    for s in tier1_slices:
        all_ef[s['key']] = {'ext_frac': s['ext_frac'], 'n': s['n_constraints'],
                              'type_counts': s['type_counts'], 'n_cls': s['n_classifications']}

    per_axis = {}
    for ax in AXES:
        main_pairs = [(tuple(p[0]), tuple(p[1]))
                      for p in pap['base_working_family'].get(ax, [])]
        sotu_pairs = [(tuple(p[0]), tuple(p[1]))
                      for p in pap['new_from_sotu'].get(ax, [])]
        all_axis_pairs = main_pairs + sotu_pairs

        valid_pairs, per_vp = [], []
        for k1, k2 in all_axis_pairs:
            v1, v2 = all_ef.get(k1), all_ef.get(k2)
            if v1 is None or v2 is None:
                continue
            ef1, ef2 = v1['ext_frac'], v2['ext_frac']
            if ef1 is None or ef2 is None:
                continue
            dist = abs(ef1 - ef2)
            val1, val2 = k1[AXIS_IDX[ax]], k2[AXIS_IDX[ax]]
            n1, n2 = v1['n'], v2['n']
            valid_pairs.append(dist)
            per_vp.append({'key1': list(k1), 'key2': list(k2),
                            'val1': val1, 'val2': val2,
                            'value_pair': f'{val1}↔{val2}',
                            'ef1': float(ef1), 'ef2': float(ef2),
                            'struct_dist': float(dist),
                            'n1': n1, 'n2': n2,
                            'noisy': n1 < 20 or n2 < 20})

        axis_mean = float(np.mean(valid_pairs)) if valid_pairs else None

        # Aggregate by value pair
        vp_groups = defaultdict(list)
        vp_noisy = defaultdict(bool)
        for entry in per_vp:
            vp_groups[entry['value_pair']].append(entry['struct_dist'])
            if entry['noisy']:
                vp_noisy[entry['value_pair']] = True

        vp_summary = {}
        for vp, dists in sorted(vp_groups.items()):
            mean_d = float(np.mean(dists))
            dev = abs(mean_d - axis_mean) if axis_mean is not None else None
            vp_summary[vp] = {
                'n': len(dists),
                'mean': mean_d,
                'range': float(max(dists) - min(dists)),
                'deviation_from_axis_mean': float(dev) if dev is not None else None,
                'flagged': dev is not None and dev > 0.3,
                'any_noisy': vp_noisy.get(vp, False),
            }

        per_axis[ax] = {
            'n_pairs': len(valid_pairs),
            'strength': axis_mean,
            'std': float(np.std(valid_pairs)) if valid_pairs else None,
            'per_value_pair': vp_summary,
            'pair_details': per_vp,
        }

    strengths = {ax: per_axis[ax]['strength'] for ax in AXES
                 if per_axis[ax]['strength'] is not None}
    if len(strengths) >= 2:
        max_s, min_s = max(strengths.values()), min(strengths.values())
        anisotropy_ratio = float(max_s / min_s) if min_s > 0 else None
    else:
        anisotropy_ratio = None

    axis_ranking = sorted(AXES, key=lambda a: -(strengths.get(a) or 0))

    # Cross-axis additivity: d=2 pairs, compare actual dist to sum of single-axis strengths
    # (informational; not blocking)
    # Omitted from per_axis for brevity; available from pair_details combinations

    return {
        'per_axis': per_axis,
        'pure_axis_structural_strengths': strengths,
        'anisotropy_ratio': anisotropy_ratio,
        'axis_ranking_by_strength': axis_ranking,
    }


# ─── Pass 4: Axis-Specific Flow Matrices ─────────────────────────────────────

def precompute_slice_types(constraints_idx, slices_info):
    """Build {label: {cid: type_str}} for a list of slice dicts."""
    result = {}
    for ws in slices_info:
        label, key = ws['label'], tuple(ws['key'])
        P, T, E, S = key
        result[label] = {}
        for cid, c in constraints_idx.items():
            for cls in c.get('classifications', []):
                ctx = cls['context']
                if (ctx.get('agent_power') == P and ctx.get('time_horizon') == T
                        and ctx.get('exit_options') == E and ctx.get('spatial_scope') == S):
                    result[label][cid] = cls['type']
                    break
    return result


def precompute_main_slice_types(pipeline_list, slices_info):
    """Variant for main corpus (pipeline is a list, not a dict)."""
    result = {}
    for ws in slices_info:
        label, key = ws['label'], tuple(ws['key'])
        P, T, E, S = key
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


def directed_flow(stypes_i, stypes_j):
    """Directed flow matrix from slice i to slice j (shared constraints)."""
    mat = {t: {t2: 0 for t2 in ALL_TYPES} for t in ALL_TYPES}
    shared = set(stypes_i) & set(stypes_j)
    for cid in shared:
        src, dst = stypes_i[cid], stypes_j[cid]
        if src in mat and dst in mat.get(src, {}):
            mat[src][dst] += 1
    return mat, len(shared)


def aggregate_flows(pairs_keys, stypes, label_lookup, bidirectional=True):
    """Aggregate flow matrices over a set of pairs. Returns (matrix, n_pairs, n_shared)."""
    total = {t: {t2: 0 for t2 in ALL_TYPES} for t in ALL_TYPES}
    n_pairs = 0
    n_shared = 0
    for k1, k2 in pairs_keys:
        l1, l2 = label_lookup.get(k1), label_lookup.get(k2)
        if l1 is None or l2 is None:
            continue
        if l1 not in stypes or l2 not in stypes:
            continue
        fm, ns = directed_flow(stypes[l1], stypes[l2])
        for src in ALL_TYPES:
            for dst in ALL_TYPES:
                total[src][dst] += fm[src][dst]
        if bidirectional:
            fm_r, _ = directed_flow(stypes[l2], stypes[l1])
            for src in ALL_TYPES:
                for dst in ALL_TYPES:
                    total[src][dst] += fm_r[src][dst]
        n_pairs += 1
        n_shared += ns
    return total, n_pairs, n_shared


def top_transitions(mat, n=3):
    trans = [{'src': s, 'dst': d, 'count': mat[s][d]}
             for s in ALL_TYPES for d in ALL_TYPES
             if s != d and mat[s][d] > 0]
    return sorted(trans, key=lambda x: -x['count'])[:n]


def run_pass4(pipeline_list, sotu_constraints, tier1_slices, tier2_slices, sotu_recon, idea):
    pap = sotu_recon['pure_axis_pairs']
    ss = sotu_recon['sotu_slices']

    # Build label lookups
    main_label_lkp = {s['key']: s['label'] for s in tier1_slices}
    sotu_label_lkp = {}
    for s in tier2_slices:
        sotu_label_lkp[s['key']] = s['label']
    for k_str in ss:
        k = ast.literal_eval(k_str)
        if k not in sotu_label_lkp:
            sotu_label_lkp[k] = str(k)  # non-tier-2 slices use str key as label

    # Precompute slice types
    main_stypes = precompute_main_slice_types(
        pipeline_list, [{'label': s['label'], 'key': list(s['key'])} for s in tier1_slices])

    sotu_all_slice_info = [{'label': sotu_label_lkp[ast.literal_eval(k)], 'key': list(ast.literal_eval(k))}
                            for k in ss]
    sotu_stypes = precompute_slice_types(sotu_constraints, sotu_all_slice_info)

    tier2_keys = {s['key'] for s in tier2_slices}

    flow_results = {'main': {}, 'sotu': {}}

    # Main corpus: base_working_family pure-axis pairs
    for ax in AXES:
        pairs = [(tuple(p[0]), tuple(p[1])) for p in pap['base_working_family'].get(ax, [])]
        if not pairs:
            continue
        fm, np_, ns = aggregate_flows(pairs, main_stypes, main_label_lkp)
        flow_results['main'][ax] = {
            'flow_matrix': fm,
            'top_transitions': top_transitions(fm),
            'n_pairs': np_,
            'n_shared': ns,
        }

    # SOTU: new_from_sotu pure-axis pairs (Tier 2 primary, all as secondary)
    for ax in AXES:
        all_pairs = [(tuple(p[0]), tuple(p[1])) for p in pap['new_from_sotu'].get(ax, [])]
        tier2_pairs = [(k1, k2) for k1, k2 in all_pairs
                       if k1 in tier2_keys and k2 in tier2_keys]
        primary_pairs = tier2_pairs if tier2_pairs else all_pairs
        note = ('Tier 2 pairs only' if tier2_pairs
                else 'all SOTU pairs (no Tier 2 pure pairs for this axis)')
        if not primary_pairs:
            continue
        fm, np_, ns = aggregate_flows(primary_pairs, sotu_stypes, sotu_label_lkp)
        flow_results['sotu'][ax] = {
            'flow_matrix': fm,
            'top_transitions': top_transitions(fm),
            'n_pairs': np_,
            'n_shared': ns,
            'note': note,
        }

    # Cross-reference: directed U_3_imm → U_3_civ rope→piton
    expected = 1171
    computed = None
    if 'T' in flow_results['main']:
        # Recompute directed (not bidirectional) for U_3_imm→U_3_civ
        t_pairs = [(tuple(p[0]), tuple(p[1])) for p in pap['base_working_family'].get('T', [])]
        if t_pairs:
            k1, k2 = t_pairs[0]
            l1, l2 = main_label_lkp.get(k1), main_label_lkp.get(k2)
            if l1 and l2:
                # Determine which is U_3_imm (immediate) and which is U_3_civ
                if 'immediate' in k1:
                    fm_dir, _ = directed_flow(main_stypes.get(l1, {}), main_stypes.get(l2, {}))
                else:
                    fm_dir, _ = directed_flow(main_stypes.get(l2, {}), main_stypes.get(l1, {}))
                computed = fm_dir.get('rope', {}).get('piton', 0)

    xref_ok = computed is not None and abs(computed - expected) <= 2

    # T-axis cross-corpus comparison
    t_comparison = {}
    t_main = flow_results['main'].get('T', {})
    t_sotu = flow_results['sotu'].get('T', {})
    if t_main.get('top_transitions') and t_sotu.get('top_transitions'):
        mt = t_main['top_transitions'][0]
        st = t_sotu['top_transitions'][0]
        agree = (mt['src'], mt['dst']) == (st['src'], st['dst'])
        t_comparison = {
            'main_top': mt,
            'sotu_top': st,
            'agree_on_top_transition': agree,
            'verdict': 'coherent' if agree else 'divergent',
        }

    return {
        'flow_results': flow_results,
        'cross_reference': {
            'expected_rope_piton': expected,
            'computed_rope_piton': computed,
            'match': xref_ok,
        },
        't_axis_cross_corpus': t_comparison,
    }


# ─── Check B: Mountain Coverage in SOTU ──────────────────────────────────────

def run_check_b(sotu_recon):
    ss = sotu_recon['sotu_slices']

    mtn_slices = {}
    total_mtns = 0
    for k_str, v in ss.items():
        k = ast.literal_eval(k_str)
        n_mtn = v['type_counts'].get('mountain', 0)
        if n_mtn > 0:
            n_cls = v['n_classifications']
            ext = sum(v['type_counts'].get(t, 0) for t in EXTRACTIVE)
            mtn_slices[k] = {
                'n_mountain': n_mtn,
                'n_total': n_cls,
                'ext_frac_at_slice': ext / n_cls if n_cls else None,
                'n_constraints': v['n_constraints'],
            }
            total_mtns += n_mtn

    if not mtn_slices:
        return {'verdict': 'no_mountains_in_sotu', 'total_mountains': 0}

    max_count = max(v['n_mountain'] for v in mtn_slices.values())
    concentration = max_count / total_mtns
    dominant_key = next(k for k, v in mtn_slices.items() if v['n_mountain'] == max_count)

    non_dominant_ef = [v['ext_frac_at_slice'] for k, v in mtn_slices.items()
                       if k != dominant_key and v['ext_frac_at_slice'] is not None]

    max_ef_nd = max(non_dominant_ef) if non_dominant_ef else None
    ef_range_nd = (max(non_dominant_ef) - min(non_dominant_ef)) if len(non_dominant_ef) >= 2 else None

    if concentration > 0.8:
        verdict = 'indeterminate'
        reason = 'concentration > 0.8 — same coverage artifact as main corpus'
    elif concentration < 0.5 and max_ef_nd is not None and max_ef_nd < 0.1:
        verdict = 'supports_decoupling'
        reason = ('concentration < 0.5 AND max ext_frac at non-dominant slices < 0.1 — '
                  'mountains appear only at observer positions with very low extractive fraction, '
                  'consistent with natural-law stability (Axiom 3)')
    elif concentration < 0.5 and ef_range_nd is not None and ef_range_nd > 0.2:
        verdict = 'refutes_decoupling'
        reason = ('concentration < 0.5 AND ext_frac range > 0.2 at non-dominant slices — '
                  'mountains co-occur with high extractive fractions, inconsistent with Axiom 3')
    else:
        verdict = 'indeterminate'
        reason = 'intermediate: concentration between 0.5–0.8 or ext_frac thresholds not clearly met'

    return {
        'total_mountains_in_sotu': int(total_mtns),
        'n_slices_with_mountains': len(mtn_slices),
        'dominant_slice': list(dominant_key),
        'dominant_count': int(max_count),
        'concentration': float(concentration),
        'n_non_dominant_slices': len(non_dominant_ef),
        'max_ext_frac_non_dominant': float(max_ef_nd) if max_ef_nd is not None else None,
        'ext_frac_range_non_dominant': float(ef_range_nd) if ef_range_nd is not None else None,
        'per_slice': {str(k): v for k, v in mtn_slices.items()},
        'verdict': verdict,
        'reason': reason,
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
    lines = ['# Position-Space Geometry Audit', '']

    xv = result['cross_validation']
    lines += [
        '## Cross-Validation Gate',
        f'Spearman(ext\\_frac\\_dist, homophily\\_dist) on {xv["n_pairs"]} main-corpus pairs: '
        f'ρ={fmt(xv["rho"])}, p={fmt(xv["p"])}',
        f'**Gate: {"PASSED" if xv["passed"] else "FAILED"}** (threshold ρ ≥ 0.85)',
        '',
    ]

    if not xv['passed']:
        lines += ['> **HALT: Cross-validation failed.**',
                  '> Extractive fraction is not a valid proxy for homophily across corpora.',
                  '> A different shared metric is needed before combining corpora.']
        Path(path).write_text('\n'.join(lines))
        return

    # Check B — promote to top if supports_decoupling
    cb = result.get('check_b', {})
    if cb.get('verdict') == 'supports_decoupling':
        lines += [
            '---',
            '## PRIORITY FINDING: Mountain Decoupling Confirmed (Check B)',
            '',
            (f'> **Verdict: supports\\_decoupling** — Resolves the prior audit\'s '
             f'`coverage_artifact_indeterminate` verdict.'),
            '',
            f'- Total mountains in SOTU: {cb["total_mountains_in_sotu"]}',
            f'- Slices with mountains: {cb["n_slices_with_mountains"]}',
            f'- Concentration: {fmt(cb["concentration"])} (< 0.5 threshold met)',
            f'- Max ext\\_frac at non-dominant slices: {fmt(cb["max_ext_frac_non_dominant"])} (< 0.1 threshold met)',
            f'- {cb["reason"]}',
            '',
            '---',
            '',
        ]

    # Pass 1
    p1 = result['pass1']
    lines += [
        '## Pass 1 — Per-Axis Contribution',
        '',
        f'Non-degenerate pairs: {p1["n_pairs"]}',
        '',
        '### Zero-Order Spearman',
        '| Axis | ρ | p |',
        '|---|---|---|',
    ]
    for ax in AXES:
        zo = p1['zero_order'][ax]
        lines.append(f'| {ax} | {fmt(zo["rho"])} | {fmt(zo["p"])} |')

    lines += ['', '### Partial Spearman (controlling for other 3 axes)',
              '| Axis | ρ | p |', '|---|---|---|']
    for ax in AXES:
        pa = p1['partial'][ax]
        lines.append(f'| {ax} | {fmt(pa["rho"])} | {fmt(pa["p"])} |')

    lines += ['', f'**Axis ranking (|partial ρ|):** {" > ".join(p1["axis_ranking"])}', '']

    ca = p1.get('check_a')
    if ca:
        lines += ['### Check A — Tier-1-Only Replication',
                  f'n_pairs: {ca["n_pairs"]}',
                  '| Axis | Zero-order ρ | Partial ρ |', '|---|---|---|']
        for ax in AXES:
            lines.append(f'| {ax} | {fmt(ca["zero_order"][ax]["rho"])} | {fmt(ca["partial"][ax]["rho"])} |')
        lines += [f'Tier-1 axis ranking: {" > ".join(ca["axis_ranking"])}', '']

    sens = p1.get('sensitivity', {})
    if sens:
        wh = sens.get('weighted_hamming_vs_struct', {})
        hell = sens.get('hellinger_vs_extfrac', {})
        lines += ['### Sensitivity Checks',
                  f'- Weighted Hamming (2,1,2,1) vs struct\\_dist: ρ={fmt(wh.get("rho"))}']
        if hell:
            lines.append(f'- Hellinger dist vs ext\\_frac dist: ρ={fmt(hell.get("rho"))}, n={hell.get("n")}')
        lines.append('')

    # Pass 2
    p2 = result['pass2']
    lf = p2['linear_fit']
    lines += [
        '## Pass 2 — Linearity',
        '',
        f'R²={fmt(lf["r_squared"])}, slope={fmt(lf["slope"])}, '
        f'intercept={fmt(lf["intercept"])}, p={fmt(lf["p"])}',
        f'**Verdict: {p2["linearity_verdict"]}**',
        '',
        '| Hamming d | n | mean | std | min | max |',
        '|---|---|---|---|---|---|',
    ]
    for d_val, g in sorted(p2['per_group'].items()):
        lines.append(f'| {d_val} | {g["count"]} | {fmt(g["mean"])} | {fmt(g["std"])} | {fmt(g["min"])} | {fmt(g["max"])} |')

    disc = p2.get('discontinuity', {})
    lines += ['',
              (f'⚠ Discontinuous steps: {disc["discontinuous_steps"]}'
               if disc.get('discontinuous_steps') else 'No discontinuous steps detected.'),
              '']

    # Pass 3
    p3 = result['pass3']
    lines += [
        '## Pass 3 — Anisotropy',
        '',
        '| Axis | n\_pairs | strength | std |',
        '|---|---|---|---|',
    ]
    for ax in AXES:
        ad = p3['per_axis'].get(ax, {})
        lines.append(f'| {ax} | {ad.get("n_pairs", 0)} | {fmt(ad.get("strength"))} | {fmt(ad.get("std"))} |')

    lines += ['',
              f'**Anisotropy ratio (max/min):** {fmt(p3.get("anisotropy_ratio"))}',
              f'**Axis ranking by strength:** {" > ".join(p3.get("axis_ranking_by_strength", []))}',
              '']

    lines.append('### Per-Value-Pair Breakdown')
    lines.append('*(⚠=deviation>0.3 from axis mean, ~=contains n<20 pairs)*')
    for ax in AXES:
        vp = p3['per_axis'].get(ax, {}).get('per_value_pair', {})
        if not vp:
            continue
        lines += [f'', f'**{ax}-axis:**',
                  '| Value Pair | n | mean dist | range | flags |',
                  '|---|---|---|---|---|']
        for vp_name, info in sorted(vp.items()):
            flags = ''
            if info.get('flagged'):
                flags += '⚠'
            if info.get('any_noisy'):
                flags += '~'
            lines.append(f'| {vp_name} | {info["n"]} | {fmt(info["mean"])} | {fmt(info["range"])} | {flags} |')
    lines.append('')

    # Pass 4
    p4 = result['pass4']
    xref = p4['cross_reference']
    lines += [
        '## Pass 4 — Axis-Specific Flow Matrices',
        '',
        f'Cross-reference U\\_3\\_imm→U\\_3\\_civ rope→piton: '
        f'expected={xref["expected_rope_piton"]}, computed={xref["computed_rope_piton"]}, '
        f'match={xref["match"]}',
        '',
    ]

    def fmt_transition(t):
        return f'{t["src"]}→{t["dst"]} (n={t["count"]})'

    tc = p4.get('t_axis_cross_corpus', {})
    if tc:
        mt = tc.get('main_top', {})
        st = tc.get('sotu_top', {})
        lines += [
            '**T-axis cross-corpus comparison:**',
            f'- Main top: {fmt_transition(mt) if mt else "n/a"}',
            f'- SOTU top: {fmt_transition(st) if st else "n/a"}',
            f'- Verdict: {tc.get("verdict")}',
            '',
        ]

    for corpus in ['main', 'sotu']:
        fr = p4['flow_results'].get(corpus, {})
        lines.append(f'**{corpus.upper()} corpus top transitions:**')
        for ax in AXES:
            ax_data = fr.get(ax, {})
            if not ax_data:
                continue
            note = f' ({ax_data["note"]})' if ax_data.get('note') else ''
            top = ax_data.get('top_transitions', [])
            top_str = ', '.join(fmt_transition(t) for t in top) if top else 'none (no shared constraints)'
            lines.append(f'- {ax}: {top_str}{note}')
        lines.append('')

    # Check B
    lines += ['## Corpus Check B — Mountain Coverage in SOTU', '']
    if cb:
        lines += [
            f'- Total mountains: {cb.get("total_mountains_in_sotu")}',
            f'- Slices with mountains: {cb.get("n_slices_with_mountains")}',
            f'- Concentration: {fmt(cb.get("concentration"))}',
            f'- Max ext\\_frac (non-dominant): {fmt(cb.get("max_ext_frac_non_dominant"))}',
            f'- Ext\\_frac range: {fmt(cb.get("ext_frac_range_non_dominant"))}',
            f'',
            f'> **Verdict: {cb.get("verdict")}**',
            f'> {cb.get("reason")}',
        ]

    lines += [
        '',
        '## Methodological Self-Report',
        '',
        '- **Structural metric**: extractive fraction = (rope+tangled\\_rope+snare)/total for cross-corpus;'
        ' homophily for main-corpus cross-validation only.',
        '- **Cross-validation gate**: Spearman on 45 main-corpus (ext\\_frac\\_dist, homophily\\_dist) pairs; ρ<0.85 halts.',
        '- **Partial Spearman**: rank-residualization (OLS on ranked variables, then Pearson on residuals).',
        '- **New SOTU axis values**: `powerful` and `mobile` treated as distinct from all main-corpus values in Hamming.',
        '- **Degenerate threshold**: main corpus n\\_extractive < 50 (from bc\\_coupling\\_audit); any pair with n\\_constraints < 5 excluded.',
        '- **n<20 flag**: pairs where either slice has n\\_constraints < 20 are flagged in per-value-pair detail.',
        '- **T-axis degeneracy**: U\\_3\\_civ flagged degenerate (n\\_extractive=27 < 50). SOTU pure-T pairs supplement.',
        '- **Tier 3 slices**: included in Pass 3 per-value-pair details; Tier 2 preferred for Pass 4 primary.',
        '- **T-axis cross-corpus**: top transition compared between main corpus (1 pair) and SOTU (Tier-2 pairs).',
        '- **Pass 4 flows**: bidirectional aggregation over pure-axis pairs; cross-reference uses directed U\\_3\\_imm→U\\_3\\_civ.',
        '- **Check B**: mountain ext\\_frac = fraction of all classifications at a mountain-containing slice that are extractive.',
    ]

    Path(path).write_text('\n'.join(lines) + '\n')


# ─── Main ─────────────────────────────────────────────────────────────────────

def main():
    print('Loading data...')
    bc, idea, sotu_recon, pipeline = load_data()
    sotu_constraints = load_sotu_constraints()
    print(f'  Loaded {len(sotu_constraints)} SOTU constraints')

    tier1 = build_tier1_slices(idea, bc)
    tier2 = build_tier2_slices(sotu_recon)
    combined = tier1 + tier2
    print(f'  Tier 1: {len(tier1)}, Tier 2: {len(tier2)}, Combined: {len(combined)}')

    print('Cross-validation gate...')
    rho_xv, pval_xv, n_xv = cross_validate(tier1)
    cv = {'rho': rho_xv, 'p': pval_xv, 'n_pairs': n_xv, 'threshold': 0.85, 'passed': rho_xv >= 0.85}
    print(f'  ρ={rho_xv:.3f} ({"PASS" if cv["passed"] else "FAIL"})')

    if not cv['passed']:
        result = {'cross_validation': cv, 'cross_validation_failed': True}
        json.dump(result, open('outputs/position_geometry_audit.json', 'w'), indent=2)
        write_markdown(result, 'outputs/position_geometry_audit.md')
        print('Cross-validation FAILED. Halting.')
        sys.exit(0)

    pairs = compute_pairs(combined)
    print(f'  {len(pairs)} pairs from {len(combined)} slices')

    print('Pass 1: per-axis contribution...')
    pass1 = run_pass1(pairs, combined)

    print('Pass 2: linearity...')
    pass2 = run_pass2(pairs)

    print('Pass 3: anisotropy...')
    pass3 = run_pass3(sotu_recon, tier1, tier2)

    print('Pass 4: flow matrices...')
    pass4 = run_pass4(pipeline['per_constraint'], sotu_constraints, tier1, tier2, sotu_recon, idea)

    print('Check B: mountain coverage in SOTU...')
    check_b = run_check_b(sotu_recon)

    result = {
        'metadata': {
            'n_main_slices': len(tier1),
            'n_sotu_tier2': len(tier2),
            'n_total': len(combined),
            'n_pairs': len(pairs),
        },
        'cross_validation': cv,
        'pass1': pass1,
        'pass2': pass2,
        'pass3': pass3,
        'pass4': pass4,
        'check_b': check_b,
        'corpus_checks': {'check_a': pass1.get('check_a'), 'check_b': check_b},
    }

    json.dump(result, open('outputs/position_geometry_audit.json', 'w'), indent=2, default=str)
    write_markdown(result, 'outputs/position_geometry_audit.md')
    print('Done. Outputs: outputs/position_geometry_audit.{json,md}')

    # Quick summary
    p1 = result['pass1']
    print(f'\nAxis ranking (partial): {p1["axis_ranking"]}')
    print(f'Linearity R²: {result["pass2"]["linear_fit"]["r_squared"]:.3f}')
    p3 = result['pass3']
    print(f'Anisotropy ratio: {p3.get("anisotropy_ratio")}')
    print(f'Axis ranking by strength: {p3["axis_ranking_by_strength"]}')
    print(f'Check B verdict: {check_b["verdict"]}')
    xref = result['pass4']['cross_reference']
    print(f'Cross-reference rope→piton: expected={xref["expected_rope_piton"]}, '
          f'computed={xref["computed_rope_piton"]}, match={xref["match"]}')


if __name__ == '__main__':
    main()
