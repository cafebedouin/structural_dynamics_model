#!/usr/bin/env python3
"""OQ-20 consolidated analysis -> evidence JSON.

Reads the captured cell outputs in outputs/oq20/ and emits a structured
evidence file with: per-cell determinism (noise floor), Arm-1 (A<->B, C<->D)
field-level diffs split by relabel-null artifact vs clean signal, and Arm-2
(E<->F) split by cs-axis output fields (expected) vs DR-observer fields (the
Theorem-7 test) with the cs-free negative-control subset called out.
"""
import json, re, glob, os, sys
from collections import Counter

OUT = 'outputs/oq20'
CS_OUTPUT_FIELDS = {  # cs-axis output fields: trivially change when cs stripped
    'cs_pattern', 'cs_pattern_signals', 'cs_instance_count', 'cs_reference_frame',
    'cs_drift_gap', 'cs_drift_moment', 'cs_drift_terminal', 'cs_drift_unacknowledged',
    'cs_verdicts', 'cs_axiom_foreclosed',
}


def load_idx(path, rekey=None):
    o = json.load(open(path))
    idx = {}
    for r in o['per_constraint']:
        rid = r['id']
        key = rekey.get(rid, rid) if rekey else rid
        idx[key] = r
    fields = set(o['per_constraint'][0]) if o['per_constraint'] else set()
    return idx, fields


def canon(v):
    return json.dumps(v, sort_keys=True)


def noise_mask(paths, fields, rekey=None):
    idxs = [load_idx(p, rekey)[0] for p in paths]
    mask = set()
    for i in range(len(idxs)):
        for j in range(i + 1, len(idxs)):
            a, b = idxs[i], idxs[j]
            for c in set(a) & set(b):
                for f in fields:
                    if canon(a[c].get(f)) != canon(b[c].get(f)):
                        mask.add((c, f))
    return mask


def arm1(la_paths, lb_paths, rekey_b, label):
    A, fA = load_idx(la_paths[0])
    B, fB = load_idx(lb_paths[0], rekey_b)
    inter = (fA & fB) - {'id'}
    common = set(A) & set(B)
    maskA = noise_mask(la_paths, inter)
    maskB = noise_mask(lb_paths, inter, rekey_b)
    mask = maskA | maskB
    # null-artifact: HEAD couldn't find facts (claimed_type null in B)
    nullset = {c for c in common if B[c].get('claimed_type') is None}
    clean = common - nullset
    fc = Counter()
    perturbed_nonid = set()
    for c in clean:
        for f in inter:
            if canon(A[c].get(f)) != canon(B[c].get(f)):
                fc[f] += 1
                if (c, f) not in mask:
                    perturbed_nonid.add((c, f))
    zero = sorted(set(inter) - set(fc))
    return {
        'label': label, 'intersection_n': len(inter),
        'common': len(common), 'null_artifact_n': len(nullset),
        'clean': len(clean), 'mask_n': len(mask),
        'id_only_tag': sorted(set(A) - set(B)),
        'id_only_head': sorted(set(B) - set(A)),
        'zero_diff_fields': zero,
        'changed_fields_over_clean': dict(fc.most_common()),
        'PERTURBED_nonid_n': len(perturbed_nonid),
    }


def arm2(le_paths, lf_paths, label):
    E, fE = load_idx(le_paths[0])
    F, fF = load_idx(lf_paths[0])
    inter = (fE & fF) - {'id'}
    common = set(E) & set(F)
    mask = noise_mask(le_paths, inter) | noise_mask(lf_paths, inter)
    # cs-free vs cs-bearing source files
    asis = f'{OUT}/corpora/kernel_v1_asis'
    csbear, csfree = set(), set()
    for p in glob.glob(asis + '/*.pl'):
        base = os.path.splitext(os.path.basename(p))[0]
        (csbear if re.search(r'narrative_ontology:cs_[a-z_]+\(', open(p).read())
         else csfree).add(base)
    dr_fields = inter - CS_OUTPUT_FIELDS
    fc_dr_bear, fc_dr_free, fc_cs = Counter(), Counter(), Counter()
    for c in common:
        for f in inter:
            if canon(E[c].get(f)) != canon(F[c].get(f)):
                if f in CS_OUTPUT_FIELDS:
                    fc_cs[f] += 1
                elif c in csbear:
                    fc_dr_bear[f] += 1
                elif c in csfree:
                    fc_dr_free[f] += 1
    return {
        'label': label, 'common': len(common),
        'cs_bearing': len(csbear), 'cs_free': len(csfree),
        'mask_n': len(mask),
        'cs_axis_fields_changed': dict(fc_cs.most_common()),
        'DR_fields_changed_cs_bearing': dict(fc_dr_bear.most_common()),
        'DR_fields_changed_cs_free_NEGCTRL': dict(fc_dr_free.most_common()),
        'negctrl_pass': len(fc_dr_free) == 0,
    }


def main():
    rk_oj = json.load(open(f'{OUT}/rekey/original_json.json'))
    rk_v6 = json.load(open(f'{OUT}/rekey/original_v6.json'))
    rep = {}

    def cell(letter):
        return sorted(glob.glob(f'{OUT}/{letter}_*.json'))

    if cell('A') and cell('B'):
        rep['arm1_AB'] = arm1(cell('A'), cell('B'), rk_oj, 'A<->B (original_json)')
    if cell('C') and cell('D'):
        rep['arm1_CD'] = arm1(cell('C'), cell('D'), rk_v6, 'C<->D (original_v6)')
    if cell('E') and cell('F'):
        rep['arm2_EF'] = arm2(cell('E'), cell('F'), 'E<->F (kernel_v1 cs-strip)')

    txt = json.dumps(rep, indent=2)
    open(f'{OUT}/reports/analysis.json', 'w').write(txt)
    print(txt)


if __name__ == '__main__':
    main()
