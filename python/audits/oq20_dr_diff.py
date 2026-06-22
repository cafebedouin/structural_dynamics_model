#!/usr/bin/env python3
"""OQ-20 — canonicalize + masked DR diff for the baseline code/data audit.

Does the load-bearing comparison work and nothing else interesting, so it ships
positive controls (--selftest): a CLEAN verdict funnels entirely through this
script and is worthless if the script is byte-identical to one that never looked.

Comparison model (CLAUDE.md plan):
  - per_constraint indexed by `id`; ids present on only one side are REPORTED,
    never silently dropped.
  - field set = intersection of per-constraint keys on both sides; dropped keys
    (a-only / b-only) are listed.
  - granularity is (id, field) -> canonical-JSON(value); volatile manifest is
    dropped before comparison.
  - noise mask = union of (id,field) diffs across all pairwise repeats in a cell.
  - three buckets: MATCH (no cross diff) / PERTURBED (diff & not masked) /
    INDETERMINATE (diff & masked).
"""
import sys, json, glob, argparse, itertools
from collections import defaultdict

VOLATILE_MANIFEST = {'pipeline_run_at', 'code_commit', 'code_commit_short', 'code_dirty'}


def load(path, rekey=None):
    """-> (id_index: dict canonical_id->{field: canon_value}, field_set, n).

    rekey: optional dict {filename_base: canonical_id}. When given, the row is
    INDEXED by rekey[row.id] (so filename-keyed HEAD rows match the tag's
    canonical-id space), but the row's own `id` FIELD is left untouched so the
    relabeling surfaces as a per-field diff (the documented PERTURBED-on-id
    finding), not silently canonicalized away.
    """
    obj = json.load(open(path))
    pc = obj.get('per_constraint')
    if not isinstance(pc, list):
        raise ValueError(f"{path}: no per_constraint list")
    idx = {}
    fields = set()
    for row in pc:
        rid = row.get('id')
        if rid is None:
            continue
        key = rekey.get(rid, rid) if rekey else rid
        canon = {k: json.dumps(v, sort_keys=True) for k, v in row.items()}
        if key in idx:
            idx[key].setdefault('__dup__', '0')
            idx[key]['__dup__'] = str(int(idx[key]['__dup__']) + 1)
        idx[key] = canon
        fields |= set(row.keys())
    return idx, fields, len(pc)


def field_intersection(fa, fb):
    inter = (fa & fb)
    return inter, sorted(fa - fb), sorted(fb - fa)


def diff_cells(a, b, fields):
    """-> (diff_keys: set of (id,field), id_only_a, id_only_b)."""
    ids_a, ids_b = set(a), set(b)
    common = ids_a & ids_b
    diff = set()
    for cid in common:
        ra, rb = a[cid], b[cid]
        for f in fields:
            if ra.get(f) != rb.get(f):
                diff.add((cid, f))
    return diff, sorted(ids_a - ids_b), sorted(ids_b - ids_a)


def build_mask(outputs, fields):
    """outputs = list of id_index (repeats of one cell). Mask = union of (id,field)
    that differ across ANY pairwise repeat comparison."""
    mask = set()
    for x, y in itertools.combinations(outputs, 2):
        d, _, _ = diff_cells(x, y, fields)
        mask |= d
    return mask


def bucket(cross_diff, mask):
    perturbed = cross_diff - mask
    indeterminate = cross_diff & mask
    return {'perturbed': perturbed, 'indeterminate': indeterminate,
            'n_match_complement': None}


def summarize(diff, mask, label):
    b = bucket(diff, mask)
    pert = b['perturbed']
    pert_id = {(i, f) for (i, f) in pert if f == 'id'}      # the relabeling finding
    pert_nonid = pert - pert_id                              # substantive DR fields
    return {
        'label': label,
        'cross_diff_n': len(diff),
        'mask_n': len(mask),
        'PERTURBED_n': len(pert),
        'PERTURBED_id_n': len(pert_id),                     # expected: id relabeling
        'PERTURBED_nonid_n': len(pert_nonid),               # the real OQ-20 test
        'INDETERMINATE_n': len(b['indeterminate']),
        'PERTURBED_nonid': sorted(pert_nonid)[:200],
        'PERTURBED_nonid_fields': sorted({f for _, f in pert_nonid}),
        'PERTURBED_id_sample': sorted(pert_id)[:10],
        'INDETERMINATE_fields': sorted({f for _, f in b['indeterminate']}),
    }


# --------------------------------------------------------------------------
# Positive controls
# --------------------------------------------------------------------------
def _synth(n=10, fields=('id', 'chi', 'cls', 'nested')):
    pc = []
    for i in range(n):
        pc.append({'id': f'c{i}', 'chi': round(0.1 * i, 4), 'cls': 'rope',
                   'nested': {'a': i, 'b': [i, i + 1]}})
    return {'per_constraint': pc, 'manifest': {'pipeline_run_at': 'X'}}


def selftest():
    import copy, tempfile, os
    ok = True
    tmp = tempfile.mkdtemp()

    def w(obj, name):
        p = os.path.join(tmp, name)
        json.dump(obj, open(p, 'w'))
        return p

    base = _synth()
    # ---- Control A: planted-different incl. id present on one side only ----
    pa = w(base, 'a.json')
    bobj = copy.deepcopy(base)
    bobj['per_constraint'][2]['chi'] = 99.0          # planted (c2, chi)
    bobj['per_constraint'][5]['cls'] = 'snare'        # planted (c5, cls)
    bobj['per_constraint'][7]['nested']['b'] = [0]     # planted (c7, nested)
    bobj['per_constraint'].append({'id': 'cZ', 'chi': 1, 'cls': 'x',
                                   'nested': {}})      # planted id-only-b
    pb = w(bobj, 'b.json')
    A, fA, _ = load(pa)
    B, fB, _ = load(pb)
    inter, _, _ = field_intersection(fA, fB)
    d, ioa, iob = diff_cells(A, B, inter)
    planted = {('c2', 'chi'), ('c5', 'cls'), ('c7', 'nested')}
    if d != planted:
        print(f"  CONTROL A FAIL: got {sorted(d)} expected {sorted(planted)}"); ok = False
    elif iob != ['cZ'] or ioa != []:
        print(f"  CONTROL A FAIL: id-only handling ioa={ioa} iob={iob}"); ok = False
    else:
        print(f"  [control A PASS] flagged exactly {sorted(planted)}; id-only-b=['cZ'] surfaced")

    # ---- Control B: planted-noisy -> mask captures -> INDETERMINATE ----
    # two repeats of a cell that jitter on (c3,chi); cross-diff also hits (c3,chi)
    r1 = w(base, 'r1.json')
    nbase = copy.deepcopy(base)
    nbase['per_constraint'][3]['chi'] = 0.3001        # jitter
    r2 = w(nbase, 'r2.json')
    R1, fR1, _ = load(r1)
    R2, fR2, _ = load(r2)
    mask = build_mask([R1, R2], inter)
    if ('c3', 'chi') not in mask:
        print(f"  CONTROL B FAIL: mask did not capture jitter {sorted(mask)}"); ok = False
    else:
        # now a cross diff that hits the masked (c3,chi) AND an unmasked (c1,cls)
        cobj = copy.deepcopy(base)
        cobj['per_constraint'][3]['chi'] = 0.3001     # same field as jitter -> masked
        cobj['per_constraint'][1]['cls'] = 'piton'    # unmasked -> PERTURBED
        pc = w(cobj, 'c.json')
        C, fC, _ = load(pc)
        cd, _, _ = diff_cells(base_idx := load(r1)[0], C, inter)
        s = bucket(cd, mask)
        if ('c3', 'chi') not in s['indeterminate']:
            print(f"  CONTROL B FAIL: masked diff not routed to INDETERMINATE"); ok = False
        elif ('c1', 'cls') not in s['perturbed']:
            print(f"  CONTROL B FAIL: unmasked diff not routed to PERTURBED"); ok = False
        else:
            print(f"  [control B PASS] masked->INDETERMINATE, unmasked->PERTURBED")

    print("SELFTEST:", "PASS" if ok else "FAIL")
    return 0 if ok else 1


# --------------------------------------------------------------------------
def run(cells, rekeys=None):
    """cells: dict label -> list of file paths (repeats). Returns report dict.
    rekeys: dict label -> rekey-map dict, applied to that cell's loads (HEAD
    filename-keyed cells B/D). Re-key is applied at load BEFORE mask construction
    so noise-floor self-diffs and cross-code diffs share one canonical id-space.
    Computes Arm1 (A,B,C,D) and Arm2 (E,F) if present."""
    rekeys = rekeys or {}
    loaded = {lab: [load(p, rekeys.get(lab)) for p in paths]
              for lab, paths in cells.items()}
    report = {'cells': {}, 'comparisons': {}}
    for lab, reps in loaded.items():
        idx0, f0, n0 = reps[0]
        report['cells'][lab] = {'n_per_constraint': n0, 'n_ids': len(idx0),
                                'n_fields': len(f0), 'n_repeats': len(reps)}

    def cmp_pair(la, lb, mask_cells):
        a_reps = loaded[la]; b_reps = loaded[lb]
        A, fA, _ = a_reps[0]; B, fB, _ = b_reps[0]
        inter, a_only, b_only = field_intersection(fA, fB)
        cross, ioa, iob = diff_cells(A, B, inter)
        mask = set()
        for mc in mask_cells:
            mask |= build_mask([r[0] for r in loaded[mc]], inter)
        s = summarize(cross, mask, f'{la}<->{lb}')
        s['intersection_n'] = len(inter)
        s['intersection'] = sorted(inter)
        s['dropped_a_only'] = a_only
        s['dropped_b_only'] = b_only
        s['id_only_a'] = ioa[:50]; s['id_only_a_n'] = len(ioa)
        s['id_only_b'] = iob[:50]; s['id_only_b_n'] = len(iob)
        return s

    if {'A', 'B'} <= set(loaded):
        report['comparisons']['A<->B'] = cmp_pair('A', 'B', ['A', 'B'])
    if {'C', 'D'} <= set(loaded):
        report['comparisons']['C<->D'] = cmp_pair('C', 'D', ['C', 'D'])
    if {'E', 'F'} <= set(loaded):
        report['comparisons']['E<->F'] = cmp_pair('E', 'F', ['E', 'F'])
    return report


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--selftest', action='store_true')
    ap.add_argument('--glob-dir', help='dir with <CELL>_<rep>.json files')
    ap.add_argument('--rekey-B', help='rekey map json applied to cell B (HEAD/original_json)')
    ap.add_argument('--rekey-D', help='rekey map json applied to cell D (HEAD/original_v6)')
    ap.add_argument('--out', help='write report JSON here')
    args = ap.parse_args()
    if args.selftest:
        sys.exit(selftest())
    if not args.glob_dir:
        ap.error('need --glob-dir or --selftest')
    cells = defaultdict(list)
    for p in sorted(glob.glob(f'{args.glob_dir}/*_*.json')):
        base = p.split('/')[-1].rsplit('.', 1)[0]
        cell = base.split('_')[0]
        cells[cell].append(p)
    rekeys = {}
    if args.rekey_B:
        rekeys['B'] = json.load(open(args.rekey_B))
    if args.rekey_D:
        rekeys['D'] = json.load(open(args.rekey_D))
    rep = run(dict(cells), rekeys)
    txt = json.dumps(rep, indent=2)
    if args.out:
        open(args.out, 'w').write(txt)
    print(txt)


if __name__ == '__main__':
    main()
