#!/usr/bin/env python3
"""OQ-195 verification: general-n H1 gap spectrum under the OQ-51 variable-real-seat regime.

Pre-registered checks (audits/2026-07-02_oq195_general_n_gap/PROPOSAL.md — BLOCKING):
  (a/b) per-band exact match, brute force vs Theorem-B recursive predictor, n=2..NMAX,
        unbounded AND T-bounded variants. Union-only matching is disallowed: the union is
        invariant under dropping the parts<=n-j constraint (review 2026-07-02 item 1).
  (b-control) the UNCONSTRAINED predictor must give identical unions but mismatched bands
        somewhere — proving the per-band check discriminates what a union check cannot.
  (c)   Theorem A: min nonzero H1 = n-1 for all n.
  (d)   record match: n=4 -> {0,3,4,5,6}; n=5 -> {0,4,6,7,8,9,10} (v6.13.1:154,156).
  (e)   negative control: a band-off-by-one perturbed predictor must be FLAGGED.
  (f)   Theorem C iff over the FULL spectrum: interval (T_j, B_{j+1}) nonempty <=>
        n >= j+3+C(j+1,2); and when nonempty, EVERY value in it is spectrum-forbidden.
  (g)   T derived from code (axiom_reachability.ALL_TYPES minus 'unknown'), asserted == 7.

The object: grothendieck_cohomology:obstruction_from_vector/3 — for an agreement partition
lam of n real seats, H1 = C(n,2) - sum C(lam_i,2) = (n^2 - sum lam_i^2)/2.

Usage: python3 python/audits/oq195_h1_spectrum_check.py [--nmax 40] [--census] [--json PATH]
Exit nonzero on any pre-registered check failure (halt-on-mismatch).
"""
import argparse, glob, json, os, re, sys
from functools import lru_cache

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(REPO, 'python'))

PROOF_T = 7  # the proof doc's value; check (g) derives independently and asserts equality


def C2(k):
    return k * (k - 1) // 2


# ---------------------------------------------------------------- brute force
def partitions(n, max_part=None):
    """All partitions of n as descending tuples (independent generator)."""
    if max_part is None:
        max_part = n
    if n == 0:
        yield ()
        return
    for p in range(min(n, max_part), 0, -1):
        for rest in partitions(n - p, p):
            yield (p,) + rest


def brute_bands(n, t_bound=None):
    """Group H1 values by band j = n - largest_part. Returns {j: set(values)}."""
    bands = {}
    for lam in partitions(n):
        if t_bound is not None and len(lam) > t_bound:
            continue
        h1 = C2(n) - sum(C2(p) for p in lam)
        bands.setdefault(n - lam[0], set()).add(h1)
    return bands


# ------------------------------------------------- Theorem-B recursive predictor
@lru_cache(maxsize=None)
def D(j, cap, maxcnt):
    """Achievable triangular sums {sum C(mu_i,2)} over mu |- j, parts <= cap,
    count <= maxcnt. Independent recursion (not the brute-force generator)."""
    if j == 0:
        return frozenset([0])
    if maxcnt <= 0 or cap <= 0:
        return frozenset()
    out = set()
    for p in range(min(j, cap), 0, -1):
        for d in D(j - p, p, maxcnt - 1):
            out.add(C2(p) + d)
    return frozenset(out)


def predicted_bands(n, t_bound=None, constrained=True):
    """Theorem B/D: band_j = { jn - C(j+1,2) - d : d in D(j; parts<=n-j[, count<=T-1]) }.
    constrained=False drops the parts<=n-j cap (the review-item-1 wrong classifier)."""
    bands = {0: {0}} if (t_bound is None or t_bound >= 1) else {}
    for j in range(1, n):
        cap = (n - j) if constrained else j
        maxcnt = (t_bound - 1) if t_bound is not None else j
        vals = {j * n - C2(j + 1) - d for d in D(j, cap, maxcnt)}
        if vals:
            bands[j] = vals
    return bands


def union(bands):
    out = set()
    for v in bands.values():
        out |= v
    return out


# ---------------------------------------------------------------- checks
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--nmax', type=int, default=40)
    ap.add_argument('--census', action='store_true')
    ap.add_argument('--json', default=None)
    args = ap.parse_args()

    if args.census:
        return census()

    failures = []
    results = {'nmax': args.nmax, 'checks': {}, 'spectra': {}}

    # (g) T derived from code, first — Theorem-D enumeration depends on it
    from axiom_reachability import ALL_TYPES
    derived_T = len([t for t in ALL_TYPES if t != 'unknown'])
    ok_g = (derived_T == PROOF_T)
    print(f"(g) T derivation: ALL_TYPES={len(ALL_TYPES)} tokens, real={derived_T}, "
          f"proof value={PROOF_T} -> {'PASS' if ok_g else 'FAIL'}")
    if not ok_g:
        failures.append('g')
    results['checks']['g_T_derived'] = {'derived': derived_T, 'proof': PROOF_T, 'pass': ok_g}

    # (a/b) per-band exact match, both variants
    band_mismatches, tband_mismatches = [], []
    union_ctrl_mismatch_unions, union_ctrl_band_diffs = [], []
    for n in range(2, args.nmax + 1):
        bb = brute_bands(n)
        pb = predicted_bands(n)
        if bb != pb:
            band_mismatches.append(n)
        bbT = brute_bands(n, t_bound=derived_T)
        pbT = predicted_bands(n, t_bound=derived_T)
        if bbT != pbT:
            tband_mismatches.append(n)
        # (b-control) unconstrained predictor: same union, some band must differ (for j>n/2 to exist)
        pu = predicted_bands(n, constrained=False)
        if union(pu) != union(pb):
            union_ctrl_mismatch_unions.append(n)
        if pu != pb:
            union_ctrl_band_diffs.append(n)
        results['spectra'][n] = {
            'reachable': sorted(union(bb)),
            'forbidden': sorted(set(range(C2(n) + 1)) - union(bb)),
            'reachable_T%d' % derived_T: sorted(union(bbT)),
            'bands': {str(j): sorted(v) for j, v in sorted(bb.items())},
        }
    ok_ab = not band_mismatches and not tband_mismatches
    print(f"(a/b) per-band match n=2..{args.nmax}: unbounded "
          f"{'PASS' if not band_mismatches else 'FAIL at n=' + str(band_mismatches)}; "
          f"T={derived_T}-bounded {'PASS' if not tband_mismatches else 'FAIL at n=' + str(tband_mismatches)}")
    if not ok_ab:
        failures.append('a/b')

    ok_bc = (not union_ctrl_mismatch_unions) and len(union_ctrl_band_diffs) > 0
    print(f"(b-control) unconstrained predictor: unions identical for all n "
          f"({'PASS' if not union_ctrl_mismatch_unions else 'FAIL'}); per-band differs at "
          f"{len(union_ctrl_band_diffs)} n-values (first: {union_ctrl_band_diffs[:5]}) "
          f"-> {'PASS' if ok_bc else 'FAIL'} (per-band check discriminates)")
    if not ok_bc:
        failures.append('b-control')
    results['checks']['b_control'] = {'union_mismatch_ns': union_ctrl_mismatch_unions,
                                      'band_diff_ns': union_ctrl_band_diffs, 'pass': ok_bc}

    # (c) Theorem A
    bad_a = []
    for n in range(3, args.nmax + 1):
        nz = sorted(v for v in union(brute_bands(n)) if v > 0)
        if nz[0] != n - 1:
            bad_a.append((n, nz[0]))
    ok_c = not bad_a
    print(f"(c) Theorem A min-nonzero = n-1, n=3..{args.nmax}: "
          f"{'PASS' if ok_c else 'FAIL ' + str(bad_a)}")
    if not ok_c:
        failures.append('c')

    # (d) record match
    rec4 = sorted(union(brute_bands(4))) == [0, 3, 4, 5, 6]
    rec5 = sorted(union(brute_bands(5))) == [0, 4, 6, 7, 8, 9, 10]
    ok_d = rec4 and rec5
    print(f"(d) record match: n=4 {'PASS' if rec4 else 'FAIL'}, n=5 {'PASS' if rec5 else 'FAIL'}")
    if not ok_d:
        failures.append('d')

    # (e) negative control: perturb band 2 by +1 — the comparator must flag it
    flagged = False
    for n in range(4, args.nmax + 1):
        pb = predicted_bands(n)
        bad = {j: (set(v) if j != 2 else {x + 1 for x in v}) for j, v in pb.items()}
        if bad != brute_bands(n):
            flagged = True
            break
    print(f"(e) negative control (band-2 off-by-one perturbation flagged): "
          f"{'PASS' if flagged else 'FAIL — comparator blind'}")
    if not flagged:
        failures.append('e')

    # (f) Theorem C iff over the full spectrum
    bad_f = []
    for n in range(3, args.nmax + 1):
        spectrum = union(brute_bands(n))
        for j in range(1, n - 1):
            Tj = j * n - C2(j + 1)
            Bj1 = (j + 1) * (n - j - 1)
            interval = list(range(Tj + 1, Bj1))
            lhs_nonempty = len(interval) > 0
            rhs = (n >= j + 3 + C2(j + 1))
            if lhs_nonempty != rhs:
                bad_f.append((n, j, 'nonempty-iff'))
            if lhs_nonempty and any(v in spectrum for v in interval):
                bad_f.append((n, j, 'value-reachable-in-gap'))
    ok_f = not bad_f
    print(f"(f) Theorem C full-spectrum iff, n=3..{args.nmax}, all j: "
          f"{'PASS' if ok_f else 'FAIL ' + str(bad_f[:5])}")
    if not ok_f:
        failures.append('f')

    results['checks'].update({
        'ab_per_band': {'pass': ok_ab}, 'c_theorem_a': {'pass': ok_c},
        'd_record': {'pass': ok_d}, 'e_negative_control': {'pass': flagged},
        'f_theorem_c': {'pass': ok_f},
    })
    results['verdict'] = 'ALL PASS' if not failures else f'FAIL: {failures}'
    if args.json:
        with open(args.json, 'w') as f:
            json.dump(results, f, indent=1, sort_keys=True)
        print(f"results -> {args.json}")
    print(f"VERDICT: {results['verdict']}")
    return 0 if not failures else 1


def census():
    """Stakeholder-seat cardinality census over the three live legs (as-of stamped)."""
    import subprocess
    stamp = subprocess.run(['git', '-C', REPO, 'rev-parse', '--short', 'HEAD'],
                           capture_output=True, text=True).stdout.strip()
    print(f"# stakeholder seat-count census — repo HEAD {stamp}")
    for leg in ('testsets', 'testsets_haiku', 'testsets_flash'):
        counts = {}
        for f in glob.glob(os.path.join(REPO, 'prolog', leg, '*.pl')):
            n = len(re.findall(r'constraint_stakeholder\(', open(f, encoding='utf-8').read()))
            counts[n] = counts.get(n, 0) + 1
        dist = ' '.join(f'{k}:{v}' for k, v in sorted(counts.items()))
        print(f"{leg}: files_by_seat_count {{{dist}}}")
    return 0


if __name__ == '__main__':
    sys.exit(main())
