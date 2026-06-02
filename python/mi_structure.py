#!/usr/bin/env python3
"""Mutual information among the 5 structural dimensions.

Separates two reasons the joint is bounded:
  - genuine multi-way structural closure  -> low/moderate pairwise redundancy, yet small
    realizable joint (the constraint is in the joint, not in dimension duplication)
  - bounded-via-redundancy  -> high pairwise MI; some dims are near-functions of others,
    so effective dimensionality < 5 and the 'cap' was inflated (weaker, still-real claim)

Reports: marginal entropies, pairwise normalized MI matrix (MI/min(Hi,Hj), 1=duplication),
and total correlation TC = sum(Hi) - H_joint with TC/sum(Hi) = redundant fraction of info.
Dims order: props, voids, actors, drift, zone.
"""
import math
from territory_test import load

DIMS = ['props','voids','actors','drift','zone']

def H(counts):
    n = sum(counts.values())
    return -sum((c/n)*math.log2(c/n) for c in counts.values() if c)

def col_counts(cells, idxs):
    from collections import Counter
    return Counter(tuple(c[i] for i in idxs) for c in cells)

for label, path in [('current(772)','/tmp/cur_sixdim.txt'), ('v5(3380)','/tmp/v5_sixdim.txt')]:
    _, cells = load(path)
    Hi = [H(col_counts(cells,[i])) for i in range(5)]
    Hjoint = H(col_counts(cells, list(range(5))))
    TC = sum(Hi) - Hjoint
    print(f'\n=== {label} ===')
    print('  marginal entropy (bits):', {d: round(h,2) for d,h in zip(DIMS,Hi)})
    print(f'  H_joint = {Hjoint:.2f} bits   sum(Hi) = {sum(Hi):.2f}   '
          f'TC = {TC:.2f}   redundant fraction TC/sum(Hi) = {TC/sum(Hi):.1%}')
    print('  pairwise normalized MI  (MI / min(Hi,Hj),  1.0 = one dim determines the other):')
    hdr = '         ' + ' '.join(f'{d:>7}' for d in DIMS)
    print(hdr)
    for i in range(5):
        row = []
        for j in range(5):
            if i == j:
                row.append('     -- '); continue
            mij = Hi[i] + Hi[j] - H(col_counts(cells,[i,j]))
            nmi = mij / min(Hi[i], Hi[j]) if min(Hi[i],Hi[j]) > 0 else 0
            row.append(f'{nmi:7.2f} ')
        print(f'  {DIMS[i]:>7} ' + ' '.join(row))
