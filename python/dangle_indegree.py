#!/usr/bin/env python3
"""Does the dangling-target in-degree distribution develop HUBS as the corpus scales?

Closed-vocabulary (saturating) model: as N grows, targets are drawn from a bounded
namespace, so popular concepts accumulate in-degree -> hubs form -> frac will eventually
fall. Open-vocabulary (fresh-mint) model: each story coins new descriptive target names,
namespace grows with N, collisions stay ~0 -> in-degree stays pinned near 1 -> frac flat
forever. The hub test reads the mechanism directly, which is what governs the asymptote.
"""
import os, glob, collections
os.chdir(os.path.join(os.path.dirname(__file__), '..'))
from dangle_curve import scan

for label, pat in [('current (772)', 'prolog/testsets/*.pl'),
                   ('v5 (3380)', 'prolog/archives/prolog_v5/*.pl')]:
    files = sorted(glob.glob(pat))
    real, edges = scan(files)
    indeg = collections.Counter(t for _, t in edges if t not in real)
    dist = collections.Counter(indeg.values())   # in-degree -> #targets with it
    distinct = len(indeg)
    dedges = sum(indeg.values())
    singletons = dist.get(1, 0)
    maxd = max(indeg.values()) if indeg else 0
    # mean in-degree, and share of dangle edges that land on a singleton target
    mean_indeg = dedges / distinct if distinct else 0
    singleton_edge_share = singletons / dedges if dedges else 0
    print(f'\n{label}: {distinct} distinct dangle targets, {dedges} dangle edges')
    print(f'  mean in-degree = {mean_indeg:.3f}   max in-degree = {maxd}')
    print(f'  targets reached exactly once (singletons): {singletons} '
          f'({singletons/distinct:.1%} of targets, {singleton_edge_share:.1%} of dangle edges)')
    print('  in-degree histogram (deg: #targets):',
          dict(sorted(dist.items())))
