#!/usr/bin/env python3
"""W2 witness 4: structural diff of pipeline_output.json, baseline (pre-W2,
stashed working tree at 00040bb9) vs post-change (W2 applied). Pre-derived
expectation: ONLY temporal_residual backed_times for the 7 marker-stamped
seriesless constraints (7 x 4 contexts = 28 leaf diffs) + manifest fields."""
import json, sys
from collections import Counter

def load(p):
    with open(p) as f:
        return json.load(f)

def walk(x, y, path, diffs):
    if type(x) != type(y):
        diffs.append((path, 'TYPE', x, y)); return
    if isinstance(x, dict):
        for k in sorted(set(x) | set(y)):
            if k not in x: diffs.append((path+'/'+k, 'ADDED', None, y[k]))
            elif k not in y: diffs.append((path+'/'+k, 'REMOVED', x[k], None))
            else: walk(x[k], y[k], path+'/'+k, diffs)
    elif isinstance(x, list):
        if len(x) != len(y):
            diffs.append((path, f'LEN {len(x)}->{len(y)}', None, None)); return
        for i, (xi, yi) in enumerate(zip(x, y)):
            walk(xi, yi, f'{path}[{i}]', diffs)
    elif x != y:
        diffs.append((path, 'VAL', x, y))

a, b = load(sys.argv[1]), load(sys.argv[2])
diffs = []
walk(a, b, '', diffs)
print(f'total diffs: {len(diffs)}')
buckets = Counter('manifest' if 'manifest' in p else ('backed_times' if 'backed' in p else p.split('/')[-1]) for p, *_ in diffs)
for k, n in buckets.most_common():
    print(f'  {k}: {n}')
for p, kind, old, new in diffs:
    print(f'  {p} {kind}: {old!r} -> {new!r}')
