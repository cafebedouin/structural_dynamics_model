#!/usr/bin/env python3
"""Separate the two spaces the six-dim fingerprint conflated.

shift = fingerprint_shift(C) = classification at 4 FIXED observer contexts -> it lives in
constraint x observer-context space (the reading axis). The other five dims are per-constraint
structural properties. Counting them jointly mixes the spaces. Here we split:
  - STRUCTURAL space  = the 5 per-constraint dims (drop shift)
  - CONTEXT-RESPONSE space = shift alone (4-tuple of types over the fixed grid)
and rarefy each separately so each gets its own (meaningful) knee.
"""
import random, statistics, math

def split_top(s):
    """Split top-level comma args of 'name(a, b, c)' respecting [] and () nesting."""
    inner = s[s.index('(')+1:s.rindep('(')] if False else s[s.index('(')+1:rfind_close(s)]
    args, depth, cur = [], 0, ''
    for ch in inner:
        if ch in '([': depth += 1
        elif ch in ')]': depth -= 1
        if ch == ',' and depth == 0:
            args.append(cur.strip()); cur = ''
        else:
            cur += ch
    if cur.strip(): args.append(cur.strip())
    return args

def rfind_close(s):
    # index of the matching final ')' for the outermost '('
    depth = 0; start = s.index('(')
    for i in range(start, len(s)):
        if s[i] == '(': depth += 1
        elif s[i] == ')':
            depth -= 1
            if depth == 0: return i
    return len(s)-1

def load(path):
    shifts, fives = [], []
    for line in open(path):
        line = line.strip()
        if not line: continue
        a = split_top(line)            # 6 args: shift, props, voids, actors, drift, zone
        shifts.append(a[0])
        fives.append('|'.join(a[1:]))
    return shifts, fives

def rarefy(items, grid, K=40):
    out = []
    for n in grid:
        if n > len(items): n = len(items)
        m = statistics.mean(len(set(random.sample(items, n))) for _ in range(K))
        out.append((n, m))
    return out

def logfit(curve):
    lx = [math.log(n) for n,_ in curve]; ys = [s for _,s in curve]
    mx, my = statistics.mean(lx), statistics.mean(ys)
    b = sum((x-mx)*(y-my) for x,y in zip(lx,ys))/sum((x-mx)**2 for x in lx)
    return b  # dS/dn = b/n

for label, path in [('current(772)', '/tmp/cur_sixdim.txt'), ('v5(3380)', '/tmp/v5_sixdim.txt')]:
    sh, fv = load(path)
    print(f'\n{label}: distinct shift(context-response)={len(set(sh))}  '
          f'distinct 5-dim(structural)={len(set(fv))}  joint6={len(set(zip(sh,fv)))}')

print('\n=== v5 rarefaction, two spaces separately ===')
sh, fv = load('/tmp/v5_sixdim.txt')
grid = [50,100,200,300,500,750,1000,1500,2000,2500,3000,3380]
for name, items in [('STRUCTURAL (5-dim)', fv), ('CONTEXT-RESPONSE (shift)', sh)]:
    curve = rarefy(items, grid)
    b = logfit(curve)
    print(f'\n{name}: end-count={curve[-1][1]:.0f}, dS/dn~={b:.1f}/n')
    print('   n, E[distinct]:', '  '.join(f'{n}:{s:.0f}' for n,s in curve))
    if b > 0:
        for floor in (0.01, 0.001):
            print(f'   knee @ {floor:.1%} novelty: n ~ {b/floor:,.0f}')
