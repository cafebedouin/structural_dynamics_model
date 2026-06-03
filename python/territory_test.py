#!/usr/bin/env python3
"""Generator-independent test: is the structural joint space closed by STRUCTURE
(bounded attractor << combinatorial cap -- 'Dewey') or only closed because authoring
stopped (open/log -- 'colon classification')?

Two territory quantities (not drawing-speed):
  1. realized fraction of the product cap (5 structural marginals), at 772 and v5.
  2. coupon-collector estimator: dS/dn = r*(1 - S/S_max). Regress per-sample discovery
     rate on CURRENT realized count S; the S-axis intercept = S_max. Finite intercept
     well below the cap => bounded structured attractor (strong form witnessed).
Paired with shift's approach to its hard 4096 = 8^4 cap (#types=8 cascade atoms; context grid fixed at 4 pts).
"""
import random, statistics

def rfind_close(s):
    depth=0; start=s.index('(')
    for i in range(start,len(s)):
        if s[i]=='(': depth+=1
        elif s[i]==')':
            depth-=1
            if depth==0: return i
    return len(s)-1

def split_top(s):
    inner=s[s.index('(')+1:rfind_close(s)]
    args,depth,cur=[],0,''
    for ch in inner:
        if ch in '([': depth+=1
        elif ch in ')]': depth-=1
        if ch==',' and depth==0: args.append(cur.strip()); cur=''
        else: cur+=ch
    if cur.strip(): args.append(cur.strip())
    return args

def load(path):
    shifts, cells = [], []
    for line in open(path):
        line=line.strip()
        if not line: continue
        a=split_top(line)                  # shift, props, voids, actors, drift, zone
        shifts.append(a[0]); cells.append(tuple(a[1:]))
    return shifts, cells

def marginals(cells):
    return [len(set(c[i] for c in cells)) for i in range(len(cells[0]))]

def prod(xs):
    p=1
    for x in xs: p*=x
    return p

def rarefy(items, grid, K=80):
    return [(n, statistics.mean(len(set(random.sample(items,n))) for _ in range(K))) for n in grid]

def coupon_fit(curve):
    """Regress dS/dn on midpoint S. Return (S_max estimate, slope, R2)."""
    rates, Smid = [], []
    for (n0,s0),(n1,s1) in zip(curve, curve[1:]):
        rates.append((s1-s0)/(n1-n0)); Smid.append((s0+s1)/2)
    mx,my=statistics.mean(Smid),statistics.mean(rates)
    b=sum((x-mx)*(y-my) for x,y in zip(Smid,rates))/sum((x-mx)**2 for x in Smid)
    a=my-b*mx
    # rate = a + b*S ; rate=0 at S = -a/b = S_max ; r = a
    Smax = -a/b if b!=0 else float('inf')
    ss_res=sum((y-(a+b*x))**2 for x,y in zip(Smid,rates))
    ss_tot=sum((y-my)**2 for y in rates)
    R2=1-ss_res/ss_tot if ss_tot else 0
    return Smax, b, R2

print('=== 1. OCCUPANCY of the combinatorial cap (5 structural marginals) ===')
for label,path in [('current(772)','/tmp/cur_sixdim.txt'),('v5(3380)','/tmp/v5_sixdim.txt')]:
    sh,cells=load(path)
    m=marginals(cells); cap=prod(m); real=len(set(cells))
    print(f'{label:13} marginals(props,voids,actors,drift,zone)={m}  cap={cap:,}  '
          f'realized={real}  fraction={real/cap:.4%}')

print('\n=== 2. COUPON-COLLECTOR estimator on v5 (bounded vs open) ===')
sh,cells=load('/tmp/v5_sixdim.txt')
grid=[100,200,400,600,900,1200,1600,2000,2400,2800,3380]
# structural 5-dim
cur=rarefy([str(c) for c in cells], grid)
Smax,b,R2=coupon_fit(cur)
print(f'STRUCTURAL 5-dim: realized@3380={cur[-1][1]:.0f}  '
      f'coupon S_max≈{Smax:.0f}  (slope={b:.5f}, R²={R2:.3f})')
print('   rate vs n still:', '  '.join(f'{n}:{s:.0f}' for n,s in cur))
# shift
curs=rarefy(sh, grid)
Smax_s,b_s,R2_s=coupon_fit(curs)
print(f'SHIFT (cap 4096=8^4): realized@3380={curs[-1][1]:.0f}  '
      f'coupon S_max≈{Smax_s:.0f}  (slope={b_s:.5f}, R²={R2_s:.3f})  '
      f'=> S_max/cap = {Smax_s/4096:.2%}')
