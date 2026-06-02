#!/usr/bin/env python3
"""Rarefaction of six-dim structural fingerprint classes, single generator (v5).

Holds the generator fixed (cross-corpus counts are generator-confounded: 197->66,
772->161, 1150->300, 3380->199 is non-monotone). Subsamples n stories K times,
averages distinct-class count, fits a saturating model to estimate (a) the asymptote
S_max and (b) the N at which discovery effectively stops -> the "stability" point past
which corpus change is drift over a fixed class set, not new structure.
"""
import random, statistics

fps = [l.strip() for l in open('/tmp/v5_sixdim.txt') if l.strip()]
Ntot = len(fps)
K = 40
grid = [50,100,200,300,500,750,1000,1500,2000,2500,3000,Ntot]

def rarefy(n):
    vals = []
    for _ in range(K):
        vals.append(len(set(random.sample(fps, n))))
    return statistics.mean(vals)

curve = [(n, rarefy(n)) for n in grid]
print(f'rarefaction on v5 (Ntot={Ntot}, K={K} shuffles)')
print(f'{"n":>6} {"E[distinct]":>12} {"new/1k stories":>16}')
prev=None
for n,s in curve:
    if prev:
        rate = (s-prev[1])/(n-prev[0])*1000
        print(f'{n:>6} {s:>12.1f} {rate:>16.2f}')
    else:
        print(f'{n:>6} {s:>12.1f} {"-":>16}')
    prev=(n,s)

# Fit Michaelis-Menten S(n) = Smax * n / (n + Kc) via linearization 1/S = 1/Smax + (Kc/Smax)/n
xs = [1.0/n for n,_ in curve]
ys = [1.0/s for _,s in curve]
mx, my = statistics.mean(xs), statistics.mean(ys)
b = sum((x-mx)*(y-my) for x,y in zip(xs,ys)) / sum((x-mx)**2 for x in xs)
a = my - b*mx
Smax = 1.0/a
Kc = b*Smax
print(f'\nMichaelis-Menten fit:  S_max = {Smax:.0f}   K (half-saturation n) = {Kc:.0f}')
# n where S reaches fractions of Smax:  S=f*Smax => n = Kc*f/(1-f)
for f in (0.90,0.95,0.99):
    print(f'  reaches {f:.0%} of S_max at n = {Kc*f/(1-f):,.0f}')
# log model for contrast S = a2 + b2*ln(n) (no asymptote)
import math
lx = [math.log(n) for n,_ in curve]
b2 = sum((x-statistics.mean(lx))*(y-statistics.mean([s for _,s in curve])) for x,(_,y) in zip(lx,curve))/sum((x-statistics.mean(lx))**2 for x in lx)
a2 = statistics.mean([s for _,s in curve]) - b2*statistics.mean(lx)
print(f'\nlog model (no asymptote): S = {a2:.1f} + {b2:.1f}*ln(n)')
print(f'  => at n=1e6: MM gives ~{Smax*1e6/(1e6+Kc):.0f};  log gives ~{a2+b2*math.log(1e6):.0f}')
print(f'  => at n=1e9: MM gives ~{Smax*1e9/(1e9+Kc):.0f};  log gives ~{a2+b2*math.log(1e9):.0f}')
