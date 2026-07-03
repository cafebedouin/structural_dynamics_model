#!/usr/bin/env python3
"""ε census analysis (OQ-205 spec recon): distance-to-threshold histogram per
candidate radius + OQ-78 grid stats, with the planted-control check FIRST.

Control logic: census_planted_control sits at snare_epsilon_floor + 0.0005.
It must be flagged (within r of a threshold) at EVERY candidate radius >= 0.0005,
else the sweep's "no values near threshold" is indistinguishable from not looking.
"""
import sys
from collections import Counter
from pathlib import Path

SP = Path(__file__).parent
# ε thresholds that gate classification paths (config.pl at HEAD 6c59615e):
# piton_epsilon_floor 0.10, mountain_extractiveness_max 0.25,
# tangled_rope_epsilon_floor 0.30, rope_epsilon_ceiling 0.45 (== scaffold/piton
# extraction ceilings), snare_epsilon_floor 0.46.
THRESHOLDS = [0.10, 0.25, 0.30, 0.45, 0.46]
RADII = [0.001, 0.002, 0.005, 0.01, 0.02, 0.05]
PLANT_ID = "census_planted_control"
PLANT_DELTA = 0.0005

def load(leg):
    vals, no_eps, plant = {}, [], None
    for line in (SP / f"eps_{leg}.tsv").read_text().splitlines():
        if "\t" not in line:
            continue  # stack init banner lines on stdout
        cid, raw = line.split("\t")
        if raw == "no_eps":
            no_eps.append(cid); continue
        v = float(raw)
        if cid == PLANT_ID:
            plant = v; continue
        vals[cid] = v
    return vals, no_eps, plant

def mindist(v):
    return min(abs(v - t) for t in THRESHOLDS)

# --- Control first (live leg carries the plant) ---
live_vals, live_noeps, plant = load("live")
if plant is None:
    print("CONTROL FAIL: planted value absent from live dump — sweep never saw it. HALT.")
    sys.exit(1)
ctl_fail = [r for r in RADII if r >= PLANT_DELTA and not (mindist(plant) <= r)]
if ctl_fail:
    print(f"CONTROL FAIL: plant at {plant} NOT flagged at radii {ctl_fail}. HALT.")
    sys.exit(1)
print(f"CONTROL PASS: plant at {plant} (snare_epsilon_floor+{PLANT_DELTA}) flagged at all "
      f"{len([r for r in RADII if r >= PLANT_DELTA])} candidate radii >= {PLANT_DELTA}; excluded from stats below.")
print()

legs = {"live": (live_vals, live_noeps)}
for leg in ("haiku", "flash", "kernel_v1"):
    v, n, _ = load(leg)
    legs[leg] = (v, n)

for leg, (vals, noeps) in legs.items():
    n = len(vals)
    print(f"=== {leg}: n={n} with ε, {len(noeps)} no_eps ===")
    if noeps and len(noeps) <= 12:
        print(f"  no_eps: {sorted(noeps)}")
    dists = sorted(mindist(v) for v in vals.values())
    print(f"  min |ε−threshold|: min={dists[0]:.3f} p10={dists[n//10]:.3f} median={dists[n//2]:.3f}")
    for r in RADII:
        within = sum(1 for d in dists if d <= r)
        print(f"  r={r:<6} within r of a threshold: {within:>4} ({100*within/n:.1f}%)")
    c = Counter(vals.values())
    mode_val, mode_n = c.most_common(1)[0]
    print(f"  distinct values: {len(c)}; mode: {mode_val} x{mode_n} ({100*mode_n/n:.1f}%)")
    last = Counter(f"{v:.2f}"[-1] for v in vals.values())
    print(f"  last-digit histogram: {dict(sorted(last.items(), key=lambda kv: -kv[1]))}")
    on_grid = sum(1 for v in vals.values() if abs(v*100 - round(v*100)) < 1e-9 and round(v*100) % 10 in (8, 5, 2, 0))
    exact = sorted(c.items(), key=lambda kv: -kv[1])[:6]
    print(f"  top values: {exact}")
    print()
