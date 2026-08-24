#!/usr/bin/env python3
"""C2 — kernel-sibling density per pure pair's FIRST leg, plus R1's feasibility filter."""
import re, glob, os, json, sys, collections
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PAIRS = [("testsets_flash2","testsets_flash3"), ("testsets_flash_think","testsets_flash_think2"),
         ("testsets_haiku2","testsets_haiku3"), ("testsets_sonnet2","testsets_sonnet3"),
         ("testsets_stealth2","testsets_stealth3")]
# feasibility: parsed from OQ-356's census, not asserted here
CENSUS = os.path.join(ROOT,"audits/2026-08-23_oq352_report_driver/giant_comp_leg_census.txt")
gc = {}
for ln in open(CENSUS, encoding="utf-8"):
    m = re.match(r"^(\S+)\s+n=(\d+)\s+([\d.]+)s\s+rc=(\d+)\s+(\w+)", ln)
    if m: gc[m.group(1)] = (int(m.group(4)), m.group(5))

FACT = re.compile(r"^narrative_ontology:cs_kernel_id\(\s*([^,]+?)\s*,\s*([^)]+?)\s*\)\.\s*$")
def leg_stats(leg):
    d = os.path.join(ROOT,"prolog",leg)
    files = sorted(glob.glob(os.path.join(d,"*.pl")))
    per_kernel = collections.Counter(); n_fact = 0
    for f in files:
        for ln in open(f, encoding="utf-8", errors="replace"):
            m = FACT.match(ln)
            if m: per_kernel[m.group(2)] += 1; n_fact += 1
    return dict(leg=leg, stories=len(files), fact_lines=n_fact,
                kernels_total=len(per_kernel),
                kernels_ge2=sum(1 for v in per_kernel.values() if v>=2),
                readings_per_kernel=round(len(files)/len(per_kernel),3) if per_kernel else None)

rows=[]
for a,b in PAIRS:
    s=leg_stats(a); rc_a=gc.get(a,(None,"?")); rc_b=gc.get(b,(None,"?"))
    s["pair"]=f"{a}/{b}"; s["gc_a"]=f"rc={rc_a[0]} {rc_a[1]}"; s["gc_b"]=f"rc={rc_b[0]} {rc_b[1]}"
    s["gc_pair_completes"]= (rc_a[0]==0 and rc_b[0]==0)
    rows.append(s)

print("=== C2: kernel-sibling density, all five pure pairs (FIRST leg), + giant_comp feasibility ===")
hdr=f"{'first leg':<22}{'stories':>8}{'k_total':>9}{'k_ge2':>7}{'st/kern':>9}  {'gc(a)':<12}{'gc(b)':<12}{'FEASIBLE':>9}"
print(hdr); print("-"*len(hdr))
for r in sorted(rows, key=lambda r:(-r["kernels_ge2"], -r["stories"], r["leg"])):
    print(f"{r['leg']:<22}{r['stories']:>8}{r['kernels_total']:>9}{r['kernels_ge2']:>7}"
          f"{r['readings_per_kernel']:>9}  {r['gc_a']:<12}{r['gc_b']:<12}{str(r['gc_pair_completes']):>9}")

print("\n=== SELECTION ===")
unf = sorted(rows, key=lambda r:(-r["kernels_ge2"], -r["stories"], r["leg"]))
print(f"  density rule ALONE (rank k_ge2, tie by stories, tie by name) would pick: {unf[0]['pair']}  [first leg {unf[0]['leg']}]")
feas = [r for r in rows if r["gc_pair_completes"]]
fs = sorted(feas, key=lambda r:(-r["kernels_ge2"], -r["stories"], r["leg"]))
print(f"  feasibility-filtered set (giant_comp rc=0 on BOTH legs): {[r['pair'] for r in feas] or 'EMPTY'}")
print(f"  R1 selection: {fs[0]['pair'] if fs else 'NONE'}  [first leg {fs[0]['leg'] if fs else '-'}]")
print(f"  ties at top of feasible set after tie-break? "
      f"{'YES — STOP AND ASK' if len(fs)>1 and (fs[0]['kernels_ge2'],fs[0]['stories'])==(fs[1]['kernels_ge2'],fs[1]['stories']) else 'no'}")
print("\n=== DEGENERACY CHECK (R1.1) ===")
sat=[r for r in rows if r["kernels_ge2"]==r["kernels_total"]]
print(f"  legs where k_ge2 == k_total (metric SATURATED at ceiling): {[r['leg'] for r in sat]}")
print(f"  legs below ceiling: {[(r['leg'],r['kernels_ge2'],r['kernels_total']) for r in rows if r not in sat]}")
print(f"  stories/kernel range across pure-pair first legs: "
      f"{min(r['readings_per_kernel'] for r in rows)} .. {max(r['readings_per_kernel'] for r in rows)}")
json.dump(rows, open(os.path.join(ROOT,"audits/2026-08-24_oq353_statistic_floors/c2_density.json"),"w"), indent=1)
