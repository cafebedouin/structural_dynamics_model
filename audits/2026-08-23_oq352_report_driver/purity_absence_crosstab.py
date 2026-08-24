"""Why do 3 of 20 corpora complete? Cross-tab pass/throw against the purity-absence
stratum, using the diagnostic block already on disk plus a direct file scan.

The throw needs a GIANT-COMPONENT MEMBER whose intrinsic purity is non-numeric
(drl_purity_network:230 propagates `unknown` effective from `unknown` intrinsic).
purity_score returns `unknown` when coordination_type is unauthored, so the
prediction is: legs that COMPLETE have no unknown-purity member in the GC.
"""
import sys, json, re
from pathlib import Path
sys.path.insert(0, "python")
import run_pipeline as R

census = Path("audits/2026-08-23_oq352_report_driver/giant_comp_leg_census.txt").read_text()
ok    = set(re.findall(r"^(\S+)\s+n=\d+\s+[\d.]+s\s+rc=0\s+OK", census, re.M))
throw = set(re.findall(r"^(\S+)\s+n=\d+\s+[\d.]+s\s+rc=2\s+THROW", census, re.M))

rows = []
for leg in sorted(ok | throw):
    d = R._resolve_corpus_dir(leg)
    files = sorted(d.glob("*.pl"))
    n = len(files)
    # direct scan: how many stories lack an authored coordination_type
    no_ct = sum(1 for f in files
                if not re.search(r"coordination_type", f.read_text(encoding="utf-8", errors="replace")))
    # diagnostic block, where a classify output exists
    name = R._classify_output_name(d.name)
    p = R.OUTPUTS_DIR / name
    nd = ns = nt = gf = None
    if p.exists():
        try:
            dg = json.loads(p.read_text())["diagnostic"]
            nd, ns, nt = dg.get("purity_n_no_data"), dg.get("purity_n_scored"), dg.get("purity_n_total")
            gf = dg.get("purity_n_gate_fail")
        except Exception:
            pass
    rows.append((leg, "OK" if leg in ok else "THROW", n, no_ct, nd, ns, nt, gf))

print(f"{'leg':32s} {'gc':6s} {'n':>6s} {'no_coord_type':>14s} {'purity_no_data':>15s} "
      f"{'scored':>7s} {'gate_fail':>10s}")
print("-"*100)
for leg, st, n, no_ct, nd, ns, nt, gf in rows:
    print(f"{leg:32s} {st:6s} {n:6d} {no_ct:14d} {str(nd):>15s} {str(ns):>7s} {str(gf):>10s}")

print("\n--- cross-tab: does 'no story lacks coordination_type' predict completion? ---")
for st in ("OK", "THROW"):
    sel = [r for r in rows if r[1] == st]
    zero = [r[0] for r in sel if r[3] == 0]
    nz   = [(r[0], r[3]) for r in sel if r[3] > 0]
    print(f"  {st:6s}: {len(sel)} legs — no_coord_type==0 on {len(zero)}: {zero}")
    if nz: print(f"            no_coord_type>0 on {len(nz)}: {nz[:8]}")
