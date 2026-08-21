#!/usr/bin/env python3
"""Paired same-seed agreement across gemini-2.5-flash legs (2026-08-21).

Legs (all classified at ONE engine commit via classify_corpus, outputs/pipeline_output.<leg>.json):
  flash        original leg, 2026-06-13, schema 2e9dff2f (pre `stakeholders` gate), thinking off
  flash2       redraw at schema 685ed7cf, thinking off
  flash3       second same-commit redraw, thinking off          <- the CLEAN redraw floor (flash2 vs flash3)
  flash_think  thinking_budget=8192 at schema 685ed7cf           <- the regime contrast (flash2 vs flash_think)
  flash_think2 redraw of flash_think                             <- thinking-regime redraw floor

Usage: python3 audits/2026-08-21_flash_regime_vs_redraw/paired_agreement.py [leg ...]
Prints per-leg marginals then pairwise same-seed agreement on computed (h1_band, verdict_join,
signature, purity_band) and authored (claimed_type, base_extractiveness) fields. Join key is the
filename-derived `id` — the twin legs' only pairing structure (GAP-35).
"""
import collections, itertools, json, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
LEGS = sys.argv[1:] or ["flash", "flash2", "flash3", "flash_think", "flash_think2"]
FIELDS = {"h1_band": lambda r: r.get("h1_band"),
          "verdict": lambda r: (r.get("verdict_join") or {}).get("verdict"),
          "signature": lambda r: r.get("signature"),
          "purity_band": lambda r: r.get("purity_band"),
          "claimed_type(auth)": lambda r: r.get("claimed_type"),
          "epsilon(auth)": lambda r: r.get("base_extractiveness")}
def load(leg):
    p = ROOT / "outputs" / f"pipeline_output.{leg}.json"
    if not p.exists():
        print(f"  [{leg}] no output at {p.relative_to(ROOT)} — skipped"); return None
    d = json.load(open(p)); return d["manifest"], {r["id"]: r for r in d["per_constraint"]}
data = {l: load(l) for l in LEGS}; data = {l: v for l, v in data.items() if v}
commits = {m["code_commit_short"] for m, _ in data.values()}
print(f"engine commits present: {sorted(commits)}" + ("  <-- NOT coherent; reclassify" if len(commits) > 1 else "  (coherent)"))
for leg, (m, rows) in data.items():
    n = len(rows); h1 = collections.Counter(r.get("h1_band") for r in rows.values())
    vj = collections.Counter((r.get("verdict_join") or {}).get("verdict") for r in rows.values())
    print(f"{leg:13} n={n:4} | h1 band3={h1.get(3,0)/n:.0%} band0={h1.get(0,0)/n:.0%} null={h1.get(None,0)/n:.0%} "
          f"| red={vj.get('red',0)/n:.1%} green={vj.get('green',0)/n:.1%}")
print(f"\n{'pair':28} {'n':>4} " + " ".join(f"{k:>19}" for k in FIELDS))
for a, b in itertools.combinations(data, 2):
    x, y = data[a][1], data[b][1]; ids = [i for i in x if i in y]
    if not ids: continue
    vals = [sum(1 for i in ids if f(x[i]) == f(y[i])) / len(ids) for f in FIELDS.values()]
    print(f"{a+' vs '+b:28} {len(ids):4} " + " ".join(f"{v:>18.0%} " for v in vals))
