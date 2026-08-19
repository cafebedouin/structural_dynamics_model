#!/usr/bin/env python3
"""OQ-302 Phase-2 analysis. Reads tsv_<tag>/*.tsv, emits the pre-registered readout.

Every number here is keyed on the ARM-NAMED columns (result__arm_defect /
result__arm_repaired) and on source_arm, never on column position — PREREG 6.1.
"""
import sys, csv, collections, pathlib

TAG = sys.argv[1] if len(sys.argv) > 1 else "phase2"
DIR = pathlib.Path(__file__).resolve().parent
LEGS = ["testsets", "testsets_haiku", "testsets_flash", "testsets_kimi",
        "testsets_sonnet", "archives_datasets_kernel_v1"]

INCONC = "inconclusive(insufficient_data)"
rows_by_leg = {}
for leg in LEGS:
    p = DIR / f"tsv_{TAG}" / f"{leg}.tsv"
    if not p.exists():
        print(f"MISSING: {p}"); continue
    with p.open() as f:
        rows_by_leg[leg] = list(csv.DictReader(f, delimiter="\t"))

out = []
W = out.append
W(f"# OQ-302 Phase-2 readout — tag={TAG}")
W("")
W("| leg | N | source_arm | N_reaching | fires-control | declines-control | T4 pass | throws | agg mismatch |")
W("|---|---:|---|---:|---|---|---:|---:|---:|")

totals = collections.Counter()
per_leg = {}
for leg, rows in rows_by_leg.items():
    n = len(rows)
    arms = {r["source_arm"] for r in rows}
    arm = arms.pop() if len(arms) == 1 else f"MIXED{sorted(arms)}"
    reaching = [r for r in rows if r["eac_value"] == "true"]
    not_reaching = [r for r in rows if r["eac_value"] == "false"]
    other_eac = [r for r in rows if r["eac_value"] not in ("true", "false")]

    if arm == "defect":
        fires_col = "result__arm_defect"
        fires_n = sum(1 for r in rows if r[fires_col] == INCONC)
        fires = f"{fires_n}/{n} = {100.0*fires_n/n:.1f}%"
        fires_ok = fires_n == n
    else:
        fires = "n/a (source is repaired)"
        fires_ok = None

    dec_n = sum(1 for r in not_reaching if r["result__arm_repaired"] == INCONC)
    if not not_reaching:
        declines = "NO SUBJECT — untested guard (scoped residue)"
    else:
        declines = f"{dec_n}/{len(not_reaching)} still {INCONC}"

    t4pass = sum(1 for r in rows if r["T4"].startswith("pass"))
    throws = sum(1 for r in rows for k in ("eac_value","result__arm_defect",
                 "result__arm_repaired","engine_result","recon_repaired",
                 "T1","T2","T3","T4") if r[k].startswith("ERROR:"))
    mism = sum(1 for r in rows if r["agg_check"] == "MISMATCH")

    per_leg[leg] = dict(n=n, arm=arm, reaching=len(reaching),
                        not_reaching=len(not_reaching), other_eac=len(other_eac),
                        fires_ok=fires_ok, dec_n=dec_n, t4pass=t4pass,
                        throws=throws, mism=mism, rows=rows)
    totals["n"] += n; totals["reaching"] += len(reaching)
    totals["t4pass"] += t4pass; totals["throws"] += throws; totals["mism"] += mism

    nr = len(reaching)
    nr_s = f"{nr}" + ("  **UNMEASURED (<=1)**" if nr <= 1 else "")
    W(f"| `{leg}` | {n} | {arm} | {nr_s} | {fires} | {declines} | {t4pass} | {throws} | {mism} |")

W("")
W(f"**Totals:** {totals['n']} constraints over {len(per_leg)} legs; "
  f"N_reaching {totals['reaching']}; T4 pass {totals['t4pass']}; "
  f"throws {totals['throws']}; agg mismatches {totals['mism']}.")
W("")

# --- payload variation, per leg, over the N_reaching complement only ---------
W("## T1-T3 variation over the N_reaching set (PREREG 3)")
W("")
W("| leg | N_reaching | distinct (T1,T2,T3) | distinct T1 | distinct T2 | distinct T3 | distinct repaired Result | verdict |")
W("|---|---:|---:|---:|---:|---:|---:|---|")
for leg, d in per_leg.items():
    reaching = [r for r in d["rows"] if r["eac_value"] == "true"]
    nr = len(reaching)
    tup = {(r["T1"], r["T2"], r["T3"]) for r in reaching}
    t1 = {r["T1"] for r in reaching}; t2 = {r["T2"] for r in reaching}
    t3 = {r["T3"] for r in reaching}
    res = {r["result__arm_repaired"] for r in reaching}
    if nr <= 1:
        v = "**UNMEASURED**"
    elif len(tup) >= 2:
        v = "**vary**"
    else:
        v = "uniform"
    W(f"| `{leg}` | {nr} | {len(tup)} | {len(t1)} | {len(t2)} | {len(t3)} | {len(res)} | {v} |")
W("")

# --- verdict marginals ------------------------------------------------------
W("## Verdict marginals (whole leg, both arms)")
W("")
for leg, d in per_leg.items():
    dc = collections.Counter(r["result__arm_defect"].split("(")[0] for r in d["rows"])
    rc = collections.Counter(r["result__arm_repaired"].split("(")[0] for r in d["rows"])
    W(f"- `{leg}`: arm(defect) {dict(dc)} | arm(repaired) {dict(rc)}")
W("")

# --- T4 marginal ------------------------------------------------------------
W("## T4 marginal (escalation clause, PREREG 4)")
W("")
t4 = collections.Counter()
for d in per_leg.values():
    for r in d["rows"]:
        t4[r["T4"]] += 1
for k, v in t4.most_common():
    W(f"- `{k}`: {v}")
W("")

# --- throw / error census ---------------------------------------------------
W("## Error cells, per column (PREREG 6.4)")
W("")
cols = ["eac_value","result__arm_defect","result__arm_repaired","engine_result",
        "recon_repaired","T1","T2","T3","T4"]
ec = collections.Counter()
fc = collections.Counter()
for d in per_leg.values():
    for r in d["rows"]:
        for c in cols:
            if r[c].startswith("ERROR:"): ec[c] += 1
            if r[c] == "FAIL": fc[c] += 1
W(f"- thrown-error cells: {dict(ec) if ec else 'NONE (0 across every column, every leg)'}")
W(f"- failed (no-solution) cells: {dict(fc) if fc else 'NONE'}")
W("")
W("## agg_check (PREREG 0b — the per-test transcription cross-check)")
W("")
ac = collections.Counter()
for d in per_leg.values():
    for r in d["rows"]:
        ac[r["agg_check"]] += 1
for k, v in ac.most_common():
    W(f"- `{k}`: {v}")

print("\n".join(out))
