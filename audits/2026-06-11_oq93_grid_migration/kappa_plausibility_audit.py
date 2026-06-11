#!/usr/bin/env python3
"""OQ-93 kappa plausibility audit — the operator-gated read of the grid batch.

Criterion PRE-WRITTEN in PREREGISTRATION.md; tolerance + N OPERATOR-RULED
2026-06-11 (typed ruling, split by indicator kind):
  - Per-story: ANY indicator firing excludes the story from consumer reads
    (fail-closed, no tolerance). Report k/N with N stated, never "clean".
  - C-echo (prompt property): ZERO tolerance — one firing HALTS the batch.
  - C-flat / C-dir (prompt quality): batch ESCALATES at >= 2/10 firings.
  - C-range firing on a schema-validated story = battery failure, HALT.
  - A passed batch is NECESSARY for the live-prompt flip, not sufficient
    (operator may demand a supplemental batch).

Indicator operationalization (pinned BEFORE the batch was read):
  C-echo:  (a) all 32 values a single constant; (b) any two stories in the
           batch with identical sorted-slot value tuples (the prompt carries
           no worked value table by design — checked here as a positive
           control on the addendum text — so cross-story identity is the
           live form of "the prompt taught a convention").
  C-flat:  for EVERY (metric, time) slot-group the 4 level values span
           < 0.05 — the level axis (the track's unique product) was never
           differentiated.
  C-dir:   engine G_sys (pattern_analysis:analyze_interval on the compiled
           story) has |G| > 0.01 and EVERY authored scalar series with
           |delta| >= 0.1 moves in the OPPOSITE direction (mixed-sign scalar
           evidence exempts; stories with no qualifying series counted
           separately as dir-exempt).
  C-range: any value outside [0,1] or authored slot count != 32.

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/kappa_plausibility_audit.py
"""
import json
import re
import subprocess
import sys
from itertools import combinations
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "audits/2026-06-11_oq93_grid_migration"
BATCH = AUDIT / "grid_batch"

GRID_METRICS = ["accessibility_collapse", "stakes_inflation", "suppression", "resistance"]
LEVELS = ["structural", "organizational", "class", "individual"]


def engine_gsys(pl_path, iid):
    goal = (
        "use_module(narrative_ontology), use_module(config), "
        "use_module(pattern_analysis), "
        f"consult('{pl_path}'), "
        f"pattern_analysis:analyze_interval({iid}, G, C, P), "
        "format('RES G=~w C=~w P=~w~n', [G, C, P]), halt."
    )
    p = subprocess.run(["swipl", "-g", goal, "-t", "halt(1)"],
                       cwd=ROOT / "prolog", capture_output=True, text=True, timeout=180)
    out = [l for l in p.stdout.splitlines() if l.startswith("RES ")]
    if not out:
        return None, None, f"engine error: {p.stderr.strip()[-150:]}"
    parts = dict(kv.split("=", 1) for kv in out[-1][4:].split(" "))
    g = parts["G"]
    return (None if g == "open" else float(g)), parts["P"], None


# Positive control: the addendum must NOT contain a worked grid value table
addendum = (ROOT / "prompts/grid_batch_addendum.md").read_text()
prompt_value_runs = re.findall(r'"value"\s*:\s*0\.\d+', addendum)
control_ok = len(prompt_value_runs) == 0

stories = sorted((BATCH / "json").glob("*.json"))
ledger_path = BATCH / "generation_ledger.json"
gen_ledger = json.loads(ledger_path.read_text()) if ledger_path.exists() else []
gen_ok = {e["cid"] for e in gen_ledger if e.get("status") == "ok"}

rows = []
value_tuples = {}
for f in stories:
    cid = f.stem
    if cid not in gen_ok:
        rows.append({"cid": cid, "verdict": "GEN-FAIL (not audited)", "fired": []})
        continue
    doc = json.loads(f.read_text())
    grid = doc.get("coercion_grid") or {}
    pts = grid.get("points") or []
    fired = []

    # C-range
    n_slots = len({(p["metric"], p["level"], p["time_point"]) for p in pts})
    if n_slots != 32 or any(not (0.0 <= p["value"] <= 1.0) for p in pts):
        fired.append("C-range")

    vals = {(p["metric"], p["level"], p["time_point"]): p["value"] for p in pts}
    tup = tuple(v for _, v in sorted(vals.items()))
    value_tuples[cid] = tup

    # C-echo (a): single constant
    if len(set(tup)) == 1 and tup:
        fired.append("C-echo:constant")

    # C-flat: every (metric,time) group spans < 0.05 across levels
    if n_slots == 32:
        spans = []
        for m in GRID_METRICS:
            for t in (grid["t0"], grid["tn"]):
                group = [vals[(m, lv, t)] for lv in LEVELS]
                spans.append(max(group) - min(group))
        if all(s < 0.05 for s in spans):
            fired.append("C-flat")

    # C-dir
    g, pat, err = engine_gsys(BATCH / "pl" / f"{cid}.pl", cid)
    dir_note = ""
    if err:
        fired.append(f"C-dir:ENGINE-ERROR")
        dir_note = err
    elif g is not None and abs(g) > 0.01:
        deltas = []
        series = {}
        for m in doc.get("measurements") or []:
            series.setdefault(m["metric"], []).append((m["time_point"], m["value"]))
        for metric, pts2 in series.items():
            pts2.sort()
            d = pts2[-1][1] - pts2[0][1]
            if abs(d) >= 0.1:
                deltas.append(d)
        if deltas:
            signs = {d > 0 for d in deltas}
            if len(signs) == 1 and (next(iter(signs)) != (g > 0)):
                fired.append("C-dir")
            dir_note = f"G={g:+.3f} scalar-deltas={[round(d,2) for d in deltas]}"
        else:
            dir_note = f"G={g:+.3f} dir-exempt (no scalar series with |d|>=0.1)"
    else:
        dir_note = f"G={g} pattern={pat}"

    rows.append({"cid": cid, "verdict": "EXCLUDE" if fired else "pass",
                 "fired": fired, "note": dir_note,
                 "gsys": g, "pattern": pat})

# C-echo (b): cross-story identical value tuples
echo_pairs = [(a, b) for a, b in combinations(value_tuples, 2)
              if value_tuples[a] == value_tuples[b]]
for a, b in echo_pairs:
    for r in rows:
        if r["cid"] in (a, b) and "C-echo:cross-story" not in r["fired"]:
            r["fired"].append("C-echo:cross-story")
            r["verdict"] = "EXCLUDE"

n_audited = sum(1 for r in rows if r["verdict"] != "GEN-FAIL (not audited)")
excluded = [r for r in rows if r["verdict"] == "EXCLUDE"]
echo_fired = [r for r in rows if any(x.startswith("C-echo") for x in r["fired"])]
range_fired = [r for r in rows if "C-range" in r["fired"]]
flatdir_fired = [r for r in rows
                 if any(x in ("C-flat", "C-dir") for x in r["fired"])]

print(f"addendum positive control (no worked value table in prompt): "
      f"{'PASS' if control_ok else 'FAIL'}")
print(f"batch: {len(rows)} stories, {n_audited} audited (gen-ok), "
      f"{len(rows) - n_audited} generation failures\n")
for r in rows:
    extra = f" fired={r['fired']}" if r["fired"] else ""
    note = f" [{r.get('note','')}]" if r.get("note") else ""
    print(f"  [{r['verdict']:>8}] {r['cid']}{extra}{note}")

print(f"\nper-story exclusions (fail-closed, no tolerance): "
      f"{len(excluded)}/{n_audited}")
print(f"C-echo firings (ZERO tolerance, batch HALTS): {len(echo_fired)}")
print(f"C-range firings (battery failure, HALT): {len(range_fired)}")
print(f"C-flat/C-dir firings (batch ESCALATES at >=2/10): {len(flatdir_fired)}")

if range_fired:
    print("\nBATCH VERDICT: HALT — C-range fired on schema-validated stories "
          "(battery failure; escalate, do not amend)")
    code = 2
elif echo_fired:
    print("\nBATCH VERDICT: HALT — template echo witnessed; the prompt teaches "
          "a convention (operator ruling: no honest fraction of this)")
    code = 2
elif len(flatdir_fired) >= 2:
    print("\nBATCH VERDICT: ESCALATE — idiosyncratic-failure fraction at/over "
          "the 2/10 line (prompt-quality problem)")
    code = 3
else:
    print(f"\nBATCH VERDICT: PASS — {len(excluded)}/{n_audited} stories "
          f"excluded per-story; batch-level indicators within ruling. "
          f"(Necessary, NOT sufficient, for the live flip — operator call.)")
    code = 0

(AUDIT / "kappa_audit_rows.json").write_text(json.dumps(rows, indent=2, default=str))
sys.exit(code)
