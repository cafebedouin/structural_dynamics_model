#!/usr/bin/env python3
"""clause_order_census.py — driver for the steal-risk census over the class-B worklist.

Feeds clause_order_census.pl the `latent-B` rows from python/dispatch_head_check.py's
DECLARED (imported, not copied) and ENFORCES the pre-registered discrimination check before
printing any table: the census must reproduce signature_grade/2's known asymmetry
(steal_risk 0 at `correction`, > 0 at `commentary`), which Unit A measured on five legs.
A census that cannot reproduce that split is measuring something else and its zeros license
nothing — so this fails closed rather than reporting.

The control is NATURALLY ARISING: the split comes from a real predicate and a real five-leg
measurement, not from a planted fixture. It has already paid for itself once — the first
version of steal_risk/4 reported 0 for BOTH atoms (it treated a variable-headed cut-bearing
clause as an unconditional commit) and this check refused the run.
"""
from __future__ import annotations

import re
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(REPO / "python"))
from dispatch_head_check import DECLARED  # noqa: E402

CENSUS_PL = HERE / "clause_order_census.pl"
ROW_RE = re.compile(
    r"^COC_ROW: (\S+) (\S+/\d+) atom=(\S+) steal_risk=(\d+) skipped=(\S*) nclauses=(\d+)")
PRED_RE = re.compile(r"^COC_PRED: (\S+) (\S+/\d+) atoms=(\d+) max_steal_risk=(\d+)")

# Pre-registered, naturally-arising control (PREREGISTRATION §3).
CONTROL = ("signature_detection.pl", "signature_grade/2")
CONTROL_EXPECT = {"correction": 0}          # must be exactly 0
CONTROL_EXPECT_POSITIVE = {"commentary"}    # must be > 0


def run(specs: list[tuple[str, str]]) -> tuple[list[dict], list[dict], str]:
    with tempfile.NamedTemporaryFile("w", suffix=".spec", delete=False) as fh:
        for f, pi in specs:
            fh.write(f"{f} {pi}\n")
        path = fh.name
    proc = subprocess.run(
        ["swipl", "-q", "-l", str(CENSUS_PL),
         "-g", f"run_clause_order_census('{path}'), halt", "-t", "halt(1)"],
        cwd=REPO, capture_output=True, text=True, timeout=600)
    Path(path).unlink(missing_ok=True)
    rows, preds, scanned = [], [], 0
    for ln in proc.stdout.splitlines():
        if (m := ROW_RE.match(ln)):
            rows.append({"file": m.group(1), "pi": m.group(2), "atom": m.group(3),
                         "steal_risk": int(m.group(4)), "skipped": m.group(5),
                         "nclauses": int(m.group(6))})
        elif (m := PRED_RE.match(ln)):
            preds.append({"file": m.group(1), "pi": m.group(2),
                          "atoms": int(m.group(3)), "max_steal_risk": int(m.group(4))})
        elif ln.startswith("COC_SCANNED:"):
            scanned = int(ln.split()[1])
    if proc.returncode != 0 or scanned == 0:
        raise SystemExit(f"clause_order_census: RED — census did not complete "
                         f"(rc={proc.returncode}, scanned={scanned}). {proc.stderr[-500:]}")
    return rows, preds, proc.stdout


def check_control(rows: list[dict]) -> list[str]:
    problems = []
    got = {r["atom"]: r["steal_risk"] for r in rows
           if (r["file"], r["pi"]) == CONTROL}
    if not got:
        return [f"CONTROL {CONTROL[1]} produced no rows — the census did not look"]
    for atom, want in CONTROL_EXPECT.items():
        if got.get(atom) != want:
            problems.append(
                f"CONTROL {CONTROL[1]} @ {atom}: steal_risk={got.get(atom)}, expected {want}. "
                f"Unit A measured bound == once+== on all five legs at this atom; a census "
                f"that disagrees is not measuring the same thing.")
    for atom in CONTROL_EXPECT_POSITIVE:
        if not got.get(atom, 0) > 0:
            problems.append(
                f"CONTROL {CONTROL[1]} @ {atom}: steal_risk={got.get(atom)}, expected > 0. "
                f"Unit A measured the bound form diverging by 29-167 constraints per leg at "
                f"this atom; a census reporting 0 here is reporting a didn't-look as a clean "
                f"result — the exact failure this control exists to catch (and did, once).")
    return problems


def main() -> int:
    latentb = sorted(k for k, v in DECLARED.items() if v == "latent-B")
    specs = sorted(set(latentb) | {CONTROL})
    rows, preds, raw = run(specs)

    problems = check_control(rows)
    if problems:
        for p in problems:
            print(f"  {p}")
        print("clause_order_census: RED — discrimination control failed; no zero in this "
              "run is readable")
        return 1

    (HERE / "clause_order_census_raw.txt").write_text(raw)
    lb = [p for p in preds if (p["file"], p["pi"]) in set(latentb)]
    assert len(lb) == len(latentb), f"{len(lb)} predicate rows != {len(latentb)} latent-B"

    at_risk = [p for p in lb if p["max_steal_risk"] > 0]
    risky_rows = [r for r in rows
                  if (r["file"], r["pi"]) in set(latentb) and r["steal_risk"] > 0]

    md = ["# clause-order steal-risk census — the 55 `converts-clean` rows",
          "",
          "GENERATED by `clause_order_census.py` (do not hand-edit). Executed 2026-08-18.",
          "",
          "`steal_risk(P, A)` = cut-bearing clauses of `P` whose head output arg is an atom",
          "≠ `A`, appearing before the LAST clause that can yield `A`. Upper bound by design:",
          "whether the skipped clause's body would actually have succeeded is not statically",
          "decidable, so a nonzero count means *could steal*, not *does steal*. A zero is a",
          "real zero.",
          "",
          "**Control (pre-registered, naturally arising, enforced before this table prints):**",
          "`signature_grade/2` @ `correction` = 0 and @ `commentary` > 0, matching the",
          "five-leg agreement measurement in Unit A. It has fired once already, on the first",
          "version of this census.",
          "",
          f"## Result: {len(at_risk)} of {len(latentb)} latent-B predicates carry a nonzero",
          f"steal-risk at some atom; {len(risky_rows)} (predicate, atom) pairs in total.",
          ""]
    if at_risk:
        md += ["| file | predicate | atoms | max steal-risk |", "|---|---|---|---|"]
        for p in sorted(at_risk, key=lambda p: -p["max_steal_risk"]):
            md.append(f"| `{p['file']}` | `{p['pi']}` | {p['atoms']} | {p['max_steal_risk']} |")
        md += ["", "### Per (predicate, atom)", "",
               "| file | predicate | atom | steal-risk | skipped atoms |", "|---|---|---|---|---|"]
        for r in sorted(risky_rows, key=lambda r: (-r["steal_risk"], r["file"], r["pi"])):
            md.append(f"| `{r['file']}` | `{r['pi']}` | `{r['atom']}` | {r['steal_risk']} | "
                      f"`{r['skipped']}` |")
    else:
        md.append("No latent-B predicate carries a nonzero steal-risk at any atom.")
    (HERE / "clause_order_census.md").write_text("\n".join(md) + "\n")

    print(f"clause_order_census: control OK (signature_grade/2 correction=0, commentary="
          f"{[r['steal_risk'] for r in rows if (r['file'], r['pi']) == CONTROL and r['atom'] == 'commentary'][0]})")
    print(f"  {len(latentb)} latent-B predicates, {len(rows) - 2} (pred, atom) pairs")
    print(f"  {len(at_risk)} predicate(s) with nonzero steal-risk at some atom")
    print(f"  {len(risky_rows)} (pred, atom) pair(s) with nonzero steal-risk")
    print(f"  wrote clause_order_census.md, clause_order_census_raw.txt")
    return 0


if __name__ == "__main__":
    sys.exit(main())
