#!/usr/bin/env python3
"""partition.py — the OQ-303(a) latent-B partition, both instrument columns.

Runs BOTH arms of the bound-caller instrument pair over the same registry and joins them
per row. Neither arm is reimplemented here:
  * regex arm    — audits/2026-08-17_bound_dispatch_hardening/caller_sweep.py, executed as
                   a subprocess and parsed. Its blind spots are its own (caller_sweep.py:44,
                   72-73): one physical line, no nested-term arguments, bare lowercase atoms,
                   name/arity textual matching with no module resolution.
  * codewalk arm — python/codewalk_caller_check.py --json, run TWICE (evaluate true/false).
                   The difference between those two runs is exactly the set of sites whose
                   selector is bound by a unification rather than written at the call.

Worklist size is read from python/dispatch_head_check.py's DECLARED (imported, not copied),
and the emitted row count is ASSERTED equal to it — a partition over a hand-copied N
verifies nothing.

Writes partition.tsv (machine) and partition.md (the receiver's worklist).
"""
from __future__ import annotations

import json
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(REPO / "python"))
from dispatch_head_check import DECLARED  # noqa: E402

PY = sys.executable
REGEX_SWEEP = REPO / "audits" / "2026-08-17_bound_dispatch_hardening" / "caller_sweep.py"
CODEWALK = REPO / "python" / "codewalk_caller_check.py"

# ---------------------------------------------------------------------------
# ADJUDICATION overlay. The machine assigns a disposition from the two columns; a
# DISAGREEMENT is a question, not a verdict, and the prereg says `not-latent` rows are
# adjudicated before conversion. These three are the adjudications, each with the read that
# produced it. Kept HERE, in the regenerating script, rather than hand-edited into the TSV —
# a hand-edited output is a second copy of the partition (Pattern 2).
# ---------------------------------------------------------------------------
ADJUDICATED: dict[tuple[str, str], tuple[str, str]] = {
    ("drl_composition.pl", "composition_rule/3"): (
        "converts-clean",
        "Regex arm reports 5 bound sites; ALL FIVE ARE PROSE inside block comments "
        "(dirac_classification.pl comment spans 224-239 and 429-455, verified by reading the "
        "delimiters 2026-08-18). caller_sweep.py's is_comment() skips only lines beginning "
        "`%` or `*`, and these begin `behavior:` / `- composition_rule(`. Already adjudicated "
        "the same way at audits/2026-08-17_bound_dispatch_hardening/RECON.md:95. The codewalk "
        "arm's 0 is the correct reading; the disagreement is a regex FALSE POSITIVE."),
    ("signature_detection.pl", "claimed_natural/2"): (
        "converts-clean",
        "Regex arm reports 3 bound sites, all REAL, all inside a Prolog goal string embedded "
        "in python/audits/oq49_override_remeasure.py:73,75,93 (first committed 2026-06-14, so "
        "present at the 2026-08-17 sweep). The codewalk arm structurally cannot see goal "
        "strings in .py files — THIS IS THE REGEX ARM'S GENUINE UNIQUE CAPABILITY, witnessed. "
        "Adjudicated class B with note at RECON.md:96 (point-in-time audit probe, no live "
        "output path). Conversion must update that probe in the same change."),
    ("signature_detection.pl", "signature_grade/2"): (
        "live-output-path",
        "NEW — never adjudicated. Codewalk finds a bound caller the regex scored 0 for "
        "(caller_sweep_output.txt:251): signature_detection.pl:1951, "
        "`signature_severity(C, moderate) :- signature_grade(C, correction).` The call is the "
        "clause's LAST goal on its own line, so caller_sweep.py's is_clause_head() heuristic "
        "reads the terminating `.` as a fact and skips it. The call existed at the census HEAD "
        "(git show 9a5d8526:prolog/signature_detection.pl -> line 1901), so this is a true "
        "false negative of the regex arm at sweep time, not a later addition. It is on a LIVE "
        "OUTPUT PATH: signature_severity/2 -> diagnostic_summary:join_alerts/2:749 -> the "
        "OQ-98 verdict_join headline. Conversion owes the full six-leg clean-vs-edited pair."),
}

HDR_RE = re.compile(r"^== (\S+)/(\d+) \((\S+)\): (\d+) bound call site\(s\)")
SITE_RE = re.compile(r"^\s+(\S+):(\d+)\s+atom=(\S+)\s+\|")


def regex_arm() -> tuple[dict[tuple[str, str], int], dict[tuple[str, str], list[str]], str]:
    proc = subprocess.run([PY, str(REGEX_SWEEP)], cwd=REPO, capture_output=True,
                          text=True, timeout=900)
    if proc.returncode != 0:
        raise SystemExit(f"partition: RED — regex arm failed: {proc.stderr[-500:]}")
    counts: dict[tuple[str, str], int] = {}
    sites: dict[tuple[str, str], list[str]] = {}
    key = None
    for ln in proc.stdout.splitlines():
        if (m := HDR_RE.match(ln)):
            key = (m.group(3), f"{m.group(1)}/{m.group(2)}")
            counts[key] = int(m.group(4))
            sites[key] = []
        elif key and (m := SITE_RE.match(ln)):
            sites[key].append(f"{m.group(1)}:{m.group(2)} atom={m.group(3)}")
    if not counts:
        raise SystemExit("partition: RED — regex arm yielded 0 predicates (broken parse)")
    return counts, sites, proc.stdout


def codewalk_arm(evaluate: bool) -> dict:
    cmd = [PY, str(CODEWALK), "--json"] + ([] if evaluate else ["--evaluate", "false"])
    proc = subprocess.run(cmd, cwd=REPO, capture_output=True, text=True, timeout=900)
    if not proc.stdout.strip():
        raise SystemExit(f"partition: RED — codewalk arm produced nothing: {proc.stderr[-500:]}")
    return json.loads(proc.stdout)


def main() -> int:
    latentb = sorted(k for k, v in DECLARED.items() if v == "latent-B")
    n_latentb = len(latentb)

    rx_counts, rx_sites, rx_raw = regex_arm()
    cw_t = codewalk_arm(True)
    cw_f = codewalk_arm(False)

    if cw_t["control_problems"]:
        # PREREGISTRATION §4: a zero from this arm is unreadable if the controls did not
        # both fire in the same run. Fail closed rather than emit an uninterpretable table.
        for p in cw_t["control_problems"]:
            print(f"  CONTROL: {p}")
        raise SystemExit("partition: RED — codewalk controls did not both fire; no zero in "
                         "this run is readable")

    cw_pred = {(r["deffile"], r["pi"]): r for r in cw_t["preds"]}
    cw_pred_f = {(r["deffile"], r["pi"]): r for r in cw_f["preds"]}
    cw_unres = {(r["deffile"], r["pi"]): r["reason"] for r in cw_t["unresolved"]}
    cw_sites: dict[tuple[str, str], list[dict]] = {}
    for s in cw_t["sites"]:
        cw_sites.setdefault((s["deffile"], s["pi"]), []).append(s)

    rows = []
    for key in latentb:
        deffile, pi = key
        rx = rx_counts.get(key)
        cw = cw_pred.get(key)
        cwf = cw_pred_f.get(key)
        unres = cw_unres.get(key)

        cw_bound = None if cw is None else cw["bound"]
        cw_sites_n = None if cw is None else cw["sites"]
        cwf_bound = None if cwf is None else cwf["bound"]
        # unification-bound stratum: sites this arm resolves ONLY because prolog_codewalk
        # executes `A=B` while walking.
        unif = None if (cw_bound is None or cwf_bound is None) else cw_bound - cwf_bound

        if unres is not None:
            disp = "regex-only"
            witness = ("NOT PRE-REGISTERED — see partition.md §Prereg gap. The codewalk arm "
                       "returned no verdict for this row, so `converts-clean` (which requires "
                       "zero under BOTH arms) cannot be assigned. Regex-arm evidence only.")
        elif cw_bound and cw_bound > 0:
            if rx == 0:
                disp = "not-latent"
                witness = ("adjudicate before converting — a bound caller the regex arm "
                           "missed; may not be class-B at all")
            else:
                disp = "not-latent"
                witness = ("adjudicate before converting — bound caller seen by BOTH arms; "
                           "the class label was already wrong when it was authored")
        elif (rx or 0) > 0:
            disp = "not-latent"
            witness = ("adjudicate before converting — regex arm reports bound caller(s) the "
                       "codewalk arm does not; check for embedded goal strings / unloaded "
                       "callers before treating either arm as wrong")
        else:
            disp = "converts-clean"
            witness = "template application; no six-leg run"

        machine_disp = disp
        adj = ADJUDICATED.get(key)
        if adj is not None:
            disp, witness = adj[0], adj[1]

        rows.append({
            "deffile": deffile, "pi": pi, "disposition": disp,
            "machine_disposition": machine_disp,
            "adjudicated": "yes" if adj is not None else "no",
            "regex_bound": rx if rx is not None else "no-census-row",
            "codewalk_sites": cw_sites_n if cw_sites_n is not None else "-",
            "codewalk_bound": cw_bound if cw_bound is not None else "UNRESOLVED",
            "unification_bound": unif if unif is not None else "-",
            "codewalk_unresolved_reason": unres or "",
            "witness_owed": witness,
            "bound_sites": "; ".join(
                f"{s['file']}:{s['line']} caller={s['caller']} atom={s['atom']}"
                for s in cw_sites.get(key, []) if s["kind"] == "bound"),
            "regex_sites": "; ".join(rx_sites.get(key, [])[:6]),
        })

    # The partition's own row count, asserted against the registry (never a hand-copied N).
    assert len(rows) == n_latentb, f"{len(rows)} rows != {n_latentb} latent-B registry entries"
    assert all(r["disposition"] for r in rows), "unclassified row"

    # Every row the machine and the adjudication disagree about must be in ADJUDICATED, and
    # every ADJUDICATED key must be a real latent-B row — a stale adjudication is a fork.
    stale = sorted(set(ADJUDICATED) - set(latentb))
    if stale:
        raise SystemExit(f"partition: RED — adjudication entries for non-latent-B rows: {stale}")
    unadj = sorted((r["deffile"], r["pi"]) for r in rows
                   if r["machine_disposition"] == "not-latent" and r["adjudicated"] == "no")
    if unadj:
        raise SystemExit(f"partition: RED — machine flagged not-latent, no adjudication: {unadj}")

    cols = ["deffile", "pi", "disposition", "machine_disposition", "adjudicated",
            "regex_bound", "codewalk_sites", "codewalk_bound",
            "unification_bound", "codewalk_unresolved_reason", "bound_sites", "regex_sites",
            "witness_owed"]
    (HERE / "partition.tsv").write_text(
        "\t".join(cols) + "\n" +
        "\n".join("\t".join(str(r[c]).replace("\t", " ") for c in cols) for r in rows) + "\n")
    (HERE / "regex_sweep_raw.txt").write_text(rx_raw)
    (HERE / "codewalk_evaluate_true.json").write_text(json.dumps(cw_t, indent=1))
    (HERE / "codewalk_evaluate_false.json").write_text(json.dumps(cw_f, indent=1))

    counts: dict[str, int] = {}
    for r in rows:
        counts[r["disposition"]] = counts.get(r["disposition"], 0) + 1
    total_unif = sum(r["unification_bound"] for r in rows
                     if isinstance(r["unification_bound"], int))

    print(f"PARTITION: {len(rows)} latent-B rows (registry N_latentB={n_latentb}), "
          f"all classified")
    for d in sorted(counts):
        print(f"  {d:16} {counts[d]}")
    print(f"  unification-bound sites (evaluate true minus false): {total_unif}")
    print(f"  codewalk unresolved: {sum(1 for r in rows if r['codewalk_bound']=='UNRESOLVED')}")
    # partition.md — the receiver's worklist, GENERATED from the same rows as the TSV.
    md = ["# partition — OQ-303(a) `latent-B` rows, both instrument columns",
          "",
          "**Generated** by `partition.py` (do not hand-edit — regenerate). Executed",
          "2026-08-18. Worklist size read from `python/dispatch_head_check.py` `DECLARED`:",
          f"**N_latentB = {n_latentb}**, and the row count below is asserted equal to it.",
          "",
          "Columns: `rx` = regex arm (`caller_sweep.py`) bound call sites; `cw` = codewalk arm",
          "(`codewalk_caller_check.py`) bound call sites, with total sites seen in parentheses;",
          "`uni` = sites the codewalk arm resolves ONLY because `prolog_codewalk` executes",
          "`A=B` while walking (evaluate true minus evaluate false).",
          "",
          "## Counts", ""]
    for d in sorted(counts):
        md.append(f"- `{d}` — {counts[d]}")
    md += ["", f"- unification-bound sites across all rows: **{total_unif}**",
           f"- rows the codewalk arm could not resolve: "
           f"**{sum(1 for r in rows if r['codewalk_bound']=='UNRESOLVED')}**",
           "", "## Rows", "",
           "| # | file | predicate | disposition | rx | cw | uni | witness a conversion owes |",
           "|---|---|---|---|---|---|---|---|"]
    for i, r in enumerate(sorted(rows, key=lambda r: (r["disposition"] != "converts-clean",
                                                      r["deffile"], r["pi"])), 1):
        cw = (f"{r['codewalk_bound']} ({r['codewalk_sites']})"
              if r["codewalk_bound"] != "UNRESOLVED" else "UNRESOLVED")
        star = " **[adjudicated]**" if r["adjudicated"] == "yes" else ""
        md.append(f"| {i} | `{r['deffile']}` | `{r['pi']}` | `{r['disposition']}`{star} | "
                  f"{r['regex_bound']} | {cw} | {r['unification_bound']} | "
                  f"{r['witness_owed'].replace('|', '/')} |")
    md += ["", "## Bound call sites found (codewalk arm)", ""]
    any_site = False
    for r in sorted(rows, key=lambda r: (r["deffile"], r["pi"])):
        if r["bound_sites"]:
            any_site = True
            md.append(f"- `{r['deffile']} {r['pi']}` — {r['bound_sites']}")
    if not any_site:
        md.append("- (none)")
    md += ["", "## Bound call sites found (regex arm)", ""]
    any_rx = False
    for r in sorted(rows, key=lambda r: (r["deffile"], r["pi"])):
        if r["regex_sites"]:
            any_rx = True
            md.append(f"- `{r['deffile']} {r['pi']}` — {r['regex_sites']}")
    if not any_rx:
        md.append("- (none)")
    (HERE / "partition.md").write_text("\n".join(md) + "\n")

    mdisp: dict[str, int] = {}
    for r in rows:
        mdisp[r["machine_disposition"]] = mdisp.get(r["machine_disposition"], 0) + 1
    print(f"  (machine, pre-adjudication: " +
          ", ".join(f"{k}={v}" for k, v in sorted(mdisp.items())) + ")")
    print(f"  wrote partition.tsv, partition.md, regex_sweep_raw.txt, "
          f"codewalk_evaluate_{{true,false}}.json")
    return 0


if __name__ == "__main__":
    sys.exit(main())
