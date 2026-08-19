#!/usr/bin/env python3
"""reachability.py — which latent-B predicates does a classification run actually EXERCISE?

WHY (operator ruling, 2026-08-19). The per-file bisect cleared 27 of 29 converted files at
`changed=0`. That is real evidence with a demonstrated firing control — the same corpus caught
both bad rows. But both were HOT: they moved five or six legs. **A COLD predicate clears
`changed=0` on absence of exercise, not on correctness.** Corpus clearance is therefore strong
exactly where the legs reach and empty where they do not, and which rows are cold is checkable.

METHOD: SWI's profiler over the same `run_json_report` goal classify_corpus runs, reading
per-predicate call counts from prolog_profile:profile_data/1. Zero calls = never entered.

CONTROL, asserted before any zero is reported: predicates on the hot path by construction must
show nonzero calls in the same run. If they do not, the profiler is not observing the run and
every zero is a didn't-look.

Usage:  reachability.py [<leg-relative-path> ...]     (default: testsets)
"""
from __future__ import annotations

import json
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
PROLOG = REPO / "prolog"
sys.path.insert(0, str(REPO / "python"))
from dispatch_head_check import DECLARED  # noqa: E402

HOT_CONTROLS = ["dr_type/3", "classify_from_metrics/6", "constraint_signature/2"]
CALL_RE = re.compile(r"^RCH (\S+):(\S+)/(\d+) (\d+)$")


def run_leg(corpus_rel: str) -> dict[str, int]:
    goal = (
        "use_module(library(prolog_profile)), "
        "retractall(config:param(corpus_path,_)), "
        f"assertz(config:param(corpus_path,'{corpus_rel}')), "
        "profile(run_json_report, [time(cpu)]), "
        "prolog_profile:profile_data(D), get_dict(nodes, D, Ns), "
        "forall(member(N, Ns), "
        "  ( get_dict(predicate, N, P), get_dict(call, N, C), "
        "    ( P = M:Nm/Ar -> true ; M = '?', P = Nm/Ar ), "
        "    format('RCH ~w:~w/~w ~w~n', [M, Nm, Ar, C]) ))")
    proc = subprocess.run(
        ["swipl", "-q", "-l", "stack.pl", "-l", "covering_analysis.pl",
         "-l", "maxent_classifier.pl", "-l", "dirac_classification.pl",
         "-l", "diagnostic_summary.pl", "-l", "post_synthesis.pl", "-l", "json_report.pl",
         "-g", f"{goal}, halt", "-t", "halt(1)"],
        cwd=PROLOG, capture_output=True, text=True, timeout=5400)
    calls: dict[str, int] = {}
    for ln in proc.stdout.splitlines():
        if (m := CALL_RE.match(ln)):
            k = f"{m.group(2)}/{m.group(3)}"
            calls[k] = calls.get(k, 0) + int(m.group(4))
    if not calls:
        raise SystemExit(f"reachability: RED — profiler returned no nodes for {corpus_rel} "
                         f"(rc={proc.returncode}). stderr: {proc.stderr[-400:]}")
    return calls


def main(argv: list[str]) -> int:
    legs = argv or ["testsets"]
    latentb = sorted(k for k, v in DECLARED.items() if v == "latent-B")
    per_leg = {}
    for leg in legs:
        calls = run_leg(leg)
        missing = [c for c in HOT_CONTROLS if calls.get(c, 0) == 0]
        if missing:
            print(f"  CONTROL {leg}: hot-path predicate(s) {missing} show 0 calls — the "
                  f"profiler is not observing this run; no zero from it is readable")
            print("reachability: RED")
            return 1
        per_leg[leg] = calls
        print(f"  {leg}: {len(calls)} predicates observed; controls "
              f"{ {c: calls[c] for c in HOT_CONTROLS} }", flush=True)

    rows = [{"file": f, "pi": pi,
             "calls": {leg: per_leg[leg].get(pi, 0) for leg in legs}} for f, pi in latentb]
    for r in rows:
        r["cold_everywhere"] = all(v == 0 for v in r["calls"].values())
    cold = [r for r in rows if r["cold_everywhere"]]
    (HERE / "reachability.json").write_text(json.dumps(
        {"legs": legs,
         "hot_controls": {c: {l: per_leg[l][c] for l in legs} for c in HOT_CONTROLS},
         "rows": rows}, indent=1))
    print(f"\n{len(cold)} of {len(rows)} latent-B rows are COLD on {', '.join(legs)} — a "
          f"`changed=0` covering these is absence of exercise, not correctness:")
    for r in sorted(cold, key=lambda r: (r["file"], r["pi"])):
        print(f"    {r['file']:32} {r['pi']}")
    print(f"\n{len(rows) - len(cold)} row(s) exercised; corpus clearance is real for those.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
