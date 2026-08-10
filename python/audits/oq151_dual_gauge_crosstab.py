#!/usr/bin/env python3
"""OQ-151 dual-gauge crosstab: h1_band (power gauge) x h1_stakeholder (seat frame).

Per leg, a 3x3 crosstab over the strata {null, 0, >0} of the two serialized
obstruction observables:
  - h1_band        : the power-gauge/observer-orbit obstruction band
                     (signature-resolved orbit; null = <2 real seats, OQ-51).
  - h1_stakeholder : the stakeholder-frame obstruction over the named
                     non-excluded agent seats (null = <2 real-typed seats,
                     OQ-207/OQ-217).

PRE-REGISTERED cell semantics (PREREGISTRATION.md in the audit dir, md5-logged
before this script runs):
  (0, >0)  : the observer orbit GLUES while the authored parties fracture —
             the realizable form of OQ-151's "role-H1>0 while power-H1=0".
  (>0, 0)  : observer fracture over seated consensus — intersected with
             empty_chair_state in the per-item verification step (swipl).
  null strata are reported WITH REASONS, never coerced: a missing key RAISES
  (no .get(..., 0) — the OQ-51/OQ-207 silent-zero trap); a null value is a
  stratum of its own.

Five independent tables — one per leg, NEVER merged (GAP-31: seats are
story-local; pooling across legs would measure the pooling convention).

Outputs (into the audit dir): crosstab_<leg>.json (cells carry ids) +
crosstab_joined.md (human table, one section per leg, manifests cited).

Usage: python3 python/audits/oq151_dual_gauge_crosstab.py <audit_dir>
"""

import json
import sys
from collections import Counter
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
OUTPUTS = REPO / "outputs"

# leg name -> manifest-bearing output produced this session (classify_corpus
# for the non-default legs; canonical run_pipeline for testsets/).
LEGS = {
    "testsets":        "pipeline_output.json",
    "testsets_haiku":  "pipeline_output.haiku.json",
    "testsets_flash":  "pipeline_output.flash.json",
    "testsets_kimi":   "pipeline_output.kimi.json",
    "testsets_sonnet": "pipeline_output.sonnet.json",
}

STRATA = ("null", "zero", "pos")


def stratum(value):
    """Stratum of an h1 observable. None is a stratum, never coerced."""
    if value is None:
        return "null"
    if not isinstance(value, (int, float)):
        raise TypeError(f"non-numeric non-null h1 value: {value!r}")
    return "zero" if value == 0 else "pos"


def crosstab_for_leg(leg, fname):
    data = json.load(open(OUTPUTS / fname, encoding="utf-8"))
    manifest = data["manifest"]
    cells = {(a, b): [] for a in STRATA for b in STRATA}
    null_reasons_band = Counter()
    null_reasons_stake = Counter()
    for rec in data["per_constraint"]:
        # KeyError on absence is the DESIRED failure mode (no .get defaults).
        cid = rec["id"]
        band = rec["h1_band"]
        stake = rec["h1_stakeholder"]
        sb, ss = stratum(band), stratum(stake)
        cells[(sb, ss)].append(cid)
        if sb == "null":
            null_reasons_band[
                f"{rec['sheaf_status']}/{rec['sheaf_undetermined_reason']}"] += 1
        if ss == "null":
            null_reasons_stake[f"n_real={rec['h1_stakeholder_n_real']}"] += 1
    n = len(data["per_constraint"])
    total = sum(len(v) for v in cells.values())
    assert total == n, f"{leg}: crosstab cells sum {total} != n {n}"
    return {
        "leg": leg,
        "manifest": {k: manifest.get(k) for k in
                     ("pipeline_run_at", "n_constraints", "code_commit",
                      "code_commit_short", "code_dirty", "corpus_path")},
        "n": n,
        "cells": {f"{a},{b}": ids for (a, b), ids in cells.items()},
        "cell_counts": {f"{a},{b}": len(ids) for (a, b), ids in cells.items()},
        "null_reasons_h1_band": dict(null_reasons_band),
        "null_reasons_h1_stakeholder": dict(null_reasons_stake),
    }


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: oq151_dual_gauge_crosstab.py <audit_dir>")
    audit_dir = Path(sys.argv[1])
    audit_dir.mkdir(parents=True, exist_ok=True)

    results = []
    for leg, fname in LEGS.items():
        r = crosstab_for_leg(leg, fname)
        out = audit_dir / f"crosstab_{leg}.json"
        out.write_text(json.dumps(r, indent=1), encoding="utf-8")
        results.append(r)
        print(f"[crosstab] {leg}: n={r['n']} code_commit={r['manifest']['code_commit_short']}"
              f" run_at={r['manifest']['pipeline_run_at']}")

    # shared-commit freshness assertion (revised Step-0 criterion)
    commits = {r["manifest"]["code_commit"] for r in results}
    if len(commits) != 1:
        raise RuntimeError(f"legs span multiple code_commits: {commits}")

    md = ["# OQ-151 dual-gauge crosstab (h1_band x h1_stakeholder)", "",
          "Five independent per-leg tables (GAP-31: never merged). "
          "Rows = h1_band stratum; columns = h1_stakeholder stratum.", ""]
    for r in results:
        m = r["manifest"]
        md.append(f"## {r['leg']}  (n={r['n']}, run_at={m['pipeline_run_at']}, "
                  f"commit={m['code_commit_short']}, dirty={m['code_dirty']})")
        md.append("")
        md.append("| h1_band \\ h1_stakeholder | null | zero | pos |")
        md.append("|---|---|---|---|")
        for a in STRATA:
            row = [str(r["cell_counts"][f"{a},{b}"]) for b in STRATA]
            md.append(f"| {a} | " + " | ".join(row) + " |")
        md.append("")
        md.append(f"- h1_band null reasons: {r['null_reasons_h1_band']}")
        md.append(f"- h1_stakeholder null reasons: {r['null_reasons_h1_stakeholder']}")
        md.append(f"- (zero,pos) ids [observer glues, parties fracture]: "
                  f"{r['cells']['zero,pos']}")
        md.append(f"- (pos,zero) ids [observer fracture over seated consensus]: "
                  f"{r['cells']['pos,zero']}")
        md.append("")
    (audit_dir / "crosstab_joined.md").write_text("\n".join(md), encoding="utf-8")
    print(f"[crosstab] wrote crosstab_joined.md; shared code_commit: {commits.pop()}")


if __name__ == "__main__":
    main()
