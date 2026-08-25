#!/usr/bin/env python3
"""oq347_stratum_split.py — Phase-8 provenance-split robustness (plan 2026-08-25).

For every coherent-set leg carrying more than one story_provenance stratum (field 5 of the
multi-line story_provenance/8 term — parsed with PROV_RE, NEVER a line-oriented grep on a
leg-name pattern: provenance leg-names are DRIVER names, and haiku's June originals carry bare
'no_scope_rebuild' with no suffix), re-reads the situation-fixed-core result split
stratum-by-stratum into every stratum actually present on the leg — never a binary
original-vs-backfilled cut with the rescue folded into one side.

Per (leg, stratum): n ids in the all-pairs intersection, core members among them, disagree rate,
and per-pair agreement (pairs involving that leg) restricted to the stratum — beside the pooled
value, so a moved answer is visible. A stratum too small to carry its own row is reported with
its n, never dropped.

Usage: oq347_stratum_split.py --core-json outputs/coherent_<H>/situation_fixed_core.json
"""
from __future__ import annotations

import argparse
import collections
import json
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
if str(REPO / "python") not in sys.path:
    sys.path.insert(0, str(REPO / "python"))

from audits.leg_diagnostic_table import PROV_RE

FIELDS = {
    "h1_band": lambda r: r.get("h1_band"),
    "verdict": lambda r: (r.get("verdict_join") or {}).get("verdict"),
    "signature": lambda r: r.get("signature"),
}


def strata_of(leg: str) -> dict:
    """id -> provenance stratum (field 5), from the leg's own .pl files."""
    out = {}
    for f in (REPO / "prolog" / leg).glob("*.pl"):
        m = PROV_RE.search(f.read_text(encoding="utf-8", errors="replace"))
        out[f.stem] = m.group(5) if m else "PROVENANCE_ABSENT"
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--core-json", required=True)
    ap.add_argument("--dir", default=str(REPO / "outputs"))
    args = ap.parse_args()
    core_d = json.load(open(args.core_json))
    pairs = [tuple(p) for p in core_d["pairs"]]
    inter = sorted(set(core_d["core"]) | set(core_d["agreed_on_null"]) | set(core_d["disagree"]))
    core = set(core_d["core"])
    out_dir = Path(args.dir)

    data = {}
    for leg in sorted({l for p in pairs for l in p}):
        suffix = leg[len("testsets_"):]
        d = json.load(open(out_dir / f"pipeline_output.{suffix}.json"))
        data[leg] = {r["id"]: r for r in d["per_constraint"]}

    n_inter = len(inter)
    print(f"pooled: intersection n={n_inter}, core={len(core)} ({len(core)/n_inter:.1%})")
    for leg in sorted(data):
        smap = strata_of(leg)
        hist = collections.Counter(smap.values())
        if len(hist) < 2:
            continue
        print(f"\n{leg} — {len(hist)} strata: { {k: v for k, v in hist.most_common()} }")
        leg_pairs = [p for p in pairs if leg in p]
        for stratum, n_files in hist.most_common():
            ids = [i for i in inter if smap.get(i) == stratum]
            n = len(ids)
            c = sum(1 for i in ids if i in core)
            line = f"  {stratum:48} inter_n={n:4}  core={c}"
            if n:
                line += f" ({c/n:.1%})"
            print(line)
            if n < 30:
                print(f"  {'':48} (n<30 — reported with its n, no rate claims)")
                continue
            for a, b in leg_pairs:
                x, y = data[a], data[b]
                pids = [i for i in ids if i in x and i in y]
                if not pids:
                    continue
                agr = {k: sum(1 for i in pids if f(x[i]) == f(y[i])) / len(pids)
                       for k, f in FIELDS.items()}
                print(f"    {a} <-> {b}: n={len(pids):4} " +
                      "  ".join(f"{k}={v:.0%}" for k, v in agr.items()))


if __name__ == "__main__":
    main()
