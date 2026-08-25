#!/usr/bin/env python3
"""oq347_coherent_reclassify.py — classify every roster leg at ONE clean HEAD (OQ-342 step 3 /
OQ-347 steps 2–3; plan 2026-08-25).

The 19 existing leg outputs were classified at 15 different engine commits, 16/19 with
code_dirty=True — no commit reconstructs them (KNOWN_STATE 2026-08-23 TRIPWIRE 5). This driver
re-runs `run_pipeline.classify_corpus` per leg, SERIAL (they share outputs/pipeline_output.raw.json),
into a NEW directory outputs/coherent_<HEAD7>/ — the existing artifacts stay untouched until the
Phase-5 diff is read (prove before you replace).

Roster: leg_diagnostic_table.declared_roster() — LIVE_LEGS ∪ {testsets} with the glob cross-check,
never a re-glob. Output names: leg_diagnostic_table.output_name_for() (the live leg maps to
pipeline_output.json). `testsets` is classified LAST with its count stamped at the moment of its
own call (it moves mid-session) and is NOT a member of the coherent comparison set (18 legs).

expected_model is DERIVED per leg (OQ-78): the distinct story_provenance field-7 values off the
leg's .pl files — exactly one ⇒ pass it; more than one ⇒ None (mixed-model, fingerprint skipped
by design). Never a hand-written leg→model table.

Preconditions asserted before any call: clean tree at code paths (run_pipeline._is_code_path —
the same predicate that stamps code_dirty), and no second classify running. HARD invariant: no
repo write between launch and completion (code_dirty is stamped per run).

Usage: oq347_coherent_reclassify.py [--only leg ...] [--verify-only]
"""
from __future__ import annotations

import argparse
import collections
import json
import subprocess
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
if str(REPO / "python") not in sys.path:
    sys.path.insert(0, str(REPO / "python"))

import run_pipeline
from audits.leg_diagnostic_table import PROV_RE, declared_roster, output_name_for

OUT = REPO / "outputs"


def derived_model(leg: str) -> tuple:
    """(expected_model_or_None, counter) from the leg's own story_provenance field 7."""
    models = collections.Counter()
    for f in (REPO / "prolog" / leg).glob("*.pl"):
        m = PROV_RE.search(f.read_text(encoding="utf-8", errors="replace"))
        if m:
            models[m.group(7)] += 1
    real = sorted(models)
    return (real[0] if len(real) == 1 else None), models


def head7() -> str:
    return subprocess.run(["git", "rev-parse", "--short=7", "HEAD"], cwd=REPO,
                          capture_output=True, text=True).stdout.strip()


def tree_dirty_at_code_paths() -> list:
    out = subprocess.run(["git", "status", "--porcelain"], cwd=REPO,
                         capture_output=True, text=True).stdout
    dirty = []
    for line in out.splitlines():
        if len(line) < 4:
            continue
        path = line[3:].strip().strip('"')
        if " -> " in path:
            path = path.split(" -> ", 1)[1]
        if run_pipeline._is_code_path(path):
            dirty.append(path)
    return dirty


def verify(dest: Path, frozen: str, roster_legs: list) -> int:
    """Coherence assertions over the written outputs. Returns #hard failures (18 legs only;
    a `testsets` count mismatch is a RECORDED NOTE — the live leg moves)."""
    hard = 0
    print(f"\n{'leg':26} {'commit':8} {'dirty':5} {'n_man':>6} {'n_disk':>6}  status")
    commits = set()
    for leg in roster_legs:
        p = dest / output_name_for(leg)
        if not p.exists():
            print(f"{leg:26} MISSING OUTPUT {p}")
            hard += 1
            continue
        m = json.load(open(p))["manifest"]
        n_disk = len(list((REPO / "prolog" / leg).glob("*.pl")))
        ok_commit = m["code_commit_short"] == frozen
        ok_dirty = m["code_dirty"] is False
        ok_n = m["n_constraints"] == n_disk
        commits.add(m["code_commit_short"])
        status = []
        if not ok_commit:
            status.append(f"COMMIT!={frozen}")
        if not ok_dirty:
            status.append("DIRTY")
        if not ok_n:
            status.append("COUNT-NOTE (live leg moved)" if leg == "testsets" else "COUNT MISMATCH")
        if leg != "testsets" and not (ok_commit and ok_dirty and ok_n):
            hard += 1
        if leg == "testsets" and not (ok_commit and ok_dirty):
            hard += 1
        print(f"{leg:26} {m['code_commit_short']:8} {str(m['code_dirty']):5} "
              f"{m['n_constraints']:>6} {n_disk:>6}  {' '.join(status) or 'ok'}")
    print(f"distinct commits: {sorted(commits)}  hard failures: {hard}")
    return hard


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--only", nargs="*", default=None,
                    help="subset of legs (default: full roster, testsets last)")
    ap.add_argument("--verify-only", action="store_true",
                    help="run only the coherence assertions over an existing coherent dir")
    args = ap.parse_args()

    frozen = head7()
    dest = OUT / f"coherent_{frozen}"
    roster, cross = declared_roster()
    if cross["only_in_glob"] or cross["only_in_declaration"]:
        raise SystemExit(f"roster/glob divergence: {cross} — resolve before classifying")
    legs = [r["leg"] for r in roster if r["leg"] != "testsets"] + ["testsets"]  # live leg LAST
    if args.only:
        legs = [l for l in legs if l in set(args.only)]

    if args.verify_only:
        sys.exit(1 if verify(dest, frozen, legs) else 0)

    dirty = tree_dirty_at_code_paths()
    if dirty:
        raise SystemExit(f"tree dirty at code paths {dirty} — the sweep would stamp "
                         f"code_dirty=True on every leg. Commit first (Phase 3 freeze).")
    dest.mkdir(parents=True, exist_ok=True)
    print(f"frozen HEAD: {frozen}  dest: {dest}  legs: {len(legs)} (testsets last)")

    for leg in legs:
        exp, models = derived_model(leg)
        n_disk = len(list((REPO / "prolog" / leg).glob("*.pl")))
        print(f"\n=== {leg}  n_disk={n_disk} (stamped at the moment of this call)  "
              f"expected_model={exp or 'None (models: %s)' % dict(models)}", flush=True)
        t0 = time.time()
        manifest = run_pipeline.classify_corpus(
            leg, f"coherent_{frozen}/{output_name_for(leg)}", exp)
        print(f"    done in {time.time()-t0:.0f}s  commit={manifest['code_commit_short']} "
              f"dirty={manifest['code_dirty']} n={manifest['n_constraints']}", flush=True)

    hard = verify(dest, frozen, legs)
    sys.exit(1 if hard else 0)


if __name__ == "__main__":
    main()
