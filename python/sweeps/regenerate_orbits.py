#!/usr/bin/env python3
"""
Atomic orbit regeneration: swipl export + corpus_hash stamp in one invocation.

Replaces the two-command path:
    cd prolog && swipl -g '[stack],[product_site_export],run_product_export,halt'
    python3 python/run_pipeline.py  # (was needed to stamp corpus_hash)

The hash is computed immediately after swipl completes — same invocation, same
testset state — so the stamp is guaranteed to reflect the corpus that was exported.

Usage:
    python3 python/sweeps/regenerate_orbits.py
    python3 python/sweeps/regenerate_orbits.py --corpus-path testsets_sonnet2 \
        --out outputs/legs/testsets_sonnet2/product_site_orbits.json

OQ-352: --corpus-path / --out exist so report_corpus can THREAD an overlay leg
through this script rather than COPY it (a second orbit-export implementation
would be Build Discipline Pattern 2, the silent fork). BOTH DEFAULTS ARE INERT:
with neither flag the argv, the goal string and the output path are exactly what
they were before, and the corpus_hash is still computed over prolog/testsets.

Two things a caller must know. The overlay uses retractall-then-assertz, ONE
deterministic clause — a bare assertz appends AFTER config.pl:489's default and
is SILENTLY IGNORED (witnessed 2026-06-13: a twin overlay loaded 44 of 960 with
no error). And the corpus_hash stamped is always the hash of the corpus actually
exported, so an overlay artifact can never be read as the default corpus.
"""

import argparse
import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
ORBITS_PATH = ROOT / "outputs" / "product_site_orbits.json"

sys.path.insert(0, str(ROOT / "python"))
from corpus_hash import compute_corpus_hash as _compute_corpus_hash


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--corpus-path", default=None,
                    help="corpus dir relative to prolog/ (default: the default corpus, "
                         "i.e. no overlay and byte-identical argv to the pre-OQ-352 script)")
    ap.add_argument("--out", default=None,
                    help="output JSON path, repo-relative or absolute "
                         f"(default: {ORBITS_PATH.relative_to(ROOT)})")
    args = ap.parse_args()

    out_path = ORBITS_PATH if args.out is None else Path(args.out)
    if not out_path.is_absolute():
        out_path = ROOT / out_path
    corpus_dir = PROLOG_DIR / (args.corpus_path or "testsets")

    if args.corpus_path is None and args.out is None:
        # INERT PATH: byte-identical to the pre-OQ-352 argv and goal.
        goal = "[stack],[product_site_export],run_product_export,halt"
    else:
        if not corpus_dir.is_dir() or not any(corpus_dir.glob("*.pl")):
            print(f"[regenerate_orbits] ERROR: {corpus_dir} is missing or holds no .pl "
                  "files — refusing (a zero-glob overlay would silently export nothing).",
                  file=sys.stderr)
            sys.exit(2)
        # Prolog writes cwd-relative from prolog/, so hand it a path relative to there.
        try:
            prolog_rel = out_path.relative_to(PROLOG_DIR)
        except ValueError:
            prolog_rel = Path("..") / out_path.relative_to(ROOT)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        overlay = ("retractall(config:param(corpus_path,_)),"
                   f"assertz(config:param(corpus_path,'{args.corpus_path or 'testsets'}')),")
        goal = (f"[stack],[product_site_export],{overlay}"
                f"run_product_export_to('{prolog_rel.as_posix()}'),halt")

    print(f"[regenerate_orbits] Running swipl product_site_export ...", flush=True)
    result = subprocess.run(
        ["swipl", "-g", goal, "-t", "halt(1)"],
        cwd=str(PROLOG_DIR),
        capture_output=False,
    )
    if result.returncode != 0:
        print(f"[regenerate_orbits] swipl exited with code {result.returncode} — aborting.",
              file=sys.stderr)
        sys.exit(result.returncode)

    if not out_path.exists():
        print(f"[regenerate_orbits] ERROR: {out_path} not found after swipl run.",
              file=sys.stderr)
        sys.exit(1)

    # The hash is of the corpus ACTUALLY exported, so an overlay artifact can
    # never be read as the default corpus (OQ-29).
    corpus_hash = _compute_corpus_hash(corpus_dir)
    data = json.loads(out_path.read_text(encoding="utf-8"))
    data["corpus_hash"] = corpus_hash
    out_path.write_text(json.dumps(data, indent=2), encoding="utf-8")

    print(f"[regenerate_orbits] corpus_hash={corpus_hash} stamped into {out_path.name}")


if __name__ == "__main__":
    main()
