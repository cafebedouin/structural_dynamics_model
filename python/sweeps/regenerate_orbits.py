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
"""

import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
ORBITS_PATH = ROOT / "outputs" / "product_site_orbits.json"

sys.path.insert(0, str(ROOT / "python"))
from sweeps.perturb import _compute_corpus_hash


def main() -> None:
    print("[regenerate_orbits] Running swipl product_site_export ...", flush=True)
    result = subprocess.run(
        ["swipl", "-g", "[stack],[product_site_export],run_product_export,halt", "-t", "halt(1)"],
        cwd=str(PROLOG_DIR),
        capture_output=False,
    )
    if result.returncode != 0:
        print(f"[regenerate_orbits] swipl exited with code {result.returncode} — aborting.",
              file=sys.stderr)
        sys.exit(result.returncode)

    if not ORBITS_PATH.exists():
        print(f"[regenerate_orbits] ERROR: {ORBITS_PATH} not found after swipl run.",
              file=sys.stderr)
        sys.exit(1)

    corpus_hash = _compute_corpus_hash(PROLOG_DIR / "testsets")
    data = json.loads(ORBITS_PATH.read_text(encoding="utf-8"))
    data["corpus_hash"] = corpus_hash
    ORBITS_PATH.write_text(json.dumps(data, indent=2), encoding="utf-8")

    print(f"[regenerate_orbits] corpus_hash={corpus_hash} stamped into {ORBITS_PATH.name}")


if __name__ == "__main__":
    main()
