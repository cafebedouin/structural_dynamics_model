#!/usr/bin/env python3
"""OQ-258 kimi-leg wrapper: rebind run_no_scope_kimi's module constants to an
arm-scoped namespace, then run() unchanged (PROPOSAL.md, regime table row 3:
kimi-k2.6, batch transport, model-default temp/reasoning).
Usage: python3 wrapper_kimi.py <arm_tag> [sync]   (from repo root; needs KIMI_API_KEY)
The optional 'sync' arg is the PROPOSAL's declared transport fallback (sampling params
identical either way) — used 2026-08-04 after the Arm B batch stalled at 0/18 for ~8h."""
import argparse
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO))
from agent import run_no_scope_kimi as leg  # noqa: E402

tag = sys.argv[1]
leg.KIMI_TESTSETS = leg.REPO_ROOT / "prolog" / f"testsets_kimi_{tag}"
leg.KIMI_JSON = leg.REPO_ROOT / f"json_kimi_{tag}"
leg.KIMI_LADDER = leg.REPO_ROOT / "prolog" / f"beta_processed_kimi_{tag}.txt"
leg.OUT_DIR = leg.REPO_ROOT / "outputs" / f"no_scope_runs_kimi_{tag}"

use_sync = len(sys.argv) > 2 and sys.argv[2] == "sync"
leg.run(argparse.Namespace(
    seeds=str(Path(__file__).parent / "seeds_18.json"), n=0,
    model=leg.DEFAULT_MODEL, sync=use_sync, batch=not use_sync,
    poll_interval=leg.POLL_INTERVAL, estimate=False, resume_batch=None))
