#!/usr/bin/env python3
"""OQ-258 flash-leg wrapper: rebind run_no_scope_gemini's module constants to an
arm-scoped namespace, then run() unchanged (PROPOSAL.md, regime table row 2).
Usage: python3 wrapper_flash.py <arm_tag>   (from repo root; needs GEMINI_API_KEY)"""
import argparse
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO))
from agent import run_no_scope_gemini as leg  # noqa: E402

tag = sys.argv[1]
leg.FLASH_TESTSETS = leg.REPO_ROOT / "prolog" / f"testsets_flash_{tag}"
leg.FLASH_JSON = leg.REPO_ROOT / f"json_flash_{tag}"
leg.FLASH_LADDER = leg.REPO_ROOT / "prolog" / f"beta_processed_flash_{tag}.txt"
leg.OUT_DIR = leg.REPO_ROOT / "outputs" / f"no_scope_runs_flash_{tag}"

leg.run(argparse.Namespace(
    seeds=str(Path(__file__).parent / "seeds_18.json"), n=0,
    model=leg.DEFAULT_MODEL, poll_interval=leg.POLL_INTERVAL,
    no_cache=False, estimate=False))
