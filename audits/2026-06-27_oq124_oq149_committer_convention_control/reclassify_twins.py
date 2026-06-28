#!/usr/bin/env python3
"""Phase 0 step 1 — re-classify both twins at ONE commit (current HEAD).

The two on-disk twin outputs were classified at DIFFERENT commits (haiku 20fab78
Jun 24, flash 8126231 Jun 13), and the OQ-138 commits (02b880cb, 9bce5afc)
converted false_ci_rope + constructed_high_extraction (Field A's two signatures)
from RECLASSIFY->ROUTE between them — so the existing outputs are NOT comparable
for Field A. Re-run both via classify_corpus (deterministic overlay + provenance
fingerprint gate) at current HEAD, serially (one pipeline at a time).

Writes outputs/pipeline_output.haiku.json and .flash.json. Run from repo root.
"""
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python"))

from run_pipeline import classify_corpus  # noqa: E402

# Pin run_at to the execution date so the manifest is stamped deterministically
# for this audit (generation is the determinism frontier; classification is
# deterministic from the committed .pl onward).
RUN_AT = "2026-06-27T00:00:00Z"

if __name__ == "__main__":
    for corpus, out, model in [
        ("testsets_haiku", "pipeline_output.haiku.json", "claude-haiku-4-5"),
        ("testsets_flash", "pipeline_output.flash.json", "gemini-2.5-flash"),
    ]:
        print(f"\n=== classify {corpus} -> {out} (expect model {model}) ===",
              flush=True)
        m = classify_corpus(corpus, out, expected_model=model, run_at=RUN_AT)
        print(f"  manifest: n={m.get('n_constraints')} "
              f"commit={m.get('code_commit_short')} "
              f"dirty={m.get('code_dirty')} run_at={m.get('pipeline_run_at')}",
              flush=True)
    print("\nDONE — both twins re-classified at one commit.")
