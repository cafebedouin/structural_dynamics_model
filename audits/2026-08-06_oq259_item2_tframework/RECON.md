# RECON — OQ-259 item 2 (T Framework conversion provenance)

Read-only pass, 2026-08-06, before the pinned-recipe run.

## Commands and witnessed outputs

1. Toolchain pin:
   ```
   $ pandoc --version | head -1
   pandoc 2.9.2.1
   ```
2. Commit history (both files):
   ```
   $ git log --oneline -- "agent/analysis/originals/k_files/T Framework - Michigan 2026 BCFP.md"
   1bd57a84 kritik ingest: commit k_file sources (15+CapKNW) + pre-registered PROPOSAL (Phase 0)
   $ git log --oneline -- "agent/analysis/originals/k_files/T Framework - Michigan 2026 BCFP.docx"
   1bd57a84 kritik ingest: commit k_file sources (15+CapKNW) + pre-registered PROPOSAL (Phase 0)
   ```
   Sole commit for both; no later edits in history.
3. Worktree cleanliness:
   ```
   $ git diff --stat HEAD -- <both files>
   (empty — CLEAN)
   ```

## What this narrows

The prototype-session observation (OQ-259 item 2 note (a): committed `.md` does not
reproduce under the pinned recipe) cannot be a post-commit edit — both files are frozen
at `1bd57a84` and the worktree is clean. The non-reproduction, if confirmed, is
conversion-side (whatever produced the committed `.md` differs from the pinned recipe).
Plan-file recon note: docx zip mtimes zeroed (non-decisive) — carried from the planning
session, not re-witnessed here.

## Answerable

Whether the pinned recipe reproduces the committed baseline byte-identically: YES —
one conversion + one diff. Cause classification of any difference: NOT answerable from
this data (no record of the original conversion command); the plan pins that no cause
verdict is issued.
