# OQ-259 item 2 (note a) — T Framework baseline does NOT reproduce under the pinned recipe; fresh baseline minted, superseded file retained

**Executed:** 2026-08-06
**OQ:** OQ-259 (item 2, note (a) — conversion provenance precondition for the graduation dry-run)
**Verdict:** The committed `T Framework - Michigan 2026 BCFP.md` (597,374 B, md5 `51caeb369d147849d07b45f1ba0926b6`) is not reproduced by the pinned recipe `pandoc -f docx -t gfm --wrap=none` (pandoc 2.9.2.1) on the committed docx; a fresh baseline (672,832 B, md5 `a365da8aa11e5039807275bcc662f956`) was minted from the pinned recipe and is Part C's pinned input — the diff shape is described below, but NO cause classification is claimed.
**Substrate:** no pipeline run (file-level provenance check only; corpus untouched)
**Evidence map:**
- `tframework_repro_pandoc2921.md` — pinned-recipe output from the committed docx (672,832 B, md5 `a365da8a…`); byte-identical to the newly minted baseline at `agent/analysis/originals/k_files/T Framework - Michigan 2026 BCFP.md`
- `superseded_baseline_51caeb36.md` — the retired committed baseline, retained verbatim (597,374 B, md5 `51caeb36…`)
- `pandoc_stderr.log` — empty; conversion exited 0
- `RECON.md` — provenance narrowing (sole commit, clean worktree) with commands

## What ran

1. Provenance narrowing (read-only): both `.docx` and `.md` frozen since sole commit
   `1bd57a84`; worktree clean vs HEAD for both files. Non-reproduction is therefore
   conversion-side relative to the committed pair, not a post-commit edit.
2. Pinned recipe run: `pandoc -f docx -t gfm --wrap=none` (pandoc 2.9.2.1) on the
   committed docx → `tframework_repro_pandoc2921.md`, exit 0, stderr empty.
3. Comparison (witnessed in-session):
   - repro: 672,832 B, md5 `a365da8aa11e5039807275bcc662f956`, 1,351 lines
   - committed baseline: 597,374 B, md5 `51caeb369d147849d07b45f1ba0926b6`, 1,351 lines
   - `diff` = 1,424 lines across the pair.

## Diff shape (descriptive only — cause claim intentionally withheld)

- Line counts identical (1,351 vs 1,351); the difference is intra-line markup
  representation, not content addition/removal.
- Dominant component: the repro carries `<span class="underline">…</span>` runs that the
  committed baseline lacks. Stripping exactly those span tags from the repro reduces the
  diff from 1,424 lines to 8.
- Residual (2 changed lines): superscript representation — repro emits `<sup>NN</sup>`;
  baseline carries Unicode superscript characters (e.g. `²⁰`, `¹³`, `²¹`, `³¹`).
- Both components are markup-encoding differences over the same text. What produced the
  committed baseline (different pandoc version, post-processing, or another converter) is
  NOT determined here and is not claimed.

## Disposition

- Fresh baseline minted from the pinned recipe: `agent/analysis/originals/k_files/T
  Framework - Michigan 2026 BCFP.md` is now md5 `a365da8a…` (672,832 B). This md5 is the
  pinned input for OQ-259 item-2 Part C (graduation dry-run prereg).
- Superseded baseline retained in this directory (`superseded_baseline_51caeb36.md`).
- Both md5s recorded in KNOWN_STATE (2026-08-06 entry).
- OQ-259 item-2 note (a) is RESOLVED as "confirmed non-reproduction, provenance
  re-anchored to the pinned recipe" — the note's precondition (resolve conversion
  provenance before any emphasis-aware variant) is discharged by the re-mint.
