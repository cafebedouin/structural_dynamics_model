# OQ-345 — Stakeholder backfill of the June haiku/flash legs: the driver's "complete 210/210" was a ladder count over a pass that wrote 168

**Executed:** 2026-08-22
**OQ:** OQ-345
**Verdict:** The backfill mode works (haiku 455/458, flash 206/210 over three passes) and the diff landed: `h1_stakeholder` null 100% → 4% (haiku) / 3% (flash) on the regenerated strata; against their June originals h1 51% / 67%, verdict 71% / 71%, ε exact 46% / 60%, purity band 2% / 4%; the untouched control reads 100% on every field except verdict 97% / 98% (corpus-relative components). The driver's completion count was ladder-sourced (flash reported 210/210 having written 168) but its completion count was sourced from the LADDER, which already listed every backfill target from June — so the flash pass reported 210/210 having written 168 and never retried 42 failures; fixed to count from the written artifact's provenance tag (`c0e7c89f`). The before/after classification diff is pending the after-arm reclassifications.
**Substrate:** `testsets_haiku` (960; 455 stories now `no_scope_rebuild_haiku+stakeholder_backfill`), `testsets_flash` (960; 168 tagged at writing, 42 in flight) — before-arm classifications preserved as `outputs/pipeline_output.{haiku,flash}.prebackfill.json` (HEAD 7597aa7 / f0ef08a, engine-coherent); after-arm = reclassification at HEAD once each backfill lands; `code_dirty` True throughout (legs being written).
**Fired:** live — the driver's success line ("Sonnet/Gemini no-scope run complete: 210/210") was contradicted by the artifact count (168 `.pl`/`.json` carrying the tag, 44 json still lacking `stakeholders`); the *count from the artifact, never the loop* rule caught it, and arm G of `module_boundary_check` independently fired the moment the backfill wrote `epsilon_provenance/5` into legs pinned `empty`.
**Evidence map:**
- `backfill_diff.py` — before/after same-seed diff stratified by the `+stakeholder_backfill` provenance tag; the untouched stratum is the control (must read ~100%). Run after the after-arm reclassification lands; its output will be added here as `backfill_diff_<leg>_<date>.txt`.
- id lists: `prolog/kernels/rebuild_2026-06-13/{haiku,flash}_stakeholder_backfill_ids.json` (458 / 210 = stories with parties and no stakeholder surface).
- run logs (gitignored): `outputs/no_scope_runs_haiku/backfill.log`, `outputs/no_scope_runs_flash/backfill.log`, `backfill2.log`.

## What the pass changed and did not
- Regenerated in place under prompt `e03e2210` / schema `685ed7cf`, thinking off (haiku via the Anthropic batch driver; flash via Gemini batch, `thinking_budget=0`). The originals are the pre-gate stratum and remain in git history (`522def40^` for flash, `c3496fcf^` for haiku). Cost ≈ $7 (haiku) + ≈ $2.5 (flash).
- The two legs are now mixed-stratum BY DESIGN; every read must stratify by `story_provenance` source. The schema-shape pin for `epsilon_provenance/5` moved from `testsets_haiku:empty,testsets_flash:empty` to `any` with the reason recorded (`prolog/schema_shape.txt:65`).
- Party-less stories (7 haiku, 2 flash) were deliberately not touched; 3 haiku + (pending) flash seeds are seed-persistent failures.

## Diff results (2026-08-22, after-arm = reclassification at HEAD)
- `backfill_diff_haiku_2026-08-22.txt`: backfilled n=455 — h1 51%, verdict 71%, signature 87%, purity_band 2%, claimed_type 85%, ε 46% (12% ≥0.10); `h1_stakeholder` null 100% → 4%. Control n=505: all 100% except verdict 97%.
- `backfill_diff_flash_2026-08-22.txt`: backfilled n=206 — h1 67%, verdict 71%, signature 84%, purity_band 4%, claimed_type 82%, ε 60% (13% ≥0.10); `h1_stakeholder` null 100% → 3%. Control n=754: all 100% except verdict 98%.
- Reading: the backfilled strata disagree with their originals at roughly the within-model floor for each model PLUS the June→August regime change (haiku floor h1 53%, flash 85%); purity band collapses because purity reads the stakeholder/coordination surface the originals lacked. The 2–3% of untouched verdicts that moved name a corpus-relative channel in `verdict_join`; the control is ≈100%, not 100%, and that residue is a finding, not noise.
