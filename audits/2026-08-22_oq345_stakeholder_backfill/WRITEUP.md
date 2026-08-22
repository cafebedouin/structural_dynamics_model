# OQ-345 — Stakeholder backfill of the June haiku/flash legs: the driver's "complete 210/210" was a ladder count over a pass that wrote 168

**Executed:** 2026-08-22
**OQ:** OQ-345
**Verdict:** The backfill mode works (haiku 455/458 written in place, flash 168 + 42 in a second pass) but its completion count was sourced from the LADDER, which already listed every backfill target from June — so the flash pass reported 210/210 having written 168 and never retried 42 failures; fixed to count from the written artifact's provenance tag (`c0e7c89f`). The before/after classification diff is pending the after-arm reclassifications.
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
