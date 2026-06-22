# Report regeneration — the 12 routed seats (rendered surface of OQ-173)

`python3 python/enhanced_report.py <12 routed seat ids>` re-run against the post-fix
`enriched_pipeline.json` (manifest `2026-06-22T02:03:39Z`). All 12 `*_report.md` + `_report.json`
sidecars written under `outputs/constraint_reports/` (gitignored — local artifacts, not committed).
Section C ("MAXENT SHADOW CLASSIFICATION") reads classical `maxent_top_type`/`maxent_probs` AND
`maxent_indexed.top_type` — so the fix surfaces in the rendered report.

Pre-fix copies of the 3 that already had reports: `report_witness_prefix/*_PREFIX.md`.

## Witnessed deltas in the rendered reports

**`fictional_construct_reading`** (FCR-routed, was boosted) — the boosted tangled_rope residual is gone:
- classical Distribution: `scaffold 0.932, tangled_rope 0.068` → `scaffold 1.000, tangled_rope 0.000`
- indexed Distribution: `scaffold 0.869, tangled_rope 0.131` → `scaffold 1.000, tangled_rope 0.000`

**`shinbutsu…incoherence_reading`** (constructed-routed, the categorical flip) — no prior report existed:
- classical line UNCHANGED by the fix: "MaxEnt … tangled_rope" P=0.563 (classical top never flips —
  the conditional boost only moves mass)
- **Indexed MaxEnt Top Type: snare** (P=0.616) — was tangled_rope pre-fix; the one manufactured
  verdict the fix corrects, now rendered honestly.

## Honesty caveat (corpus-epoch confound on the stale reports)

The 5 pre-existing reports were dated 2026-06-13…06-20 — BEFORE the OQ-138 conversions (06-21) and on
older corpus snapshots. Report-to-report diffs therefore conflate this fix with corpus growth +
ensemble refit (the MaxEnt ensemble is corpus-relative). The CLEAN, same-corpus witness for the fix
itself is `diff_witness.out` (baseline vs post-fix pipeline_output.json on the identical n=92 corpus),
not these report diffs. The regenerated reports are now fresh and consistent with the post-fix engine;
the fictional/shinbutsu deltas above are directionally the fix (tangled_rope mass removed / indexed
flip) and corroborate it, but the authoritative magnitude is the same-corpus slice.
