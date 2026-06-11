# Archived pipeline outputs (pre-reset forensic baselines)

Frozen pre-reset (pre-2026-06-05) output artifacts relocated out of the live gitignored
`outputs/` tree per the OQ-33 close (operator disposition ruling 2026-06-11; evidence and
checksums: `audits/2026-06-11_oq33_close/`). These are historical record, not live substrate —
cite them only with their manifest stamp.

- `2026-06-03_pipeline_output_pre_agency_fix.json` — full pipeline output snapshot taken just
  before the FSM agency-gate fix (manifest `pipeline_run_at=2026-06-03T16:10:13Z`, commit
  `669eab5`, n=1106 + the pre-2026-06-04 `catholic_church_1200` demo entry = 1,107
  per_constraint rows; drift fields populated). Retained as a pre-fix forensic baseline.
- `2026-06-04_schema_sieve/` — orphaned extractor output of `python/audits/schema_sieve.py` +
  `schema_sieve_analyze.py` (manifest `pipeline_run_at=2026-06-04T14:15:56Z`, commit `1460e87`,
  n=1106/189 sotu); no audit writeup ever consumed it.
