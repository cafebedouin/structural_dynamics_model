# C0 — commentary-only pipeline diff (corroboration) — PASS

**Date:** 2026-06-26. **Leg:** `testsets/` (n=104). **Plan step:** Part A / A1.

## Protocol
Same-session before/after of `outputs/pipeline_output.json` across the
`config.pl:571 trajectory_enabled` flag, both runs in this session (no prior-run
baseline — build_discipline's byte-identical-on-stale trap):
1. baseline run, flag=0 (`c0_baseline_run.log`) → `c0_pipeline_baseline.json`
2. flag flipped 0→1, run (`c0_enabled_diag.log`) → `c0_pipeline_enabled.json`
3. flag **reverted** to 0 (verified `git diff prolog/config.pl` empty)
4. normalized diff (`c0_diff_witness.log`)

Both runs: **exit 0**, output **mtime advanced** (baseline 18:42:41 → enabled
18:55:01). Report `context_profile_report.md` went 0 → 7129 bytes, well-formed
(11 families — matches the C-null witnessed partition).

## Result — PASS
- `per_constraint` (104 entries): **0 deltas**.
- `diagnostic`, `validation`, `type_hierarchy`: byte-identical.
- Only difference after normalizing `manifest.pipeline_run_at`:
  `config.trajectory_enabled: 0 → 1` — the flag value echoed into the serialized
  config block, **not** a classification effect.
- **Positive control (teeth):** planting a single classification field
  (`claimed_type`) into a copy of the enabled output IS flagged by the same
  per_constraint diff. The probe is not blind. (Available classification fields
  confirmed present: claimed_type, signature, purity_score/band, classifications,
  maxent_top_type, signature_pressure, diagnostic_verdict, verdict_join, cs_verdicts.)

Corroborates the structural primary already on record (the trajectory stage,
`run_pipeline.py:516–546 _prolog_trajectory`, writes **only**
`context_profile_report.md`; `pipeline_output.json` is written solely by the
manifest step, which does not read trajectory output).

## Caveat carried to A4 (gate flip) — NOT a C0 failure
The **first** flag=1 attempt stalled past 10 min and was killed (the trajectory
report wrote at +22s but `pipeline_output.json` was never reached). A second,
bounded flag=1 run completed cleanly in **12.6s, exit 0, 47/47 steps**. The
trajectory stage runs **in parallel** (one of 12 Phase-2 tasks, 4 workers,
`run_pipeline.py:711–725`) alongside `giant_comp` — the known OOM/SIGSEGV-prone
stage. The transient stall is consistent with added concurrent memory pressure
from HAC clustering raising `giant_comp`'s failure probability, **not** a
deterministic trajectory hang. Flagged for the operator at A4: enabling the
flag for every pipeline run may want the trajectory stage serialized or
`giant_comp` isolated. Does not affect the C0 invariant (the completed run's
output is byte-identical in all classification fields).
