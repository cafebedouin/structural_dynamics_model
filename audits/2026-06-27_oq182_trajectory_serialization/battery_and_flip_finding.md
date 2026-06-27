# OQ-182 — Liveness battery + freshness control + C0 re-witness + flip — PASS

**Date:** 2026-06-27. **Leg:** `testsets/` (n≈104). Fix in place (trajectory serialized),
all runs `trajectory_enabled=1`.

## 2. Liveness battery — N=10 post-fix flag=1 runs — 10/10 GREEN
`battery.py 10` → `battery_n10.log`. Per run asserted: exit 0; no error steps;
`trajectory==ok` AND `giant_comp==ok`; `giant_comp` in serial band (<60s); wall <600s;
`context_profile_report.md` mtime advanced past run-start AND size>0 (the literal A4 freshness
criterion); `pipeline_output.json` mtime advanced.
- 10/10 GREEN. Wall 14.1–15.1s; giant_comp 0.6–0.8s (vs 600s stall threshold); trajectory ok
  1.6–1.9s; report 7129 bytes every run. No stall, no co-residency regression.

## 3. Freshness-detector positive control (non-vacuity) — PASS
`freshness_control.py` → `freshness_control.log`. Seeds a REAL empty, old-mtime report and runs
the battery's exact stat-based predicates against it, plus a planted 650s giant_comp:
- empty report (size 0) → FLAGGED; stale mtime (run_start−100s) → FLAGGED; 650s stall → FLAGGED.
- Proves the green battery saw real freshness, not a probe that never looked.

## 4. C0 invariant re-witness — PASS
`c0_diff.py` → `c0_rewitness.log`. flag=0 baseline (`c0_pipeline_baseline.json`, captured this
session, post-fix) vs flag=1 enabled (`c0_pipeline_enabled.json`, battery's last run):
- `config.trajectory_enabled`: 0 → 1 (proves the enabled output is genuinely flag=1).
- classification-field diff (per_constraint/diagnostic/validation/type_hierarchy): **ZERO**.
- Positive control (plant `claimed_type` on entry 0): **CAUGHT**. The diff has teeth.
- Confirms the fix changes only *when* trajectory runs, not *what* it writes.

## Flip — config.pl:571 trajectory_enabled 0 → 1
- `validate_config: PASS` at flag=1 (consulted `config_validation.pl`).
- `trajectory_weights_sum` gate **active and satisfied**: 0.35+0.25+0.25+0.15 = 1.0
  (non-vacuous — the violation goal does not succeed).
- Enabled `context_profile_report.md` landed (7129 bytes, 11 families).

## Verdict
All four witness components GREEN + config validation confirmed → flip licensed and applied.
Revert path (revert-on-red) unused.
