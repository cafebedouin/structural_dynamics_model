# A3 (partial) — C-prov re-checkpoint on kernel_v1 — PASS

**Date:** 2026-06-26. **Leg:** `archives/datasets/kernel_v1` (1106 constraints).
**Plan step:** Part A / A3 (breadth leg). Harness: `c_prov_probe.pl` (unchanged
from the cheap-tier commit `fc4cadbf`), overlay `corpus_path` via `asserta`.

## Result — PASS (with positive control)
```
CORPUS: 1106 constraints loaded
SUMMARY: trajectory_summary(total_constraints(1106),families(30),
         cross_domain_twins(68730),anomalies(6))
PASS: classify_at_time_eps UNSET after trajectory_run
PASS: classify_at_time_theater UNSET after trajectory_run
EXCLUDED_CONSTRAINTS: 0 (no global set => no impute coupling => nothing excluded)
```
Positive control (separate process):
```
POSCTRL_CONSTRAINT: abrahamic_covenant__isaac_covenant_reading (Time=0)
classify_at_time(...) => Type=tangled_rope
PASS_POSCTRL: classify_at_time_eps SET = eps(...,0.45) (probe CAN detect a set global)
```
On the 1,106-story breadth corpus, `trajectory_run` leaves both `classify_at_time_*`
globals UNSET (no imputed value reaches the clustering); the positive control proves
the probe would detect a set global if one occurred. Confirms C-prov on breadth,
matching the testsets/ leg.

## C-null on kernel_v1 — DEFERRED (cost-gated; see status)
The frozen C-null protocol on kernel_v1 (1,106 stories × N=200 per-component null
draws; silhouette O(n²) per draw ≈ ~100× the testsets/ leg's per-draw cost) is the
expensive remaining A3 item. Deferred to operator discretion because the A4 gate
flip is **already blocked** by the C-gen (A2) failure — a tens-of-minutes
computation cannot change the "do not flip" outcome, and the operator's re-scope
(triggered by A2) may re-specify what breadth validation should even measure.
Re-runnable on request with `c_null_harness.pl` + a `corpus_path` overlay.
