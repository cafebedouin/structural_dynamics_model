# OQ-182 — Trajectory/giant_comp serialization: MECHANISM WITNESS — PASS

**Date:** 2026-06-27. **Leg:** `testsets/` (n≈104 — production scale, the scale at which
the original stall occurred). **Fix:** `python/run_pipeline.py` `_phase_prolog` — `trajectory`
pulled out of the parallel Phase-2 task set, run sequentially after `_run_parallel` returns.

## What licenses the flip
The two-arm **mechanism** witness (the two O(N²) heavy stages are provably never co-resident),
plus a positive control proving the battery scale exercises the failure path. NOT the run count.

## Method
A background sampler (`sampler.sh`: `ps -eww -o pid,rss,etimes,args` filtered to `swipl`,
every ~0.1s, each line wall-clock-stamped) runs during a full `trajectory_enabled=1` pipeline.
Stages disambiguated by their unique loaded module in `args`:
`giant_comp → giant_component_analysis.pl`, `trajectory → context_profile_report.pl`.

## Arm 1 — PRE-FIX positive control (CO-RESIDENCY CAPTURE) — PASS on run 1, deterministic
Pre-fix code (`git stash push -- python/run_pipeline.py`), flag=1. Dense (0.1s) capture:
- `giant_comp`: 6 samples, window [911.53, 912.18], peakRSS 16MB
- `trajectory`: 13 samples, window [910.91, 912.63], peakRSS 19MB
- **CO-RESIDENT: overlap = 0.64s** (giant_comp's entire swipl window sits inside trajectory's).
  This IS the proof the battery scale exercises the failure path — discharges the positive
  control by itself. (The full ~1-in-2 stall is the intermittent symptom *given* this overlap;
  both pre-fix runs here completed clean ~13–14s — per plan, NOT read as a failed control.)
- Log: `prefix_arm_sampler.log`, run: `prefix_arm_run.log` (exit 0, 12/12).

## Arm 2 — CURED arm (LOAD-BEARING — closes the thread-vs-subprocess gap) — PASS
Post-fix code restored (`results.append(_run_step("trajectory", ...))`), flag=1. Dense capture:
- `giant_comp`: 6 samples, window [945.42, 945.96]
- `trajectory`: 15 samples, window [946.75, 948.25]
- **DISJOINT: gap = 0.79s** — trajectory's swipl starts 0.79s AFTER giant_comp's swipl has
  exited. Zero RSS co-residency → the direct empirical proof of **subprocess** non-co-residency
  (not merely thread-join inference).
- Log: `cured_arm_sampler.log`, run: `cured_arm_run.log` (exit 0, 12/12).

## Timeout margin (measured, not assumed)
Trajectory measured **alive-window = 1.5s** at n≈104 (run log reports the step at 1.7s).
This is far *below* the plan's prior ~30s estimate, so the held 300s `run_prolog` timeout is a
**≥175× margin** — the margin claim strengthens. **Hold 300** (no 300→900 bump); a real
regression would still trip it.

## HALT condition — NOT triggered
Disambiguation succeeded (unique modules in `args`); pre-fix arm showed overlap at n≈104.
Witness witnessed → proceed to battery + flip.
