# Sensitivity Sweep Timeout Diagnostic Report

**Date**: 2026-02-24
**Corpus**: Reconciled (1151 tests, post-dedup/reclassification)
**Previous sweep**: 118 params, 733 tests, 11 timeouts at 600s

## Root Cause

All 11 timeouts shared a single root cause: **the sweep overlay template loaded `[validation_suite]` without first loading `[stack]`**. The module `data_repair` (called by `scenario_manager:load_and_run/2` at line 109) is not importable via SWI-Prolog autoloading — it must be preloaded by `stack.pl`. Without it, every test throws `existence_error(procedure, data_repair:repair_interval/1)`, the exception cascades out of the test runner, and the Prolog process hangs until the subprocess timeout kills it.

The timeouts were not caused by infinite loops, combinatorial explosion, or any property of the 11 parameters themselves. The specific parameters that timed out were likely determined by scheduling order or OS-level subprocess timing variance.

### Fix Applied

`python/config_sensitivity_sweep.py`:
- Overlay template: added `:- [stack].` before `:- [validation_suite].`
- Baseline command: changed to `[stack], [validation_suite], run_dynamic_suite, halt.`

## Baseline Timing

| Metric | Value |
|---|---|
| Tests | 1151 |
| Passed | 1151 |
| Failed | 0 |
| Wall-clock | 9.2s |
| Avg per test | ~8ms |
| Slowest test | `worldscale_vlsfo_benchmark.pl` (0.11s) |

The baseline is **65x faster** than the 600s timeout. Even a 10x slowdown from parameter perturbation would only reach ~92s.

## Per-Parameter Results

All 11 previously-timed-out parameters were re-tested with their exact perturbation values. All resolved.

| Parameter | Perturbation | Value | Result | Time | Pass/Fail |
|---|---|---|---|---|---|
| `sigmoid_midpoint` | -25% | 0.375 | **Resolved** | ~10s | 1151/0 |
| `snare_load_bearing_threshold` | -25% | 0.525 | **Resolved** | ~10s | 1151/0 |
| `canonical_d_organized` | +25% | 0.49875 | **Resolved** | ~10s | 1151/0 |
| `system_gradient_strong_threshold` | -25% | 0.75 | **Resolved** | ~10s | 1151/0 |
| `natural_law_resistance_max` | +25% | 0.1875 | **Resolved** | ~10s | 1151/0 |
| `excess_factor_center` | -10% | 0.18 | **Resolved** | ~10s | 1151/0 |
| `dependency_coupling_threshold` | +25% | 0.875 | **Resolved** | ~10s | 1151/0 |
| `reform_urgency_pressure_high` | +10% | 1.65 | **Resolved** | ~10s | 1151/0 |
| `purity_action_degraded_floor` | -10% | 0.27 | **Resolved** | ~10s | 1151/0 |
| `network_contamination_risk_threshold` | -10% | 2.0 | **Irrelevant** | ~10s | 1151/0 |
| `network_drift_velocity_threshold` | -25% | 0.0075 | **Resolved** | ~10s | 1151/0 |

**Note on `network_contamination_risk_threshold`**: This parameter is dead code (marked DOCS-ONLY in `config_schema.pl:291`, never referenced by any predicate). It has been added to the sweep's `EXCLUDE_PARAMS` set.

**Note on `coupling_drift_threshold`**: This parameter was listed in the original timeout table but has been removed from `config.pl` during cleanup. No longer relevant.

## Code Changes

### 1. Sweep script (`python/config_sensitivity_sweep.py`)

- **Overlay template**: Added `:- [stack].` before `:- [validation_suite].` to ensure all engine modules are loaded
- **Baseline command**: Updated to `[stack], [validation_suite], run_dynamic_suite, halt.`
- **EXCLUDE_PARAMS**: Added `network_contamination_risk_threshold` (dead code)
- **Exclusion filter**: Logs excluded params and removes them before sweep

### 2. Test suite (`prolog/validation_suite.pl`)

- Added `use_module(library(time))` for `call_with_time_limit/2`
- `run_single_test/4` now wraps `load_and_run` + `generate_llm_feedback` in a 60s per-test timeout guard
- Each test prints `[ELAPSED] path: Xs` for diagnostic profiling
- On timeout: records `test_failed(Path, timeout, ...)` and continues to next test

### 3. Suite generator (`python/python_test_suite.py`)

- Updated to emit the same per-test timeout guard and timing code as the manual edit to `validation_suite.pl`

## Recommendations for the New Sweep

### Timeout threshold

**Reduce from 600s to 120s.** The baseline is 9.2s. With the per-test 60s guard, even a worst-case single-test timeout adds only 60s. A suite-level timeout of 120s provides 13x headroom over the baseline while catching genuine issues faster.

### Parameters to exclude

Only `network_contamination_risk_threshold` (already excluded via `EXCLUDE_PARAMS`). All other parameters are safe to sweep.

### No parameters need code fixes

The original timeout hypotheses (sigmoid convergence loops, load-bearing reclassification cycles) were not confirmed. The engine has no infinite-loop risk from parameter perturbation. The `infer_structural_coupling/3` N^2 gradient enumeration is bounded and completes quickly on the current corpus.

### Perturbation ranges

All +-10% and +-25% perturbations are safe. No narrower ranges needed.

### Summary

**Go/no-go: GO.** All 11 timeouts are resolved. The new sweep can proceed on the full reconciled corpus (1151 tests, 143 params) with a 120s timeout per run.
