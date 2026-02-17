# Fixed-Point Network Iteration Report

*Generated: corpus-wide multi-hop purity propagation analysis via drl_modal_logic:fpn_run/3*

## Convergence Metadata

| Property | Value |
|----------|-------|
| **Constraints in corpus** | 1032 |
| **Constraints compared** | 1007 |
| **Iterations to convergence** | 4 |
| **Final max residual** | 0.000709 |
| **Converged** | Yes |
| **Epsilon** | 0.001000 |
| **Max iterations** | 20 |

## Summary

| Metric | Value |
|--------|-------|
| **Constraints with significant shift (>0.01)** | 5 |
| **Zone migrations** | 0 |
| **Max EP shift** | 0.034135 |
| **Average EP shift** | 0.000218 |

## Zone Migrations

No constraints changed purity zone under multi-hop propagation.

## Significant Movers (shift > 0.01)

| Constraint | Type | Intrinsic | One-Hop EP | FPN EP | Shift |
|------------|------|-----------|-----------|--------|-------|
| boom_bust_path_dependency | rope | 0.7428 | 0.5524 | 0.5182 | 0.0341 |
| cmr_001 | tangled_rope | 0.5750 | 0.4436 | 0.4260 | 0.0176 |
| us_canada_geopolitical_asymmetry | tangled_rope | 0.5063 | 0.4202 | 0.4040 | 0.0163 |
| tiktok_us_divestiture_mandate | tangled_rope | 0.5063 | 0.4202 | 0.4040 | 0.0163 |
| omega1_patching_process | tangled_rope | 0.5063 | 0.4676 | 0.4565 | 0.0110 |

## Type Breakdown

Average EP shift by constraint type:

| Type | Count | Avg One-Hop EP | Avg FPN EP | Avg Shift |
|------|-------|---------------|------------|-----------|
| mountain | 126 | 0.9841 | 0.9841 | 0.0000 |
| rope | 47 | 0.8543 | 0.8534 | 0.0009 |
| snare | 511 | 0.4485 | 0.4485 | 0.0001 |
| tangled_rope | 323 | 0.5011 | 0.5006 | 0.0004 |

---
*End of FPN report*
