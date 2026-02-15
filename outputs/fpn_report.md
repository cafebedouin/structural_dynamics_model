# Fixed-Point Network Iteration Report

*Generated: corpus-wide multi-hop purity propagation analysis via drl_modal_logic:fpn_run/3*

## Convergence Metadata

| Property | Value |
|----------|-------|
| **Constraints in corpus** | 1021 |
| **Constraints compared** | 928 |
| **Iterations to convergence** | 4 |
| **Final max residual** | 0.000421 |
| **Converged** | Yes |
| **Epsilon** | 0.001000 |
| **Max iterations** | 20 |

## Summary

| Metric | Value |
|--------|-------|
| **Constraints with significant shift (>0.01)** | 3 |
| **Zone migrations** | 0 |
| **Max EP shift** | 0.017582 |
| **Average EP shift** | 0.000176 |

## Zone Migrations

No constraints changed purity zone under multi-hop propagation.

## Significant Movers (shift > 0.01)

| Constraint | Type | Intrinsic | One-Hop EP | FPN EP | Shift |
|------------|------|-----------|-----------|--------|-------|
| cmr_001 | tangled_rope | 0.5750 | 0.4436 | 0.4260 | 0.0176 |
| us_canada_geopolitical_asymmetry | tangled_rope | 0.5063 | 0.4202 | 0.4040 | 0.0163 |
| tiktok_us_divestiture_mandate | tangled_rope | 0.5063 | 0.4202 | 0.4040 | 0.0163 |

## Type Breakdown

Average EP shift by constraint type:

| Type | Count | Avg One-Hop EP | Avg FPN EP | Avg Shift |
|------|-------|---------------|------------|-----------|
| mountain | 71 | 0.9860 | 0.9860 | 0.0000 |
| rope | 42 | 0.8798 | 0.8792 | 0.0006 |
| snare | 508 | 0.4498 | 0.4498 | 0.0001 |
| tangled_rope | 259 | 0.5249 | 0.5244 | 0.0004 |
| unknown | 48 | 0.4572 | 0.4572 | 0.0000 |

---
*End of FPN report*
