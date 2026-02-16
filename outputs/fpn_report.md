# Fixed-Point Network Iteration Report

*Generated: corpus-wide multi-hop purity propagation analysis via drl_modal_logic:fpn_run/3*

## Convergence Metadata

| Property | Value |
|----------|-------|
| **Constraints in corpus** | 1021 |
| **Constraints compared** | 992 |
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
| **Average EP shift** | 0.000140 |

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
| mountain | 124 | 0.9839 | 0.9839 | 0.0000 |
| rope | 43 | 0.8635 | 0.8634 | 0.0001 |
| snare | 511 | 0.4487 | 0.4486 | 0.0001 |
| tangled_rope | 267 | 0.5094 | 0.5089 | 0.0004 |
| unknown | 47 | 0.4538 | 0.4538 | 0.0000 |

---
*End of FPN report*
