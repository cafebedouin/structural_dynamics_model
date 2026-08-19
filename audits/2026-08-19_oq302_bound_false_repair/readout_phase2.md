# OQ-302 Phase-2 readout — tag=phase2

| leg | N | source_arm | N_reaching | fires-control | declines-control | T4 pass | throws | agg mismatch |
|---|---:|---|---:|---|---|---:|---:|---:|
| `testsets` | 279 | defect | 236 | 279/279 = 100.0% | 43/43 still inconclusive(insufficient_data) | 0 | 0 | 0 |
| `testsets_haiku` | 960 | defect | 494 | 960/960 = 100.0% | 466/466 still inconclusive(insufficient_data) | 0 | 0 | 0 |
| `testsets_flash` | 960 | defect | 748 | 960/960 = 100.0% | 212/212 still inconclusive(insufficient_data) | 0 | 0 | 0 |
| `testsets_kimi` | 1005 | defect | 976 | 1005/1005 = 100.0% | 29/29 still inconclusive(insufficient_data) | 0 | 0 | 0 |
| `testsets_sonnet` | 1001 | defect | 1000 | 1001/1001 = 100.0% | 1/1 still inconclusive(insufficient_data) | 0 | 0 | 0 |
| `archives_datasets_kernel_v1` | 1106 | defect | 1104 | 1106/1106 = 100.0% | 2/2 still inconclusive(insufficient_data) | 0 | 0 | 0 |

**Totals:** 5311 constraints over 6 legs; N_reaching 4558; T4 pass 0; throws 0; agg mismatches 0.

## T1-T3 variation over the N_reaching set (PREREG 3)

| leg | N_reaching | distinct (T1,T2,T3) | distinct T1 | distinct T2 | distinct T3 | distinct repaired Result | verdict |
|---|---:|---:|---:|---:|---:|---:|---|
| `testsets` | 236 | 129 | 17 | 7 | 73 | 128 | **vary** |
| `testsets_haiku` | 494 | 234 | 24 | 8 | 88 | 234 | **vary** |
| `testsets_flash` | 748 | 133 | 16 | 7 | 66 | 132 | **vary** |
| `testsets_kimi` | 976 | 224 | 18 | 10 | 101 | 223 | **vary** |
| `testsets_sonnet` | 1000 | 263 | 16 | 10 | 100 | 262 | **vary** |
| `archives_datasets_kernel_v1` | 1104 | 270 | 25 | 9 | 86 | 270 | **vary** |

## Verdict marginals (whole leg, both arms)

- `testsets`: arm(defect) {'inconclusive': 279} | arm(repaired) {'variant': 236, 'inconclusive': 43}
- `testsets_haiku`: arm(defect) {'inconclusive': 960} | arm(repaired) {'inconclusive': 466, 'variant': 494}
- `testsets_flash`: arm(defect) {'inconclusive': 960} | arm(repaired) {'variant': 748, 'inconclusive': 212}
- `testsets_kimi`: arm(defect) {'inconclusive': 1005} | arm(repaired) {'variant': 976, 'inconclusive': 29}
- `testsets_sonnet`: arm(defect) {'inconclusive': 1001} | arm(repaired) {'variant': 1000, 'inconclusive': 1}
- `archives_datasets_kernel_v1`: arm(defect) {'inconclusive': 1106} | arm(repaired) {'variant': 1104, 'inconclusive': 2}

## T4 marginal (escalation clause, PREREG 4)

- `fail(natural_law_signature)`: 5311

## Error cells, per column (PREREG 6.4)

- thrown-error cells: NONE (0 across every column, every leg)
- failed (no-solution) cells: NONE

## agg_check (PREREG 0b — the per-test transcription cross-check)

- `match`: 4558
- `not_reached`: 753
