# Twin conditioned re-analyses — RESULTS (OQ-125 / OQ-123)

Pre-registration: `audits/2026-07-04_twin_conditioned/PRE_REGISTRATION.md` (committed before this run).
Inputs: haiku `outputs/pipeline_output.haiku.json` sha256 `c3150612801eeed1…`; flash `outputs/pipeline_output.flash.json` sha256 `e6c01619c4e3bada…`
Intersection n = 960; permutations 1000; seed 20260613; per-analysis random.Random(f'{seed}:{tag}').

## Track A — OQ-125 conditioned |Δχ| (same-side = same seat type)

Observed AND band computed from the SAME same-side id set (stamped).

| field | typed pairs | same-side n | idset | obs mean\|Δχ\| | band5 | band95 | verdict |
|---|---|---|---|---|---|---|---|
| `chi:powerless` | 960 | 378 | `fbb336986b932a6e` | 0.0920 | 0.2605 | 0.2869 | VALUE_INVARIANCE_BEYOND_H1 |
| `chi:moderate` | 960 | 566 | `4668fdbf20b70a9f` | 0.0754 | 0.2676 | 0.2887 | VALUE_INVARIANCE_BEYOND_H1 |
| `chi:institutional` | 960 | 664 | `158e9890cf3265cb` | 0.0129 | 0.0188 | 0.0198 | VALUE_INVARIANCE_BEYOND_H1 |
| `chi:analytical` | 960 | 546 | `c3bc215426780b59` | 0.0879 | 0.3095 | 0.3346 | VALUE_INVARIANCE_BEYOND_H1 |

### Disagreeing-subset reach control

| field | n | obs | band5 | band95 | below-band | reach | reading |
|---|---|---|---|---|---|---|---|
| `chi:powerless` | 582 | 0.2298 | 0.2480 | 0.2682 | True | True | informative |
| `chi:moderate` | 394 | 0.1887 | 0.2663 | 0.2919 | True | True | informative |
| `chi:institutional` | 296 | 0.2128 | 0.2114 | 0.2128 | False | True | informative |
| `chi:analytical` | 414 | 0.2272 | 0.3337 | 0.3635 | True | True | informative |

## Track B — OQ-123 partition (persp:powerless)

Partition counts: absent(haiku)×absent(flash)=47, absent(haiku)×authored(flash)=67, authored(haiku)×absent(flash)=36, authored(haiku)×authored(flash)=805, imputed(haiku)×authored(flash)=5

Marginals: {'haiku': {'authored': 841, 'absent': 114, 'imputed': 5}, 'flash': {'authored': 877, 'absent': 83}}

| cell | n ids | both-pop | agree | rate | Wilson lo | Wilson hi | band5 | band95 |
|---|---|---|---|---|---|---|---|---|
| authored_both | 805 | 805 | 316 | 0.3925 | 0.3594 | 0.4267 | 0.2434 | 0.2882 |
| imputed_both | 0 | — | — | — | — | — | — | — | <!-- VACUOUS — empty cell -->
| imputed_one | 5 | 5 | 1 | 0.2000 | 0.0362 | 0.6245 | 0.0000 | 0.2000 |
| residual:absent(haiku)×authored(flash) | 67 | 67 | 25 | 0.3731 | 0.2672 | 0.4928 | 0.1493 | 0.2985 |
| residual:authored(haiku)×absent(flash) | 36 | 36 | 14 | 0.3889 | 0.2478 | 0.5514 | 0.1111 | 0.2778 |
| residual:absent(haiku)×absent(flash) | 47 | 47 | 22 | 0.4681 | 0.3333 | 0.6077 | 0.1702 | 0.3617 |
| UNCONDITIONED (all matched) | 960 | 960 | 378 | 0.3937 | 0.3633 | 0.4250 | 0.1938 | 0.2313 |

### Decision (authored-both alone; frozen rules)

- cell: authored_both
- power_benchmark: 0.672
- benchmark_wilson_lo: 0.638868141018353
- band95: 0.28819875776397513
- powered: True
- r_ab_minus_r_all: -0.001203416149068326
- outcome: PERSISTENT DIVERGENCE — (a) imputation-drag REFUTED; (b)-or-(c2) LIVE; B4 not auto-armed (operator discretion)

### Asymmetry probes ((c1))

- primary (imputed-one): {'haiku-omits(imputed)': 5, 'flash-omits(imputed)': 0, 'n': 5, 'larger_share_wilson_lo': 0.5655085052479191, 'reading': 'REPORT-ONLY — n=5 < 10'}
- secondary (any omission): {'haiku-omits(any)': 72, 'flash-omits(any)': 36, 'n': 108, 'larger_share_wilson_lo': 0.5733874039358369, 'reading': 'DIRECTIONAL (haiku-omits(any)) — (c1)-signature'}

### Seat-gradient bootstrap (95% percentile CIs)

- powerless: [0.3646, 0.4260]
- moderate: [0.5594, 0.6208]
- institutional: [0.6635, 0.7208]
- analytical: [0.5354, 0.5990]
- gradient robust (powerless hi < min other lo): True

## Sonnet control (`/home/scott/bin/structural_dynamics_model/prolog/testsets`, unpaired)

- slices: {'authored': 100, 'absent': 24, 'imputed': 4} (n=128)
- (i) classifier control: {'authored_slice_populated': True, 'imputed_slice_populated': True, 'reading': 'one-sided/THIN — imputed slice n=4 < 10', 'scope': 'does NOT control the pair-crossing (three-cell) logic'}
- (ii) authoring level: sonnet 0.781 vs twins haiku=0.876, flash=0.914 (pooled 0.895) → **INDETERMINATE**
  - non-blind (marginals observed at recon; disclosed in pre-reg); says NOTHING about (c2)

## Scope

- OQ-125 headline covers the 4 TYPED χ fields; `theater_ratio` stays exploratory (no seat type to condition on).
- One twin pair earns 'model-sensitive/invariant HERE' (haiku vs flash, this corpus, this commit) — never 'in general'.
