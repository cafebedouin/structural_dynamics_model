# Audit 3 Report — Profile Accumulation Impact on Corpus

Manifest: pipeline_output.json commit `f6f47db`, run `2026-05-17T23:41:52Z`, n_constraints=3371
Audit sample: 481 constraints from corpus of 3371 (full corpus run, sample filters output)
Config.pl thresholds read at audit time: entropy_thresh=0.15, uncertainty_thresh=0.4, shadow_thresh=0.85, stress_min=4, purity_thresh=0.6, coupling_thresh=0.75, drift_mode=any

## Output 1: Population-Level Impact Statement

185 of 3371 constraints in the full corpus show top-type divergence between clean and accumulated sessions (5.5%). 224 of 481 constraints in the sample show trigger-firing divergence on at least one of the six trigger conditions (46.6% of sample). 
Missing data: 0 constraints had no clean distribution, 0 had no accumulated distribution.
The discrepancy is **moderate (1–10% of corpus)**.

## Output 2: Trigger-Class Impact Table

Sample N = 481. Counts: how many sample constraints fall in each cell.

| Condition | clean_only | accum_only | both | neither |
|-----------|-----------|-----------|------|---------|
| t1 | 57 (11%) | 13 (2%) | 197 (40%) | 214 (44%) |
| t4 | 1 (0%) | 0 (0%) | 0 (0%) | 480 (99%) |
| t9 | 9 (1%) | 43 (8%) | 16 (3%) | 413 (85%) |
| elevated_entropy | 208 (43%) | 1 (0%) | 206 (42%) | 66 (13%) |
| maxent_hard_disagree | 57 (11%) | 13 (2%) | 197 (40%) | 214 (44%) |
| t10 | 7 (1%) | 42 (8%) | 3 (0%) | 429 (89%) |

**Notes on T4:** Orbit and drift (session-independent conditions) were not available from pipeline_output.json for constraints not previously confirmed as T4. Conservative: only the known confirmed_liminal constraint was checked for T4 accumulated firing; new T4 candidates in accumulated session (H_accum > 0.40 but not previously T4) are not counted (orbit unverified).

**Notes on T1 approximation:** T1 accumulated fires if the constraint has a known override signature AND accum_hard_disagree. This approximates `det_type == override_target(sig)` as always true when the signature is active. Constraints where the signature is present but det_type ≠ override_target would be over-counted.

## Output 3: Wasserstein Dependency Statement

**Code-reading findings:**

```prolog
% measurement_layer.pl — wasserstein_edge_transport/4
wasserstein_edge_transport(C, Ctx1, Ctx2, W1) :-
    maxent_classifier:maxent_distribution(C, Ctx1, P),
    maxent_classifier:maxent_distribution(C, Ctx2, Q),
    wasserstein_l1(P, Q, W1).

% maxent_classifier.pl — maxent_distribution/3 definition
maxent_distribution(C, Context, Dist) :-
    maxent_dist(C, Context, Dist).

% maxent_classifier.pl — maxent_dist/3 assertz call
assertz(maxent_dist(C, Context, FinalDist))
```

maxent refs in measurement_layer.pl: ['maxent_classifier', 'maxent_distribution', 'maxent_multi_run']
edge transport calls in wasserstein_transport_profile: 3 (needs all 4 contexts)

**Verdict:**
Wasserstein reads from `maxent_dist/3` (via thin wrapper `maxent_distribution/3` at measurement_layer.pl:214–215, confirmed: `maxent_distribution(C, Context, Dist) :-
    maxent_dist(C, Context, Dist).`); `wasserstein_transport_profile/2` makes 3 calls to `wasserstein_edge_transport`, requiring distributions at all 4 canonical contexts simultaneously in `maxent_dist/3`, so a cleanup-between-contexts fix is NOT safe for Wasserstein without redesigning distribution storage.

## Verification

Sanity check — `collective_action_as_leverage_conversion`:
  clean_top_type=tangled_rope, clean_H=0.4456
  accum_top_type=scaffold, accum_H=0.000229
  Expected (Audit 2): clean≈tangled_rope/H≈0.45, accum≈scaffold/H≈0.000229
  Sanity: clean=PASS, accum=PASS