# Phase 3 witness — the refinement (OUTPUT-CHANGING)

Code: `config.pl` (param `piton_refinement_enabled` default 1), `config_schema.pl` (its
oneof([0,1]) spec — the config schema gate fail-loud requires it), `signature_detection.pl`
(the new cascade clause). `validation_suite.pl` intentionally NOT in this commit — its diff is
pure test_case renumbering driven by the 4 untracked testsets (verified: no non-`test_case`
lines changed), and committing it bakes dangling references to uncommitted testsets.

## Committed cascade sequence (position-encoded priority)

```prolog
resolve_with_perspectival_check(C, piton, false_ci_rope, piton) :-      % :789 dead-coordination
    drl_core:coordination_dead(C), !.
resolve_with_perspectival_check(C, _ModalType, false_ci_rope, piton) :- % :801 NEW capture piton
    config:param(piton_refinement_enabled, 1),
    narrative_ontology:piton_candidate(C), !.
resolve_with_perspectival_check(C, ModalType, false_ci_rope, AdjustedType) :- % :804 generic FCR
    !, ...
```

The new clause sits between the two neighbors. The Phase-2 battery (re-run through this committed
code, not an overlay) exercises both boundaries: shape 1 fires the capture clause (piton); shapes
2–4 fall through it to the generic clause — proving dispatch order is correct and the off-by-one
is not present.

## Pipeline diff — `piton_refinement_enabled` 0 vs 1 (same 52-corpus working tree)

Manifests: arm0 2026-06-11T21:03:49Z, arm1 2026-06-11T21:03:53Z, n_constraints=52 both,
commit 411db0e7 working-tree-dirty. Artifacts: `pipeline_output.refine0.json`,
`pipeline_output.refine1.json`.

**Perspective-type delta = EXACTLY two rows (the re-registered expectation):**

```
institutional_trust_erosion:  powerless/moderate/institutional/analytical  tangled_rope -> piton
regulatory_measurement_gap:   powerless/moderate/institutional/analytical  tangled_rope -> piton
constraints with perspective-type changes: 2
```

- **Signatures unchanged** across all 52 (dr_signature stays `false_ci_rope` for both flippers —
  the invariant: piton is an FCR-branch dr_type refinement, NOT a signature change).
- **Leak controls held:** `organization_floor` and `reprogramming_safety_toxicity` stay `rope` at
  all four perspectives (byte-identical arm0/arm1). They are diffuse+prohibitive piton_candidates
  but CI_Rope-certified upstream of FCR, so the refinement never reaches them — the designed
  shadow, not a miss.
- **Corpus-fitted ripple (expected, not a stop signal):** 47 other constraints differ only in
  `maxent_probs` / `raw_maxent_probs` / `maxent_entropy` / `wasserstein_*` / `arakelov_height` /
  `signature_pressure` (corpus-relative distributions recomputed when two classifications move),
  plus a few `diagnostic_verdict` / `contamination_network` recomputations downstream of the two
  type flips (network-coupled neighbors). No third constraint changes its `perspectives`. Full
  field-level census: `phase3_ripple_summary.txt`.

**Verdict:** zero-delta would have been a failure (the flips are pre-witnessed reachable); a third
flip or a leak-control flip would have been a failure. Observed: exactly the two re-registered
rows, signatures stable, leak controls stable. PASS.
