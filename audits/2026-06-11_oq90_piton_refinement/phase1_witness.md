# Phase 1 witness — substrate (behavior-preserving)

Commit scope: `prolog/narrative_ontology.pl` (uncaptured/1, piton_candidate/1,
transient_neglect/1), `prolog/signature_detection.pl` (fcr_evidence/6→/7 +
capture_disposition/2, all 4 in-file sites updated). `validation_suite.pl` intentionally
NOT regenerated here (its only diff is corpus-membership renumbering driven by 4 untracked
testsets — see corpus_drift_provenance; regenerating it would commit dangling references to
uncommitted testsets). No JSON-serialized change (json_report serializes only the signature
atom; fcr_evidence is destructured nowhere outside signature_detection.pl — verified by grep).

## Behavior-preserving proof

**Pipeline verdict diff (old-vs-new, same 52-corpus working tree):** 0 verdict-field diffs
across all 52 constraints over {signature, perspectives, claimed_type, classifications}.
Baseline `pipeline_output.preedit.json` (run 2026-06-11T16:44:05Z, commit 411db0e7) vs the
post-edit run. The fcr_evidence field is populated but read by no classification clause.

```
old n: 52 new n: 52
TOTAL verdict-field diffs: 0
```

**Disposition field populated on the live path** (smoke probe):
- `regulatory_measurement_gap` → fcr_evidence(explicit_rope_claim, [...], ..., **piton_candidate**)
- `institutional_trust_erosion` → fcr_evidence(low_extraction_profile, [...], ..., **piton_candidate**)
- `organization_floor` → NO FCR (CI_Rope-certified; disposition read from predicate, = piton_candidate)

## Suite green

- `validation_suite` / `run_dynamic_suite`: **0 errors, 0 warnings, 1 info**. (The "piton check
  VACUOUS: 0 resistance_to_change facts" info line is OQ-37, pre-existing — and is precisely the
  old resistance-keyed piton gate this OQ supersedes.)
- `tests/test_snapshot_migration`: all 10 passed.
- `tests/test_maxent_profile_indexing`: all passed.
- `tests/test_contradiction_signatures`: **5 failures — PRE-EXISTING, not introduced here.**
  Positive control: stashing my two file changes and re-running the test at baseline 411db0e7
  reproduces the identical 5 failures (`both_signatures_expressible` et al.); the test references
  neither `fcr_evidence` nor `false_ci_rope` (grep empty). Logged as an unrelated standing failure,
  not an OQ-90 regression.

## Sweep (per-diffuse-story disposition trace — ruling 4)

`capture_disposition_sweep.out`:

```
corpus N=52 | diffuse=4 | piton_candidate=4 | transient_neglect=0 | captured=7
```

| constraint | gain_flow | fixing_cost | signature | disposition[source] | dr_type(analytical) |
|---|---|---|---|---|---|
| institutional_trust_erosion | diffuse | prohibitive | false_ci_rope | piton_candidate[fcr_evidence] | tangled_rope |
| regulatory_measurement_gap | diffuse | prohibitive | false_ci_rope | piton_candidate[fcr_evidence] | tangled_rope |
| organization_floor | diffuse | prohibitive | coupling_invariant_rope | piton_candidate[predicate] | rope |
| reprogramming_safety_toxicity | diffuse | prohibitive | coupling_invariant_rope | piton_candidate[predicate] | rope |

**Ruling-4 caveat, in substrate:** 4 piton_candidates exist, but only 2 are reachable by the FCR
refinement; the other 2 are shadowed by CI_Rope certification *upstream* of FCR. "Piton sparse" is
never to be read without this upstream-shadow caveat. `transient_neglect` cell corpus-empty.
