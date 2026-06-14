# OQ-116 MMC engine witness — what the engine actually does with a claimed-mountain / high-ε firing

**Date:** 2026-06-14. **Trigger:** operator pointed at the newly-generated corpora
(`prolog/testsets*`) while implementing the OQ-116 close. Converts the plan's "optional,
non-gating" residual check (run the cohort-zero ε=0.68/claim=mountain story through the
engine) from a proxy into a measured result — over **all 9** MOUNTAIN_METRIC_CONFLICT
firings in the live corpus (`prolog/testsets/`, 57 files), not just the one named story.

## Method

`cd prolog && swipl -q -g "[stack], corpus_loader:ensure_corpus_loaded, <forall over the 9>"`.
For each constraint: `signature_detection:false_natural_law/2` (fires?),
`signature_detection:boltzmann_compliant/2`, `drl_core:dr_type/2` (final type).
The 9 were found by linting `prolog/testsets/*.pl` and selecting MMC firings.

## Result (raw)

| constraint | FNL | final type | boltzmann |
|---|---|---|---|
| animal_status_kernel__property_reading | no | snare | inconclusive(insufficient_classifications) |
| demographic_resource_allocation | no | rope | compliant(0) |
| demographic_skill_mismatch_c0 | no | rope | compliant(0) |
| **institutional_trust_erosion_c0** (the cohort-zero ε=0.68 story) | **no** | **snare** | compliant(0) |
| jewish_self_determination__indigenous_return_reading | no | snare | inconclusive(insufficient_classifications) |
| neutron_star_bombardment_reading | no | tangled_rope | compliant(0) |
| organization_floor_c0 | **fires** | unknown | non_compliant(1.0,0.3) |
| scale_ceiling_c0 | no | rope | compliant(0) |
| secession_legitimacy_boundary__constitutional_impossibility_reading | no | snare | inconclusive(insufficient_classifications) |

## Findings (evidence, not doc-restatement)

**Framing (operator steer, OQ-74, this session):** these readings do NOT collapse to one
true type. The authored mountain claim is one seat; the engine's metric/signature reading is
another. The numbers below characterize the *divergence between seats*, not a "correction"
of a wrong claim — promoting the computed type over the authored claim would be the de-leak
principle in reverse (OQ-74). "Off mountain" = the metric seat reads differently, not "the
mountain claim was refuted."

1. **The metric seat diverges from the mountain claim on 9/9** (snare ×4, rope ×3,
   tangled_rope ×1, unknown ×1). MMC's "operator-readout" verdict is *reinforced*: every
   live firing is a genuine claim-vs-metric seat divergence — exactly the authored signal the
   lint exists to surface. None collapse; the mountain claim and the metric reading both stand.

2. **FNL is the MINORITY divergence-route, not THE route — fires on 1/9.** Only
   `organization_floor_c0` (Boltzmann `non_compliant`) trips `false_natural_law`. The other 8
   are Boltzmann `compliant(0)` or `inconclusive(insufficient_classifications)`, so FNL's gate
   (`boltzmann_compliant(C, non_compliant(_,_))`, `signature_detection.pl:993–994`) fails. The
   metric classifier (`classify_from_metrics/6`) is what reads high authored ε as
   snare/rope/tangled_rope; FNL is an *additional* signature seat that routes to tangled_rope
   only under Boltzmann non-compliance.

3. **Even when FNL fires, it does not always → tangled_rope.** `organization_floor_c0`
   fires FNL but resolves to **unknown**, because the base modal type is `unknown` and
   `resolve_modal_signature_conflict(unknown, false_natural_law, unknown)` (the OQ-37
   preserve-unknown clause, `signature_detection.pl:877`) takes precedence over the
   `_ → tangled_rope` clause (line 878). Preserve-unknown is itself an anti-collapse rule.

4. **The named cohort-zero story refines the plan's residual prediction.**
   The OQ-116 plan's Part A residual said "If it produces `false_natural_law → tangled_rope`...
   Falsifier: if it does NOT, that firing is a case where the engine corrects nothing."
   `institutional_trust_erosion_c0` → **snare, FNL=no**: the antecedent is false, AND the
   falsifier's "corrects nothing" is the wrong frame — the metric seat reads snare, a genuine
   divergence, no FNL needed. The plan's disjunction missed the common branch (metric-seat
   divergence without FNL). Under OQ-74 this is not "corrected vs not corrected" at all; it is
   "which seats diverge, and how."

## Consequence for the close

- MMC's operator-readout resolve **holds and is reinforced** (claim-vs-metric divergence on 9/9).
- The MMC message must NOT headline "false_natural_law → tangled_rope" as THE route (1/9), and
  must NOT frame the engine as "correcting" the claim. Reworded message (applied to
  `python/linter.py` this session) frames it as a claim-vs-metric-seat **divergence that need
  not collapse** (OQ-74 / seat theorem), names the metric seat (snare/rope/tangled_rope) and FNL
  (tangled_rope under Boltzmann non-compliance) as the two seated readings.
- This is the "FSM → FNL" correction from the plan, taken two rungs further by measurement:
  (i) **metric-seat-primary, FNL-secondary**; (ii) **divergence, not correction** (OQ-74).
  OQ-116's original "FSM exists for it" was wrong; the plan's "FNL is the analog" was right in
  kind but overstated FNL's share and leaned toward a collapse frame the seat theorem forbids.

Raw goal output: `engine_run.out` in this directory.
