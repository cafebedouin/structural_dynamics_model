# RECON — OQ-261 forced-gluing experiment (fiat_efficacy_kernel family frame)

**Executed:** 2026-08-07 (read-only; post-Item-B edge-naming reconciliation).
**OQ:** OQ-261.
**Manifest cite:** `pipeline_output.json` manifest `pipeline_run_at=2026-08-07T23:44:21Z`,
`n_constraints=225`, `code_commit_short=f724379`, `code_dirty=true`. Corpus leg
fingerprints (md5 over concatenated `*.pl`, held identical across the whole session's
diff pair): testsets `d555ff4c98e08adf8e651df36582734c` (225 files), haiku `f697246d…`,
flash `6c6a2dbd…`, kimi `57d48523…`, sonnet `2427448c…`.

**Instruments:** `family_frame_probe.pl` (this dir; run from `prolog/` with
`swipl -l … -g "recon, halt"`), raw output `family_frame_probe_output.txt`;
`flat_control_fingerprint.txt`; `verdict_join_headlines.txt`.

## A. Resolved edge table (committer axis, authored-edge-only)

All 30 authored `cs_reading_relation` edges of the 6-reading family resolve through
`cs_kernel_registry:cs_edge_target_member/4` (post-`4f646665`; every authored target was
prefixed `fiat_efficacy_kernel__X`, resolved by the strip form). Per-pair summary
(15 pairs; full table in `family_frame_probe_output.txt`):

- **13 pairs carry `coexists_with`** (some also `influences`).
- **2 pairs carry `forecloses`:** `empirical_precedent → utopian_fiction` and
  `predictive_synthesis → utopian_fiction`. **Both are ASYMMETRIC:** the reverse
  direction of each pair is authored `coexists_with` (utopian_fiction claims coexistence
  with both readings that foreclose it). The obstruction predicate counts a pair
  foreclosing if EITHER direction forecloses, so the asymmetry does not change H1r —
  but it is authored structure the proposal should not ignore.
- 2 pairs are edge-typed only by `influences`
  (`scholarship|predictive_synthesis`, `truth_procedure|utopian_fiction`).

## B. Kernel obstruction (the committer-axis "does it glue" verdict)

`cs_kernel_obstruction(fiat_efficacy_kernel, H1r=2, ClosureN=2, PluralityN=13)`,
status **`real_closure`** — matches the Item-B pre-derivation exactly.

Axiom layer: 2 authored `cs_axiom_contradiction` pairs
(`truth_procedure ↔ utopian_fiction`, `empirical_precedent ↔ empathy_simulation`, from
`fiat_efficacy_kernel_contradictions.pl`; engine confirms exactly these 2 conflicts).
**Neither coincides with a forecloses pair** (pre-derived and confirmed): the trifurcation
diagnostic for this kernel is `axiom_foreclosed(edge_only)` — foreclosure authored on
pairs the axiom layer does not indict, and axiom conflicts on pairs authored coexistent.

## C. Kernel-family H¹ frame (observer axis, computed — NEVER fed into B)

Per-context reading-type vectors (from `compare_kernel_readings/3` Profile verdicts) fed
into pure `grothendieck_cohomology:obstruction_from_vector/3`, over all 156 contexts:

- **73 contexts glue** (H0=1), **80 obstruct** (H1>0), **3 undetermined** (<2 real seats).
- H1 histogram: `0×73, 3×14, 6×36, 9×30`. **Every numeric H1 in-spectrum** for its
  context's n_real (probe prints a BUG WITNESS line on any violation; none fired —
  H(6)={0,5,8,9,11,12,13,14,15}, smaller-n bands for unknown-thinned contexts).
- **Two clean observer-frame blocs** (pairwise section-graph Jaccard): bloc A =
  {scholarship, empirical_precedent, truth_procedure} (all `claimed_type=tangled_rope`),
  bloc B = {empathy_simulation, predictive_synthesis, utopian_fiction} (all
  `claimed_type=rope`). Jaccard = 1 within blocs, 0 across. H1=9 is exactly the 3+3
  bloc split (15−3−3); H1∈{3,6} are unknown-thinned contexts.
- **Cross-axis observation (Theorem 7 shape, worth carrying into the proposal):** one
  forecloses pair is cross-bloc (`empirical_precedent → utopian_fiction`) but the other
  is WITHIN bloc B (`predictive_synthesis → utopian_fiction`) — the committer axis
  declares no-global-section between readings the observer axis cannot distinguish at
  any context. Detection independence, instantiated in authored data.

## D. Per-story stakeholder-frame reads (7 stories: 6 readings + flat control)

From `family_frame_probe_output.txt` and `verdict_join_headlines.txt`:

| story | consensus_provenance | excluded seats | verdict_join |
|---|---|---|---|
| empirical_precedent | mc_candidate_untypeable([2 seats]) | 2 | **null** |
| scholarship | mc_candidate_untypeable([1]) | 1 | **null** |
| truth_procedure | mc_candidate_untypeable([2]) | 2 | yellow |
| predictive_synthesis | mc_candidate([empirical_policy_scientists]) | 1 | red |
| empathy_simulation | mc_candidate([skeptical_debate_theorists]) | 1 | red |
| utopian_fiction | mc_candidate([policymakers, debate_critics]) | 2 | red |
| flat_control | mc_candidate([elected_officials_and_agencies]) | 1 | red |

All 7 stories (control included) read `manufactured_consensus_candidate[_untypeable]` —
every story authored its dissenting seats as `excluded`. `verdict_join` is authored-null
for 2 of 6 readings (absence token per OQ-98 — recorded, not coerced; reason not
diagnosed here).

## E. Flat-control fingerprint (operator rider: "same substrate" is a CLAIM)

Full table: `flat_control_fingerprint.txt`. Summary of the comparison the proposal must
cite instead of asserting substrate identity:

- **Same topic:** `topic_domain` identical (`debate_theory/political_philosophy`, modulo
  spacing/underscore variants in 2 readings); all 7 `human_readable` titles are
  fiat-efficacy subjects.
- **Metric profile:** control ε=0.38 / sup=0.24 / tr=0.42 sits INSIDE the readings'
  ranges (ε 0.22–0.42, sup 0.15–0.35, tr 0.22–0.48) — closest to
  `empirical_precedent_reading`; `claimed_type=rope` (matches bloc B, not bloc A).
- **Structural differences (NOT identical substrate):** the control authors **no
  `cs_kernel_id`** (by design — it is outside the family cover), **no
  `coordination_type`** (the 6 readings author `identity_coordination`; purity for the
  control is in the OQ-60 no-data stratum), and its 8-seat roster shares only ONE seat
  name with any reading (`academic_debate_community`, with scholarship_reading) — seat
  rosters are otherwise disjoint across all 7 stories.

**Verdict for the proposal:** the control is same-TOPIC, metric-range-interior, but NOT
same-substrate in seats or CS-layer authoring. Any H_perf/H_topic contrast leaning on
"identical substrate minus the kernel structure" overclaims; the licensed claim is
"same topic and metric regime, absent the family cover."

## F. Frame-mismatch note (stated so the proposal cannot conflate frames)

Three distinct frames appear above and are NOT interchangeable:
1. **Committer/kernel frame (B):** `cs_kernel_obstruction` over authored edges —
   family-level, observer-blind, the OQ-261 "topic kernel" object.
2. **Observer/family frame (C):** per-context H¹ over the readings' dr_type vectors —
   family-level but computed, gradient-orthogonal to B by Theorem 7; used here as
   context only, never fed into B (observer-blindness constraint).
3. **Stakeholder frame (D):** `consensus_provenance/2` and `h1_stakeholder` are
   PER-STORY over stakeholder seats. **There is no family-level analogue of
   `consensus_provenance/2`** — a proposal that treats story-level
   manufactured-consensus tokens as a family-level gluing verdict conflates frames.

## Open facts carried to the proposal

- The family is `real_closure` (H1r=2) with plurality mass 13 — the "topic presheaf
  does not glue" precondition for the experiment HOLDS on the committer axis, while the
  observer axis says "two internally-glued blocs."
- The performance presheaf candidate (the debate-community seats every reading names —
  debaters/coaches/judges/programs appear in every reading's agent roster) is present in
  the authored seat data; the flat control names a policy-community roster instead.
- 2/6 readings have null `verdict_join` — any ballot-model read over verdict_join must
  handle the absence token explicitly (OQ-51/OQ-98 discipline).
