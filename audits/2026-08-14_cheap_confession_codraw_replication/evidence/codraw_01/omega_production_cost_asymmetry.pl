% ============================================================================
% CONSTRAINT STORY: omega_production_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omega_production_cost_asymmetry, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: omega_production_cost_asymmetry
 *   human_readable: Asymmetry Between Falling Falsifier-Production Cost and Fixed Precommitment-Abiding Cost
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This constraint names a structural mountain: the price of computing an
 *   alternative hypothesis, a falsifier, or a taxonomy label via LLM
 *   inference has collapsed by roughly two orders of magnitude in three
 *   years, while the price of actually abiding by a stated kill condition —
 *   changing an institution's mind, retracting a public claim, absorbing a
 *   disconfirming result, paying the social cost of being wrong in public —
 *   has not moved and is not the kind of thing any tooling improvement
 *   touches. The asymmetry itself is a fixed fact about two independent cost
 *   curves (compute economics vs. human/institutional status economics) that
 *   do not share a mechanism; no one 'benefits' from the asymmetry existing
 *   as a brute fact the way one benefits from a toll booth. But the asymmetry
 *   becomes a site of extraction once institutions and individuals with slack
 *   use the cheap side (register size, falsifier volume, taxonomy
 *   sophistication) to perform the expensive side (genuine corrigibility)
 *   without paying for it. That secondary, extractive layer is downstream and
 *   would properly warrant its own tangled_rope or piton story rather than
 *   being folded into the mountain claim here — the claim is that the
 *   underlying cost-differential is a fixed, naturally emerging fact of two
 *   independent economies, not that the institutional practices built on top
 *   of it are innocent.
 *
 * KEY AGENTS:
 *   - llm_tooling_vendors: benefit from adoption of falsifier-generation tooling as a rigor marker, independent of downstream belief revision
 *   - credentialed_forecasters_with_slack: exploit the asymmetry to accrue reputational credit for volume without proportional exposure to being wrong
 *   - institutions_performing_rigor_theater: set the terms under which register size substitutes for actual corrigibility
 *   - practitioners_without_slack: bear the abiding-cost side without the institutional cover to generate or curate a register that would be taken seriously
 *   - belief_revision_targets: absorb the full, unmoved social and career cost when a kill condition is actually triggered
 *   - epistemic_community_observers: track the absent metric — no published curve for retraction/policy-reversal rates exists to compare against the well-documented API pricing curve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omega_production_cost_asymmetry, 0.15).
domain_priors:suppression_score(omega_production_cost_asymmetry, 0.1).
domain_priors:theater_ratio(omega_production_cost_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, extractiveness, 0.15).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omega_production_cost_asymmetry, mountain).
narrative_ontology:human_readable(omega_production_cost_asymmetry, "Asymmetry Between Falling Falsifier-Production Cost and Fixed Precommitment-Abiding Cost").
narrative_ontology:topic_domain(omega_production_cost_asymmetry, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:emerges_naturally(omega_production_cost_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(omega_production_cost_asymmetry, 'e9dcd17e-16e1-41f0-9416-ea312d90e76c').
narrative_ontology:cs_kernel_codification('e9dcd17e-16e1-41f0-9416-ea312d90e76c', distributed).
narrative_ontology:cs_authority_grounding('e9dcd17e-16e1-41f0-9416-ea312d90e76c', distributed).
narrative_ontology:cs_reading_relation('e9dcd17e-16e1-41f0-9416-ea312d90e76c', omega_production_cost_asymmetry__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9dcd17e-16e1-41f0-9416-ea312d90e76c', omega_production_cost_asymmetry__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9dcd17e-16e1-41f0-9416-ea312d90e76c', omega_production_cost_asymmetry__proceduralist_reading, forecloses).
narrative_ontology:cs_axiom('e9dcd17e-16e1-41f0-9416-ea312d90e76c', foundational, generation_cost_and_abiding_cost_are_distinct_locations).
narrative_ontology:cs_axiom_status(generation_cost_and_abiding_cost_are_distinct_locations, holdable).
narrative_ontology:cs_axiom_grounding('e9dcd17e-16e1-41f0-9416-ea312d90e76c', generation_cost_and_abiding_cost_are_distinct_locations, empirically_contingent).
narrative_ontology:cs_axiom('e9dcd17e-16e1-41f0-9416-ea312d90e76c', secondary, cheap_production_can_still_yield_legitimate_omegas).
narrative_ontology:cs_axiom_status(cheap_production_can_still_yield_legitimate_omegas, holdable).
narrative_ontology:cs_axiom_grounding('e9dcd17e-16e1-41f0-9416-ea312d90e76c', cheap_production_can_still_yield_legitimate_omegas, instrumental).
narrative_ontology:cs_reference_frame('e9dcd17e-16e1-41f0-9416-ea312d90e76c', positional_testimony_as_symmetric_input).
narrative_ontology:cs_drift_state('e9dcd17e-16e1-41f0-9416-ea312d90e76c', post_llm_inference_cost_collapse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9dcd17e-16e1-41f0-9416-ea312d90e76c', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, llm_tooling_vendors).
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, credentialed_forecasters_with_slack).
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, institutions_performing_rigor_theater).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, practitioners_without_slack).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, belief_revision_targets).
narrative_ontology:constraint_vindicates(omega_production_cost_asymmetry, cost_of_generation_is_not_cost_of_belief_revision).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell inference at collapsing marginal cost and market it as a discipline-enhancing tool for generating alternative hypotheses, falsifiers, and adversarial samples. Benefit whenever institutions adopt the tooling as a visible marker of rigor, regardless of whether any actual belief revision follows from the outputs produced.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, llm_tooling_vendors, beneficiary,
    institutional, biographical, arbitrage, global).

% Have the time, institutional cover, and survivable-error budget to run large omega-generation loops, publish long registers of alternative positions and kill conditions, and accrue reputational credit for apparent rigor. Rarely face a career-costing moment where a generated kill condition actually fires against their own standing claim, because the cost of producing the register fell but the cost of visibly eating the result did not.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, credentialed_forecasters_with_slack, beneficiary,
    moderate, biographical, mobile, national).

% Adopt LLM-assisted omega-generation and taxonomy labeling as institutional practice, presenting large falsifier registers as evidence of epistemic seriousness. Set the terms under which 'we generated the alternative case' substitutes for 'we changed course when the alternative case held up.' The institution absorbs no social cost from the mismatch because it controls the reporting surface.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, institutions_performing_rigor_theater, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(omega_production_cost_asymmetry, institutions_performing_rigor_theater, agenda_setter).

% Cannot afford the model-assisted loop's downstream cost even though the tokens themselves are cheap: lack institutional cover to publish a kill condition against a superior's claim, lack a survivable-error budget if a generated falsifier turns out to indict their own prior work, and lack the standing to have their own generated register taken as evidence rather than noise. The cheap production layer does not touch their exposure at the abiding layer.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, practitioners_without_slack, payer,
    powerless, biographical, constrained, national).

% Are the specific people or institutions a fired kill condition would actually cost — the ones who staked a public position, a policy, or a research program on a claim now disconfirmed by a cheaply-generated falsifier. Bear the full unmoved cost of retraction, reputational damage, and policy reversal regardless of how trivial the falsifier was to produce.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, belief_revision_targets, payer,
    moderate, biographical, trapped, national).

% Track whether falling generation cost is actually correlated with rising rates of documented retraction, policy reversal, or public mind-change. Note the absence of any published metric analogous to the token-cost curve for belief-revision rates, and treat that absence itself as diagnostic.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, epistemic_community_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None at the level of the structural delta itself — it is a fixed asymmetry between two cost curves, not a coordination mechanism. Downstream institutional practices (adopting LLM tooling as a rigor signal) do coordinate expectations about what counts as diligence, but that is a separate, non-mountain layer riding on top of the asymmetry.
% TRANSFER_FUNCTION: The asymmetry itself transfers nothing directly; it creates an arbitrage opportunity that downstream institutional practices exploit — apparent rigor (register size, falsifier count, taxonomy sophistication) is transferred to actors with slack, while the unrewarded cost of actually abiding a kill condition remains parked on whoever would have to eat a real retraction.
% ABSENT_VOICES: Practitioners without institutional slack rarely appear in discussions of 'omega production' because the discourse is conducted mostly by the people who benefit from generating registers; their absence means the mismatch between production ease and abiding cost is rarely named from underneath.
% DISAPPEARANCE_RATIONALE: The underlying cost differential (falling compute cost vs. fixed social/psychological cost of belief revision) would not disappear even if the institutional practices built on top of it did — it reflects a real physical/economic trend (compute pricing) against a real human/institutional trend (status and career cost of public error) that are not coupled by any mechanism. What WOULD disappear if named and acted on is the theatrical conflation of the two: institutions could stop treating falsifier-register size as evidence of actual corrigibility. Parties dispute whether that conflation is central to current practice or incidental.
% FOUNDING_PROBLEM: The asymmetry was never 'built' to solve a problem — it emerged as a byproduct of two independent trends (LLM inference pricing collapse; unchanged human/institutional cost of public retraction) that happen to interact where people use the first to perform the second.
% FOUNDING_PROBLEM_CORROBORATION: Independent of any benefiting party: empirical LLM API pricing data (documented ~99% decline 2023-2026, publicly reported by API providers and independent trackers) corroborates the falling-cost side; the absence of any comparable published metric for retraction or policy-reversal rates is itself attested by the epistemic_community_observers seat, which has no stake in either cost curve moving.
narrative_ontology:disappearance_verdict(omega_production_cost_asymmetry, contested).
narrative_ontology:founding_problem_status(omega_production_cost_asymmetry, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(omega_production_cost_asymmetry, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(omega_production_cost_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(omega_production_cost_asymmetry, 0.15, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omega_production_cost_asymmetry_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(omega_production_cost_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(omega_production_cost_asymmetry),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(omega_production_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.15) at the level of the bare cost-asymmetry itself, because the differential between two independent cost curves is not, by itself, an extraction mechanism — it is a fact about physics/economics of computation on one side and human institutional psychology on the other, and neither side collects from the other directly. Suppression is low (0.1) for the same reason: nothing coercive holds the asymmetry in place; it would exist even with zero enforcement. Theater ratio is authored high and rising (0.2 to 0.58) because the DOWNSTREAM institutional practice of treating falsifier-register volume as evidence of rigor is substantially performative, and that performative layer has visibly grown as the tooling got cheaper and adoption spread — this is the T17-relevant trajectory: watch for downstream stories where accumulating extraction on top of this mountain crosses into false-summit or tangled-rope territory. Accessibility collapse is moderate (0.35): the fact that cheap falsifier-generation does not substitute for costly belief-revision is not fully collapsed as an insight — motivated institutions can still obscure it, and many observers have not yet drawn the distinction, but it is not a genuinely occluded natural-law-grade collapse either. Resistance is moderate-low (0.3): there is some pushback (this very essay's framing is an act of resistance), but it is not yet a widely contested claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as those positioned to exploit the gap between the two curves: vendors selling the cheap side, forecasters with slack to generate volume without matching exposure, and institutions that control how the gap is reported. No victims are declared at the mountain level because the bare cost-asymmetry does not, by itself, transfer anything from a specific named victim group to a specific named beneficiary group — it is a background condition that downstream extractive practices exploit. This is why the story is authored as mountain-with-beneficiaries (an FSM candidate) rather than tangled_rope: the beneficiary declaration flags that a real institution treating this natural fact as license for a specific extractive practice (theater-as-rigor) deserves engine scrutiny, without asserting that the fact itself is constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem framing deliberately declines to assign the asymmetry a founding purpose — it was not built to solve anything, which is itself evidence for the mountain reading. What would count as mandatrophy is not the asymmetry (which cannot be resolved because it reflects independent economies) but the institutional practice of treating cheap generation as a substitute for costly abiding; that practice's founding problem (making rigor legible and auditable) may indeed be alive while its actual function (visible-diligence signaling without corrigibility) has drifted. Classifying the asymmetry itself as mountain, and reserving tangled_rope/piton classification for the specific institutional practices riding on it, prevents mislabeling a fixed economic fact as pure extraction while still allowing the extractive downstream practice to be caught by its own story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_constructed_incentive_structure,
    'Is the cost asymmetry between falsifier-production and belief-revision-abiding a brute fact about independent economies (compute pricing vs. human status psychology), or is part of the ''unmoved'' cost of abiding itself a constructed institutional artifact (e.g., career penalty structures that could in principle be redesigned) rather than an irreducible feature of minds and institutions?',
    'Compare belief-revision cost across institutional designs that vary career-penalty structure for public retraction (e.g., forecasting tournaments with anonymized scoring vs. named public commitments) — if the cost of abiding varies substantially with institutional design, the ''fixed'' side is partly constructed, not purely natural.',
    'If the abiding-cost side is substantially constructed, the overall story shifts from mountain toward a tangled_rope or snare framing in which institutions have an interest in keeping retraction costly precisely because it makes cheap falsifier-generation a performable, low-risk substitute for real corrigibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_constructed_incentive_structure, conceptual, 'Whether the unmoved abiding-cost side is a natural fact or a maintained institutional artifact').

omega_variable(
    absent_metric_measurement_gap,
    'Does the absence of a published, tracked metric for belief-revision/retraction rates (parallel to the well-documented LLM pricing curve) reflect genuine unmeasurability, or a preference by beneficiary institutions not to measure something that would make the asymmetry visible?',
    'Attempt to construct a retraction/policy-reversal rate metric from available public records (retracted papers, reversed policies with stated kill conditions, public forecaster track records) and see whether the difficulty is technical or institutional.',
    'If the metric is constructible but simply not being tracked, that supports reading the measurement gap itself as a form of theater (0.58 theater_ratio) rather than pure natural absence; if genuinely unmeasurable, the mountain framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absent_metric_measurement_gap, empirical, 'Whether the missing belief-revision metric is a measurement problem or a maintained blind spot').

omega_variable(
    framing_choice_kernel_vs_instrumentalist,
    'Should this story be authored as a bare mountain (the cost-differential) independent of any kernel-reading structure, or does the instrumentalist_reading framing (from positional_disagreement_as_evidence) actually change what counts as ε here, since a proceduralist reading would deny that cheaply-produced falsifiers carry evidentiary weight at all?',
    'Compare classification outcomes if this story were re-authored strictly as a flat (non-kernel) mountain versus explicitly as the instrumentalist_reading — check whether beneficiary declarations or extractiveness would differ.',
    'If the two framings produce materially different beneficiary sets or extraction profiles, that confirms the decomposition is doing real work and the kernel_context should remain; if they converge, the kernel framing may be decorative here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_instrumentalist, conceptual, 'Whether the instrumentalist kernel-reading framing materially changes this story''s structural content versus a flat mountain framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omega_production_cost_asymmetry, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omeg_tr_t0, omega_production_cost_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(omeg_tr_t0, observed).
narrative_ontology:measurement(omeg_tr_t6, omega_production_cost_asymmetry, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(omeg_tr_t6, observed).
narrative_ontology:measurement(omeg_tr_t12, omega_production_cost_asymmetry, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(omeg_tr_t12, observed).
narrative_ontology:measurement(omeg_tr_t18, omega_production_cost_asymmetry, theater_ratio, 18, 0.48).
narrative_ontology:measurement_basis(omeg_tr_t18, observed).
narrative_ontology:measurement(omeg_tr_t24, omega_production_cost_asymmetry, theater_ratio, 24, 0.53).
narrative_ontology:measurement_basis(omeg_tr_t24, observed).
narrative_ontology:measurement(omeg_tr_t30, omega_production_cost_asymmetry, theater_ratio, 30, 0.56).
narrative_ontology:measurement_basis(omeg_tr_t30, projected).
narrative_ontology:measurement(omeg_tr_t36, omega_production_cost_asymmetry, theater_ratio, 36, 0.58).
narrative_ontology:measurement_basis(omeg_tr_t36, projected).

% Extraction over time
narrative_ontology:measurement(omeg_be_t0, omega_production_cost_asymmetry, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(omeg_be_t0, observed).
narrative_ontology:measurement(omeg_be_t6, omega_production_cost_asymmetry, base_extractiveness, 6, 0.07).
narrative_ontology:measurement_basis(omeg_be_t6, observed).
narrative_ontology:measurement(omeg_be_t12, omega_production_cost_asymmetry, base_extractiveness, 12, 0.09).
narrative_ontology:measurement_basis(omeg_be_t12, observed).
narrative_ontology:measurement(omeg_be_t18, omega_production_cost_asymmetry, base_extractiveness, 18, 0.11).
narrative_ontology:measurement_basis(omeg_be_t18, observed).
narrative_ontology:measurement(omeg_be_t24, omega_production_cost_asymmetry, base_extractiveness, 24, 0.13).
narrative_ontology:measurement_basis(omeg_be_t24, observed).
narrative_ontology:measurement(omeg_be_t30, omega_production_cost_asymmetry, base_extractiveness, 30, 0.14).
narrative_ontology:measurement_basis(omeg_be_t30, projected).
narrative_ontology:measurement(omeg_be_t36, omega_production_cost_asymmetry, base_extractiveness, 36, 0.15).
narrative_ontology:measurement_basis(omeg_be_t36, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(omega_production_cost_asymmetry, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omega_production_cost_asymmetry, information_standard).
narrative_ontology:boltzmann_floor_override(omega_production_cost_asymmetry, 0.02).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, llm_assisted_forecasting_rigor_theater).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, adversarial_collaboration_precommitment_practice).

% DUAL FORMULATION NOTE:
% omega_production_cost_asymmetry is the upstream mountain-level fact (a fixed differential between two independent cost curves). It is distinguished from a sibling downstream constraint (llm_assisted_forecasting_rigor_theater, not yet authored in this batch) which would describe the specific institutional practice of using cheap falsifier-generation as a substitute for costly belief revision — that downstream constraint would likely classify as tangled_rope or piton, with concentrated beneficiaries and diffuse victims, rather than mountain. Keeping them separate preserves ε-invariance: the bare cost differential has low, stable extraction; the institutional exploitation of it does not.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
