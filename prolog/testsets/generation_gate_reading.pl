% ============================================================================
% CONSTRAINT STORY: generation_gate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generation_gate_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: generation_gate_reading
 *   human_readable: Generation Gate Reading of General Capability Standard
 *   domain: epistemology/forecasting/ai_capability
 *
 * SUMMARY:
 *   The generation gate reading holds that general capability requires a
 *   qualitative faculty — the ability to author novel evaluation standards,
 *   not merely perform well on existing ones. This reading treats the
 *   threshold as inherently unforecastable because no instances exist to
 *   measure until the faculty appears. The constraint coordinates epistemic
 *   humility about timelines while extracting from researchers whose
 *   quantitative progress is systematically discounted. The claim/metric
 *   independence is deliberate: the constraint is claimed as tangled_rope
 *   (genuine coordination function with asymmetric extraction) while metrics
 *   describe substantial and rising extraction as the standard's
 *   unfalsifiability becomes more apparent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generation_gate_reading, 0.68).
domain_priors:suppression_score(generation_gate_reading, 0.72).
domain_priors:theater_ratio(generation_gate_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generation_gate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(generation_gate_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(generation_gate_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(generation_gate_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(generation_gate_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generation_gate_reading, tangled_rope).
narrative_ontology:human_readable(generation_gate_reading, "Generation Gate Reading of General Capability Standard").
narrative_ontology:topic_domain(generation_gate_reading, "epistemology/forecasting/ai_capability").

domain_priors:requires_active_enforcement(generation_gate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(generation_gate_reading, '7464dfc9-5e7f-4e5f-9823-7c5888b71bf3').
narrative_ontology:cs_kernel_codification('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', distributed).
narrative_ontology:cs_authority_grounding('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', expertise).
narrative_ontology:cs_interpretation_layer_present('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3').
narrative_ontology:cs_reading_relation('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', generality_standard__trajectory_extrapolation_reading, forecloses).
narrative_ontology:cs_reading_relation('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', generality_standard__partition_choice_reading, coexists_with).
narrative_ontology:cs_axiom('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', foundational, qualitative_threshold_primacy).
narrative_ontology:cs_axiom_status(qualitative_threshold_primacy, holdable).
narrative_ontology:cs_axiom_grounding('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', qualitative_threshold_primacy, deontological).
narrative_ontology:cs_axiom('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', foundational, verification_generation_discontinuity).
narrative_ontology:cs_axiom_status(verification_generation_discontinuity, holdable).
narrative_ontology:cs_axiom_grounding('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', verification_generation_discontinuity, empirically_contingent).
narrative_ontology:cs_reference_frame('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', qualitative_threshold_epistemology).
narrative_ontology:cs_drift_state('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', contemporary_forecasting_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7464dfc9-5e7f-4e5f-9823-7c5888b71bf3', '').
narrative_ontology:cs_kernel_id(generation_gate_reading, generality_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generation_gate_reading, ai_safety_institutions).
narrative_ontology:constraint_beneficiary(generation_gate_reading, forecasting_methodologists).
narrative_ontology:constraint_victim(generation_gate_reading, capability_researchers).
narrative_ontology:constraint_victim(generation_gate_reading, timeline_forecasters).
narrative_ontology:constraint_vindicates(generation_gate_reading, qualitative_threshold_doctrine).
narrative_ontology:constraint_vindicates(generation_gate_reading, faculty_emergence_unpredictability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the standard for what counts as general capability by requiring demonstration of generation faculty — the ability to author novel standards, not merely verify against existing ones. They frame this as a principled epistemological boundary that prevents premature declarations of generality. The standard justifies resource allocation toward safety research and regulatory preparation while the generation threshold remains uncrossed.
narrative_ontology:constraint_stakeholder(generation_gate_reading, ai_safety_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from the reading's implication that timeline forecasting is fundamentally constrained until generation faculty appears. The generation gate validates their epistemic humility and justifies wide uncertainty bounds in capability projections. They can point to the qualitative threshold as the reason quantitative extrapolation fails.
narrative_ontology:constraint_stakeholder(generation_gate_reading, forecasting_methodologists, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of the standard's enforcement: their demonstrations of rapid verification progress, scaling laws, and benchmark saturation are systematically discounted as not crossing the generation threshold. They argue the gate is a moving target that will be redefined once generation faculty appears, and that the qualitative framing prevents falsification.
narrative_ontology:constraint_stakeholder(generation_gate_reading, capability_researchers, payer,
    powerful, biographical, constrained, global).

% Are told their quantitative methods cannot adjudicate the timeline because the generation threshold is qualitative and no instances exist to measure. Their trajectory extrapolations and convergence tests are dismissed as category errors. Exit means abandoning the forecasting question entirely, not adopting an alternative standard.
narrative_ontology:constraint_stakeholder(generation_gate_reading, timeline_forecasters, payer,
    moderate, biographical, constrained, global).

% Propose operationalizable definitions of generality based on task transfer, sample efficiency, or compositional reasoning. They are excluded from the standard-setting conversation because the generation gate reading treats these as verification-flavored proxies that miss the qualitative leap. Their standards would make the timeline forecastable.
narrative_ontology:constraint_stakeholder(generation_gate_reading, alternative_standard_proponents, excluded,
    moderate, biographical, constrained, global).

% Examine whether the generation gate is an epistemological principle or a strategic framing. They note that every proposed operationalization of generation faculty is rejected as insufficient, and ask whether the standard is designed to be unfalsifiable until the outcome is already determined.
narrative_ontology:constraint_stakeholder(generation_gate_reading, philosophy_of_measurement_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared epistemic standard for what counts as general capability, preventing premature declarations that could misallocate resources or create false confidence in safety measures designed for narrow systems.
% TRANSFER_FUNCTION: Moves epistemic authority over capability assessment from quantitative forecasters and benchmark researchers to institutions that control the definition of generation faculty. Transfers timeline uncertainty from a measurable quantity to an inherently unforecastable event.
% ABSENT_VOICES: Researchers proposing falsifiable operationalizations of generality are structurally excluded because their standards would make the generation threshold measurable and thus forecastable. The conversation is held among those who benefit from unfalsifiability.
% DISAPPEARANCE_RATIONALE: If the generation gate standard vanished, capability assessment would reorganize around operationalizable metrics. Timeline forecasting would shift from 'fundamentally unforecastable' to 'uncertain but bounded by measurable progress.' Safety institutions would lose the epistemic ground for treating AGI timelines as inherently unknowable.
% FOUNDING_PROBLEM: Early AI capability assessments conflated narrow task performance with general intelligence, leading to both premature declarations of human-level capability and failures to recognize genuine qualitative leaps when they occurred.
% FOUNDING_PROBLEM_CORROBORATION: Safety institutions attest the problem is live and the generation gate prevents recurrence. Capability researchers and forecasting methodologists outside the safety community attest the standard has overcorrected: it now prevents any falsifiable claim about generality, which is a different failure mode than premature declaration. Philosophy of measurement analysts document that the standard's unfalsifiability is a feature, not a bug.
narrative_ontology:disappearance_verdict(generation_gate_reading, world_rearranges).
narrative_ontology:founding_problem_status(generation_gate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(generation_gate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-29',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(generation_gate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generation_gate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(generation_gate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(generation_gate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68 at interval end) because the standard systematically discounts measurable progress as not crossing the qualitative threshold, transferring epistemic authority from those who measure to those who define the unmeasurable. Suppression is similarly high (0.72) because alternative operationalizable standards are excluded as category errors. Theater ratio is moderate (0.41): the epistemic principle is genuine, but a growing share of enforcement activity defends the standard's unfalsifiability rather than its discriminative power. The measurement series shows extraction and suppression rising as the standard's strategic value becomes clearer.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the generation gate is a principled epistemological boundary preventing premature capability claims. From the payer seats, the same structure operates as an unfalsifiable standard that moves the goalposts and prevents any quantitative forecast from being taken seriously. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety institutions are structural beneficiaries (control the standard, benefit from timeline unfalsifiability — d near beneficiary end). Forecasting methodologists benefit secondarily (the reading validates their uncertainty). Capability researchers and timeline forecasters are targets (their work is systematically discounted — d near target end). Alternative standard proponents are excluded rather than coordinated.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generation_operationalization,
    'Can generation faculty be operationalized in a way that allows measurement before the threshold is crossed, or is the concept inherently post-hoc?',
    'Attempt to specify falsifiable criteria for generation faculty that could be tested on current systems. If every proposed operationalization is rejected as insufficient, the concept is post-hoc by construction.',
    'If operationalizable, the generation gate becomes a measurable threshold and timeline forecasting becomes possible. If inherently post-hoc, the standard functions as an unfalsifiable claim that prevents any quantitative forecast from being taken seriously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generation_operationalization, conceptual, 'Whether generation faculty is a measurable property or an unfalsifiable claim.').

omega_variable(
    goalpost_movement,
    'Will the generation gate be redefined once systems demonstrate what is currently described as generation faculty?',
    'Historical pattern analysis: track whether the standard''s definition shifts when systems approach the current threshold. If the threshold consistently moves, the standard is a moving target rather than a fixed principle.',
    'If the threshold moves, the generation gate is revealed as a strategic framing rather than an epistemological principle. If it holds, the reading''s claim to principled boundary-drawing is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(goalpost_movement, empirical, 'Whether the standard is a fixed principle or a moving target.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the generation gate reading logically foreclose the trajectory extrapolation reading, or do they coexist as competing frameworks?',
    'Examine whether a single agent could coherently hold both readings simultaneously. If the generation gate''s qualitative threshold claim contradicts the extrapolation reading''s quantitative continuity claim, one forecloses the other.',
    'If they foreclose each other, the kernel is a genuine either-or choice. If they coexist, different communities can hold different readings without logical contradiction, and the dispute is about which framework is more useful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading and trajectory_extrapolation_reading can coexist in one framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generation_gate_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, generation_gate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gene_tr_t5, generation_gate_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(gene_tr_t10, generation_gate_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(gene_tr_t15, generation_gate_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(gene_tr_t20, generation_gate_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(gene_tr_t25, generation_gate_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, generation_gate_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gene_be_t5, generation_gate_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(gene_be_t10, generation_gate_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(gene_be_t15, generation_gate_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(gene_be_t20, generation_gate_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(gene_be_t25, generation_gate_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, generation_gate_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gene_su_t5, generation_gate_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(gene_su_t10, generation_gate_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(gene_su_t15, generation_gate_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(gene_su_t20, generation_gate_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(gene_su_t25, generation_gate_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generation_gate_reading, information_standard).
narrative_ontology:affects_constraint(generation_gate_reading, trajectory_extrapolation_reading).
narrative_ontology:affects_constraint(generation_gate_reading, partition_choice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the generality_standard kernel. The kernel decomposes into three structurally distinct readings with different ε values: generation_gate_reading (this file, substantially extractive due to unfalsifiability), trajectory_extrapolation_reading (lower extraction, treats threshold as quantitatively forecastable), partition_choice_reading (lowest extraction, treats boundary as pragmatic choice rather than natural kind). The readings are linked via network.affects_constraints because they compete for epistemic authority over the same capability assessment question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
