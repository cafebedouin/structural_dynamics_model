% ============================================================================
% CONSTRAINT STORY: partition_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_partition_choice_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: partition_choice_reading
 *   human_readable: General Capability as Seated Partition Choice
 *   domain: philosophy_of_measurement/ai_capability_assessment/epistemology_of_forecasting
 *
 * SUMMARY:
 *   The partition-choice reading holds that 'general capability' in AI
 *   systems is not a natural threshold waiting to be discovered, but a
 *   weighted aggregation of competences whose weighting is chosen by
 *   measurement institutions. Under this reading, timeline disagreements are
 *   not empirical disputes resolvable by more data—they are
 *   position-correlated disagreements about which partition to adopt. The
 *   constraint coordinates AI capability assessment around shared benchmarks
 *   while extracting planning costs from timeline-dependent actors who must
 *   treat estimates as bets on frame adoption. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as tangled_rope (genuine
 *   coordination with asymmetric extraction) while the metrics describe the
 *   actual operation—the engine measures whether the coordination and
 *   extraction components are both present and whether the extraction is
 *   substantial enough to warrant the classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(partition_choice_reading, 0.68).
domain_priors:suppression_score(partition_choice_reading, 0.71).
domain_priors:theater_ratio(partition_choice_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(partition_choice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(partition_choice_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(partition_choice_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(partition_choice_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(partition_choice_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(partition_choice_reading, tangled_rope).
narrative_ontology:human_readable(partition_choice_reading, "General Capability as Seated Partition Choice").
narrative_ontology:topic_domain(partition_choice_reading, "philosophy_of_measurement/ai_capability_assessment/epistemology_of_forecasting").

domain_priors:requires_active_enforcement(partition_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(partition_choice_reading, '66bdf36b-062e-44ec-adae-6b4693461f33').
narrative_ontology:cs_kernel_codification('66bdf36b-062e-44ec-adae-6b4693461f33', distributed).
narrative_ontology:cs_authority_grounding('66bdf36b-062e-44ec-adae-6b4693461f33', expertise).
narrative_ontology:cs_interpretation_layer_present('66bdf36b-062e-44ec-adae-6b4693461f33').
narrative_ontology:cs_reading_relation('66bdf36b-062e-44ec-adae-6b4693461f33', generality_standard__trajectory_extrapolation_reading, influences).
narrative_ontology:cs_reading_relation('66bdf36b-062e-44ec-adae-6b4693461f33', generality_standard__generation_gate_reading, influences).
narrative_ontology:cs_axiom('66bdf36b-062e-44ec-adae-6b4693461f33', foundational, capability_aggregation_is_frame_choice).
narrative_ontology:cs_axiom_status(capability_aggregation_is_frame_choice, holdable).
narrative_ontology:cs_axiom_grounding('66bdf36b-062e-44ec-adae-6b4693461f33', capability_aggregation_is_frame_choice, conventional).
narrative_ontology:cs_axiom('66bdf36b-062e-44ec-adae-6b4693461f33', secondary, timeline_disagreement_is_position_correlated).
narrative_ontology:cs_axiom_status(timeline_disagreement_is_position_correlated, holdable).
narrative_ontology:cs_axiom_grounding('66bdf36b-062e-44ec-adae-6b4693461f33', timeline_disagreement_is_position_correlated, empirically_contingent).
narrative_ontology:cs_reference_frame('66bdf36b-062e-44ec-adae-6b4693461f33', aggregation_pragmatism).
narrative_ontology:cs_drift_state('66bdf36b-062e-44ec-adae-6b4693461f33', contemporary_forecasting_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66bdf36b-062e-44ec-adae-6b4693461f33', '').
narrative_ontology:cs_kernel_id(partition_choice_reading, generality_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(partition_choice_reading, forecasting_institutions).
narrative_ontology:constraint_beneficiary(partition_choice_reading, capability_assessment_frameworks).
narrative_ontology:constraint_victim(partition_choice_reading, timeline_dependent_actors).
narrative_ontology:constraint_victim(partition_choice_reading, resource_allocation_decision_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(partition_choice_reading, ai_capability_researchers).
narrative_ontology:constraint_victim(partition_choice_reading, ai_capability_researchers).
narrative_ontology:constraint_vindicates(partition_choice_reading, measurement_frame_dependence).
narrative_ontology:constraint_vindicates(partition_choice_reading, observer_position_correlation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the weighting schemes and competence partitions used to operationalize 'general capability' in AI systems. They choose which dimensions count, how much each counts, and what threshold constitutes achievement. Their frameworks structure funding decisions, research priorities, and policy timelines. They benefit from the flexibility to reweight partitions as systems advance, maintaining relevance regardless of which capabilities emerge first.
narrative_ontology:constraint_stakeholder(partition_choice_reading, forecasting_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Provide the technical infrastructure for measuring AI progress: benchmark suites, evaluation protocols, aggregation methods. They collect citation authority and institutional standing from being the recognized measurement apparatus. The partition-choice reading legitimates continuous framework revision as systems improve, preventing obsolescence.
narrative_ontology:constraint_stakeholder(partition_choice_reading, capability_assessment_frameworks, beneficiary,
    institutional, biographical, constrained, global).

% Make resource allocation, safety investment, and strategic decisions based on capability timelines. When 'general capability' is a partition choice rather than a discoverable threshold, their timeline estimates become bets on which weighting will be adopted, not predictions of a natural boundary. They bear the cost of planning under irreducible measurement ambiguity while the measurement apparatus itself remains stable.
narrative_ontology:constraint_stakeholder(partition_choice_reading, timeline_dependent_actors, payer,
    powerful, biographical, constrained, global).

% Allocate funding for AI safety research, compute governance, and capability development based on proximity to 'general capability' milestones. Under the partition-choice reading, they cannot resolve disagreements about timelines by gathering more data—the disagreement is about which partition to adopt, not about what the data shows. They pay the coordination cost of acting under persistent frame disagreement.
narrative_ontology:constraint_stakeholder(partition_choice_reading, resource_allocation_decision_makers, payer,
    institutional, biographical, constrained, national).

% Develop systems and publish capability results measured against the prevailing frameworks. They benefit from clear benchmarks that structure research agendas, but pay the cost when partition shifts retroactively reframe their achievements—a system that was 'generally capable' under one weighting becomes 'narrow' under another. Their work is coordinated by shared measurement but extracted from when the measurement frame is revised.
narrative_ontology:constraint_stakeholder(partition_choice_reading, ai_capability_researchers, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(partition_choice_reading, ai_capability_researchers, beneficiary).

% Study how forecasting institutions handle irreducible measurement ambiguity and whether position-correlated disagreement patterns indicate frame choice rather than empirical dispute. They see the full structure: that more measurement sharpens rather than dissolves the camps, and that no party benefits from resolving the ambiguity because the flexibility itself is valuable.
narrative_ontology:constraint_stakeholder(partition_choice_reading, epistemology_of_forecasting_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared operationalization of 'general capability' so that AI progress can be measured, compared across systems, and used to structure research priorities and policy timelines. Without some partition choice, the concept remains too abstract to guide resource allocation.
% TRANSFER_FUNCTION: Moves decision-making authority and citation standing to the institutions that set the partition weightings, while imposing planning costs on actors who must treat timeline estimates as bets on frame adoption rather than empirical predictions.
% ABSENT_VOICES: Alternative measurement philosophies that would treat 'general capability' as a natural kind with discoverable boundaries, or frameworks that would make partition choices explicit and revisable through transparent governance, are structurally excluded from the dominant assessment apparatus.
% DISAPPEARANCE_RATIONALE: If the partition-choice reading vanished and 'general capability' were treated as either a natural threshold or an explicitly governed frame choice, forecasting institutions would lose the flexibility to reweight partitions post-hoc, timeline-dependent actors would face different planning constraints, and the structure of disagreement would shift from position-correlated camps to empirical disputes resolvable by more data.
% FOUNDING_PROBLEM: Early AI capability assessment had no shared vocabulary for comparing systems across different competence dimensions—researchers needed some way to aggregate narrow capabilities into a general progress metric to guide funding and safety prioritization.
% FOUNDING_PROBLEM_CORROBORATION: Forecasting institutions attest the founding problem remains live, citing ongoing need for aggregation frameworks. Epistemology analysts and some timeline-dependent actors attest the problem has shifted: the original need was for any shared metric; the current arrangement maintains ambiguity about whether the metric tracks a natural boundary or institutional choice, which serves the measurement apparatus more than the decision-makers. Independent analysis from philosophy of measurement supports the shifted-function reading.
narrative_ontology:disappearance_verdict(partition_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(partition_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(partition_choice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-29',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(partition_choice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(partition_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(partition_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(partition_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the partition-choice ambiguity imposes irreducible planning costs on timeline-dependent actors while preserving flexibility for the measurement apparatus. Suppression is high (0.71) because alternative measurement philosophies that would treat generality as a natural kind or make partition choices explicit are structurally excluded from dominant frameworks. Theater ratio is moderate (0.42): the benchmarking function is real and valuable, but a growing share of framework activity defends the partition-choice reading against calls for transparent governance of weighting decisions. Accessibility collapse is moderate-low (0.48): alternative framings remain conceptually available but are not adopted by resource-allocation institutions. Resistance is substantial (0.63): timeline-dependent actors and some researchers actively contest the reading, arguing that more measurement should resolve disagreement rather than sharpen camps.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute differently: from the forecasting institutions' position, the partition-choice reading is a pragmatic response to genuine measurement complexity; from the timeline-dependent actors' position, the same structure operates as enforced ambiguity that prevents coordination on timelines while preserving institutional flexibility. The engine computes this divergence from the structural data—the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Forecasting institutions and capability assessment frameworks are structural beneficiaries (set the weightings, collect citation authority, maintain relevance through frame flexibility—d near the beneficiary end). Timeline-dependent actors and resource allocation decision-makers are targets (bear planning costs under irreducible measurement ambiguity, cannot resolve disagreements with more data—d near the target end). AI capability researchers are mixed (coordinated by shared benchmarks but extracted from when partition shifts reframe their work—d near symmetric). Epistemology analysts are observers (see the full structure without being coordinated or extracted by it).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_naturality,
    'Is there a natural weighting of competences that tracks a real boundary in capability space, or are all weightings equally conventional?',
    'Empirical test: if different measurement institutions converge on similar weightings without coordination, the partition tracks something natural; if weightings remain institution-dependent despite shared data, the partition is conventional.',
    'If natural, the partition-choice reading collapses to trajectory extrapolation (the weighting discovers a real threshold). If conventional, the reading is vindicated but the extraction becomes more visible (institutions are choosing frames, not discovering boundaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_naturality, empirical, 'Whether capability partitions track natural boundaries or institutional choices.').

omega_variable(
    measurement_convergence_vs_camp_sharpening,
    'Does more capability measurement resolve timeline disagreements or sharpen position-correlated camps?',
    'Longitudinal analysis of forecasting disagreement patterns: if disagreement variance decreases with more data, measurement is resolving empirical disputes; if camps sharpen and correlate with institutional position, disagreement is about frame choice.',
    'If measurement resolves disagreement, the partition-choice reading is wrong (generality is discoverable). If camps sharpen, the reading is vindicated and the suppression of alternative measurement philosophies becomes more visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_convergence_vs_camp_sharpening, empirical, 'Whether capability measurement resolves or entrenches timeline disagreement.').

omega_variable(
    frame_governance_alternative,
    'Could the coordination function be preserved while making partition choices explicit and revisable through transparent governance?',
    'Natural experiment from institutions that adopt explicit frame-choice governance: if coordination holds while extraction decreases, the functions are separable.',
    'If separable, the current arrangement''s extraction is avoidable and the constraint should be classified as more extractive than coordinating. If inseparable, some extraction is the price of coordination under irreducible measurement ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(frame_governance_alternative, conceptual, 'Whether coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(partition_choice_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(part_tr_t0, partition_choice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(part_tr_t5, partition_choice_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(part_tr_t10, partition_choice_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(part_tr_t15, partition_choice_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(part_tr_t20, partition_choice_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(part_tr_t25, partition_choice_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(part_be_t0, partition_choice_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(part_be_t5, partition_choice_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(part_be_t10, partition_choice_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(part_be_t15, partition_choice_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(part_be_t20, partition_choice_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(part_be_t25, partition_choice_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(part_su_t0, partition_choice_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(part_su_t5, partition_choice_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(part_su_t10, partition_choice_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(part_su_t15, partition_choice_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(part_su_t20, partition_choice_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(part_su_t25, partition_choice_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(partition_choice_reading, information_standard).
narrative_ontology:affects_constraint(partition_choice_reading, trajectory_extrapolation_reading).
narrative_ontology:affects_constraint(partition_choice_reading, generation_gate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the generality_standard kernel. The kernel decomposes into three structurally distinct readings with different ε values: partition_choice_reading (this constraint, ε=0.68, substantial extraction from timeline-dependent actors under irreducible measurement ambiguity), trajectory_extrapolation_reading (ε lower, treats timelines as discoverable by curve extrapolation), generation_gate_reading (ε potentially higher, treats generality as controlled by specific capability unlocks). The readings form a constraint family linked by network.affects_constraints. The partition-choice reading influences both siblings: if partition choice is vindicated, trajectory extrapolation becomes a bet on which partition will be adopted rather than a discovery process, and generation-gate claims must specify which partition the gate is defined relative to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
