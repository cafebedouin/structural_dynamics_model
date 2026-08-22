% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: State Capacity as Intervention Justification
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the graded_sovereignty reading of the
 *   contested westphalia_sovereignty kernel. Under this reading, territorial
 *   authority is not a categorical equal-status property but a scalar
 *   capacity metric ranging from full sovereignty (consolidated democracies)
 *   to nominal sovereignty (failed or fragile states). International
 *   evaluation authorities assess state capacity through governance indices
 *   and development benchmarks; deficits legitimate graduated intervention,
 *   conditionality, and paternalistic oversight. The constraint creates a
 *   hierarchical state system with de jure sovereign equality but de facto
 *   tiering. The engine should detect significant seat divergence: capacity
 *   evaluators and great powers experience a coordination mechanism that
 *   secures international order, while weak states experience extraction of
 *   autonomous decision-making masked as assistance.
 *
 * KEY AGENTS:
 *   - capacity_evaluation_authorities (institutional/analytical): Establish governance metrics and intervention triggers; structural beneficiaries and agenda-setters
 *   - weak_states (powerless/trapped): Subject to capacity scoring and paternalistic oversight; bear autonomy loss proportional to deficits
 *   - great_powers (institutional/arbitrage): Enjoy full sovereignty and set implicit benchmarks; beneficiaries of hierarchical order
 *   - humanitarian_intervention_bodies (organized/constrained): Receive operational mandates in nominal-sovereignty zones
 *   - post_colonial_scholars (moderate/constrained): Critique metric neutrality; structurally excluded from threshold-setting bodies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.62).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.58).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: State Capacity as Intervention Justification").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '553f8b5f-d189-4241-9c43-a9c60ebe1cf1').
narrative_ontology:cs_kernel_codification('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', formalized).
narrative_ontology:cs_authority_grounding('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', lineage).
narrative_ontology:cs_interpretation_layer_present('553f8b5f-d189-4241-9c43-a9c60ebe1cf1').
narrative_ontology:cs_reading_relation('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', foundational, sovereignty_is_scalar_capacity).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_capacity, holdable).
narrative_ontology:cs_axiom_grounding('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', sovereignty_is_scalar_capacity, conventional).
narrative_ontology:cs_axiom('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', foundational, capacity_deficit_legitimizes_paternalistic_oversight).
narrative_ontology:cs_axiom_status(capacity_deficit_legitimizes_paternalistic_oversight, holdable).
narrative_ontology:cs_axiom_grounding('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', capacity_deficit_legitimizes_paternalistic_oversight, instrumental).
narrative_ontology:cs_reference_frame('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', capacity_calibrated_statehood).
narrative_ontology:cs_drift_state('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', multipolar_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('553f8b5f-d189-4241-9c43-a9c60ebe1cf1', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, great_powers).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_bodies).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and calibrate state-capacity metricsâgovernance indices, fragile-states assessments, development benchmarksâthat determine when territorial sovereignty is nominal rather than full. Legitimize intervention, conditionality, and paternalistic oversight based on these scores. Collect institutional authority, funding, and mandates from the evaluation role itself.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, beneficiary).

% Classified as having capacity deficits through externally imposed metrics. Subject to graduated intervention, oversight missions, structural-adjustment governance reforms, and loss of autonomous decision-making proportional to their scored deficit. Cannot exit the evaluation framework without collapsing international recognition or essential aid flows.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% Enjoy unqualified territorial sovereignty and full international legal standing. Their state capacity is treated as the implicit benchmark against which weaker states are measured. Benefit from a hierarchical system that legitimizes their oversight role and precludes reciprocal evaluation of their own domestic conduct.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, great_powers, beneficiary,
    institutional, civilizational, arbitrage, global).

% Receive mandates and resources to operate in nominal-sovereignty zones where local consent is bypassed or substituted due to capacity scoring. Their operational legitimacy depends on the graded-sovereignty framework that treats weak-state consent as replaceable by international approval.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_bodies, beneficiary,
    organized, biographical, constrained, global).

% Critique capacity metrics as embedded with colonial assumptions about legitimate statehood. Argue that sovereignty tiering reproduces a hierarchical international racialized order. Present in academic and diplomatic discourse but structurally excluded from the formal bodies that codify capacity thresholds and intervention triggers.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, post_colonial_scholars, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total state collapse and humanitarian catastrophe in territories where central governments lack effective governance capacity, by substituting international oversight, conditional aid, and targeted intervention for autonomous but failing domestic authority.
% TRANSFER_FUNCTION: Moves decision-making autonomy, policy sovereignty, and territorial control from weak-state governments to international evaluation authorities and intervening powers, conditioned on capacity-deficit scoring.
% ABSENT_VOICES: Post-colonial scholars and representatives of targeted weak states who reject capacity metrics as culturally specific and argue for unconditional sovereign equality; they are present in academic and diplomatic discourse but excluded from the threshold-setting bodies.
% DISAPPEARANCE_RATIONALE: If the graded-sovereignty framework vanished, weak states would regain full autonomy regardless of capacity scores, international oversight missions would require consent-based mandates, and the hierarchical distinction between full and nominal sovereignty would collapse into absolute non-intervention or conditional-responsibility alternatives.
% FOUNDING_PROBLEM: Post-Cold War state fragmentation and intra-state conflict producing humanitarian emergencies in territories with collapsed or predatory central governments, where classical sovereignty appeared to block effective protective action.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian organizations and some international-law scholars attest the problem remains live. Post-colonial scholars and many Global South governments attest the problem has been weaponized to justify neo-trusteeship; corroboration from outside the benefiting parties is divided rather than unanimous.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.62) because capacity scoring systematically transfers autonomy from weak states to evaluation authorities and interveners. Suppression is moderate-high (0.58): alternatives such as absolute non-intervention are actively discredited for failed states, and weak-state resistance is met with conditionality and diplomatic pressure. Theater ratio is moderate-low (0.25): much capacity assessment is genuine administrative work, but a growing share is performative metric production that justifies pre-determined oversight. Accessibility collapse (0.45) reflects that unconditional sovereignty is still asserted by some states but is largely unavailable to those scored as fragile. Resistance (0.52) captures organized pushback from Global South coalitions and targeted states. The measurement series runs on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the capacity-evaluation seat, the constraint is a necessary evolution of international law that prevents catastrophe in failed states while preserving global order. From the weak-state seat, it is a neo-colonial hierarchy that substitutes external judgment for self-determination. The engine computes this divergence from the same structural data: identical metrics produce different per-seat classifications because directionality inverts extractiveness for beneficiaries and amplifies it for trapped payers.
 *
 * DIRECTIONALITY LOGIC:
 *   capacity_evaluation_authorities and great_powers are structural beneficiaries (low directionality): they collect authority, funding, and strategic flexibility from the tiered system. weak_states are the primary payers (high directionality): they lose autonomy proportionate to their scored deficits. humanitarian_intervention_bodies are secondary beneficiaries (low-moderate directionality): they receive mandates and resources. post_colonial_scholars are excluded (analytical directionality). The engine derives this from beneficiary/victim declarations combined with exit options: evaluators have analytical and arbitrage exits; weak states are trapped because their recognition and aid depend on compliance with the scoring framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problemâstate collapse and humanitarian emergencies in the post-Cold War eraâwas genuine, but its persistence has outlived the exceptional circumstances of its origin. The framework now operates as a standing hierarchy rather than an emergency tool, risking mandatrophy. Classifying it as tangled_rope preserves the genuine coordination function (capacity building, protective intervention) while registering the asymmetric extraction (autonomy loss, paternalistic oversight). A snare classification would miss the coordination; a rope classification would miss the hierarchy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metrics_neutrality,
    'Do state-capacity indices embed universal administrative standards, or do they privilege specific liberal-democratic institutional forms associated with Western states?',
    'Comparative historical analysis of metric construction and correlation with intervention targets; testing whether non-Western state forms score systematically lower independent of empirical stability outcomes.',
    'If the metrics are culturally specific, the constraint''s justification collapses from technocratic neutrality to ideological hierarchy, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metrics_neutrality, conceptual, 'Whether capacity metrics are culturally neutral or Western-biased').

omega_variable(
    coordination_extraction_fusion,
    'Can the protective coordination in failed states be separated from the extractive autonomy-loss, or are they structurally fused?',
    'Natural experiment comparing consent-based capacity partnerships versus conditional-sovereignty interventions for comparable state-failure scenarios.',
    'If separable, the constraint is a tangled rope whose extraction is removable; if fused, the hierarchy is inseparable from the protective function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_fusion, conceptual, 'Whether coordination and extraction are structurally separable in graded sovereignty').

omega_variable(
    kernel_reading_contest,
    'Does the scalar-capacity reading of sovereignty represent a necessary legal evolution or a contested reinterpretation that privileges interventionist powers?',
    'Track adoption patterns in UN General Assembly voting and treaty practice to see whether the scalar reading is consolidating or eroding relative to absolute and conditional alternatives.',
    'If erosion is confirmed, the constraint may be a transitional scaffold rather than a stable tangled rope; if consolidation, the hierarchy is hardening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether graded sovereignty is consolidating or eroding as a legal framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(west_tr_t8, westphalia_sovereignty__graded_sovereignty, theater_ratio, 8, 0.13).
narrative_ontology:measurement(west_tr_t16, westphalia_sovereignty__graded_sovereignty, theater_ratio, 16, 0.17).
narrative_ontology:measurement(west_tr_t24, westphalia_sovereignty__graded_sovereignty, theater_ratio, 24, 0.21).
narrative_ontology:measurement(west_tr_t32, westphalia_sovereignty__graded_sovereignty, theater_ratio, 32, 0.24).
narrative_ontology:measurement(west_tr_t40, westphalia_sovereignty__graded_sovereignty, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(west_be_t8, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(west_be_t16, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(west_be_t24, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(west_be_t32, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(west_be_t40, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(west_su_t8, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(west_su_t16, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(west_su_t24, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(west_su_t32, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(west_su_t40, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, conditional_responsibility).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the westphalia_sovereignty kernel. The graded_sovereignty reading decomposes sovereignty into a scalar capacity metric, while siblings treat it as categorical (absolute_non_intervention) or conditional (conditional_responsibility). Each reading carries distinct epsilon, beneficiaries, and victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
