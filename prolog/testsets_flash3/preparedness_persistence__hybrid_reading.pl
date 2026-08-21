% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes disaster preparedness as a stratified system
 *   where some components (e.g., engineering inspections, critical
 *   infrastructure maintenance) remain genuinely competent and functional,
 *   while others (e.g., many evacuation drills, certain bureaucratic
 *   reporting requirements) have become ritualized performances. This 'hybrid
 *   reading' acknowledges both functional and performative aspects, leading
 *   to a classification as a Piton due to the significant theater ratio and
 *   diffuse costs, but with a lower overall extractiveness than a pure Snare,
 *   reflecting the genuinely competent subsystems. The claimed type is Piton,
 *   reflecting the overall inertial and performative nature, even with
 *   functional components.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.45).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.3).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'c7063a47-ffef-48fe-9483-0f5db9d409fc').
narrative_ontology:cs_kernel_codification('c7063a47-ffef-48fe-9483-0f5db9d409fc', formalized).
narrative_ontology:cs_authority_grounding('c7063a47-ffef-48fe-9483-0f5db9d409fc', lineage).
narrative_ontology:cs_interpretation_layer_present('c7063a47-ffef-48fe-9483-0f5db9d409fc').
narrative_ontology:cs_reading_relation('c7063a47-ffef-48fe-9483-0f5db9d409fc', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7063a47-ffef-48fe-9483-0f5db9d409fc', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_axiom('c7063a47-ffef-48fe-9483-0f5db9d409fc', foundational, preparedness_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('c7063a47-ffef-48fe-9483-0f5db9d409fc', preparedness_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('c7063a47-ffef-48fe-9483-0f5db9d409fc', foundational, functional_and_ritualized_components_coexist).
narrative_ontology:cs_axiom_status(functional_and_ritualized_components_coexist, holdable).
narrative_ontology:cs_axiom_grounding('c7063a47-ffef-48fe-9483-0f5db9d409fc', functional_and_ritualized_components_coexist, empirically_contingent).
narrative_ontology:cs_reference_frame('c7063a47-ffef-48fe-9483-0f5db9d409fc', comprehensive_functional_preparedness).
narrative_ontology:cs_drift_state('c7063a47-ffef-48fe-9483-0f5db9d409fc', contemporary_institutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7063a47-ffef-48fe-9483-0f5db9d409fc', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, public_safety_agencies).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspectors).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, evacuation_drill_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining preparedness protocols and budgets. They benefit from the appearance of readiness and the avoidance of blame, often by maintaining ritualized components while underfunding critical ones. Their exit is constrained by career path dependence and institutional inertia.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, institutional_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Receive funding and mandates for preparedness activities. They benefit from the existence of protocols, even if some are ritualized, as it provides a framework for their operations. Their ability to exit or fundamentally alter the system is constrained by their institutional role.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, public_safety_agencies, beneficiary,
    organized, biographical, constrained, regional).

% Perform critical, competent inspections of infrastructure. Their work is genuinely functional and they benefit from the clear mandate and professional standards. They have relatively mobile exit options due to specialized skills.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspectors, beneficiary,
    moderate, immediate, mobile, local).

% Participate in ritualized drills that often lack real-world applicability. They bear the time and effort cost without gaining commensurate preparedness. Their exit is trapped by mandatory participation.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, evacuation_drill_participants, payer,
    powerless, immediate, trapped, local).

% Fund the entire preparedness apparatus, including both competent and ritualized components. They bear the cost of inefficient or performative activities without full transparency or direct benefit. Their exit is constrained by the political system.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, taxpayers, payer,
    organized, generational, constrained, national).

% Are often the first to encounter the gap between ritualized preparedness and actual disaster conditions. They bear the operational costs of inadequate training or outdated protocols. Their exit is constrained by professional commitment and identity.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_responders, payer,
    moderate, biographical, constrained, local).

% Are the ultimate targets of preparedness failures, bearing the direct costs of inadequate response. They are often excluded from the design and evaluation of preparedness systems until after a failure occurs.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, disaster_victims, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a multi-agency, multi-level response to potential disasters, ensuring that critical infrastructure is maintained and populations are informed and ready.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel time) from taxpayers to institutional administrators and public safety agencies, ostensibly for preparedness. It also transfers a sense of security (or false security) to the public.
% ABSENT_VOICES: Disaster victims and independent auditors who would expose the gap between claimed and actual readiness are often absent from the design and evaluation of preparedness systems, or their warnings are downplayed.
% DISAPPEARANCE_RATIONALE: If the entire preparedness framework vanished, there would be immediate chaos in response to any major event. Critical infrastructure would degrade faster, and public trust in government's ability to protect them would collapse, leading to significant societal reorganization.
% FOUNDING_PROBLEM: The need to mitigate the impact of natural and man-made disasters, protect lives and property, and ensure rapid recovery.
% FOUNDING_PROBLEM_CORROBORATION: All stakeholders, including independent experts and historical analysis of disaster events, corroborate that the founding problem of disaster mitigation remains live and critical. The contest is over the effectiveness of the current solutions.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while some resources are genuinely used for effective preparedness, a significant portion is diverted to maintaining ritualized components that yield little real benefit. Suppression is low (0.30) as there's little active coercion to maintain the rituals beyond institutional inertia; resistance is also low (0.20) because the costs are diffuse and the benefits of challenging the system are unclear. The theater ratio is high (0.55) because a substantial part of 'preparedness' is performative rather than functional. Accessibility collapse is moderate (0.40) as alternatives to the current system are difficult to implement due to institutional lock-in, but not impossible.
 *
 * PERSPECTIVAL GAP:
 *   Administrators may perceive the entire system as a necessary Rope, emphasizing the functional components and downplaying the ritualized ones. Taxpayers and frontline responders, however, experience the system as a Piton, bearing diffuse costs for a system that often fails to deliver on its promises, with significant performative elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional administrators and public safety agencies are beneficiaries, as they maintain their roles and budgets through the system. Engineering inspectors are also beneficiaries, as their functional work is valued. Taxpayers and frontline responders are payers, bearing the costs of inefficiency and the risks of inadequate preparedness. Evacuation drill participants are also payers, expending effort for minimal gain. Disaster victims are excluded, bearing the ultimate costs without input.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy, as the original mandate of comprehensive disaster preparedness has partially atrophied into ritualized performance. The classification as a Piton prevents mislabeling it as a pure Rope (which would ignore the performative extraction) or a pure Snare (which would overstate active, concentrated extraction). The hybrid nature means the mandate is not entirely dead, but significantly degraded in specific areas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_ritualized_proportion,
    'What is the precise proportion of resources (budget, personnel time) allocated to genuinely functional preparedness components versus ritualized, performative ones?',
    'Detailed, independent audit of preparedness budgets and activity outcomes, distinguishing between activities with measurable impact on readiness and those primarily serving symbolic or compliance functions.',
    'A higher proportion of ritualized spending would increase the effective extractiveness and theater ratio, pushing the classification closer to a Snare or a more severe Piton. A lower proportion would support a more Rope-like classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_ritualized_proportion, empirical, 'Quantifying the balance between effective and performative preparedness.').

omega_variable(
    institutional_inertia_vs_active_maintenance,
    'To what extent does the persistence of ritualized preparedness stem from genuine institutional inertia (no one benefits enough to change it, no one is hurt enough to fix it) versus active, self-serving maintenance by administrators?',
    'Analysis of decision-making processes and budget allocations: if administrators actively resist reforms that would eliminate ritualized components, it suggests active maintenance. If reforms are simply neglected, it suggests inertia.',
    'If active maintenance is dominant, the constraint leans more towards a Tangled Rope or Snare, as there are identifiable beneficiaries actively defending the extractive components. If inertia is dominant, the Piton classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_active_maintenance, empirical, 'Distinguishing between passive inertia and active defense of performative elements.').

omega_variable(
    kernel_reading_difference,
    'What are the specific structural elements that differentiate this ''hybrid_reading'' from the ''competence_reading'' and ''husk_reading'' of preparedness persistence?',
    'Comparative analysis of the declared axioms and structural relationships across all three readings, identifying points of logical contradiction or divergence in their claims about the functional status of preparedness components.',
    'If the hybrid reading''s axioms are found to be more empirically robust, it strengthens its validity as a description of the system. If a sibling reading''s axioms are more consistent with observed data, it suggests a re-evaluation of this reading''s structural claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Clarifying the distinct structural claims of the hybrid reading within the preparedness persistence kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__hybrid_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__hybrid_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__hybrid_reading, theater_ratio, 30, 0.53).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.54).
narrative_ontology:measurement(prep_tr_t50, preparedness_persistence__hybrid_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__hybrid_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__hybrid_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__hybrid_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(prep_be_t50, preparedness_persistence__hybrid_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__hybrid_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__hybrid_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(prep_su_t30, preparedness_persistence__hybrid_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(prep_su_t50, preparedness_persistence__hybrid_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_persistence' kernel. The 'competence_reading' claims full functionality, the 'husk_reading' claims full performativity, and this 'hybrid_reading' claims a mix of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
