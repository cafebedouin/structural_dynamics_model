% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Ritual Boundary Maintenance through Shared Mourning
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how a group's shared mourning rituals function
 *   primarily to enforce and maintain its social boundaries, particularly in
 *   the aftermath of a catastrophic event. The ritual coordinates collective
 *   memory and emotional expression, but in doing so, it extracts conformity
 *   from in-group members and actively excludes outsiders, solidifying the
 *   group's distinct identity. This is one reading of the
 *   'catastrophe_memory_kernel', focusing on the social function of boundary
 *   maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.55).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Ritual Boundary Maintenance through Shared Mourning").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, '45e3cdf5-0305-492b-99db-fe5d75e008d8').
narrative_ontology:cs_kernel_codification('45e3cdf5-0305-492b-99db-fe5d75e008d8', implicit).
narrative_ontology:cs_authority_grounding('45e3cdf5-0305-492b-99db-fe5d75e008d8', practice).
narrative_ontology:cs_interpretation_layer_present('45e3cdf5-0305-492b-99db-fe5d75e008d8').
narrative_ontology:cs_reading_relation('45e3cdf5-0305-492b-99db-fe5d75e008d8', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('45e3cdf5-0305-492b-99db-fe5d75e008d8', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('45e3cdf5-0305-492b-99db-fe5d75e008d8', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('45e3cdf5-0305-492b-99db-fe5d75e008d8', foundational, group_cohesion_requires_boundary_enforcement).
narrative_ontology:cs_axiom_status(group_cohesion_requires_boundary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('45e3cdf5-0305-492b-99db-fe5d75e008d8', group_cohesion_requires_boundary_enforcement, conventional).
narrative_ontology:cs_axiom('45e3cdf5-0305-492b-99db-fe5d75e008d8', foundational, shared_mourning_defines_group_identity).
narrative_ontology:cs_axiom_status(shared_mourning_defines_group_identity, holdable).
narrative_ontology:cs_axiom_grounding('45e3cdf5-0305-492b-99db-fe5d75e008d8', shared_mourning_defines_group_identity, conventional).
narrative_ontology:cs_reference_frame('45e3cdf5-0305-492b-99db-fe5d75e008d8', traditional_group_solidarity).
narrative_ontology:cs_drift_state('45e3cdf5-0305-492b-99db-fe5d75e008d8', contemporary_individualism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45e3cdf5-0305-492b-99db-fe5d75e008d8', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, group_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy_seekers).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets the mourning rituals, ensuring adherence to practices that reinforce group identity and boundaries. Benefits from the stability and authority derived from a cohesive group.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, group_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Participate in shared mourning practices, gaining a strong sense of belonging, collective identity, and social support. Their identity is deeply intertwined with the group's shared memory and rituals, making exit costly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, in_group_members, beneficiary,
    moderate, biographical, identity_locked, local).

% Bear the costs of conformity, including suppression of individual expression, emotional labor, and potential ostracization for deviating from prescribed mourning practices. Their desire for individual autonomy conflicts with the group's demand for collective adherence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, individual_autonomy_seekers, payer,
    powerless, biographical, constrained, local).

% Are explicitly or implicitly excluded from the core mourning rituals, reinforcing the group's boundaries. They are denied the social capital and belonging that participation would confer, and may face suspicion or hostility from the in-group.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_members, excluded,
    powerless, biographical, mobile, regional).

% Study the function and impact of such rituals, analyzing their role in collective memory, identity formation, and social cohesion, often from an external, critical perspective.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, ritual_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and reinforces a distinct group identity and cohesion by coordinating shared emotional experiences and collective memory around a catastrophic event, thereby maintaining clear social boundaries.
% TRANSFER_FUNCTION: Transfers social capital, belonging, and a sense of shared purpose to conforming in-group members, while extracting conformity, emotional labor, and the exclusion of those who do not adhere to the ritual or are outside the group.
% ABSENT_VOICES: Out-group members, who are actively excluded by the boundary-defining nature of the ritual, would object to its exclusionary practices. Individuals within the group who prioritize personal grief and autonomy over collective performance might also object to the pressure for conformity.
% DISAPPEARANCE_RATIONALE: If the shared mourning ritual and its enforcement vanished, the group's distinct boundaries would blur, internal cohesion would weaken, and its collective identity would erode. This would lead to a significant reorganization of social relations, potentially dissolving the group or transforming it into a less cohesive entity.
% FOUNDING_PROBLEM: The need to forge and maintain a strong, distinct group identity and solidarity, particularly after a shared catastrophic experience, to prevent internal fragmentation and external assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of religion and cultural anthropologists attest to the ongoing role of such rituals in maintaining group cohesion and identity, often observing these dynamics from an external, analytical perspective, corroborating the problem's persistence beyond the group's self-assertion.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the costs borne by individuals for conformity and the exclusion of others, which are inherent to the boundary-maintenance function. Suppression (0.60) is moderate, as social pressure and the deep identity-lock of in-group members enforce adherence. Theater ratio is low (0.15) because the ritual is highly functional in achieving its boundary-maintenance goal, even if that function is extractive for some. The slight increase in extractiveness and suppression over time reflects a potential hardening of boundaries as the group seeks to preserve its distinctiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of group leadership and many in-group members, the ritual is a vital coordination mechanism for survival and identity. From the perspective of individual autonomy seekers or out-group members, it is a coercive structure that demands conformity and enforces exclusion. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   In-group members and leadership are beneficiaries, gaining cohesion and authority. Individual autonomy seekers and out-group relations are victims, bearing the costs of conformity and exclusion. The 'identity_locked' exit option for in-group members reflects the deep psychological and social ties that make leaving the group, and thus the ritual, extremely difficult.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''catastrophe_memory_kernel'', or is its boundary-maintenance function merely an aspect of a broader symbolic or survival function?',
    'Comparative analysis of ritual variations: if rituals exist that maintain symbolic continuity or transmit survival competence without explicit boundary enforcement, then this reading is distinct. If boundary maintenance is always co-present, it may be a sub-function.',
    'If distinct, this classification stands. If a sub-function, its extractiveness might be re-attributed to a more foundational coordination function, potentially altering its type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishes the boundary-maintenance reading from other interpretations of catastrophe memory rituals.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression of individual autonomy structural (e.g., social ostracism, economic dependence) versus internalized (e.g., self-censorship, belief in the necessity of conformity)?',
    'Post-exit trajectory analysis: if individuals who leave the group continue to exhibit self-censorship or guilt, it suggests a higher degree of internalized suppression. Ethnographic studies on individual experiences of conformity.',
    'If internalized suppression is high, the constraint''s effective suppression is higher than the structural measure suggests, as the individual carries the suppression with them even after physical exit, making true exit more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in ritual conformity.').

omega_variable(
    necessity_of_exclusion,
    'Is the exclusion of out-group members a necessary component of in-group cohesion, or is it an incidental or even counterproductive side-effect?',
    'Sociological studies of groups that achieve high cohesion without explicit exclusionary rituals, or historical analysis of groups where exclusionary practices led to internal dissent or external conflict.',
    'If exclusion is not necessary, the extractiveness attributed to ''out_group_relations'' could be re-evaluated as pure extraction without a coordination justification, potentially increasing the overall extractiveness of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_exclusion, empirical, 'Whether group cohesion inherently requires the exclusion of outsiders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel', which describes how groups process and remember catastrophic events. Each reading focuses on a different structural function of the memory-ritual complex, leading to different extractiveness profiles and classifications. This reading emphasizes boundary maintenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
