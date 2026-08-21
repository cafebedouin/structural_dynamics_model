% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Commemorative Ritual as Mourning Practice and Boundary-Norm Maintenance
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the Tisha B'Av ritual as a pure mourning
 *   practice and a mechanism for maintaining group identity and
 *   boundary-norms (D1/D4). It focuses on the commemorative and
 *   identity-forming aspects, explicitly excluding any direct transmission of
 *   survival competence or adaptive mechanisms. The ritual's persistence is
 *   driven by its role in collective memory and the reinforcement of a shared
 *   cultural and religious identity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__mourning_practice_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_function__mourning_practice_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_function__mourning_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_function__mourning_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Commemorative Ritual as Mourning Practice and Boundary-Norm Maintenance").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, '857230d4-b476-4dbd-8730-10d3282b3241').
narrative_ontology:cs_kernel_codification('857230d4-b476-4dbd-8730-10d3282b3241', formalized).
narrative_ontology:cs_authority_grounding('857230d4-b476-4dbd-8730-10d3282b3241', lineage).
narrative_ontology:cs_interpretation_layer_present('857230d4-b476-4dbd-8730-10d3282b3241').
narrative_ontology:cs_reading_relation('857230d4-b476-4dbd-8730-10d3282b3241', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('857230d4-b476-4dbd-8730-10d3282b3241', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('857230d4-b476-4dbd-8730-10d3282b3241', foundational, mourning_as_identity_foundation).
narrative_ontology:cs_axiom_status(mourning_as_identity_foundation, holdable).
narrative_ontology:cs_axiom_grounding('857230d4-b476-4dbd-8730-10d3282b3241', mourning_as_identity_foundation, deontological).
narrative_ontology:cs_axiom('857230d4-b476-4dbd-8730-10d3282b3241', foundational, ritual_as_boundary_maintenance).
narrative_ontology:cs_axiom_status(ritual_as_boundary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('857230d4-b476-4dbd-8730-10d3282b3241', ritual_as_boundary_maintenance, conventional).
narrative_ontology:cs_reference_frame('857230d4-b476-4dbd-8730-10d3282b3241', traditional_commemorative_practice).
narrative_ontology:cs_drift_state('857230d4-b476-4dbd-8730-10d3282b3241', contemporary_secular_context, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('857230d4-b476-4dbd-8730-10d3282b3241', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, religious_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_function__mourning_practice_reading, secular_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in the ritual, finding solace, reaffirming shared identity, and transmitting cultural memory. The ritual reinforces group cohesion and a sense of belonging, but also imposes obligations of observance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, religious_community, beneficiary,
    organized, generational, identity_locked, global).

% Administer and interpret the ritual, ensuring its continuity and adherence to tradition. They benefit from the authority and legitimacy derived from their role in preserving collective memory and identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, community_leaders, agenda_setter,
    institutional, generational, constrained, regional).

% May feel the obligation to participate in the ritual due to social pressure or family ties, even if they do not fully adhere to its religious tenets. They bear the cost of time and emotional labor without necessarily receiving the full spiritual benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, secular_members, payer,
    moderate, biographical, constrained, local).

% Analyze the ritual's historical development, social function, and evolution of meaning. They are external to the practice but provide critical insights into its persistence and impact on collective identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective mourning, reinforces group identity, and transmits boundary-norms across generations by providing a shared framework for remembering catastrophic loss.
% TRANSFER_FUNCTION: Transfers a sense of shared history, collective identity, and moral obligation from past generations to current and future members of the religious community.
% ABSENT_VOICES: Those who have left the religious community or reject the premise of collective obligation might argue that the ritual perpetuates trauma or hinders adaptation, but their voices are outside the ritual's internal discourse.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the religious community would lose a central mechanism for collective mourning and identity formation. This would lead to a fragmentation of shared memory and a weakening of group cohesion, forcing the community to find new ways to define itself and transmit its values.
% FOUNDING_PROBLEM: The problem of preserving the memory of catastrophic historical losses and maintaining group identity and cohesion in the face of existential threats.
% FOUNDING_PROBLEM_CORROBORATION: Community leaders and most active members attest that the problem of remembering and maintaining identity in a changing world is still live. Historical scholars corroborate the ritual's historical role in addressing this problem, noting its continued relevance for group survival and cultural transmission.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__mourning_practice_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_function__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is coordination of collective memory and identity, with minimal direct material extraction. Suppression is moderate (0.4) due to social pressure and identity-lock mechanisms, rather than overt coercion. Theater ratio is low (0.1) as the ritual is genuinely performed for its stated purpose of mourning and identity maintenance. Accessibility collapse is high (0.7) because for those within the community, alternatives for expressing this specific collective mourning and identity are limited. Resistance is low (0.15) as the ritual is largely accepted within the community, though some secular members may experience internal friction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious community, the ritual is a vital, low-cost mechanism for identity and memory. From a secular member's perspective, it can feel more extractive due to social obligation. The engine's classification will reflect these different experiences based on their declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious community is the primary beneficiary, gaining identity and cohesion. Community leaders are agenda-setters, benefiting from their role in preserving tradition. Secular members are payers, bearing the social and emotional costs without full belief. Historical scholars are observers, analyzing the phenomenon without direct participation or benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_ambiguity,
    'Is the ritual''s function purely commemorative and identity-forming, or does it implicitly transmit survival competence or adaptive mechanisms?',
    'Longitudinal ethnographic studies tracking community resilience and adaptive strategies in response to new challenges, correlating with ritual observance patterns. If communities with strong observance show demonstrably higher adaptive capacity, it suggests an implicit survival competence function.',
    'If implicit survival competence is found, the constraint would be reclassified towards a ''hybrid_transformation_reading'' or ''survival_competence_reading'', increasing its perceived coordination function and potentially altering its extractiveness if the ''competence'' is costly to acquire.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_ambiguity, empirical, 'Ambiguity regarding the ritual''s full functional scope beyond explicit mourning and identity.').

omega_variable(
    identity_lock_vs_voluntary_adherence,
    'To what extent is adherence to the ritual driven by genuine voluntary commitment versus identity-locked social pressure?',
    'Surveys and interviews with former community members who have exited, assessing the perceived costs and benefits of adherence and exit. If exit costs are primarily social and psychological, it points to stronger identity-lock.',
    'If identity-lock is a stronger driver, the effective suppression for ''religious_community'' and ''secular_members'' would be higher than currently measured, potentially shifting the constraint towards a ''tangled_rope'' for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_adherence, conceptual, 'Distinguishing genuine adherence from identity-based social coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
