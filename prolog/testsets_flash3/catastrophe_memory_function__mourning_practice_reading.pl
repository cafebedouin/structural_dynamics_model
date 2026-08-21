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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_function__mourning_practice_reading
 *   human_readable: Catastrophe Memory Function: Mourning Practice Reading (Tisha B'Av)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the function of a ritual (e.g., Tisha B'Av) as
 *   primarily preserving mourning practices and boundary norms, thereby
 *   maintaining group identity through memorial obligation. This reading
 *   emphasizes the D1/D4 aspects of ritual (identity and boundary
 *   maintenance) without incorporating elements of survival competence or
 *   adaptive transformation. It is one reading of the broader
 *   'catastrophe_memory_function' kernel.
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
narrative_ontology:human_readable(catastrophe_memory_function__mourning_practice_reading, "Catastrophe Memory Function: Mourning Practice Reading (Tisha B'Av)").
narrative_ontology:topic_domain(catastrophe_memory_function__mourning_practice_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__mourning_practice_reading, 'e79a8a90-82ef-49ae-88c5-fc7b96a7b07f').
narrative_ontology:cs_kernel_codification('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', formalized).
narrative_ontology:cs_authority_grounding('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', lineage).
narrative_ontology:cs_interpretation_layer_present('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f').
narrative_ontology:cs_reading_relation('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', foundational, memorial_obligation_is_identity).
narrative_ontology:cs_axiom_status(memorial_obligation_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', memorial_obligation_is_identity, deontological).
narrative_ontology:cs_axiom('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', foundational, ritual_as_boundary_maintenance).
narrative_ontology:cs_axiom_status(ritual_as_boundary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', ritual_as_boundary_maintenance, conventional).
narrative_ontology:cs_reference_frame('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', pure_commemorative_tradition).
narrative_ontology:cs_drift_state('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', contemporary_functionalist_interpretations, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('e79a8a90-82ef-49ae-88c5-fc7b96a7b07f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, religious_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__mourning_practice_reading, diaspora_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, collective_memory_theory).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__mourning_practice_reading, group_identity_cohesion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the ritual, finding meaning, solidarity, and continuity of identity through shared mourning. The obligation is deeply internalized, making exit from the practice a form of identity rupture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, community_members, beneficiary,
    organized, generational, identity_locked, global).

% Administer and interpret the ritual, ensuring its continuity and adherence to tradition. They benefit from the cohesion and authority derived from maintaining the practice, but are also bound by its strictures.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, religious_leaders, agenda_setter,
    institutional, generational, constrained, global).

% Study the ritual as a mechanism for collective memory and identity formation, analyzing its social and psychological functions without participating in its religious claims.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, secular_historians, observer,
    analytical, generational, analytical, global).

% Utilize the ritual as a primary means of maintaining cultural and religious identity across geographical dispersion, reinforcing shared heritage and belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__mourning_practice_reading, diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective mourning and remembrance of historical catastrophes, ensuring the transmission of shared identity and cultural memory across generations.
% TRANSFER_FUNCTION: Transfers a sense of shared history, collective grief, and group solidarity from past generations to present and future community members, reinforcing group boundaries and identity.
% ABSENT_VOICES: Those who have assimilated or disaffiliated from the community might view the ritual as an anachronism or an unnecessary burden, but their voices are typically outside the discourse of the practicing community.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community's sense of shared identity, historical continuity, and collective memory would be severely fractured, leading to significant social and cultural reorganization as members sought alternative forms of belonging or drifted away.
% FOUNDING_PROBLEM: The need to preserve the memory of catastrophic historical losses and maintain group identity and cohesion in the face of dispersion and existential threats.
% FOUNDING_PROBLEM_CORROBORATION: Community members and religious leaders universally attest to the ongoing relevance of the founding problem. Secular historians corroborate the function of such rituals in maintaining collective identity and memory, even if they do not endorse the theological claims.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__mourning_practice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because participation is largely voluntary and driven by internalized identity, with benefits (group cohesion, meaning) outweighing direct costs. Suppression is moderate (0.4) due to strong social norms and identity-lock mechanisms, rather than active coercion. Theater ratio is low (0.1) as the ritual's function is genuinely enacted and not primarily performative. Accessibility collapse is high (0.7) because for identity-locked members, alternatives for maintaining this specific form of group identity are limited. Resistance is low (0.15) as the practice is deeply embedded and widely accepted within the community.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a vital, identity-affirming practice. From an external, purely functional perspective, it might be seen as a mechanism for social reproduction. The internal experience of meaning and belonging is central to its persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and diaspora communities are beneficiaries, as the ritual directly provides identity, cohesion, and meaning (d near 0.0). Religious leaders are agenda-setters, benefiting from the authority and stability the ritual provides, but also bearing the costs of its administration (d near 0.2). Secular historians are observers, neither benefiting nor paying directly (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving memory and identity) remains live, as attested by both internal and external observers. The low theater ratio and ongoing relevance of the founding problem indicate it is not a piton. Its primary function is coordination of collective memory and identity, making it a Rope, not a Snare, despite the identity-locked exit options for participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_scope,
    'Does this ritual exclusively preserve mourning practice and boundary norms, or does it also implicitly transmit survival competence or adaptive mechanisms?',
    'Longitudinal ethnographic studies tracking community responses to new crises, or comparative analysis with other rituals explicitly designed for adaptive transmission. If new adaptive behaviors are consistently linked to ritual participation, the scope is broader.',
    'If the ritual also transmits survival competence, its classification might shift towards a ''hybrid_transformation_reading'' or ''survival_competence_reading'', implying a more complex coordination function (D1/D4 + D5) and potentially altering its extractiveness profile if adaptive benefits are substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_scope, empirical, 'Ambiguity regarding the full functional scope of the ritual beyond pure mourning and identity maintenance.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option a result of genuine internalized commitment versus subtle social or institutional pressure?',
    'Surveys of former community members on their reasons for disaffiliation and the social costs incurred, or analysis of community norms around dissent and non-participation. If social costs are high and actively enforced, the ''suppression'' metric might be understated.',
    'If a significant portion of the ''identity_locked'' status is due to active social pressure rather than purely internalized commitment, the ''suppression'' metric for the ''community_members'' seat would be higher, potentially shifting the constraint towards a ''Tangled Rope'' or ''Snare'' for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Distinguishing between internalized identity commitment and external social pressure in maintaining ritual adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__mourning_practice_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__mourning_practice_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 40, 0.23).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 80, 0.25).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__mourning_practice_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__mourning_practice_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__mourning_practice_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel, focusing on mourning practice and boundary norms. It is linked to sibling readings that emphasize survival competence or a hybrid function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
