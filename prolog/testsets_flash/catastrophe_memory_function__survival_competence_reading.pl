% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Preserves Survival-Competence (Commemorative Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint describes the 'survival-competence' reading of
 *   commemorative ritual, where the ritual's primary function is to transmit
 *   adaptive capacity and resilience strategies across generations, enabling
 *   a community to survive and transform after catastrophe. It is a 'Rope'
 *   because it genuinely coordinates collective memory and adaptive behavior,
 *   with low extraction and suppression, and clear benefits for the
 *   community. The Passover Seder, in this reading, is not just about
 *   remembering an event, but about rehearsing the skills and mindset needed
 *   for future liberation and survival.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Preserves Survival-Competence (Commemorative Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'b5555a0e-2c61-46e0-a69e-b74e40822395').
narrative_ontology:cs_kernel_codification('b5555a0e-2c61-46e0-a69e-b74e40822395', formalized).
narrative_ontology:cs_authority_grounding('b5555a0e-2c61-46e0-a69e-b74e40822395', lineage).
narrative_ontology:cs_interpretation_layer_present('b5555a0e-2c61-46e0-a69e-b74e40822395').
narrative_ontology:cs_reading_relation('b5555a0e-2c61-46e0-a69e-b74e40822395', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5555a0e-2c61-46e0-a69e-b74e40822395', catastrophe_memory_function__hybrid_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('b5555a0e-2c61-46e0-a69e-b74e40822395', foundational, ritual_as_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('b5555a0e-2c61-46e0-a69e-b74e40822395', ritual_as_adaptive_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('b5555a0e-2c61-46e0-a69e-b74e40822395', foundational, collective_memory_as_survival_tool).
narrative_ontology:cs_axiom_status(collective_memory_as_survival_tool, holdable).
narrative_ontology:cs_axiom_grounding('b5555a0e-2c61-46e0-a69e-b74e40822395', collective_memory_as_survival_tool, empirically_contingent).
narrative_ontology:cs_reference_frame('b5555a0e-2c61-46e0-a69e-b74e40822395', ritual_as_adaptive_transmission).
narrative_ontology:cs_drift_state('b5555a0e-2c61-46e0-a69e-b74e40822395', contemporary_secularization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b5555a0e-2c61-46e0-a69e-b74e40822395', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, future_generations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) as the ritual primarily serves the community's adaptive needs, with minimal costs beyond participation. Suppression is also low (0.2) as adherence is largely voluntary and identity-driven, rather than coercively enforced. Theater ratio is low (0.1) because the ritual's functional role in transmitting survival knowledge is central and actively maintained. Accessibility collapse is high (0.85) because, once the value of this collective memory is understood, there are few effective alternatives for transmitting such deep, embodied adaptive capacity across generations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a vital source of strength and continuity. From an external, analytical perspective, its efficacy in transmitting survival competence can be empirically studied and compared to other forms of collective memory. There is little divergence in perceived type, as the benefits are widely acknowledged within the community.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are clear beneficiaries, gaining adaptive capacity and resilience. Ritual leaders act as agenda-setters, guiding the transmission. The historical catastrophe itself is framed as a 'payer' in the sense that its 'cost' is what the ritual seeks to mitigate. External observers analyze its function without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_efficacy_measurement,
    'How can the ''survival competence'' transmitted by ritual be empirically measured and its effectiveness quantified in real-world adaptive outcomes?',
    'Longitudinal studies comparing communities with strong ritual memory to those without, assessing resilience metrics, institutional adaptability, and post-catastrophe recovery rates.',
    'Strong empirical evidence would solidify the ''Rope'' classification by demonstrating tangible coordination benefits. Lack of measurable impact might shift the classification towards ''Piton'' if the function is merely theatrical, or ''Tangled Rope'' if the benefits are diffuse while maintenance costs are concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_efficacy_measurement, empirical, 'Quantifying the adaptive benefits of ritual memory.').

omega_variable(
    survival_vs_mourning_primary_function,
    'Is the primary function of this ritual truly the transmission of survival competence, or is it primarily about mourning and identity maintenance, with survival as a secondary effect?',
    'Detailed ethnographic analysis of participant interpretations, textual exegesis of ritual scripts, and observation of ritual adaptations in response to new crises. This would distinguish between the ''survival_competence_reading'' and the ''mourning_practice_reading'' or ''hybrid_transformation_reading''.',
    'If mourning/identity is primary, the constraint might shift towards a different type (e.g., ''Tangled Rope'' if identity maintenance involves significant unacknowledged costs). If survival competence is clearly primary, the ''Rope'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_vs_mourning_primary_function, conceptual, 'Distinguishing the primary function of commemorative ritual between survival competence and mourning/identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 60, 0.19).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_function' kernel, focusing on the transmission of survival competence. It is linked to sibling readings that emphasize mourning practice or a hybrid function, as they all derive from the same underlying ritual phenomena but emphasize different structural functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
