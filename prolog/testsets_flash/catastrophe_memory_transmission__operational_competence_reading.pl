% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission (Operational Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint represents the 'operational competence' reading of
 *   catastrophe memory transmission through ritual. Here, ritual is
 *   understood primarily as a mechanism to encode and transmit practical
 *   survival skills, pattern recognition, resource coordination, and threat
 *   assessment rehearsal. Its value is in its functional yield for future
 *   survival capacity. This is one reading of the
 *   'catastrophe_memory_transmission' kernel, which also includes
 *   'symbol_continuity_reading' and 'hybrid_embedded_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission (Operational Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '98376a8a-bb78-456f-95ee-36fbde85345c').
narrative_ontology:cs_kernel_codification('98376a8a-bb78-456f-95ee-36fbde85345c', implicit).
narrative_ontology:cs_authority_grounding('98376a8a-bb78-456f-95ee-36fbde85345c', practice).
narrative_ontology:cs_interpretation_layer_present('98376a8a-bb78-456f-95ee-36fbde85345c').
narrative_ontology:cs_reading_relation('98376a8a-bb78-456f-95ee-36fbde85345c', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('98376a8a-bb78-456f-95ee-36fbde85345c', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('98376a8a-bb78-456f-95ee-36fbde85345c', foundational, ritual_as_operational_training).
narrative_ontology:cs_axiom_status(ritual_as_operational_training, holdable).
narrative_ontology:cs_axiom_grounding('98376a8a-bb78-456f-95ee-36fbde85345c', ritual_as_operational_training, empirically_contingent).
narrative_ontology:cs_axiom('98376a8a-bb78-456f-95ee-36fbde85345c', foundational, survival_competence_is_transmissible).
narrative_ontology:cs_axiom_status(survival_competence_is_transmissible, holdable).
narrative_ontology:cs_axiom_grounding('98376a8a-bb78-456f-95ee-36fbde85345c', survival_competence_is_transmissible, empirically_contingent).
narrative_ontology:cs_reference_frame('98376a8a-bb78-456f-95ee-36fbde85345c', functional_transmission_paradigm).
narrative_ontology:cs_drift_state('98376a8a-bb78-456f-95ee-36fbde85345c', contemporary_secularization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('98376a8a-bb78-456f-95ee-36fbde85345c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_leaders).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, those_mistaking_symbol_for_substance).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is genuine coordination for collective survival, with low extractiveness and suppression. The 'victim' group (those mistaking symbol for substance) highlights a failure mode where the operational content is lost, leading to a form of self-extraction due to misunderstanding, rather than active coercion. The low theater ratio reflects that the ritual's elements are largely functional, even if their purpose is not always consciously articulated by participants. Accessibility collapse is high because, once the operational value is understood, alternatives for such robust, trans-generational competence transmission are few.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community leaders, the ritual is a vital, low-cost coordination mechanism. From the perspective of those who only see the symbolic form, it might appear as an inert tradition, but this reading emphasizes the underlying operational function. The engine's classification will reflect the overall low extractiveness and high coordination, consistent with a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Community leaders and future generations are the primary beneficiaries, gaining resilience and continuity. Ritual participants bear the cost of participation but are also beneficiaries of the competence. Those mistaking symbol for substance are victims because they fail to extract the operational value, effectively paying for a performance without gaining its core benefit. Anthropological observers are analytical, outside the direct flow of costs and benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_primacy,
    'Is the operational competence the primary function of the ritual, or is it a secondary effect of symbolic continuity?',
    'Comparative analysis of ritual efficacy in communities where operational content is explicitly taught versus those where it is implicitly transmitted through symbolic fidelity. If explicit teaching yields higher competence, operational primacy is supported.',
    'If symbolic continuity is primary, the constraint might lean towards a more ''Mountain'' or ''Piton'' classification for the symbolic aspects, with operational competence being a less robust ''Rope'' component. If operational competence is primary, the ''Rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_primacy, conceptual, 'Ambiguity regarding the primary function of ritual: operational transmission vs. symbolic preservation.').

omega_variable(
    competence_measurement_validity,
    'How reliably can ''survival competence'' be measured and attributed to ritual transmission, independent of other learning mechanisms?',
    'Longitudinal studies tracking communities'' responses to actual or simulated catastrophes, correlating outcomes with ritual adherence and comparing to control groups without such rituals.',
    'If competence is difficult to measure or is primarily derived from other sources, the ''beneficiary'' claim for future generations weakens, potentially shifting the constraint towards a more ''Piton'' or ''Snare'' if the costs of participation remain high without clear benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_measurement_validity, empirical, 'Uncertainty in measuring the direct impact of ritual on survival competence.').

omega_variable(
    natural_law_vs_constructed_competence,
    'Is the transmission of operational competence through ritual a universal, emergent property of human collective memory (Mountain), or a culturally constructed coordination mechanism (Rope)?',
    'Cross-cultural comparative studies of diverse societies'' catastrophe memory rituals. If similar operational structures emerge independently across vastly different cultural contexts, it supports a ''Mountain'' claim. If structures are highly context-dependent, it supports ''Rope''.',
    'If a ''Mountain'' (natural law) is established, the extractiveness and suppression metrics would be re-evaluated as inherent costs of an irreducible reality. If ''Rope'' (constructed coordination) is confirmed, the current metrics hold, and the potential for optimization or alternative mechanisms remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_competence, conceptual, 'Ambiguity between natural law and constructed coordination for competence transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_transmission' kernel. This 'operational_competence_reading' focuses on the functional transmission of survival skills, distinct from the 'symbol_continuity_reading' (identity/mourning) and 'hybrid_embedded_reading' (inseparable form/function).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
