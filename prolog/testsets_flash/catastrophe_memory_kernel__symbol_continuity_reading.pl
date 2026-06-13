% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual Preserves Symbolic Continuity and Collective Identity (Symbol Continuity Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in preserving symbolic
 *   continuity and collective identity across generations, particularly in
 *   the context of catastrophic memory. It is a 'piton' because its primary
 *   function has atrophied into theatrical maintenance, with the
 *   'beneficiary' being the abstract concept of tradition itself, rather than
 *   active agents. The costs are borne by those who might seek adaptive
 *   modification or individual expression outside the ritual's rigid forms.
 *
 * KEY AGENTS:
 *   - religious_community: Agenda-setter (institutional/generational) — Administers and transmits the ritual forms.
 *   - tradition_continuity: Beneficiary (abstract/civilizational) — The abstract concept of the tradition's unbroken line, which is reinforced by the ritual.
 *   - group_identity: Beneficiary (abstract/civilizational) — The collective self-concept of the community, sustained by shared symbolic practice.
 *   - adaptive_modification: Victim (abstract/generational) — The potential for the community to adapt its practices or beliefs in response to changing circumstances, which is constrained by ritual rigidity.
 *   - individual_adherents: Payer (moderate/biographical) — Bear the costs of adhering to rigid ritual forms, potentially sacrificing individual autonomy or contemporary relevance.
 *   - critical_scholars: Observer (analytical/generational) — Analyze the ritual's function, its historical evolution, and its costs/benefits to the community.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.2).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual Preserves Symbolic Continuity and Collective Identity (Symbol Continuity Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '0ec3e9f0-9881-4a2c-b5e9-4a4820332435').
narrative_ontology:cs_kernel_codification('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', implicit).
narrative_ontology:cs_authority_grounding('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', practice).
narrative_ontology:cs_interpretation_layer_present('0ec3e9f0-9881-4a2c-b5e9-4a4820332435').
narrative_ontology:cs_reading_relation('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', foundational, symbolic_continuity_is_primary_function).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', symbolic_continuity_is_primary_function, conventional).
narrative_ontology:cs_axiom('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', foundational, collective_identity_requires_shared_symbols).
narrative_ontology:cs_axiom_status(collective_identity_requires_shared_symbols, holdable).
narrative_ontology:cs_axiom_grounding('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', collective_identity_requires_shared_symbols, conventional).
narrative_ontology:cs_reference_frame('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', unbroken_symbolic_transmission).
narrative_ontology:cs_drift_state('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ec3e9f0-9881-4a2c-b5e9-4a4820332435', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, group_identity).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, individual_autonomy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because the primary 'gain' is symbolic and diffuse (tradition, identity), not material. Suppression is moderate (0.3) as adherence is largely social and internalized, rather than coercively enforced. The theater_ratio is high (0.6) because the ritual's original, more active function (e.g., direct survival instruction) has largely faded, leaving behind a performative shell that primarily signals continuity. The constraint persists due to institutional inertia and the perceived value of unbroken tradition, even if its practical utility is diminished.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious community, the ritual is a vital Rope, a necessary coordination mechanism for maintaining identity. From the perspective of an individual adherent seeking adaptive change, it can feel like a Snare, trapping them in outdated practices. The analytical observer sees it as a Piton, a structure whose original function has atrophied but persists due to inertia and symbolic value.
 *
 * DIRECTIONALITY LOGIC:
 *   The abstract 'tradition_continuity' and 'group_identity' are beneficiaries (d=0.0-0.1) as they are reinforced by the ritual. The 'religious_community' as an institution is an agenda-setter (d=0.2-0.3) as it administers the ritual, but its 'benefit' is largely the perpetuation of its own role. 'Individual_adherents' are payers (d=0.7-0.8) as they bear the costs of rigidity. 'Adaptive_modification' is a victim (d=1.0) as its very possibility is suppressed by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a strong candidate for mandatrophy. Its original mandate might have been to transmit concrete survival skills or to process acute trauma. Over time, this active function has atrophied, but the ritual persists due to its symbolic power and the inertia of the institutions that administer it. The high theater_ratio (0.6) indicates that a significant portion of the activity is now performative, maintaining the appearance of function rather than delivering it. The classification as a Piton reflects this atrophy, preventing it from being mislabeled as a Rope (if its coordination function is now minimal) or a Snare (if the extraction is diffuse and primarily symbolic, rather than concentrated and material).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily about symbolic continuity, or is it better understood as encoding survival competence, intergenerational trauma, or group boundary maintenance?',
    'Empirical study of ritual function in different contexts: if the primary observed effect is symbolic transmission and identity reinforcement, this reading is validated. If it consistently yields adaptive survival skills, trauma processing, or boundary enforcement, a sibling reading is more appropriate.',
    'If a sibling reading is more accurate, the classification (and associated metrics) would shift to reflect that primary function. For instance, a ''survival_competence_reading'' might have lower theater_ratio and higher extractiveness (from those who fail to adapt).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading (symbol_continuity_reading) of the catastrophe_memory_kernel. Sibling readings include survival_competence_reading, trauma_encoding_reading, and boundary_maintenance_reading. The core disagreement is on the primary function and beneficiary of the ritual.').

omega_variable(
    ritual_rigidity_cost,
    'What is the actual cost of ritual rigidity in terms of foregone adaptive modification or individual autonomy?',
    'Comparative analysis of groups with varying ritual rigidity in similar environments: measure differences in adaptation rates, innovation, and individual well-being.',
    'If the costs are high, the extractiveness and suppression metrics for this reading would increase, potentially shifting it towards a Snare, as the ''symbolic continuity'' justification would be seen as cover for the costs imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_rigidity_cost, empirical, 'The cost of maintaining symbolic continuity through rigid ritual practices, potentially hindering adaptive change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel'. Each reading emphasizes a different primary function and has a different structural profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
