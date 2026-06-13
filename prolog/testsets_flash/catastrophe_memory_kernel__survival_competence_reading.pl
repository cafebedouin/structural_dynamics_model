% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Persecution-Survival Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes how ritual practices within a community facing
 *   historical or ongoing persecution function to encode and transmit
 *   practical adaptive capacities for survival. It's a 'survival-training'
 *   mechanism, where the 'mourning-practice' of past catastrophes serves as a
 *   rehearsal for future threats. The community benefits from enhanced
 *   resilience, while individuals face pressure to conform to these
 *   practices, bearing the costs of resisting assimilation into a dominant
 *   culture. This is one reading of the broader 'catastrophe_memory_kernel'.
 *
 * KEY AGENTS:
 *   - community_elders: Agenda setter (institutional/generational) – transmit and enforce ritual practices.
 *   - community_members: Payer/Beneficiary (moderate/biographical) – participate in rituals, gain survival competence, bear assimilation costs.
 *   - assimilating_individuals: Victim (powerless/biographical) – bear the direct cost of resisting community norms or losing cultural connection.
 *   - dominant_culture: Excluded (institutional/civilizational) – exerts pressure for assimilation, but is not directly part of the ritual system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Persecution-Survival Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'd64705d4-ea7e-4064-8b5a-886669bf1b00').
narrative_ontology:cs_kernel_codification('d64705d4-ea7e-4064-8b5a-886669bf1b00', implicit).
narrative_ontology:cs_authority_grounding('d64705d4-ea7e-4064-8b5a-886669bf1b00', lineage).
narrative_ontology:cs_interpretation_layer_present('d64705d4-ea7e-4064-8b5a-886669bf1b00').
narrative_ontology:cs_reading_relation('d64705d4-ea7e-4064-8b5a-886669bf1b00', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64705d4-ea7e-4064-8b5a-886669bf1b00', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('d64705d4-ea7e-4064-8b5a-886669bf1b00', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('d64705d4-ea7e-4064-8b5a-886669bf1b00', foundational, catastrophe_as_training_ground).
narrative_ontology:cs_axiom_status(catastrophe_as_training_ground, holdable).
narrative_ontology:cs_axiom_grounding('d64705d4-ea7e-4064-8b5a-886669bf1b00', catastrophe_as_training_ground, empirically_contingent).
narrative_ontology:cs_axiom('d64705d4-ea7e-4064-8b5a-886669bf1b00', foundational, ritual_as_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('d64705d4-ea7e-4064-8b5a-886669bf1b00', ritual_as_adaptive_rehearsal, instrumental).
narrative_ontology:cs_reference_frame('d64705d4-ea7e-4064-8b5a-886669bf1b00', community_as_survival_unit).
narrative_ontology:cs_drift_state('d64705d4-ea7e-4064-8b5a-886669bf1b00', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d64705d4-ea7e-4064-8b5a-886669bf1b00', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, community_resilience).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, cultural_integration_pressures).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a Tangled Rope because it genuinely coordinates (transmitting survival skills for collective benefit) but also extracts (demanding conformity and imposing costs on individuals who might prefer assimilation). Extractiveness is moderate (0.45) as the 'cost' is primarily the effort of maintaining distinct cultural practices and resisting external pressures. Suppression is moderate (0.6) due to social pressure within the community to participate and the active enforcement of ritual adherence by elders. Theater ratio is low (0.2) because the rituals are largely functional in their stated purpose of preserving adaptive capacity, with minimal performative excess.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community elders, the constraint is a vital Rope, ensuring the group's long-term survival. From the perspective of assimilating individuals, it can feel like a Snare, trapping them between cultural loyalty and external integration. The engine's classification as Tangled Rope reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Community elders (agenda_setter) are beneficiaries as they maintain the community's integrity and future. Community members are both beneficiaries (gaining survival competence) and payers (bearing the burden of ritual adherence and resisting assimilation). Assimilating individuals are victims, as they face the highest costs for non-conformity. The dominant culture is an external force, not directly participating in the constraint's internal dynamics, but its pressure is the context for the constraint's existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (persecution-survival) remains live as long as the community perceives a threat of persecution or assimilation. If external threats diminish significantly, the constraint could drift towards a Piton (ritual for ritual's sake) or a Snare (if the internal enforcement of cultural distinctiveness becomes purely extractive without a genuine external threat). The current classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring individual costs) or a pure Snare (ignoring the genuine survival function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint primarily about transmitting survival competence, or is it better understood as a different reading of the catastrophe memory kernel?',
    'Empirical study of ritual outcomes: does ritual participation demonstrably improve adaptive capacity and survival rates under threat, or does it primarily reinforce identity, encode trauma, or maintain boundaries?',
    'If the primary function is different, the constraint would be reclassified under a sibling reading (e.g., symbol_continuity_reading as a Rope, trauma_encoding_reading as a Snare, boundary_maintenance_reading as a Tangled Rope with different beneficiaries/victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''survival_competence_reading'' of the ''catastrophe_memory_kernel''.').

omega_variable(
    assimilation_pressure_source,
    'Is the ''assimilation pressure'' a genuine external threat or an internalized group norm enforced by the community itself?',
    'Sociological analysis of external vs. internal pressures on community members; ethnographic study of individual experiences of assimilation and community response.',
    'If primarily internalized, the suppression metric might be higher, and the ''victims'' would be more directly ''identity_locked'' by internal community dynamics rather than external pressures, potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_pressure_source, empirical, 'Source of assimilation pressure on individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_memory_kernel', each representing a distinct structural claim about how ritual functions in response to collective trauma.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
