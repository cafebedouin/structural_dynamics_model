% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint defines personhood as contingent on demonstrated
 *   'fitness,' excluding pre-fitness entities from moral standing. It is a
 *   specific reading of the broader 'personhood_boundary' kernel. The state
 *   authority and dominant social groups benefit from this exclusion, which
 *   allows for the instrumentalization or neglect of those deemed 'unfit.'
 *   The constraint is actively enforced through legal and social mechanisms,
 *   and it meets significant resistance from advocates for universal
 *   personhood.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.85).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.9).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '18ec8b68-1dfe-442c-9280-a6eac0b0eace').
narrative_ontology:cs_kernel_codification('18ec8b68-1dfe-442c-9280-a6eac0b0eace', formalized).
narrative_ontology:cs_authority_grounding('18ec8b68-1dfe-442c-9280-a6eac0b0eace', extraction).
narrative_ontology:cs_interpretation_layer_present('18ec8b68-1dfe-442c-9280-a6eac0b0eace').
narrative_ontology:cs_reading_relation('18ec8b68-1dfe-442c-9280-a6eac0b0eace', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('18ec8b68-1dfe-442c-9280-a6eac0b0eace', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('18ec8b68-1dfe-442c-9280-a6eac0b0eace', foundational, personhood_requires_demonstrated_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('18ec8b68-1dfe-442c-9280-a6eac0b0eace', personhood_requires_demonstrated_capacity, empirically_contingent).
narrative_ontology:cs_axiom('18ec8b68-1dfe-442c-9280-a6eac0b0eace', foundational, state_has_right_to_define_moral_community).
narrative_ontology:cs_axiom_status(state_has_right_to_define_moral_community, holdable).
narrative_ontology:cs_axiom_grounding('18ec8b68-1dfe-442c-9280-a6eac0b0eace', state_has_right_to_define_moral_community, conventional).
narrative_ontology:cs_reference_frame('18ec8b68-1dfe-442c-9280-a6eac0b0eace', rational_agent_as_sole_moral_subject).
narrative_ontology:cs_drift_state('18ec8b68-1dfe-442c-9280-a6eac0b0eace', contemporary_human_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('18ec8b68-1dfe-442c-9280-a6eac0b0eace', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, dominant_social_group).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the criteria for personhood, granting or denying moral and legal standing based on demonstrated fitness. Benefits from the flexibility to exclude certain populations from rights and protections, reducing resource demands or consolidating power.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from a system that prioritizes the 'fit' and potentially reduces obligations to those deemed 'unfit.' This group's values often shape the definition of fitness, reinforcing their social and political dominance.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, dominant_social_group, beneficiary,
    powerful, generational, mobile, national).

% Lack moral and legal standing until they demonstrate 'fitness.' They are vulnerable to instrumentalization, neglect, or elimination, bearing the full cost of exclusion from the moral community.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_entities, payer,
    powerless, immediate, trapped, local).

% Are explicitly excluded from personhood if they cannot meet the 'fitness' criteria. They face the risk of being treated as non-persons, with their lives and well-being subject to the discretion of others.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Challenge the fitness-contingent view, arguing for inherent moral worth. They document the harms of exclusion and advocate for legal and ethical reforms, but operate outside the constraint's direct enforcement mechanism.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, advocates_for_universal_personhood, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit exclusionary, boundary for moral and legal personhood, allowing the state and society to allocate rights and resources based on a defined set of criteria.
% TRANSFER_FUNCTION: Transfers moral and legal standing (and associated rights/protections) from pre-fitness entities to the state and dominant social groups, who gain control over these entities' fates and resources.
% ABSENT_VOICES: The pre-fitness entities themselves, who cannot articulate their interests. Also, historical and philosophical traditions that assert inherent moral worth from conception or birth, which are actively suppressed or dismissed by this framework.
% DISAPPEARANCE_RATIONALE: If personhood were no longer contingent on fitness, the moral and legal status of many individuals would immediately change. Rights and protections would extend to previously excluded groups, fundamentally altering social obligations, resource allocation, and the very definition of who counts as a member of the moral community.
% FOUNDING_PROBLEM: To manage resource allocation and social obligations by defining a clear, justifiable boundary for moral personhood, particularly in contexts of scarcity or perceived social burden.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this view, often from state authorities or dominant social groups, argue that the problem of defining moral boundaries and managing societal resources remains live. Critics, including human rights advocates and ethicists, attest that the 'problem' is often a pretext for exclusion and control, not a genuine coordination challenge.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the complete denial of moral standing and rights to a class of beings, allowing others to benefit from their exclusion. Suppression (0.9) is severe, as the very definition of personhood is controlled, making resistance from within the excluded class impossible and external advocacy highly challenging. Theater ratio is low (0.1) because the constraint's function is direct exclusion, not performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state authority and dominant social group, this constraint provides a rational and necessary framework for social order and resource management. From the perspective of the excluded and their advocates, it is a mechanism of profound injustice and extraction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority and dominant social groups are clear beneficiaries, gaining power and resources through the exclusion. Pre-fitness entities and severely disabled infants are the primary victims, bearing the full cost of non-personhood. Advocates for universal personhood act as observers, challenging the constraint from an external analytical position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_fitness,
    'What constitutes ''fitness'' and who defines it? Is the definition objective or culturally/politically constructed?',
    'Historical and sociological analysis of how ''fitness'' criteria have been applied and evolved, identifying biases and power dynamics in their formulation.',
    'If ''fitness'' is found to be a social construct serving specific interests, the constraint''s legitimacy as a ''natural'' boundary collapses, reclassifying it more firmly as a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_fitness, conceptual, 'Ambiguity in the definition and source of ''fitness'' criteria.').

omega_variable(
    scope_of_exclusion,
    'What is the full scope of entities excluded by this fitness-contingent definition, beyond the obvious cases of infants?',
    'Detailed legal and ethical analysis of historical and contemporary applications of fitness criteria, including their use against marginalized groups, the elderly, or those with cognitive impairments.',
    'A broader scope of exclusion would increase the measured extractiveness and suppression, reinforcing the snare classification and highlighting the systemic nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_exclusion, empirical, 'Uncertainty regarding the full range of entities denied personhood.').

omega_variable(
    alternative_personhood_grounding,
    'Could an alternative grounding for personhood (e.g., sentience, inherent dignity) provide a more inclusive and less extractive boundary?',
    'Philosophical and ethical argumentation, coupled with legal and social experiments in jurisdictions adopting alternative personhood criteria.',
    'If a viable, less extractive alternative exists, the fitness-contingent constraint is revealed as a choice, not an inevitability, strengthening arguments for its reform or abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_personhood_grounding, preference, 'The existence and viability of alternative personhood definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__fitness_contingent_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__fitness_contingent_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__fitness_contingent_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__fitness_contingent_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__fitness_contingent_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__fitness_contingent_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__fitness_contingent_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__fitness_contingent_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__fitness_contingent_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__fitness_contingent_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
