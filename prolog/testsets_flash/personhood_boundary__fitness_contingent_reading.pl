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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes a historical or philosophical position where
 *   personhood, and thus moral standing, is not inherent but contingent upon
 *   an individual demonstrating certain 'fitness' criteria (e.g.,
 *   rationality, self-awareness, social contribution). Entities that do not
 *   meet these criteria, such as infants before a certain developmental stage
 *   or individuals with severe disabilities, are denied full moral standing.
 *   This grants state authority the power to exclude these pre-fitness
 *   entities from the moral community, often leading to their exploitation or
 *   neglect. The constraint is claimed as a snare due to its inherent
 *   extraction and suppression of vulnerable populations.
 *
 * KEY AGENTS:
 *   - state_authority: Agenda setter (institutional/arbitrage) — defines and enforces fitness criteria, benefits from control.
 *   - dominant_social_group: Beneficiary (powerful/mobile) — benefits from the exclusion of others, maintains social hierarchy.
 *   - pre_fitness_entities: Victim (powerless/trapped) — denied moral standing, subject to exploitation.
 *   - severely_disabled_infants: Victim (powerless/trapped) — denied moral standing due to perceived lack of fitness.
 *   - marginalized_groups: Victim (powerless/constrained) — at risk of being reclassified as 'unfit' to justify their exploitation.
 *   - moral_philosophers: Observer (analytical/analytical) — analyze and critique the ethical implications of such a personhood definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.85).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.95).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, 'c7fbe8a5-b905-4456-ae01-64efbda35935').
narrative_ontology:cs_kernel_codification('c7fbe8a5-b905-4456-ae01-64efbda35935', formalized).
narrative_ontology:cs_authority_grounding('c7fbe8a5-b905-4456-ae01-64efbda35935', extraction).
narrative_ontology:cs_interpretation_layer_present('c7fbe8a5-b905-4456-ae01-64efbda35935').
narrative_ontology:cs_reading_relation('c7fbe8a5-b905-4456-ae01-64efbda35935', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('c7fbe8a5-b905-4456-ae01-64efbda35935', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('c7fbe8a5-b905-4456-ae01-64efbda35935', foundational, moral_standing_is_earned).
narrative_ontology:cs_axiom_status(moral_standing_is_earned, holdable).
narrative_ontology:cs_axiom_grounding('c7fbe8a5-b905-4456-ae01-64efbda35935', moral_standing_is_earned, empirically_contingent).
narrative_ontology:cs_axiom('c7fbe8a5-b905-4456-ae01-64efbda35935', foundational, state_defines_moral_community).
narrative_ontology:cs_axiom_status(state_defines_moral_community, holdable).
narrative_ontology:cs_axiom_grounding('c7fbe8a5-b905-4456-ae01-64efbda35935', state_defines_moral_community, conventional).
narrative_ontology:cs_reference_frame('c7fbe8a5-b905-4456-ae01-64efbda35935', meritocratic_moral_order).
narrative_ontology:cs_drift_state('c7fbe8a5-b905-4456-ae01-64efbda35935', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c7fbe8a5-b905-4456-ae01-64efbda35935', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, dominant_social_group).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, marginalized_groups).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).

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
 *   The extractiveness (0.85) is high because it denies fundamental rights and resources to a class of beings, allowing others to benefit from their lack of standing. Suppression (0.95) is severe, as the very definition of personhood is used to justify the denial of moral consideration, making resistance from the 'unfit' impossible by definition. Theater ratio is low (0.1) because the constraint is actively enforced and serves a clear, albeit extractive, function; there is little performative maintenance without real consequence. Accessibility collapse is high (0.9) because the conceptual framework itself collapses alternatives to personhood for the 'unfit'. Resistance (0.7) is high from those who advocate for universal personhood, but not from the 'unfit' themselves, who are structurally unable to resist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state authority and dominant social group, this constraint might be framed as a necessary mechanism for social order or resource allocation, where only 'contributing' members receive full moral standing. From the perspective of the victims, it is a fundamental denial of their existence and rights, leading to severe exploitation. The engine's classification as a snare reflects the latter, highlighting the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The state authority and dominant social group are clear beneficiaries (d=0.0-0.2) as they gain control and resources by defining who counts as a person. Pre-fitness entities, severely disabled infants, and marginalized groups are direct targets (d=0.9-1.0) as they bear the full cost of exclusion and lack of moral standing. Moral philosophers act as analytical observers (d=0.5), analyzing the system without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a snare, not a piton, because it actively serves the function of maintaining social hierarchies and resource control for the beneficiaries, rather than persisting due to inertia. The 'mandate' of defining personhood is actively leveraged for extraction, not atrophied. The classification prevents mislabeling active exploitation as mere institutional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criteria_ambiguity,
    'What constitutes ''demonstrated fitness'' and who defines it?',
    'Historical analysis of legal codes and philosophical texts that instantiate this reading; examination of contemporary policy debates where similar criteria are proposed.',
    'If criteria are vague or arbitrarily defined, the constraint''s extractiveness and suppression are higher, as the ''fitness'' test becomes a tool for social control rather than an objective measure. If criteria are clear and universally applicable, it might reduce perceived extraction, though the fundamental premise remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fitness_criteria_ambiguity, conceptual, 'Ambiguity in defining ''fitness'' for personhood.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the ''personhood_boundary'' kernel, or a distinct, unrelated constraint?',
    'Analysis of historical and philosophical arguments that explicitly link personhood to fitness, demonstrating a lineage of thought that grounds this reading within the broader debate on personhood.',
    'If it is a genuine reading, it highlights the contested nature of personhood and the potential for highly extractive interpretations. If it is unrelated, the ''personhood_boundary'' kernel itself might be less contested than assumed, or the contestation lies elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''personhood_boundary'' kernel, specifically the ''fitness_contingent_reading''. Sibling readings include ''birth_threshold_reading'' and ''potential_based_reading''. This reading differs by making moral standing conditional on demonstrated capabilities, allowing the state to exclude pre-test entities from the moral community.').

omega_variable(
    state_authority_legitimacy,
    'What is the source of the state authority''s legitimacy to define and enforce fitness criteria for personhood?',
    'Examination of the political philosophy and legal history underpinning the state''s claim to such authority. Is it derived from popular sovereignty, divine right, or a social contract that implicitly grants this power?',
    'If the legitimacy is weak or contested, the constraint''s suppression is more clearly coercive and less justified. If strongly legitimized, the suppression might be perceived as a necessary function of social order, even if still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_authority_legitimacy, conceptual, 'Source of state authority to define personhood fitness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__fitness_contingent_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__fitness_contingent_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__fitness_contingent_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'personhood_boundary' kernel. It defines personhood as contingent on demonstrated fitness, leading to the exclusion of pre-fitness entities. It is linked to other readings of the same kernel, such as 'birth_threshold_reading' and 'potential_based_reading', which offer alternative definitions of personhood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
