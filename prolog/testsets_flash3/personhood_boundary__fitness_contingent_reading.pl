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
 *   'fitness' (e.g., cognitive capacity, social contribution), denying moral
 *   standing to entities that do not meet these criteria. This reading of the
 *   personhood boundary kernel allows for the exclusion of pre-fitness
 *   entities and severely disabled infants from the moral community,
 *   justifying their instrumentalization or neglect. The state authority and
 *   dominant social groups are the primary beneficiaries, gaining control
 *   over resources and populations. This is one reading of the
 *   'personhood_boundary' kernel, distinct from 'birth_threshold_reading' and
 *   'potential_based_reading'.
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
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, 'd99d2bc6-ae81-4048-b320-c977cc91ae13').
narrative_ontology:cs_kernel_codification('d99d2bc6-ae81-4048-b320-c977cc91ae13', formalized).
narrative_ontology:cs_authority_grounding('d99d2bc6-ae81-4048-b320-c977cc91ae13', extraction).
narrative_ontology:cs_interpretation_layer_present('d99d2bc6-ae81-4048-b320-c977cc91ae13').
narrative_ontology:cs_reading_relation('d99d2bc6-ae81-4048-b320-c977cc91ae13', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('d99d2bc6-ae81-4048-b320-c977cc91ae13', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('d99d2bc6-ae81-4048-b320-c977cc91ae13', foundational, personhood_requires_demonstrated_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('d99d2bc6-ae81-4048-b320-c977cc91ae13', personhood_requires_demonstrated_capacity, empirically_contingent).
narrative_ontology:cs_axiom('d99d2bc6-ae81-4048-b320-c977cc91ae13', foundational, state_has_right_to_define_moral_community).
narrative_ontology:cs_axiom_status(state_has_right_to_define_moral_community, holdable).
narrative_ontology:cs_axiom_grounding('d99d2bc6-ae81-4048-b320-c977cc91ae13', state_has_right_to_define_moral_community, conventional).
narrative_ontology:cs_reference_frame('d99d2bc6-ae81-4048-b320-c977cc91ae13', rational_agent_supremacy).
narrative_ontology:cs_drift_state('d99d2bc6-ae81-4048-b320-c977cc91ae13', contemporary_human_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d99d2bc6-ae81-4048-b320-c977cc91ae13', '').
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

% Defines and enforces the criteria for personhood, granting or denying moral and legal standing based on demonstrated fitness. Benefits from the flexibility to exclude certain populations from rights and protections, reducing resource demands or justifying control.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the moral framework that justifies the exclusion of certain groups, often those perceived as 'unfit' or burdensome. This can lead to resource reallocation away from excluded groups and reinforce social hierarchies.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, dominant_social_group, beneficiary,
    powerful, generational, mobile, national).

% Lack moral and legal standing until they demonstrate 'fitness' according to criteria set by the state. They are vulnerable to instrumentalization, neglect, or elimination without legal recourse, bearing the full cost of exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_entities, payer,
    powerless, immediate, trapped, local).

% Are often categorized as failing to meet 'fitness' criteria, leading to their exclusion from full moral standing. They bear the cost of this exclusion through denial of rights, care, or even life, depending on the severity of the fitness test.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Challenge the fitness-contingent definition of personhood, arguing for inherent moral standing regardless of capacity. They face significant resistance from the state and dominant social groups who benefit from the existing framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, advocates_for_universal_personhood, observer,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit exclusionary, boundary for moral and legal personhood, allowing the state to allocate resources and define social obligations based on a measurable standard of 'fitness'.
% TRANSFER_FUNCTION: Transfers moral and legal standing (and associated rights/protections) from pre-fitness entities to the state and dominant social groups, who gain control over these populations and their resources.
% ABSENT_VOICES: The pre-fitness entities themselves, who are by definition unable to articulate their interests or resist their exclusion. Their advocates speak on their behalf, but lack direct representation in the decision-making process.
% DISAPPEARANCE_RATIONALE: If personhood were no longer contingent on fitness, the state would lose a powerful tool for social control and resource allocation. Previously excluded populations would gain rights, demanding a fundamental reorganization of legal, social, and ethical frameworks.
% FOUNDING_PROBLEM: To manage societal resources and define social obligations by establishing a clear, rational, and defensible boundary for who counts as a full member of the moral community, particularly in contexts of scarcity or perceived burden.
% FOUNDING_PROBLEM_CORROBORATION: The state authority and dominant social groups assert the problem is live, citing ongoing challenges in resource allocation and social order. Advocates for universal personhood contest the legitimacy of the 'problem' itself, arguing it's a justification for exclusion rather than a genuine societal need.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Snare due to its high extractiveness (0.85) and suppression (0.90). It actively extracts moral standing and rights from vulnerable populations, transferring power and resources to those who define and enforce the 'fitness' criteria. Suppression is high because the excluded entities are inherently powerless and their advocates face significant institutional resistance. Theater ratio is low (0.10) as the constraint's function is direct and coercive, with little performative cover.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and dominant social groups, this constraint provides a rational and necessary framework for social order and resource management. From the perspective of the excluded and their advocates, it is a mechanism of profound injustice and dehumanization. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state authority and dominant social group are clear beneficiaries (low d) as they define and profit from the exclusion. Pre-fitness entities and severely disabled infants are full targets (high d) as they bear the entire cost of exclusion, lacking any exit options. Advocates for universal personhood are observers, attempting to shift the constraint's definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_fitness_ambiguity,
    'What constitutes ''fitness'' and who legitimately defines it? Is it cognitive capacity, social contribution, or something else entirely?',
    'Philosophical consensus on objective criteria for moral relevance, or a democratic process involving all affected parties (including proxies for the currently excluded).',
    'A narrower, less exclusionary definition of fitness would reduce the victim set and lower extractiveness; a broader, more inclusive definition could dissolve the constraint entirely. If the definition is arbitrary, the constraint is revealed as pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_fitness_ambiguity, conceptual, 'Ambiguity in the criteria for ''fitness'' that determines personhood.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal exclusion, institutional neglect) or internalized (societal acceptance of exclusion)?',
    'Post-exclusion trajectory: if exclusion persists after legal barriers are removed, reclassify as partially internalized. Historical analysis of public discourse and educational curricula.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the excluded carry the suppression with them. This would make the snare more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded entities.').

omega_variable(
    historical_contingency_vs_natural_law,
    'Is the fitness-contingent personhood boundary a historically contingent social construct, or is it presented as a natural, inevitable feature of reality?',
    'Genealogical analysis of philosophical and legal texts, tracing the historical emergence and contestation of ''fitness'' criteria. Cross-cultural comparison of personhood definitions.',
    'If presented as natural law, it functions as a false summit, masking its extractive nature. If recognized as contingent, it opens pathways for contestation and redefinition, potentially lowering extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_vs_natural_law, conceptual, 'Whether the constraint''s naturalness claim is genuine or a cover for social construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__fitness_contingent_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__fitness_contingent_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__fitness_contingent_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__fitness_contingent_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__fitness_contingent_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__fitness_contingent_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__fitness_contingent_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__fitness_contingent_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__fitness_contingent_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__fitness_contingent_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__fitness_contingent_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__fitness_contingent_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__fitness_contingent_reading, suppression_requirement, 30, 0.9).
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
