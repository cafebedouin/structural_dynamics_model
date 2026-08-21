% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Statehood Criteria: Hybrid Reading (Objective + Normative Legitimacy)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of the Montevideo
 *   Convention criteria for statehood, which posits that in addition to
 *   objective criteria (territory, population, government, capacity for
 *   international relations), normative legitimacy (e.g., democratic
 *   governance, respect for human rights, non-aggression) is also required
 *   for an entity to be recognized as a state. This reading emerged
 *   particularly after the Cold War, influencing recognition practices in
 *   cases like the former Yugoslavia. It is a contested interpretation, with
 *   significant implications for secessionist movements and the justification
 *   of international intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.65).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.75).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Statehood Criteria: Hybrid Reading (Objective + Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '2976aa6f-c645-4d5f-9a5a-09c059afe622').
narrative_ontology:cs_kernel_codification('2976aa6f-c645-4d5f-9a5a-09c059afe622', formalized).
narrative_ontology:cs_authority_grounding('2976aa6f-c645-4d5f-9a5a-09c059afe622', lineage).
narrative_ontology:cs_interpretation_layer_present('2976aa6f-c645-4d5f-9a5a-09c059afe622').
narrative_ontology:cs_reading_relation('2976aa6f-c645-4d5f-9a5a-09c059afe622', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('2976aa6f-c645-4d5f-9a5a-09c059afe622', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('2976aa6f-c645-4d5f-9a5a-09c059afe622', foundational, statehood_requires_normative_legitimacy).
narrative_ontology:cs_axiom_status(statehood_requires_normative_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2976aa6f-c645-4d5f-9a5a-09c059afe622', statehood_requires_normative_legitimacy, deontological).
narrative_ontology:cs_axiom('2976aa6f-c645-4d5f-9a5a-09c059afe622', secondary, human_rights_are_precondition_for_sovereignty).
narrative_ontology:cs_axiom_status(human_rights_are_precondition_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2976aa6f-c645-4d5f-9a5a-09c059afe622', human_rights_are_precondition_for_sovereignty, deontological).
narrative_ontology:cs_reference_frame('2976aa6f-c645-4d5f-9a5a-09c059afe622', post_cold_war_normative_shift).
narrative_ontology:cs_drift_state('2976aa6f-c645-4d5f-9a5a-09c059afe622', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2976aa6f-c645-4d5f-9a5a-09c059afe622', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, states_with_authoritarian_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from a framework that justifies their non-recognition of entities lacking democratic governance or human rights records, and provides a basis for intervention. They actively shape and enforce this hybrid interpretation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These movements may meet objective criteria for statehood (territory, population, government, capacity to enter relations) but are denied recognition due to their non-democratic or human rights-violating nature. They bear the cost of non-recognition, including lack of international aid, trade, and security guarantees.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, generational, trapped, regional).

% Existing states with authoritarian regimes face potential delegitimization or intervention under this reading, as their internal governance fails the normative test. They resist this interpretation but are constrained by the power of liberal states.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, states_with_authoritarian_regimes, payer,
    powerful, biographical, constrained, national).

% These organizations gain a stronger legal and moral basis for advocating human rights and democratic principles within the framework of state recognition, influencing policy and public opinion.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_organizations, beneficiary,
    organized, generational, mobile, global).

% Advocates of the purely declaratory theory of statehood argue that normative criteria are irrelevant to legal statehood, but their arguments are often sidelined in practical international relations where normative considerations are paramount.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, declaratory_theorists, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the international community to coordinate on the recognition of new states, ensuring that recognized entities adhere to certain shared normative values (democracy, human rights) beyond mere objective capacity.
% TRANSFER_FUNCTION: Transfers legitimacy and international standing from entities that fail normative tests to those that uphold them, and from non-liberal secessionist movements to the existing liberal-democratic order. It also transfers the burden of non-recognition onto entities deemed normatively illegitimate.
% ABSENT_VOICES: Purely declaratory theorists, who would argue that statehood is an objective legal fact independent of normative judgments, are often excluded from the practical application of this hybrid reading. Non-liberal secessionist movements, whose very existence is delegitimized by this reading, have little voice in its formulation.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the criteria for state recognition would revert to either purely objective (declaratory) or purely political (constitutive), leading to a significant rearrangement of international relations. Non-liberal entities might gain easier recognition, and the justification for humanitarian intervention would weaken, fundamentally altering the global political landscape.
% FOUNDING_PROBLEM: The original Montevideo Convention criteria (territory, population, government, capacity for relations) proved insufficient to address situations where entities met objective criteria but engaged in egregious human rights abuses or lacked democratic legitimacy, leading to moral and political dilemmas for the international community.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights advocates, and numerous UN resolutions corroborate the ongoing problem of balancing objective statehood with normative expectations. While some states might prefer a purely objective standard, the consensus among liberal democracies and international organizations supports the need for normative criteria.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading imposes significant costs on entities that meet objective criteria but fail normative ones, denying them the benefits of statehood. Suppression (0.75) is high as it requires active political and diplomatic enforcement by powerful states to deny recognition and legitimacy to non-compliant entities. The theater ratio (0.20) is relatively low, as the normative criteria are genuinely applied, though sometimes selectively. The increasing extractiveness and suppression over time reflect the growing emphasis on normative criteria in post-Cold War international relations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberal democratic states, this is a legitimate and necessary evolution of international law, ensuring a more just and stable world order. From the perspective of non-liberal secessionist movements, it is a form of political extraction, denying their right to self-determination based on criteria that are selectively applied and serve the interests of powerful states.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and human rights organizations are beneficiaries, as this reading aligns with their values and provides tools for their foreign policy and advocacy. Non-liberal secessionist movements and authoritarian regimes are victims, as they are denied recognition or face delegitimization. The 'declaratory theorists' are excluded, as their purely objective view is often overridden by political realities shaped by this hybrid reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_of_application,
    'To what extent are the normative criteria (democracy, human rights) applied consistently across all potential states, or are they selectively applied based on geopolitical interests?',
    'Empirical analysis of state recognition patterns over time, comparing cases where normative criteria were invoked versus cases where they were overlooked for strategic reasons.',
    'If application is highly selective, the effective extractiveness of this reading is higher, as it functions more as a tool of power projection than a consistent legal standard. This would push the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_of_application, empirical, 'Assesses the consistency of normative criteria application.').

omega_variable(
    legitimacy_of_intervention,
    'Does the ''normative legitimacy'' component of statehood criteria genuinely provide a legal basis for humanitarian intervention or regime change, or is it primarily a moral justification that often precedes political decisions?',
    'Analysis of international court rulings and UN Security Council resolutions regarding intervention, examining the explicit legal grounds cited.',
    'If it primarily serves as a moral justification, the ''legal cover'' aspect of this reading is weaker, reducing its perceived legitimacy as a coordination mechanism and highlighting its extractive potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_intervention, conceptual, 'Clarifies the legal vs. moral grounding for intervention under this reading.').

omega_variable(
    balance_of_objective_and_normative,
    'What is the precise weighting or hierarchy between the objective Montevideo criteria and the normative legitimacy criteria in practice?',
    'Case studies of contested statehood claims where both sets of criteria were present but in tension, analyzing which ultimately prevailed in recognition decisions.',
    'If normative criteria consistently override objective ones, the reading''s extractiveness is higher, as it fundamentally redefines statehood away from traditional legal positivism. If objective criteria retain significant weight, it remains a more balanced ''tangled rope''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balance_of_objective_and_normative, empirical, 'Determines the practical hierarchy of objective vs. normative criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(mont_tr_t2000, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(mont_be_t2000, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(mont_su_t2000, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Montevideo Statehood Criteria kernel. It emphasizes normative legitimacy alongside objective criteria, influencing and being influenced by the purely declaratory and constitutive readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
