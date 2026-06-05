% ============================================================================
% CONSTRAINT STORY: conversational_dogmas_interuption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conversational_dogmas_interuption, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conversational_dogmas_interuption
 *   human_readable: Conversational Dogmas (Interruption vs. Strong Civility)
 *   domain: social/technological
 *
 * SUMMARY:
 *   Conversational dogmas, particularly those surrounding interruption,
 *   create a subtle but powerful constraint on discourse. A norm against
 *   interruption can foster inclusion and respect for diverse viewpoints. But
 *   a dogma of 'strong civility,' where all interruptions are condemned, can
 *   also suppress valuable rapid clarification, dissenting opinions, and
 *   urgent information. This constraint is particularly evident in online
 *   discussions and algorithmically mediated communication. Dominant
 *   personalities can exploit anti-interruption norms to control the flow of
 *   conversation. Algorithmic design for conversational AI needs to consider
 *   these conflicting needs.
 *
 * KEY AGENTS:
 *   - Low Status Participants: Primary target (powerless/trapped). Bear cost of suppressed expression.
 *   - High Status Participants: Primary beneficiaries (powerful/mobile). Benefit from controlled conversation flow.
 *   - General Participants: Constrained by norms (moderate/constrained). Experience mix of benefit and cost.
 *   - Algorithm Developers: Institutional actors (institutional/constrained). Shape conversation norms through design.
 *   - Minority Viewpoints: Target of suppression (powerless/constrained). Risk of being unheard.
 *   - Analytical Observer: Analyzes meta-dynamics (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conversational_dogmas_interuption, 0.55).
domain_priors:suppression_score(conversational_dogmas_interuption, 0.6).
domain_priors:theater_ratio(conversational_dogmas_interuption, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conversational_dogmas_interuption, extractiveness, 0.55).
narrative_ontology:constraint_metric(conversational_dogmas_interuption, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(conversational_dogmas_interuption, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conversational_dogmas_interuption, tangled_rope).
narrative_ontology:human_readable(conversational_dogmas_interuption, "Conversational Dogmas (Interruption vs. Strong Civility)").
narrative_ontology:topic_domain(conversational_dogmas_interuption, "social/technological").

domain_priors:requires_active_enforcement(conversational_dogmas_interuption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conversational_dogmas_interuption, high_status_participants).
narrative_ontology:constraint_beneficiary(conversational_dogmas_interuption, algorithm_developers).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, low_status_participants).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, minority_viewpoints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Low-status participants often find themselves trapped in conversations where their contributions are suppressed. They may lack the social capital or confidence to interrupt, leading to their viewpoints being marginalized.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Most participants experience a mix of coordination and extraction. While conversation aims at information exchange (coordination), differing conversational styles or power dynamics (extraction) mean some participants are interrupted and others benefit from the coordination (but risk missing other contributions).
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% Perspective 3: High-status individuals benefit from the dogma. Their interruptions are more likely to be accepted and even seen as valuable contributions. Their ability to control conversation flow serves to reinforce their position.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Perspective 4: Algorithm developers creating conversational AI also face this dilemma. They must decide how to handle interruptions, and their choices have a wide-ranging impact on who gets heard in AI-mediated conversation. Developers gain from a smoother product but could harm marginalized group discussion.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Analyzing conversation norms reveals competing forces. A norm against interruption fosters inclusion, but strong civility can also suppress needed rapid clarification or dissent. The observer sees this as a tangled rope.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conversational_dogmas_interuption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conversational_dogmas_interuption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conversational_dogmas_interuption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conversational_dogmas_interuption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(conversational_dogmas_interuption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The conversational dominance by certain parties or viewpoints constitutes a real extraction from other participants. Suppression (0.60): Moderate-High. The norm against interruption, while intended to promote civility, actively suppresses dissenting voices and viewpoints. Some social pressure maintains anti-interruption norms.
 *
 * PERSPECTIVAL GAP:
 *   Low-status individuals experience conversation dogma as a snare, suppressing their voices. High-status participants benefit from it (rope). Algorithmic designers face a tangled rope dilemma: design for maximal civility or for a more raucous but potentially more inclusive discourse. A meta-observer sees the tangled dynamics: conflicting benefits of pro- and anti-interruption norms.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction flows from those whose contributions are marginalized to those who dominate the conversation. High-status participants and the algorithm developers (who benefit from wider acceptance) benefit from the suppression of marginalized viewpoints. The analytical observer sees the tug-of-war. The directionality of algorithm developers could be improved by implementing features to give low-status participants extra opportunity to contribute.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling coordination as pure extraction. There is a coordination element – the conversation itself. There also is extraction: the control of conversation by certain individuals/viewpoints, and the algorithmic control over whose viewpoint is heard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interruption_threshold,
    'What interruption rate crosses the line from beneficial contribution to extractive domination?',
    'Analyze conversation transcripts and participant feedback to identify the relationship between interruption frequency and perceived conversation quality.',
    'Determines whether anti-interruption policy is over- or under-correcting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interruption_threshold, empirical, 'Threshold for defining ''excessive'' interruptions.').

omega_variable(
    civility_definition,
    'What constitutes ''civil'' discourse, and how much does this definition vary across cultures and social groups?',
    'Ethnographic studies of conversation norms in different cultural contexts.',
    'Determines how interventions should be targeted; a one-size-fits-all approach may not be effective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civility_definition, conceptual, 'Definition of civil discourse for algorithm design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conversational_dogmas_interuption, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conv_tr_t0, conversational_dogmas_interuption, theater_ratio, 0, 0.2).
narrative_ontology:measurement(conv_tr_t5, conversational_dogmas_interuption, theater_ratio, 5, 0.3).
narrative_ontology:measurement(conv_tr_t10, conversational_dogmas_interuption, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(conv_be_t0, conversational_dogmas_interuption, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(conv_be_t5, conversational_dogmas_interuption, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(conv_be_t10, conversational_dogmas_interuption, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conversational_dogmas_interuption, information_standard).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, social_dominance_hierarchy).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, algorithmic_bias).

% DUAL FORMULATION NOTE:
% This constraint interacts with the broader social dominance hierarchy, which determines who has conversational power. Algorithmic design can either mitigate or exacerbate the biases inherent in the social dominance hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
