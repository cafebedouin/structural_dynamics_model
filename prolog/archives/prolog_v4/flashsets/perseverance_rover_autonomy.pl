% ============================================================================
% CONSTRAINT STORY: perseverance_rover_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perseverance_rover_autonomy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perseverance_rover_autonomy
 *   human_readable: Perseverance Rover Autonomy
 *   domain: technological
 *
 * SUMMARY:
 *   The Perseverance rover's autonomous exploration capability on Mars
 *   presents a constraint on direct, real-time human control. This autonomy
 *   allows the rover to make decisions about navigation, sample selection,
 *   and data collection without immediate input from Earth-based operators,
 *   who are limited by signal delays. While increasing efficiency and the
 *   volume of data collected, this autonomy reduces direct human oversight
 *   and potentially limits the range of scientific questions that can be
 *   addressed.
 *
 * KEY AGENTS:
 *   - Earth-Based Rover Operators: Primary target (powerless/trapped) - Experience a loss of direct control over the rover.
 *   - NASA Science Team: Primary beneficiary (moderate/constrained) - Benefits from increased data collection speed but is also limited by the AI's choices.
 *   - AI Developers: Beneficiary (institutional/arbitrage) - Use the mission to test and improve AI algorithms.
 *   - Analytical Observer: (analytical/analytical) - Sees both benefits and drawbacks of the autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perseverance_rover_autonomy, 0.55).
domain_priors:suppression_score(perseverance_rover_autonomy, 0.65).
domain_priors:theater_ratio(perseverance_rover_autonomy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perseverance_rover_autonomy, extractiveness, 0.55).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perseverance_rover_autonomy, tangled_rope).
narrative_ontology:human_readable(perseverance_rover_autonomy, "Perseverance Rover Autonomy").
narrative_ontology:topic_domain(perseverance_rover_autonomy, "technological").

domain_priors:requires_active_enforcement(perseverance_rover_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perseverance_rover_autonomy, nasa_science_team).
narrative_ontology:constraint_beneficiary(perseverance_rover_autonomy, ai_developers).
narrative_ontology:constraint_victim(perseverance_rover_autonomy, earth_based_rover_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Earth-based rover operators experience a loss of direct control. The autonomous system dictates exploration to some extent, limiting direct, real-time input. They are trapped because of signal delay and reliance on the autonomous system.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% The science team benefits from increased exploration speed and efficiency due to autonomy. However, they are constrained by the AI's decision-making, requiring adaptation to its directives. They benefit from the increased data but must adapt to the rover's autonomous choices. Their exit option is constrained because they cannot abandon the autonomous exploration entirely without significantly impacting the mission's scope and speed.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% AI developers see the autonomy as a coordination mechanism to test and improve AI algorithms for space exploration, contributing to future missions. The AI Development Community benefits directly from the data collected and the opportunity to refine their algorithms and expertise. They have an arbitrage exit option because they can apply their skills to other projects without significantly impacting their overall career or the development of the field.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% An analytical observer recognizes the autonomy as a mixed blessing. It increases data acquisition but may reduce human understanding and serendipitous discoveries. Overall, there is a trade-off between human control and automated exploration efficiency.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_rover_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perseverance_rover_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(perseverance_rover_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): The autonomous system extracts direct control and real-time decision-making from the earth-based operators, although it increases the data acquisition rate for the science team. Suppression (0.65): The reliance on AI suppresses the ability of human operators to make real-time adjustments based on unexpected findings. The suppression comes from the signal delay between Earth and Mars, which makes immediate human intervention impractical. Theater Ratio (0.30): The low theater ratio indicates that the autonomous system is primarily functional, with limited performative or ceremonial aspects.
 *
 * PERSPECTIVAL GAP:
 *   The Earth-based operators see the system as a snare because they lose direct control. The science team sees it as a tangled rope because they gain data but lose some control over the direction of exploration. The AI developers see it as a rope because it allows them to test and improve their algorithms, but this also depends on the other agents relinquishing some control. The analytical observer attempts to balance the scientific gains against the loss of the potential for human-directed discovery.
 *
 * DIRECTIONALITY LOGIC:
 *   Earth based operators are the victims with no escape, as they are powerless and trapped by distance. NASA science team has limited escape as they are constrained to follow the parameters of the mission. The AI development community can apply their algorithms in many other domains, so they benefit, with an arbitrage exit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_oversight_threshold,
    'What level of human oversight is needed to maintain scientific integrity and prevent unintended consequences?',
    'Compare data quality and incidence of anomalies under different oversight protocols.',
    'Insufficient oversight leads to data corruption; excessive oversight negates efficiency gains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_oversight_threshold, empirical, 'Optimal level of human oversight in autonomous rover operations.').

omega_variable(
    algorithm_bias,
    'To what extent does the rover''s algorithm introduce bias in data collection and analysis?',
    'Analyze datasets for representational imbalances; test algorithm against diverse simulated environments.',
    'High bias skews scientific understanding; low bias increases accuracy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_bias, empirical, 'Potential bias introduced by the rover''s algorithms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perseverance_rover_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, perseverance_rover_autonomy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(pers_tr_t5, perseverance_rover_autonomy, theater_ratio, 5, 0.25).
narrative_ontology:measurement(pers_tr_t10, perseverance_rover_autonomy, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, perseverance_rover_autonomy, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(pers_be_t5, perseverance_rover_autonomy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(pers_be_t10, perseverance_rover_autonomy, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perseverance_rover_autonomy, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
