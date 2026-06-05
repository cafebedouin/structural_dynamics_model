% ============================================================================
% CONSTRAINT STORY: cuban_missile_crisis_excomm_delibration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cuban_missile_crisis_excomm_delibration, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cuban_missile_crisis_excomm_delibration
 *   human_readable: The ExComm Multi-Channel Deliberation Protocol
 *   domain: political/military
 *
 * SUMMARY:
 *   Following the failure of the Bay of Pigs, President Kennedy established
 *   the Executive Committee of the National Security Council (ExComm) to
 *   deliberate on the Cuban Missile Crisis. The ExComm Multi-Channel
 *   Deliberation Protocol involved open debate, dissenting opinions, and
 *   rigorous analysis from diverse experts. Kennedy's encouragement of
 *   diverse perspectives and structured deliberation is credited with helping
 *   avoid a nuclear war.
 *
 * KEY AGENTS:
 *   - President Kennedy: Primary beneficiary (powerful/mobile) - information arbitrage, control
 *   - ExComm Members: Secondary beneficiaries (institutional/constrained) - coordinated expertise
 *   - United States National Security: Beneficiary (institutional/analytical) - crisis prevention
 *   - Soviet Union: Indirect beneficiary (institutional/constrained) - avoidance of nuclear war (assumed mutual interest)
 *   - Analytical Observers: Analytical view (analytical/analytical) - positive coordination mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuban_missile_crisis_excomm_delibration, 0.35).
domain_priors:suppression_score(cuban_missile_crisis_excomm_delibration, 0.25).
domain_priors:theater_ratio(cuban_missile_crisis_excomm_delibration, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, extractiveness, 0.35).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_delibration, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cuban_missile_crisis_excomm_delibration, rope).
narrative_ontology:human_readable(cuban_missile_crisis_excomm_delibration, "The ExComm Multi-Channel Deliberation Protocol").
narrative_ontology:topic_domain(cuban_missile_crisis_excomm_delibration, "political/military").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, united_states_national_security).
narrative_ontology:constraint_beneficiary(cuban_missile_crisis_excomm_delibration, president_kennedy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the US National Security apparatus: Views the deliberation protocol as a vital coordination mechanism for navigating the crisis and preventing nuclear war. Analytical exit as historians and policymakers learn from this event.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of ExComm members: Experience the protocol as a mechanism to coordinate diverse opinions and expertise, despite constraints imposed by the urgency of the situation. Constrained because members were expected to participate actively.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% President Kennedy views the protocol as a tool for information arbitrage, enabling him to make informed decisions while maintaining control. Mobile because he has other advisors and avenues.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% An analytical observer sees the protocol as a positive coordination mechanism that facilitated effective decision-making and averted a catastrophic outcome.
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuban_missile_crisis_excomm_delibration_tests).
:- end_tests(cuban_missile_crisis_excomm_delibration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) Low-moderate extraction due to the coordination value. Suppression (0.25) - low suppression as multiple points of view were encouraged. Theater ratio (0.15) Low Theater as the function of the group was earnest.
 *
 * PERSPECTIVAL GAP:
 *   All actors view the protocol as a positive coordination mechanism, though to differing degrees.
 *
 * DIRECTIONALITY LOGIC:
 *   Kennedy benefits most directly through improved decision-making. ExComm benefits through coordinated effort. The US national security benefits from averted crisis. All experience it as rope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cuban_missile_crisis_excomm_delibration, 1962, 1962).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cuban_missile_crisis_excomm_delibration, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
