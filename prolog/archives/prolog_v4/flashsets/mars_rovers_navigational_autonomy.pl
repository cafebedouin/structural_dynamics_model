% ============================================================================
% CONSTRAINT STORY: mars_rovers_navigational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mars_rovers_navigational_autonomy, []).

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
 *   constraint_id: mars_rovers_navigational_autonomy
 *   human_readable: Mars Surface Navigational Autonomy (AutoNav)
 *   domain: technological
 *
 * SUMMARY:
 *   The navigational autonomy of Mars rovers is a complex interplay between
 *   the desire for scientific efficiency and the inherent risks of operating
 *   in an unpredictable environment with limited communication. The signal
 *   latency acts as a fixed constraint, while the acceptable risk of hardware
 *   loss is a more flexible parameter. Different stakeholders experience the
 *   system differently based on their priorities and relationship to the
 *   mission.
 *
 * KEY AGENTS:
 *   - Science Mission Planners: Moderate/Constrained – benefit from increased data acquisition, but bear the risk of rover loss.
 *   - Rover Hardware Teams: Institutional/Arbitrage – prioritize hardware integrity, can adjust autonomy to manage risk.
 *   - Planetary Science Community: Powerless/Trapped – affected by mission success or failure.
 *   - Analytical Observer: Signal Latency (Mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mars_rovers_navigational_autonomy, 0.55).
domain_priors:suppression_score(mars_rovers_navigational_autonomy, 0.4).
domain_priors:theater_ratio(mars_rovers_navigational_autonomy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, extractiveness, 0.55).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(mars_rovers_navigational_autonomy, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mars_rovers_navigational_autonomy, tangled_rope).
narrative_ontology:human_readable(mars_rovers_navigational_autonomy, "Mars Surface Navigational Autonomy (AutoNav)").
narrative_ontology:topic_domain(mars_rovers_navigational_autonomy, "technological").

domain_priors:requires_active_enforcement(mars_rovers_navigational_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mars_rovers_navigational_autonomy, science_mission_planners).
narrative_ontology:constraint_beneficiary(mars_rovers_navigational_autonomy, rover_hardware_teams).
narrative_ontology:constraint_victim(mars_rovers_navigational_autonomy, planetary_science_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The planetary science community bears the risk of lost scientific opportunity due to rover failure from navigational errors. They are largely trapped, as they depend on the mission for data.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% Rover hardware teams benefit from maintaining control over navigation, ensuring hardware integrity and mission success. They can arbitrage risk management strategies.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Science mission planners benefit from autonomy by enabling more efficient data collection and exploration but are constrained by the risk of rover failure. They have limited exit options within the mission parameters.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Signal latency between Earth and Mars poses a fixed, physical constraint on real-time rover control, limiting the extent of direct human intervention in navigation.
constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mars_rovers_navigational_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mars_rovers_navigational_autonomy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mars_rovers_navigational_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mars_rovers_navigational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The system extracts from the planetary science community (potential lost data) to benefit the science mission planners and rover teams. Suppression (0.40): Moderate. Some suppression exists because the autonomy is intentionally limited to protect the rover, which can hinder immediate scientific goals. Theater Ratio (0.20): Low. Most of the system's processes are functional; only a small portion is performative (e.g., formal risk assessments).
 *
 * PERSPECTIVAL GAP:
 *   The planetary science community experiences the constraint as a Snare because they have little control over the rover's navigation but bear the cost of failure. The rover hardware teams view the constraint as a Rope because they can manage risk by limiting autonomy. Science Mission Planners view it as a tangled rope due to the balance of benefits and risk.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the degree to which each agent benefits or is harmed by the constraint. Rover hardware teams benefit from prioritizing rover safety (low d), while the planetary science community potentially suffers from limited exploration capability or complete loss of mission (high d). The science mission planners experience both benefits and risks, so their d value is moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction by accounting for the genuine need to protect expensive and irreplaceable hardware. While limiting autonomy might seem extractive from the perspective of scientific discovery, it is necessary for the long-term viability of the mission and future missions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rover_failure_cost,
    'What is the acceptable cost threshold for rover failure versus increased autonomy?',
    'Mission risk assessment models, technology validation experiments, fault-tolerance analysis.',
    'Higher threshold enables more aggressive autonomy, lower threshold necessitates higher degrees of human intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rover_failure_cost, preference, 'Quantifying tolerable loss from rover failure.').

omega_variable(
    perception_robustness,
    'What level of environmental uncertainty can the perception systems of the rover tolerate?',
    'Testing navigation algorithms in realistic simulated and field environments, evaluating robustness to different terrain types.',
    'More robust systems allow for greater autonomous decision-making, less robust systems require more human oversight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perception_robustness, empirical, 'Assessing perception system accuracy in varied terrain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mars_rovers_navigational_autonomy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mars_tr_t0, mars_rovers_navigational_autonomy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mars_tr_t5, mars_rovers_navigational_autonomy, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mars_tr_t10, mars_rovers_navigational_autonomy, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(mars_be_t0, mars_rovers_navigational_autonomy, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mars_be_t5, mars_rovers_navigational_autonomy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mars_be_t10, mars_rovers_navigational_autonomy, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mars_rovers_navigational_autonomy, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
