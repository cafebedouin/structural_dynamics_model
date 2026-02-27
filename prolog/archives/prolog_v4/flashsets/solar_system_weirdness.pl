% ============================================================================
% CONSTRAINT STORY: solar_system_weirdness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solar_system_weirdness, []).

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
 *   constraint_id: solar_system_weirdness
 *   human_readable: The Solar System Configuration Anomaly
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   For decades, astronomers assumed our solar system was a typical model for
 *   the universe. However, exoplanet surveys have revealed a wide diversity
 *   of planetary systems, many of which are strikingly different from our
 *   own. This has led to the realization that our solar system may be an
 *   anomaly, potentially biasing our understanding of planetary formation and
 *   evolution. This constraint explores that bias.
 *
 * KEY AGENTS:
 *   - Theoretical Modelers: Institutional, benefit from solar system models.
 *   - Exoplanet Survey Teams: Powerless, suffer bias from solar system models.
 *   - Analytical Observers: Analytical, evaluate the system.
 *   - New Generation of Observers: Organized, working to correct bias
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solar_system_weirdness, 0.3).
domain_priors:suppression_score(solar_system_weirdness, 0.4).
domain_priors:theater_ratio(solar_system_weirdness, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solar_system_weirdness, extractiveness, 0.3).
narrative_ontology:constraint_metric(solar_system_weirdness, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(solar_system_weirdness, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solar_system_weirdness, tangled_rope).
narrative_ontology:human_readable(solar_system_weirdness, "The Solar System Configuration Anomaly").
narrative_ontology:topic_domain(solar_system_weirdness, "technological/scientific").

domain_priors:requires_active_enforcement(solar_system_weirdness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(solar_system_weirdness, theoretical_modelers).
narrative_ontology:constraint_victim(solar_system_weirdness, exoplanet_survey_teams).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Exoplanet survey teams are trapped by the readily available data from our solar system, potentially biasing their analyses and hindering the discovery of truly novel planetary configurations. They face difficulty in deviating from established models. High extraction, low exit.
constraint_indexing:constraint_classification(solar_system_weirdness, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% Theoretical modelers benefit from the easily accessible data and well-established models of our solar system as a starting point for developing and refining planetary formation theories. They can publish based on adapting these models. Low extraction, high exit.
constraint_indexing:constraint_classification(solar_system_weirdness, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% From a civilizational perspective, the configuration of our solar system presents a complex interplay of coordination and extraction. It is a well-studied case that can be used for comparative planetology, but also potentially biases research. Moderate extraction and coordination.
constraint_indexing:constraint_classification(solar_system_weirdness, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% New, more comprehensive survey techniques (e.g., JWST, Roman Space Telescope) offer a chance to break free from the readily available bias.
constraint_indexing:constraint_classification(solar_system_weirdness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solar_system_weirdness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(solar_system_weirdness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solar_system_weirdness, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(solar_system_weirdness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction comes from the bias introduced, the coordination from the utility of the data, and the theater ratio is low as there is little performative action. The tangled rope classification comes from the mix of bias and coordination.
 *
 * PERSPECTIVAL GAP:
 *   Exoplanet survey teams feel trapped and biased. The theoretical modelers use this information effectively. The analytical observer sees both the benefit and detriments of our current position.
 *
 * DIRECTIONALITY LOGIC:
 *   The exoplanet survey teams are negatively affected and don't have a way to exit. The theoretical modelers benefit from solar system information.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sample_size_bias,
    'To what extent does the unique configuration of our solar system bias our understanding of planetary system formation?',
    'Statistical analysis of exoplanet systems to quantify the prevalence of similar configurations.',
    'If our solar system is statistically common, the bias is minimal. If it is rare, current theories may be significantly skewed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sample_size_bias, empirical, 'The degree to which the solar system biases planetary formation models.').

omega_variable(
    model_complexity,
    'Are current planetary formation models sufficiently complex to account for the diversity of observed exoplanet systems?',
    'Comparison of model predictions with observations of diverse exoplanet systems.',
    'If models are too simplistic, new physics may be required. If they are too complex, overfitting may obscure fundamental processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_complexity, conceptual, 'The suitability of planetary formation models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solar_system_weirdness, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sola_tr_t0, solar_system_weirdness, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sola_tr_t10, solar_system_weirdness, theater_ratio, 10, 0.2).
narrative_ontology:measurement(sola_tr_t20, solar_system_weirdness, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(sola_be_t0, solar_system_weirdness, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sola_be_t10, solar_system_weirdness, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(sola_be_t20, solar_system_weirdness, base_extractiveness, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solar_system_weirdness, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
