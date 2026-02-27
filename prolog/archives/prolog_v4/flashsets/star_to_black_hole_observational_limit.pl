% ============================================================================
% CONSTRAINT STORY: star_to_black_hole_observational_limit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_star_to_black_hole_observational_limit, []).

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
 *   constraint_id: star_to_black_hole_observational_limit
 *   human_readable: Observational Limit on Directly Observing Star-to-Black Hole Transformation
 *   domain: technological
 *
 * SUMMARY:
 *   The direct observation of a star collapsing into a black hole is
 *   extremely rare and difficult due to the speed of the event, obscuration
 *   by ejected material, and the inability to predict its occurrence.
 *   Observational astronomers are thus in a difficult position whereas
 *   theoretical astrophysicists have more freedom to theorize.
 *
 * KEY AGENTS:
 *   - Observational Astronomers: Primary target (powerless/trapped) — limited by technology and event rarity.
 *   - Theoretical Astrophysicists: Primary beneficiary (institutional/arbitrage) — benefits from the observational limits as it provides them with a wide space to model.
 *   - Analytical Observer: Sees interplay between theoretical modeling and observational limitations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(star_to_black_hole_observational_limit, 0.35).
domain_priors:suppression_score(star_to_black_hole_observational_limit, 0.4).
domain_priors:theater_ratio(star_to_black_hole_observational_limit, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, extractiveness, 0.35).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(star_to_black_hole_observational_limit, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(star_to_black_hole_observational_limit, tangled_rope).
narrative_ontology:human_readable(star_to_black_hole_observational_limit, "Observational Limit on Directly Observing Star-to-Black Hole Transformation").
narrative_ontology:topic_domain(star_to_black_hole_observational_limit, "technological").

domain_priors:requires_active_enforcement(star_to_black_hole_observational_limit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(star_to_black_hole_observational_limit, theoretical_astrophysicists).
narrative_ontology:constraint_victim(star_to_black_hole_observational_limit, observational_astronomers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The observational astronomers are trapped by the unpredictable nature of the events and technological limits, which makes direct observation extremely difficult and rare.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Theoretical astrophysicists benefit from the observational limits as it provides them with a wide space to model and hypothesize about the processes and phenomena around black hole formation, and refine their models when new (albeit rare) observational evidence becomes available.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Analytical perspective sees the interplay between theoretical modeling and observational limitations. New theories need observations but, if something is rarely observable, can lead to untestable models.
constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(star_to_black_hole_observational_limit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_to_black_hole_observational_limit, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(star_to_black_hole_observational_limit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.35) due to the difficulty in observing such events. Suppression is at 0.40 due to the technological constraints and inherent unpredictability of these events.
 *
 * PERSPECTIVAL GAP:
 *   The observational astronomers are limited by what they can observe, making it a snare for them. Theoretical astrophysicists, on the other hand, can benefit from the observational limits, seeing it as a rope. An analytical observer sees the balance between the two, leading to a tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Observational astronomers have no exit option, and are thus more severely affected. Theoretical astrophysicists have more flexibility. Analytical observer takes a broad civilizational perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rate_of_stellar_collapse,
    'What is the true rate of stellar collapse events in our observable universe, and how much of this rate is obscured by dust, gas, or distance?',
    'Advanced simulations incorporating population synthesis models, better accounting for dust extinction, and gravitational wave detection correlation.',
    'If the rate is higher, more observational opportunities exist (less snare). If the rate is lower, theoretical models are further unconstrained (more tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rate_of_stellar_collapse, empirical, 'Uncertainty around the rate of stellar collapse').

omega_variable(
    detection_technology_limits,
    'What are the ultimate technological limits on detecting short-duration electromagnetic and gravitational wave signals from stellar collapse?',
    'Continued development of next-generation telescopes, interferometers, and detectors coupled with advanced signal processing algorithms.',
    'If detection limits are significantly extended, transforms observational astronomy to more of a rope. If limits are reached, observational astronomers stay trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_technology_limits, empirical, 'Uncertainty around technological limits on detecting stellar collapse signals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(star_to_black_hole_observational_limit, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(star_tr_t0, star_to_black_hole_observational_limit, theater_ratio, 0, 0.1).
narrative_ontology:measurement(star_tr_t50, star_to_black_hole_observational_limit, theater_ratio, 50, 0.2).
narrative_ontology:measurement(star_tr_t100, star_to_black_hole_observational_limit, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(star_be_t0, star_to_black_hole_observational_limit, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(star_be_t50, star_to_black_hole_observational_limit, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(star_be_t100, star_to_black_hole_observational_limit, base_extractiveness, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(star_to_black_hole_observational_limit, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
