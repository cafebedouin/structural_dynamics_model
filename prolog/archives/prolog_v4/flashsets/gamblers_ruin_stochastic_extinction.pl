% ============================================================================
% CONSTRAINT STORY: gamblers_ruin_stochastic_extinction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gamblers_ruin_stochastic_extinction, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gamblers_ruin_stochastic_extinction
 *   human_readable: Gambler's Ruin
 *   domain: mathematical/economic
 *
 * SUMMARY:
 *   Gambler's Ruin is a statistical theorem stating that a gambler with
 *   finite wealth, playing a fair or negative-expectation game against an
 *   opponent with effectively infinite wealth (the "House"), will eventually
 *   go bankrupt with a probability approaching 1. This is a mathematical
 *   inevitability.
 *
 * KEY AGENTS:
 *   - Individual Gambler: powerless/trapped
 *   - The House (Casino): institutional/analytical
 *   - Analytical Observer: analytical/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gamblers_ruin_stochastic_extinction, 0.15).
domain_priors:suppression_score(gamblers_ruin_stochastic_extinction, 0.05).
domain_priors:theater_ratio(gamblers_ruin_stochastic_extinction, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, extractiveness, 0.15).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gamblers_ruin_stochastic_extinction, mountain).
narrative_ontology:human_readable(gamblers_ruin_stochastic_extinction, "Gambler's Ruin").
narrative_ontology:topic_domain(gamblers_ruin_stochastic_extinction, "mathematical/economic").

domain_priors:emerges_naturally(gamblers_ruin_stochastic_extinction).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The individual gambler, even if unaware of the theorem, is ultimately subject to its statistical inevitability.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The house benefits from the theorem's consequences, understanding the statistical advantage over time.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% An objective observer recognizes Gambler's Ruin as a mathematical certainty within the specified conditions.
constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gamblers_ruin_stochastic_extinction_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gamblers_ruin_stochastic_extinction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, ExtMetricName, E),
    domain_priors:suppression_score(gamblers_ruin_stochastic_extinction, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gamblers_ruin_stochastic_extinction),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gamblers_ruin_stochastic_extinction, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gamblers_ruin_stochastic_extinction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low as the theorem describes a probabilistic outcome rather than a direct extraction. Suppression is also low as the gambler is free to choose whether or not to gamble. Theater ratio is zero as there is no performative aspect to the theorem. The theorem emerges naturally from the mathematics of probability.
 *
 * PERSPECTIVAL GAP:
 *   All agents, from the powerless gambler to the powerful house and the analytical observer, ultimately perceive the theorem as a mathematical certainty, leading to a Mountain classification from all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   While the House benefits from the theorem in the long run, the theorem itself isn't an active extraction mechanism. It's a probabilistic description of outcomes. Therefore, the perspective is largely the same from different agents.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gamblers_ruin_stochastic_extinction, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
