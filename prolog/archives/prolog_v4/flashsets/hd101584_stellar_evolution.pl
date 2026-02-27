% ============================================================================
% CONSTRAINT STORY: hd101584_stellar_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hd101584_stellar_evolution, []).

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
 *   constraint_id: hd101584_stellar_evolution
 *   human_readable: The Gravitational Dynamics of the HD101584 Stellar System
 *   domain: physical
 *
 * SUMMARY:
 *   The HD101584 system, consisting of a dying red giant and a smaller
 *   companion star, demonstrates the immutable laws of physics governing
 *   gravitational dynamics. This system's evolution and interactions serve as
 *   an observable case study, reflecting the core tenets of gravitational
 *   physics and stellar evolution.
 *
 * KEY AGENTS:
 *   - Red Giant: Primary actor, subject to gravitational laws.
 *   - Companion Star: Secondary actor, interacting gravitationally with the red giant.
 *   - Analytical Observer:  Understands and models the gravitational interaction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hd101584_stellar_evolution, 0.15).
domain_priors:suppression_score(hd101584_stellar_evolution, 0.02).
domain_priors:theater_ratio(hd101584_stellar_evolution, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, extractiveness, 0.15).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hd101584_stellar_evolution, mountain).
narrative_ontology:human_readable(hd101584_stellar_evolution, "The Gravitational Dynamics of the HD101584 Stellar System").
narrative_ontology:topic_domain(hd101584_stellar_evolution, "physical").

domain_priors:emerges_naturally(hd101584_stellar_evolution).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The gravitational interaction between the stars in the HD101584 system follows the laws of physics, and is thus classified as a mountain.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Astronomers observe and model the system based on the fundamental laws of physics.
constraint_indexing:constraint_classification(hd101584_stellar_evolution, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hd101584_stellar_evolution_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hd101584_stellar_evolution, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, ExtMetricName, E),
    domain_priors:suppression_score(hd101584_stellar_evolution, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hd101584_stellar_evolution),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hd101584_stellar_evolution, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hd101584_stellar_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The gravitational interaction is a fundamental force, and any observation or intervention is constrained by this force. The low extractiveness and suppression reflects the fundamental nature of the interaction.
 *
 * PERSPECTIVAL GAP:
 *   Both the analytical observer and the astronomer perceive the interaction as a mountain, because it is a fundamental law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hd101584_stellar_evolution, 0, 100).

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
