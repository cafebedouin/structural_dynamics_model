% ============================================================================
% CONSTRAINT STORY: lorenz_attractor_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lorenz_attractor_dynamics, []).

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
 *   constraint_id: lorenz_attractor_dynamics
 *   human_readable: Lorenz Attractor (Deterministic Chaos)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   The Lorenz attractor, derived from a simplified model of atmospheric
 *   convection, demonstrates deterministic chaos, where seemingly random
 *   behavior arises from deterministic equations. This system is a prime
 *   example of how simple mathematical models can exhibit complex and
 *   unpredictable dynamics.
 *
 * KEY AGENTS:
 *   - System Itself: Primary actor (powerless/trapped) — exhibits inherent dynamics.
 *   - Researchers/Modelers: Secondary actors (analytical/analytical) — observe and analyze the system.
 *   - Applications (weather forecasting, etc.): Beneficiaries (moderate/mobile) - utilizes models derived from the core principles.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lorenz_attractor_dynamics, 0.15).
domain_priors:suppression_score(lorenz_attractor_dynamics, 0.02).
domain_priors:theater_ratio(lorenz_attractor_dynamics, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, extractiveness, 0.15).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lorenz_attractor_dynamics, mountain).
narrative_ontology:human_readable(lorenz_attractor_dynamics, "Lorenz Attractor (Deterministic Chaos)").
narrative_ontology:topic_domain(lorenz_attractor_dynamics, "mathematical/physical").

domain_priors:emerges_naturally(lorenz_attractor_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Lorenz attractor's dynamics are deterministic and follow fixed equations, irrespective of observation.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% From a mathematical and physics perspective, the Lorenz attractor represents a natural phenomenon governed by inherent laws.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The Lorenz attractor's mathematical structure and chaotic behavior are fundamental properties, independent of human influence or manipulation.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lorenz_attractor_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(lorenz_attractor_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lorenz_attractor_dynamics),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lorenz_attractor_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. The Lorenz attractor exists independent of observation or manipulation. Suppression (0.02): Extremely low. No coercion or suppression is involved; it's a mathematical and physical system. Theater ratio (0.01): Minimal. The system is primarily functional, with little performative activity.
 *
 * PERSPECTIVAL GAP:
 *   Since it's a mountain classification from all perspectives, there isn't a perspectival gap. All observers, regardless of power or exit options, recognize it as a fixed and unchangeable phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   The dynamics of the Lorenz attractor are intrinsic, not driven by external agency or structural relations, resulting in directionality being a negligible factor. Observers seek to understand the underlying equations, rather than extract anything from the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lorenz_attractor_dynamics, 0, 100).

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
