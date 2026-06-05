% ============================================================================
% CONSTRAINT STORY: compounding_logic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compounding_logic, []).

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
 *   constraint_id: compounding_logic
 *   human_readable: The Law of Compounding Returns
 *   domain: economic
 *
 * SUMMARY:
 *   Compounding is the mathematical process where the value of a system
 *   increases exponentially because earnings are reinvested to generate
 *   further earnings. This is a fundamental principle in economics,
 *   particularly in the context of investment and wealth accumulation. Due to
 *   the mathematical nature and universal applicability of the law, it is
 *   considered a 'mountain' constraint.
 *
 * KEY AGENTS:
 *   - The Impoverished: Target (powerless/trapped) – Subject to the widening wealth gap exacerbated by compounding.
 *   - Financial Institutions: Beneficiary (institutional/analytical) – Leverage the law of compounding for growth and profit.
 *   - Analytical Observer: Observer (analytical/analytical) -  Understands the mathematical inevitability of compounding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compounding_logic, 0.15).
domain_priors:suppression_score(compounding_logic, 0.01).
domain_priors:theater_ratio(compounding_logic, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compounding_logic, extractiveness, 0.15).
narrative_ontology:constraint_metric(compounding_logic, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(compounding_logic, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(compounding_logic, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(compounding_logic, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compounding_logic, mountain).
narrative_ontology:human_readable(compounding_logic, "The Law of Compounding Returns").
narrative_ontology:topic_domain(compounding_logic, "economic").

domain_priors:emerges_naturally(compounding_logic).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of someone starting with zero capital, the law of compounding returns is a mountain: an insurmountable force leading to ever-increasing wealth inequality. While not strictly 'trapped,' the starting disadvantage presents a near-immovable barrier.
constraint_indexing:constraint_classification(compounding_logic, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% Financial institutions, understanding the law of compounding returns, see it as a fundamental principle governing economic growth and investment. They can analyze and leverage it but not alter its fundamental nature.
constraint_indexing:constraint_classification(compounding_logic, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The law of compounding returns, viewed analytically across a long time horizon and universal scope, is a mountain. It is a mathematical inevitability that wealth will accumulate faster for those who already have it.
constraint_indexing:constraint_classification(compounding_logic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compounding_logic_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(compounding_logic, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compounding_logic, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(compounding_logic, ExtMetricName, E),
    domain_priors:suppression_score(compounding_logic, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(compounding_logic),
    narrative_ontology:constraint_metric(compounding_logic, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(compounding_logic, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(compounding_logic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): The inherent advantage of compounding creates a wealth asymmetry, but the core principle is not inherently extractive; extraction in economic systems is driven by other factors. Suppression (0.01): The principle of compounding is not suppressing alternatives; it is a fundamental mathematical concept. Theater Ratio (0.05): There is minimal performative behavior associated with the basic compounding principle. It is a direct mathematical effect.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspective is mountain, though the powerless agent may experience it as a negative constraint (snare).
 *
 * DIRECTIONALITY LOGIC:
 *   The analytical perspective is neutral (analytical/analytical), the institutional perspective is beneficiary (institutional/analytical). The powerless perspective experiences the outcome of extraction in a wealth system where compounding widens wealth gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by identifying that the experience is fundamentally mathematical and structural as applied to wealth. While the effects may be socially adverse, the structure itself is a fundamental property of exponential growth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compounding_logic, 0, 100).

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
