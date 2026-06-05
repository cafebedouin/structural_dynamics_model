% ============================================================================
% CONSTRAINT STORY: birthday_paradox_collison
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birthday_paradox_collison, []).

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
 *   constraint_id: birthday_paradox_collison
 *   human_readable: The Birthday Paradox (Collision Probability)
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   The Birthday Paradox is a mathematical principle demonstrating that in a
 *   set of n randomly chosen items, the probability of a collision (a shared
 *   property, like a birthday) reaches 50% with only sqrt(N) items, where N
 *   is the total number of possibilities. This has significant implications
 *   in computer science, cryptography, and statistics, where collisions can
 *   lead to vulnerabilities or unexpected behavior.
 *
 * KEY AGENTS:
 *   - Analytical Observer: understands and uses the principle.
 *   - Naive User: is surprised by the results, but cannot change them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birthday_paradox_collison, 0.1).
domain_priors:suppression_score(birthday_paradox_collison, 0.01).
domain_priors:theater_ratio(birthday_paradox_collison, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birthday_paradox_collison, extractiveness, 0.1).
narrative_ontology:constraint_metric(birthday_paradox_collison, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(birthday_paradox_collison, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(birthday_paradox_collison, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(birthday_paradox_collison, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birthday_paradox_collison, mountain).
narrative_ontology:human_readable(birthday_paradox_collison, "The Birthday Paradox (Collision Probability)").
narrative_ontology:topic_domain(birthday_paradox_collison, "mathematical/technological").

domain_priors:emerges_naturally(birthday_paradox_collison).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The birthday paradox is a mathematical truth, and from an analytical perspective, it's a fundamental limit.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% A user unaware of the paradox will be surprised by collisions, but the underlying mathematics remains a fixed constraint.
constraint_indexing:constraint_classification(birthday_paradox_collison, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birthday_paradox_collison_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(birthday_paradox_collison, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birthday_paradox_collison, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(birthday_paradox_collison, ExtMetricName, E),
    domain_priors:suppression_score(birthday_paradox_collison, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(birthday_paradox_collison),
    narrative_ontology:constraint_metric(birthday_paradox_collison, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(birthday_paradox_collison, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(birthday_paradox_collison_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The birthday paradox is a mathematical result. The extractiveness and suppression are both very low, because the result doesn't extract anything and doesn't suppress any choices. The theater ratio is also low, as there is no performance aspect.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives recognize the birthday paradox as a mathematical constraint. The analytical observer understands the underlying math, while the naive user might be initially surprised, but their experience does not change the underlying principle.
 *
 * DIRECTIONALITY LOGIC:
 *   The birthday paradox is a natural law, not an actively enforced constraint. There are no beneficiaries or victims in the traditional sense.
 *
 * MANDATROPHY ANALYSIS:
 *   The Birthday Paradox exemplifies a situation where a mathematical truth can be surprising or counter-intuitive, leading to potential misinterpretations if not properly understood. The classification as mountain highlights its nature as an immutable constraint, preventing mislabeling as a human-created snare or tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birthday_paradox_collison, 0, 100).

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
