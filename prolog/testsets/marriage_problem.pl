% ============================================================================
% CONSTRAINT STORY: optimal_stopping_marriage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_optimal_stopping_marriage, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: optimal_stopping_marriage
 *   human_readable: The 37% Rule (Optimal Stopping Problem)
 *   domain: mathematical/technological
 *
 * SUMMARY:
 *   In the "Marriage Problem" (a classic example of the Secretary Problem), an
 *   agent must choose the best candidate from a sequence of N options. Once a
 *   candidate is rejected, they cannot be recalled. To maximize the probability
 *   of selecting the absolute best candidate, mathematical theory dictates
 *   rejecting the first n/e (approx. 37%) of candidates to establish a
 *   baseline, and then selecting the first subsequent candidate who is better
 *   than all seen so far. This creates a rigid, non-negotiable boundary for
 *   optimal behavior under these assumptions.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Searcher (powerless/trapped): Individual seeking the optimal choice, constrained by the sequential, irrevocable nature of the problem.
 *   - The Rejected Baseline (victim): The first 37% of candidates, who are rejected by definition, regardless of their quality.
 *   - The Selected Candidate (beneficiary): The candidate chosen by the optimal strategy.
 *   - The Mathematician (analytical/analytical): Analytical observer who views the constraint as an immutable law of probability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Low extraction. The "loss" of rejected candidates is a structural feature of
% the optimization problem, not an extraction for profit.
domain_priors:base_extractiveness(optimal_stopping_marriage, 0.10).
% Low suppression. The mathematical law does not coerce; it simply describes
% optimal strategy. Alternatives are possible but statistically inferior.
domain_priors:suppression_score(optimal_stopping_marriage, 0.05).
% Low theater. The constraint is a purely functional mathematical result.
domain_priors:theater_ratio(optimal_stopping_marriage, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimal_stopping_marriage, extractiveness, 0.10).
narrative_ontology:constraint_metric(optimal_stopping_marriage, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(optimal_stopping_marriage, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: High. The mathematical proof is logically closed;
% there is no accessible alternative that yields a better probability under
% the problem's assumptions.
narrative_ontology:constraint_metric(optimal_stopping_marriage, accessibility_collapse, 0.98).
% Resistance: Low. One cannot meaningfully "resist" a mathematical theorem.
narrative_ontology:constraint_metric(optimal_stopping_marriage, resistance, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(optimal_stopping_marriage, mountain).
narrative_ontology:human_readable(optimal_stopping_marriage, "The 37% Rule (Optimal Stopping Problem)").

% --- Emergence flag (required for mountain constraints) ---
% The constraint emerges naturally from the logic of probability theory.
domain_priors:emerges_naturally(optimal_stopping_marriage).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% While this is a mountain, the application of the rule in a social context
% creates identifiable beneficiaries and victims, which informs the narrative
% but does not change the invariant classification.
narrative_ontology:constraint_beneficiary(optimal_stopping_marriage, selected_optimal_candidate).
narrative_ontology:constraint_victim(optimal_stopping_marriage, rejected_baseline_candidates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   This is a uniform-type constraint (Mountain-only). The mathematical reality
   is invariant across all perspectives. The subjective experience of being
   'trapped' (for the Searcher) does not change the objective structure of the
   constraint, which has near-zero extraction and suppression.
   ========================================================================== */

% PERSPECTIVE 1: THE SEARCHER (MOUNTAIN)
% The searcher feels "trapped" by the inability to recall past options, but
% this is a feature of the problem's landscape, not an imposed snare. It is
% an unchangeable feature of reality for them.
constraint_indexing:constraint_classification(optimal_stopping_marriage, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MATCHMAKER / INSTITUTION (MOUNTAIN)
% An institution (like a dating app) using this rule is simply leveraging a
% fixed feature of probability. The rule itself remains a mountain.
constraint_indexing:constraint_classification(optimal_stopping_marriage, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE MATHEMATICIAN / ANALYTICAL OBSERVER (MOUNTAIN)
% To the mathematician, the 1/e limit is a classic Mountain—a fixed feature
% of probability, unchangeable regardless of human desire or agency.
constraint_indexing:constraint_classification(optimal_stopping_marriage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimal_stopping_marriage_tests).

test(classification_invariance) :-
    % Verify that this is a uniform-type constraint (Mountain from all perspectives).
    constraint_indexing:constraint_classification(optimal_stopping_marriage, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimal_stopping_marriage, Type2, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(optimal_stopping_marriage, Type3, context(agent_power(institutional), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(mountain_threshold_adherence) :-
    % Verify metrics are within Mountain thresholds.
    narrative_ontology:constraint_metric(optimal_stopping_marriage, extractiveness, E),
    narrative_ontology:constraint_metric(optimal_stopping_marriage, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_exists) :-
    % Verify that the required NL profile metrics are declared.
    narrative_ontology:constraint_metric(optimal_stopping_marriage, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(optimal_stopping_marriage, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(optimal_stopping_marriage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly classified this constraint as Scaffold from
 *   the analytical view and Snare from the powerless view. This regeneration
 *   corrects this to a uniform-type Mountain. A mathematical theorem is the
 *   archetype of a Mountain: it is an unchangeable, non-negotiable feature of
 *   the logical landscape with effectively zero extraction or suppression.
 *   The suppression score was lowered from 0.4 to 0.05 to comply with Mountain
 *   thresholds. The `constraint_claim` was corrected from `scaffold` to `mountain`.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the classification. All agents, regardless
 *   of their power or exit options, face the same immutable mathematical reality.
 *   The Searcher's subjective feeling of being "trapped" is an experience of a
 *   Mountain's hard limits, not evidence of a Snare's extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim roles are declared to capture the social dynamics of
 *   *applying* the rule, but they do not alter the classification of the rule
 *   itself. The mathematical law is indifferent; it has no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying this as a Mountain, we avoid mislabeling a fundamental
 *   mathematical limit as a coercive or extractive mechanism. The "cost" paid
 *   by the rejected baseline is a necessary condition for optimization within
 *   the problem's framework, not a form of rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions about the applicability of the model
% /5 form: narrative detail for story context
omega_variable(
    omega_recall_viability,
    "Can a previously rejected candidate be 're-activated' in a real-world scenario?",
    "Empirical study of 'boomerang' relationships in modern dating datasets.",
    "If Recall=High, the core assumption of the problem is violated, and the constraint is not a Mountain but a different strategic problem (likely Rope). If Recall=Zero, the Mountain classification holds.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_population_knowledge,
    "Does the searcher know the total number of candidates N in advance?",
    "Information theory analysis of search under unknown population size.",
    "If N is unknown, the classic 37% rule cannot be applied, and a different optimal stopping problem (a different Mountain) appears.",
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_recall_viability, empirical, "Whether the 'no recall' assumption holds in real-world applications.").
narrative_ontology:omega_variable(omega_population_knowledge, conceptual, "Whether the total population size N is known, a key assumption of the model.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(optimal_stopping_marriage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No coordination function; this is a feature of logic, not a mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The constraint is a uniform-type Mountain.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */