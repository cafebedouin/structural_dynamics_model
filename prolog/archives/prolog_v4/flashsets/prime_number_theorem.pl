% ============================================================================
% CONSTRAINT STORY: prime_number_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prime_number_theorem, []).

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
 *   constraint_id: prime_number_theorem
 *   human_readable: Prime Number Theorem (Asymptotic Density)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Prime Number Theorem (PNT) describes the asymptotic distribution of
 *   prime numbers among the positive integers. It states that the probability
 *   that a given, randomly selected number is prime is inversely proportional
 *   to the number of digits in that number (i.e., to its logarithm). The PNT
 *   is a fundamental result in number theory with implications for
 *   cryptography and other areas. Since it is a fundamental mathematical
 *   theorem, it is properly classified as a mountain.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Analyzes the inherent properties of prime numbers.
 *   - Mathematical Community: Accepts and utilizes the PNT as a foundational result.
 *   - Beginning Student: Learns the PNT as a given truth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_number_theorem, 0.01).
domain_priors:suppression_score(prime_number_theorem, 0.01).
domain_priors:theater_ratio(prime_number_theorem, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_number_theorem, extractiveness, 0.01).
narrative_ontology:constraint_metric(prime_number_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(prime_number_theorem, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(prime_number_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prime_number_theorem, mountain).
narrative_ontology:human_readable(prime_number_theorem, "Prime Number Theorem (Asymptotic Density)").
narrative_ontology:topic_domain(prime_number_theorem, "mathematical").

domain_priors:emerges_naturally(prime_number_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, the PNT is a fundamental mathematical truth, reflecting an inherent property of the distribution of prime numbers. The proof and theorem itself is independent of any particular agent or application. No exit is possible as it is a mathematical necessity.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The mathematical community accepts the PNT as a foundational result. It's a building block for further research and understanding in number theory. No agent has the power to change it.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The beginning student learns the PNT as a given truth to be accepted. The student cannot change the truth of the PNT. It is an immutable part of mathematics.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_number_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(prime_number_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_number_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(prime_number_theorem, ExtMetricName, E),
    domain_priors:suppression_score(prime_number_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(prime_number_theorem),
    narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prime_number_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prime_number_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The PNT is a mathematical theorem with no known exceptions, and no apparent way to circumvent it. Base extractiveness and suppression are near zero, as this is simply a statement of objective mathematical fact. The theorem emerges naturally from the properties of prime numbers themselves. The accessibility collapse is high, meaning that is a very strong result that is widely accepted by the community. The resistance is quite low, showing that there is no viable alternative
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap because the PNT is a mathematical theorem and is universally accepted in mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   Since the Prime Number Theorem is a mathematical truth, it does not have beneficiaries or victims. Therefore, directionality is not a factor in its classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The PNT is a fundamental mathematical truth and hence cannot be misclassified. There is no real need for a mandatrophy analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_number_theorem, 0, 1000).

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
