% ============================================================================
% CONSTRAINT STORY: st_petersburg_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_st_petersburg_paradox, []).

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
 *   constraint_id: st_petersburg_paradox
 *   human_readable: St. Petersburg Paradox (Divergent Expected Value)
 *   domain: mathematical/economic
 *
 * SUMMARY:
 *   The St. Petersburg Paradox highlights a divergence between the
 *   theoretical expected value of a game and the amount people are actually
 *   willing to pay to play it. The game involves flipping a coin until tails
 *   appears, with the payoff doubling for each successive heads. The expected
 *   value is infinite, yet people typically offer only a small amount to
 *   play. This paradox illustrates limitations in expected utility theory and
 *   the importance of risk aversion and diminishing marginal utility. The St.
 *   Petersburg Paradox is a conceptual mountain arising from a mathematical
 *   and economic perspective.
 *
 * KEY AGENTS:
 *   - The Impartial Analyst: Sees a mathematical consequence
 *   - The Naive Gambler: Faces a practically risky proposition
 *   - The Actuary: Grapples with the divergence between theory and consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(st_petersburg_paradox, 0.15).
domain_priors:suppression_score(st_petersburg_paradox, 0.01).
domain_priors:theater_ratio(st_petersburg_paradox, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(st_petersburg_paradox, extractiveness, 0.15).
narrative_ontology:constraint_metric(st_petersburg_paradox, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(st_petersburg_paradox, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(st_petersburg_paradox, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(st_petersburg_paradox, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(st_petersburg_paradox, mountain).
narrative_ontology:human_readable(st_petersburg_paradox, "St. Petersburg Paradox (Divergent Expected Value)").
narrative_ontology:topic_domain(st_petersburg_paradox, "mathematical/economic").

domain_priors:emerges_naturally(st_petersburg_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a purely analytical standpoint, the divergent expected value of the St. Petersburg game is a mathematical consequence of the payoff structure. No amount of capital can change the mathematical expectation. The paradox highlights a fundamental tension between mathematical expectation and human decision-making under uncertainty.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even for someone with very limited resources and immediate needs, the theoretical expectation of the St. Petersburg game is infinite, which poses a cognitive constraint. However, the extremely low probability of receiving the large payouts makes it a very risky proposition in practice.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% From the viewpoint of someone calculating insurance or fair value of risky assets, the SPP highlights the divergence between abstract math and practical consequences. The theoretical infinite expectation is an asymptote: a property that can be approached but never truly reached.
constraint_indexing:constraint_classification(st_petersburg_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(st_petersburg_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(st_petersburg_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(st_petersburg_paradox, ExtMetricName, E),
    domain_priors:suppression_score(st_petersburg_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(st_petersburg_paradox),
    narrative_ontology:constraint_metric(st_petersburg_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(st_petersburg_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(st_petersburg_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core issue is the divergent expected value. The extractiveness and suppression are very low because this is a thought experiment revealing a fundamental limit of applying mathematical models to real-world decisions. The emergent nature is self-evident given the game rules.
 *
 * PERSPECTIVAL GAP:
 *   Each agent can experience the constraint in slightly different ways. An individual with no capital would likely view the gamble as not worthwhile despite the extremely high potential value. However, an actuary may view the game as being nearly valueless as the expectation is more theoretical than practical. All still classify as mountains.
 *
 * DIRECTIONALITY LOGIC:
 *   The analytical observer, the powerless gambler and the sophisticated financial analyst all perceive the underlying mathematical/economic truth, though each may react differently. The key feature is the universal recognition of the math behind the paradox. Since all parties see the math, there isn't extraction from any perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(st_petersburg_paradox, 0, 100).

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
