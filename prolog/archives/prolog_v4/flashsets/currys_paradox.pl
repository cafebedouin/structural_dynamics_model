% ============================================================================
% CONSTRAINT STORY: currys_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currys_paradox, []).

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
 *   constraint_id: currys_paradox
 *   human_readable: Curry's Paradox
 *   domain: analytical/logic
 *
 * SUMMARY:
 *   Curry's Paradox is a logical result that proves any arbitrary claim using
 *   a self-referential sentence of the form "If this sentence is true, then X
 *   is true." Unlike the Liar Paradox, it does not require negation, making
 *   it a fundamental threat to naive set theory and logics with unrestricted
 *   comprehension. The paradox reveals a fundamental limitation in formal
 *   systems if not carefully addressed.
 *
 * KEY AGENTS:
 *   - Naive Theorem Prover: Cannot derive sound results in the presence of the paradox (powerless/trapped)
 *   - Formal System Designer: Must constrain systems to avoid the paradox (institutional/analytical)
 *   - Logician: Analyzes the paradox and its implications for logic (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currys_paradox, 0.1).
domain_priors:suppression_score(currys_paradox, 0.01).
domain_priors:theater_ratio(currys_paradox, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currys_paradox, extractiveness, 0.1).
narrative_ontology:constraint_metric(currys_paradox, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(currys_paradox, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(currys_paradox, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currys_paradox, mountain).
narrative_ontology:human_readable(currys_paradox, "Curry's Paradox").
narrative_ontology:topic_domain(currys_paradox, "analytical/logic").

domain_priors:emerges_naturally(currys_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A naive theorem prover is trapped by the paradox, unable to derive meaningful results.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The formal system designer must account for Curry's Paradox to ensure consistency.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The logician recognizes Curry's Paradox as a fundamental constraint on formal systems.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currys_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(currys_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currys_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(currys_paradox, ExtMetricName, E),
    domain_priors:suppression_score(currys_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(currys_paradox),
    narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(currys_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(currys_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Curry's paradox highlights a necessary limitation in logical systems. The extractiveness and suppression are low as it reveals a constraint rather than actively extracting or suppressing agents. The theater ratio is also low due to its fundamental nature.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify Curry's Paradox as a Mountain, indicating that it is an immutable constraint on formal systems, regardless of the agent's power or exit options. The differing labels reflect the specific role the agent plays within that constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no explicit beneficiaries or victims as the paradox reveals a necessary logical constraint. The formal system designer benefits from understanding the paradox (lower effective extraction), whereas the naive theorem prover is rendered ineffective (slightly higher extractiveness).
 *
 * MANDATROPHY ANALYSIS:
 *   N/A: As a mountain, no mandatrophy analysis is necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currys_paradox, 0, 1).

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
