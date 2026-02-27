% ============================================================================
% CONSTRAINT STORY: burali_forte_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burali_forte_paradox, []).

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
 *   constraint_id: burali_forte_paradox
 *   human_readable: Burali-Forti Paradox
 *   domain: technological/mathematics
 *
 * SUMMARY:
 *   The Burali-Forti paradox highlights a fundamental contradiction within
 *   naive set theory, demonstrating that the collection of all ordinal
 *   numbers cannot be a set. This has significant implications for the
 *   foundations of mathematics and the development of axiomatic set theory.
 *
 * KEY AGENTS:
 *   - Mathematical Logician: (analytical/analytical) - Understands the formal implications of the paradox.
 *   - Naive Set Theorist: (powerless/analytical) - May encounter the paradox without understanding its formal proof.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burali_forte_paradox, 0.05).
domain_priors:suppression_score(burali_forte_paradox, 0.01).
domain_priors:theater_ratio(burali_forte_paradox, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burali_forte_paradox, extractiveness, 0.05).
narrative_ontology:constraint_metric(burali_forte_paradox, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(burali_forte_paradox, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(burali_forte_paradox, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burali_forte_paradox, mountain).
narrative_ontology:human_readable(burali_forte_paradox, "Burali-Forti Paradox").
narrative_ontology:topic_domain(burali_forte_paradox, "technological/mathematics").

domain_priors:emerges_naturally(burali_forte_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The paradox represents a fundamental limit on set theory, regardless of the logician's actions.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Even someone with a very limited understanding of mathematics will eventually encounter contradictions if they assume the collection of all ordinals is a set.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burali_forte_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(burali_forte_paradox, ExtMetricName, E),
    domain_priors:suppression_score(burali_forte_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(burali_forte_paradox),
    narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(burali_forte_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(burali_forte_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Burali-Forti paradox is a fundamental limitation on set theory, indicating it is a mountain. Extractiveness and suppression are minimal as it is a logical constraint that does not involve coercion.
 *
 * PERSPECTIVAL GAP:
 *   Since the paradox is a fundamental limitation, both perspectives classify it as a mountain. There is no meaningful perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no beneficiaries or victims as the paradox is a natural limitation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burali_forte_paradox, 0, 100).

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
