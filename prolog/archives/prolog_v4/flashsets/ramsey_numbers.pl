% ============================================================================
% CONSTRAINT STORY: ramsey_numbers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ramsey_numbers, []).

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
 *   constraint_id: ramsey_numbers
 *   human_readable: Inevitable Order (Ramsey's Theorem)
 *   domain: mathematical
 *
 * SUMMARY:
 *   Ramsey's Theorem states that in any sufficiently large system where
 *   elements are partitioned into a finite number of classes, a large,
 *   orderly substructure must exist. It demonstrates the inevitability of
 *   order in sufficiently large systems. The theorem's existence demonstrates
 *   a limitation on pure disorder.
 *
 * KEY AGENTS:
 *   - The Unbound Combinatorialist: Primary target (powerless/trapped) — confronts limits of disorder
 *   - Mathematical Community: Primary beneficiary (institutional/analytical) — utilizes established truths
 *   - Analytical Observer: Sees the fundamental truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ramsey_numbers, 0.05).
domain_priors:suppression_score(ramsey_numbers, 0.02).
domain_priors:theater_ratio(ramsey_numbers, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ramsey_numbers, extractiveness, 0.05).
narrative_ontology:constraint_metric(ramsey_numbers, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ramsey_numbers, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ramsey_numbers, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ramsey_numbers, mountain).
narrative_ontology:human_readable(ramsey_numbers, "Inevitable Order (Ramsey's Theorem)").
narrative_ontology:topic_domain(ramsey_numbers, "mathematical").

domain_priors:emerges_naturally(ramsey_numbers).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a combinatorialist exploring the edges of order and disorder, Ramsey theory represents a fundamental limit on how much disorder one can create before order inevitably emerges.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% For the mathematical community, Ramsey's theorem stands as a non-negotiable result, a fact regardless of preference or immediate utility.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical perspective, Ramsey's theorem represents a fundamental limit on disorder, irrespective of human agency. It's an inherent property of sufficiently large systems.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ramsey_numbers_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ramsey_numbers, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ramsey_numbers, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ramsey_numbers, ExtMetricName, E),
    domain_priors:suppression_score(ramsey_numbers, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ramsey_numbers),
    narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ramsey_numbers, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ramsey_numbers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Very low, this is a foundational mathematical truth not a mechanism of extraction. Suppression (0.02): Very low, the theorem's structure is immutable. Theater Ratio (0.01): Very low, there is no performative element.
 *
 * PERSPECTIVAL GAP:
 *   There is no real perspectival gap; Ramsey's theorem is a Mountain from every vantage point.  All perspectives classify as Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is close to zero, as Ramsey's theorem is a foundaational truth, not a constraint. As a result, it benefits all parties and does not extract from them.
 *
 * MANDATROPHY ANALYSIS:
 *   Ramsey's theorem does not create a mandatrophy as it is universally true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ramsey_numbers, 0, 100).

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
