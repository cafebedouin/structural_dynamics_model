% ============================================================================
% CONSTRAINT STORY: reciprocity_laws_math
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reciprocity_laws_math, []).

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
 *   constraint_id: reciprocity_laws_math
 *   human_readable: Mathematical Reciprocity Laws (Quadratic)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Reciprocity laws, beginning with Gauss's Quadratic Reciprocity, describe
 *   a deep, non-obvious symmetry in the behavior of prime numbers. They are
 *   fundamental properties of number theory, arising from the structure of
 *   integers and modular arithmetic. There are no beneficiaries or victims;
 *   these laws simply exist as mathematical truths.
 *
 * KEY AGENTS:
 *   - Analytical Observer: (analytical/analytical) perceives the inherent symmetry.
 *   - Novice Mathematician: (powerless/analytical) learns and applies the fixed rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reciprocity_laws_math, 0.01).
domain_priors:suppression_score(reciprocity_laws_math, 0.01).
domain_priors:theater_ratio(reciprocity_laws_math, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, extractiveness, 0.01).
narrative_ontology:constraint_metric(reciprocity_laws_math, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(reciprocity_laws_math, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reciprocity_laws_math, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(reciprocity_laws_math, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reciprocity_laws_math, mountain).
narrative_ontology:human_readable(reciprocity_laws_math, "Mathematical Reciprocity Laws (Quadratic)").
narrative_ontology:topic_domain(reciprocity_laws_math, "mathematical/logical").

domain_priors:emerges_naturally(reciprocity_laws_math).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, the reciprocity laws are a fundamental property of number theory, and therefore a mountain. The existence of these laws does not depend on human institutions or actions.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% To a novice mathematician, the reciprocity laws are a fixed rule that must be learned and applied. While the underlying reason for these rules may not be immediately apparent, they function as a fixed law.
constraint_indexing:constraint_classification(reciprocity_laws_math, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reciprocity_laws_math_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reciprocity_laws_math, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reciprocity_laws_math, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reciprocity_laws_math, ExtMetricName, E),
    domain_priors:suppression_score(reciprocity_laws_math, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reciprocity_laws_math),
    narrative_ontology:constraint_metric(reciprocity_laws_math, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reciprocity_laws_math, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reciprocity_laws_math_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are near zero, as these are inherent mathematical laws. The theater ratio is also near zero, reflecting the minimal performative content associated with their application. The classification is mountain because they are fixed and immutable properties.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap, because the underlying structure imposes the same classification from most perspectives. The existence and validity of these laws are independent of perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no beneficiaries or victims, so directionality is irrelevant. The inherent symmetry implies no extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The primary concern is distinguishing between mathematical laws and human institutions. Reciprocity laws are purely mathematical, and their truth does not depend on human action or belief. The lack of any asymmetry avoids any mandatrophy concern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reciprocity_laws_math, 0, 100).

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
