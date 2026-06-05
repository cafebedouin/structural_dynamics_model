% ============================================================================
% CONSTRAINT STORY: liar_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liar_paradox, []).

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
 *   constraint_id: liar_paradox
 *   human_readable: The Liar Paradox (Self-Referential Inconsistency)
 *   domain: logic/epistemology
 *
 * SUMMARY:
 *   The Liar Paradox arises from the self-referential statement 'This
 *   statement is false.' It presents a fundamental inconsistency in systems
 *   that allow self-reference and a classical truth predicate. The paradox is
 *   not easily avoided without fundamentally changing the underlying logical
 *   system. This story considers its nature as a fundamental limit.
 *
 * KEY AGENTS:
 *   - Naive Truth Bearer: Individual encountering the paradox and attempting to assign a truth value
 *   - Formal Logician: Analyzing the paradox within a logical framework
 *   - Mathematical Foundation: Perspective viewing the paradox as an inherent limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liar_paradox, 0.05).
domain_priors:suppression_score(liar_paradox, 0.01).
domain_priors:theater_ratio(liar_paradox, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liar_paradox, extractiveness, 0.05).
narrative_ontology:constraint_metric(liar_paradox, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(liar_paradox, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(liar_paradox, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liar_paradox, mountain).
narrative_ontology:human_readable(liar_paradox, "The Liar Paradox (Self-Referential Inconsistency)").
narrative_ontology:topic_domain(liar_paradox, "logic/epistemology").

domain_priors:emerges_naturally(liar_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Liar sentence leads to contradiction within a naive conception of truth. The agent accepting the liar sentence is trapped in inconsistency.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% A formal logician, recognizing the paradox, sees it as an inherent limitation within a logical system that allows self-reference and classical truth predicates. This is an immutable constraint of such systems.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The liar paradox is viewed as a hard constraint to any formal system that has the capability of self-reference, making it a mathematical limit to such systems.
constraint_indexing:constraint_classification(liar_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liar_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(liar_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liar_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(liar_paradox, ExtMetricName, E),
    domain_priors:suppression_score(liar_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(liar_paradox),
    narrative_ontology:constraint_metric(liar_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(liar_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(liar_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low as it's a hard constraint on any system attempting self-reference and a classical truth predicate. The suppression is also very low, since alternative logical systems can be made without the said features.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives agree on the mountain nature of the constraint, stemming from its nature as a fixed point in logic/epistemology.
 *
 * DIRECTIONALITY LOGIC:
 *   Since this constraint is a Mountain, beneficiaries/victims are less relevant. The primary driver of directionality is the analytical understanding of the inherent limitations imposed by the paradox itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This is fundamentally not a coordination problem misclassified as extraction, or vice versa. The paradox represents an inherent limitation, regardless of how it is framed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liar_paradox, 0, 1000).

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
