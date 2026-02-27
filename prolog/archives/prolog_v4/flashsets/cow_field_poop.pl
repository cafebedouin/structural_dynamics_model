% ============================================================================
% CONSTRAINT STORY: cow_field_poop
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-03-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cow_field_poop, []).

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
 *   constraint_id: cow_field_poop
 *   human_readable: The Cow Field Hazard (Pragmatic Avoidance)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint models the presence of 'poop' (negative events, social
 *   hazards, or nonsense) as an inherent and diverse feature of existence. It
 *   is largely perceived as immutable due to the complexity and scale of
 *   systems, and limited individual agency. All perspectives converge on a
 *   mountain classification.
 *
 * KEY AGENTS:
 *   - The Unwary Individual: Experiences the constraint locally with limited power (powerless/trapped)
 *   - Systems-Level Observer: Analyzes 'poop' as a system-level property (institutional/analytical)
 *   - Analytical Observer: Views 'poop' from a civilizational, analytical perspective (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cow_field_poop, 0.15).
domain_priors:suppression_score(cow_field_poop, 0.02).
domain_priors:theater_ratio(cow_field_poop, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cow_field_poop, extractiveness, 0.15).
narrative_ontology:constraint_metric(cow_field_poop, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(cow_field_poop, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cow_field_poop, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(cow_field_poop, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cow_field_poop, mountain).
narrative_ontology:human_readable(cow_field_poop, "The Cow Field Hazard (Pragmatic Avoidance)").
narrative_ontology:topic_domain(cow_field_poop, "social/psychological").

domain_priors:emerges_naturally(cow_field_poop).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Unwary Individual. Encounters the 'poop' (negative events) as an unavoidable aspect of their immediate environment. Limited power and exit options reinforce the perception of an immutable mountain.
constraint_indexing:constraint_classification(cow_field_poop, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: Systems-Level Observer. Views 'poop' as an inevitable outcome of complex adaptive systems, whether social, economic, or ecological. A long-term, broad perspective acknowledges inherent messiness and irreducible noise.
constraint_indexing:constraint_classification(cow_field_poop, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 3: Analytical Observer (Natural Law View). From a civilizational/universal perspective, the existence of unpredictable or undesirable events is an inevitable aspect of complex systems and bounded rationality.
constraint_indexing:constraint_classification(cow_field_poop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cow_field_poop_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cow_field_poop, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cow_field_poop, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cow_field_poop, ExtMetricName, E),
    domain_priors:suppression_score(cow_field_poop, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cow_field_poop),
    narrative_ontology:constraint_metric(cow_field_poop, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cow_field_poop, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cow_field_poop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low as there is minimal systematic extraction occurring. Suppression is also low since avoidance is typically voluntary. The theater ratio is low because the action being performed does not hide any underlying function.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this scenario. All agents, regardless of power or exit options, perceive the existence of 'poop' as an inherent property of the world around them.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is neutral as there are no explicit beneficiaries or victims. All agents are merely observers of an unavoidable aspect of existence.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not applicable here as the constraint is claimed to be a mountain. There is no possibility of it being misclassified as pure extraction or coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cow_field_poop, 0, 100).

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
