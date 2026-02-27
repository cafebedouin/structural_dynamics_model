% ============================================================================
% CONSTRAINT STORY: khantivadin_radical_patience
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_khantivadin_radical_patience, []).

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
 *   constraint_id: khantivadin_radical_patience
 *   human_readable: The Teacher of Patience (Khantivadin)
 *   domain: religious/ethical
 *
 * SUMMARY:
 *   This constraint models the state of radical patience (Khanti) in the face
 *   of absolute physical liquidation. This concept, exemplified by the
 *   Khantivadin, represents an ethical ideal of non-resistance even when
 *   facing torture and death. It posits that true patience transcends
 *   physical suffering, leading to inner peace and enlightenment.
 *
 * KEY AGENTS:
 *   - Khantivadin (the Teacher of Patience): Subjected to torture, embodies radical patience
 *   - Ethical/Religious System: Promotes non-violence and patience as a virtue
 *   - Analytical Observer: Detached observer analyzing the phenomenon of radical patience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(khantivadin_radical_patience, 0.01).
domain_priors:suppression_score(khantivadin_radical_patience, 0.01).
domain_priors:theater_ratio(khantivadin_radical_patience, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(khantivadin_radical_patience, extractiveness, 0.01).
narrative_ontology:constraint_metric(khantivadin_radical_patience, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(khantivadin_radical_patience, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(khantivadin_radical_patience, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(khantivadin_radical_patience, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(khantivadin_radical_patience, mountain).
narrative_ontology:human_readable(khantivadin_radical_patience, "The Teacher of Patience (Khantivadin)").
narrative_ontology:topic_domain(khantivadin_radical_patience, "religious/ethical").

domain_priors:emerges_naturally(khantivadin_radical_patience).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: From the viewpoint of the individual undergoing torture, radical patience is the only remaining choice. Escape is impossible, and the physical torment is inevitable. This lack of agency makes the constraint appear as a Mountain.
constraint_indexing:constraint_classification(khantivadin_radical_patience, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: From the perspective of an ethical or religious system promoting non-violence and patience, the Khantivadin's behavior is seen as an ideal standard. The system reinforces the idea of acceptance and non-resistance as a virtue, framing it as a natural consequence of understanding reality.
constraint_indexing:constraint_classification(khantivadin_radical_patience, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective 3: A detached observer might view the Khantivadin's radical patience as an intrinsic aspect of human resilience. The ability to maintain composure under extreme duress could be seen as a fundamental capacity of the human mind.
constraint_indexing:constraint_classification(khantivadin_radical_patience, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(khantivadin_radical_patience_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(khantivadin_radical_patience, ExtMetricName, E),
    domain_priors:suppression_score(khantivadin_radical_patience, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(khantivadin_radical_patience),
    narrative_ontology:constraint_metric(khantivadin_radical_patience, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(khantivadin_radical_patience, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(khantivadin_radical_patience_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The values for extractiveness, suppression, and theater_ratio are all minimal, reflecting the nature of radical patience as a perceived inevitability. Extractiveness is low as it represents the individual giving up agency rather than it being taken from them. Suppression is low as the ethical choice is seen as natural. The theater ratio is negligible, suggesting little performative action.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the situation as a Mountain. This is because radical patience, under these circumstances, is seen as an unchangeable and unavoidable response to external circumstances. Each actor views the scenario as a limit case or an inevitable situation.
 *
 * DIRECTIONALITY LOGIC:
 *   Since the constraint is classified as a Mountain from all perspectives, the beneficiaries and victims aren't necessary, and the directionality logic would not apply.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(khantivadin_radical_patience, 0, 1).

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
