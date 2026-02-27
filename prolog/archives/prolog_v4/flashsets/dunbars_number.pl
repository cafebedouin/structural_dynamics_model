% ============================================================================
% CONSTRAINT STORY: dunbars_number
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dunbars_number, []).

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
 *   constraint_id: dunbars_number
 *   human_readable: Dunbar's Number (Cognitive Limit)
 *   domain: social/biological
 *
 * SUMMARY:
 *   This constraint models Dunbar's number as a biological cognitive limit on
 *   the number of stable social relationships an individual can maintain
 *   (approx. 150). While the exact number is debated, the existence of a
 *   cognitive limit on social group size is generally accepted in social and
 *   evolutionary biology. This limit influences the structure of human social
 *   organizations.
 *
 * KEY AGENTS:
 *   - Individual: Faces a cognitive limit on the number of relationships.
 *   - Analytical Observer: Observes the limit as a fixed parameter.
 *   - Large Organizations: Must account for this limit in their design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dunbars_number, 0.15).
domain_priors:suppression_score(dunbars_number, 0.05).
domain_priors:theater_ratio(dunbars_number, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dunbars_number, extractiveness, 0.15).
narrative_ontology:constraint_metric(dunbars_number, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(dunbars_number, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dunbars_number, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dunbars_number, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dunbars_number, mountain).
narrative_ontology:human_readable(dunbars_number, "Dunbar's Number (Cognitive Limit)").
narrative_ontology:topic_domain(dunbars_number, "social/biological").

domain_priors:emerges_naturally(dunbars_number).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The cognitive limit feels like a hard constraint to the individual.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% From an analytical perspective, the cognitive limit is a relatively fixed parameter of human social organization.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Large organizations need to account for limitations on individual social bandwidth. These limits are considered fixed.
constraint_indexing:constraint_classification(dunbars_number, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dunbars_number_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(dunbars_number, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunbars_number, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dunbars_number, ExtMetricName, E),
    domain_priors:suppression_score(dunbars_number, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dunbars_number),
    narrative_ontology:constraint_metric(dunbars_number, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dunbars_number, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dunbars_number_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because Dunbar's number primarily acts as a capacity limit rather than an extractive force. Suppression is low as individuals can attempt to maintain more relationships, though these may be weaker or less stable. Theater ratio is low because the observed social structures are genuinely influenced by the cognitive limit, not merely performing to it. Accessibility collapse is high as the limit naturally emerges in social group sizes across different contexts. Resistance is low as conscious efforts to bypass the limit are often unsuccessful in creating the same level of social cohesion.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the constraint as a mountain due to its perceived fixed nature. Different perspectives highlight the limit's impact, but the fundamental constraint remains constant.
 *
 * DIRECTIONALITY LOGIC:
 *   Since the constraint is primarily a mountain, there are no clear beneficiaries or victims. The cognitive limit affects everyone, but there is no extraction or coercion involved. The directionality is therefore neutral.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dunbars_number, 0, 100).

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
