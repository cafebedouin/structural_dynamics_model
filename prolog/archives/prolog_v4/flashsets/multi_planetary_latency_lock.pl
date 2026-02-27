% ============================================================================
% CONSTRAINT STORY: multi_planetary_latency_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_multi_planetary_latency_lock, []).

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
 *   constraint_id: multi_planetary_latency_lock
 *   human_readable: Earth-Centric Interplanetary Clearing
 *   domain: technological/economic
 *
 * SUMMARY:
 *   As civilization expands to Mars, the speed of light creates an
 *   irreducible communication latency (3-22 minutes). This latency
 *   fundamentally constraints certain clearing operations (financial,
 *   logistical, governance) to remain Earth-centric, as human reaction time
 *   is much faster than inter-planetary data transfer.
 *
 * KEY AGENTS:
 *   - Martian Colonist: Primary target (powerless/trapped) — experiences latency
 *   - Earth-based Corporations: Beneficiary (institutional/constrained) — able to act faster in certain markets
 *   - Analytical Observer: Sees the full physics of the situation (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(multi_planetary_latency_lock, 0.15).
domain_priors:suppression_score(multi_planetary_latency_lock, 0.03).
domain_priors:theater_ratio(multi_planetary_latency_lock, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(multi_planetary_latency_lock, extractiveness, 0.15).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(multi_planetary_latency_lock, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(multi_planetary_latency_lock, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(multi_planetary_latency_lock, mountain).
narrative_ontology:human_readable(multi_planetary_latency_lock, "Earth-Centric Interplanetary Clearing").
narrative_ontology:topic_domain(multi_planetary_latency_lock, "technological/economic").

domain_priors:emerges_naturally(multi_planetary_latency_lock).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The speed of light delay is immutable. Trapped by physics.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Earth-based corporations will always have a delay relative to Mars. Even with orbital relays, there will be some delay. This is a physical limitation.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The speed of light is a fundamental physical constant. Information cannot travel faster than light.
constraint_indexing:constraint_classification(multi_planetary_latency_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(multi_planetary_latency_lock_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(multi_planetary_latency_lock, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, ExtMetricName, E),
    domain_priors:suppression_score(multi_planetary_latency_lock, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(multi_planetary_latency_lock),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(multi_planetary_latency_lock, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(multi_planetary_latency_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint is a physical limit, not a mechanism of exploitation. Suppression is low because while the constraint is real, it doesn't actively prevent alternatives, it simply makes them slower. The theater ratio is low because there is little performative activity associated with this constraint, it's just the speed of light.
 *
 * PERSPECTIVAL GAP:
 *   All parties experience the constraint as a physical limitation. There is no real disagreement on the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Martian colonist is the primary target, experiencing the latency. Earth-based entities benefit slightly from the faster reaction time to real-time events. The analytical observer sees the immutable physical law.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(multi_planetary_latency_lock, 0, 100).

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
