% ============================================================================
% CONSTRAINT STORY: cap_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cap_theorem, []).

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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cap_theorem
 *   human_readable: CAP Theorem (Brewer's Theorem)
 *   domain: distributed_systems/computer_science
 *
 * SUMMARY:
 *   The CAP theorem, or Brewer's theorem, is a fundamental principle in
 *   distributed computing. It states that any distributed data store can only
 *   provide two of three guarantees simultaneously: Consistency (every read
 *   receives the most recent write or an error), Availability (every request
 *   receives a non-error response, without guarantee that it contains the
 *   most recent write), and Partition Tolerance (the system continues to
 *   operate despite an arbitrary number of messages being dropped or delayed
 *   by the network between nodes). Since network partitions are a given in
 *   real-world distributed systems, the choice is effectively between
 *   Consistency and Availability (CP vs. AP). This is not a policy choice but
 *   a logical impossibility, making it a classic example of a Mountain
 *   constraint.
 *
 * KEY AGENTS:
 *   - System Architects: Analytical agents who use the theorem as a foundational design principle.
 *   - Startup Founders: Powerless agents (relative to the theorem) who must build their systems within its constraints.
 *   - Cloud Providers: Institutional agents who offer services that embody different CAP trade-offs, but cannot escape the theorem itself.
 *   - End Users: Moderate agents who experience the downstream effects of the design choices mandated by the theorem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cap_theorem, 0.02).
domain_priors:suppression_score(cap_theorem, 0.01).
domain_priors:theater_ratio(cap_theorem, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cap_theorem, extractiveness, 0.02).
narrative_ontology:constraint_metric(cap_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(cap_theorem, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cap_theorem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(cap_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cap_theorem, mountain).
narrative_ontology:human_readable(cap_theorem, "CAP Theorem (Brewer's Theorem)").
narrative_ontology:topic_domain(cap_theorem, "distributed_systems/computer_science").

domain_priors:emerges_naturally(cap_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SYSTEM ARCHITECT (MOUNTAIN) — The theorem is a fundamental, unchangeable law of the design space for distributed systems. It cannot be exited, only analyzed and designed around. This is the canonical analytical view.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE STARTUP FOUNDER (MOUNTAIN) — A resource-constrained actor is completely trapped by the theorem's logic. They cannot build a system that violates it, regardless of their goals or resources. It is an immutable environmental constraint.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE CLOUD PROVIDER (MOUNTAIN) — An institutional actor with vast resources still cannot violate the theorem. Their 'exit' is arbitrage: offering different services that make different trade-offs (e.g., a CP database vs. an AP one), but the underlying constraint remains fixed.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE END USER (MOUNTAIN) — The user experiences the consequences of the trade-offs (e.g., stale data vs. an error message), but the underlying constraint is a fixed limit. Their ability to switch services doesn't change the theorem's validity.
constraint_indexing:constraint_classification(cap_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cap_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(cap_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cap_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cap_theorem, ExtMetricName, E),
    domain_priors:suppression_score(cap_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(cap_theorem),
    narrative_ontology:constraint_metric(cap_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(cap_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(cap_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it represents a fundamental, logically derived limit. Extractiveness (ε=0.02) and Suppression (0.01) are near zero because the theorem does not extract value or coerce behavior; it merely describes the possible state space. It emerges naturally (true) from the properties of distributed networks. Resistance (0.05) is minimal as the theorem is formally proven. Accessibility Collapse (0.95) is high; once the terms are understood, the trade-off becomes self-evident and is a cornerstone of distributed systems education. There is no theater (0.0).
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a key diagnostic feature of a true Mountain constraint. All observers, regardless of their power, exit options, or time horizon, converge on the same classification. The theorem is an invariant feature of the environment for all actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is symmetric and non-extractive, so there are no beneficiaries or victims. The directionality `d` for any agent will be derived from canonical fallbacks based on their power level. However, with a base extractiveness (ε) of 0.02, the effective extraction (χ) is negligible (≈0) for all perspectives, reinforcing the Mountain classification across the board.
 *
 * MANDATROPHY ANALYSIS:
 *   The CAP theorem serves as a perfect baseline for identifying true Mountain constraints. It prevents the mandatrophy of misclassifying a fundamental limit as a contingent policy choice or an extractive system. Any attempt to frame the CAP trade-off as a 'snare' imposed by cloud providers, for example, would be immediately falsified by the structural metrics. It is a non-negotiable feature of reality, not an artificial rule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cap_theorem, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cap_theorem, eventual_consistency).
narrative_ontology:affects_constraint(cap_theorem, acid_compliance).

% DUAL FORMULATION NOTE:
% The CAP theorem is an upstream logical constraint that necessitates the creation of downstream coordination patterns like 'eventual consistency' to manage the trade-offs it imposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
