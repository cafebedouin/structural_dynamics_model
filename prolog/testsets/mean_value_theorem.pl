% ============================================================================
% CONSTRAINT STORY: mean_value_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mean_value_theorem, []).

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
 *   constraint_id: mean_value_theorem
 *   human_readable: Application of the Mean Value Theorem
 *   domain: mathematics/technological
 *
 * SUMMARY:
 *   The Mean Value Theorem (MVT) — stating that for a continuous function
 *   differentiable on an open interval, there exists at least one point where
 *   the instantaneous rate of change equals the average rate of change —
 *   functions as a mathematical constraint on all systems that model
 *   continuous change. Unlike institutional or social constraints that vary
 *   with observer position, the MVT is universally binding: it holds
 *   regardless of the agent's power, exit options, or temporal perspective.
 *   Applications range from physics (relating average velocity to
 *   instantaneous velocity), to engineering (verifying measurement
 *   consistency), to pure mathematics (proving the fundamental theorem of
 *   calculus). The constraint is not imposed; it is an irreducible logical
 *   consequence of the underlying mathematical axioms. This makes it a
 *   canonical Mountain-type constraint: zero degrees of freedom,
 *   accessibility collapse ≥ 0.85 (agents cannot find alternative
 *   mathematical frameworks without contradicting axioms), resistance ≤ 0.15
 *   (no institutional power is wasted suppressing alternatives because
 *   alternatives do not exist).
 *
 * KEY AGENTS:
 *   - Analytical Mathematician (analytical/civilizational/analytical/universal) — observes the theorem as logical necessity
 *   - Engineer (powerful/generational/mobile/global) — applies MVT to real systems despite having power and mobility
 *   - Physics Community (organized/biographical/constrained/national) — cannot avoid instantiating MVT in measurement verification
 *   - Navigation Algorithm (moderate/immediate/trapped/local) — computationally instantiates MVT without choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mean_value_theorem, 0.08).
domain_priors:suppression_score(mean_value_theorem, 0.02).
domain_priors:theater_ratio(mean_value_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mean_value_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(mean_value_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(mean_value_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mean_value_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(mean_value_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mean_value_theorem, mountain).
narrative_ontology:human_readable(mean_value_theorem, "Application of the Mean Value Theorem").
narrative_ontology:topic_domain(mean_value_theorem, "mathematics/technological").

domain_priors:emerges_naturally(mean_value_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL MATHEMATICIAN (MOUNTAIN) — From the civilizational/universal view, the Mean Value Theorem is a mathematical necessity. For any continuous function differentiable on an open interval, there must exist at least one point where the instantaneous rate of change equals the average rate of change. This is an irreducible logical constraint — not a convention, not a social choice, but a consequence of the continuity and differentiability axioms. Zero degrees of freedom; applies universally.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER (MOUNTAIN) — Even agents with power and exit options cannot escape the MVT constraint. An engineer computing vehicle speed from distance and time records is constrained by the theorem's logic: the average speed (total distance / total time) guarantees that the instantaneous speed matched that average at some moment during the journey. This is not enforced by institutional power but by mathematical structure. The engineer cannot 'opt out' of having instantiated this constraint in their system.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: PHYSICS COMMUNITY (MOUNTAIN) — Organized actors in physics, even with constrained exit options, face the MVT as an immutable constraint on physical measurement. When verifying that a moving object traveled distance d in time t, the community cannot assign arbitrary velocity profiles. The MVT guarantees the existence of at least one moment where instantaneous velocity matched d/t. This is not a rule imposed by the community; it is a property of the physical systems being measured.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NAVIGATION SYSTEM (MOUNTAIN) — A GPS or dead-reckoning system computing position changes from timestamp sequences is structurally constrained by MVT. Even a computationally constrained, trapped agent (the algorithm has no alternative processing model) cannot avoid instantiating the MVT: the average velocity over a time interval guarantees the existence of an instantaneous velocity matching that average. The constraint operates identically whether the agent is aware of it or not.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mean_value_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(mean_value_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mean_value_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mean_value_theorem, ExtMetricName, E),
    domain_priors:suppression_score(mean_value_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(mean_value_theorem),
    narrative_ontology:constraint_metric(mean_value_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(mean_value_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(mean_value_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The MVT extracts nothing from any agent — it is a constraint that governs the logical structure of continuous systems, not a mechanism that transfers resources from one actor to another. The value is nonzero only to account for the minimal epistemic cost of verifying or applying the theorem (acknowledging that knowledge work has a small material footprint). Suppression (0.02): Negligible. There are no alternatives to suppress; the theorem follows necessarily from the axioms. No institutional power is required to maintain it; it maintains itself through logical necessity. Theater ratio (0.15): Very low. Application of MVT involves minimal performative activity. When an engineer uses MVT to verify a measurement or a physicist invokes it in a derivation, the activity is directly functional — the theorem is applied because it solves the problem, not because it maintains institutional legitimacy. The 0.15 reflects only the minimal formal presentation required to communicate the result.
 *
 * PERSPECTIVAL GAP:
 *   The Mean Value Theorem exhibits zero perspectival gap — all observers classify it identically as Mountain. This is the hallmark of a natural law constraint: the mathematical structure is invariant across all measurement contexts and observer positions. An analytical mathematician, an engineer, an organized community, and a computational algorithm all encounter the same constraint with the same logical force. The lack of perspectival disagreement is not a limitation but a feature: it demonstrates that the constraint is genuinely structural (not socially constructed) because it persists across all observation sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to Mountain constraints. There is no d-value (beneficiary/victim axis) because no agent benefits at another's expense. The MVT is symmetric: all agents are simultaneously bound by it, none are targets of extraction. The theorem governs the logical space, not the distribution of resources. The analytical observer correctly identifies this as universal, not indexed to any particular interest.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mean_value_theorem, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(mean_value_theorem, fundamental_theorem_calculus).
narrative_ontology:affects_constraint(mean_value_theorem, rolle_theorem_equivalence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
