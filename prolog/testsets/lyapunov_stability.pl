% ============================================================================
% CONSTRAINT STORY: lyapunov_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lyapunov_stability, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lyapunov_stability
 *   human_readable: Lyapunov Stability Criteria
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   Lyapunov stability criteria define a fundamental constraint in the theory
 *   of dynamical systems: the behavior of trajectories near equilibrium
 *   points and the existence of functions whose decrease along trajectories
 *   certifies stability. This constraint is invariant across all mathematical
 *   frameworks and physical domains where differential equations apply.
 *   Unlike empirical constraints that emerge from institutional arrangements
 *   or strategic interactions, Lyapunov stability is a mathematical necessity
 *   — it follows from the definitions of equilibrium and continuity in
 *   dynamical systems. No agent, institution, or community can escape,
 *   suppress, or redefine it. The constraint's accessibility collapse is high
 *   (≥0.85): the mathematical formulation is precise and published; the core
 *   theorems (Lyapunov's direct and indirect methods) are universally known.
 *   Resistance is minimal (≤0.15): no meaningful institutional inertia,
 *   denial, or performance theater maintains this constraint — it persists
 *   because the mathematics is correct and universally applicable.
 *
 * KEY AGENTS:
 *   - Physical Systems: Governed entirely by differential equations — no agency, no exit, no choice in obeying Lyapunov stability.
 *   - Engineering Community: Organized agents applying Lyapunov theory to control design — experience the criteria as a hard constraint on design space, not an extractive mechanism.
 *   - Analytical Observer: The mathematical/theoretical perspective — sees Lyapunov criteria as a logical necessity, not a contingent institutional feature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lyapunov_stability, 0.12).
domain_priors:suppression_score(lyapunov_stability, 0.03).
domain_priors:theater_ratio(lyapunov_stability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lyapunov_stability, extractiveness, 0.12).
narrative_ontology:constraint_metric(lyapunov_stability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(lyapunov_stability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lyapunov_stability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(lyapunov_stability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lyapunov_stability, mountain).
narrative_ontology:human_readable(lyapunov_stability, "Lyapunov Stability Criteria").
narrative_ontology:topic_domain(lyapunov_stability, "mathematical/physical").

domain_priors:emerges_naturally(lyapunov_stability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL SYSTEM (MOUNTAIN) — A dynamical system governed by differential equations has no choice in whether it obeys Lyapunov stability criteria. The constraint is inherent to the mathematics describing the system's evolution. No agent can modify or escape this fundamental property — it is a structural feature of how systems respond to perturbations. Zero degrees of freedom.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEERING COMMUNITY (MOUNTAIN) — Control engineers cannot design systems that violate Lyapunov stability. The criterion is a hard constraint on what is physically possible: no amount of engineering ingenuity can make an unstable equilibrium stable without altering the system's fundamental dynamics. Organized agents can apply the criteria to design, but cannot escape or suppress them.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a mathematical standpoint, Lyapunov stability is a logically necessary consequence of the definitions of equilibrium and perturbation in dynamical systems theory. The criterion cannot be relaxed, reinterpreted, or circumvented through clever mathematics — it follows from first principles. Universal applicability across all well-defined dynamical systems.
constraint_indexing:constraint_classification(lyapunov_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lyapunov_stability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lyapunov_stability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lyapunov_stability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lyapunov_stability, ExtMetricName, E),
    domain_priors:suppression_score(lyapunov_stability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lyapunov_stability),
    narrative_ontology:constraint_metric(lyapunov_stability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lyapunov_stability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lyapunov_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. No agent extracts value from other agents by controlling access to Lyapunov criteria. The mathematical result is freely published and equally available to all practitioners. The 0.12 reflects minimal structural measurement noise and the trivial fact that some engineers have easier access to computational tools than others — but this is not extraction via the Lyapunov constraint itself, it is a separate inequality about computational resources. Suppression (0.03): Minimal. There are no meaningful barriers to using or verifying Lyapunov stability criteria. The mathematics is taught in standard textbooks; computational implementations are open-source; the theory is not gatekept. The 0.03 reflects only the universal feature that some humans find mathematics harder to learn than others. Theater ratio (0.15): Very low. Using Lyapunov stability to certify control system design is functional, not performative. Either the system is stable (satisfies Lyapunov) or it is not. There is no meaningful divergence between appearance and reality, no ritual performance masking lack of function. The slight theater (0.15 rather than 0.0) reflects that published proofs sometimes omit routine computational steps, requiring readers to verify them independently — but this is epistemic completeness, not institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap — all three perspectives produce identical mountain classifications. The physical system obeys Lyapunov stability invariantly. The engineering community experiences it as an invariant constraint on design. The analytical observer sees it as a logical necessity. This uniformity is a key signature of a true mountain: every agent perceives it identically because no agent has structural power over it. The convergence of all perspectives on Mountain classification is not weakness in the framework — it is the correct reading of a mathematical invariant.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY: Lyapunov stability criteria are uniform-type (mountain-only) across all perspectives. The mandate is singular: systems either satisfy or violate the Lyapunov conditions based on their mathematical structure. There is no tension between 'this appears to be coordination' (beneficiary narrative) and 'this is actually extraction' (victim narrative) because the constraint has no beneficiaries or victims — it is a pure mathematical fact. The analytical observer's mountain is verified, not a false summit: the constraint genuinely emerges from first principles in dynamical systems theory, not from institutional arrangement. This exemplifies the structural difference between mountains (mathematical/logical limits) and snares (institutional extraction disguised as natural law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lyapunov_function_existence,
    'For a given stable equilibrium, does a Lyapunov function always exist, or only sometimes?',
    'Converse theorems in stability theory (Kurzweil, Yoshizawa); examination of whether every stable system admits a Lyapunov function under standard regularity assumptions',
    'If always exists: stability concept is perfectly complete (true mountain). If sometimes fails: there are stable systems the criteria cannot fully capture (reveals hidden contingency, not pure mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lyapunov_function_existence, conceptual, 'Existence of Lyapunov functions for stable equilibria').

omega_variable(
    computational_accessibility,
    'How difficult is it in practice to find a Lyapunov function for a system known to be stable?',
    'Complexity analysis of Lyapunov function construction algorithms; empirical study of failure rates for systems known to be stable but lacking explicit Lyapunov functions',
    'If finding functions is tractable: the mountain is accessible to practitioners. If NP-hard or otherwise intractable: mountain exists but is epistemically hidden — extractive asymmetry emerges between those who can and cannot construct proofs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_accessibility, empirical, 'Computational tractability of Lyapunov function discovery').

omega_variable(
    nonlinear_stability_frontier,
    'Do the standard Lyapunov definitions fully capture stability for highly nonlinear systems, or do they miss important stability phenomena?',
    'Study of chaotic systems with stable manifolds; investigation of whether Lyapunov exponents capture all relevant stability behavior in deterministic nonlinear systems',
    'If fully captures: mountain extends to all nonlinear regimes. If misses phenomena: additional structural constraints exist that Lyapunov alone cannot describe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonlinear_stability_frontier, empirical, 'Completeness of Lyapunov criteria for nonlinear systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lyapunov_stability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lyap_tr_t0, lyapunov_stability, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lyap_tr_t50, lyapunov_stability, theater_ratio, 50, 0.14).
narrative_ontology:measurement(lyap_tr_t100, lyapunov_stability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(lyap_be_t0, lyapunov_stability, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(lyap_be_t50, lyapunov_stability, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(lyap_be_t100, lyapunov_stability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lyapunov_stability, information_standard).
narrative_ontology:affects_constraint(lyapunov_stability, bifurcation_analysis).
narrative_ontology:affects_constraint(lyapunov_stability, chaos_theory_determinism).
narrative_ontology:affects_constraint(lyapunov_stability, control_system_robustness).

% DUAL FORMULATION NOTE:
% Lyapunov stability is a foundational constraint in dynamical systems theory. Related constraints (bifurcation analysis, chaos characterization, robust control design) inherit their mathematical foundations from Lyapunov criteria. These downstream constraints may exhibit higher extractiveness (through computational complexity, experimental verification difficulty, or institutional gatekeeping) even though their mathematical core rests on the mountain of Lyapunov stability itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
