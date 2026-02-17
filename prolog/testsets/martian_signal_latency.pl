% ============================================================================
% CONSTRAINT STORY: martian_signal_latency
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_martian_signal_latency, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: martian_signal_latency
 *   human_readable: Martian Signal Latency (One-Way Light Time)
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   Martian signal latency is the absolute delay in communication between Earth
 *   and Mars caused by the finite speed of light. Ranging from 3 to 22 minutes
 *   one-way depending on orbital positions, this constraint dictates the
 *   architecture of all deep-space missions, rendering real-time teleoperation
 *   of assets impossible. It is a fundamental, non-negotiable property of
 *   the physical universe.
 *
 * KEY AGENTS (by structural relationship):
 *   - Rover Driver (JPL): Primary target (powerless/trapped) — cannot act in
 *     real-time; must operate asynchronously.
 *   - Mission Architect (NASA): Analytical observer (institutional/arbitrage) —
 *     designs systems (e.g., rover autonomy) to operate within the constraint.
 *   - Radio Physicist: Analytical observer (analytical/analytical) — measures
 *     the constraint as a fundamental property of spacetime.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(martian_signal_latency, 0.05).
domain_priors:suppression_score(martian_signal_latency, 0.0).
domain_priors:theater_ratio(martian_signal_latency, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(martian_signal_latency, extractiveness, 0.05).
narrative_ontology:constraint_metric(martian_signal_latency, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(martian_signal_latency, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(martian_signal_latency, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(martian_signal_latency, mountain).
narrative_ontology:human_readable(martian_signal_latency, "Martian Signal Latency (One-Way Light Time)").

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the physics of the universe.
% Required for the mountain metric gate.
domain_priors:emerges_naturally(martian_signal_latency).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain (natural law), this constraint has no direct beneficiaries or
% victims in the structural sense. Human adaptations to it (e.g., autonomy
% software) are separate constraints (Ropes) that are affected by this one.
% No enrichment needed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it is a fundamental law of physics.
% The perspectival minimum (powerless/institutional) is relaxed.

% PERSPECTIVE 1: THE ROVER DRIVER (MOUNTAIN)
% Agent who directly experiences the operational impossibility of real-time control.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MISSION ARCHITECT (MOUNTAIN)
% Agent who must design systems around this immutable fact.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage), % Can 'exit' by designing more autonomy
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The physicist measuring a fundamental constant of nature.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE FUTURE MARS COLONIST (MOUNTAIN)
% An individual whose entire life is structured by this communication barrier.
constraint_indexing:constraint_classification(martian_signal_latency, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(martian_signal_latency_tests).

test(type_invariance) :-
    % Verify that as a natural law, it is a Mountain from all perspectives.
    constraint_indexing:constraint_classification(martian_signal_latency, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(martian_signal_latency, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(martian_signal_latency, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type2 == mountain,
    Type3 == mountain.

test(mountain_thresholds) :-
    % Verify metrics adhere to Mountain classification thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(martian_signal_latency, ExtMetricName, E),
    narrative_ontology:constraint_metric(martian_signal_latency, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_present) :-
    % Verify the required metrics for NL certification are present.
    narrative_ontology:constraint_metric(martian_signal_latency, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(martian_signal_latency, resistance, R),
    domain_priors:emerges_naturally(martian_signal_latency),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(martian_signal_latency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a canonical example of a Mountain. Its properties are
 *   derived from fundamental physics, not human systems.
 *
 *   - Extractiveness (0.05): Low. The speed of light does not "extract" value
 *     in a coercive sense, but it imposes an irreducible opportunity cost on
 *     mission duration and real-time scientific discovery.
 *   - Suppression (0.0): The suppression score for a natural law is zero. It
 *     does not "suppress" alternatives (like FTL communication) in the way a
 *     human policy suppresses dissent; the alternatives are simply physically
 *     incoherent or non-existent. The constraint doesn't need to expend energy
 *     to maintain its dominance.
 *   - NL Profile: Accessibility Collapse is 1.0 because no alternative is
 *     conceivable under current physics. Resistance is 0.0 because one cannot
 *     meaningfully "resist" the speed of light.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a fundamental physical law, its
 *   classification is Mountain from all possible indices. Human agents can
 *   build adaptive systems (Ropes, like the sol-based planning cycle) in
 *   response to the Mountain, but this does not change the nature of the
 *   Mountain itself. This aligns with the ε-invariance principle: the human
 *   adaptation is a separate constraint with its own properties.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint does not have beneficiaries or victims.
 *   The concept is inapplicable to a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a uniform Mountain prevents mislabeling a physical
 *   law as a human-construct like a Snare or Rope. It correctly identifies
 *   the constraint as an immutable boundary condition for human action, not
 *   a product of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_martian_signal_latency,
    'Will quantum entanglement or other exotic physics provide a method for faster-than-light information transfer?',
    'Resolution requires a fundamental breakthrough in theoretical and experimental physics, specifically violating the no-communication theorem.',
    'If Yes: Signal latency was a temporary Scaffold of physics, not a permanent Mountain. If No: It remains an eternal Mountain.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_martian_signal_latency, empirical, 'Possibility of FTL information transfer via exotic physics.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(martian_signal_latency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a physical constant, the metrics do not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% The human response to this Mountain is a separate constraint. For example,
% the rigid "sol-based" planning cycle used by JPL is a Rope built to
% coordinate activity around the latency. This could be modeled as:
%
% narrative_ontology:affects_constraint(martian_signal_latency, jpl_sol_planning_cycle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. This is a Mountain with no beneficiaries or victims.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */