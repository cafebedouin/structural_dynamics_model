% ============================================================================
% CONSTRAINT STORY: logistic_map_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_logistic_map_dynamics, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: logistic_map_dynamics
 *   human_readable: The Logistic Map (Bifurcation and Chaos)
 *   domain: mathematical/biological
 *
 * SUMMARY:
 *   The logistic map (x_{n+1} = r·x_n·(1-x_n)) is a canonical exemplar of a
 *   mathematical mountain constraint. It is a simple one-dimensional
 *   dynamical system that encodes all the essential phenomena of chaos:
 *   period-doubling bifurcations, chaotic attractors, strange attractors with
 *   fractal dimension, sensitivity to initial conditions (Lyapunov chaos),
 *   and the universal route to chaos discovered by Feigenbaum. The map
 *   originated as a model of discrete-time population dynamics (hence the
 *   name 'logistic'), but its significance transcends biology — it appears in
 *   diverse domains including chemical reactions, optics, and circuit design.
 *   What makes it a mountain is not its simplicity but its irreducibility:
 *   the chaotic dynamics cannot be engineered away, cannot be averaged,
 *   cannot be solved in closed form for most parameter values, and cannot be
 *   escaped by any observer or agent. Every mathematician, physicist, or
 *   computational theorist who engages with the map encounters the same
 *   structure. The constraint is that complexity emerges from simplicity
 *   according to mathematically determined rules.
 *
 * KEY AGENTS:
 *   - Mathematical logician: Analytical observer — studies the formal structure of the iteration rule and the topology of the parameter space
 *   - Empiricist: Analytical observer — verifies bifurcation diagrams and chaotic attractors through computation and experiment
 *   - Physicist: Institutional observer — interprets the map as evidence of natural limits on predictability and complexity emergence
 *   - Population biologist: Implicit analyst — original motivation (discrete population models) but no special privileged perspective; the dynamics constrain all users equally
 *   - Computational theorist: Analytical observer — studies algorithmic properties of trajectories and parameter-space structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(logistic_map_dynamics, 0.08).
domain_priors:suppression_score(logistic_map_dynamics, 0.02).
domain_priors:theater_ratio(logistic_map_dynamics, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, extractiveness, 0.08).
narrative_ontology:constraint_metric(logistic_map_dynamics, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(logistic_map_dynamics, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(logistic_map_dynamics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(logistic_map_dynamics, mountain).
narrative_ontology:human_readable(logistic_map_dynamics, "The Logistic Map (Bifurcation and Chaos)").
narrative_ontology:topic_domain(logistic_map_dynamics, "mathematical/biological").

domain_priors:emerges_naturally(logistic_map_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL LOGICIAN — The logistic map is an irreducible mathematical truth. For any parameter r in [0, 4], the iteration x_{n+1} = r·x_n·(1-x_n) produces deterministic dynamics. The bifurcation structure (period-doubling cascade, onset of chaos at r≈3.57) is a proven mathematical fact, not contingent on implementation or interpretation. Zero degrees of freedom; zero external extraction. This is a mountain from the purely formal perspective.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: EMPIRICIST MATHEMATICIAN — The logistic map exhibits bifurcation diagrams and chaotic attractors that can be computed and visualized. The route to chaos (period-doubling, chaotic windows, fractal structure) has been experimentally verified in discrete biological populations, chemical reactions, and physical systems. These observational landmarks are robust — they appear regardless of observer, measurement precision, or computational method. The constraint is the same from all empirical vantage points.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PHYSICIST — The logistic map demonstrates that simple systems can produce complex behavior. This is a fundamental limit on predictability: initial condition sensitivity (Lyapunov exponents > 0 in chaotic regimes) means long-term trajectories are not practically computable despite full determinism. This constraint — the emergence of deterministic chaos from simple rules — is a natural law that cannot be engineered away or dissolved through policy.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPUTATIONAL THEORIST — The logistic map exhibits algorithmic irreducibility: trajectories in chaotic regimes cannot be compressed into a closed-form formula significantly shorter than simulation. The bifurcation structure itself exhibits fractal self-similarity — the Mandelbrot set boundary analogue embedded in the parameter space. These properties are not artifacts of representation; they are consequences of the map's structure. This is a constraint on what can be known and computed about the system.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(logistic_map_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(logistic_map_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(logistic_map_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(logistic_map_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(logistic_map_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(logistic_map_dynamics),
    narrative_ontology:constraint_metric(logistic_map_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(logistic_map_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(logistic_map_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε = 0.08): Minimal. The logistic map is a pure mathematical object. No agent extracts value from others; no costs are imposed differentially on groups. The dynamics are the same for all observers. The 0.08 value reflects slight measurement ambiguity at the precision boundary (is the chaos truly deterministic or just pseudo-random within computational precision?), not structural extraction. Suppression (0.05): Minimal. There are no alternatives suppressed, no choices constrained. The iteration rule is universally accessible. The only 'suppression' is logical: once you accept the axioms of real numbers and iteration, the bifurcation diagram follows necessarily. Theater ratio (0.05): Minimal. No performative behavior is required to engage with the logistic map. Computation is straightforward; visualization is direct. The map's behavior speaks for itself. Accessibility collapse (0.92): High. The logistic map is mathematically irreducible — you cannot reduce its dynamics to a simpler representation for most parameter values. The chaotic regimes have fractal structure that defies closed-form simplification. This high accessibility collapse is the signature of a mountain constraint: you cannot circumvent or collapse the constraint into a simpler form. Resistance to substitution (0.08): Low. There is minimal resistance to accepting the map's dynamics as true. The mathematics is rigorous, the empirical verification is robust, and the theoretical understanding is mature. Only at the extreme precision boundary (floating-point rounding errors vs true chaos) is there any interpretive debate.
 *
 * PERSPECTIVAL GAP:
 *   MINIMAL PERSPECTIVAL GAP — This is a uniform-type constraint (mountain-only). All four perspectives classify the logistic map as a mountain because the underlying mathematical structure is invariant across all observational positions. The logician, empiricist, physicist, and theorist all encounter the same bifurcation diagram, the same chaotic attractors, the same Lyapunov exponents. The gap that exists is purely narrative (how to interpret the constraint's significance) rather than structural (whether the constraint exists). The physicist interprets chaos as a limit on predictability; the theorist interprets it as algorithmic irreducibility; the empiricist verifies its reality; the logician proves its necessity. But all four are measuring the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies. The logistic map has no beneficiaries or victims. All agents experience the same constraint with equal force. Directionality (d) is undefined for mountains because there is no extraction structure — no asymmetric costs, no differential exit options, no power asymmetry relative to the constraint itself. The constraint simply is.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(logistic_map_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(logistic_map_dynamics, information_standard).
narrative_ontology:affects_constraint(logistic_map_dynamics, feigenbaum_constant_universality).
narrative_ontology:affects_constraint(logistic_map_dynamics, lyapunov_exponent_positivity).
narrative_ontology:affects_constraint(logistic_map_dynamics, period_doubling_cascade).

% DUAL FORMULATION NOTE:
% The logistic map dynamics form a constraint family with three related but distinct mathematical objects: the Feigenbaum constant (universal rate of period-doubling), Lyapunov exponent positivity (sensitivity to initial conditions), and the period-doubling cascade route to chaos. The logistic map itself serves as the primary exemplar; the downstream constraints are structural consequences of the map's intrinsic properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
