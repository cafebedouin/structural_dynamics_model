% ============================================================================
% CONSTRAINT STORY: lorenz_attractor_dynamics
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-01
% ============================================================================

:- module(constraint_lorenz_attractor_dynamics, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lorenz_attractor_dynamics
 *   human_readable: Lorenz Attractor (Deterministic Chaos)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   The Lorenz system is a set of ordinary differential equations describing
 *   atmospheric convection. It famously demonstrated "Deterministic Chaos," where
 *   simple, non-linear rules lead to complex, non-repeating, and sensitive
 *   trajectories. This constraint models the practical effect of this system:
 *   its sensitive dependence on initial conditions ("The Butterfly Effect"),
 *   which enables short-term coordination (weather forecasting) while
 *   extracting the possibility of long-term certainty.
 *
 * KEY AGENTS (by structural relationship):
 *   - Long-term Planners (powerless/constrained): Primary target — bears the extraction of predictive certainty.
 *   - Short-term Forecasters (institutional/mobile): Primary beneficiary — uses the system's short-term predictability as a coordination tool (e.g., aviation).
 *   - Complexity Scientists (organized/mobile): Secondary beneficiary — the system's properties create a field of study.
 *   - Analytical Observer (analytical/analytical): Sees the full dual nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate (0.4). Chaos "extracts" certainty and predictability
% from observers, demanding massive computational re-investment (Ensembles)
% to maintain even minimal functional coordination. This value represents the
% long-term extractive nature of the system.
domain_priors:base_extractiveness(lorenz_attractor_dynamics, 0.4).
% Rationale: Moderate (0.5). It suppresses the visibility of long-term
% linear causality, rendering "simple" prediction models based on linear
% extrapolation functionally useless or fraudulent.
domain_priors:suppression_score(lorenz_attractor_dynamics, 0.5).
% Rationale: The system is a formal truth with near-zero performative aspect.
domain_priors:theater_ratio(lorenz_attractor_dynamics, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, extractiveness, 0.4).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, theater_ratio, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lorenz_attractor_dynamics, tangled_rope).
narrative_ontology:human_readable(lorenz_attractor_dynamics, "Lorenz Attractor (Deterministic Chaos)").

% --- Binary flags ---
% Enforcement emerges naturally from the non-linear coupling of variables.
% Required for Tangled Rope.
domain_priors:requires_active_enforcement(lorenz_attractor_dynamics).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(lorenz_attractor_dynamics, short_term_forecasters).
narrative_ontology:constraint_beneficiary(lorenz_attractor_dynamics, complexity_scientists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(lorenz_attractor_dynamics, long_term_planners).
narrative_ontology:constraint_victim(lorenz_attractor_dynamics, linear_extrapolation_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MATHEMATICAL POINT (SNARE)
% WHO: powerless - The coordinate (x,y,z) cannot deviate from its determined path.
% WHEN: immediate - The vector field is local and instantaneous.
% WHERE: trapped - Bound within the geometric bounds of the attractor.
% SCOPE: local - Immediate neighborhood in phase space.
%
% WHY THIS CLASSIFICATION:
% For the state variable, the Lorenz equations are an unyielding law. While this
% feels like a Mountain, the system's high base extraction (ε=0.4) means it
% cannot be one by definition. Instead, it is a Snare whose mechanism is so
% total and inescapable that it appears as a law of nature to the trapped agent.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AVIATION DISPATCHER (ROPE)
% WHO: institutional - Power to coordinate global flight paths using forecasts.
% WHEN: biographical - Planning routes for the next 24-48 hours.
% WHERE: mobile - Can adjust paths or delay flights based on the data.
% SCOPE: national - Managing airspace.
%
% WHY THIS CLASSIFICATION:
% In the short term (within the Lyapunov time), the system is a Rope. It
% provides a functional coordination mechanism that allows for safe,
% efficient transit. As a beneficiary with arbitrage, the effective
% extraction is negative, yielding a Rope classification.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LONG-TERM POLICY PLANNER (SNARE)
% WHO: powerless - Bound by the "Butterfly Effect" limits of math.
% WHEN: civilizational - Trying to predict long-term climate/social shifts.
% WHERE: constrained - No alternative but to rely on increasingly noisy data.
% SCOPE: global - Global environmental/economic planning.
%
% WHY THIS CLASSIFICATION:
% For the long-term planner, chaos is a Snare. It extracts their ability to
% provide certain outcomes, forcing enormous investment in probabilistic
% scenarios that can never be fully verified. High base extraction and high
% directionality (d≈1.0) yield a high effective extraction (χ), defining a Snare.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% WHY THIS CLASSIFICATION:
% The analytical observer sees both the coordination function (the Rope for
% short-term forecasters) and the asymmetric extraction (the Snare for
% long-term planners). This dual nature, combined with active enforcement
% (the math itself), is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(lorenz_attractor_dynamics, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lorenz_attractor_tests).

test(perspectival_gap_user_vs_victim) :-
    % Verify the gap between the short-term user (Rope) and long-term victim (Snare).
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, TypeVictim, context(agent_power(powerless), time_horizon(civilizational), _, _)),
    TypeBeneficiary = rope,
    TypeVictim = snare.

test(analytical_view_is_tangled_rope) :-
    % The analytical view must resolve the Rope/Snare duality into a Tangled Rope.
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_threshold_validation) :-
    % Verify metrics meet the Tangled Rope criteria.
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, extractiveness, E),
    narrative_ontology:constraint_metric(lorenz_attractor_dynamics, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(lorenz_attractor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this constraint was a Mountain while
 *   assigning high extraction (0.4) and suppression (0.5) scores, violating
 *   the Mountain definition (ε ≤ 0.25, S ≤ 0.05). This regeneration corrects
 *   the core claim to Tangled Rope, which is consistent with the metrics and
 *   the perspectival narrative. The system exhibits a clear coordination
 *   function (short-term weather forecasting) and a clear asymmetric
 *   extraction function (destroying long-term certainty for planners).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for an aviation dispatcher planning flights for tomorrow,
 *   the system is a pure coordination Rope. For a climate scientist modeling
 *   outcomes 50 years hence, it is a Snare that extracts the very possibility
 *   of certainty. The analytical observer, seeing both functions, correctly
 *   classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `short_term_forecasters` and `complexity_scientists` benefit from the system's properties. Their relationship yields a low directionality `d`, resulting in a low/negative effective extraction `χ` (Rope).
 *   - Victims: `long_term_planners` and users of `linear_extrapolation_models` have their primary function (prediction) destroyed by the system. Their relationship yields a high `d`, resulting in a high `χ` (Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that while the Lorenz system has
 *   a valid coordination function, it is not a pure Rope. Labeling it a
 *   Mountain (as the original file did) would be a False Natural Law error,
 *   hiding the significant extractive costs imposed on long-term planning.
 *   Labeling it a pure Snare would ignore its vital role in short-term
 *   coordination. Tangled Rope is the only classification that captures this
 *   essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_lorenz_shadowing,
    "Do stable 'shadow' trajectories exist that would make long-term numerical simulations reliable despite chaos?",
    "Numerical verification of hyperbolic vs non-hyperbolic regions in the Lorenz attractor.",
    "If Yes: The Snare aspect is an illusion and the constraint is closer to a Rope. If No: The Snare is a fundamental feature.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lorenz_attractor_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is not > 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system provides a standard for information processing (weather models).
narrative_ontology:coordination_type(lorenz_attractor_dynamics, information_standard).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint models the long-term unpredictability (the "Butterfly Effect")
% arising from the Lorenz equations, which has ε ≈ 0.4. A different constraint,
% `lorenz_instantaneous_flow`, could model the purely deterministic,
% non-chaotic behavior over an infinitesimal time step. That constraint would
% be a Mountain (ε ≈ 0.02) and would be linked via:
% affects_constraint(lorenz_instantaneous_flow, lorenz_attractor_dynamics).
% This file models the more consequential of the two phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality for all agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */