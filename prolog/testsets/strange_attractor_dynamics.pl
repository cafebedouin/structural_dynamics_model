% ============================================================================
% CONSTRAINT STORY: strange_attractor_dynamics
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_strange_attractor_dynamics, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: strange_attractor_dynamics
 *   human_readable: Strange Attractor Dynamics (Sensitive Dependence)
 *   domain: mathematical/physical
 *
 * SUMMARY:
 *   This constraint models the dynamics *on* a strange attractor, specifically
 *   the sensitive dependence on initial conditions that characterizes chaos.
 *   Nearby trajectories diverge exponentially, "extracting" the informational
 *   value of initial measurements and making long-term prediction impossible.
 *   This is distinct from the topological existence of the attractor itself,
 *   which would be a separate Mountain constraint.
 *
 * KEY AGENTS (by structural relationship):
 *   - Long-term Predictors: Primary target (powerless/constrained) — their ability
 *     to forecast is destroyed by information loss.
 *   - System Designers & Cryptographers: Primary beneficiary (institutional/mobile) —
 *     they leverage the attractor's bounded chaos for robust system design
 *     and secure communications.
 *   - The System State: Secondary target (powerless/trapped) — a trajectory
 *     bound to the manifold, its path determined by the chaotic flow.
 *   - Analytical Observer: Sees the full structure of information extraction and
 *     coordination.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 2 stories decomposed from "Strange Attractor".
 * Decomposed because ε differs across observables (ε-invariance principle).
 * This story (strange_attractor_dynamics) models the information-extracting
 * dynamics on the attractor.
 * Related stories:
 *   - attractor_topology_existence (ε=0.02, Mountain) - The topological
 *     necessity of a bounded region in phase space.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: High (0.50). The base extractiveness represents the rate of
% information loss, quantified by the Kolmogorov-Sinai entropy (the sum of
% positive Lyapunov exponents). This exponential divergence rapidly extracts
% the value of any initial state measurement.
domain_priors:base_extractiveness(strange_attractor_dynamics, 0.50).

% Rationale: High (0.60). The existence of deterministic chaos suppresses
% simpler models, such as purely stochastic noise or stable limit cycles, as
% valid explanations for complex turbulent behavior. It forces a specific,
% computationally expensive paradigm.
domain_priors:suppression_score(strange_attractor_dynamics, 0.60).

% Rationale: Near-zero. The constraint is a mathematical reality, not a
% performative or social construct.
domain_priors:theater_ratio(strange_attractor_dynamics, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strange_attractor_dynamics, extractiveness, 0.50).
narrative_ontology:constraint_metric(strange_attractor_dynamics, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(strange_attractor_dynamics, theater_ratio, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(strange_attractor_dynamics, snare).
narrative_ontology:human_readable(strange_attractor_dynamics, "Strange Attractor Dynamics (Sensitive Dependence)").
narrative_ontology:topic_domain(strange_attractor_dynamics, "mathematical/physical").

% --- Binary flags ---
% Rationale: The system's own governing equations (e.g., Lorenz equations)
% continuously enforce the stretching and folding of phase space that
% generates the chaotic dynamics. This is an intrinsic, active process.
domain_priors:requires_active_enforcement(strange_attractor_dynamics).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(strange_attractor_dynamics, system_design_engineers).
narrative_ontology:constraint_beneficiary(strange_attractor_dynamics, chaos_based_cryptographers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(strange_attractor_dynamics, long_term_forecasters).
narrative_ontology:constraint_victim(strange_attractor_dynamics, laplacian_determinists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LONG-TERM FORECASTER (SNARE)
% Agent whose predictive models are destroyed by chaotic dynamics.
% χ = 0.50 * f(d=0.90) * σ(national=1.0) = 0.50 * 1.35 * 1.0 = 0.675 -> Snare
constraint_indexing:constraint_classification(strange_attractor_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SYSTEM DESIGNER (ROPE)
% Agent who uses the bounded nature of the attractor for coordination.
% χ = 0.50 * f(d=0.15) * σ(global=1.2) = 0.50 * -0.01 * 1.2 = -0.006 -> Rope
constraint_indexing:constraint_classification(strange_attractor_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEM STATE TRAJECTORY (TANGLED ROPE)
% The point moving through phase space, trapped on the manifold.
% χ = 0.50 * f(d=0.95) * σ(local=0.8) = 0.50 * 1.42 * 0.8 = 0.568 -> Tangled Rope
constraint_indexing:constraint_classification(strange_attractor_dynamics, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context. Sees the dominant feature as information extraction.
% χ = 0.50 * f(d=0.72) * σ(global=1.2) = 0.50 * 1.15 * 1.2 = 0.69 -> Snare
constraint_indexing:constraint_classification(strange_attractor_dynamics, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strange_attractor_dynamics_tests).

test(perspectival_gap_forecaster_vs_designer) :-
    constraint_indexing:constraint_classification(strange_attractor_dynamics, snare,
        context(agent_power(powerless), time_horizon(biographical), exit_options(constrained), spatial_scope(national))),
    constraint_indexing:constraint_classification(strange_attractor_dynamics, rope,
        context(agent_power(institutional), time_horizon(generational), exit_options(mobile), spatial_scope(global))).

test(snare_threshold_validation) :-
    % Verify the analytical perspective correctly classifies as Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, ExtMetricName, E),
    E >= 0.46,
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(strange_attractor_dynamics, SuppMetricName, S),
    S >= 0.60.

test(tangled_rope_emergence_for_trajectory) :-
    % Verify the trapped trajectory sees a Tangled Rope.
    constraint_indexing:constraint_classification(strange_attractor_dynamics, tangled_rope,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))).

:- end_tests(strange_attractor_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated the attractor's topological existence (a Mountain)
 *   with its internal dynamics (a Snare). This version focuses strictly on the
 *   dynamics, characterized by sensitive dependence on initial conditions.
 *   - Base Extractiveness (ε=0.50): Represents the high rate of information loss
 *     (positive Lyapunov exponent), which makes long-term prediction impossible.
 *   - Suppression (0.60): The discovery of deterministic chaos suppressed
 *     competing theories of turbulence based on pure stochasticity or simple
 *     periodic behavior, establishing a new, dominant paradigm.
 *   - Claim (Snare): From an analytical view, the primary effect is the
 *     extraction of predictive certainty, which fits the Snare archetype.
 *
 * PERSPECTIVAL GAP:
 *   - Forecasters (Snare): They are the primary victims. The system extracts
 *     the value of their initial data, rendering their models useless over time.
 *   - System Designers (Rope): They are beneficiaries. They don't care about a
 *     specific trajectory, only that the system stays within a bounded,
 *     statistically predictable envelope. For them, the attractor is a pure
 *     coordination device ensuring system stability.
 *   - The Trajectory (Tangled Rope): It is trapped (extraction) but its path also
 *     defines the shape of the attractor (coordination), making it a hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `system_design_engineers` and `chaos_based_cryptographers`
 *     exploit the attractor's properties for practical benefit (boundedness,
 *     entropy generation).
 *   - Victims: `long_term_forecasters` and `laplacian_determinists` represent
 *     the group whose goals are fundamentally thwarted by the constraint. The
 *     engine derives a high directionality (d) for them, leading to high
 *     effective extraction (χ) and a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that while a coordination function
 *   exists (the Rope for designers), the dominant structural feature from a
 *   global, analytical perspective is the irreversible extraction of information.
 *   Labeling it a pure Rope would ignore the profound negative consequences for
 *   prediction, while labeling it a Mountain (as the original file did) would
 *   be a category error, conflating dynamics with topology and ignoring the
 *   high, measurable extraction rate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_attractor_shadowing,
    'Does a true trajectory in a physical system reliably shadow the numerical orbit from a simulation, despite the simulation being a Snare for long-term prediction?',
    'Empirical verification of the Shadowing Lemma for specific physical systems (e.g., fluid dynamics) versus their digital twins.',
    'If YES, the digital Snare is a useful Scaffold that points to a real-world Rope. If NO, the simulation is a pure Snare with no reliable connection to reality over time.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(strange_attractor_dynamics, 1971, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is a stable mathematical feature. The metrics are constant
% over the defined interval. Required because base_extractiveness > 0.46.
narrative_ontology:measurement(sad_tr_t0, strange_attractor_dynamics, theater_ratio, 0, 0.01).
narrative_ontology:measurement(sad_tr_t5, strange_attractor_dynamics, theater_ratio, 5, 0.01).
narrative_ontology:measurement(sad_tr_t10, strange_attractor_dynamics, theater_ratio, 10, 0.01).

narrative_ontology:measurement(sad_ex_t0, strange_attractor_dynamics, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(sad_ex_t5, strange_attractor_dynamics, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(sad_ex_t10, strange_attractor_dynamics, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: Used to generate pseudo-random numbers for encryption
% and to define stable operating envelopes for systems.
narrative_ontology:coordination_type(strange_attractor_dynamics, information_standard).

% Network relationships: The topological existence of the attractor is a
% precondition for its chaotic dynamics.
narrative_ontology:affects_constraint(attractor_topology_existence, strange_attractor_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */