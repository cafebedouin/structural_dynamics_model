% ============================================================================
% CONSTRAINT STORY: damped_harmonics
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_damped_harmonics, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: damped_harmonics
 *   human_readable: Damped Harmonic Motion
 *   domain: technological
 *
 * SUMMARY:
 *   Damped harmonic motion describes an oscillatory system where the amplitude
 *   decreases over time due to dissipative forces (friction, air resistance,
 *   or internal viscosity). It represents the inevitable conversion of kinetic
 *   energy into thermal entropy, a fundamental physical process. This constraint
 *   is foundational to engineering, where damping is either a problem to be
 *   overcome (energy loss) or a tool to be used (stability control).
 *
 * KEY AGENTS (by structural relationship):
 *   - The Oscillator (e.g., Mass on a Spring): Primary target (powerless/trapped) — its motion is governed by the physical law.
 *   - The Mechanical Engineer: Primary beneficiary (institutional/mobile) — uses damping as a coordination tool to stabilize systems.
 *   - The Analytical Observer: Analytical observer — sees the full structure as a physical law (Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: ε=0.25 represents the "heat tax" of thermodynamics. Damping
% extracts kinetic energy from a system, converting it to non-recoverable
% thermal energy. This value is at the upper limit for a Mountain, reflecting
% its tangible, extractive effect in mechanical contexts.
domain_priors:base_extractiveness(damped_harmonics, 0.25).

% Rationale: Suppression is 0.05. The law of damping is a fundamental aspect
% of physics and does not suppress alternatives; they are physically impossible
% in most macro-scale systems. The score is not zero to account for idealized
% models (e.g., frictionless surfaces) that make perpetual motion seem plausible
% in theoretical contexts. This value is compliant with the Mountain ceiling.
domain_priors:suppression_score(damped_harmonics, 0.05).
domain_priors:theater_ratio(damped_harmonics, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(damped_harmonics, extractiveness, 0.25).
narrative_ontology:constraint_metric(damped_harmonics, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(damped_harmonics, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% accessibility_collapse=1.0: No alternative to thermodynamic dissipation is conceivable.
% resistance=0.0: One cannot meaningfully "resist" friction as a physical law.
narrative_ontology:constraint_metric(damped_harmonics, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(damped_harmonics, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(damped_harmonics, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Damping emerges from physical laws (e.g., Second Law of Thermodynamics)
% and does not require active enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(damped_harmonics).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These are declared because the constraint has non-mountain perspectives (Rope).
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(damped_harmonics, system_designers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(damped_harmonics, energy_efficiency_goals).

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

% PERSPECTIVE 1: THE OSCILLATING MASS (MOUNTAIN)
% For the physical object, the decay of its motion is a natural law. It cannot
% "choose" to ignore friction. The transition to equilibrium is an unyielding
% physical fate.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE AUTOMOTIVE ENGINEER (ROPE)
% For an engineer, damping is a functional coordination tool. By designing the
% damping coefficient (e.g., in a shock absorber), they ensure a vehicle
% returns to equilibrium quickly without oscillation, providing safety and
% performance. It's a pure coordination function from this perspective.
constraint_indexing:constraint_classification(damped_harmonics, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer, viewing the system from a fundamental physics
% standpoint, sees damping as an inescapable consequence of thermodynamics.
% The metrics (ε=0.25, suppression=0.05) fall within the Mountain classification,
% reflecting its status as a natural law.
constraint_indexing:constraint_classification(damped_harmonics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(damped_harmonics_tests).

test(perspectival_gap) :-
    % Verify the gap between the physical object (Mountain) and the engineer (Rope).
    constraint_indexing:constraint_classification(damped_harmonics, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(damped_harmonics, rope, context(agent_power(institutional), _, _, _)),
    \+ constraint_indexing:constraint_classification(damped_harmonics, rope, context(agent_power(powerless), _, _, _)).

test(analytical_view_is_mountain) :-
    % The fundamental, analytical view should classify this as a Mountain.
    constraint_indexing:constraint_classification(damped_harmonics, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_are_met) :-
    % Verify the base metrics are compliant with the Mountain classification.
    narrative_ontology:constraint_metric(damped_harmonics, extractiveness, E),
    narrative_ontology:constraint_metric(damped_harmonics, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(damped_harmonics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint models a fundamental physical law. To pass the natural law
 *   certification chain, the file now includes the required NL profile metrics:
 *   accessibility_collapse is 1.0 (no alternatives to thermodynamics exist),
 *   resistance is 0.0 (one cannot meaningfully "resist" friction), and the
 *   emerges_naturally flag is set. The base extractiveness of 0.25 represents
 *   the unavoidable "heat tax" of thermodynamics—a real energy loss that is
 *   still within the Mountain classification's ceiling.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the physical object experiencing an unchangeable law
 *   (Mountain) and the engineer who harnesses that law as a predictable design
 *   tool for stability (Rope). For the engineer, damping is not extractive; it's
 *   a coordination mechanism to prevent destructive resonance. This highlights
 *   how a single physical law can be perceived differently based on the agent's
 *   power to manipulate the system's parameters.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: 'system_designers' benefit by using damping to create stable,
 *     safe, and predictable mechanical systems (e.g., vehicle suspension, building
 *     stabilizers).
 *   - Victim: 'energy_efficiency_goals' are the "victim," as damping represents
 *     an unavoidable loss of useful energy from a system to heat, reducing
 *     overall efficiency.
 *
 * MANDATROPHY ANALYSIS:
 *   This story demonstrates how a physical law (Mountain) can be instrumented
 *   into a coordination function (Rope). An earlier version might have conflated
 *   this with signal attenuation, which would violate the ε-invariance principle.
 *   Signal attenuation in a high-resistance medium is a different physical
 *   process with a much higher effective extractiveness (ε) and should be modeled
 *   in a separate constraint story (e.g., `constraint_signal_attenuation`).
 *   This corrected version focuses strictly on the canonical damped harmonic
 *   oscillator to maintain a stable ε.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_damped_harmonics_1,
    'Does the Mountain classification hold if damping is non-linear (e.g., quadratic air resistance)?',
    'Compare system behavior in high-velocity contexts (where F is proportional to v^2) with the linear model.',
    'If non-linear effects dominate, the predictable Rope becomes a chaotic system, reducing predictability and control.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_damped_harmonics_2,
    'What is the classification of an active damping system (a Rope) when it loses power?',
    'Simulate power failure in active suspension or skyscraper damping systems.',
    'If power fails, the engineered Rope vanishes, and the underlying Mountain of physics reasserts itself, potentially leading to catastrophic resonant failure.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(damped_harmonics, 1687, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a physical law; its core metrics do not drift over time.
% No temporal measurements are needed.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships declared for this fundamental physical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% The standard derivation from beneficiary/victim groups is accurate for this
% constraint. No overrides are needed.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */