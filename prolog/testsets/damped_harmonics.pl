% ============================================================================
% CONSTRAINT STORY: damped_harmonics
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Classical Mechanics / Differential Equations
% ============================================================================

:- module(constraint_damped_harmonics, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: damped_harmonics
 * human_readable: Damped Harmonic Motion
 * domain: physics/technological
 * temporal_scope: 17th Century - Present
 * spatial_scope: Universal (Physical Systems)
 * * SUMMARY:
 * Damped harmonic motion describes an oscillatory system where the amplitude
 * decreases over time due to dissipative forces (friction, air resistance,
 * or internal viscosity). It is the inevitable transition from kinetic energy
 * to thermal entropy.
 * * KEY AGENTS:
 * - The Oscillator (Mass/Pendulum): The powerless subject following the
 * second-order differential equation.
 * - The Mechanical Engineer: An institutional agent who "tunes" the damping
 * ratio to ensure system stability.
 * - The Signal Analyst: An analytical observer measuring the rate of
 * information decay in a transmission medium.
 * * NARRATIVE ARC:
 * In the vacuum of pure theory, an oscillator lives forever as a Mountain
 * of perpetual motion. In the real world, "Damping" is the first law of
 * physical constraints. For the car passenger, the shock absorber is a
 * "Rope" of comfort and coordination. For the high-speed data packet,
 * signal damping is a "Snare" that extracts clarity, forcing the
 * re-investment of power to bridge the distance.
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(damped_harmonics, 1687, 2026).
narrative_ontology:constraint_claim(damped_harmonics, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.25. Friction/Damping "takes" kinetic energy from the
% system and converts it to heat, which is functionally extractive
% in mechanical contexts.
domain_priors:base_extractiveness(damped_harmonics, 0.25).

% Suppression score (0.0-1.0)
% Rationale: 0.1. Damping is widely understood; it does not hide
% its presence, though it renders "Ideal" perpetual motion invisible.
domain_priors:suppression_score(damped_harmonics, 0.1).

% Enforcement: Emerges naturally from the Second Law of Thermodynamics.
domain_priors:emerges_naturally(damped_harmonics).

% Metrics
narrative_ontology:constraint_metric(damped_harmonics, extractiveness, 0.25).
narrative_ontology:constraint_metric(damped_harmonics, suppression_requirement, 0.1).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(damped_harmonics, structural_safety). % Prevents resonant collapse.
constraint_victim(damped_harmonics, energy_efficiency). % Energy lost to heat.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE OSCILLATING MASS - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - The particle has no agency over the viscous force.
   WHEN: immediate - The force acts instantaneously proportional to velocity.
   WHERE: trapped - Bound by the physical medium (fluid/air).
   SCOPE: local - Immediate physical environment.

   WHY THIS CLASSIFICATION:
   For the physical object, the decay of its motion is a natural law. It cannot
   "choose" to ignore friction. The transition from $x(t)$ to equilibrium is
   an unyielding geometric fate.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    damped_harmonics,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AUTOMOTIVE ENGINEER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to design the damping coefficient ($c$).
   WHEN: biographical - Planning the lifecycle and safety of a vehicle.
   WHERE: mobile - Can adjust valve sizes or fluid viscosity to "exit" instability.
   SCOPE: global - Applies to all units in a fleet.

   WHY THIS CLASSIFICATION:
   Damping is a functional coordination tool. By choosing "Critical Damping,"
   the engineer ensures the car returns to equilibrium as fast as possible
   without oscillation, providing safety and performance.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    damped_harmonics,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE WEAK ACOUSTIC SIGNAL - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - The signal "packet" is destroyed by the medium.
   WHEN: immediate - Every meter of travel extracts more amplitude.
   WHERE: constrained - The signal must pass through the high-resistance path.
   SCOPE: local - The segment of the wire or air.

   WHY THIS CLASSIFICATION:
   In high-resistance contexts, damping "strangles" the signal. It extracts
   information value from the carrier, forcing the use of expensive repeaters
   (extraction of capital) just to survive the distance.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    damped_harmonics,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(damped_harmonics, E),
    E >= 0.2,
    !.

/* ==========================================================================
   4. TESTS
   ========================================================================= */

:- begin_tests(damped_harmonics_tests).

test(perspective_shift) :-
    % The Mass sees a Mountain; the Engineer sees a Rope.
    constraint_indexing:constraint_classification(damped_harmonics, mountain, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(damped_harmonics, rope, context(institutional, biographical, mobile, global)).

test(extraction_logic) :-
    % Signals in a medium should feel the "Snare" effect of energy loss.
    constraint_indexing:constraint_classification(damped_harmonics, snare, context(individual_powerless, immediate, constrained, local)).

test(natural_emergence) :-
    domain_priors:emerges_naturally(damped_harmonics).

:- end_tests(damped_harmonics_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. CLASSIFICATION: Damping is a unique physical constraint because it is
 * the "enforcer" of equilibrium. I classified it as a Mountain for the
 * subject but a Rope for the designer.
 * 2. EXTRACTIVENESS: Set at 0.25 to reflect the "heat tax" of thermodynamics.
 * 3. PERSPECTIVES: Chose the "Automotive Engineer" to represent the peak
 * utility of the "Rope" classification (stability).
 */

% OMEGA IDENTIFICATION
omega_variable(
    linearity_assumption,
    'Does the \'Mountain\' remain stable if the damping is non-linear (e.g., quadratic air resistance)?',
    resolution_mechanism('Compare $F propto v$ with $F propto v^2$ in high-velocity contexts.'),
    impact('If non-linear: The \'Rope\' becomes a chaotic \'Snare\' for predictability.'),
    confidence_without_resolution(medium)
).

omega_variable(
    active_damping_failure,
    'What happens when the \'Rope\' (active suspension) loses power?',
    resolution_mechanism('Simulation of damping loss in 100-story skyscrapers.'),
    impact('If failure: The Mountain of resonance returns to destroy the system.'),
    confidence_without_resolution(high)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Active Resonance (Energy Injection)
 * Viability: Counteracting damping with external power (e.g., clock escapement).
 * Suppression: Rejected in most passive safety designs due to reliability risk.
 * * ALTERNATIVE 2: Superfluidity / Superconductivity
 * Viability: Elimination of damping through phase transitions.
 * Suppression: Not viable at STP (Standard Temperature/Pressure).
 * * CONCLUSION:
 * At standard scales, Damping is an inescapable Mountain. Attempts to
 * suppress it (Alternatives) require high-power institutional "Ropes."
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [damped_harmonics].
% Analyze: ?- constraint_indexing:multi_index_report(damped_harmonics).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
