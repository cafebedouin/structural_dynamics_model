% ============================================================================
% CONSTRAINT STORY: micro_robot_electronics_integration
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_micro_robot_electronics_integration, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: micro_robot_electronics_integration
 *   human_readable: The Structural Barrier to Microrobot Electronics Integration
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint represents the fundamental "gap" in microscopic robotics:
 *   the inability to integrate semiconductor microelectronics with existing
 *   propulsion platforms. While macroscopic robots benefit from
 *   unified electronic control, microscopic platforms are traditionally
 *   bifurcated between mechanically robust propulsion and intelligent
 *   electronic circuitry, "limiting their potential for intelligence". The
 *   constraint is the set of research paradigms and physical trade-offs that
 *   maintain this separation.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Microrobot: Primary target (powerless/trapped) — subject to the
 *     physical laws of electrokinetic flows and photovoltaic control.
 *   - Researchers Pursuing Autonomous Intelligence: Secondary target (organized/mobile) —
 *     their research potential is extracted by the difficulty of integration.
 *   - Researchers Favoring Mechanical Simplicity: Primary beneficiary (institutional/arbitrage) —
 *     their simpler, non-integrated platforms are easier to fund and publish.
 *   - The Analytical Observer: Analytical observer — sees the full structure
 *     of coordination (simplicity) and extraction (lost potential).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(micro_robot_electronics_integration, 0.50).
domain_priors:suppression_score(micro_robot_electronics_integration, 0.60).
domain_priors:theater_ratio(micro_robot_electronics_integration, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(micro_robot_electronics_integration, extractiveness, 0.50).
narrative_ontology:constraint_metric(micro_robot_electronics_integration, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(micro_robot_electronics_integration, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain. The 'powerless'
% perspective classifies as mountain, so these are required to pass the linter.
narrative_ontology:constraint_metric(micro_robot_electronics_integration, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(micro_robot_electronics_integration, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(micro_robot_electronics_integration, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(micro_robot_electronics_integration).
domain_priors:emerges_naturally(micro_robot_electronics_integration).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(micro_robot_electronics_integration, researchers_favoring_mechanical_simplicity).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(micro_robot_electronics_integration, researchers_pursuing_autonomous_intelligence).
narrative_ontology:constraint_victim(micro_robot_electronics_integration, the_microrobot).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE MICROROBOT (MOUNTAIN)
% For the bot, the relationship between current (I) and speed (v) is a
% Mountain (v ∝ I). It is an unchangeable law of its world, emerging
% naturally from physics.
constraint_indexing:constraint_classification(micro_robot_electronics_integration, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RESEARCH ENGINEER (ROPE)
% To the scientist developing the new integrated platform, the electrokinetic
% model is a Rope. It is a functional coordination tool that links electronics
% with propulsion, allowing them to synchronize agent movements.
constraint_indexing:constraint_classification(micro_robot_electronics_integration, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function (mechanical simplicity for
% some researchers) and the asymmetric extraction (lost potential for
% intelligence, suppressed alternative research paths). This hybrid nature,
% requiring active enforcement through research paradigms, is a Tangled Rope.
constraint_indexing:constraint_classification(micro_robot_electronics_integration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(micro_robot_integration_tests).

test(perspectival_gap) :-
    % Bot (Mountain) vs Scientist (Rope) vs Historian (Tangled Rope)
    constraint_indexing:constraint_classification(micro_robot_electronics_integration, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(micro_robot_electronics_integration, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(micro_robot_electronics_integration, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == rope,
    TypeAnalytical == tangled_rope.

test(threshold_validation) :-
    % Metrics must be consistent with Tangled Rope classification.
    narrative_ontology:constraint_metric(micro_robot_electronics_integration, extractiveness, E),
    narrative_ontology:constraint_metric(micro_robot_electronics_integration, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(micro_robot_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated a physical law (Mountain) with a technological
 *   development bottleneck (Snare). This version clarifies the constraint as
 *   the bottleneck itself, which is better modeled as a Tangled Rope.
 *   - Base Extractiveness (ε=0.50): Increased from 0.2 to reflect the high
 *     cost of lost potential and wasted research funding on dead-end,
 *     non-integrable platforms. This aligns with a high-extraction claim.
 *   - Suppression (0.60): Kept high, as existing paradigms (e.g., chemical
 *     propulsion) are incompatible with and thus suppress the development of
 *     integrated semiconductor-based logic.
 *   - Claim (Tangled Rope): The situation has both a coordination function
 *     (mechanical simplicity is a valid, albeit limited, research goal that
 *     coordinates effort) and asymmetric extraction (this focus suppresses
 *     the more complex goal of autonomous intelligence). This duality is the
 *     hallmark of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - The Microrobot (Mountain): Experiences the underlying physics of its
 *     operation as an immutable law.
 *   - The Engineer (Rope): Focuses on the coordination aspect of their new
 *     solution, seeing it as a tool to solve a problem.
 *   - The Analyst (Tangled Rope): Sees the wider system, including the
 *     coordination function that benefits one research paradigm and the
 *     extractive effect on another, recognizing the hybrid nature.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope instead of a Snare correctly identifies
 *   that there is a genuine, albeit limited, coordination function at play
 *   (achieving mechanical simplicity). A Snare classification would incorrectly
 *   dismiss this function as pure theater, missing the subtle trade-offs that
 *   kept the field locked in this state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_micro_robot_electronics_integration,
    'Does the current requirement for off-board optical control (lasers) simply create a new Snare of external dependency?',
    'Evaluation of bots with fully autonomous on-board decision logic',
    'If Yes: The system remains a Tangled Rope/Snare. If No: It has successfully transformed into a fully internal Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_micro_robot_electronics_integration, empirical, 'Whether off-board optical control constitutes a new dependency snare').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(micro_robot_electronics_integration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required because base_extractiveness > 0.46.
% Models the period of stagnation where the integration problem became more
% costly over time as alternative paths were suppressed.
narrative_ontology:measurement(mrei_tr_t0, micro_robot_electronics_integration, theater_ratio, 0, 0.10).
narrative_ontology:measurement(mrei_tr_t5, micro_robot_electronics_integration, theater_ratio, 5, 0.12).
narrative_ontology:measurement(mrei_tr_t10, micro_robot_electronics_integration, theater_ratio, 10, 0.15).

narrative_ontology:measurement(mrei_ex_t0, micro_robot_electronics_integration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mrei_ex_t5, micro_robot_electronics_integration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mrei_ex_t10, micro_robot_electronics_integration, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The constraint governs how information (control logic) is integrated with
% physical platforms, making it an information standard.
narrative_ontology:coordination_type(micro_robot_electronics_integration, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */