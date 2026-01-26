% ============================================================================
% CONSTRAINT STORY: lorenz_attractor_dynamics
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Edward Lorenz (1963) / Chaos Theory / Deterministic Nonperiodic Flow
% ============================================================================

:- module(constraint_lorenz_attractor, []).

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
 * * constraint_id: lorenz_attractor_dynamics
 * human_readable: Lorenz Attractor (Deterministic Chaos)
 * domain: mathematical/technological/physical
 * temporal_scope: 1963 - Present (Civilizational)
 * spatial_scope: Global/Abstract (Phase Space)
 * * SUMMARY:
 * The Lorenz system is a set of ordinary differential equations describing 
 * atmospheric convection. It famously demonstrated "Deterministic Chaos," where 
 * simple, non-linear rules lead to complex, non-repeating, and sensitive 
 * trajectories. It established the "Butterfly Effect" as a fundamental limit 
 * on long-term prediction.
 * * KEY AGENTS:
 * - The Trajectory (Subject): The powerless agent following the vector field, 
 * trapped in a "strange" butterfly-shaped manifold.
 * - The Meteorologist (Institutional): An agent attempting to use the 
 * system as a "Rope" for coordination (weather forecasting).
 * - The Predictive Model (Victim): An agent whose accuracy is "strangled" by 
 * infinitesimal initial errors, turning the math into a "Snare."
 * * NARRATIVE ARC:
 * The Lorenz equations are a "Mountain" of physical reality—the rules are 
 * simple and unchangeable. In the short term, they provide a "Rope" for 
 * flight planning and logistics. However, as time $t$ increases, the 
 * sensitivity to initial conditions acts as a "Snare," extracting the 
 * possibility of certainty and propping up a "Scaffold" of probability 
 * instead of fact.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(lorenz_era, 1963, 2026).
narrative_ontology:constraint_claim(lorenz_attractor_dynamics, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). Chaos "extracts" certainty and predictability 
% from observers, demanding massive computational re-investment (Ensembles) 
% to maintain even minimal functional coordination.
domain_priors:base_extractiveness(lorenz_attractor_dynamics, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). It suppresses the visibility of long-term 
% linear causality, rendering "simple" prediction models functionally 
% invisible or fraudulent.
domain_priors:suppression_score(lorenz_attractor_dynamics, 0.5).

% Enforcement: Emerges naturally from the non-linear coupling of variables.
domain_priors:emerges_naturally(lorenz_attractor_dynamics).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, extractiveness, 0.4).
narrative_ontology:constraint_metric(lorenz_attractor_dynamics, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(lorenz_attractor_dynamics, ensemble_forecasting). % Profits from uncertainty management.
constraint_beneficiary(lorenz_attractor_dynamics, complexity_science).
constraint_victim(lorenz_attractor_dynamics, long_term_determinism). % The "death" of Laplace's Demon.
constraint_victim(lorenz_attractor_dynamics, linear_extrapolation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MATHEMATICAL POINT - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless - The coordinate (x,y,z) cannot deviate from $\dot{x}, \dot{y}, \dot{z}$.
   WHEN: immediate - The vector field is local and instantaneous.
   WHERE: trapped - Bound within the geometric bounds of the attractor.
   SCOPE: local - Immediate neighborhood in phase space.
   
   WHY THIS CLASSIFICATION:
   For the state variable, the Lorenz equations are an unyielding law. 
   The "Butterfly" is not a choice; it is a fixed, non-negotiable 
   attractor of its existence.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    lorenz_attractor_dynamics,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AVIATION DISPATCHER - Rope
   --------------------------------------------------------------------------
   WHO: institutional - Power to coordinate global flight paths using forecasts.
   WHEN: biographical - Planning routes for the next 24-48 hours.
   WHERE: mobile - Can adjust paths or delay flights based on the data.
   SCOPE: national - Managing airspace.
   
   WHY THIS CLASSIFICATION:
   In the short term (within the Lyapunov time), the Lorenz system is a "Rope." 
   It provides a functional coordination mechanism that allows for safe, 
   efficient transit through a complex atmosphere.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    lorenz_attractor_dynamics,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE POLICY PLANNER (10-YEAR) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless - Bound by the "Butterfly Effect" limits of math.
   WHEN: civilizational - Trying to predict long-term climate/social shifts.
   WHERE: constrained - No alternative but to rely on increasingly noisy data.
   SCOPE: global - Global environmental/economic planning.
   
   WHY THIS CLASSIFICATION:
   For the long-term planner, chaos is a "Snare." It "strangles" their 
   ability to provide certain outcomes, extracting enormous political and 
   social capital to fund "scenarios" that can never be fully verified.
   
   NARRATIVE EVIDENCE:
   "Prediction of the sufficiently distant future is impossible by any method, 
   unless the present conditions are known with perfect accuracy." (Lorenz, 1963)
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    lorenz_attractor_dynamics,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(civilizational),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- 
    domain_priors:base_extractiveness(lorenz_attractor_dynamics, E),
    E >= 0.4,
    !.

/* ==========================================================================
   4. TESTS (Insights into Deterministic Chaos)
   ========================================================================== */

:- begin_tests(lorenz_attractor_tests).

test(lyapunov_time_variance) :-
    % Short term (immediate) -> Mountain/Rope
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, Type1, context(individual_powerless, immediate, trapped, local)),
    % Long term (civilizational) -> Snare
    constraint_indexing:constraint_classification(lorenz_attractor_dynamics, Type2, context(individual_powerless, civilizational, constrained, global)),
    Type1 = mountain,
    Type2 = snare.

test(extraction_of_certainty) :-
    % The "powerless" long-term observer suffers high extraction of predictive value.
    Context = context(individual_powerless, civilizational, constrained, global),
    constraint_indexing:extractiveness_for_agent(lorenz_attractor_dynamics, Context, Score),
    Score > 0.5.

test(natural_emergence) :-
    domain_priors:emerges_naturally(lorenz_attractor_dynamics).

:- end_tests(lorenz_attractor_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.4): 
 * I chose this because Chaos is a "predatory" mathematical state—it takes 
 * information (initial precision) and returns heat/noise.
 * * 2. SUPPRESSION SCORE (0.5): 
 * Chaos theory suppressed the 19th-century "Clockwork Universe" (Laplace). 
 * It rendered the linear-determinist alternative invisible to serious science.
 * * 3. PERSPECTIVE SELECTION:
 * Chose the "Mathematical Point" (Subject), "Aviation Dispatcher" (Short-term 
 * User), and "Long-term Planner" (Victim) to illustrate the Lyapunov horizon.
 */

% OMEGA IDENTIFICATION
omega_variable(
    shadowing_lemma_validity,
    "Do 'shadow' trajectories exist that make numerical simulations a reliable Rope despite Chaos?",
    resolution_mechanism("Numerical verification of hyperbolic vs non-hyperbolic regions in the Lorenz attractor."),
    impact("If Yes: The Snare is a 'Scaffold' (fake fear). If No: The Snare is a true Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Laplace's Demon (Perfect Determinism)
 * Viability: The belief that better measurement would solve all chaos.
 * Suppression: Explicitly rejected by Lorenz; precision is an infinite 
 * "extraction" that can never be fulfilled.
 * * ALTERNATIVE 2: Purely Stochastic Modeling
 * Viability: Treating the weather as a random walk.
 * Suppression: Rejected because it ignores the "Rope" of deterministic 
 * physics that *does* allow for short-term coordination.
 * * CONCLUSION:
 * The presence of short-term "Rope" utility (Alternative 2 rejection) 
 * while facing long-term "Snare" limits (Alternative 1 rejection) defines 
 * the Lorenz constraint.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [lorenz_attractor_dynamics].
% Analyze: ?- constraint_indexing:multi_index_report(lorenz_attractor_dynamics).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
