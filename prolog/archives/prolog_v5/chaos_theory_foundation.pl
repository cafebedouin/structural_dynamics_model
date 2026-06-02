% ============================================================================
% CONSTRAINT STORY: chaos_theory_foundation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chaos_theory_foundation, []).

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
 *   constraint_id: chaos_theory_foundation
 *   human_readable: Chaos Theory Foundation: Sensitivity to Initial Conditions
 *   domain: mathematics/physics/dynamical_systems
 *
 * SUMMARY:
 *   Chaos theory's central constraint — sensitivity to initial conditions —
 *   is a mathematical and physical limit on predictability in nonlinear
 *   dynamical systems. The constraint is that arbitrarily small uncertainties
 *   in initial conditions of a chaotic system amplify exponentially over time
 *   (characterized by positive Lyapunov exponents), rendering long-term
 *   deterministic prediction impossible. This is not a coordin mechanism,
 *   institutional barrier, or extractive arrangement. It is a structural
 *   property of certain classes of dynamical equations. The constraint has no
 *   beneficiaries or victims — no agent benefits from the predictability
 *   limit and no agent is exploited by it. The constraint simply exists as a
 *   boundary on what is computationally and epistemologically possible. All
 *   perspectives classify it identically as mountain because the barrier is
 *   invariant across all observational contexts and agent positions.
 *
 * KEY AGENTS:
 *   - Prediction agents: Attempt to forecast the long-term behavior of chaotic systems; universally subject to the limit regardless of power or resources
 *   - Measurement systems: Endeavor to characterize initial conditions with arbitrary precision; face absolute limits on measurement accuracy
 *   - Mathematical community: Recognize the constraint as a logical necessity arising from the structure of nonlinear equations
 *   - Applied domains (weather, climate, economics): Operationally constrained by predictability horizons derived from Lyapunov exponent estimates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chaos_theory_foundation, 0.12).
domain_priors:suppression_score(chaos_theory_foundation, 0.03).
domain_priors:theater_ratio(chaos_theory_foundation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chaos_theory_foundation, extractiveness, 0.12).
narrative_ontology:constraint_metric(chaos_theory_foundation, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(chaos_theory_foundation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(chaos_theory_foundation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(chaos_theory_foundation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chaos_theory_foundation, mountain).
narrative_ontology:human_readable(chaos_theory_foundation, "Chaos Theory Foundation: Sensitivity to Initial Conditions").
narrative_ontology:topic_domain(chaos_theory_foundation, "mathematics/physics/dynamical_systems").

domain_priors:emerges_naturally(chaos_theory_foundation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PREDICTION AGENT (MOUNTAIN) — An agent attempting to predict the future state of a chaotic system faces an insurmountable barrier: arbitrarily small uncertainties in initial conditions amplify exponentially over time, rendering long-term deterministic prediction impossible. This is not a policy constraint or institutional arrangement — it is a structural property of dynamical systems with positive Lyapunov exponents. No degree of freedom exists to escape this limitation.
constraint_indexing:constraint_classification(chaos_theory_foundation, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE INSTITUTIONAL OBSERVER (MOUNTAIN) — Even a well-resourced institution with advanced measurement technology and computational capacity cannot overcome the fundamental limit. More precise initial conditions and faster computation extend the predictability horizon but do not eliminate the barrier. The constraint is invariant across all observables and measurement methodologies.
constraint_indexing:constraint_classification(chaos_theory_foundation, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE MATHEMATICAL OBSERVER (MOUNTAIN) — From the perspective of dynamical systems theory, sensitivity to initial conditions emerges necessarily from the mathematics of nonlinear differential equations. The constraint is a logical/mathematical necessity, not contingent on physical implementation or institutional choice. The engine validates this as a true mountain: accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true.
constraint_indexing:constraint_classification(chaos_theory_foundation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chaos_theory_foundation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(chaos_theory_foundation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chaos_theory_foundation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(chaos_theory_foundation, ExtMetricName, E),
    domain_priors:suppression_score(chaos_theory_foundation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(chaos_theory_foundation),
    narrative_ontology:constraint_metric(chaos_theory_foundation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(chaos_theory_foundation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(chaos_theory_foundation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint does not extract value from any agent — it is a neutral mathematical limit. The value is near-zero because there is no asymmetry, no beneficiary, no mechanism of coercion. Theater ratio (0.15): Very low. The constraint does not perform or disguise itself. The mathematical description of chaos and sensitivity to initial conditions is fully transparent and requires no theatrical maintenance. Suppression (0.03): Minimal. There are no alternatives suppressed by this constraint — it is not suppressing anything. Accessibility collapse (0.92): Very high. The predictability limit is essentially inaccessible: one cannot compute or measure arbitrarily precisely; one cannot overcome exponential divergence through resources. Resistance (0.08): Very low. There is no active resistance or opposition to this constraint because it is not imposed by any agent — it emerges from mathematical structure. All metrics point unambiguously to mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is NO perspectival gap in this constraint. All six potential perspectives (powerless/trapped, moderate/constrained, powerful/mobile, organized/mobile, institutional/arbitrage, analytical/analytical) classify the constraint as mountain. This uniform classification reflects that the constraint is a true natural law: invariant across all observables, all measurement methodologies, all observer positions, and all time horizons. A constraint that classifies identically from every perspective is exceptionally rare and diagnostically significant — it indicates a genuine natural law, not a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no extraction flow. The constraint has no beneficiaries and no victims. No agent is advantaged or disadvantaged relative to the barrier — the barrier is universal and invariant. The mathematical limit binds all agents equally, regardless of power, resources, or exit options. This is the hallmark of a true mountain: the constraint does not create asymmetric advantage or burden.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint demonstrates the mandatrophy is a non-issue for true mountains. The constraint is universally classified as mountain. There is no risk of misclassifying it as coordination (rope) or extraction (snare or tangled rope) because the structure is mathematically transparent and the classification is invariant. The uniform type across all perspectives proves that no institutional or extractive mechanism is hiding beneath natural law framing — the natural law is real and irreducible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_precision_limit,
    'Is the predictability barrier fundamentally due to mathematics or due to physical limits on measurement precision?',
    'Quantum mechanical analysis: examine whether chaotic sensitivity reflects deterministic nonlinearity or quantum indeterminacy at the foundation',
    'If mathematical: constraint remains mountain regardless of quantum effects. If physical: the constraint might be decomposed into a mountain (mathematical chaos) plus a separate physics constraint (quantum measurement limits). Current evidence: both aspects are present, but the mathematical aspect dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_precision_limit, empirical, 'Mathematical vs physical origin of predictability limit').

omega_variable(
    ensemble_vs_trajectory_semantics,
    'Does the constraint apply to trajectory prediction (single initial condition) or ensemble statistics (distribution of outcomes)?',
    'Formalization of what ''prediction'' means in the target domain. Ensemble statistics for chaotic systems are often predictable even when individual trajectories diverge.',
    'If trajectory-focused: mountain classification stands — individual trajectories are unpredictable. If ensemble-focused: a separate constraint (statistical prediction in chaos) might classify as rope — ensemble properties are often computable. These are structurally distinct claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ensemble_vs_trajectory_semantics, conceptual, 'Whether constraint applies to trajectories or ensemble statistics').

omega_variable(
    pragmatic_horizon_definition,
    'Is there an operationally meaningful time horizon beyond which ''prediction'' becomes meaningless, or is unpredictability absolute?',
    'Establish whether weather prediction at 10-day horizon is fundamentally impossible or merely economically inefficient to pursue',
    'If pragmatic horizon exists: the constraint''s bite is contextual — different for weather (short horizon) vs cosmology (very long). If absolute: the constraint is perfectly universal. Current evidence: horizon is observably dependent, but the mathematical limit is absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pragmatic_horizon_definition, conceptual, 'Whether unpredictability is absolute or pragmatically horizon-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chaos_theory_foundation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chaos_tr_t0, chaos_theory_foundation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(chaos_tr_t50, chaos_theory_foundation, theater_ratio, 50, 0.14).
narrative_ontology:measurement(chaos_tr_t100, chaos_theory_foundation, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(chaos_be_t0, chaos_theory_foundation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(chaos_be_t50, chaos_theory_foundation, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(chaos_be_t100, chaos_theory_foundation, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chaos_theory_foundation, information_standard).
narrative_ontology:affects_constraint(chaos_theory_foundation, weather_prediction_horizon).
narrative_ontology:affects_constraint(chaos_theory_foundation, long_term_climate_modeling).
narrative_ontology:affects_constraint(chaos_theory_foundation, molecular_dynamics_simulation).
narrative_ontology:affects_constraint(chaos_theory_foundation, orbit_prediction_limit).

% DUAL FORMULATION NOTE:
% The chaos theory foundation is a mountain-level constraint that establishes the predictability horizon for all downstream constraints in applied domains (weather, climate, molecular dynamics, celestial mechanics). These downstream constraints inherit the mathematical limit but combine it with domain-specific factors (measurement error, computational cost, relevance timescales) that may produce different classifications at institutional or applied levels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
