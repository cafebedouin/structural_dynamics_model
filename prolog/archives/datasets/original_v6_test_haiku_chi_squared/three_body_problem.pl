% ============================================================================
% CONSTRAINT STORY: three_body_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_three_body_problem, []).

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
 *   constraint_id: three_body_problem
 *   human_readable: Predictability Limit in the Three-Body Problem
 *   domain: mathematics/physics/technology
 *
 * SUMMARY:
 *   The three-body problem represents a fundamental limit on predictability
 *   in deterministic dynamical systems. Unlike two-body gravitational systems
 *   (solved exactly by Newton and Kepler), systems of three or more mutually
 *   gravitating bodies exhibit sensitive dependence on initial conditions:
 *   arbitrarily small changes in starting position or velocity lead to
 *   exponentially diverging trajectories. This is not an engineering
 *   limitation or a measurement problem — it is a structural feature of the
 *   phase space topology. The constraint manifests differently across
 *   technological domains: satellite navigation can predict Earth-Moon-Sun
 *   orbital configurations reliably for a few centuries but becomes
 *   probabilistic beyond that horizon; asteroid impact prediction degrades
 *   rapidly after 10-20 years; molecular dynamics simulations of three-body
 *   interactions in chemical reactions operate on picosecond timescales
 *   before deterministic trajectories become indistinguishable. Yet the
 *   constraint is universal and invariant: no computational method, no
 *   measurement precision, no technological advance can eliminate the
 *   underlying mathematical barrier. This makes it a canonical mountain
 *   constraint — an irreducible feature of dynamical systems that agents
 *   encounter not as coercion but as a boundary condition of rational
 *   prediction.
 *
 * KEY AGENTS:
 *   - Satellite Navigation Technician: Powerless agent (trapped/immediate) — operates within prediction horizon constraints set by the three-body dynamics; cannot extend forecasting windows arbitrarily
 *   - Space Agency Orbital Mechanics Team: Organized institutional actor (analytical/civilizational) — develops Monte Carlo methods, numerical simulations, and probabilistic orbital estimates as rational response to the constraint
 *   - Mathematical Analyst (Dynamicist): Theoretical observer (analytical/civilizational) — recognizes the constraint as a consequence of Hamiltonian topology and nonintegrability, not a technological gap
 *   - Computational Approximation Framework: Institutional infrastructure (institutional/arbitrage) — operationalizes the limit as a tool boundary; accepts prediction horizon windows and works within them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(three_body_problem, 0.12).
domain_priors:suppression_score(three_body_problem, 0.02).
domain_priors:theater_ratio(three_body_problem, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(three_body_problem, extractiveness, 0.12).
narrative_ontology:constraint_metric(three_body_problem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(three_body_problem, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(three_body_problem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(three_body_problem, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(three_body_problem, mountain).
narrative_ontology:human_readable(three_body_problem, "Predictability Limit in the Three-Body Problem").
narrative_ontology:topic_domain(three_body_problem, "mathematics/physics/technology").

domain_priors:emerges_naturally(three_body_problem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SATELLITE NAVIGATION TECHNICIAN (MOUNTAIN) — Cannot escape the three-body limit. Long-term orbit prediction for Earth-Moon-Sun systems is fundamentally constrained by sensitivity to initial conditions. No exit option exists; the limit is intrinsic to the dynamical system. d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(three_body_problem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: SPACE AGENCY ORBITAL MECHANICS TEAM (MOUNTAIN) — Organized agents managing space missions encounter the three-body constraint as a fixed limit on prediction horizon, not as extraction or coercion. They respond by developing numerical methods, Monte Carlo propagation, and probabilistic orbital estimates rather than seeking to escape the constraint. The constraint enables rather than blocks their work by defining the boundary of valid prediction. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(three_body_problem, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / MATHEMATICIAN (MOUNTAIN) — From the vantage of pure dynamical systems theory, the three-body problem is a structural feature of Hamiltonian mechanics itself: the nonintegrability of three or more gravitationally interacting bodies follows from the topology of phase space and the absence of sufficient conserved quantities. This is not a law of physics but a law of mathematics. The constraint is universal, unchangeable, and applies identically to all observers. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(three_body_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: COMPUTATIONAL APPROXIMATION FRAMEWORK (MOUNTAIN) — Modern numerical solvers (N-body simulations, perturbative expansions, chaos-aware step-size control) have institutionalized the limit into algorithmic practice. The framework is not evading the constraint but operationalizing it: users accept a prediction horizon window (a few centuries for solar-system scale, microseconds for atomic-scale three-body interactions) and work within it. The mountain becomes a tool boundary rather than a barrier. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.001.
constraint_indexing:constraint_classification(three_body_problem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(three_body_problem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(three_body_problem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(three_body_problem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(three_body_problem, ExtMetricName, E),
    domain_priors:suppression_score(three_body_problem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(three_body_problem),
    narrative_ontology:constraint_metric(three_body_problem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(three_body_problem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(three_body_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The three-body constraint does not extract value from any agent — it imposes equal structural limits on all users of predictive systems. No agent gains advantage by suppressing alternatives; the constraint is invariant across all technological pathways. Base extraction ≤ 0.25 satisfies mountain gate. Suppression (0.02): Minimal. No coercion or lack of alternatives is operative. The constraint is not enforced through institutional suppression but through mathematical necessity. Agents can choose different approximation methods, computational scales, or prediction horizons, but cannot eliminate the underlying limit. Theater ratio (0.08): Very low. No performative activity is required to maintain the constraint — it requires no institutional enforcement, no theatrical legitimacy, no ritual. The constraint is self-evident in numerical results.
 *
 * PERSPECTIVAL GAP:
 *   Surprisingly small. All four perspectives classify the constraint as mountain. The perspectival gap that does exist is in experience, not in classification: the satellite technician experiences the constraint as a barrier (cannot predict 1000 years ahead); the space agency experiences it as a tool boundary (here is our valid prediction envelope); the mathematician experiences it as an insight (here is why nonintegrability follows); the computational framework experiences it as a design specification (work within this error budget). But all agree that the constraint is unchangeable, universal, and not subject to extraction or coercion. The mountain classification is robust.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims. All agents experience the three-body limit uniformly as a constraint on their own actions and on what is knowable, not as an extraction mechanism. The directionality derivation is uniform: the constraint is not asymmetric. Different agents have different exit options (technician: trapped, mathematician: analytical), but all trajectories through the phase space follow the same sensitivity rules. The limit is not a zero-sum extraction but a shared structural reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chaos_threshold_precision,
    'Is the predictability limit a consequence of exponential sensitivity (Lyapunov exponents) or of fundamental measurement precision constraints in quantum mechanics?',
    'Analysis of whether classical Lyapunov exponents fully explain observed prediction horizons, or whether quantum uncertainty enters the classical dynamics through gravitational wave effects or measurement backaction',
    'If purely classical chaos: the constraint is mathematical (mountain). If quantum effects intrude: the constraint is hybrid (physical law + measurement limit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chaos_threshold_precision, empirical, 'Whether the limit stems from classical chaos or includes quantum effects').

omega_variable(
    stability_island_exploitability,
    'Can the Kirkwood gaps and resonance structures in planetary systems be leveraged to extend prediction horizons selectively, or are those structures themselves chaotic?',
    'Study of whether quasi-periodic orbits near resonances permit longer-baseline predictions than generic trajectories; analysis of whether metric stability of resonance structures generalizes',
    'If islands permit extended prediction: constraint is relaxable for special cases (mountain with structure). If all resonances degrade: constraint is absolute (mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_island_exploitability, empirical, 'Whether resonance structures enable selective prediction extension').

omega_variable(
    observer_dependent_timescale,
    'Is the predictability limit observer-independent or does it depend on the required precision of the prediction task?',
    'Parametric study of prediction error growth rates as a function of (1) required accuracy threshold, (2) system scale, (3) measure of ''separatedness'' among the three bodies',
    'If observer-independent: pure mountain. If dependent on required precision: the limit is relational (mountain for some observers, lower-extractiveness constraint for others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observer_dependent_timescale, conceptual, 'Whether the limit is objective or observer-relative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(three_body_problem, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thre_tr_t0, three_body_problem, theater_ratio, 0, 0.08).
narrative_ontology:measurement(thre_tr_t150, three_body_problem, theater_ratio, 150, 0.08).
narrative_ontology:measurement(thre_tr_t300, three_body_problem, theater_ratio, 300, 0.08).

% Extraction over time
narrative_ontology:measurement(thre_be_t0, three_body_problem, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(thre_be_t150, three_body_problem, base_extractiveness, 150, 0.12).
narrative_ontology:measurement(thre_be_t300, three_body_problem, base_extractiveness, 300, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(three_body_problem, information_standard).
narrative_ontology:affects_constraint(three_body_problem, lyapunov_exponent_measurement).
narrative_ontology:affects_constraint(three_body_problem, weather_prediction_horizon).
narrative_ontology:affects_constraint(three_body_problem, molecular_dynamics_timescale).

% DUAL FORMULATION NOTE:
% The three-body problem is often decomposed into specialized constraints for specific domains (orbital mechanics, atmospheric chaos, molecular dynamics). Each domain constraint has its own ε reflecting domain-specific measurement precision and required prediction accuracy. The three-body problem itself is the upstream mathematical constraint (ε=0.12, Mountain) that conditions all downstream domain-specific constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
