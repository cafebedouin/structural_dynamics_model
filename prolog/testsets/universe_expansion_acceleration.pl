% ============================================================================
% CONSTRAINT STORY: universe_expansion_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_universe_expansion_acceleration, []).

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
 *   constraint_id: universe_expansion_acceleration
 *   human_readable: Universe Expansion Acceleration
 *   domain: cosmology/observational_physics
 *
 * SUMMARY:
 *   The accelerating expansion of the universe, discovered observationally in
 *   1998 through Type Ia supernovae distance measurements, represents a
 *   candidate for a pure natural law constraint. The universe's metric
 *   expansion rate is increasing over time, driven by an unknown entity
 *   termed 'dark energy' that comprises ~68% of the cosmic energy density.
 *   This constraint is not negotiable, not extractive, and not a coordination
 *   mechanism among institutional actors. It is a discovered property of the
 *   physical universe itself. The constraint exhibits zero degrees of
 *   freedom: all observers, regardless of their institutional position or
 *   epistemic framework, experience the same accelerating expansion. This
 *   makes it an exemplar of a mountain-only constraint — a natural law that
 *   classifies identically from all perspectives because it is not a human
 *   social arrangement.
 *
 * KEY AGENTS:
 *   - The Physical Universe: Not an agent in the DR sense but the system exhibiting the constraint. Expansion acceleration is an intrinsic geometric property.
 *   - Observing Cosmologists: Empirical observers who measure the acceleration through supernovae, baryon acoustic oscillations, and other probes. They discover but do not create the constraint.
 *   - Theoretical Physicists: Interpret the measurements and propose mechanisms (cosmological constant, quintessence, modified gravity). Their theories do not change the underlying fact.
 *   - The Analytical Observer: A civilizational perspective that recognizes the acceleration as an immutable property of spacetime evolution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(universe_expansion_acceleration, 0.12).
domain_priors:suppression_score(universe_expansion_acceleration, 0.02).
domain_priors:theater_ratio(universe_expansion_acceleration, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(universe_expansion_acceleration, extractiveness, 0.12).
narrative_ontology:constraint_metric(universe_expansion_acceleration, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(universe_expansion_acceleration, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(universe_expansion_acceleration, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(universe_expansion_acceleration, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(universe_expansion_acceleration, mountain).
narrative_ontology:human_readable(universe_expansion_acceleration, "Universe Expansion Acceleration").
narrative_ontology:topic_domain(universe_expansion_acceleration, "cosmology/observational_physics").

domain_priors:emerges_naturally(universe_expansion_acceleration).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PHYSICAL UNIVERSE (MOUNTAIN) — The expansion acceleration is an intrinsic property of spacetime geometry. No agent can exit this constraint; it is not a choice or institutional arrangement. The universe's metric expansion is determined by the Einstein field equations coupled to the energy density and equation of state of the cosmos. This is an unchangeable physical law.
constraint_indexing:constraint_classification(universe_expansion_acceleration, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE OBSERVING COSMOLOGIST (MOUNTAIN) — From the vantage of an experimental cosmologist studying Type Ia supernovae redshifts, the accelerating expansion appears as an invariant physical property. The measurement reveals a fact about the universe's geometry, not a constraint imposed by human institutions or power structures. The cosmologist has no choice in the matter — the data either shows acceleration or it does not. The constraint is discovered, not negotiated.
constraint_indexing:constraint_classification(universe_expansion_acceleration, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational timescale and universal scope, cosmic expansion acceleration is an immutable property of the observable universe. It is not a snare designed to extract value from any agent, nor a rope coordinating behavior among institutional actors. It is a natural law — a fact about how the universe evolves. The constraint has zero degrees of freedom: all observers, regardless of power or exit options, experience the same accelerating metric expansion.
constraint_indexing:constraint_classification(universe_expansion_acceleration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(universe_expansion_acceleration_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(universe_expansion_acceleration, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(universe_expansion_acceleration, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(universe_expansion_acceleration, ExtMetricName, E),
    domain_priors:suppression_score(universe_expansion_acceleration, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(universe_expansion_acceleration),
    narrative_ontology:constraint_metric(universe_expansion_acceleration, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(universe_expansion_acceleration, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(universe_expansion_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The accelerating expansion does not extract value from any agent — it is not a constraint designed to benefit one party over another. The low value reflects that this is a brute fact of physics, not a social arrangement. The non-zero value (rather than 0.0) accounts for the epistemological point that all knowledge has a cost — understanding the acceleration requires resources (telescopes, computation, professional attention). But this cognitive overhead is not extractive in the DR sense. Suppression (0.02): Negligible. There are no alternatives to the constraint — the universe expands as it does. No agent can suppress alternatives because there is only one universe with one expansion history. Theater ratio (0.08): Minimal. The constraint exhibits high functional clarity — the accelerating expansion is directly observable and theoretically tractable. Performative content is negligible. The measurement program does not involve ritual or theater; it directly probes the constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap. All perspectives classify as Mountain because the constraint is not dependent on observer position, power level, exit options, or temporal scale. A powerless agent trapped in the universe experiences the same acceleration as an institutional actor with theoretical mobility. The civilizational analytical observer and the individual cosmologist measure the same effect. This invariance across all (P,T,E,S) tuples is the diagnostic signature of a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint because there is no extraction relationship. The chi formula χ = ε × f(d) × σ(S) does not apply to natural laws — the expansion acceleration is not an extractive relationship between agents. All beneficiary/victim designations are vacuous. The constraint is immutable from all perspectives because it is a property of spacetime itself, not a social arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits no mandatrophy because it is a uniform-type (mountain-only) constraint with zero perspectival gap. The classification is robust across all observables and all observer positions. There is no risk of misclassifying coordination as extraction or extraction as coordination — the constraint is neither. It is a natural law that all observers must accept as immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dark_energy_mechanism,
    'Is the observed acceleration caused by a cosmological constant (Λ), dynamical dark energy (quintessence), or modified gravity?',
    'Precision cosmological measurements: supernova distance moduli, baryon acoustic oscillations, cosmic microwave background anisotropies, weak gravitational lensing, and growth rate of structure.',
    'If cosmological constant (Λ=0): acceleration is a brute fact requiring no further explanation — mountain classification is robust. If dark energy (dynamical): the mechanism is unknown but still immutable given current physics. If modified gravity: the constraint might be reframed as institutional (an artifact of our theory-choice) rather than natural — but observational equivalence makes this distinction moot.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dark_energy_mechanism, empirical, 'Whether accelerating expansion derives from Λ, dark energy, or modified gravity').

omega_variable(
    measurement_methodology_circularity,
    'Do Type Ia supernovae distance measurements assume a standard Λ-dominated cosmology, potentially circularly confirming the acceleration?',
    'Model-independent reconstruction of the cosmic expansion history using baryon acoustic oscillations and model-independent parameterization techniques. Comparison with gravitational lensing time-delay measurements and growth-rate estimates.',
    'If measurement methodology circularly assumes acceleration: the constraint might be partly epistemic artifact rather than pure natural law. If independent probes (BAO, lensing, growth) confirm acceleration: mountain classification is confirmed from multiple observable bases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_methodology_circularity, empirical, 'Whether Type Ia supernova measurements assume rather than discover acceleration').

omega_variable(
    fine_tuning_contingency,
    'Is the observed acceleration (Ω_Λ ≈ 0.68) a necessary feature of the universe or an improbable contingency requiring explanation?',
    'Multiverse theories (eternal inflation, string landscape) and anthropic reasoning; comparison of observed Ω_Λ to predicted distributions in high-energy physics models.',
    'If necessary: acceleration is a natural law. If contingent: the constraint might be reframed as a selection bias (anthropic principle) rather than a law. But even if contingent, the constraint remains immutable from the perspective of any single universe — the acceleration is still a fixed property we must accept.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fine_tuning_contingency, conceptual, 'Whether acceleration is necessary or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(universe_expansion_acceleration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(univ_tr_t0, universe_expansion_acceleration, theater_ratio, 0, 0.08).
narrative_ontology:measurement(univ_tr_t5, universe_expansion_acceleration, theater_ratio, 5, 0.08).
narrative_ontology:measurement(univ_tr_t10, universe_expansion_acceleration, theater_ratio, 10, 0.08).

% Extraction over time
narrative_ontology:measurement(univ_be_t0, universe_expansion_acceleration, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(univ_be_t5, universe_expansion_acceleration, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(univ_be_t10, universe_expansion_acceleration, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(universe_expansion_acceleration, information_standard).

% DUAL FORMULATION NOTE:
% This constraint stands alone and does not decompose into distinct observables with different ε values. The accelerating expansion is a single, unified phenomenon measured consistently across all detection methods (Type Ia supernovae, baryon acoustic oscillations, cosmic microwave background, gravitational lensing time delays).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
