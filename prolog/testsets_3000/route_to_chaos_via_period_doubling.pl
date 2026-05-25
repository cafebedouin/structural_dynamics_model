% ============================================================================
% CONSTRAINT STORY: route_to_chaos_via_period_doubling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_route_to_chaos_via_period_doubling, []).

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
 *   constraint_id: route_to_chaos_via_period_doubling
 *   human_readable: Route to Chaos via Period Doubling
 *   domain: nonlinear_dynamics/mathematical_physics
 *
 * SUMMARY:
 *   Period doubling is a natural law of dynamical systems. As a bifurcation
 *   parameter increases in a 1D iterated map with negative Schwarzian
 *   derivative, periodic orbits double their period in a precise sequence:
 *   period 1 → period 2 → period 4 → period 8 → period 2^n → chaos. The
 *   Feigenbaum constant δ ≈ 4.669 describes the rate of accumulation — the
 *   ratio of successive bifurcation intervals remains constant regardless of
 *   the specific map. This universality is a mathematical necessity, not a
 *   contingent fact. No observer can escape it, no system can avoid it, no
 *   institutional arrangement can suppress it. The constraint arises from the
 *   mathematical structure of smooth, continuous functions, not from any
 *   coercive mechanism or suppression of alternatives.
 *
 * KEY AGENTS:
 *   - Mathematical Structure: The constraint itself — intrinsic to nonlinear dynamics (not an agent)
 *   - Physicist/Dynamicist: Observer of the constraint (analytical/civilizational) — cannot escape or renegotiate the bifurcation sequence
 *   - Physical Systems: Instantiations of the constraint (organized/constrained) — dynamical systems exhibiting period doubling when parameters are varied
 *   - Mathematics Community: Institutional codifier of the constraint (institutional/arbitrage) — confirms universality, no alternative frameworks available
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(route_to_chaos_via_period_doubling, 0.12).
domain_priors:suppression_score(route_to_chaos_via_period_doubling, 0.03).
domain_priors:theater_ratio(route_to_chaos_via_period_doubling, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, extractiveness, 0.12).
narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(route_to_chaos_via_period_doubling, mountain).
narrative_ontology:human_readable(route_to_chaos_via_period_doubling, "Route to Chaos via Period Doubling").
narrative_ontology:topic_domain(route_to_chaos_via_period_doubling, "nonlinear_dynamics/mathematical_physics").

domain_priors:emerges_naturally(route_to_chaos_via_period_doubling).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIVERSAL BIFURCATION SEQUENCE (MOUNTAIN) — Period doubling is a natural law of nonlinear dynamics. Independent of system details, parameter values, or observer position. The sequence emerges from the mathematical structure of 1D maps with negative Schwarzian derivative. No degrees of freedom for escape or reframing. Feigenbaum constant (δ ≈ 4.669) is invariant across logistic map, sine map, and all physical realizations. Zero suppression because the constraint is purely structural — no coercion mechanism exists.
constraint_indexing:constraint_classification(route_to_chaos_via_period_doubling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHYSICIST STUDYING DETERMINISTIC SYSTEMS (MOUNTAIN) — Individual researchers cannot escape the period-doubling bifurcation sequence when studying 1D iterated maps or any dynamical system with negative Schwarzian. Whether a physicist wants to study chaos or not, if they engage with parametrized families of unimodal maps, the period-doubling route is inevitable. The constraint is not coercive — it is structural to the mathematical landscape itself. Feigenbaum universality means you will encounter the same bifurcation pattern in different systems because the mathematical structure is unchanged.
constraint_indexing:constraint_classification(route_to_chaos_via_period_doubling, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EXPERIMENTAL DYNAMICIST OBSERVING PHYSICAL CHAOS (MOUNTAIN) — Water wheels, laser cavity dynamics, parametric oscillators, chemical reactions: every physical system governed by unimodal 1D dynamics exhibits period doubling. Experimentalists cannot arrange parameters to avoid the route — it is an invariant property of the system's dynamical structure. Suppression is zero because there is no alternative being withheld; the constraint is not suppressing options, it is the only option available. The cascade proceeds regardless of measurement strategy or engineering design.
constraint_indexing:constraint_classification(route_to_chaos_via_period_doubling, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MATHEMATICS AND PHYSICS COMMUNITY (MOUNTAIN) — Period doubling is established across every framework that examines iterated maps. Institutional acceptance of the constraint is universal because the mathematical proof (Coullet-Tresser, Feigenbaum) closed the logical gap in the 1970s-1980s. No amount of institutional reorganization changes the bifurcation sequence. The Feigenbaum constant is invariant regardless of which university studies it or which funding agency supports the research. Institutional power does not provide arbitrage — all institutions accessing the same parameter space encounter the same constraint.
constraint_indexing:constraint_classification(route_to_chaos_via_period_doubling, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(route_to_chaos_via_period_doubling_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(route_to_chaos_via_period_doubling, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(route_to_chaos_via_period_doubling, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, ExtMetricName, E),
    domain_priors:suppression_score(route_to_chaos_via_period_doubling, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(route_to_chaos_via_period_doubling),
    narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(route_to_chaos_via_period_doubling, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(route_to_chaos_via_period_doubling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Period doubling imposes no cost on any observer — it is a constraint on the mathematical structure of iterated maps, not a mechanism that extracts resources from agents. The low extractiveness reflects that the constraint is purely structural (mathematical) rather than distributive (economic or social). The slight nonzero value accounts for the epistemic cost of understanding and predicting the bifurcation sequence — not extraction per se, but the inevitable consequence of engaging with complex dynamics. Suppression (0.03): Negligible. No alternative routes to chaos are being suppressed for period-doubling dynamics. Other routes exist (intermittency, crisis bifurcation) but are not suppressed — they simply apply to different classes of systems. For 1D unimodal maps, period doubling is not suppressing anything; it is the only available route. Theater ratio (0.15): Very low. Period doubling is not performative. The bifurcation cascade is fully predictable from mathematical analysis; there is no gap between theory and observation that requires theater. Experimental measurements of the Feigenbaum constant in water wheels and laser cavities confirm the mathematical predictions directly. Accessibility collapse (0.92): Very high. Period doubling is inaccessible to circumvention. The only way to avoid it is to change the class of systems studied or change the model — not to stay within the same parameter space. Resistance (0.08): Very low. No meaningful resistance to the constraint exists because no agent or system is being constrained — the constraint is descriptive of the mathematical landscape, not prescriptive over agent behavior.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify identically as Mountain. This is not a perspectival gap — it is a uniform-type constraint where the classification is invariant across all observation positions. The analytical observer, the physicist, the experimentalist, and the institutional community all agree on the classification because the constraint is purely structural. This uniformity is a diagnostic signature of a true natural law: no observer position changes the classification, no alternative framing available.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality does not apply to mountain constraints. The period-doubling constraint has no beneficiaries or victims — it does not extract value from any agent. All agents experience the constraint identically as an immutable structural property. The constraint is not relational; it is not relative to observer position in a way that would produce d values. Period doubling either exists in the dynamics or it does not — there is no asymmetry of power, time horizon, exit options, or scope that would alter its character.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    higher_dimensional_extension,
    'Does the period-doubling route remain a universal natural law in higher-dimensional systems?',
    'Analysis of 2D and higher-dimensional bifurcation sequences; comparison with Lyapunov exponent evolution and strange attractor formation; empirical study of whether the Feigenbaum constant applies to multi-dimensional maps',
    'If period doubling generalizes to higher dimensions with the same universal constant: stronger mountain classification (even more universal). If dimensional dependence introduces variability: possible reclassification as rope (coordination between different dimensional regimes) or rope with context-dependent parameters. Current evidence supports universality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(higher_dimensional_extension, empirical, 'Whether period doubling universality extends to higher-dimensional systems').

omega_variable(
    observability_vs_mathematical_structure,
    'Is period doubling a property of the mathematical model or of the observable physical phenomenon?',
    'Comparative analysis: trace period doubling in model equations vs measurement apparatus output; test whether measurement noise, discretization, or finite precision alters the bifurcation sequence; study systems where observation introduces feedback',
    'If purely mathematical: mountain classification is certain. If observation-dependent: constraint may be rope (coordination of measurement with dynamics) or tangled_rope (mixed coordinate role). For deterministic unimodal maps, evidence strongly supports mathematical rather than observational constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observability_vs_mathematical_structure, empirical, 'Whether period doubling is intrinsic to dynamics or dependent on observation method').

omega_variable(
    chaos_onset_as_physical_phase_transition,
    'Is the period-doubling cascade to chaos a fundamental phase transition in physical systems, or merely a mathematical artifact of idealized models?',
    'Study of period-doubling accumulation in experimental systems with noise, nonlinearity, and dissipation; measurement of critical exponents near chaos onset; determination of whether Feigenbaum constant appears in real physical systems or only in abstract maps',
    'If Feigenbaum constant emerges in realistic physical systems: mountain classification is strengthened (natural law). If period doubling is destroyed by realistic conditions: reclassification as piton (mathematically performative, physically inertial). Current experimental evidence (water wheels, laser dynamics) supports emergence in realistic systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chaos_onset_as_physical_phase_transition, empirical, 'Whether period doubling manifests as fundamental phase transition in physical systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(route_to_chaos_via_period_doubling, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rtc_tr_t0, route_to_chaos_via_period_doubling, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rtc_tr_t50, route_to_chaos_via_period_doubling, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(rtc_be_t0, route_to_chaos_via_period_doubling, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(rtc_be_t50, route_to_chaos_via_period_doubling, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(route_to_chaos_via_period_doubling, information_standard).
narrative_ontology:affects_constraint(route_to_chaos_via_period_doubling, logistic_map_bifurcation_universality).
narrative_ontology:affects_constraint(route_to_chaos_via_period_doubling, chaos_from_deterministic_systems).

% DUAL FORMULATION NOTE:
% Period doubling is upstream of specific chaotic systems (Lorenz attractor, logistic map dynamics, laser cavities). Those specific systems instantiate the period-doubling constraint as part of their bifurcation structure. The period-doubling constraint itself is a universal mathematical principle independent of any particular physical realization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
