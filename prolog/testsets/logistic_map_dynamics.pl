% ============================================================================
% CONSTRAINT STORY: logistic_map_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_logistic_map_dynamics, []).

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
 *   constraint_id: logistic_map_dynamics
 *   human_readable: The Logistic Map (Bifurcation and Chaos)
 *   domain: mathematical/biological
 *
 * SUMMARY:
 *   The logistic map x_{n+1} = r * x_n * (1 - x_n) is a canonical exemplar of
 *   a mathematical mountain constraint. It demonstrates how low-dimensional
 *   nonlinear dynamics necessarily generate bifurcation cascades leading to
 *   chaos as a control parameter (r) increases. The constraint is invariant
 *   across domains: whether applied to rabbit populations, bacterial growth,
 *   laser dynamics, or economic models, the same bifurcation structure and
 *   route to chaos emerges. The constraint imposes zero degrees of freedom on
 *   observers — it cannot be negotiated, modified, or circumvented through
 *   institutional arrangement. The extractiveness is minimal (0.12) because
 *   there is no asymmetric extraction: all observers, regardless of power or
 *   position, confront the identical mathematical truth. The suppression is
 *   minimal (0.03) because no alternative exists to suppress — the
 *   bifurcation structure is the only possible behavior of this system. The
 *   theater ratio is minimal (0.15) because the constraint is purely
 *   functional: all activity dedicated to understanding, predicting, or
 *   applying the logistic map contributes directly to epistemic yield with
 *   negligible performative overhead.
 *
 * KEY AGENTS:
 *   - Mathematical System: The logistic map itself — the constraint, not an agent, but the object constraining all agents
 *   - Analytical Observer: Mathematician or theoretical physicist studying the map's properties in the abstract
 *   - Applied Modeler: Biologist, economist, or engineer using the logistic map to model a real system
 *   - Empirical System: The actual biological population, economic variable, or physical system being modeled
 *   - Universal Reason: Civilizational perspective — commitment to following logical necessity wherever it leads
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(logistic_map_dynamics, 0.12).
domain_priors:suppression_score(logistic_map_dynamics, 0.03).
domain_priors:theater_ratio(logistic_map_dynamics, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, extractiveness, 0.12).
narrative_ontology:constraint_metric(logistic_map_dynamics, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(logistic_map_dynamics, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(logistic_map_dynamics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(logistic_map_dynamics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(logistic_map_dynamics, mountain).
narrative_ontology:human_readable(logistic_map_dynamics, "The Logistic Map (Bifurcation and Chaos)").
narrative_ontology:topic_domain(logistic_map_dynamics, "mathematical/biological").

domain_priors:emerges_naturally(logistic_map_dynamics).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIVERSAL MATHEMATICAL OBSERVER (MOUNTAIN) — From first principles, the logistic map's dynamics are determined entirely by the recurrence relation x_{n+1} = r * x_n * (1 - x_n). The transition from fixed points to periodic orbits to chaos as r increases is a structural inevitability of the nonlinear feedback loop. This is not contingent institutional arrangement — it is mathematical fact, true in all universes where quadratic polynomials behave as they do.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED MODELER (MOUNTAIN) — Whether modeling population dynamics, economic fluctuations, or neural firing patterns, the constraints imposed by the logistic map's bifurcation structure are immutable within the class of models using this functional form. The modeler cannot escape the onset of chaos by choosing different parameters; the chaotic regime exists as an inexorable feature. Even mobile agents with alternative modeling frameworks encounter equivalent constraints in other nonlinear systems.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EMPIRICAL BIOLOGIST (MOUNTAIN) — A population biologist studying a species whose growth follows logistic dynamics confronts the bifurcation structure as an immutable constraint on population stability. The window of r values that produce stable equilibria or periodic cycles is fixed by the mathematics. There is no institutional workaround, no negotiation with nature. The constraint appears as an irreducible property of how populations behave under density-dependent regulation.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE POPULATION INSTANCE (MOUNTAIN) — From the perspective of an actual biological population subject to logistic growth with bifurcating parameter, the dynamics are inscrutable and inescapable. The population evolves according to the constraint with zero degrees of freedom. Whether the species experiences stable equilibrium or chaotic boom-bust cycles depends entirely on growth-rate parameters determined by ecology, not by the population's choices or preferences.
constraint_indexing:constraint_classification(logistic_map_dynamics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(logistic_map_dynamics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(logistic_map_dynamics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(logistic_map_dynamics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(logistic_map_dynamics, ExtMetricName, E),
    domain_priors:suppression_score(logistic_map_dynamics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(logistic_map_dynamics),
    narrative_ontology:constraint_metric(logistic_map_dynamics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(logistic_map_dynamics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(logistic_map_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The logistic map imposes no extraction in the sense of asymmetric benefit. All observers gain the same epistemic value from understanding the bifurcation structure — the mathematical truth is universally accessible. There is no distribution of benefit (beneficiaries) vs cost (victims) because the constraint is purely structural. The 0.12 value reflects minimal overhead in computation, representation, and empirical application. Suppression (0.03): Minimal. There are no suppressed alternatives — the logistic map's behavior is uniquely determined by the recurrence relation. No agent benefits from ignorance of the bifurcation structure. No coercion is required to enforce the constraint because it operates through logical necessity, not through institutional power. Theater ratio (0.15): Minimal. All activity related to the logistic map (mathematical analysis, numerical simulation, empirical testing) contributes directly to functional understanding. There is negligible performative content — no ritual, no signaling, no coordination theater. The small nonzero value reflects necessary overhead in representation and communication, not any degradation of function.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, this constraint produces MINIMAL perspectival gap. All observers, from the analytical mathematician to the powerless population instance, confront the identical structural constraint. The mathematical modeler and the biologist agree on the bifurcation structure and onset of chaos. The population cannot negotiate or circumvent the dynamics. The mountain classification is invariant across all (P,T,E,S) tuples because the constraint is purely mathematical — independent of power level, time horizon, exit options, or spatial scope. This invariance is the hallmark of a true mountain: no matter how you observe it, the classification does not change. The minimal perspectival gap confirms the mountain classification and eliminates mandatrophy risk.
 *
 * DIRECTIONALITY LOGIC:
 *   The logistic map has no beneficiaries or victims in the structural sense. All agents are neutral observers of a mathematical truth. The directionality parameter d is undefined or canonical (neutral) across all perspectives because there is no extraction flow. The constraint does not favor any agent or group — it constrains all equally. This absence of differential benefit is diagnostic for mountain classification. If the logistic map were contingent (e.g., a useful but replaceable model, a coordination mechanism that could be negotiated away), we would expect to see beneficiaries (those who benefit from the model's simplicity or predictive power) and victims (those trapped by the model's limitations). But the mathematical structure is inevitable, not negotiable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_applicability_limit,
    'Is the logistic map a true universal law of nonlinear systems, or a stylized approximation whose predictive validity varies with domain?',
    'Empirical: compare bifurcation predictions of logistic map against actual data in biological populations, economic time series, and physical systems. Does the predicted onset of chaos occur at r≈3.57 in real systems?',
    'If universal: mountain classification confirmed — the constraint is a fundamental feature of nonlinearity. If domain-dependent: the constraint becomes a rope or scaffold — a useful coordination framework that breaks down outside its domain of applicability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_applicability_limit, empirical, 'Whether logistic bifurcation structure appears universally across domains or is model-specific').

omega_variable(
    measurement_discretization_invariance,
    'Does the bifurcation and chaos structure depend on the discretization (continuous vs discrete time) or is it a fundamental property of the underlying dynamics?',
    'Mathematical: compare continuous-time analog (logistic differential equation dx/dt = rx(1-x)) against discrete-time logistic map. Empirical: measure whether real biological populations exhibit discrete or continuous population updates, and whether observed dynamics match the discrete logistic map or require continuous formulation.',
    'If invariant to discretization: mountain classification strengthened — the constraint is robust across formulations. If dependent: the discrete logistic map is a convenient model (rope) but the underlying constraint is the continuous dynamics (mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_discretization_invariance, empirical, 'Whether bifurcation structure is invariant to continuous/discrete formulation choice').

omega_variable(
    chaos_observability_in_finite_systems,
    'In finite populations (say, N < 1000 individuals), is the chaotic regime of the logistic map empirically observable, or do stochastic effects overwhelm the deterministic chaos?',
    'Empirical: measure actual population time series in laboratory-controlled species (Drosophila, yeast, algae) across the bifurcation range. Compare observed trajectory variance against deterministic logistic map predictions. Identify the critical population size below which chaos becomes indistinguishable from noise.',
    'If chaos observable in realistic population sizes: mountain classification holds for biological applications. If stochasticity dominates: the logistic map is a mountain in the mathematical limit, but becomes a rope (useful coordination framework) or scaffold (useful model with practical sunset) in biological domains with finite populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chaos_observability_in_finite_systems, empirical, 'Whether chaotic bifurcations are observable in finite biological populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(logistic_map_dynamics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(logmap_tr_t0, logistic_map_dynamics, theater_ratio, 0, 0.1).
narrative_ontology:measurement(logmap_tr_t5, logistic_map_dynamics, theater_ratio, 5, 0.12).
narrative_ontology:measurement(logmap_tr_t10, logistic_map_dynamics, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(logmap_be_t0, logistic_map_dynamics, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(logmap_be_t5, logistic_map_dynamics, base_extractiveness, 5, 0.11).
narrative_ontology:measurement(logmap_be_t10, logistic_map_dynamics, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(logistic_map_dynamics, information_standard).
narrative_ontology:affects_constraint(logistic_map_dynamics, lorenz_attractor_chaos).
narrative_ontology:affects_constraint(logistic_map_dynamics, period_doubling_universality).

% DUAL FORMULATION NOTE:
% The logistic map is upstream of more complex chaotic systems (Lorenz attractor, Hénon map). The bifurcation structure and route to chaos visible in the logistic map appear universally in nonlinear dynamical systems. The network links this constraint to the period-doubling route to chaos and to universality classes in chaos theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
