% ============================================================================
% CONSTRAINT STORY: period_doubling_universality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_period_doubling_universality, []).

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
 *   constraint_id: period_doubling_universality
 *   human_readable: Period Doubling Route to Chaos — Feigenbaum Universality
 *   domain: dynamical_systems/chaos_theory
 *
 * SUMMARY:
 *   Period-doubling universality describes a phenomenon in which
 *   one-parameter families of smooth dynamical maps exhibit a universal
 *   cascade of bifurcations leading from fixed-point stability to chaos. The
 *   bifurcations occur at parameter values whose spacing contracts by a
 *   universal factor (Feigenbaum's constant δ ≈ 4.669...), independent of the
 *   map's functional form. This property was discovered empirically in the
 *   logistic map by Mitchell Feigenbaum (1978), subsequently observed in
 *   dozens of physical systems (fluid instabilities, lasers, chemical
 *   oscillators, electronic circuits), and rigorously understood through
 *   renormalization-group analysis in dynamical systems theory. The
 *   constraint is the impossibility of avoiding this cascade: any smooth
 *   one-parameter family sufficiently close to a 2-to-1 map MUST exhibit
 *   period-doubling bifurcations with the universal constant. There is no
 *   experimental, computational, or design pathway to circumvent the
 *   phenomenon — only to recognize and exploit it.
 *
 * KEY AGENTS:
 *   - The Dynamical System: The mathematical object itself (smooth map in one-parameter family) — trapped in the bifurcation structure
 *   - Physical Systems: Experimental realizations (lasers, fluids, oscillators) — powerless to avoid the cascade, yet universally exhibit the same constant
 *   - Chaos Theorists: Analytical observers (Feigenbaum, Lanford, Eckmann, Wittwer) — perceive the universality as a mathematical necessity, not a contingent pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(period_doubling_universality, 0.09).
domain_priors:suppression_score(period_doubling_universality, 0.02).
domain_priors:theater_ratio(period_doubling_universality, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(period_doubling_universality, extractiveness, 0.09).
narrative_ontology:constraint_metric(period_doubling_universality, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(period_doubling_universality, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(period_doubling_universality, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(period_doubling_universality, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(period_doubling_universality, mountain).
narrative_ontology:human_readable(period_doubling_universality, "Period Doubling Route to Chaos — Feigenbaum Universality").
narrative_ontology:topic_domain(period_doubling_universality, "dynamical_systems/chaos_theory").

domain_priors:emerges_naturally(period_doubling_universality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The dynamical system itself cannot escape period-doubling bifurcations. Any one-parameter family of smooth maps traversing control parameter space MUST exhibit this cascade. No alternative pathway exists.
constraint_indexing:constraint_classification(period_doubling_universality, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Observing period-doubling in a physical system (laser dynamics, fluid instability, chemical oscillator) cannot escape the phenomenon. The bifurcation cascade appears with Feigenbaum's universal constant (δ ≈ 4.669...) across wildly different physical substrates. This is not a law the experimenter can negotiate with or avoid.
constraint_indexing:constraint_classification(period_doubling_universality, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% From the vantage of dynamical systems theory, period-doubling is a universal property of smooth maps in one-parameter families. Logistic map, tent map, sine map, physical systems — all exhibit the same cascade structure with the same universal constant. This is a theorem, not a contingent empirical pattern.
constraint_indexing:constraint_classification(period_doubling_universality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(period_doubling_universality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(period_doubling_universality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(period_doubling_universality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(period_doubling_universality, ExtMetricName, E),
    domain_priors:suppression_score(period_doubling_universality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(period_doubling_universality),
    narrative_ontology:constraint_metric(period_doubling_universality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(period_doubling_universality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(period_doubling_universality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.09): Minimal. The constraint does not extract from any agent because there is no agent-beneficiary asymmetry — period-doubling is a natural property of dynamical evolution. No power asymmetry, no suppression, no coercion. The low value reflects that this is a description of natural law, not an institutional arrangement. Suppression (0.02): Near zero. The phenomenon operates without coercion or alternative suppression. Physical systems follow their dynamics; there is no 'other choice' being blocked. Theater ratio (0.05): Negligible. The constraint describes a mathematical theorem, not a social performance. The minimal theater reflects that understanding period-doubling requires no ritualistic or performative component — the mathematics is transparent.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify identically as Mountain. This is a uniform-type constraint — no perspectival gap. The forced oscillator cannot escape bifurcation. The experimenter observes the universal constant. The mathematician proves the universality. These are not conflicting views of the same phenomenon; they are the same phenomenon viewed from different epistemic positions, all arriving at the same conclusion: this is an immutable property of smooth dynamical systems.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim structure exists for a mountain-type constraint. Directionality derivation is bypassed — there is no agent-relative extraction to measure. The constraint does not benefit or harm any party; it is a logical/mathematical necessity. The apparent 'powerlessness' of the dynamical system is not oppression but mathematical inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. Period-doubling universality exhibits zero tension between coordination and extraction. The constraint is logically immutable (all perspectives agree on Mountain) and mathematically transparent (theater_ratio ≈ 0.05). There is no risk of mislabeling coordination as extraction or vice versa because the constraint is neither — it is a natural law of dynamical evolution. The absence of mandatrophy confirms the mountain classification: the constraint is what it appears to be.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renormalization_conjecture_vs_proof,
    'Is period-doubling universality a theorem (proven within the smooth-map framework) or a mathematical conjecture validated empirically across domains?',
    'Review status of renormalization-group proofs in rigorous analysis; check Lanford, Eckmann, Wittwer results for completeness and scope of rigor.',
    'If fully proven: universality is mountain (zero degrees of freedom, logically immutable). If conjectural: universality is rope with very high suppression (nearly all observed systems comply, but the mechanism is not fully understood).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renormalization_conjecture_vs_proof, conceptual, 'Whether Feigenbaum universality is a proven theorem or empirically validated conjecture').

omega_variable(
    smoothness_assumption_boundary,
    'Does universality hold for non-smooth maps, discontinuous systems, or stochastic perturbations?',
    'Comparative analysis of period-doubling cascades in piecewise-linear maps, discontinuous systems, and maps with noise; identify where Feigenbaum constant breaks down.',
    'If universality requires smoothness: the constraint is contingent on a mathematical assumption (mountain with caveats). If universality persists in non-smooth cases: the constraint is more fundamental than the smooth-map proof suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smoothness_assumption_boundary, empirical, 'Whether universality persists beyond smooth one-parameter families').

omega_variable(
    physical_substrate_exception_search,
    'Are there any physical systems that undergo period-doubling bifurcations WITHOUT exhibiting Feigenbaum''s universal constant and cascade structure?',
    'Systematic review of period-doubling observations in experimental systems; quantitative measurement of bifurcation point spacing and convergence to δ ≈ 4.669 across domains.',
    'If exceptions found: universality is not truly universal (mountain classification fails). If none found after 50+ years of testing: universality is empirically universal (mountain confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_substrate_exception_search, empirical, 'Whether all physical period-doubling systems exhibit Feigenbaum''s universal constant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(period_doubling_universality, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peri_tr_t0, period_doubling_universality, theater_ratio, 0, 0.02).
narrative_ontology:measurement(peri_tr_t25, period_doubling_universality, theater_ratio, 25, 0.04).
narrative_ontology:measurement(peri_tr_t50, period_doubling_universality, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(peri_be_t0, period_doubling_universality, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(peri_be_t25, period_doubling_universality, base_extractiveness, 25, 0.09).
narrative_ontology:measurement(peri_be_t50, period_doubling_universality, base_extractiveness, 50, 0.09).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(period_doubling_universality, information_standard).
narrative_ontology:affects_constraint(period_doubling_universality, chaos_onset_universality).
narrative_ontology:affects_constraint(period_doubling_universality, renormalization_group_fixed_points).

% DUAL FORMULATION NOTE:
% Period-doubling universality is upstream of more specific chaos-theoretic constraints. The abstract universality of Feigenbaum's constant influences constraints about the onset of chaotic dynamics in specific physical systems. Related but distinct constraint: renormalization-group fixed points (ε ≈ 0.06, also Mountain), which provide the theoretical foundation for understanding why universality emerges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
