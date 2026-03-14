% ============================================================================
% CONSTRAINT STORY: reversible_computing_limits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reversible_computing_limits, []).

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
 *   constraint_id: reversible_computing_limits
 *   human_readable: Physical Limits of Reversible Computing
 *   domain: physics/computation
 *
 * SUMMARY:
 *   Reversible computing is the theoretical and practical project of
 *   designing computational systems in which no logical information is
 *   irreversibly discarded during the computation. Landauer's principle
 *   (1961) establishes a fundamental thermodynamic limit: any computation
 *   that logically erases information must dissipate at least kT ln(2) joules
 *   of heat per bit erased, where k is Boltzmann's constant and T is
 *   temperature. This constraint is not an engineering limitation of current
 *   technology but a consequence of the second law of thermodynamics. The
 *   constraint's extractiveness is minimal (ε=0.12) because it is a boundary
 *   condition on what is physically possible, not an imposed extraction
 *   mechanism. No agent benefits from the constraint, and no agent bears
 *   extraction — it simply defines the physical namespace within which
 *   reversible computing must operate. All perspectives classify as Mountain
 *   because the constraint is invariant: it applies equally regardless of
 *   observer position, time horizon, exit options, or spatial scope.
 *
 * KEY AGENTS:
 *   - Computational physicists: Analyze the theoretical limits; cannot exit the constraint but can choose to study other problems
 *   - Semiconductor manufacturers: Constrained by physical principles; can engineer around specific barriers but not fundamentally evade thermodynamic law
 *   - Energy-constrained systems (mobile, IoT, space): Experience the constraint as immutable boundary on operational efficiency
 *   - Quantum computing researchers: Explore whether quantum coherence provides loopholes; still bound by universal thermodynamic principles
 *   - The theoretical physics community: Maintains and extends the foundational derivations from which the constraint follows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reversible_computing_limits, 0.12).
domain_priors:suppression_score(reversible_computing_limits, 0.03).
domain_priors:theater_ratio(reversible_computing_limits, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reversible_computing_limits, extractiveness, 0.12).
narrative_ontology:constraint_metric(reversible_computing_limits, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(reversible_computing_limits, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reversible_computing_limits, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(reversible_computing_limits, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reversible_computing_limits, mountain).
narrative_ontology:human_readable(reversible_computing_limits, "Physical Limits of Reversible Computing").
narrative_ontology:topic_domain(reversible_computing_limits, "physics/computation").

domain_priors:emerges_naturally(reversible_computing_limits).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal/civilizational frame, reversible computing is constrained by fundamental thermodynamics. Landauer's principle (1961) establishes that logically irreversible operations must dissipate minimum energy kT ln(2) per bit erased. This is not a law of any particular engineering paradigm but a consequence of the second law of thermodynamics and information theory. The constraint is invariant across all proposed computing substrates and measurement methodologies.
constraint_indexing:constraint_classification(reversible_computing_limits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SEMICONDUCTOR INDUSTRY (MOUNTAIN) — Even from the perspective of well-resourced actors, reversible computing limits appear as fixed physical law. Industry can engineer around specific implementation barriers (heat dissipation, timing synchronization) but cannot escape the fundamental entropy cost of logical irreversibility. The constraint is non-negotiable regardless of capital investment or technological maturity.
constraint_indexing:constraint_classification(reversible_computing_limits, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: ENERGY-CONSTRAINED DEVICE (MOUNTAIN) — From the perspective of a mobile phone, IoT sensor, or spacecraft constrained by power budget, reversible computing limits are immutable. The device cannot escape Landauer's principle through any available action. The constraint appears with maximum structural rigidity: no power means no computation, regardless of the device's structural position.
constraint_indexing:constraint_classification(reversible_computing_limits, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: RESEARCH INSTITUTION (MOUNTAIN) — Physics research groups studying reversible computing may benefit from grant funding and publication prestige, but they cannot arbitrage away the fundamental constraint. The thermodynamic law applies identically to their research as to industrial applications. Their 'exit' — choosing to study a different problem — sidesteps the constraint rather than falsifying it.
constraint_indexing:constraint_classification(reversible_computing_limits, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reversible_computing_limits_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(reversible_computing_limits, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reversible_computing_limits, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reversible_computing_limits, ExtMetricName, E),
    domain_priors:suppression_score(reversible_computing_limits, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reversible_computing_limits),
    narrative_ontology:constraint_metric(reversible_computing_limits, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reversible_computing_limits, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reversible_computing_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.12): Minimal. The constraint does not extract from any agent — it is a boundary condition on physical possibility. The non-zero value reflects the minimal epistemic labor required to understand and work within the constraint, not any asymmetric cost distribution. Suppression (0.03): Negligible. There are no suppression mechanisms because there are no alternatives to escape. The constraint is not suppressive; it is simply physics. Theater ratio (0.15): Low. Reversible computing research does not perform or simulate the constraint — it works directly within it. The small theatrical component reflects pedagogical exposition and theoretical justification, not functional masking. Accessibility collapse (0.89): Very high. The only way to 'access an alternative' to Landauer's principle would be to discover that the second law of thermodynamics is not universal — a collapse so total that accessibility is functionally zero. Resistance (0.08): Very low. There is no institutional, economic, or technical resistance preventing the constraint from operating — it operates necessarily. The small non-zero value reflects the lag between theoretical prediction and empirical verification, now largely closed by 60+ years of experimental physics.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on Mountain classification because the constraint is truly invariant. There is no perspectival gap — this is the defining property of a natural law constraint. An agent with vast resources (powerful + arbitrage) sees the same boundary as a powerless trapped agent. A research institution studying reversible computing sees the same thermodynamic floor as a smartphone manufacturer. A civilizational/universal observer sees exactly what a biographical/immediate observer sees. The absence of perspectival divergence is the diagnostic signal that the classification is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) does not apply meaningfully to Mountain constraints because there is no beneficiary-victim asymmetry. The constraint is not imposed by one agent on another; it is a property of physical law. No derivative d value exists for Mountain classification. The constraint affects all agents symmetrically: they all face the same thermodynamic floor, and none can arbitrage around it.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. Reversible computing limits does not exhibit the classification ambiguity that mandatrophy addresses. The constraint is not misclassified as a higher-order coordination mechanism when it is actually extraction, or vice versa. It is not an extractive mechanism masquerading as coordination. It is simply a boundary condition, and all perspectives recognize it as such. This is the canonical structure of a genuine Mountain constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_vs_classical_boundary,
    'Does Landauer''s principle apply identically to quantum and classical reversible computation, or do quantum systems exhibit loopholes via entanglement and coherence?',
    'Rigorous derivation of information-theoretic bounds in quantum phase space; experimental demonstration of quantum reversible gates with heat dissipation below Landauer floor',
    'If quantum loopholes exist: reversible quantum computing might achieve lower-energy operation than classical bound predicts. If principle holds: quantum systems face identical thermodynamic floor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_vs_classical_boundary, empirical, 'Applicability of Landauer principle to quantum reversible computation').

omega_variable(
    implementation_vs_fundamental_limit,
    'Is the measured heat dissipation in reversible computing experiments limited by fundamental Landauer principle or by engineering inefficiency (adiabatic switching overhead, parasitic heat channels)?',
    'Comparison of measured dissipation vs theoretical Landauer floor across multiple implementation technologies (photonic, superconducting, trapped-ion); isolation of implementation losses via precision calorimetry',
    'If gap is engineering: improvements in switching speed and coupling efficiency can approach the floor. If gap is fundamental: Landauer bound is even tighter than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_vs_fundamental_limit, empirical, 'Whether dissipation gap reflects implementation losses or fundamental limits').

omega_variable(
    information_erasure_definition,
    'Can information be ''practically irreversible'' (logically one-way but thermodynamically recoverable via work input) without triggering Landauer dissipation?',
    'Analysis of hybrid reversible-irreversible architectures; measurement of energy cost to reverse ''lossy'' operations; theoretical bounds on work extraction from partial information recovery',
    'If practical irreversibility bypasses Landauer: the constraint is narrower than classical theory suggests. If all irreversibility has identical cost: constraint is fundamental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_erasure_definition, conceptual, 'Whether practical irreversibility can avoid Landauer dissipation requirement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reversible_computing_limits, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(revcomp_tr_t0, reversible_computing_limits, theater_ratio, 0, 0.15).
narrative_ontology:measurement(revcomp_tr_t20, reversible_computing_limits, theater_ratio, 20, 0.15).
narrative_ontology:measurement(revcomp_tr_t40, reversible_computing_limits, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(revcomp_be_t0, reversible_computing_limits, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(revcomp_be_t20, reversible_computing_limits, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(revcomp_be_t40, reversible_computing_limits, base_extractiveness, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reversible_computing_limits, information_standard).
narrative_ontology:affects_constraint(reversible_computing_limits, thermodynamic_computation_efficiency).
narrative_ontology:affects_constraint(reversible_computing_limits, quantum_coherence_overhead).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
