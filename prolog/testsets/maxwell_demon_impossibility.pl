% ============================================================================
% CONSTRAINT STORY: maxwell_demon_impossibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maxwell_demon_impossibility, []).

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
 *   constraint_id: maxwell_demon_impossibility
 *   human_readable: Maxwell Demon Impossibility (Second Law of Thermodynamics)
 *   domain: physics/thermodynamics
 *
 * SUMMARY:
 *   The Maxwell Demon impossibility represents a fundamental constraint on
 *   physical systems: no process can decrease the entropy of an isolated
 *   system. This constraint has been proven through multiple independent
 *   routes — classical thermodynamic arguments, Szilard's
 *   information-theoretic analysis, Bennett's computational mechanics, and
 *   quantum measurement coupling — all converging on the same conclusion. The
 *   constraint is invariant across all observables and measurement bases:
 *   whether analyzed thermodynamically, informationally, or
 *   quantum-mechanically, the impossibility persists. No degree of freedom,
 *   information access, or computational power can circumvent it. This makes
 *   it a canonical mountain: unchangeable, universally applicable, zero
 *   degrees of freedom.
 *
 * KEY AGENTS:
 *   - Isolated Physical System: The system governed by the constraint. Entropy of the isolated system cannot decrease under any internal process.
 *   - Maxwell's Hypothetical Demon: Thought experiment agent with perfect information and arbitrary control. Even with maximal power and knowledge, cannot violate the constraint.
 *   - Analytical Observer: Universal perspective across all formulations (thermodynamic, information-theoretic, quantum) confirming the invariance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maxwell_demon_impossibility, 0.08).
domain_priors:suppression_score(maxwell_demon_impossibility, 0.03).
domain_priors:theater_ratio(maxwell_demon_impossibility, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maxwell_demon_impossibility, extractiveness, 0.08).
narrative_ontology:constraint_metric(maxwell_demon_impossibility, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(maxwell_demon_impossibility, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maxwell_demon_impossibility, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(maxwell_demon_impossibility, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maxwell_demon_impossibility, mountain).
narrative_ontology:human_readable(maxwell_demon_impossibility, "Maxwell Demon Impossibility (Second Law of Thermodynamics)").
narrative_ontology:topic_domain(maxwell_demon_impossibility, "physics/thermodynamics").

domain_priors:emerges_naturally(maxwell_demon_impossibility).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED SYSTEM (MOUNTAIN) — Cannot violate entropy increase regardless of observer position, information capacity, or measurement precision. The constraint is absolute from the ground-level perspective.
constraint_indexing:constraint_classification(maxwell_demon_impossibility, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MAXWELL DEMON (MOUNTAIN) — Even with perfect information and arbitrary physical power, cannot circumvent the constraint. High agency does not overcome immutable law. Demonstrates that the constraint is not dependent on agent capacity.
constraint_indexing:constraint_classification(maxwell_demon_impossibility, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From information-theoretic, thermodynamic, and quantum mechanical perspectives, all known resolutions converge on the same conclusion: entropy cannot decrease in an isolated system. The constraint is invariant across measurement basis and analytical frame.
constraint_indexing:constraint_classification(maxwell_demon_impossibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maxwell_demon_impossibility_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(maxwell_demon_impossibility, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maxwell_demon_impossibility, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(maxwell_demon_impossibility, ExtMetricName, E),
    domain_priors:suppression_score(maxwell_demon_impossibility, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(maxwell_demon_impossibility),
    narrative_ontology:constraint_metric(maxwell_demon_impossibility, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(maxwell_demon_impossibility, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(maxwell_demon_impossibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint has no extraction mechanism — it does not transfer resources from one agent to another or enable asymmetric advantage. It is a pure natural law that applies uniformly. The low value reflects that there is no beneficiary or victim; the constraint simply is. Suppression (0.03): Minimal. The constraint does not suppress alternatives; it eliminates the impossible option entirely. There are no alternatives to suppress — the option (spontaneous decrease of entropy in isolation) is logically/physically void. Theater ratio (0.15): Very low. The constraint has no performative content. Its enforcement is absolute and immediate; no theater is required or possible. The small nonzero value reflects only the minimal formalism needed to state the constraint precisely, not functional theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap. All three perspectives — ground-level, agent with high power, analytical observer — produce identical mountain classification. The uniformity is diagnostic of a true natural law. No observer position creates an alternative reading. The constraint is invariant under all transformations of perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. Mountain constraints have no directionality derivation. The constraint is not derived from beneficiary/victim relationships or power asymmetries. It applies identically regardless of the observer's position in power space or their exit options. The absence of directionality (d undefined for mountain) is itself confirmation of the mountain classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_thermodynamic_identity,
    'Is the impossibility a fundamental law of thermodynamics or a logical consequence of information theory?',
    'Historical analysis of Maxwell''s original formulation, Szilard''s analysis, Landauer''s principle, and Bennett''s resolution. Determination of whether information-theoretic entropy and thermodynamic entropy refer to the same quantity or are distinct mathematical objects that happen to obey parallel laws.',
    'If identical: the constraint is thermodynamic at base level. If distinct: the constraint represents a deep connection between information and physical law, possibly suggesting different mechanisms at different scales.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_thermodynamic_identity, conceptual, 'Whether the impossibility is thermodynamic or information-theoretic in origin').

omega_variable(
    quantum_measurement_coupling,
    'Does quantum measurement decoherence provide the unique sufficient mechanism for entropy increase, or are there classical information-theoretic arguments that work independently?',
    'Review of Szilard, Bennett, and post-quantum-mechanics resolutions. Determination of whether classical thermodynamics requires quantum mechanics for its proofs or whether classical mechanics with sufficient detail can show the demon''s impossibility.',
    'If quantum-dependent: the constraint relies on measurement mechanics. If classical-sufficient: the constraint is a property of macroscopic physics independent of microscopic detail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_measurement_coupling, empirical, 'Role of quantum measurement in entropy constraint').

omega_variable(
    negentropy_extraction_distinction,
    'Can a hypothetical system extract order from chaos at the cost of producing disorder elsewhere, or is the constraint absolute across all distributed configurations?',
    'Rigorous thermodynamic accounting of all degrees of freedom in systems claiming to demonstrate negentropy extraction (biological systems, self-organizing structures, Benard cells). Verification that entropy production in dissipative mechanism exceeds entropy decrease in organized subsystem.',
    'If strict accounting confirms always positive total entropy: mountain classification holds universally. If distributed entropy accounting reveals edge cases: constraint may be tangled_rope (genuine order creation coupled to greater disorder production) rather than pure mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negentropy_extraction_distinction, empirical, 'Whether entropy decrease in subsystems requires compensating entropy increase elsewhere').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maxwell_demon_impossibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maxdem_tr_t0, maxwell_demon_impossibility, theater_ratio, 0, 0.12).
narrative_ontology:measurement(maxdem_tr_t5, maxwell_demon_impossibility, theater_ratio, 5, 0.14).
narrative_ontology:measurement(maxdem_tr_t10, maxwell_demon_impossibility, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(maxdem_be_t0, maxwell_demon_impossibility, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(maxdem_be_t5, maxwell_demon_impossibility, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(maxdem_be_t10, maxwell_demon_impossibility, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maxwell_demon_impossibility, information_standard).
narrative_ontology:affects_constraint(maxwell_demon_impossibility, perpetual_motion_impossibility).
narrative_ontology:affects_constraint(maxwell_demon_impossibility, carnot_efficiency_limit).
narrative_ontology:affects_constraint(maxwell_demon_impossibility, arrow_of_time_asymmetry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
