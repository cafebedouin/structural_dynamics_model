% ============================================================================
% CONSTRAINT STORY: maximum_entropy_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maximum_entropy_principle, []).

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
 *   constraint_id: maximum_entropy_principle
 *   human_readable: Maximum Entropy Principle in Statistical Mechanics
 *   domain: statistical_mechanics/information_theory
 *
 * SUMMARY:
 *   The maximum entropy principle in statistical mechanics asserts that
 *   isolated systems evolve toward states of maximum entropy consistent with
 *   their constraints. This is a genuine natural law: it emerges from the
 *   mathematical structure of probability distributions, the symmetries of
 *   microscopic dynamics, and the measure-preserving nature of phase-space
 *   evolution (Liouville's theorem). Unlike institutional or coordination
 *   constraints, the maximum entropy principle has no enforcer, no
 *   beneficiary structure, no exit mechanism. No coalition can exit it; no
 *   agent extracts value from it; its validity is independent of observation,
 *   measurement framework, or belief systems. It is ineliminable — any theory
 *   that violates it is demonstrably false. The principle holds at every
 *   timescale and across every domain: gases, liquids, solids, quantum
 *   systems, astrophysical objects, information channels. The extractiveness
 *   value (0.05) reflects the minimal conceptual and mathematical overhead of
 *   applying the principle — it is not zero because using the principle
 *   requires specifying the constraint surface (the set of accessible
 *   microstates), which is an epistemic choice. The theater ratio (0.08)
 *   reflects that entropy calculations may be presented with varying degrees
 *   of technical exposition, but the underlying structure is transparent:
 *   entropy is a well-defined function of microstate distribution. No
 *   performative activity is required to apply the principle; it yields
 *   testable predictions.
 *
 * KEY AGENTS:
 *   - Isolated Physical System: Passive subject (powerless/trapped/civilizational) — bears the constraint with no exit. The system evolves according to the principle regardless of external observers or measurement choices.
 *   - Analytical Physicist: Observer (analytical/analytical/civilizational) — can measure and verify the principle but cannot escape or modify it. The principle bounds all possible theories.
 *   - Research Institution: Institutional actor (institutional/constrained/generational) — can refine models and measurements but operates within the invariant constraint. Theoretical revision is possible only within bounds set by the principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maximum_entropy_principle, 0.05).
domain_priors:suppression_score(maximum_entropy_principle, 0.02).
domain_priors:theater_ratio(maximum_entropy_principle, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maximum_entropy_principle, extractiveness, 0.05).
narrative_ontology:constraint_metric(maximum_entropy_principle, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(maximum_entropy_principle, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maximum_entropy_principle, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(maximum_entropy_principle, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maximum_entropy_principle, mountain).
narrative_ontology:human_readable(maximum_entropy_principle, "Maximum Entropy Principle in Statistical Mechanics").
narrative_ontology:topic_domain(maximum_entropy_principle, "statistical_mechanics/information_theory").

domain_priors:emerges_naturally(maximum_entropy_principle).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL SYSTEM (MOUNTAIN) — A macroscopic system cannot exit the entropy constraint. Microscopic particles follow deterministic dynamics; the ensemble of possible microstates evolves according to the principle with mathematical necessity. No escape mechanism exists at any timescale.
constraint_indexing:constraint_classification(maximum_entropy_principle, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The principle is a mathematical consequence of the phase-space measure and Liouville's theorem. From any epistemic position, the constraint is invariant: entropy increases for an isolated system approaching equilibrium. The mathematical structure is independent of observation method or measurement framework.
constraint_indexing:constraint_classification(maximum_entropy_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (MOUNTAIN) — From the perspective of a research program modeling thermodynamic systems, the maximum entropy principle is ineliminable. Institutions can refine measurements or improve models, but cannot escape the constraint. The constraint bounds all possible theories; any theory that violates it is demonstrably false. Constrained exit reflects that theoretical revision is possible but only within the constraint's bounds.
constraint_indexing:constraint_classification(maximum_entropy_principle, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maximum_entropy_principle_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(maximum_entropy_principle, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maximum_entropy_principle, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(maximum_entropy_principle, ExtMetricName, E),
    domain_priors:suppression_score(maximum_entropy_principle, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(maximum_entropy_principle),
    narrative_ontology:constraint_metric(maximum_entropy_principle, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(maximum_entropy_principle, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(maximum_entropy_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.05): Minimal. The maximum entropy principle imposes no asymmetric extraction. There is no beneficiary group that gains advantage from the principle; no victim group that bears costs. The principle is universally applicable and universally ineliminable. The non-zero value (0.05, not 0.00) reflects the minimal overhead of specifying the constraint surface — defining which microstates are accessible requires epistemic work (boundary conditions, conservation laws, field specifications), but this work is a prerequisite to applying the principle, not an extractive feature of it. SUPPRESSION (0.02): Minimal. The principle imposes no coercive enforcement mechanism. Physical systems do not 'choose' to increase entropy; they follow deterministic dynamics. There are no alternatives to suppress — all possible dynamical trajectories consistent with the constraint manifest entropy increase. The non-zero value reflects that systems with low initial entropy must pass through intermediate states before reaching maximum entropy equilibrium, creating an apparent 'constraint' during the transient approach to equilibrium. But this is not suppression in the DR sense — it is the system's own evolution, not an external force preventing alternatives. THEATER RATIO (0.08): Minimal. The mathematical structure is transparent: entropy is defined as S = -k_B Σ p_i ln(p_i) (or equivalent formulations). The principle follows from Liouville's theorem and phase-space measure preservation. No performative activity obscures the structure. Variations in how the principle is presented (thermodynamic vs. information-theoretic framing, classical vs. quantum formulations) are interpretive layers, not theater. The 0.08 value reflects minimal epistemic translation overhead — the principle must be stated in mathematical language and connected to empirical systems, which requires some expository work, but the connection is direct and verifiable.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as mountain because the principle is genuinely invariant across observation contexts. The physical system experiences the principle as an ineliminable law governing its evolution. The analytical observer reconstructs the principle from mathematical structure and finds it invariant across all measurement frameworks and epistemic positions. The research institution models systems using the principle and finds it ineliminable — any theory that violates it is falsified. There is NO perspectival gap because the constraint is a true natural law, not a contingent institutional arrangement or coordination mechanism. This is the canonical case: all observers, regardless of power level or exit options, perceive the same constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The maximum entropy principle exhibits zero mandatrophy. There is no tension between coordination and extraction because there is no coordination function and no extraction. The principle is pure law — it constrains all possible states but imposes no asymmetric costs. The principle cannot be reframed as 'really a coordination mechanism' (no coordination function exists) nor as 'really an extractive mechanism' (no asymmetric beneficiary exists). The mathematical structure is univocal: all agents, all observers, all systems experience the same constraint. This is the diagnostic gold standard for mountain classification — no reframing, no perspectival escape, no hidden beneficiary structure can dissolve the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    microscopic_reversibility_gap,
    'How does the microscopically reversible dynamics of particles lead to irreversible entropy increase at the macroscopic level?',
    'Analysis of phase-space coarse-graining: the apparent irreversibility emerges from the informational limit on tracking individual microstates, not from fundamental asymmetry in microscopic laws. Loschmidt''s paradox and Poincaré recurrence are the central interpretive puzzles.',
    'If the apparent irreversibility is purely informational (coarse-grained), the maximum entropy principle is a constraint on observer knowledge rather than on physical evolution. If there is fundamental microscopic asymmetry, the principle reflects real physical directedness. This does not change the classification (mountain remains mountain) but clarifies the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(microscopic_reversibility_gap, conceptual, 'Microscopic reversibility vs. macroscopic entropy increase').

omega_variable(
    equilibrium_definition_underdetermined,
    'Is ''equilibrium'' an intrinsic property of a system or a property relative to the set of accessible microstates (the constraint surface)?',
    'Examination of whether equilibrium is uniquely defined by first-principles dynamics or depends on the specification of which microstates are accessible (initial conditions, boundary conditions, field constraints). The debate between ergodic and non-ergodic interpretations of equilibration.',
    'If equilibrium is intrinsic: maximum entropy principle is an unconditional law. If equilibrium depends on constraint specification: the principle is conditional on defining the constraint surface correctly. The mountain classification holds either way because the principle itself is invariant; the ambiguity concerns interpretation rather than falsifiability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_definition_underdetermined, conceptual, 'Whether equilibrium is intrinsic or relative to constraint surface').

omega_variable(
    information_theoretic_vs_thermodynamic,
    'Is the maximum entropy principle a fundamental law of thermodynamics (Boltzmann, Gibbs) or a principle of inference under uncertainty (Jaynes)?',
    'Comparison of empirical predictions and theoretical scope. Thermodynamic interpretation: entropy is a physical property of matter. Information-theoretic interpretation: entropy is the measure of uncertainty about the underlying state given available information. Both yield the same mathematical form and empirical predictions.',
    'If thermodynamic: the principle describes how matter actually behaves, independent of observers. If information-theoretic: the principle describes valid inference procedures for observers with limited information. This does not change the mountain classification (the principle is invariant) but affects how we understand what the constraint is constraining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_theoretic_vs_thermodynamic, conceptual, 'Thermodynamic vs. information-theoretic interpretations').

omega_variable(
    false_summit_natural_law,
    'Does the maximum entropy principle describe a true natural law, or does its universality mask domain-specific institutional adoption of a particular formalism?',
    'Historical analysis: how did the principle become canonical? Did it emerge from mathematical necessity or institutional/disciplinary consensus? Do alternative formalisms (Tsallis entropy, quantum information measures) violate or generalize the principle? Are there physical domains where maximum entropy fails empirically?',
    'If true natural law: the principle is invariant across all physical domains and measurement contexts. If institutionalized formalism: domains that have adopted alternative entropy measures (non-extensive systems, quantum foundations) might reveal the principle as contingent on specific modeling choices. Engine''s false summit detection applies here: genuine natural laws have no beneficiaries, but if the principle naturalizes a modeling convention that benefits specific research programs, FSM should trigger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Natural law vs. institutionalized modeling convention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maximum_entropy_principle, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maxent_tr_t0, maximum_entropy_principle, theater_ratio, 0, 0.08).
narrative_ontology:measurement(maxent_tr_t100, maximum_entropy_principle, theater_ratio, 100, 0.08).
narrative_ontology:measurement(maxent_tr_t1000, maximum_entropy_principle, theater_ratio, 1000, 0.08).

% Extraction over time
narrative_ontology:measurement(maxent_be_t0, maximum_entropy_principle, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(maxent_be_t100, maximum_entropy_principle, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(maxent_be_t1000, maximum_entropy_principle, base_extractiveness, 1000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maximum_entropy_principle, information_standard).
narrative_ontology:affects_constraint(maximum_entropy_principle, second_law_of_thermodynamics).
narrative_ontology:affects_constraint(maximum_entropy_principle, ergodic_hypothesis).
narrative_ontology:affects_constraint(maximum_entropy_principle, arrow_of_time).

% DUAL FORMULATION NOTE:
% The maximum entropy principle is foundational to three related constraints: (1) the second law of thermodynamics (empirical manifestation of entropy increase in closed systems), (2) the ergodic hypothesis (assumption that phase-space exploration allows entropy maximization), and (3) the arrow of time (the thermodynamic distinction between past and future). The maximum entropy principle is upstream — it provides the mathematical framework for understanding all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
