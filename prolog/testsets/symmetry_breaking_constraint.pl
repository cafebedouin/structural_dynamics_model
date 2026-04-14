% ============================================================================
% CONSTRAINT STORY: symmetry_breaking_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symmetry_breaking_constraint, []).

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
 *   constraint_id: symmetry_breaking_constraint
 *   human_readable: Symmetry Breaking in Physics and Mathematics
 *   domain: foundational_physics/mathematics
 *
 * SUMMARY:
 *   Symmetry breaking is a foundational constraint that appears across
 *   physics, mathematics, and dynamical systems theory. It describes the
 *   irreversible transition from a symmetric state to an asymmetric one—the
 *   moment a system chooses a particular solution from a manifold of
 *   equivalent symmetric possibilities. This constraint is universal: in
 *   spontaneous magnetization, electroweak symmetry breaking in particle
 *   physics, phase transitions in condensed matter, pattern formation in
 *   nonlinear dynamics, and bifurcations in mathematics. No agent—whether
 *   physicist, engineer, mathematician, or observer—can exit or modify this
 *   constraint. It is not a policy choice, institutional arrangement, or
 *   contingent feature of how we organize knowledge. It is an immutable law
 *   of how systems behave.
 *
 * KEY AGENTS:
 *   - Analytical Observer: (analytical/civilizational) — sees symmetry breaking as universal, unchangeable principle
 *   - Physicists: (powerful/civilizational) — cannot evade symmetry breaking despite powerful research programs
 *   - Engineers: (institutional/biographical) — must work within symmetry-breaking constraints in design (superconductors, lasers, magnets)
 *   - Mathematicians: (moderate/generational) — encounter symmetry breaking as intrinsic feature of nonlinear dynamics and bifurcation theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symmetry_breaking_constraint, 0.18).
domain_priors:suppression_score(symmetry_breaking_constraint, 0.03).
domain_priors:theater_ratio(symmetry_breaking_constraint, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symmetry_breaking_constraint, extractiveness, 0.18).
narrative_ontology:constraint_metric(symmetry_breaking_constraint, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(symmetry_breaking_constraint, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(symmetry_breaking_constraint, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(symmetry_breaking_constraint, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symmetry_breaking_constraint, mountain).
narrative_ontology:human_readable(symmetry_breaking_constraint, "Symmetry Breaking in Physics and Mathematics").
narrative_ontology:topic_domain(symmetry_breaking_constraint, "foundational_physics/mathematics").

domain_priors:emerges_naturally(symmetry_breaking_constraint).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUNDAMENTAL PRINCIPLE (MOUNTAIN) — Symmetry breaking is an irreducible feature of how systems transition from symmetric to asymmetric states. At civilizational/universal scope, this is a constraint on what is logically possible, not contingent on observation method. All agents experience this as unchangeable law.
constraint_indexing:constraint_classification(symmetry_breaking_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHYSICIST'S PERSPECTIVE (MOUNTAIN) — Even powerful research programs cannot evade symmetry breaking. It is a universal constraint on dynamical systems. No exit option changes this classification.
constraint_indexing:constraint_classification(symmetry_breaking_constraint, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: APPLIED ENGINEER (MOUNTAIN) — Symmetry breaking is an immutable feature of phase transitions and critical phenomena. Whether designing superconductors, lasers, or ferromagnets, engineers cannot escape symmetry-breaking constraints. They work within them, not against them.
constraint_indexing:constraint_classification(symmetry_breaking_constraint, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: APPLIED MATHEMATICIAN (MOUNTAIN) — Symmetry breaking appears in bifurcation theory, partial differential equations, and pattern formation. The constraint is structural to nonlinear dynamics itself. No mathematical technique circumvents it.
constraint_indexing:constraint_classification(symmetry_breaking_constraint, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symmetry_breaking_constraint_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(symmetry_breaking_constraint, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symmetry_breaking_constraint, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(symmetry_breaking_constraint, ExtMetricName, E),
    domain_priors:suppression_score(symmetry_breaking_constraint, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(symmetry_breaking_constraint),
    narrative_ontology:constraint_metric(symmetry_breaking_constraint, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(symmetry_breaking_constraint, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(symmetry_breaking_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. Symmetry breaking extracts nothing from agents—it is a constraint on logical possibility, not a mechanism that transfers resources or value from one agent to another. The value represents the bare irreducibility of the constraint—agents cannot negotiate or reduce its force. Suppression (0.03): Minimal. The constraint does not suppress alternatives through coercion or hidden mechanisms. It operates through logical necessity: once a symmetric system evolves, it must choose a state. The suppression is purely structural—not the result of intentional enforcement. Theater ratio (0.12): Very low. There is no performative element. Symmetry breaking does not maintain itself through ritual, narrative, or institutional theater. It simply happens. The accessibility collapse (0.92) reflects that there is virtually no accessible path around this constraint—all approaches to dynamical systems, whether mathematical, computational, or experimental, encounter it. The resistance (0.08) reflects that no one resists symmetry breaking as such—it is accepted as fundamental wherever it appears.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All perspectives classify symmetry breaking identically as mountain. This is characteristic of true natural laws: they do not change in classification based on the observer's position, power level, time horizon, or exit options. Whether viewed immediately or across civilizations, by powerless agents or powerful institutions, locally or universally, symmetry breaking remains unchanged. This uniformity is the defining signature of a genuine mountain constraint. The absence of perspectival disagreement is not a limitation but a confirmation of the constraint's fundamental character.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality computation applies. Symmetry breaking has no beneficiaries or victims because it is not an extraction mechanism. It is not a constraint imposed by one agent on another but a constraint imposed by the structure of dynamical systems themselves. There is no flow of resources or value—only the irreversible transition from symmetry to asymmetry. The d-value derivation chain is not engaged because the constraint operates outside the framework of asymmetric power relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY: Symmetry breaking resolves the mandatrophy by being a constraint that is purely mountain with zero perspectival variation. There is no hidden coordination function masquerading as extraction, no extractive benefit misconstrued as coordination. The constraint simply IS an irreducible feature of how systems transition between states. There is no mandatrophy because there is nothing to resolve—the classification is unambiguous from all positions. This is the gold standard for mountain constraints: universality, immutability, and perspectival invariance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spontaneous_vs_explicit_distinction,
    'Is the distinction between spontaneous symmetry breaking and explicit symmetry breaking a fundamental division or a measurement-dependent artifact?',
    'Renormalization group analysis; comparison of explicit perturbations at different scales; investigation of whether ''spontaneous'' breaking always emerges as limit of explicit breaking with vanishing parameter',
    'If artifact: only explicit breaking exists; spontaneity is apparent, not fundamental. If fundamental: two distinct mechanisms require separate constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spontaneous_vs_explicit_distinction, conceptual, 'Whether spontaneous and explicit symmetry breaking are fundamentally distinct').

omega_variable(
    goldstone_boson_necessity,
    'Is the emergence of Goldstone bosons an inevitable consequence of spontaneous symmetry breaking or a contingent feature of continuous symmetries in infinite volume?',
    'Analysis of finite-size systems; finite-volume lattice calculations; investigation of whether Goldstone bosons disappear in finite systems or merely become massive',
    'If inevitable: adds constraint structure (massless modes). If contingent: the mountain classification may conflate two distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goldstone_boson_necessity, empirical, 'Whether Goldstone bosons are necessary consequence of symmetry breaking').

omega_variable(
    anthropic_selection_coupling,
    'Does the appearance of symmetry breaking depend on anthropic selection effects? Could universes with unbroken symmetries be imperceptible, creating selection bias in observers?',
    'Cosmological analysis of symmetry-breaking phase transitions in early universe; investigation of whether complex structures require preceding asymmetry; study of whether consciousness/observers require broken symmetries',
    'If selection effect: the mountain classification is observer-dependent, making it a rope or scaffold rather than mountain. If fundamental: mountain classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anthropic_selection_coupling, conceptual, 'Whether anthropic selection biases perception of symmetry breaking as inevitable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symmetry_breaking_constraint, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symbreak_tr_t0, symmetry_breaking_constraint, theater_ratio, 0, 0.12).
narrative_ontology:measurement(symbreak_tr_t50, symmetry_breaking_constraint, theater_ratio, 50, 0.12).
narrative_ontology:measurement(symbreak_tr_t100, symmetry_breaking_constraint, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(symbreak_be_t0, symmetry_breaking_constraint, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(symbreak_be_t50, symmetry_breaking_constraint, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(symbreak_be_t100, symmetry_breaking_constraint, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symmetry_breaking_constraint, information_standard).
narrative_ontology:affects_constraint(symmetry_breaking_constraint, spontaneous_symmetry_breaking_physics).
narrative_ontology:affects_constraint(symmetry_breaking_constraint, electroweak_symmetry_breaking).
narrative_ontology:affects_constraint(symmetry_breaking_constraint, pattern_formation_dynamics).
narrative_ontology:affects_constraint(symmetry_breaking_constraint, phase_transition_theory).

% DUAL FORMULATION NOTE:
% Symmetry breaking is a meta-constraint that affects multiple downstream constraints in physics and mathematics. Specific instantiations (electroweak breaking, ferromagnetism, bifurcations) are separate constraint stories that inherit the mountain classification from this foundational principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
