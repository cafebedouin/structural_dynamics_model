% ============================================================================
% CONSTRAINT STORY: angular_momentum_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_angular_momentum_conservation, []).

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
 *   constraint_id: angular_momentum_conservation
 *   human_readable: Angular Momentum Conservation
 *   domain: physics/mechanics
 *
 * SUMMARY:
 *   Angular momentum conservation is a fundamental principle in classical and
 *   quantum mechanics stating that the total angular momentum of an isolated
 *   system remains constant unless acted upon by an external torque. This
 *   constraint emerges from rotational symmetry of physical systems via
 *   Noether's theorem: for every continuous symmetry of the Lagrangian, there
 *   is a corresponding conserved quantity. The rotational symmetry of
 *   spacetime (isotropy) yields angular momentum conservation. This is a
 *   canonical example of a mountain constraint — it exhibits zero degrees of
 *   freedom for modification, emerges necessarily from deeper symmetries, has
 *   near-universal applicability, and has never been observed to be violated
 *   across three centuries of increasingly sensitive experiments.
 *
 * KEY AGENTS:
 *   - Isolated Physical Systems: Completely trapped (universal/civilizational) — cannot violate the law; bound by fundamental spacetime symmetry
 *   - Experimental Physicists: Moderate power (analytical/civilizational) — can design tests but cannot find violations; constrained by the law's universality
 *   - Theoretical Framework: Institutional (analytical/universal) — Noether's theorem and Lagrangian formalism encode the conservation law; no alternative framework exists that preserves consistency with all other confirmed laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(angular_momentum_conservation, 0.08).
domain_priors:suppression_score(angular_momentum_conservation, 0.02).
domain_priors:theater_ratio(angular_momentum_conservation, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(angular_momentum_conservation, extractiveness, 0.08).
narrative_ontology:constraint_metric(angular_momentum_conservation, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(angular_momentum_conservation, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(angular_momentum_conservation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(angular_momentum_conservation, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(angular_momentum_conservation, mountain).
narrative_ontology:human_readable(angular_momentum_conservation, "Angular Momentum Conservation").
narrative_ontology:topic_domain(angular_momentum_conservation, "physics/mechanics").

domain_priors:emerges_naturally(angular_momentum_conservation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONSTRAINED PHYSICAL SYSTEM (MOUNTAIN) — Any rotating body in isolation must conserve angular momentum. This is not a negotiable constraint or a coordination mechanism. The system cannot exit this law; it is trapped by the fundamental symmetry of spacetime under rotation. No alternatives exist.
constraint_indexing:constraint_classification(angular_momentum_conservation, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE EXPERIMENTAL OBSERVER (MOUNTAIN) — Attempts to measure or violate angular momentum conservation have failed consistently across 300+ years of increasingly sensitive experiments. The observer can choose different measurement apparatus, coordinate systems, or initial conditions, but the law holds across all contexts. No violation pathway exists.
constraint_indexing:constraint_classification(angular_momentum_conservation, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL FRAMEWORK (MOUNTAIN) — Angular momentum conservation derives necessarily from Noether's theorem applied to rotational symmetry of the Lagrangian. The law is not contingent on measurement choice, coordinate selection, or experimental context. It follows from the structure of spacetime itself. The analyst can verify this through pure mathematical derivation independent of any empirical test.
constraint_indexing:constraint_classification(angular_momentum_conservation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(angular_momentum_conservation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(angular_momentum_conservation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(angular_momentum_conservation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(angular_momentum_conservation, ExtMetricName, E),
    domain_priors:suppression_score(angular_momentum_conservation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(angular_momentum_conservation),
    narrative_ontology:constraint_metric(angular_momentum_conservation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(angular_momentum_conservation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(angular_momentum_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint does not extract value from any agent; it is a law of nature that applies uniformly. The low value reflects that the constraint is non-adversarial — there is no beneficiary and no victim, only fundamental symmetry. Suppression (0.02): Negligible. There are no alternatives to angular momentum conservation, no escape pathways, no workarounds. But suppression is measured as the absence of alternatives, which is nearly complete. The non-zero value reflects measurement uncertainty and the theoretical possibility of undiscovered exceptions in extreme regimes. Theater ratio (0.05): Nearly zero. The constraint requires no performative maintenance. Experimental verification of angular momentum conservation is straightforward; no theater or ritual is needed to maintain it. The minimal non-zero value reflects only incidental presentation overhead in scientific communication, not any functional theater.
 *
 * PERSPECTIVAL GAP:
 *   Not applicable. This constraint shows perfect invariance across all indexical positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) does not apply to this constraint. Angular momentum conservation has no beneficiaries or victims — it is a symmetry law, not an extractive or coordinative mechanism. All agents are equally constrained by it. The engine will return a canonical d value reflecting pure constraint with zero asymmetry (d ≈ 0.50 at the analytical level, yielding f(d) ≈ 0.65, but this is a mathematical artifact rather than a meaningful power differential — the constraint has no directionality because it has no directionality target).
 *
 * MANDATROPHY ANALYSIS:
 *   PURE MOUNTAIN — NO MANDATROPHY. Angular momentum conservation exhibits no mandatrophy because it has no coordinative or extractive structure to be confused. The constraint is pure law: it does not solve a coordination problem, it does not extract value, it does not have beneficiaries or victims. All perspectives agree uniformly. The only theoretical risk is that the constraint is not truly universal (omega variables about closed-system boundaries, quantum exceptions, or extreme regime breakdowns), in which case the classification might degrade to conditional mountain (rope for certain domains) or piton (emergent property). But within classical mechanics and quantum mechanics as currently understood, the constraint is unconditionally mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    closed_system_boundary,
    'Does angular momentum truly conserve in the presence of external torques, or is the apparent violation merely a boundary-definition problem?',
    'Rigorous accounting of all torques (including gravity gradient, electromagnetic radiation reaction) in experimental systems; demonstration that apparent violations resolve to boundary-definition issues.',
    'If all violations resolve to boundary issues: the law is universal and unconditional (pure mountain). If some violations remain unexplained: the constraint may degrade to rope (coordination mechanism for closed systems) or piton (broken law maintained by institutional assumption).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(closed_system_boundary, empirical, 'Whether apparent violations are boundary-definition artifacts').

omega_variable(
    quantum_measurement_anomaly,
    'In quantum systems, does measurement collapse or entanglement create exceptions to classical angular momentum conservation that scale to macroscopic systems?',
    'Test angular momentum conservation in mesoscopic systems (molecular rotors, quantum dots); identify whether quantum mechanical effects violate or modify the conservation law.',
    'If quantum mechanics preserves conservation: law holds across all physical scales (universal mountain). If quantum effects violate conservation at any scale: the constraint becomes conditional (mountain only for classical systems) and may decompose into separate constraints for quantum vs classical domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_measurement_anomaly, empirical, 'Whether quantum measurement effects violate angular momentum conservation').

omega_variable(
    noether_symmetry_dependence,
    'Is angular momentum conservation truly fundamental (derived from spacetime rotational symmetry via Noether''s theorem) or is it a low-energy approximation valid only in certain regimes?',
    'Analysis of angular momentum conservation in regimes where spacetime may be non-isotropic (near black hole horizons, early universe, high-energy quantum gravity); test whether Noether''s symmetry argument holds universally.',
    'If Noether symmetry is universal: angular momentum conservation is a necessary law following from spacetime structure (pure mountain, no exceptions). If Noether symmetry breaks in extreme regimes: the constraint degrades to rope (coordination assumption) or piton (emergent property of low-energy physics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(noether_symmetry_dependence, empirical, 'Whether Noether symmetry is universal or regime-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(angular_momentum_conservation, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(angu_tr_t0, angular_momentum_conservation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(angu_tr_t100, angular_momentum_conservation, theater_ratio, 100, 0.05).
narrative_ontology:measurement(angu_tr_t300, angular_momentum_conservation, theater_ratio, 300, 0.05).

% Extraction over time
narrative_ontology:measurement(angu_be_t0, angular_momentum_conservation, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(angu_be_t100, angular_momentum_conservation, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(angu_be_t300, angular_momentum_conservation, base_extractiveness, 300, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(angular_momentum_conservation, information_standard).
narrative_ontology:affects_constraint(angular_momentum_conservation, energy_conservation).
narrative_ontology:affects_constraint(angular_momentum_conservation, linear_momentum_conservation).
narrative_ontology:affects_constraint(angular_momentum_conservation, noether_symmetry_principle).

% DUAL FORMULATION NOTE:
% Angular momentum conservation is a special case of Noether's theorem applied to rotational symmetry. It is structurally linked to energy conservation (time translation) and linear momentum conservation (spatial translation) as coordinate-system-dependent manifestations of the same underlying principle: invariance of the Lagrangian under continuous symmetries yields conserved quantities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
