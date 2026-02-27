% ============================================================================
% CONSTRAINT STORY: thermodynamics_entropy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thermodynamics_entropy, []).

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
 *   constraint_id: thermodynamics_entropy
 *   human_readable: The Second Law of Thermodynamics (Entropy)
 *   domain: technological/physics
 *
 * SUMMARY:
 *   The Second Law of Thermodynamics is a foundational constraint on all
 *   physical systems in the universe. It states that the total entropy of an
 *   isolated system can never decrease — it can only increase or remain
 *   constant. This constraint emerges naturally from statistical mechanics:
 *   disorder is overwhelmingly more probable than order because macroscopic
 *   states correspond to vastly more microstates than organized
 *   configurations. The second law is universal, invariant across all
 *   observation positions, and applies equally to heat engines, biological
 *   systems, chemical reactions, and the cosmos itself. It is not a policy,
 *   coordination mechanism, or institutional arrangement — it is a
 *   consequence of the combinatorial structure of phase space and the nature
 *   of probability. The constraint exhibits zero degrees of freedom: no agent
 *   can negotiate with it, engineer around it, or find loopholes through
 *   novel technology or institutional design. It is the paradigmatic exemplar
 *   of a mountain constraint in the Deferential Realism system.
 *
 * KEY AGENTS:
 *   - Heat Engine Designers: Powerless/trapped — must accept Carnot efficiency bounds; cannot exceed them through innovation
 *   - Industrial Energy Systems Managers: Institutional/analytical — all thermal systems under their control must dissipate waste heat; constraint is universal and uniform
 *   - Chemical Processes: All chemical reactions, mixing, and phase transitions follow entropy production laws; no subsystem can violate the constraint
 *   - Analytical Observer: Civilizational/analytical — understands that entropy increase follows from combinatorics and probability, not from enforcement or policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thermodynamics_entropy, 0.08).
domain_priors:suppression_score(thermodynamics_entropy, 0.02).
domain_priors:theater_ratio(thermodynamics_entropy, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, 0.08).
narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(thermodynamics_entropy, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thermodynamics_entropy, mountain).
narrative_ontology:human_readable(thermodynamics_entropy, "The Second Law of Thermodynamics (Entropy)").
narrative_ontology:topic_domain(thermodynamics_entropy, "technological/physics").

domain_priors:emerges_naturally(thermodynamics_entropy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEAT ENGINE DESIGNER (MOUNTAIN) — No matter what engineering approach is attempted, Carnot efficiency bounds apply universally. Cannot exit the constraint through design cleverness or resource investment. d≈1.0, f(d)≈1.42, but f(d) applies only to extraction mechanics; entropy increase is invariant across all directionality. χ computation yields negative value due to natural law signature — classification is mountain regardless of χ.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INDUSTRIAL ENERGY SYSTEMS MANAGER (MOUNTAIN) — All thermal systems, regardless of scale or institutional resources, must dissipate waste heat. The second law cannot be negotiated, bribed, or engineered around. Applies equally to power plants, refrigerators, and biological metabolism. d≈0.0 (institutional beneficiary of thermodynamic knowledge), but again, natural law signature dominates: classification is mountain.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / STATISTICAL MECHANICS (MOUNTAIN) — From first principles, entropy increase follows from the overwhelming probability that macrostates correspond to far more microstates than ordered configurations. This is not a contingent law but a consequence of combinatorics and the nature of probability itself. The constraint emerges naturally from the mathematical structure of phase space. d≈0.5 (observer symmetry), but the natural law gate dominates: classification is mountain across all observation angles.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: CHEMIST / DISSIPATIVE PROCESSES (MOUNTAIN) — Chemical reactions, phase transitions, and mixing processes all follow entropy production laws. Cannot construct isolated systems where entropy decreases. The constraint is an invariant of thermochemical space, not a policy or coordination mechanism. d≈0.5, natural law signature: mountain.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamics_entropy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thermodynamics_entropy, ExtMetricName, E),
    domain_priors:suppression_score(thermodynamics_entropy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thermodynamics_entropy),
    narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thermodynamics_entropy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The second law does not extract value in the coordination sense — it does not benefit one agent over another. The minimal non-zero value reflects that entropy increase can be harnessed for work extraction (entropy differentials drive turbines, refrigerators, biochemistry), but this is utilization of the constraint, not extraction from one party by another. The constraint itself is symmetric and universal. Suppression (0.02): Minimal. There are no suppressed alternatives — entropy decrease in isolated systems is not an alternative pathway that is actively blocked. It is simply forbidden by the laws of physics. Theater ratio (0.05): Near-zero. The second law requires no performative activity, no ritual, no theatrical maintenance. It operates identically whether observed or unobserved. The minimal value reflects only the unavoidable overhead of stating and teaching the law itself. Accessibility collapse (0.92): Very high. The second law is irreducibly fundamental — it cannot be reduced to more basic principles without leaving out essential explanatory content. Statistical mechanics explains the mechanism but does not weaken the law. No known experiment has ever violated it. Resistance (0.08): Very low. No resistance to the law exists because no alternative is possible. Agents do not resist entropy increase — they work within it. The minimal value reflects only the pedagogical effort required to teach the concept.
 *
 * PERSPECTIVAL GAP:
 *   The second law produces NO perspectival gap. All four perspectives (heat engine designer, industrial manager, chemist, analytical observer) classify it identically as mountain. This is the defining property of a true natural law: invariance across all observation positions and structural relationships. The constraint is the same whether viewed by a powerless agent facing Carnot bounds or an institutional agent managing industrial systems or an analytical observer understanding statistical mechanics. This uniformity is NOT a limitation of the indexical system — it is a core prediction: true mountains should exhibit no perspectival gap. The absence of gap is evidence FOR the mountain classification, not against it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality becomes analytically irrelevant for mountain constraints. The natural law signature (ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally=true) takes precedence over directionality derivation. Even if we compute d values: heat engine designer trapped/powerless → d≈1.0, but this high d value does not shift the classification from mountain because the natural law gates dominate. Industrial manager institutional/analytical → d≈0.0, but this low d also preserves mountain classification. The Second Law is invariant to directionality — it applies equally to all agents regardless of their structural position relative to the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    microscopic_reversibility_asymmetry,
    'How does the time-reversibility of microscopic dynamics (Hamilton''s equations) reconcile with the irreversibility of macroscopic entropy increase?',
    'Statistical mechanics interpretation of the Boltzmann H-theorem; analysis of phase-space dynamics and coarse-graining; examination of initial-condition measure asymmetries',
    'If reconciliation is purely epistemic (asymmetry in observer knowledge, not physical law): mountain persists. If microscopic dynamics contain genuine irreversibility (new fundamental asymmetry): classification unchanged but mechanism requires refinement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(microscopic_reversibility_asymmetry, conceptual, 'Reconciliation of microscopic reversibility with macroscopic irreversibility').

omega_variable(
    quantum_information_preservation,
    'Does quantum mechanics allow information-theoretic violation of entropy increase through entanglement, quantum error correction, or black hole complementarity?',
    'Black hole thermodynamics and Hawking radiation analysis; quantum information theory proofs; experimental tests of information recovery in quantum systems',
    'If information is preserved (black hole information paradox resolved in information''s favor): entropy increase remains universal law but its interpretation shifts from information loss to information redistribution. Mountain classification unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_information_preservation, empirical, 'Whether quantum mechanics preserves information against entropy increase').

omega_variable(
    universe_boundary_conditions,
    'Does the second law apply to the universe as a whole, or only to subsystems within a larger context?',
    'Cosmological observations of universe expansion, entropy content estimates, time-asymmetry in the early universe; analysis of whether boundary conditions (low-entropy initial state) are part of physical law or contingent cosmological fact',
    'If universe-wide entropy increase is a boundary condition (not fundamental law): second law becomes contingent on cosmology rather than universal. Classification might shift to rope (coordination of initial conditions) or scaffold (boundary conditions as temporary feature). Probability: low. If entropy increase is fundamental even for the universe: mountain classification is universal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universe_boundary_conditions, conceptual, 'Applicability of second law to universe as a whole vs subsystems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thermodynamics_entropy, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thermo_tr_t0, thermodynamics_entropy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(thermo_tr_t200, thermodynamics_entropy, theater_ratio, 200, 0.05).
narrative_ontology:measurement(thermo_tr_t400, thermodynamics_entropy, theater_ratio, 400, 0.05).

% Extraction over time
narrative_ontology:measurement(thermo_be_t0, thermodynamics_entropy, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(thermo_be_t200, thermodynamics_entropy, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(thermo_be_t400, thermodynamics_entropy, base_extractiveness, 400, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thermodynamics_entropy, global_infrastructure).
narrative_ontology:affects_constraint(thermodynamics_entropy, carnot_efficiency_limits).
narrative_ontology:affects_constraint(thermodynamics_entropy, heat_dissipation_irreversibility).
narrative_ontology:affects_constraint(thermodynamics_entropy, gibbs_free_energy_spontaneity).

% DUAL FORMULATION NOTE:
% The Second Law can be formulated in multiple equivalent mathematical forms: Clausius statement (heat flows from hot to cold), Kelvin-Planck statement (no heat engine can convert heat entirely to work), entropy production (ΔS ≥ 0 for isolated systems), and statistical mechanics (disorder overwhelmingly more probable than order). These are not separate constraints but equivalent expressions of a single universal principle. Each formulation applies to different domains (thermodynamics, engineering, chemistry, statistical mechanics) but all exhibit identical mountain classification. Network links connect to downstream constraints (Carnot limits, heat dissipation, chemical spontaneity) that are derived consequences of the Second Law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
