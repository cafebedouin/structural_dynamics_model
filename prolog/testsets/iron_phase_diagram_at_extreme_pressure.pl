% ============================================================================
% CONSTRAINT STORY: iron_phase_diagram_at_extreme_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iron_phase_diagram_at_extreme_pressure, []).

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
 *   constraint_id: iron_phase_diagram_at_extreme_pressure
 *   human_readable: Iron Phase Diagram at Extreme Pressure
 *   domain: condensed_matter_physics/materials_science
 *
 * SUMMARY:
 *   The iron phase diagram at extreme pressure defines the equilibrium
 *   crystal structures of iron across the pressure-temperature space relevant
 *   to planetary cores, impact dynamics, and materials physics. Phase
 *   transitions occur at fixed pressures and temperatures: body-centered
 *   cubic (BCC) iron at ambient conditions; face-centered cubic (FCC) above
 *   ~13 GPa at room temperature; hexagonal close-packed (HCP) at higher
 *   pressures relevant to Earth's core. These boundaries are not negotiable —
 *   they are determined by the thermodynamic stability of crystal structures
 *   under compression. The constraint is a natural law: iron atoms organize
 *   into specific lattice geometries when subjected to extreme pressure, and
 *   the boundaries between these geometries are immutable. This exemplifies a
 *   true mountain constraint in the DR system: no agent can alter the phase
 *   boundaries; no beneficiary extracts value through manipulation; no victim
 *   bears extractive cost. The constraint simply is.
 *
 * KEY AGENTS:
 *   - Experimental physicists (powerless/trapped): Cannot change phase boundaries; must map them through expensive, technically difficult diamond anvil cell experiments
 *   - Materials scientists (institutional/analytical): Use the phase diagram as a fixed reference frame for understanding material properties under compression
 *   - Planetary scientists (institutional/analytical): Depend on the phase diagram to model Earth's core composition and dynamics
 *   - Theoretical models (analytical/analytical): Must be consistent with observed phase boundaries; constrained by natural law rather than constraining it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iron_phase_diagram_at_extreme_pressure, 0.18).
domain_priors:suppression_score(iron_phase_diagram_at_extreme_pressure, 0.03).
domain_priors:theater_ratio(iron_phase_diagram_at_extreme_pressure, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, extractiveness, 0.18).
narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iron_phase_diagram_at_extreme_pressure, mountain).
narrative_ontology:human_readable(iron_phase_diagram_at_extreme_pressure, "Iron Phase Diagram at Extreme Pressure").
narrative_ontology:topic_domain(iron_phase_diagram_at_extreme_pressure, "condensed_matter_physics/materials_science").

domain_priors:emerges_naturally(iron_phase_diagram_at_extreme_pressure).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTER (MOUNTAIN) — Cannot alter the phase transition boundaries of iron under extreme pressure. The constraint is immutable from the laboratory perspective — the phase transitions occur at fixed pressures and temperatures regardless of experimental preference. The experimenter is constrained by natural law, not by institutional arrangement. Zero degrees of freedom.
constraint_indexing:constraint_classification(iron_phase_diagram_at_extreme_pressure, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INSTITUTIONAL OBSERVER (MOUNTAIN) — From the standpoint of establishing measurement standards and calibration protocols, the iron phase diagram at extreme pressure is a physical constant. Institutions adopt it as a standard pressure-temperature reference precisely because it cannot be negotiated or altered. The phase transitions serve as fixed calibration points. This is the institutional use of a natural law — unchanged by institutional perspective.
constraint_indexing:constraint_classification(iron_phase_diagram_at_extreme_pressure, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — The iron phase diagram at extreme pressure derives from fundamental thermodynamic principles: crystal lattice stability under compression, electronic structure evolution, and entropy minimization. The phase boundaries are not contingent institutional arrangements but consequences of matter's intrinsic properties. The constraint is a natural law in the strictest sense — a consequence of how iron atoms interact under extreme compression. Emerges necessarily from physics.
constraint_indexing:constraint_classification(iron_phase_diagram_at_extreme_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iron_phase_diagram_at_extreme_pressure_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(iron_phase_diagram_at_extreme_pressure, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iron_phase_diagram_at_extreme_pressure, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, ExtMetricName, E),
    domain_priors:suppression_score(iron_phase_diagram_at_extreme_pressure, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(iron_phase_diagram_at_extreme_pressure),
    narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(iron_phase_diagram_at_extreme_pressure, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(iron_phase_diagram_at_extreme_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The phase diagram exhibits minimal extraction — no agent captures systematic surplus from the constraint's existence. The value reflects only the thermodynamic cost of phase transitions themselves (entropy change, latent heat), which is structural cost, not extractive overhead. Suppression (0.03): Minimal. There are no barriers to understanding or observing the constraint — it manifests identically regardless of observer preference. The small nonzero value reflects only measurement difficulty (extremely high pressures require specialized equipment), not suppression of alternatives or coercion. Theater ratio (0.12): Very low. The phase diagram is almost entirely functional — the experimental and theoretical activities required to map it serve the direct purpose of understanding iron's properties. There is minimal performative component. Accessibility collapse (0.92): High. The phase boundaries are extremely difficult to access experimentally — diamond anvil cells operate at the edge of current technology. Yet the boundaries themselves are perfectly definite once accessed, allowing no room for alternative interpretations. Resistance (0.08): Low. Once the phase transitions are observed, there is minimal resistance to accepting them — they are reproducible, theoretically explained, and universally applicable. The only resistance is the inherent limitation of measurement precision.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap — all agents classify this constraint identically as mountain. This is by design. The iron phase diagram is a uniform-type constraint: a natural law that appears the same from every structural position. The experimenter sees an immutable physical boundary. The institutional actor sees a fixed calibration standard. The analytical observer sees a consequence of fundamental thermodynamic principles. All three perspectives produce the same type with the same underlying mechanism: the constraint emerges from the intrinsic properties of iron atoms, not from any institutional or agent-dependent arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the structural sense because it is a pure natural law. No agent benefits from the phase boundaries being where they are; no agent is harmed by their location. The extractiveness value derives solely from the thermodynamic cost of the phase transitions themselves, which is a structural cost, not an extraction mechanism. All perspectives derive the same d value (approximately 0.50, reflecting symmetry between no beneficiary and no victim), and all produce the same classification (mountain). The constraint is invariant across all observer positions because it is invariant across all physical states.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy trivially: all perspectives agree on classification because the constraint is a natural law. There is no risk of mislabeling extraction as coordination (or vice versa) because there is no extraction and no coordination — the constraint simply manifests. The mountain classification is not a theoretical possibility that needs confirmation; it is the empirical reality. The omega variables address measurement precision and extrapolation validity, but these are epistemic questions about our knowledge of the constraint, not structural questions about the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_precision_limits,
    'Do current experimental techniques access the true phase boundary or only a band of measurement uncertainty around it?',
    'Meta-analysis of diamond anvil cell studies; comparison of phase boundary determinations across independent research groups; convergence analysis of successive refinements',
    'If true boundary is sharply defined: mountain classification confirmed. If boundary is intrinsically fuzzy due to kinetic barriers: may reclassify as rope (coordination around uncertainty rather than immutable boundary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_precision_limits, empirical, 'Whether phase boundaries are sharply defined or measured with intrinsic uncertainty').

omega_variable(
    extrapolation_validity_regimes,
    'Do phase boundary predictions beyond the highest-pressure experimental data points remain empirically valid or become speculative?',
    'Shock compression experiments; powder diffraction at megabar pressures; comparison of theoretical predictions to new experimental data',
    'If valid: constraint extends to universal scope (true mountain). If breaks down: constraint is mountain only in measured regime, becomes rope or snare in speculative regime (extraction of predictive authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extrapolation_validity_regimes, empirical, 'Validity of phase boundary extrapolation beyond measured pressures').

omega_variable(
    iron_alloy_versus_pure_iron,
    'Does the pure-iron phase diagram differ fundamentally from the iron-rich phase diagrams relevant to geophysics and planetary core modeling?',
    'Cross-domain literature analysis; examination of how geophysicists use or modify the pure-iron diagram; tests of whether core composition corrections change the phase boundary structure',
    'If fundamentally different: may decompose into separate constraints for pure-iron physics vs applied metallurgy. If similar: single constraint holds across contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iron_alloy_versus_pure_iron, conceptual, 'Whether pure iron phase diagram is separate from applied iron-alloy diagrams').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iron_phase_diagram_at_extreme_pressure, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iron_phase_tr_t0, iron_phase_diagram_at_extreme_pressure, theater_ratio, 0, 0.08).
narrative_ontology:measurement(iron_phase_tr_t25, iron_phase_diagram_at_extreme_pressure, theater_ratio, 25, 0.11).
narrative_ontology:measurement(iron_phase_tr_t50, iron_phase_diagram_at_extreme_pressure, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(iron_phase_be_t0, iron_phase_diagram_at_extreme_pressure, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(iron_phase_be_t25, iron_phase_diagram_at_extreme_pressure, base_extractiveness, 25, 0.17).
narrative_ontology:measurement(iron_phase_be_t50, iron_phase_diagram_at_extreme_pressure, base_extractiveness, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iron_phase_diagram_at_extreme_pressure, information_standard).
narrative_ontology:affects_constraint(iron_phase_diagram_at_extreme_pressure, earth_core_phase_state).
narrative_ontology:affects_constraint(iron_phase_diagram_at_extreme_pressure, planetary_differentiation_dynamics).
narrative_ontology:affects_constraint(iron_phase_diagram_at_extreme_pressure, shock_compression_equation_of_state).

% DUAL FORMULATION NOTE:
% The iron phase diagram at extreme pressure is a foundational constraint for several downstream domains: planetary core thermodynamics depends on knowing which iron phase is stable at core pressures; shock compression studies use the phase diagram as a reference frame; impact dynamics models require phase boundary information. This constraint is upstream in the dependency hierarchy — other constraints derive from or reference it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
