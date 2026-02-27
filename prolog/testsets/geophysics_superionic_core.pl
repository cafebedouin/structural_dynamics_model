% ============================================================================
% CONSTRAINT STORY: geophysics_superionic_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geophysics_superionic_core, []).

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
 *   constraint_id: geophysics_superionic_core
 *   human_readable: Super-ionic state of matter in Earth's inner core
 *   domain: geophysics/mineral_physics/planetary_science
 *
 * SUMMARY:
 *   The super-ionic state of matter discovered in Earth's inner core through
 *   seismic analysis represents a fundamental phase transition of iron and
 *   oxygen under extreme pressure and temperature conditions. This constraint
 *   exemplifies a pure mountain-class phenomenon: the inner core's
 *   super-ionic character emerges directly from thermodynamic and quantum
 *   mechanical principles operating at pressures exceeding 330 GPa and
 *   temperatures above 5200 K. No human agent, institution, or policy can
 *   alter this phase state. The constraint is not enforced through
 *   suppression or coercion — it simply is a consequence of matter under
 *   extreme conditions. The seismic observations that revealed the
 *   super-ionic state (velocity anisotropy, electrical conductivity,
 *   attenuation patterns) are natural laws expressed through geophysical
 *   measurement. Oxygen atoms become sufficiently mobile to migrate through
 *   the iron lattice while maintaining the overall crystal structure,
 *   creating the observed seismic properties. This is not a coordination
 *   mechanism, not an extractive arrangement, and not a theatrical ritual —
 *   it is a natural physical law discovered through scientific observation.
 *
 * KEY AGENTS:
 *   - Seismic Wave Propagation: Physical constraint (no agency) — carries information about core structure through the planet via body waves that cannot penetrate the super-ionic phase differently
 *   - Iron-Oxygen Thermodynamics: Physical law (no agency) — determines the phase diagram; no negotiation or alternative outcome possible
 *   - Geophysicists: Analytical observers (powerful/arbitrage) — interpret seismic data to understand the super-ionic state; benefit from accurate models but do not alter the phenomenon
 *   - Planetary Interior: Physical system (no agency) — exhibits the super-ionic state necessarily as a consequence of cooling from initial differentiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geophysics_superionic_core, 0.12).
domain_priors:suppression_score(geophysics_superionic_core, 0.02).
domain_priors:theater_ratio(geophysics_superionic_core, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, extractiveness, 0.12).
narrative_ontology:constraint_metric(geophysics_superionic_core, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(geophysics_superionic_core, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geophysics_superionic_core, mountain).
narrative_ontology:human_readable(geophysics_superionic_core, "Super-ionic state of matter in Earth's inner core").
narrative_ontology:topic_domain(geophysics_superionic_core, "geophysics/mineral_physics/planetary_science").

domain_priors:emerges_naturally(geophysics_superionic_core).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / SEISMIC CONSTRAINT (MOUNTAIN) — The superior conductivity and seismic wave propagation characteristics of the inner core are properties of the super-ionic phase that emerge directly from the thermodynamic conditions at Earth's center. From the perspective of seismic inversion and mineral physics theory, the super-ionic state is an immutable consequence of iron crystal structure under extreme pressure (>330 GPa) and temperature (>5200 K). Oxygen atoms become mobile while iron remains lattice-fixed, creating the observed seismic anisotropy and electrical conductivity. No agent can alter this phase transition — it is determined by fundamental physics.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PLANETARY FORMATION / CORE DYNAMICS (MOUNTAIN) — The super-ionic phase is an emergent property of planetary differentiation and iron-oxygen phase diagrams at core conditions. Planetary bodies with sufficient mass and thermal history will necessarily experience this phase transition in their inner cores. The constraint is not enforced by any agent or institution — it emerges from the closure of matter under extreme conditions. Even with full technological capacity, humanity cannot prevent or modify this state through policy, law, or institutional intervention.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SEISMIC DATA MEASUREMENT CONSTRAINT (MOUNTAIN) — The shear-wave anisotropy observed in the inner core (faster wave propagation along the north-south axis than along the equatorial plane) is a direct physical consequence of iron crystal alignment under stress and the super-ionic oxygen mobility. The observational constraint is fixed: seismic waves cannot propagate through the super-ionic iron-oxygen mixture in any other way. Measurement uncertainty exists, but the underlying physical law does not yield to measurement methodology. The constraint is invariant across all seismic observation techniques.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: MINERAL PHYSICS / PHASE DIAGRAM CONSTRAINT (MOUNTAIN) — The iron-oxygen phase diagram at pressures >330 GPa and temperatures >5200 K is determined by quantum mechanics and thermodynamics, not by convention or institutional choice. The super-ionic phase emerges as a stable state because oxygen atoms gain sufficient thermal energy to overcome the potential energy barriers constraining them to lattice positions. This phase transition is a natural law of condensed matter physics. No alternative phase diagram is possible under these conditions.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geophysics_superionic_core_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(geophysics_superionic_core, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geophysics_superionic_core, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(geophysics_superionic_core, ExtMetricName, E),
    domain_priors:suppression_score(geophysics_superionic_core, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(geophysics_superionic_core),
    narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(geophysics_superionic_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The super-ionic phase does not extract value from any agent — it is a physical property that geophysicists measure and theorists model. The measurement uncertainty (extractiveness value) reflects epistemic limits (seismic resolution, thermodynamic model precision) rather than intentional suppression. Theater ratio (0.15): Very low. Scientific description of the super-ionic state requires minimal performative element. Seismic data, high-pressure experiments, and phase diagram calculations are falsifiable and reproducible. The low theater ratio confirms the mountain classification — the phenomenon is functionally necessary and informationally transparent. Suppression (0.02): Negligible. There are no significant barriers to understanding or studying the super-ionic state beyond the technical challenges of seismic inversion and mineral physics computation. Accessibility collapse (0.88): Very high. The super-ionic state is equally inaccessible to all agents — it exists 6,400 km beneath Earth's surface at conditions impossible to replicate. No observer has special access or can negotiate the phase transition. Resistance (0.08): Very low. The phase transition occurs regardless of whether we measure it, study it, or ignore it. The resistance metric reflects only the challenge of scientific verification, not any active defense of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inner_core_composition_certainty,
    'Is the inner core composition truly pure iron-oxygen, or does it contain significant nickel, sulfur, or lighter elements that would alter the super-ionic phase structure?',
    'Seismic tomography refinement; cosmochemical analysis of core formation; comparison with iron meteorite compositions and high-pressure experimental data on multi-component iron alloys',
    'If significant nickel/sulfur: super-ionic phase structure is modified but still emergent from thermodynamics. If pure iron-oxygen: current model is accurate. Either way, the constraint remains mountain-class — the phase is determined by physical law, not by agent choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inner_core_composition_certainty, empirical, 'Compositional precision of the inner core iron-oxygen system').

omega_variable(
    oxygen_mobility_mechanism,
    'Is oxygen atom mobility in the super-ionic phase achieved through point-defect hopping (Frenkel disorder), interstitial migration, or a fundamentally different transport mechanism?',
    'High-pressure molecular dynamics simulations; X-ray diffraction of quenched high-pressure samples; comparison with ionic conductivity measurements of iron-oxygen systems at extreme conditions',
    'Different mechanisms would refine the theoretical model but would not change the classification — the super-ionic phase would still be a natural consequence of thermodynamic conditions. The constraint remains mountain regardless of transport mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oxygen_mobility_mechanism, empirical, 'Microscopic mechanism of oxygen atom mobility in the super-ionic phase').

omega_variable(
    boundary_layer_interpretation,
    'Does the velocity discontinuity observed at the inner core boundary represent a true phase boundary or a compositional/thermal gradient across a diffuse boundary layer?',
    'High-resolution seismic imaging; modeling of thermal and compositional transport across the core-outer core interface; analysis of velocity gradient sharpness',
    'Regardless of boundary character, the super-ionic state exists in the inner core interior at conditions where it is thermodynamically stable. The constraint classification remains mountain — the phase is determined by pressure and temperature, not by boundary interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_layer_interpretation, empirical, 'Nature of the inner core boundary layer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geophysics_superionic_core, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(superionic_tr_t0, geophysics_superionic_core, theater_ratio, 0, 0.12).
narrative_ontology:measurement(superionic_tr_t2, geophysics_superionic_core, theater_ratio, 2, 0.14).
narrative_ontology:measurement(superionic_tr_t4, geophysics_superionic_core, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(superionic_be_t0, geophysics_superionic_core, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(superionic_be_t2, geophysics_superionic_core, base_extractiveness, 2, 0.11).
narrative_ontology:measurement(superionic_be_t4, geophysics_superionic_core, base_extractiveness, 4, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geophysics_superionic_core, information_standard).
narrative_ontology:affects_constraint(geophysics_superionic_core, earth_magnetic_field_generation).
narrative_ontology:affects_constraint(geophysics_superionic_core, core_mantle_boundary_dynamics).
narrative_ontology:affects_constraint(geophysics_superionic_core, iron_phase_diagram_at_extreme_pressure).

% DUAL FORMULATION NOTE:
% The super-ionic state is a primary physical constraint that affects multiple downstream phenomena in geodynamics. The Earth's magnetic field generation depends on electrical conductivity in the outer core and the thermal boundary layer at the inner core boundary, which are affected by the super-ionic phase properties. The core-mantle boundary dynamics are influenced by the thermal and compositional structure of the inner core. The iron phase diagram at extreme pressure is the fundamental constraint from which the super-ionic state emerges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
