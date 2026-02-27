% ============================================================================
% CONSTRAINT STORY: geophysics_superionic_core
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: geophysics/mineral_physics/seismology
 *
 * SUMMARY:
 *   The super-ionic state of matter in Earth's inner core represents a
 *   natural law constraint: a discoverable physical property of
 *   hydrogen-bearing iron oxides under the extreme pressure (330–360 GPa) and
 *   temperature (5200–5700 K) conditions found 5,200 km below Earth's
 *   surface. Recent seismic studies, supported by diamond-anvil cell
 *   experiments and mineral physics modeling, indicate that oxygen atoms form
 *   a rigid close-packed lattice while hydrogen ions diffuse freely through
 *   interstitial sites, dramatically increasing ionic conductivity and
 *   altering seismic wave velocities and attenuation. This constraint
 *   exhibits no exploitative asymmetry, no suppression of alternatives
 *   (because there are no alternatives given the physical conditions), and no
 *   performance theater. It is not discovered through institutional
 *   negotiation or convention; it is revealed through systematic observation
 *   of wave propagation through Earth's interior. The constraint is invariant
 *   across all perspectives: no observer with accurate seismic data can deny
 *   or escape the super-ionic phase transition at those depths and
 *   temperatures. This makes it a canonical mountain constraint — an
 *   immutable property of physical law that all agents must accommodate.
 *
 * KEY AGENTS:
 *   - Seismic wave propagation: Primary measurable phenomenon (analytical/analytical) — compressional and shear velocities, attenuation patterns reveal phase structure
 *   - Geophysicists/mineral physicists: Primary investigators (powerful/mobile) — access shock labs and seismic arrays; discover and characterize the phase transition
 *   - Global seismic array network: Observational infrastructure (institutional/arbitrage) — IRIS, Geoscope, regional networks provide the empirical foundation for constraint detection
 *   - Earth's material: Agent subject to constraint (powerless/trapped) — inner core iron oxides have no choice but to exhibit super-ionic state under the given conditions
 *   - Alternative explanatory frameworks: Analytical competitors (analytical/analytical) — grain-size effects, crystallographic texture, phase separation models that might explain seismic anomalies without invoking super-ionicity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geophysics_superionic_core, 0.12).
domain_priors:suppression_score(geophysics_superionic_core, 0.03).
domain_priors:theater_ratio(geophysics_superionic_core, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, extractiveness, 0.12).
narrative_ontology:constraint_metric(geophysics_superionic_core, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(geophysics_superionic_core, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geophysics_superionic_core, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geophysics_superionic_core, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geophysics_superionic_core, mountain).
narrative_ontology:human_readable(geophysics_superionic_core, "Super-ionic state of matter in Earth's inner core").
narrative_ontology:topic_domain(geophysics_superionic_core, "geophysics/mineral_physics/seismology").

domain_priors:emerges_naturally(geophysics_superionic_core).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / SEISMIC PHYSICS (MOUNTAIN) — From the civilizational/universal view of geophysical constraint, the super-ionic state emerges as an inevitable consequence of extreme pressure and temperature at Earth's core boundary. Seismic wave behavior (compressional and shear velocity structure, attenuation patterns) reflects the phase transition to a state where oxygen sublattice is rigid while hydrogen ions diffuse freely. This is a natural law of matter under conditions ~330-360 GPa and ~5200-5700 K. No degree of freedom: the ionic conductivity and sound velocity structure follow from thermodynamics and quantum mechanics. d≈0.73, f(d)≈1.15, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: GEOPHYSICIST / MINERAL PHYSICIST (MOUNTAIN) — Researchers with access to shock compression labs, seismic arrays, and computational modeling tools observe the super-ionic phase as a fixed, discoverable property of H-bearing iron oxides. The constraint is inaccessible to alternative approaches: you cannot avoid the phase transition by choosing a different experimental method — it is inherent to the material at those conditions. Seismic observations unambiguously constrain the phase state. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.07.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SEISMIC ARRAY NETWORK / EARTH OBSERVATION SYSTEM (MOUNTAIN) — Global seismic networks (IRIS, Geoscope, regional arrays) measure velocity anomalies and attenuation at the inner core boundary and reveal the presence of a super-ionic layer. The constraint appears as a natural physical boundary: you cannot build a seismic network that reads a different phase state at that location. The network experiences this as a fixed feature of Earth's structure, not subject to institutional negotiation. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARTH SYSTEM ITSELF (MOUNTAIN) — The inner core has no choice but to exhibit the super-ionic state under those conditions. The constraint is absolute: the material properties are fixed by thermodynamics. No exit, no suppression of alternatives (there are no alternatives). d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(geophysics_superionic_core, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
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
 *   Extractiveness (0.12): Minimal. The super-ionic state does not extract value from any agent; it is a fixed property of matter. The 0.12 is near the mountain floor (ε ≤ 0.25) and reflects only residual measurement uncertainty and the modest empirical gap (seismic observations do not perfectly determine water content or phase-boundary sharpness). Suppression (0.03): Negligible. No agent suppresses knowledge of this constraint; no alternatives are coercively forbidden. The low value reflects that the constraint is entirely transparent to accurate measurement. Theater ratio (0.15): Minimal. Seismic observations map directly to physical properties with high fidelity. There is some performance in the interpretation (researchers must design experiments, argue for the super-ionic interpretation against alternatives), but the core measurement-to-physics mapping is direct. The constraint emerges naturally from the material and is not maintained through performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type (mountain-only) constraint. All four perspectives classify as mountain because the constraint is invariant across all observation sites. The seismic physicist observes the same phase transition as the analytical observer; the institutional seismic network measures the same velocity structure as the powerless earth material must exhibit. There is no perspectival gap — no agent experiences this constraint differently. This uniformity is diagnostic of a true natural law. When a constraint exhibits the same classification from all indices, it signals that the agent's power, time horizon, and exit options do not influence the constraint's structural properties. This is the defining mark of mountain constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim structure exists for this constraint. All agents are symmetrically positioned: they all confront the same fixed physical reality. Directionality values are derived from the canonical atoms: analytical→d≈0.73, powerful→d≈0.48, institutional→d≈0.05, powerless→d≈1.0. These vary only in their structural position relative to knowledge and measurement, not in their ability to negotiate or escape the constraint. The canonical fallback chain applies directly: the constraint's physical absoluteness means directionality reflects the agent's capacity to observe or investigate, not their power to negotiate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    water_content_quantification,
    'What is the actual hydrogen/water concentration in the inner core iron oxides, and how does this affect the magnitude of ionic conductivity and seismic attenuation?',
    'High-pressure diamond-anvil experiments with varying water content; acoustic velocity and electrical conductivity measurements across phase boundaries; seismic inversion with water-dependent elastic moduli',
    'If water content is high (>1 wt%): super-ionic conductivity dominates the core''s thermal and electrical properties, confirming the phase transition signature in seismic data. If low (<0.1 wt%): super-ionic state may be present but contributes minimally to observable seismic patterns, shifting classification toward a marginally-detectable constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(water_content_quantification, empirical, 'Water content in inner core iron oxides and its effect on ionic conductivity').

omega_variable(
    phase_boundary_sharpness,
    'Is the transition to super-ionic state a sharp first-order phase boundary or a broad region of partial ionic mobility (crossover)?',
    'Lattice-dynamics simulations; high-resolution velocity and attenuation profiles across the inner core boundary; comparison with transitions in analogue materials (superionic water ice, yttria-stabilized zirconia)',
    'If sharp: seismic signatures (velocity jumps, attenuation peaks) are diagnostic and unambiguous — mountain classification confirmed. If broad crossover: the constraint is less a fixed boundary and more a gradient; ambiguity increases about where ''super-ionic state'' begins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_boundary_sharpness, empirical, 'Sharpness of the super-ionic phase transition at the inner core boundary').

omega_variable(
    alternative_phase_explanations,
    'Could the observed seismic anomalies at the inner core boundary be explained by non-ionic mechanisms: grain-size effects, phase separation, or crystallographic texture rather than a hydrogen-ion conductivity transition?',
    'Systematic comparison of seismic predictions from super-ionic models vs alternative phase models; forward modeling with varying grain size, crystal orientation, and phase composition; new constraints from core-reflected seismic phases and normal modes',
    'If super-ionic explanation is unique: the constraint is a natural law discovered, not constructed. If multiple explanations fit the data equally: the constraint is partially observational and the classification may shift toward Rope (coordinating competing interpretations) rather than pure Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_phase_explanations, empirical, 'Whether seismic anomalies require super-ionic mechanism or allow alternative explanations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geophysics_superionic_core, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(superionic_tr_t0, geophysics_superionic_core, theater_ratio, 0, 0.1).
narrative_ontology:measurement(superionic_tr_t5, geophysics_superionic_core, theater_ratio, 5, 0.12).
narrative_ontology:measurement(superionic_tr_t10, geophysics_superionic_core, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(superionic_be_t0, geophysics_superionic_core, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(superionic_be_t5, geophysics_superionic_core, base_extractiveness, 5, 0.11).
narrative_ontology:measurement(superionic_be_t10, geophysics_superionic_core, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geophysics_superionic_core, information_standard).
narrative_ontology:affects_constraint(geophysics_superionic_core, earth_core_heat_flow).
narrative_ontology:affects_constraint(geophysics_superionic_core, magnetic_field_generation).
narrative_ontology:affects_constraint(geophysics_superionic_core, seismic_attenuation_patterns).

% DUAL FORMULATION NOTE:
% The super-ionic state is a foundational constraint in core physics. Upstream: depends on the thermodynamic stability of hydrogen-bearing iron oxides under extreme conditions (constraint: iron_oxide_phase_diagram). Downstream: affects thermal conductivity, electrical conductivity, and seismic wave propagation in the inner core. These downstream constraints have higher ε values because they involve extraction of value or information from the super-ionic property, whereas the super-ionic state itself is a pure natural law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
