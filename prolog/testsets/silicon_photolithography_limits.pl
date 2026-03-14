% ============================================================================
% CONSTRAINT STORY: silicon_photolithography_limits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silicon_photolithography_limits, []).

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
 *   constraint_id: silicon_photolithography_limits
 *   human_readable: Silicon Photolithography Physical Limits
 *   domain: physics/semiconductor_manufacturing
 *
 * SUMMARY:
 *   Silicon photolithography has enabled five decades of exponential
 *   transistor scaling through increasingly sophisticated optical engineering
 *   — immersion lithography, extreme ultraviolet (EUV) sources, and
 *   computational lithography. Yet the constraint underlying all
 *   photolithography is physical, not institutional: classical optics and
 *   quantum mechanics impose hard limits on the smallest feature that optical
 *   systems can print. The Rayleigh diffraction criterion (minimum feature
 *   size ≈ wavelength / (2×numerical aperture)) is a natural law, as is
 *   quantum tunneling probability through resist films and gate dielectrics
 *   at nanometer scales. This constraint is invariant across all measurement
 *   methodologies and all institutional contexts. It exhibits zero degrees of
 *   freedom — no engineering, investment, or organizational arrangement can
 *   abolish the speed of light or Planck's constant. Extractiveness (0.18)
 *   reflects that the constraint does not extract in the economic sense; it
 *   is a floor, not a mechanism of transfer. Suppression (0.04) is minimal —
 *   the constraint operates through transparent physical mechanisms, not
 *   coercion or information asymmetry. Theater (0.12) is low —
 *   photolithography process reports measure directly against physical
 *   reality (feature size measurements via scanning electron microscopy,
 *   resist profiles, electrical parametrics); there is minimal room for
 *   performative claims.
 *
 * KEY AGENTS:
 *   - Semiconductor Manufacturers: Embedded in the constraint (powerful/mobile) — operate within the diffraction and tunneling boundaries; cannot negotiate exit
 *   - Equipment Manufacturers (ASML, Canon, Nikon): Powerful actors (powerful/mobile) — develop tools to approach the limits but cannot transcend them
 *   - Process Engineers: Craft specialists (moderate/constrained) — innovate within the constraint's boundaries (multiple-patterning, resist chemistry, computational lithography)
 *   - Physics Community: Analytical observers (analytical/analytical) — understand the natural law basis; no institutional bias toward either confirmation or denial
 *   - Alternative Lithography Researchers: Specialized actors (organized/constrained) — explore non-photonic paths (electron-beam, EUV, directed self-assembly) but each method has its own physical limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silicon_photolithography_limits, 0.18).
domain_priors:suppression_score(silicon_photolithography_limits, 0.04).
domain_priors:theater_ratio(silicon_photolithography_limits, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silicon_photolithography_limits, extractiveness, 0.18).
narrative_ontology:constraint_metric(silicon_photolithography_limits, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(silicon_photolithography_limits, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(silicon_photolithography_limits, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(silicon_photolithography_limits, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silicon_photolithography_limits, mountain).
narrative_ontology:human_readable(silicon_photolithography_limits, "Silicon Photolithography Physical Limits").
narrative_ontology:topic_domain(silicon_photolithography_limits, "physics/semiconductor_manufacturing").

domain_priors:emerges_naturally(silicon_photolithography_limits).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANUFACTURER (MOUNTAIN) — All semiconductor fabrication is bound by the diffraction limit and quantum mechanical tunneling constraints. No exit exists from these physical laws. The manufacturer cannot negotiate or circumvent the wavelength-scale feature limits; they can only adapt process chemistry and masking strategies within this immutable boundary.
constraint_indexing:constraint_classification(silicon_photolithography_limits, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EQUIPMENT MANUFACTURER (MOUNTAIN) — Despite global market dominance and billions in R&D investment, the equipment maker cannot escape the Rayleigh criterion or quantum tunneling physics. Physical constraints are indifferent to corporate power or financial resources. The constraint is immutable even at the highest material and institutional power levels.
constraint_indexing:constraint_classification(silicon_photolithography_limits, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Silicon photolithography bottlenecks emerge directly from quantum mechanics and classical optics. The Rayleigh diffraction limit (d ≈ λ/2NA), quantum tunneling probability scaling, and thermodynamic entropy constraints are natural laws. No institutional, economic, or technical arrangement can abolish these limits. This is the canonical mountain — a constraint that requires no enforcement, exhibits zero degrees of freedom, and persists across all measurement methodologies.
constraint_indexing:constraint_classification(silicon_photolithography_limits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silicon_photolithography_limits_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(silicon_photolithography_limits, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silicon_photolithography_limits, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(silicon_photolithography_limits, ExtMetricName, E),
    domain_priors:suppression_score(silicon_photolithography_limits, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(silicon_photolithography_limits),
    narrative_ontology:constraint_metric(silicon_photolithography_limits, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(silicon_photolithography_limits, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(silicon_photolithography_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The constraint imposes a physical ceiling, not a mechanism of extraction from one party to another. The boundary exists regardless of who is measuring or paying. No one extracts value from the constraint itself — the constraint is a limitation on what can be printed, not a redistribution mechanism. Suppression (0.04): Minimal. The constraint operates through open physical mechanisms (diffraction, quantum mechanics) that are fully transparent and measurable. There are no hidden mechanisms, no information asymmetries, no coercion — the constraint simply is. Theater (0.12): Very low. Photolithography metrics are directly observable: feature size is measured via scanning electron microscopy, resist profiles via cross-section analysis, electrical function via on-wafer testing. Claims about lithography capability can be falsified in real time. There is almost no room for performative or theatrical claims to persist.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this constraint because it is truly universal. All perspectives (manufacturer, equipment maker, analyst) arrive at the same classification: mountain. Power level does not change the classification — even the most capable equipment manufacturers cannot print features smaller than the diffraction limit or prevent quantum tunneling. Exit options do not change it — there is no 'exit' from physics. Time horizon does not matter — the constraint has been present since the first photolithography system and will persist as long as photons and atoms behave according to quantum mechanics. The uniformity across all perspectives confirms that this is a natural law constraint, not a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation does not apply to mountain constraints — there are no beneficiaries or victims, no extraction direction, no agent-relative perception. The constraint is agent-invariant. All agents experience the same immutable limit regardless of their structural position. Directionality d is undefined (or uniformly constant across all agents at d ≈ 0.5, representing neutral exposure to an external boundary). The chi formula χ = ε × f(d) × σ(S) becomes degenerate because ε is so low (0.18) that chi remains near zero regardless of f(d) or scope modifiers. This is the signature of a mountain: the classification is independent of who is measuring and from where.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW CONFIRMATION: This constraint resolves all mandatrophy risks because there is no mandatrophy to resolve. Mandatrophy (Type V error — misclassifying extraction as coordination) arises when an asymmetric mechanism claims to be coordination. Silicon photolithography limits claim to be physics, and they are. There is no hidden extraction mechanism. There are no beneficiaries or victims relative to the constraint itself. The constraint is not enforced by any institutional actor — it is enforced by the speed of light and Planck's constant. The analytical observer sees exactly what the affected manufacturers see: an immutable physical boundary. No perspective-dependent reclassification occurs. This is the cleanest mountain case: the natural law claim is genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extreme_ultraviolet_feasibility,
    'Can extreme ultraviolet (EUV) lithography extend the photolithography regime indefinitely, or does it hit its own insurmountable physical limits?',
    'Empirical tracking of EUV source power scaling limits, photoresist sensitivity degradation at shorter wavelengths, and quantum noise in photon detection at extreme ultraviolet wavelengths',
    'If EUV can extend indefinitely: photolithography constraint shifts to resource scarcity (cost of EUV plants), not physics. If EUV hits its own wall: the constraint remains mountain, but the boundary shifts to a smaller feature size. Either way, a hard physical limit exists somewhere.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extreme_ultraviolet_feasibility, empirical, 'Whether EUV lithography can indefinitely extend photolithography regime').

omega_variable(
    quantum_tunneling_vs_feature_control,
    'At what feature size does quantum tunneling through resist films and gate dielectrics become the dominant failure mechanism, making gate control and pattern transfer impossible?',
    'Empirical characterization of tunneling current and resist integrity at sub-3nm feature sizes; direct measurement of gate control loss due to tunneling leakage',
    'If tunneling dominates below 5nm: hard physical wall. If tunneling is manageable to 2nm: the constraint extends further but still terminates. The exact boundary matters less than the existence of the boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_tunneling_vs_feature_control, empirical, 'Feature size at which quantum tunneling dominates resist and gate control').

omega_variable(
    alternative_lithography_regimes,
    'Do non-photonic approaches (electron-beam lithography, ion-beam lithography, directed self-assembly, nanoimprint) offer genuine alternatives or do they merely shift the constraint to different physical bottlenecks (throughput, defect rates, cost)?',
    'Economic and technological comparison of alternative lithography methods at production scale; assessment of whether they can replace optical lithography for commodity chip manufacturing or only for niche applications',
    'If alternatives can truly replace photolithography: this constraint becomes about manufacturing economics, not physics. If alternatives have their own immutable limits: the constraint family fragments into multiple mountains, one per lithography method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_lithography_regimes, empirical, 'Whether non-photonic lithography methods can replace photolithography at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silicon_photolithography_limits, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(silphotolith_tr_t0, silicon_photolithography_limits, theater_ratio, 0, 0.12).
narrative_ontology:measurement(silphotolith_tr_t20, silicon_photolithography_limits, theater_ratio, 20, 0.12).
narrative_ontology:measurement(silphotolith_tr_t40, silicon_photolithography_limits, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(silphotolith_be_t0, silicon_photolithography_limits, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(silphotolith_be_t20, silicon_photolithography_limits, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(silphotolith_be_t40, silicon_photolithography_limits, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silicon_photolithography_limits, information_standard).
narrative_ontology:affects_constraint(silicon_photolithography_limits, semiconductor_cost_scaling).
narrative_ontology:affects_constraint(silicon_photolithography_limits, quantum_tunneling_leakage).
narrative_ontology:affects_constraint(silicon_photolithography_limits, extreme_ultraviolet_feasibility).

% DUAL FORMULATION NOTE:
% Silicon photolithography limits is a master constraint that influences downstream semiconductor economics (cost scaling follows Moore's Law until photolithography hits walls), quantum tunneling mechanisms in devices, and the feasibility of alternative lithography methods. Each downstream constraint has its own ε value reflecting domain-specific extraction or coordination; this master constraint has minimal ε because it is purely a physical boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
