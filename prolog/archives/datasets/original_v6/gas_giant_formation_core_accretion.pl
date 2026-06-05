% ============================================================================
% CONSTRAINT STORY: gas_giant_formation_core_accretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gas_giant_formation_core_accretion, []).

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
 *   constraint_id: gas_giant_formation_core_accretion
 *   human_readable: Core Accretion Theory of Gas Giant Formation
 *   domain: planetary_science/astrophysics
 *
 * SUMMARY:
 *   Core accretion theory describes how planetary cores form through
 *   gravitational collapse and collision of planetesimals in protoplanetary
 *   disks, followed by runaway accretion of gas if the core reaches
 *   sufficient mass before the disk dissipates. This constraint operates as a
 *   natural law governing all observed gas giant formation. The mechanism is
 *   invariant across all stellar systems: gravitational dynamics determine
 *   core growth rates, and disk physics determines the timescale for gas
 *   availability. No agent or observer can exit this constraint — it binds
 *   the formation of planetary systems across the observable universe. The
 *   constraint classifies as mountain from all perspectives because the
 *   underlying physics admits no alternatives and no degrees of freedom for
 *   strategic behavior.
 *
 * KEY AGENTS:
 *   - Protoplanetary Disk: Substrate (powerless/trapped) — passive gravitational field subject to accretion dynamics; bears the constraint completely
 *   - Planetary Core: Emergent agent (powerless/trapped) — growth is determined entirely by available material and gravitational interactions; no exit options
 *   - Exoplanet Observational Networks: Institutional observer (organized/analytical) — detect signatures of core accretion across all systems; cannot identify alternative formation pathways
 *   - Planetary Science Community: Knowledge producer (institutional/analytical) — theoretically models accretion processes; recognizes universality of the constraint
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes core accretion as an irreducible physical law invariant across all contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gas_giant_formation_core_accretion, 0.12).
domain_priors:suppression_score(gas_giant_formation_core_accretion, 0.03).
domain_priors:theater_ratio(gas_giant_formation_core_accretion, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, extractiveness, 0.12).
narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gas_giant_formation_core_accretion, mountain).
narrative_ontology:human_readable(gas_giant_formation_core_accretion, "Core Accretion Theory of Gas Giant Formation").
narrative_ontology:topic_domain(gas_giant_formation_core_accretion, "planetary_science/astrophysics").

domain_priors:emerges_naturally(gas_giant_formation_core_accretion).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTOPLANETARY DISK (MOUNTAIN) — The accretion process is an unavoidable consequence of gravitational dynamics operating on dust and gas in protoplanetary disks. No observer or agent can exit this constraint; it is a fixed physical law governing planetary formation across all stellar systems.
constraint_indexing:constraint_classification(gas_giant_formation_core_accretion, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — Core accretion theory describes an irreducible physical constraint: planetary cores form through deterministic gravitational collapse and collision processes. The mechanism is invariant across all exoplanetary systems observed to date. No observational basis or theoretical framework permits exit from this constraint.
constraint_indexing:constraint_classification(gas_giant_formation_core_accretion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PLANETARY SCIENCE COMMUNITY (MOUNTAIN) — Institutional observers across all research groups recognize core accretion as a natural law governing planetary formation. The constraint operates identically regardless of observation method, measurement precision, or theoretical framework applied. Research institutions cannot escape or circumvent this physical requirement.
constraint_indexing:constraint_classification(gas_giant_formation_core_accretion, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: EXOPLANET DETECTION NETWORKS (MOUNTAIN) — Organized collaborative observation across all exoplanet surveys (TESS, Kepler legacy, radial velocity networks) confirms the constraint is invariant. No detection method reveals an alternative formation pathway. The constraint binds all observed exoplanetary systems uniformly.
constraint_indexing:constraint_classification(gas_giant_formation_core_accretion, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gas_giant_formation_core_accretion_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(gas_giant_formation_core_accretion, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gas_giant_formation_core_accretion, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, ExtMetricName, E),
    domain_priors:suppression_score(gas_giant_formation_core_accretion, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(gas_giant_formation_core_accretion),
    narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(gas_giant_formation_core_accretion, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(gas_giant_formation_core_accretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. Core accretion imposes no extraction in the sense of asymmetric value capture — it is a physical process operating without beneficiaries or victims. The minimal value reflects measurement of constraint coerciveness rather than economic extraction. Suppression (0.03): Negligible. Accretion processes operate with full determinism; there is no coercion mechanism because there is no alternative behavior. Theater ratio (0.15): Very low. The constraint's functional content is nearly 100% genuine physics; observational data from exoplanet surveys, disk imaging, and radiative transfer models directly verify accretion signatures without performative overlay. The small theater component reflects minor uncertainties in parameter estimates and model simplifications necessary for computational tractability. Accessibility collapse (0.88): Very high. All protoplanetary disk systems without exception undergo core accretion if gravitationally bound; there is no accessible alternative pathway. Resistance (0.08): Very low. No agent or process resists the constraint; it operates independently of any observer's preferences or actions.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists. All six positions (if computed) would classify this constraint as mountain because the underlying physics is invariant across all observation frames. Powerless agents experience it as immutable law. Institutional observers confirm its universality. Analytical observers recognize its necessity. Beneficiaries and victims do not exist because there is no extraction. This uniformity across perspectives is the diagnostic signature of a true natural law constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis does not apply to this constraint. Core accretion imposes no directional extraction; it is a symmetrical physical process. All agents (disk, cores, stars) are equally subject to gravitational dynamics. The beneficiary/victim framework is inapplicable because there is no asymmetric value transfer. The constraint operates uniformly on all particles in the system without strategic differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by eliminating the ambiguity: it is unambiguously a mountain. All six possible perspectives classify it identically. The extracted effectiveness chi is determined entirely by physics, not by observer position or institutional interest. The constraint is not performing coordination (beneficiaries and victims are absent). It is not temporary (no sunset clause applies to planetary physics). It is not degraded (theater ratio is minimal). It is not hybrid (no extraction and coordination coexist). The mandatrophy is resolved by recognizing that this class of constraint — fundamental physical law — has zero degrees of freedom across all indexical dimensions. The classification is determinate and invariant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disk_migration_threshold_ambiguity,
    'Does core migration before gas accretion completes represent a modification of core accretion or a distinct formation pathway?',
    'High-resolution disk simulation and comparison with observed exoplanet architectures; distinguish between core accretion with migration and alternative in-situ formation mechanisms',
    'If migration is integral to core accretion: constraint remains mountain. If migration represents distinct pathway: constraint is more epistemically constrained than classification suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disk_migration_threshold_ambiguity, empirical, 'Whether planetary migration constitutes modification of core accretion or distinct pathway').

omega_variable(
    pebble_accretion_mechanism_status,
    'Does pebble accretion represent a subcategory of core accretion or a mechanistically distinct formation mode?',
    'Theoretical integration of pebble accretion into core accretion framework; laboratory measurements of dust collision properties and disk turbulence effects',
    'If subsumed into core accretion: framework remains unified. If mechanistically independent: core accretion is narrower than claimed, reducing accessibility collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pebble_accretion_mechanism_status, conceptual, 'Whether pebble accretion is subsumed within core accretion').

omega_variable(
    super_earth_formation_mechanism,
    'Do super-Earths and mini-Neptunes form through core accretion mechanisms identical to gas giants, or do they require fundamentally different physics?',
    'Exoplanet mass distribution analysis; disk modeling of super-Earth formation timescales; compositional inference from transmission spectroscopy',
    'If identical: core accretion constraint applies universally across all planetary masses. If different: constraint has narrower scope than suggested by mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(super_earth_formation_mechanism, empirical, 'Whether super-Earths form via identical core accretion mechanisms').

omega_variable(
    disk_dissipation_timing_constraint,
    'Is the requirement that planetary cores reach runaway gas accretion before disk dissipation a fundamental physical limit or an empirical timing accident?',
    'Disk lifetime measurements from infrared surveys; protoplanetary disk age dating; correlation between disk properties and planetary system architecture',
    'If fundamental: suppression value is too low, constraint is more coercive than measured. If empirical accident: constraint permits wider formation pathways than currently observed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disk_dissipation_timing_constraint, empirical, 'Whether disk dissipation timing is fundamental or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gas_giant_formation_core_accretion, 0, 4000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ggfca_tr_t0, gas_giant_formation_core_accretion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ggfca_tr_t2000, gas_giant_formation_core_accretion, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(ggfca_tr_t4000, gas_giant_formation_core_accretion, theater_ratio, 4000, 0.15).

% Extraction over time
narrative_ontology:measurement(ggfca_be_t0, gas_giant_formation_core_accretion, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ggfca_be_t2000, gas_giant_formation_core_accretion, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(ggfca_be_t4000, gas_giant_formation_core_accretion, base_extractiveness, 4000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gas_giant_formation_core_accretion, global_infrastructure).
narrative_ontology:affects_constraint(gas_giant_formation_core_accretion, exoplanet_mass_radius_relationship).
narrative_ontology:affects_constraint(gas_giant_formation_core_accretion, habitable_zone_planetary_composition).
narrative_ontology:affects_constraint(gas_giant_formation_core_accretion, protoplanetary_disk_lifetime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
