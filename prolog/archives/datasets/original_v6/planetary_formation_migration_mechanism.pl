% ============================================================================
% CONSTRAINT STORY: planetary_formation_migration_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_formation_migration_mechanism, []).

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
 *   constraint_id: planetary_formation_migration_mechanism
 *   human_readable: Planetary Formation and Migration Mechanism
 *   domain: astrophysics/planetary_science
 *
 * SUMMARY:
 *   Planetary formation and migration represents a fundamental constraint in
 *   astrophysics: planets orbiting main-sequence stars exhibit orbital
 *   configurations (hot Jupiters, close-in superEarths, mean-motion
 *   resonances) that cannot be explained by formation alone at their current
 *   locations under standard disk physics. The mechanism by which planetary
 *   embryos and cores change their orbital parameters — through gravitational
 *   interactions with protoplanetary disks, resonant interactions with other
 *   planets, and dynamical scattering — is an immutable consequence of
 *   orbital mechanics and conservation of angular momentum. This constraint
 *   exhibits mountain characteristics across all observational frames: it is
 *   invariant, emerges naturally from first principles, has extremely low
 *   extractiveness and suppression (the physics operates regardless of any
 *   agent or institutional framework), and shows zero degrees of freedom. No
 *   known planetary system can avoid or exit the effects of orbital dynamics
 *   and migration. The constraint is not subject to policy, institutional
 *   arrangements, or observational choice — it is a physical law.
 *
 * KEY AGENTS:
 *   - Forming Planetary Systems: Primary actor (powerless/trapped) — subject to immutable gravitational dynamics with no exit options
 *   - Protoplanetary Disks: Secondary actor (powerless/trapped) — exchange angular momentum with planets through tidal torques; no alternative mechanism exists
 *   - Stellar Gravity Field: Primary structural element (powerful/mobile) — establishes the potential well that dominates all planetary motion
 *   - Exoplanet Research Community: Analytical observer (organized/constrained) — studies the constraint through observation and simulation but cannot modify the underlying physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_formation_migration_mechanism, 0.12).
domain_priors:suppression_score(planetary_formation_migration_mechanism, 0.03).
domain_priors:theater_ratio(planetary_formation_migration_mechanism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, extractiveness, 0.12).
narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_formation_migration_mechanism, mountain).
narrative_ontology:human_readable(planetary_formation_migration_mechanism, "Planetary Formation and Migration Mechanism").
narrative_ontology:topic_domain(planetary_formation_migration_mechanism, "astrophysics/planetary_science").

domain_priors:emerges_naturally(planetary_formation_migration_mechanism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FORMING PLANETARY SYSTEM (MOUNTAIN) — Planetary embryos and cores are subject to gravitational dynamics and disk migration regardless of any theoretical framework or observational perspective. The physics is immutable: gravity acts; tidal interactions occur; orbital decay follows from angular momentum exchange. No system can 'exit' its own gravitational environment. Zero degrees of freedom.
constraint_indexing:constraint_classification(planetary_formation_migration_mechanism, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE STELLAR GRAVITY FIELD (MOUNTAIN) — The primary gravitational structure (stellar mass potential) that dominates planetary motion is a constraint that cannot be negotiated or circumvented by any planetary process. It emerges from stellar physics and cannot be exited or suppressed by disk dynamics or planetary interaction.
constraint_indexing:constraint_classification(planetary_formation_migration_mechanism, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of celestial mechanics and gravitational theory, planetary migration through disk interaction is a mathematical consequence of conservation laws and orbital mechanics. The constraint is a natural law: given a protoplanetary disk and a forming planetary system, migration follows necessarily from angular momentum transfer and tidal torques. This is not a policy or institutional arrangement — it is a physical necessity.
constraint_indexing:constraint_classification(planetary_formation_migration_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE EXOPLANET RESEARCH COMMUNITY (MOUNTAIN) — Despite decades of effort to test alternative formation and migration models (in-situ formation, multi-stage migration, planet-planet scattering), the fundamental constraint remains: planets in systems with hot Jupiters and close-in superEarths must have migrated or formed with their present orbital parameters. No realistic formation model escapes this requirement. The constraint is resistant to theoretical alternatives — every viable model must accommodate migration as a structural feature.
constraint_indexing:constraint_classification(planetary_formation_migration_mechanism, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_formation_migration_mechanism_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(planetary_formation_migration_mechanism, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_formation_migration_mechanism, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, ExtMetricName, E),
    domain_priors:suppression_score(planetary_formation_migration_mechanism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(planetary_formation_migration_mechanism),
    narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(planetary_formation_migration_mechanism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(planetary_formation_migration_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The constraint shows minimal extractiveness because no agent is being extracted FROM — it is a structural feature of orbital mechanics with no beneficiaries or victims. The small non-zero value reflects measurement uncertainty and the reality that the constraint can be characterized in different ways depending on observational methodology. Suppression (0.03): Negligible. There are no alternatives to planetary migration given the observed orbital configurations; the physics is not suppressed by competing mechanisms. Theater ratio (0.15): Very low. The constraint's function is entirely transparent to theory — there is no performative overlay. Models differ in detail (Type I vs Type II migration, disk structure, planet-planet scattering), but all agree on the fundamental mechanism. The slight theater reflects that observational verification of migration in forming systems (rather than just its necessity in explaining final states) remains challenging and partly indirect.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify identically as mountain because the constraint is truly invariant across observational frames. A forming planetary system cannot experience migration differently than a distant observer predicts it based on gravitational theory. The unification of all perspectives is itself the diagnostic signal: when a constraint appears immutable from the position of the powerless agent, the beneficiary, the organized observer, and the analytical frame, the classification is robust. There is no perspectival gap to resolve because the constraint has no degrees of freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW EXEMPLAR: Planetary migration satisfies all criteria for mountain classification without ambiguity. Extractiveness ≤ 0.25 (0.12 measured), suppression ≤ 0.05 (0.03 measured), accessibility_collapse ≥ 0.85 (0.92 measured), resistance ≤ 0.15 (0.08 measured), emerges_naturally: true. No mandatrophy is present because there is no extraction asymmetry, no coordination function, and no institutional arrangement. The constraint is a pure natural law. The measurement trajectory (theater_ratio and extractiveness both stable and very low across the interval) confirms that this is not a degraded or temporally shifting constraint — it maintains its character as a fundamental physical principle across all observational timeframes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disk_dissipation_timescale_ambiguity,
    'What is the precise timescale of protoplanetary disk dissipation, and does it constrain the migration window sufficiently to rule out certain multi-stage migration scenarios?',
    'High-resolution infrared and millimeter observations of disk age in nearby star-forming regions; isotopic dating of meteoritic material; direct observations of disk lifetime in young stellar clusters',
    'If dissipation is rapid (< 2–3 Myr): single-stage migration models are strongly favored. If dissipation is extended (5–10 Myr): late-stage planet-planet scattering and secondary migration become viable, increasing model degrees of freedom but not removing the fundamental migration constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disk_dissipation_timescale_ambiguity, empirical, 'Precise timescale of protoplanetary disk dissipation').

omega_variable(
    type_i_vs_type_ii_migration_regime,
    'Under what conditions does Type I migration (planet-disk torque balance on low-mass cores) transition to Type II migration (gap-opening in massive disk, planet-gap co-evolution)?',
    'Hydrodynamic simulations with varying disk masses, turbulence, and planet masses; comparison with core accretion timescales; direct measurements of disk structure around young systems',
    'If Type I dominates for all observed systems: migration is smooth and deterministic. If Type II is frequent: migration exhibits episodic behavior and stronger coupling to disk evolution, but the constraint (migration must occur) remains invariant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(type_i_vs_type_ii_migration_regime, empirical, 'Transition between Type I and Type II planetary migration regimes').

omega_variable(
    stochastic_planetesimal_scattering_role,
    'How much of the observed exoplanet orbital configuration is due to deterministic disk migration versus stochastic gravitational scattering by planetesimals and residual planetary embryos?',
    'N-body simulations with realistic planetesimal populations; statistical comparison of predicted vs observed orbital eccentricity and inclination distributions; direct imaging of debris around young systems',
    'If scattering dominates: migration becomes a secondary rather than primary shaping force, but planets still must achieve their final orbital parameters through some combination of migration and scattering. The constraint is unchanged — planets must move.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stochastic_planetesimal_scattering_role, empirical, 'Relative contribution of deterministic migration versus stochastic gravitational scattering').

omega_variable(
    in_situ_formation_falsifiability,
    'Can in-situ formation (planets forming at their current orbital locations without subsequent migration) be definitively ruled out, or will observational limitations always leave this as a viable alternative model?',
    'Measurements of metallicity gradients in protoplanetary disks; observations of planet orbital parameters in systems at different ages; chemical abundance patterns in planetary atmospheres correlated with formation location',
    'If in-situ is ruled out: migration is mandatory by elimination. If in-situ remains viable for some systems: the constraint ''all planets must migrate'' becomes ''planets must either form in situ or migrate,'' which is a weaker but still-immutable natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(in_situ_formation_falsifiability, empirical, 'Whether in-situ planetary formation can be falsified or remains perpetually viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_formation_migration_mechanism, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfmm_tr_t0, planetary_formation_migration_mechanism, theater_ratio, 0, 0.08).
narrative_ontology:measurement(pfmm_tr_t2, planetary_formation_migration_mechanism, theater_ratio, 2, 0.12).
narrative_ontology:measurement(pfmm_tr_t4, planetary_formation_migration_mechanism, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(pfmm_be_t0, planetary_formation_migration_mechanism, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pfmm_be_t2, planetary_formation_migration_mechanism, base_extractiveness, 2, 0.11).
narrative_ontology:measurement(pfmm_be_t4, planetary_formation_migration_mechanism, base_extractiveness, 4, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_formation_migration_mechanism, information_standard).
narrative_ontology:affects_constraint(planetary_formation_migration_mechanism, exoplanet_orbital_eccentricity_distribution).
narrative_ontology:affects_constraint(planetary_formation_migration_mechanism, hot_jupiter_existence_problem).
narrative_ontology:affects_constraint(planetary_formation_migration_mechanism, mean_motion_resonance_stability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
