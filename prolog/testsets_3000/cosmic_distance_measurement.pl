% ============================================================================
% CONSTRAINT STORY: cosmic_distance_measurement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cosmic_distance_measurement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cosmic_distance_measurement
 *   human_readable: Cosmic Distance Measurement and the Hubble Tension
 *   domain: observational_cosmology/fundamental_physics
 *
 * SUMMARY:
 *   The Hubble tension — a persistent 5-sigma discrepancy between
 *   early-universe measurements (Planck, early recombination epoch) and
 *   late-universe measurements (local distance ladder, Type Ia supernovae,
 *   gravitational lensing) of the cosmic expansion rate H0 — illustrates how
 *   measurement standardization creates both genuine coordination benefits
 *   and extractive institutional constraints. The tension cannot be resolved
 *   by improving measurement precision alone; it requires either discovery of
 *   systematic errors in one method or modification of fundamental
 *   cosmological physics. This creates a structural asymmetry: the early
 *   universe framework (Lambda-CDM) benefits from high-precision early-epoch
 *   data and institutional dominance, while late-universe measurement
 *   communities are trapped defending measurements that contradict the
 *   dominant model. The constraint exhibits mixed coordination and
 *   extraction: genuine calibration problems require coordination mechanisms
 *   (standard candles, parallax anchors, lensing time delays), yet the
 *   institutional enforcement of a particular cosmological framework
 *   suppresses alternative measurement schemes and models. Theater ratio
 *   (0.65) reflects that defending distance ladder methodology has become
 *   increasingly performative — resources spent justifying existing
 *   calibrations rather than discovering new systematic errors.
 *
 * KEY AGENTS:
 *   - Early Universe Cosmology Institutions: Primary beneficiary (institutional/arbitrage) — Lambda-CDM framework concentrates funding and attention on Planck, WMAP, CMB-S4 observations
 *   - Late Universe Measurement Teams: Primary victim (powerless/trapped) — Cepheid and supernova teams cannot change foundational calibrations without institutional penalty; trapped defending higher H0 values
 *   - Alternative Cosmology Researchers: Secondary victim (organized/constrained) — Early dark energy, modified gravity, and non-flat models benefit from tension as evidence but face higher evidentiary bars than standard model
 *   - Independent Distance Programs: Organized agents (organized/constrained) — JWST, Vera Rubin, DESI, LIGO building alternative calibrations; see tension as temporary coordination problem with 5-10 year sunset
 *   - Local Distance Ladder Institution: Institutional actor (institutional/arbitrage) — Maintains foundational calibrations through citation inheritance and career path dependence; increasingly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — Risks naturalizing the tension as inherent to measuring cosmic expansion rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cosmic_distance_measurement, 0.35).
domain_priors:suppression_score(cosmic_distance_measurement, 0.42).
domain_priors:theater_ratio(cosmic_distance_measurement, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cosmic_distance_measurement, extractiveness, 0.35).
narrative_ontology:constraint_metric(cosmic_distance_measurement, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cosmic_distance_measurement, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cosmic_distance_measurement, tangled_rope).
narrative_ontology:human_readable(cosmic_distance_measurement, "Cosmic Distance Measurement and the Hubble Tension").
narrative_ontology:topic_domain(cosmic_distance_measurement, "observational_cosmology/fundamental_physics").

domain_priors:requires_active_enforcement(cosmic_distance_measurement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cosmic_distance_measurement, early_universe_cosmology_framework).
narrative_ontology:constraint_beneficiary(cosmic_distance_measurement, standard_model_institutions).
narrative_ontology:constraint_victim(cosmic_distance_measurement, late_universe_expansion_measurements).
narrative_ontology:constraint_victim(cosmic_distance_measurement, alternative_cosmological_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE UNIVERSE MEASUREMENT COMMUNITY (SNARE) — Local distance ladders (Cepheids, Type Ia supernovae, gravitational lensing time delays) are trapped in a verification crisis. Teams cannot change foundational calibrations without rejecting 40+ years of institutional practice. Maximum extraction: the constraint forces them to defend higher H0 values that contradict early-universe measurements, while lacking resources or institutional support to resolve the contradiction. No exit option without career penalty.
constraint_indexing:constraint_classification(cosmic_distance_measurement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE COSMOLOGY RESEARCHERS (TANGLED ROPE) — Modified gravity, early dark energy, and non-flat universe models benefit from the Hubble tension as evidence against standard Lambda-CDM, yet remain constrained by the difficulty of upending 30 years of concordance cosmology. Some genuine coordination function (tension highlights measurement systematics), but asymmetric extraction: their models must clear higher evidentiary bars than the standard model.
constraint_indexing:constraint_classification(cosmic_distance_measurement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY UNIVERSE FRAMEWORK (ROPE) — The standard model benefits from Planck and WMAP data that are highly precise and operate at the inflation/recombination epoch. This institution experiences cosmic distance measurement as a coordination mechanism: the tension with late-universe measurements incentivizes investment in early-universe data (more Planck-type experiments), concentrating institutional resources and funding toward early-epoch observations. Net beneficiary — extraction runs toward this framework.
constraint_indexing:constraint_classification(cosmic_distance_measurement, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDEPENDENT MEASUREMENT INITIATIVE (SCAFFOLD) — James Webb Space Telescope and next-generation surveys (Vera Rubin, DESI, CMB-S4) are building independent distance calibrations bypassing local ladders. Organized coalitions see the tension as temporary — new distance anchors from parallax (Gaia), gravitational wave sirens (LIGO), and megamaser distances offer alternative pathways. Low effective extraction because these coalitions have agency and clear sunset: within 5-10 years, multiple independent H0 measurements will replace local ladder dominance.
constraint_indexing:constraint_classification(cosmic_distance_measurement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LOCAL DISTANCE LADDER INSTITUTION (PITON) — Cepheid variable calibrations, supernova standardization, and parallax refinement are foundational to observational cosmology yet have become increasingly performative. New data from JWST and Gaia reveal systematic uncertainties that were previously papered over by institutional consensus. The ladder persists through citation inheritance and career path dependence, not because it is optimal. Theater ratio high: much time spent defending methodological choices rather than improving measurements.
constraint_indexing:constraint_classification(cosmic_distance_measurement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, measuring cosmic distances is fundamentally constrained by the expansion of spacetime itself and the light-travel-time horizon. Any measurement of distance fundamentally relies on comparing standard candles across redshifts, which inevitably couples distance measurement to the underlying cosmological model. This perspective naturalizes what is actually a contingent institutional arrangement as inherent to cosmology. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(cosmic_distance_measurement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cosmic_distance_measurement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cosmic_distance_measurement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cosmic_distance_measurement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cosmic_distance_measurement, TR),
    TR >= 0.70.

:- end_tests(cosmic_distance_measurement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts institutional resources and career attention toward early-universe observations at the expense of late-universe measurement innovations. But the extraction is not severe (ε not > 0.46) because genuine coordination functions exist — measuring distances across the universe does require calibration standards, and the tension itself has motivated methodological improvements. The value increased from 0.22 to 0.35 over the interval as the tension became institutionally entrenched. Suppression (0.42): Moderate. Barriers include enormous technical difficulty of distance measurements (faintness of distant objects), massive resource requirements for new surveys, and citation pressure favoring Lambda-CDM-consistent results. But suppression is not total — alternative models are published, JWST is producing independent data, and organized coalitions are forming around new methods. Theater ratio (0.65): Moderately high. Significant time is spent defending Cepheid calibrations, supernova standardization, and local ladder methodology through detailed papers and conference presentations. The theater has increased over the interval as the calibration debates have become more technical and less likely to produce genuine resolution. New methods (gravitational sirens, JWST, Gaia) skip much of this performative debate and measure distance directly.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival variation across institutional power levels and exit options. All perspectives read genuine structural data, but they classify differently because the constraint's extraction mechanisms affect agents differently. The beneficiary sees coordination; the victim sees extraction; the coalition sees temporary problems with solutions; the institution sees degraded rituals; the observer risks naturalizing contingency. The perspectival gap between institutional early-universe actors (Rope) and powerless late-universe teams (Snare) is the largest — a factor of ~2-3 in experienced extractiveness. The Scaffold perspective (independent measurements) provides the key diagnostic: if new methods will truly converge on a consistent H0 value, the tension is a temporary coordination failure (Scaffold). If they reveal irreducible method-dependent offsets, the constraint degrades toward Piton.
 *
 * DIRECTIONALITY LOGIC:
 *   The early universe framework benefits from institutional concentration of resources and citations, so institutional actors in that position derive low directionality (beneficiary status + arbitrage exit). Late universe measurement teams bear career and resource costs defending measurements that contradict the dominant model, so they derive high directionality (victim status + trapped exit). Alternative cosmology researchers occupy an intermediate position — they benefit from the tension as evidence for their models, yet face institutional barriers to acceptance and publication. Organized independent measurement programs have constrained but real exit options through new technologies (JWST, Vera Rubin, LIGO), so they experience moderate extracted extractiveness. The local distance ladder institution has institutional arbitrage — it can maintain funding and citation through citation inheritance — but increasingly performative theater suggests the institution is degrading toward Piton.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing genuine measurement coordination from institutional gatekeeping. The early universe framework contributes real coordination value (Planck's precision is legitimate), but derives extraction benefits from institutional dominance. The late universe measurements contribute real coordination value (local distance ladders are genuine calibration tools), but suffer extraction costs from institutional marginalization. The tension is not resolvable by asserting 'measurement is coordination' — the structural asymmetry is real. The Scaffold perspective is diagnostic: if independent methods truly offer sunset pathways (5-10 year timescale), the constraint is institutionally enforced but not naturally immutable. If the independent methods also show systematic method-dependence, the constraint degrades toward Piton (performative measurement) rather than resolving toward pure Rope. The mandatrophy is resolved by showing that institutional factors (resource concentration, career path dependence, citation patterns) genuinely shape which measurements get made and how they are interpreted — not as a cynical dismissal of measurement science, but as a recognition that the same physical phenomenon (cosmic expansion) can be measured through different institutional paths that produce different numerical results.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systematic_calibration_bias,
    'Is the 5 sigma Hubble tension driven by undiscovered systematic errors in local distance ladder calibrations, or does it indicate genuine physics beyond Lambda-CDM?',
    'Independent calibration of Cepheid distances using gravitational lensing time delays, geometric distance anchors from GAIA parallax, and megamaser distances in independent galaxies; cross-validation against JWST distance measurements',
    'If systematic error: tension resolves, early/late measurements converge, constraint converts to Rope (coordination). If genuine physics: tension persists, constraint remains Tangled Rope or Snare (extraction against standard model).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systematic_calibration_bias, empirical, 'Whether Hubble tension reflects systematic calibration errors or genuine physical discrepancy').

omega_variable(
    early_universe_model_flexibility,
    'How much can the early universe physics be modified without losing predictive power and concordance with other precision cosmological data (BBN, CMB acoustic peaks)?',
    'Systematic exploration of modified early dark energy models, early recombination scenarios, and non-standard inflation models; Bayesian model comparison against Planck/BAO/BBN data',
    'If early universe physics is rigid: Lambda-CDM is nearly inevitable, constraint is institutional gatekeeping (Snare). If flexible: multiple viable models exist, constraint reflects genuine disambiguation ambiguity (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(early_universe_model_flexibility, empirical, 'Flexibility of early universe physics modifications given other constraints').

omega_variable(
    measurement_method_convergence,
    'Will independent distance measurement methods (gravitational sirens, JWST, megamaser chains, Gaia parallax) converge on a single H0 value, or will they reveal irreducible methods-dependent systematic offsets?',
    '5-10 year integration of LIGO gravitational wave sirens, JWST supernova observations, Vera Rubin optical transients, and DESI BAO measurements; identification of correlated vs independent systematic errors across methods',
    'If convergence: tension is coordinate-system artifact (Rope/Scaffold resolve). If methods-dependent offsets persist: measurement is index-dependent (Piton degradation intensifies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_method_convergence, empirical, 'Whether independent distance methods will converge or show systematic method-dependence').

omega_variable(
    institutional_lock_in_mechanism,
    'To what degree does the Hubble tension persist due to institutional investment in Lambda-CDM and career path dependence among cosmologists trained in the standard framework, rather than genuine physical ambiguity?',
    'Citation network analysis of citations to Lambda-CDM papers vs alternative models; career trajectory mapping for cosmologists proposing modifications to standard model; funding allocation patterns across theoretical frameworks',
    'If institutional lock-in is primary driver: constraint is Snare maintained by institutional extraction. If tension is genuine: institutional factors are secondary to physical ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_lock_in_mechanism, conceptual, 'Degree to which institutional factors versus genuine physical ambiguity drive the tension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cosmic_distance_measurement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cosm_tr_t0, cosmic_distance_measurement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cosm_tr_t5, cosmic_distance_measurement, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cosm_tr_t10, cosmic_distance_measurement, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(cosm_be_t0, cosmic_distance_measurement, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cosm_be_t5, cosmic_distance_measurement, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(cosm_be_t10, cosmic_distance_measurement, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cosmic_distance_measurement, information_standard).
narrative_ontology:affects_constraint(cosmic_distance_measurement, cosmic_microwave_background_standardization).
narrative_ontology:affects_constraint(cosmic_distance_measurement, supernova_standardization_framework).

% DUAL FORMULATION NOTE:
% The Hubble tension can be decomposed into two structurally distinct constraints: (1) early universe precision measurements (Planck epoch) which classify as Mountain/Rope depending on institutional perspective, and (2) late universe distance ladder calibrations which classify as Tangled Rope/Snare depending on measurement community perspective. This story addresses the tension between them as a single constraint exhibiting perspectival variation. Upstream constraints (CMB standardization, supernova standardization) influence this constraint through their own institutional arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cosmic_distance_measurement, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
