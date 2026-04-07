% ============================================================================
% CONSTRAINT STORY: exoplanet_mass_metallicity_relation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exoplanet_mass_metallicity_relation, []).

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
 *   constraint_id: exoplanet_mass_metallicity_relation
 *   human_readable: Exoplanet Mass-Metallicity Relation: Observed Correlation vs. Causal Mechanism
 *   domain: exoplanetology/observational_astronomy
 *
 * SUMMARY:
 *   The exoplanet mass-metallicity relation is an observed statistical trend:
 *   planets orbiting metal-rich stars tend to be more massive than planets
 *   orbiting metal-poor stars. This correlation has become foundational to
 *   exoplanet demographics, population synthesis models, and survey target
 *   selection. However, the constraint is fundamentally ambiguous: it may
 *   represent a genuine causal relationship (metallicity affects planetary
 *   formation efficiency), an observational artifact (selection bias in
 *   detection and characterization), or a mixture of both. The tension
 *   between treating the correlation as an empirical regularity that demands
 *   mechanistic explanation and treating it as sufficient for prediction
 *   without mechanism creates a structural extraction mechanism. Survey
 *   collaborations benefit from the relation's predictive utility and use it
 *   to justify their work; theory groups are constrained to explain it;
 *   causal mechanism research is marginalized; and null results are
 *   systematically suppressed. The constraint exhibits all six DR types
 *   because the ambiguity is not resolvable from the existing data — it
 *   requires alternative observational pathways (direct imaging,
 *   high-resolution spectroscopy of forming systems) that are only now
 *   becoming feasible.
 *
 * KEY AGENTS:
 *   - Survey Collaborations (Kepler, TESS, RV): Primary beneficiary (institutional/arbitrage) — use mass-metallicity relation to justify survey design and funding; have exit option to move to new targets
 *   - Causal Mechanism Research: Primary victim (powerless/trapped) — marginalized within literature and funding landscape; no exit without abandoning research agenda
 *   - Theory Groups: Secondary victim (moderate/constrained) — pressured to fit data rather than derive predictions; also benefit from testing models against the relation
 *   - Direct Imaging Community: Organized agents (organized/mobile) — building alternative verification pathways with explicit sunset as technology matures
 *   - Classical Planet Formation Framework: Institutional actor (institutional/arbitrage) — maintains performative explanations that accommodate any data
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing observational selection artifact as law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_mass_metallicity_relation, 0.38).
domain_priors:suppression_score(exoplanet_mass_metallicity_relation, 0.48).
domain_priors:theater_ratio(exoplanet_mass_metallicity_relation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_mass_metallicity_relation, extractiveness, 0.38).
narrative_ontology:constraint_metric(exoplanet_mass_metallicity_relation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(exoplanet_mass_metallicity_relation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_mass_metallicity_relation, tangled_rope).
narrative_ontology:human_readable(exoplanet_mass_metallicity_relation, "Exoplanet Mass-Metallicity Relation: Observed Correlation vs. Causal Mechanism").
narrative_ontology:topic_domain(exoplanet_mass_metallicity_relation, "exoplanetology/observational_astronomy").

domain_priors:requires_active_enforcement(exoplanet_mass_metallicity_relation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_mass_metallicity_relation, early_detection_surveys).
narrative_ontology:constraint_beneficiary(exoplanet_mass_metallicity_relation, statistical_prediction_frameworks).
narrative_ontology:constraint_victim(exoplanet_mass_metallicity_relation, causal_understanding).
narrative_ontology:constraint_victim(exoplanet_mass_metallicity_relation, mechanism_discovery_research).
narrative_ontology:constraint_victim(exoplanet_mass_metallicity_relation, null_result_reporting).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAUSAL MECHANISM RESEARCH (SNARE) — The scientific goal of understanding WHY the mass-metallicity relation exists is structurally trapped. Survey data and statistical correlations dominate the literature and funding landscape. Researchers seeking mechanistic understanding face resource starvation, publication resistance, and marginalization. No exit option exists without abandoning the research agenda entirely. The correlation has become a substitute for understanding, and the field extracts this substitution without accountability.
constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THEORY GROUPS (TANGLED ROPE) — Constrained by pressure to fit existing survey data rather than derive first-principles predictions. Face career cost of theoretical work that doesn't reproduce the observed trend. Also benefit from the correlation's existence — it provides a testbed for planet formation models. Mixed: genuine coordination function (theory can test hypotheses against data) alongside asymmetric extraction (theories are tested for compliance with the empirical trend, not the reverse).
constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SURVEY COLLABORATIONS (ROPE) — Kepler, TESS, radial velocity surveys experience the mass-metallicity relation as a high-value coordination mechanism: it provides a testable prediction that drives survey design and funding justification. The collaboration benefits from the relation's prominence without bearing the cost of mechanistic uncertainty. Arbitrage option: can publish statistical correlations with limited interpretation and move to new survey targets.
constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIRECT IMAGING COMMUNITY (SCAFFOLD) — James Webb Space Telescope and future direct-imaging missions (Habitable Worlds Observatory) provide alternative verification pathways that bypass the mass-metallicity relation. Direct measurement of exoplanet atmospheres, composition, and formation histories will enable causal hypothesis testing without relying on the parent-star metallicity proxy. This pathway has an explicit sunset: as direct imaging capability matures, reliance on the proxy relation declines. Constraint has sunset clause because the mechanism-discovery pathway (high-resolution spectroscopy of forming systems) is structurally viable, just expensive.
constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL PLANET FORMATION FRAMEWORK (PITON) — Core accretion theory is substantially performative when applied to the mass-metallicity relation. The framework can accommodate the observed correlation through post-hoc parameter tuning (envelope opacity, disk metal abundance, migration rates) but generates no independent predictions that uniquely distinguish it from alternatives. The theory persists through institutional inertia (it's standard in all textbooks) despite producing theater rather than insight. High theater ratio because explanations are flexible enough to fit any observed data without constraining mechanism.
constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN?) — From civilizational/universal perspective, the mass-metallicity relation appears to be a natural law: metallicity affects planetary formation efficiency, and this must produce some correlation with planetary mass. However, structural data contradicts mountain classification. The observed relation's strength depends critically on survey bias (selection limits, detection thresholds, metallicity measurement uncertainty), suggesting the constraint is contingent rather than necessary. The analytical perspective risks naturalizing what is actually an observational artifact.
constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanet_mass_metallicity_relation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exoplanet_mass_metallicity_relation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(exoplanet_mass_metallicity_relation, TR),
    TR >= 0.70.

:- end_tests(exoplanet_mass_metallicity_relation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The relation extracts from causal understanding in multiple ways — it substitutes correlation for mechanism, it drives research toward prediction rather than explanation, and it systematizes the exclusion of alternative mechanistic pathways. But the extraction is not complete: the relation is empirically useful for prediction, and genuine coordination functions exist (survey design, population synthesis testing). Suppression (0.48): Moderate. Barriers to mechanism discovery include resource concentration on surveys, publication bias against null results, theoretical framework inflexibility, and the career risk of questioning a 'well-established' result. But suppression is not total — dissenting work exists (pebble accretion models, revisionist analyses), and direct imaging offers a genuine escape route. Theater ratio (0.65): Moderately high. Classical core accretion theory explains the relation through flexible parameters that can be tuned to fit any data. The relation itself functions as theater — it is displayed as an achievement of understanding while actually serving as a substitute for understanding. Theater has increased over the interval as the relation has become more embedded in standard frameworks and as null results have become harder to publish.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (powerless mechanism research) sees the relation as extracting understanding without providing it. The rope perspective (survey collaborations) sees it as enabling coordination. The tangled rope perspective (theory groups) sees mixed benefit and constraint. The scaffold perspective (direct imaging) sees a temporary bottleneck with a realistic exit path. The piton perspective (classical theory) sees a performative ritual maintained through inertia. The mountain perspective (analytical observer) risks naturalizing a contingent observational constraint. These perspectives do not converge — they reveal genuine structural ambiguity about whether the relation is causal or artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Survey collaborations are beneficiaries because the relation provides testable predictions that drive funding and publication without requiring mechanistic validation. Their directionality (d) is low because they have arbitrage options — they can publish the correlation and move to new phenomena. Theory groups are both beneficiary (the relation provides a testbed) and victim (they are constrained to explain rather than refute it). Their directionality is moderate because they are constrained but not trapped. Mechanism research is a victim because it is marginalized by the prevalence of the relation as explanation-substitute. Their directionality is high because they have no exit without abandoning their core research goal. Direct imaging has high mobility because it offers a genuinely alternative pathway, so suppression (perceived from this perspective) is moderate rather than severe.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY STRUCTURE: The exoplanet mass-metallicity relation should be decomposed into at least two distinct constraints with different extractiveness values. (1) The empirical correlation itself (ε ≈ 0.08–0.15, Mountain or Rope): a genuine observational finding, standardly measured, reproducible. (2) The causal mechanism puzzle (ε ≈ 0.42, Tangled Rope or Snare): the structural tension between using the correlation as explanation-substitute and pursuing genuine mechanistic understanding. The empirical correlation is not itself extractive — it is useful and valid. The extraction arises from treating the correlation as sufficient explanation and marginalizing mechanism research. The mandatrophy is resolved by recognizing these as distinct constraints with different ε values, different beneficiaries/victims, and different classification architectures. The current story conflates them, which is appropriate for corpus balance (we need exemplars of high-ambiguity constraints), but in operational use, the constraints should be decomposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correlation_causation_ambiguity,
    'Does the observed mass-metallicity relation reflect a causal dependence of planetary mass on host-star metallicity, or is it primarily driven by observational selection bias and detection threshold effects?',
    'Forward modeling of survey detection limits; statistical deconvolution of selection effects; comparison across different metallicity measurement techniques and wavelengths',
    'If causal (>70% signal): constraint is Tangled Rope (genuine mechanism coordinating with extraction). If observational artifact (<30% signal): constraint is primarily Snare (the correlation extracts from causal understanding without providing it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correlation_causation_ambiguity, empirical, 'Whether observed correlation is causal or observational artifact').

omega_variable(
    mechanism_pathway_plurality,
    'If the relation is causal, are there multiple competing pathways (core accretion efficiency, pebble accretion, migration, photo-evaporation) that each produce similar correlations, making the observed trend compatible with incompatible mechanisms?',
    'Theoretical population synthesis across mechanism variants; identification of diagnostic signatures unique to each pathway; direct observation of planet-forming disks at high metallicity',
    'If high pathway plurality: the observed relation cannot distinguish mechanisms, and theory remains trapped by the correlation (high extraction). If low plurality: the relation has diagnostic power and unlocks mechanism discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanism_pathway_plurality, empirical, 'Whether multiple incompatible mechanisms produce same correlation').

omega_variable(
    direct_imaging_timeline_credibility,
    'Will direct imaging and high-resolution spectroscopy of forming systems (JWST, ELTs, future missions) actually enable causal mechanism discovery at timescales matching the scaffold sunset projection (15-25 years)?',
    'Assessment of JWST commissioning results; timeline analysis of ELT construction and science capability; technical feasibility of spectrographic resolution needed for formation mechanism signatures',
    'If credible (>80% probability): scaffold perspective is structural and sun set is real. If not credible (<50%): the ''alternative pathway'' is aspirational and the snare extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_imaging_timeline_credibility, empirical, 'Whether direct imaging provides realistic alternative to mass-metallicity relation').

omega_variable(
    metallicity_measurement_standardization,
    'Do different metallicity measurement techniques (spectroscopic vs photometric vs astrometric proxies) produce consistent rankings of host-star metallicity, or are systematic differences sufficient to explain apparent correlation strength?',
    'Cross-correlation of same stars measured by different techniques; uncertainty quantification across methods; reanalysis of mass-metallicity relation using different metallicity calibrations',
    'If inconsistent (>30% disagreement): theater is high and the measured relation is partly instrument artifact. If consistent (<10% disagreement): the correlation has genuine empirical content.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metallicity_measurement_standardization, empirical, 'Whether metallicity measurements are standardized across techniques').

omega_variable(
    null_result_publication_bias,
    'What is the ratio of completed-but-unpublished null results (exoplanet masses uncorrelated with metallicity, or contrary to prediction) to positive-result publications?',
    'Survey of exoplanet research groups for unpublished null findings; meta-analysis of publication bias in exoplanet mass-metallicity studies; comparison with published effect sizes in systematic reviews',
    'If high publication bias (10:1 positive:null ratio): suppression is severely underestimated and the extraction mechanism relies on institutional silencing. If low bias (<3:1 ratio): the field is adequately self-correcting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(null_result_publication_bias, empirical, 'Publication bias in mass-metallicity relation studies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_mass_metallicity_relation, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exo_mmr_tr_t0, exoplanet_mass_metallicity_relation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(exo_mmr_tr_t4, exoplanet_mass_metallicity_relation, theater_ratio, 4, 0.58).
narrative_ontology:measurement(exo_mmr_tr_t8, exoplanet_mass_metallicity_relation, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(exo_mmr_be_t0, exoplanet_mass_metallicity_relation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(exo_mmr_be_t4, exoplanet_mass_metallicity_relation, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(exo_mmr_be_t8, exoplanet_mass_metallicity_relation, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_mass_metallicity_relation, resource_allocation).
narrative_ontology:affects_constraint(exoplanet_mass_metallicity_relation, planet_formation_timescale_degeneracy).
narrative_ontology:affects_constraint(exoplanet_mass_metallicity_relation, disk_metallicity_abundance_coupling).

% DUAL FORMULATION NOTE:
% The mass-metallicity relation is downstream of disk metallicity effects but represents a distinct observational constraint. The upstream constraints govern disk metal abundance and planet formation efficiency; this constraint governs how those upstream properties correlate with observable planetary mass. Decomposition into empirical correlation (Mountain) and mechanistic understanding (Tangled Rope) is recommended for operational use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exoplanet_mass_metallicity_relation, analytical, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
