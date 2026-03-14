% ============================================================================
% CONSTRAINT STORY: fast_radio_burst_catalog_completeness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fast_radio_burst_catalog_completeness, []).

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
 *   constraint_id: fast_radio_burst_catalog_completeness
 *   human_readable: Fast Radio Burst Catalog Completeness and Detection Bias
 *   domain: astronomy/transient_detection/observational_bias
 *
 * SUMMARY:
 *   The fast radio burst (FRB) catalog completeness constraint arises from
 *   the systematic bias introduced by finite instrumental sensitivity. As
 *   radio telescopes discover FRBs, detection efficiency varies with source
 *   flux density, distance, and sky position. High-amplitude, nearby sources
 *   are reliably detected and cataloged; faint, distant sources are
 *   systematically missed. This creates a catalog biased toward the bright
 *   end of the luminosity function, while the true FRB population
 *   distribution remains hidden. The constraint exhibits all six DR types
 *   from different structural positions: the source population itself
 *   (snare), the field's statistical inference capability (snare),
 *   low-resource observatories (tangled rope), large survey teams (rope),
 *   legacy catalog standards (piton), and population correction frameworks
 *   (scaffold). The constraint's theater ratio (0.61) reflects that
 *   significant effort is devoted to catalog maintenance, sensitivity
 *   reporting, and ad-hoc bias corrections — performative work that increases
 *   in complexity as the catalog grows but does not address the fundamental
 *   observation selection problem.
 *
 * KEY AGENTS:
 *   - High-Sensitivity Survey Teams: Primary beneficiary (institutional/arbitrage) — large international facilities operate at global scope and set de facto catalog standards; their sensitivity thresholds define what counts as 'confirmed FRB'
 *   - Field Statistical Integrity: Primary victim (powerless/trapped) — abstract collective property that cannot organize or escape bias; true source population properties become unrecoverable
 *   - Low-Resource Observatories: Secondary victim (moderate/constrained) — participate in catalog but systematically disadvantaged by bias statistics; face resource constraints limiting sensitivity improvements
 *   - Source Population: Passive victim (powerless/trapped) — unobservable true distribution; erased from the catalog by detection selection
 *   - Population Inference Researchers: Moderate victim (moderate/constrained) — must develop post-hoc statistical corrections to work around incompleteness; constrained by information loss
 *   - Legacy Catalog Maintainers: Institutional performer (institutional/arbitrage) — maintain standards and formats through institutional inertia; see their own catalog as increasingly degraded (piton perspective)
 *   - Bayesian Correction Framework Teams: Organized problem-solvers (organized/constrained) — develop interim solutions with sunset logic; constrained by underlying incompleteness but building pathways toward next-generation surveys
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fast_radio_burst_catalog_completeness, 0.38).
domain_priors:suppression_score(fast_radio_burst_catalog_completeness, 0.48).
domain_priors:theater_ratio(fast_radio_burst_catalog_completeness, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fast_radio_burst_catalog_completeness, extractiveness, 0.38).
narrative_ontology:constraint_metric(fast_radio_burst_catalog_completeness, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fast_radio_burst_catalog_completeness, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fast_radio_burst_catalog_completeness, tangled_rope).
narrative_ontology:human_readable(fast_radio_burst_catalog_completeness, "Fast Radio Burst Catalog Completeness and Detection Bias").
narrative_ontology:topic_domain(fast_radio_burst_catalog_completeness, "astronomy/transient_detection/observational_bias").

domain_priors:requires_active_enforcement(fast_radio_burst_catalog_completeness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fast_radio_burst_catalog_completeness, high_sensitivity_survey_teams).
narrative_ontology:constraint_beneficiary(fast_radio_burst_catalog_completeness, institutional_survey_designers).
narrative_ontology:constraint_victim(fast_radio_burst_catalog_completeness, field_statistical_integrity).
narrative_ontology:constraint_victim(fast_radio_burst_catalog_completeness, low_resource_observatories).
narrative_ontology:constraint_victim(fast_radio_burst_catalog_completeness, source_population_inference).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRB SOURCE POPULATION (SNARE) — The actual population of fast radio bursts cannot exit or organize. Detection bias systematically selects for high-amplitude, nearby sources while erasing faint, distant populations from the catalog. The true distribution becomes unknowable. Maximum extraction — no agency, no representation in the observational record.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD STATISTICAL INFERENCE (SNARE) — The field-wide effort to infer FRB population properties, luminosity functions, and source evolution is trapped by incomplete catalogs biased toward high-amplitude events. Systematic selection effects are known but mathematically intractable to correct without independent ground truth. Cannot exit the bias; must work within catalogs contaminated by detection selection.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LOW-RESOURCE OBSERVATORIES (TANGLED ROPE) — Regional and national-scale facilities have genuine coordination benefit: they participate in the global FRB detection network, contribute to population statistics, and gain collaborative access to large surveys. However, they bear asymmetric costs: systematic biases skew catalog statistics in favor of large surveys operating at global scope with advanced sensitivity. Constrained by technical limitations, funding constraints, and observing time allocation. Some extraction, some benefit.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-SENSITIVITY SURVEY TEAMS (ROPE) — Large international surveys (ASKAP, VLA, Parkes) benefit from catalog completeness within their own detection sensitivity range. The constraint is coordination: standardized catalog formats, data-sharing protocols, and sensitivity reporting enable collaborative science. These teams have arbitrage options — they can pivot to other observing targets or coordinate with different partner surveys. Net beneficiary position but sees the constraint as solving a coordination problem.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CATALOG STANDARDS (PITON) — The FRB catalog (FRBSTATS, online compilations) maintains standardized formats and reporting practices, but these standards were developed in the era of ~1000 sources and are becoming inadequate for >10,000 events. The performative work of maintaining catalog standards (definitions of 'confirmed FRB', sensitivity limits, completeness corrections) persists through institutional inertia despite everyone recognizing the standards are degraded. Theater ratio reflects that much work is formatting and validation rather than advancing discovery.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: POPULATION INFERENCE FRAMEWORKS (SCAFFOLD) — Organized efforts to develop Bayesian hierarchical models and detection-bias correction algorithms provide temporary scaffolding for working within incomplete catalogs. These frameworks have a sunset clause: as new wide-field surveys (e.g., DSA-2000, SKA pathfinders) dramatically improve real completeness at intermediate sensitivity, the need for post-hoc statistical corrections diminishes. Extraction is low because the framework explicitly acknowledges bias and provides exit routes through better observations.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, detection bias is an immutable property of observational astronomy: finite sensitivity always selects for luminous, nearby sources. This perspective naturalizes the constraint as inherent to the observing process. However, the structural data reveals this as a false summit: bias is contingent on instrumental design, survey strategy, and institutional resource allocation — not a law of nature but an engineered consequence of observational choices.
constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fast_radio_burst_catalog_completeness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fast_radio_burst_catalog_completeness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fast_radio_burst_catalog_completeness, TR),
    TR >= 0.70.

:- end_tests(fast_radio_burst_catalog_completeness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The catalog bias creates real asymmetries in who benefits from the detection network — large surveys capture priority and recognition, while the field's ability to infer true population properties is compromised. However, the extraction is not maximal because the bias is not hidden; it is acknowledged and partially corrected. The field has developed interim workarounds (Bayesian hierarchical models) that provide some agency within the constraint. Over the interval, extractiveness increased from 0.18 to 0.38 as the catalog grew and bias effects became more pronounced (selection effects scale with catalog size). Suppression (0.48): Moderate. Barriers include finite instrument sensitivity (structural), resource constraints on building more sensitive telescopes (economic/institutional), and the intractability of correcting for unknown unknowns (epistemic). Not maximal because there are organized research programs (DSA-2000, SKA) that will reduce bias. Theater ratio (0.61): Moderate-high. The increase from 0.35 to 0.61 over the interval reflects rising performative work: as the catalog expanded from ~1000 to ~6000 sources, effort devoted to standardized formatting, sensitivity documentation, and post-hoc statistical correction increased relative to fundamental discovery. Much of this work is necessary (documentation) but also increasingly theatrical (corrections that cannot fully solve the problem).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival gap. Large survey teams see coordination (rope) — the catalog solves the real problem of sharing discoveries. Low-resource teams see mixed coordination and extraction (tangled rope) — they benefit from collaboration but are disadvantaged by bias statistics. The source population and field inference both see pure extraction (snare) — they bear costs with no compensation. The constraint's theater is visible to legacy catalog maintainers (piton) — they see their own standards becoming degraded. Population correction frameworks see a temporary problem with a sunset (scaffold) — next-generation surveys will provide alternative pathways by improving real completeness rather than relying on statistical correction. The analytical observer risks naturalizing the bias as immutable (mountain), but the structural data reveals this as false — the bias is an engineered consequence of observational design, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Large survey teams (institutional/arbitrage) derive d ≈ 0.10 — they are net beneficiaries with low structural extraction because they have exit options (pivot to other targets, coordinate with different surveys). Low-resource observatories (moderate/constrained) derive d ≈ 0.55 — they bear moderate extraction (participate in catalog but disadvantaged by bias statistics) while having some exit options (focus on high-amplitude sources within their sensitivity range). The source population itself (powerless/trapped) derives d ≈ 1.0 — maximum extraction because it has no agency and is systematically selected against. The field's statistical inference capability (powerless/trapped) derives d ≈ 0.95 — near-maximum because information loss from bias is largely irreversible with current methods. The analytical observer's natural law perspective (analytical/analytical) derives d ≈ 0.72 via canonical fallback — this is the false summit perspective that naturalizes contingent design choices.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that apparent 'coordination' from the large-survey perspective masks genuine extraction from the field-inference and population perspectives. The large survey teams correctly perceive rope (coordination mechanism for data sharing) AND are the beneficiaries of the extraction mechanism that disadvantages smaller players. The catalog is both a coordination tool and an extraction mechanism simultaneously — the mandatrophy dissolves when you recognize these are different structural properties viewed from different positions. The tangled rope classification captures this duality: genuine coordination function (data standards, collaborative discovery) paired with asymmetric extraction (bias favoring large surveys, information loss for population inference). The field-level statistical integrity and population inference capability cannot exit — they are 'victims' in the sense that they bear structural costs, but there is no alternative to working within the biased catalog in the current observational regime. The scaffold perspective provides a true resolution pathway: next-generation surveys with dramatically improved real completeness will make the post-hoc correction crutch obsolete, providing genuine sunset logic. The piton perspective reveals institutional inertia in catalog standards that were adequate for 1000 sources but are increasingly performative theater for 10,000+ sources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_correction_sufficiency,
    'Can Bayesian hierarchical models and detection-bias corrections adequately recover true source population properties from biased catalogs, or is the information loss irreversible?',
    'Simulation-based validation: generate synthetic FRB catalogs with known source distribution, apply realistic detection selection, run correction algorithms, and compare recovered vs true population parameters',
    'If corrections are sufficient: the constraint is primarily organizational (Rope/Tangled Rope). If information loss is irreversible: the constraint is extraction (Snare) because the true population becomes unknowable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bias_correction_sufficiency, empirical, 'Whether statistical corrections can recover true population from biased catalog').

omega_variable(
    survey_coordination_asymmetry,
    'Are small-scale observatories genuinely benefiting from participation in global catalogs, or are they systematically disadvantaged by bias statistics that favor large surveys?',
    'Quantify catalog bias as function of survey sensitivity; measure contribution of small vs large observatories to different flux/redshift bins; analyze feedback effects on funding allocation to low-resource facilities',
    'If asymmetry is strong: tangled_rope classification stands. If asymmetry is weak: reclassify as rope with more equitable coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survey_coordination_asymmetry, empirical, 'Whether small observatories benefit or are disadvantaged in global coordination').

omega_variable(
    completeness_plateau,
    'Will next-generation surveys (DSA-2000, SKA) achieve sufficient real completeness to break the catalog-correction dependency, or will new biases emerge at higher sensitivity?',
    'Track observing proposals and completed surveys for DSA-2000 and SKA pathfinders; monitor emergence of new systematic effects (e.g., scattering bias, redshift-dependent detectability); measure actual flux limits vs claimed limits',
    'If true completeness is achieved: scaffold perspective confirmed, sunset is real. If new biases emerge: constraint deepens, reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(completeness_plateau, empirical, 'Whether next-generation surveys will resolve completeness bias').

omega_variable(
    institutional_incentive_alignment,
    'Do survey team institutional incentives (publication count, discovery priority, funding) align with or diverge from field-wide interest in unbiased catalogs?',
    'Analysis of publication patterns: do teams cite catalog completeness issues when their own survey contributions are dominant? Do funding agencies reward bias-correction work equally to discovery papers?',
    'If misaligned: extraction mechanism is embedded in career incentives (snare from field perspective). If aligned: extraction is incidental to genuine coordination challenges (tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_alignment, empirical, 'Alignment of institutional incentives with unbiased catalog goals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fast_radio_burst_catalog_completeness, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frb_tr_t0, fast_radio_burst_catalog_completeness, theater_ratio, 0, 0.35).
narrative_ontology:measurement(frb_tr_t3, fast_radio_burst_catalog_completeness, theater_ratio, 3, 0.48).
narrative_ontology:measurement(frb_tr_t6, fast_radio_burst_catalog_completeness, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(frb_be_t0, fast_radio_burst_catalog_completeness, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(frb_be_t3, fast_radio_burst_catalog_completeness, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(frb_be_t6, fast_radio_burst_catalog_completeness, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fast_radio_burst_catalog_completeness, information_standard).
narrative_ontology:affects_constraint(fast_radio_burst_catalog_completeness, pulsar_dispersion_measure_bias).
narrative_ontology:affects_constraint(fast_radio_burst_catalog_completeness, neutron_star_merger_rate_inference).
narrative_ontology:affects_constraint(fast_radio_burst_catalog_completeness, fast_radio_burst_source_evolution).

% DUAL FORMULATION NOTE:
% The FRB catalog completeness constraint is upstream of source-population inference constraints. Three downstream constraints depend on catalog completeness: dispersion measure bias (affects distance estimates), merger rate inference (affected by incompleteness in nearby redshifts), and source evolution models (require unbiased luminosity function). Each downstream constraint has its own ε value reflecting specific observables and correction mechanisms. Decomposition recognizes that 'FRB science' encompasses multiple structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fast_radio_burst_catalog_completeness, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
