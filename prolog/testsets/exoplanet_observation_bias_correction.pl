% ============================================================================
% CONSTRAINT STORY: exoplanet_observation_bias_correction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exoplanet_observation_bias_correction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: exoplanet_observation_bias_correction
 *   human_readable: Exoplanet Observation Bias Correction and Detection Threshold Control
 *   domain: astronomy/observational_bias/exoplanet_detection
 *
 * SUMMARY:
 *   Exoplanet detection combines systematic instrumental effects with
 *   statistical inference under heterogeneous observational conditions. Major
 *   surveys (Kepler, TESS, ground-based radial velocity programs) employ
 *   different bias correction protocols — some detector-specific, some
 *   model-based, some empirical. This creates a structural tension:
 *   coordinating across surveys requires standardized bias correction, but
 *   standardization advantages well-resourced institutions that can absorb
 *   methodological switching costs. Low-resource groups face asymmetric
 *   pressure to adopt correction standards they did not design. The
 *   constraint exhibits tangled rope properties: genuine coordination
 *   function (surveys must be compatible) paired with asymmetric extraction
 *   (power concentrates in standard-setting institutions). Theater ratio has
 *   increased over 15 years as computational complexity of corrections has
 *   grown, creating a performative layer where uncertainty budgets multiply
 *   without proportionate gains in actual systematic control.
 *
 * KEY AGENTS:
 *   - Major Survey Institutions (Kepler, TESS, ESA missions): Institutional beneficiaries (institutional/arbitrage) — control detection threshold definitions and bias correction standard adoption; first-mover advantage in catalog releases
 *   - Low-Resource Research Groups: Primary victims (powerless/trapped) — lack computational infrastructure and training datasets for modern corrections; face exclusion pressure from international catalogs
 *   - Field Statistical Reliability: Abstract victim (powerless/trapped) — bears cost of accumulated methodological drift; cannot organize or exit
 *   - Mid-Tier National Survey Programs: Moderate victims (moderate/constrained) — gain access to correction methodologies but constrained by need to maintain standard compatibility
 *   - Open Exoplanet Initiative Advocates: Organized agents (organized/mobile) — building open-source correction pipelines and decentralized validation; see sunset pathway
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing institutional gatekeeping as inherent measurement physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_observation_bias_correction, 0.38).
domain_priors:suppression_score(exoplanet_observation_bias_correction, 0.48).
domain_priors:theater_ratio(exoplanet_observation_bias_correction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_observation_bias_correction, extractiveness, 0.38).
narrative_ontology:constraint_metric(exoplanet_observation_bias_correction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(exoplanet_observation_bias_correction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_observation_bias_correction, tangled_rope).
narrative_ontology:human_readable(exoplanet_observation_bias_correction, "Exoplanet Observation Bias Correction and Detection Threshold Control").
narrative_ontology:topic_domain(exoplanet_observation_bias_correction, "astronomy/observational_bias/exoplanet_detection").

domain_priors:requires_active_enforcement(exoplanet_observation_bias_correction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_observation_bias_correction, major_survey_institutions).
narrative_ontology:constraint_beneficiary(exoplanet_observation_bias_correction, well_resourced_research_groups).
narrative_ontology:constraint_victim(exoplanet_observation_bias_correction, field_statistical_reliability).
narrative_ontology:constraint_victim(exoplanet_observation_bias_correction, low_resource_research_groups).
narrative_ontology:constraint_victim(exoplanet_observation_bias_correction, small_telescope_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD STATISTICAL INTEGRITY (SNARE) — The collective epistemic reliability of exoplanet catalogs cannot exit the bias correction regime. As detection techniques proliferate with heterogeneous bias profiles, no single correction standard captures the full space of observational systematics. Smaller surveys face asymmetric pressure to conform to correction standards designed by major institutions, or risk exclusion from meta-analyses. The field bears the cost of accumulated methodological drift.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-RESOURCE RESEARCH GROUPS (SNARE) — Face structural barriers to participating in bias correction standards: limited computational infrastructure for Bayesian hierarchical models, no access to large training datasets for machine learning corrections, insufficient staff for detailed systematic uncertainty characterization. Correction protocols developed by well-resourced institutions become de facto entrance requirements. Groups cannot exit without abandoning exoplanet research entirely.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER NATIONAL SURVEY (TANGLED ROPE) — Constrained by the need to maintain catalog compatibility with international standards, but also benefits from access to correction methodologies and collaborative validation networks. Genuine coordination function exists (shared calibration, cross-survey validation), but asymmetric extraction occurs: major institutions control correction standard definition, determining which detection thresholds mid-tier programs must adopt.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR SURVEY INSTITUTIONS (ROPE) — Institutional beneficiaries with full arbitrage capacity. Control detection thresholds, define bias correction standards, and benefit from first-mover advantage in catalog releases. Experience the constraint as coordination: establishing systematic correction protocols enables follow-on science and international collaboration. Net benefit flows toward these institutions through citation priority and mission justification.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN EXOPLANET INITIATIVE (SCAFFOLD) — Organized coalition (arXiv preprints of corrected catalogs, open-source bias correction software, transparent parameter documentation) building alternative verification pathways independent of institutional gatekeeping. Low effective extraction because the coalition has agency and sees a sunset: open-source machine learning libraries, standardized Bayesian correction pipelines, and decentralized validation networks are making proprietary correction standards less valuable. Estimated sunset: 8-15 years as tooling matures.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY PHOTOMETRIC PROTOCOLS (PITON) — Earlier-generation bias correction methods (fixed polynomial systematics, magnitude-dependent calibration) persist through institutional inertia despite known limitations. Newer techniques (Gaussian process systematics, hierarchical Bayesian models) handle complex detector behavior better, but legacy protocols remain in use because computational barriers and cultural inertia slow adoption. The ritual of 'bias correction' is maintained performatively through published uncertainty budgets that don't reflect actual systematic residuals.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, observation bias in exoplanet detection is an immutable consequence of instrumental physics: photon counting has intrinsic Poisson noise, detector response functions are device-specific, and atmospheric turbulence introduces correlated systematics. These are structural limits to measurement precision. However, the base properties contradict the mountain classification — the suppression (0.48) and theater (0.58) suggest that much of the 'bias' is institutionally contingent, not physically immutable. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanet_observation_bias_correction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exoplanet_observation_bias_correction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(exoplanet_observation_bias_correction, TR),
    TR >= 0.70.

:- end_tests(exoplanet_observation_bias_correction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The original research group (major survey institutions) captures career and funding benefits through priority access to detection data and authority over standard definitions. The extraction is less severe than a pure snare because the correction protocols do serve a genuine coordination function — surveys genuinely must be compatible. Theater ratio (0.58): Moderate-high. Bias correction in exoplanet detection has grown increasingly performative: published uncertainty budgets reflect institutional methodology rather than validated systematic residuals. As detector complexity (CCDs, fiber positioning errors, atmospheric effects) has escalated faster than correction sophistication, the theater of presenting numerical error budgets has decoupled from actual systematic control. Suppression (0.48): Moderate. Significant barriers exist to independent verification: access to training datasets, computational infrastructure for Bayesian methods, and publication bias against negative results (non-detections). However, suppression is not total — some validation occurs through cross-survey overlap regions and blind re-observations. Open-source tools are reducing some barriers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how indexical classification reveals institutional power asymmetries hidden by technical language. Major institutions experience rope (coordination), low-resource groups experience snare (extraction), and the field's statistical integrity experiences snare with no voice. The organized coalition sees a scaffold with a sunset — open-source tools are building exits. The analytical observer risks seeing a mountain (measurement physics), but the theater and suppression metrics reveal institutional contingency. The perspectival gap is: who defines what counts as 'bias' and what counts as 'correction'? Institutions with large training datasets and computational resources define the standard. Institutions without those resources are told their measurements are 'biased' until they adopt the standard. This is not a matter of physics — it is a matter of institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality flows from the agent's structural position relative to extraction and coordination. Major survey institutions (institutional/arbitrage) benefit from standard-setting authority — they derive low d values from their beneficiary status and arbitrage exit capacity, experiencing negative effective extraction (net benefit). Low-resource groups (powerless/trapped) face high d values from victim status and lack of exit options, experiencing maximum extraction. Mid-tier programs (moderate/constrained) occupy intermediate positions — they benefit from access to correction methodologies but constrained by standard compatibility requirements. The open science coalition (organized/mobile) derives moderate d because they have clear exit pathways through open-source alternatives. The false summit at the analytical level suggests that 'immutable measurement physics' framing masks contingent institutional arrangements (control over standard definition, resource concentration, training dataset access).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the six types emerge from different structural positions in a genuine hybrid system. The major institution's rope experience is real (they do coordinate). The low-resource group's snare experience is real (they have no exit). The scaffold is a real structural feature (open-source alternatives exist and are maturing). The piton is a real observation (legacy protocols persist through inertia). The false summit at the analytical level diagnoses the risk: naturalizing institutional gatekeeping as inevitable measurement physics. The tangled rope classification at the moderate/constrained level captures the hybrid: coordination benefit paired with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_correction_standard_convergence,
    'Will heterogeneous bias correction protocols converge on a single dominant standard, or will the field fragment into incompatible correction regimes?',
    'Longitudinal tracking of correction method adoption across surveys; analysis of cross-survey catalog compatibility metrics over 10-year intervals',
    'If convergence: extraction mechanism strengthens (single standard concentrates power). If fragmentation: extraction weakens but field reliability degrades. Current trajectory unclear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bias_correction_standard_convergence, empirical, 'Convergence vs. fragmentation of bias correction standards').

omega_variable(
    computational_barrier_persistence,
    'Are resource barriers to implementing modern bias correction (Bayesian hierarchical models, neural network systematics) temporary infrastructure gaps or structural inequalities?',
    'Cost analysis of correction software deployment; tracking of adoption curves as computational tools become commoditized; comparison of correction quality across institution sizes',
    'If temporary: suppression decreases as tools democratize. If structural: suppression persists and asymmetric extraction remains stable across decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_barrier_persistence, empirical, 'Persistence of computational barriers to bias correction').

omega_variable(
    statistical_verification_sufficiency,
    'Do published uncertainty budgets in exoplanet catalogs actually capture residual systematic errors, or do they underestimate true systematic variance?',
    'Blind validation: re-observe subsets of published exoplanet detections with independent instruments and compare statistical properties; systematic residual analysis across catalog versions',
    'If underestimated: false summit (mountain) is confirmed — systematic bias is uncontrolled and inherent. If captured: bias correction works as designed and extraction mechanism is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_verification_sufficiency, empirical, 'Whether published uncertainty budgets capture true systematic error').

omega_variable(
    open_source_correction_effectiveness,
    'Can open-source bias correction pipelines (developed outside major survey institutions) achieve comparable accuracy to proprietary institutional methods?',
    'Benchmark validation: apply open-source and institutional correction methods to identical datasets; compare false positive rates, sensitivity, and catalog contamination',
    'If comparable: scaffold sunset is real — institutional gatekeeping loses force. If inferior: open-source solutions remain aspirational and suppression persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_correction_effectiveness, empirical, 'Effectiveness of open-source bias correction pipelines').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_observation_bias_correction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exoplanet_tr_t0, exoplanet_observation_bias_correction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exoplanet_tr_t5, exoplanet_observation_bias_correction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(exoplanet_tr_t10, exoplanet_observation_bias_correction, theater_ratio, 10, 0.58).
narrative_ontology:measurement(exoplanet_tr_t15, exoplanet_observation_bias_correction, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(exoplanet_be_t0, exoplanet_observation_bias_correction, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(exoplanet_be_t5, exoplanet_observation_bias_correction, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(exoplanet_be_t10, exoplanet_observation_bias_correction, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(exoplanet_be_t15, exoplanet_observation_bias_correction, base_extractiveness, 15, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_observation_bias_correction, information_standard).
narrative_ontology:boltzmann_floor_override(exoplanet_observation_bias_correction, 0.12).
narrative_ontology:affects_constraint(exoplanet_observation_bias_correction, detection_threshold_multiplicity).
narrative_ontology:affects_constraint(exoplanet_observation_bias_correction, instrument_calibration_coupling).
narrative_ontology:affects_constraint(exoplanet_observation_bias_correction, exoplanet_population_statistical_bias).

% DUAL FORMULATION NOTE:
% Exoplanet observation bias correction is downstream of specific instrumental systematics but represents a distinct structural constraint at the meta-analysis level. Upstream constraints have their own extractiveness values reflecting specific bias sources; this constraint has extractiveness reflecting institutional gatekeeping over bias correction standards.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exoplanet_observation_bias_correction, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
