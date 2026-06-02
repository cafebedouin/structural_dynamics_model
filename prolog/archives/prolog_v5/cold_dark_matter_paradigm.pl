% ============================================================================
% CONSTRAINT STORY: cold_dark_matter_paradigm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cold_dark_matter_paradigm, []).

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
 *   constraint_id: cold_dark_matter_paradigm
 *   human_readable: The Lambda-CDM model's 'cold dark matter' tenet as a dominant scientific paradigm
 *   domain: cosmology/physics/paradigm_structure
 *
 * SUMMARY:
 *   The Lambda-CDM (ΛCDM) model has dominated theoretical cosmology since
 *   ~1998 (accelerating expansion discovery) through ~2024. It provides a
 *   unified framework explaining cosmic microwave background (CMB)
 *   anisotropies, large-scale structure, supernovae distances, and baryon
 *   acoustic oscillations with minimal parameters (six: Ω_b, Ω_cdm, Ω_Λ, H0,
 *   A_s, n_s). Yet mounting observational tensions (Hubble tension: H0
 *   discrepancy, S8 tension: matter clustering strength, small-scale
 *   anomalies: missing satellites, core-cusp problem, too-big-to-fail) have
 *   accumulated without resolution. Simultaneously, the framework's core
 *   mechanism — cold dark matter and inflationary initial conditions —
 *   remains undetected and, many argue, unfalsifiable. This constraint
 *   exhibits the structural properties of a tangled rope: ΛCDM provides
 *   genuine coordination (CMB + large-scale structure analysis under unified
 *   model) while simultaneously suppressing alternative frameworks through
 *   funding concentration, publication bias, and career risk for dissenters.
 *   The paradigm is not a natural law (mountain) but an institutional
 *   arrangement with growing tensions. The theater ratio has increased as
 *   inflation parameters have become post-hoc fit variables rather than
 *   predictive tools. Small-scale structure anomalies accumulate as
 *   constraints on dark matter properties rather than motivating alternative
 *   models.
 *
 * KEY AGENTS:
 *   - Lambda-CDM Research Establishment: Primary beneficiary (institutional/arbitrage) — controls major survey resources, observatories, graduate training. Benefits from paradigm dominance through funding allocation, publication prestige, institutional legitimacy.
 *   - Alternative Framework Researchers: Primary victim (powerless/trapped) — bear career costs (funding scarcity, publication barriers, marginalization) for proposing modified gravity or non-standard dark matter. Cannot exit paradigm without sacrificing prospects.
 *   - Observational Anomaly Detectors: Secondary victim (moderate/constrained) — observe tensions and anomalies; benefit from ΛCDM framework for data interpretation but constrained from pursuing alternative explanations. Must work within paradigm.
 *   - Alternative Physics Coalition: Organized agents (organized/constrained) — building parallel infrastructure (MOND communities, axion collaborations, modified gravity networks). Creating exit paths but not yet competitive.
 *   - Inflationary Cosmology Community: Institutional piton (institutional/arbitrage) — inflation framework has atrophied into post-hoc parametrization. Persists through textbook canonicity and foundational myth status rather than predictive power.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing ΛCDM as universal law rather than contingent paradigm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cold_dark_matter_paradigm, 0.52).
domain_priors:suppression_score(cold_dark_matter_paradigm, 0.58).
domain_priors:theater_ratio(cold_dark_matter_paradigm, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, extractiveness, 0.52).
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cold_dark_matter_paradigm, tangled_rope).
narrative_ontology:human_readable(cold_dark_matter_paradigm, "The Lambda-CDM model's 'cold dark matter' tenet as a dominant scientific paradigm").
narrative_ontology:topic_domain(cold_dark_matter_paradigm, "cosmology/physics/paradigm_structure").

domain_priors:requires_active_enforcement(cold_dark_matter_paradigm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cold_dark_matter_paradigm, lambda_cdm_research_establishment).
narrative_ontology:constraint_victim(cold_dark_matter_paradigm, alternative_cosmology_frameworks).
narrative_ontology:constraint_victim(cold_dark_matter_paradigm, cosmological_anomaly_detection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE FRAMEWORK RESEARCHER (SNARE) — Researchers proposing modified gravity (MOND, TeVeS), non-standard dark matter (self-interacting, fuzzy, axion-specific), or alternative cosmologies face career barriers. Funding concentrated in ΛCDM-aligned research. Publication bias against non-standard models. Cannot exit the paradigm without sacrificing career prospects. Full extraction with no meaningful exit.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVATIONAL ANOMALY DETECTOR (TANGLED ROPE) — Observers of small-scale structure anomalies (missing satellites problem, core-cusp problem, too-big-to-fail problem, Hubble tension, S8 tension) benefit from ΛCDM framework for data interpretation and coordination with large surveys. Simultaneously, paradigm commitment suppresses alternative explanations — constraints on data analysis paths, model degrees of freedom. Mixed coordination and extraction.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ΛCDM RESEARCH ESTABLISHMENT (ROPE) — Major benefit from paradigm dominance. Access to observatories, survey resources, funding allocation, graduate student recruitment. ΛCDM framework solves the coordination problem of interpreting disparate cosmological observations (CMB, large-scale structure, supernovae, baryon acoustic oscillations) into a unified model. Net institutional beneficiary with high arbitrage options (can migrate resources to new paradigm if ΛCDM fails).
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE PHYSICS COALITION (SCAFFOLD) — Organized groups (MOND conferences, axion collaborations, modified gravity workshops, arXiv preprint communities) are building parallel infrastructure for alternative frameworks. Not yet competitive with ΛCDM in explaining observations, but creating exit paths. Sunset mechanism: if tensions (S8, H0, Hubble) persist and ΛCDM requires > 5 additional free parameters, alternative frameworks become viable. Estimated sunset: 15-25 years if observational tensions remain unresolved.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EARLY BIG BANG INFLATION FRAMEWORK (PITON) — Inflationary cosmology (scalar field + potential + reheating mechanism) is now substantially performative within ΛCDM. Originally a genuine theoretical solution (initial conditions problem, horizon problem, flatness problem). Now theater: inflation parameters are fit post-hoc to observations. Framework persists through institutional inertia (textbooks, canonical status, foundational grant programs) despite low predictive power and inability to falsify. Theater ratio ≥ 0.70 — primarily serves to justify ΛCDM phenomenology retrospectively.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the dominance of ΛCDM is viewed as discovery of fundamental truths: the universe is spatially flat (k=0), dominated by dark energy (Ω_Λ ≈ 0.68), cold dark matter provides gravitational scaffolding (Ω_CDM ≈ 0.27), and the inflationary paradigm is a natural law of primordial cosmology. This perspective naturalizes institutional arrangements as universal necessities. However, the structural data (suppression of anomalies, publication bias, non-falsifiable parametrization) contradicts the mountain classification — the engine will identify this as a false summit.
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_dark_matter_paradigm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cold_dark_matter_paradigm, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cold_dark_matter_paradigm, TR),
    TR >= 0.70.

:- end_tests(cold_dark_matter_paradigm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. ΛCDM provides genuine coordination benefit (unified CMB + structure framework) but extraction is real: alternative models are starved of resources, careers are constrained for dissenters, publication pathways are narrowed, anomalies are treated as internal ΛCDM problems rather than paradigm challenges. The extraction is not total (alternatives can still be published, discussed, funded at lower rates) but substantial. Over the interval, extractiveness has increased as anomalies have accumulated without paradigm revision — the coordination benefit (unity of framework) is offset by growing suppression of anomalies and alternatives. Suppression (0.58): Significant. Mechanisms include: (1) funding bias toward ΛCDM proposals at NSF/DOE; (2) editorial bias in high-impact journals (Nature, Astrophysical Journal) against modified gravity and alternative dark matter papers; (3) postdoctoral hiring favoring ΛCDM-trained researchers; (4) anomalies reframed as internal problems (baryonic physics, environmental effects) rather than paradigm challenges; (5) career risk for prominent alternative-framework advocates (loss of grants, fewer citations, marginalization). Theater ratio (0.64): Moderate-high and increasing. Inflation has become substantially performative — scalar field potential is chosen post-hoc to match observations rather than derived from fundamental principles. Dark matter is a placeholder for unknown physics rather than a predictive mechanism. CMB fit is achieved through parameter tuning (6 base parameters + Ad hoc additions like running spectral index, isocurvature perturbations, early dark energy). As anomalies accumulate, more degrees of freedom are added, increasing theater. The constraint's theater has risen from 0.35 (1998-2008, when ΛCDM explained major observations) to 0.64 (2015-2024, when anomalies require continuous parameter adjustment).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single set of structural properties (extractiveness 0.52, suppression 0.58) generates divergent classifications depending on the observer's structural position. The establishment sees rope (coordination + low extraction). The marginalized researchers see snare (pure extraction, no exit). The detectors see tangled rope (mixed). The organized alternative community sees scaffold (temporary constraint, sunset mechanism visible). The piton perspective shows how institutional structures become theatrical. The analytical perspective risks false naturalization. The perspectival gap is not due to disagreement on facts but on structural position — each agent accurately reports their own experience, and the disagreement reveals that the constraint is not a natural law but an institutional arrangement with multiple conflicting interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationships. ΛCDM establishment benefits from the constraint (low d, around 0.10-0.20 from beneficiary status + arbitrage exit) — they experience negative effective extraction, i.e., subsidy. Alternative researchers bear costs (high d, around 0.85-0.95 from victim status + trapped exit) — they experience maximum extractiveness. Anomaly detectors are intermediate (moderate d around 0.55-0.65 from mixed victim/beneficiary role + constrained exit). The organized coalition has lower d (around 0.40-0.50 from constrained exit + some exit paths via alternative infrastructure). Inflation researchers have low d as institutional beneficiaries despite piton status. The analytical observer has moderate-high d (around 0.70 from analytical position) reflecting the epistemic cost of paradigm naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that ΛCDM is NOT a mountain (false summit diagnosis). If ΛCDM were a natural law, all perspectives would classify it as mountain — it would be immutable across all observation points. Instead, the constraint produces rope (beneficiary), snare (victim), tangled rope (mixed), scaffold (organized alternative), piton (degraded component), and false-summit mountain (analytical naturalization). The multiplicity of classifications reveals that ΛCDM's dominance is institutional, not natural. The paradigm could be replaced if alternative frameworks achieved comparable explanatory power (which would shift the scaffold sunset mechanism to actual transition) or if institutional mechanisms were reformed (funding reallocation, editorial bias reduction). The mandatrophy is resolved by noting that ΛCDM exhibits tangled-rope structure at the analytical level — genuine coordination function (CMB + structure unification) combined with asymmetric extraction (suppression of alternatives) and active enforcement (funding mechanisms, publication barriers, hiring practices). This is precisely the signature of a contingent institutional arrangement, not a discovered law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    small_scale_structure_resolution,
    'Can ΛCDM + baryonic physics + environmental effects resolve the core-cusp, missing satellites, too-big-to-fail, and similar anomalies without ad-hoc parameter tuning?',
    'High-resolution cosmological simulations (FIRE, EAGLE, IllustrisTNG evolution) comparing predictions to dwarf galaxy surveys and Milky Way satellite observations. Quantification of degrees of freedom added vs. anomalies resolved.',
    'If resolved within existing ΛCDM: anomalies reclassified as solved problems, suppression decreases, paradigm strengthens. If requiring > 3 additional free parameters per anomaly: ΛCDM extraction mechanism becomes visible, suppression increases, alternative frameworks gain credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_scale_structure_resolution, empirical, 'Whether small-scale structure anomalies resolve within ΛCDM without parameter proliferation').

omega_variable(
    hubble_tension_fundamental_or_systematic,
    'Does the Hubble tension (H0 ≈ 67 km/s/Mpc from CMB vs. ≈ 73 km/s/Mpc from local measurements) reflect a fundamental failure of ΛCDM or systematic error in measurements?',
    'Independent H0 measurements from James Webb Space Telescope, gravitational lensing time delays, gravitational wave standard sirens, and megamaser distance measurements. Cross-validation of systematic error sources in Cepheid calibration and supernova photometry.',
    'If systematic: tension resolves, ΛCDM strengthened, alternative frameworks lose primary motivating anomaly. If fundamental: ΛCDM requires early dark energy or other exotic components, parameter space expands, alternative frameworks become competitive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hubble_tension_fundamental_or_systematic, empirical, 'Whether Hubble tension is fundamental physics or systematic measurement error').

omega_variable(
    dark_matter_direct_detection_timeline,
    'Will direct detection experiments (XENON, LUX, SuperCDMS, COSINE) find a positive signal for WIMP-like dark matter within 10 years, or should null results be interpreted as evidence against cold dark matter?',
    'Continuation of null result trends in sensitivity curves. Calculation of detection probability for standard WIMP candidates given current constraints. Evaluation of whether parameter space is already ruled out or merely ''hidden.''',
    'If positive signal: validates ΛCDM dark matter narrative, suppression mechanism weakens, paradigm solidifies. If null results persist: ΛCDM dark matter becomes unfalsifiable component, extraction mechanism strengthens, alternative dark matter candidates (axions, sterile neutrinos, primordial black holes) gain visibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_matter_direct_detection_timeline, empirical, 'Whether WIMPs or other cold dark matter candidates will be directly detected').

omega_variable(
    inflation_falsifiability_criterion,
    'What observational signature would constitute falsification of inflationary cosmology? Can the framework be constrained to produce specific tensor-to-scalar ratio (r) predictions, or does parameter freedom allow post-hoc fit to any data?',
    'Analysis of inflation model classes (slow-roll, hybrid, chaotic, etc.) for unique observational predictions. Evaluation of CMB, gravitational wave, and primordial gravitational wave data against theoretical predictions. Count of free parameters and their sensitivity to observational variation.',
    'If falsifiable predictions exist: inflation becomes testable, ΛCDM component gains or loses credibility based on data. If framework is unfalsifiable: inflation reclassified as metaphysical assumption, piton classification confirmed, theater ratio increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_falsifiability_criterion, conceptual, 'Whether inflationary cosmology has falsifiable predictions or is post-hoc parametrization').

omega_variable(
    paradigm_lock_institutional_mechanisms,
    'To what degree does funding concentration (NSF/DOE cosmology programs favor ΛCDM proposals), journal editorial bias, and grant review panel composition institutionally enforce ΛCDM dominance vs. reflecting genuine superior explanatory power?',
    'Quantitative analysis of funding allocation (proposal success rates by framework), editorial decision rates by journal for ΛCDM vs. alternative submissions, panel composition bias (demographics of cosmological theory reviewers). Comparison with other scientific fields undergoing paradigm shifts (quantum computing, climate modeling) for institutional change timescales.',
    'If institutional mechanisms are primary driver: suppression is contingent and could be reduced through policy change, tangled rope classification strengthened, sunset mechanism becomes visible. If mechanisms reflect genuine explanatory gaps: suppression is justified, paradigm dominance is earned, extraction interpretation weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_lock_institutional_mechanisms, empirical, 'Degree to which institutional mechanisms vs. explanatory power drives ΛCDM dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cold_dark_matter_paradigm, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cdm_tr_t0, cold_dark_matter_paradigm, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cdm_tr_t10, cold_dark_matter_paradigm, theater_ratio, 10, 0.5).
narrative_ontology:measurement(cdm_tr_t20, cold_dark_matter_paradigm, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(cdm_be_t0, cold_dark_matter_paradigm, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cdm_be_t10, cold_dark_matter_paradigm, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cdm_be_t20, cold_dark_matter_paradigm, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cold_dark_matter_paradigm, information_standard).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, small_scale_structure_anomalies).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, dark_matter_detection_null_results).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, hubble_tension_measurement).
narrative_ontology:affects_constraint(cold_dark_matter_paradigm, inflation_parameter_space).

% DUAL FORMULATION NOTE:
% The cold dark matter paradigm can be decomposed into separable constraints: (1) CDM_empirical_status (ε~0.08): the observational evidence base supporting cold dark matter particles; (2) CDM_paradigm_lock (ε~0.52): the institutional mechanism suppressing alternatives; (3) inflation_falsifiability (ε~0.45): the degree to which inflationary initial conditions can be tested. These three stories are linked by network.affects_constraints — paradigm lock extraction depends on low empirical falsifiability, and parametrization flexibility increases when anomalies emerge. The present story addresses the paradigm_lock component (tangled rope). The empirical_status story would be lower extractiveness (~0.15-0.25) if solely observational; the parametrization story would be higher (~0.60+) if treating inflation as unfalsifiable metaphysics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cold_dark_matter_paradigm, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
