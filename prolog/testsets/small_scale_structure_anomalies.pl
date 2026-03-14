% ============================================================================
% CONSTRAINT STORY: small_scale_structure_anomalies
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_small_scale_structure_anomalies, []).

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
 *   constraint_id: small_scale_structure_anomalies
 *   human_readable: Small-Scale Structure Anomalies in Cosmological Data
 *   domain: observational_cosmology/dark_matter
 *
 * SUMMARY:
 *   Small-scale structure anomalies in cosmology represent decades of
 *   observational tension with Lambda-CDM predictions at scales below ~1 Mpc.
 *   Key anomalies include the Satellite Problem (too few observed satellite
 *   galaxies), Missing Satellites Problem (predicted substructure exceeds
 *   observations), Core-Cusp Problem (predicted density profiles are too
 *   cuspy), and planes-of-satellites arrangements inconsistent with isotropic
 *   simulations. These anomalies have accumulated since the 1980s, yet
 *   research into fundamental alternatives remains institutionally suppressed
 *   despite strong observational signatures. The constraint exhibits
 *   tangled_rope structure: institutional cosmology benefits from model
 *   stability and large-survey planning enabled by consensus, while
 *   alternative researchers bear severe career penalties. The theater ratio
 *   has risen from 0.38 to 0.64 over the interval, indicating increasing
 *   parameter-fitting theater relative to primary falsification attempts. The
 *   constraint prevents honest evaluation of whether the standard model's
 *   failures are fundamental (requiring new physics) or methodological
 *   (requiring better simulations and analysis).
 *
 * KEY AGENTS:
 *   - Alternative Model Researchers: Primary victim (powerless/trapped) — cannot secure funding or positions for anomaly-focused research; severe career penalties; no exit option without abandoning research identity
 *   - Early-Career Observationalists: Secondary victim (moderate/constrained) — face tenure and funding constraints; benefit from institutional support but cannot freely investigate anomalies; significant mobility barriers
 *   - Standard Cosmology Institutions: Primary beneficiary (institutional/arbitrage) — major surveys (Planck, WMAP, SDSS, DES, LSST planning), funding agencies, tenure committees; benefit from consensus stability; high exit flexibility
 *   - Independent Observation Networks: Organized challengers (organized/mobile) — arXiv preprints, open-data initiatives, citizen science, independent observatories building alternative analysis pathways
 *   - Dark Matter Model Consensus: Institutional constraint (institutional/arbitrage) — maintains Lambda-CDM through assumption-layering and ad-hoc parameters; exhibits piton degradation (performative fitting replaces primary verification)
 *   - Observational Integrity: Collective victim (powerless/trapped) — abstract epistemic commons; bears cost of deferred model testing; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(small_scale_structure_anomalies, 0.52).
domain_priors:suppression_score(small_scale_structure_anomalies, 0.48).
domain_priors:theater_ratio(small_scale_structure_anomalies, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(small_scale_structure_anomalies, extractiveness, 0.52).
narrative_ontology:constraint_metric(small_scale_structure_anomalies, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(small_scale_structure_anomalies, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(small_scale_structure_anomalies, tangled_rope).
narrative_ontology:human_readable(small_scale_structure_anomalies, "Small-Scale Structure Anomalies in Cosmological Data").
narrative_ontology:topic_domain(small_scale_structure_anomalies, "observational_cosmology/dark_matter").

domain_priors:requires_active_enforcement(small_scale_structure_anomalies).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(small_scale_structure_anomalies, dark_matter_model_defenders).
narrative_ontology:constraint_beneficiary(small_scale_structure_anomalies, standard_cosmology_institutions).
narrative_ontology:constraint_victim(small_scale_structure_anomalies, alternative_model_researchers).
narrative_ontology:constraint_victim(small_scale_structure_anomalies, observational_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE MODEL RESEARCHERS (SNARE) — Powerless researchers proposing modifications to Lambda-CDM cannot secure funding, journal space, or institutional positions. Career penalties for anomaly-focused work are severe and irreversible. Maximum suppression: alternative theories are systematically defunded despite observational anomalies.
constraint_indexing:constraint_classification(small_scale_structure_anomalies, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER OBSERVATIONALISTS (TANGLED ROPE) — Constrained by tenure track and funding dependencies. Benefit from institutional support and collaborative access to large surveys, but cannot freely investigate anomalies without career risk. Mixed extraction with genuine coordination components.
constraint_indexing:constraint_classification(small_scale_structure_anomalies, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STANDARD COSMOLOGY DEFENDERS (ROPE) — Institutional actors (major cosmology collaborations, funding agencies) benefit from the framework's stability. Experience the constraint as coordination: maintaining model consensus enables large survey planning and resource allocation. Net beneficiary position with exit flexibility.
constraint_indexing:constraint_classification(small_scale_structure_anomalies, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDEPENDENT OBSERVATION NETWORKS (SCAFFOLD) — Organized agents (independent surveys, citizen science collaborations, open-data initiatives) building alternative verification and analysis pathways. See the bottleneck as temporary, solvable through distributed analysis and transparent data sharing with clear sunset as network maturity increases.
constraint_indexing:constraint_classification(small_scale_structure_anomalies, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DARK MATTER MODEL CONSENSUS (PITON) — The institutional commitment to Lambda-CDM + dark matter persists despite decades of small-scale anomalies (Satellite Problem, Missing Satellites Problem, Core-Cusp Problem, Planes of Satellites). The framework is maintained through assumption-layering and parameter adjustment rather than primary functional verification. Theater ratio reflects performative parameter fitting rather than genuine model testing.
constraint_indexing:constraint_classification(small_scale_structure_anomalies, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, structure formation below ~1 Mpc is inherently difficult to model: N-body simulations fail at small scales, substructure is chaotic, and detailed prediction requires unreachable computational resolution. This naturalizes the anomalies as inherent limits to predictive power. Engine's false summit detection will identify this as naturalization of institutional conservatism rather than fundamental physical limit.
constraint_indexing:constraint_classification(small_scale_structure_anomalies, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(small_scale_structure_anomalies_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(small_scale_structure_anomalies, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(small_scale_structure_anomalies, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(small_scale_structure_anomalies, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(small_scale_structure_anomalies, TR),
    TR >= 0.70.

:- end_tests(small_scale_structure_anomalies_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The institutional cosmology community captures funding priority, journal space, and hiring advantage from consensus maintenance. This is extraction relative to alternative researchers, but not maximum extraction because the standard model did generate genuine, validated predictions (CMB, large-scale structure, accelerated expansion). The extraction is real but asymmetrically concentrated on researchers who question the model. Suppression (0.48): Moderate. Significant barriers to alternative research include funding bias, publication barriers, recruitment discrimination, and citation suppression. But suppression is not total — some alternative papers are published, and some research groups maintain minority positions. The barriers are institutional and reversible through policy change, not structural impossibilities. Theater ratio (0.64): Moderately high and rising. The standard model now relies heavily on parameter fitting (dark energy equation-of-state tuning, baryon acoustic oscillation parameterization, subgrid physics in simulations) rather than primary falsification. The measurement trajectory shows theater increasing from 0.38 to 0.64 as observational constraints have multiplied, forcing more parameters to be added. This indicates the constraint is shifting from genuine model testing (rope) toward performative parameter adjustment (piton degradation). The rise in theater_ratio is the diagnostic signal of Goodhart drift — the model's success at fitting data comes from fitting parameters, not from correct physics.
 *
 * PERSPECTIVAL GAP:
 *   The standard cosmology institutions (rope perspective) experience the constraint as successful coordination: their consensus enables large-scale survey planning (Dark Energy Survey, Vera Rubin Observatory, Euclid) and stable funding allocation. Their effective extraction (chi) is negative — they benefit from consensus. Alternative researchers (snare perspective) experience maximum suppression: career penalties are severe and institutional. The early-career observationalists (tangled rope) are in the middle — they benefit from institutional support and survey access, but cannot freely explore anomalies. The independent networks (scaffold) see a temporary institutional lock with a sunset path: distributed, transparent analysis can eventually enable model-free tests. The dark matter consensus itself (piton perspective) exhibits degradation — its verification function has atrophied and been replaced by parameter-fitting theater. The false summit (mountain perspective) naturalizes institutional conservatism as inherent scientific limits. The perspectival gap reveals that the constraint's type depends entirely on structural position: beneficiaries see stable coordination (rope), challengers see extraction (snare), and the system itself is degrading into theater (piton).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Standard cosmology institutions are beneficiaries with arbitrage exit options (d ≈ 0.05, f(d) ≈ -0.12), producing negative effective extraction — they benefit from the constraint. Alternative researchers are victims with trapped exit options (d ≈ 0.95, f(d) ≈ 1.42), producing maximum effective extraction — they bear severe costs. Early-career observationalists are victims with constrained exit options (d ≈ 0.75, f(d) ≈ 1.10), producing high extraction but not maximum. This directionality structure explains why the tangled rope classification emerges: the constraint has genuine coordination components (survey planning, resource allocation) that benefit the dominant institution, PLUS asymmetric extraction from researchers who question the model. Both mechanisms coexist — coordination for insiders, extraction for outsiders.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT TYPE AMBIGUITY: The constraint could be classified as Snare (if the institutional suppression of alternatives is the primary mechanism) or as Rope (if the genuine coordination benefits of consensus planning outweigh the extraction). The tangled rope classification resolves this by declaring both mechanisms explicitly: beneficiaries (standard model defenders) whose exit flexibility and coordination benefits give them low d; victims (alternative researchers) whose trapped status gives them high d; and active enforcement (funding allocation, publication gatekeeping, hiring discrimination). The classification reflects that institutional cosmology genuinely solves a coordination problem (enabling large surveys) while simultaneously extracting from researchers outside the consensus. The mandatrophy is resolved by the presence of both beneficiary and victim declarations — the constraint IS both coordination and extraction, operating simultaneously at different positions in the institutional hierarchy. The rising theater_ratio (0.38 → 0.64) indicates that the coordination function is degrading relative to extraction: as observational anomalies accumulate, the model is maintained through parameter-fitting theater rather than empirical confirmation, suggesting piton transition over longer timescales.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anomaly_reality_vs_systematics,
    'Are small-scale structure anomalies real physical signatures or artifacts of measurement and analysis methods?',
    'Multi-wavelength, multi-instrument confirmation of anomalies; independent verification by collaborations with different systematic error profiles; consistency checks across independent datasets (Gaia, HST, JWST, radio surveys)',
    'If real: anomalies falsify current dark matter models and extraction constraint is severe (snare for researchers, clear victim designation). If systematic: anomalies are observational noise and constraint collapses to coordination problem (rope). Current uncertainty: ~70% confidence anomalies are real, ~30% systematic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anomaly_reality_vs_systematics, empirical, 'Reality status of small-scale structure anomalies').

omega_variable(
    alternative_model_viability,
    'Do alternative theories (MOND, TeVeS, self-interacting dark matter, fuzzy dark matter) provide better fits to anomalies without introducing ad-hoc parameters?',
    'Systematic comparison of model flexibility: parameter count, prediction vs post-diction, performance on held-out test data from independent surveys',
    'If alternatives are viable: institutional resistance to testing them represents extraction constraint (tangled rope or snare). If alternatives are degenerative: Lambda-CDM dominance reflects genuine empirical superiority and constraint is rope (coordination, not extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_viability, empirical, 'Comparative viability of alternative dark matter models').

omega_variable(
    funding_bias_quantification,
    'How much of the anomaly non-investigation reflects institutional funding allocation bias vs genuine empirical constraints on research feasibility?',
    'Historical grant allocation data: proposal success rates by topic; comparison of funding ratios (Lambda-CDM to alternative models) to publication ratios; career outcome tracking for researchers by research direction',
    'If significant bias: extractive mechanism is confirmed (institutional actors enforcing consensus). If minimal bias: researchers self-select based on empirical assessments and constraint reverts to coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(funding_bias_quantification, empirical, 'Quantification of funding allocation bias toward standard model').

omega_variable(
    computational_sufficiency,
    'Are modern N-body simulations with sufficient resolution actually capable of producing small-scale structures that match observations?',
    'Resolution convergence studies: systematic increase in particle count and force softening to establish convergence behavior; comparison to observational targets with explicit error budgets',
    'If simulations converge to observations: current anomalies may be simulation artifacts, not physics. If simulations diverge: anomalies are real and model requires revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_sufficiency, empirical, 'Computational convergence of N-body simulations to observations').

omega_variable(
    model_degeneracy_resolution,
    'Can dark matter model parameters be constrained independently, or does the standard model require degenerate parameter families to fit observations?',
    'Independent constraints from different observational windows (CMB, large-scale structure, local dynamics, lensing); parameter correlation analysis; forecast studies using simulated data with known input parameters',
    'If independent constraints converge: model has genuine predictive power. If degeneracies persist: model achieves empirical fits through parameter-fitting theater, supporting piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_degeneracy_resolution, empirical, 'Parameter degeneracy and independent constrainability of dark matter models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(small_scale_structure_anomalies, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sssa_tr_t0, small_scale_structure_anomalies, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sssa_tr_t5, small_scale_structure_anomalies, theater_ratio, 5, 0.51).
narrative_ontology:measurement(sssa_tr_t10, small_scale_structure_anomalies, theater_ratio, 10, 0.64).
narrative_ontology:measurement(sssa_tr_t15, small_scale_structure_anomalies, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(sssa_be_t0, small_scale_structure_anomalies, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sssa_be_t5, small_scale_structure_anomalies, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(sssa_be_t10, small_scale_structure_anomalies, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sssa_be_t15, small_scale_structure_anomalies, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(small_scale_structure_anomalies, resource_allocation).
narrative_ontology:affects_constraint(small_scale_structure_anomalies, dark_matter_model_falsifiability).
narrative_ontology:affects_constraint(small_scale_structure_anomalies, observational_cosmology_funding_allocation).

% DUAL FORMULATION NOTE:
% Small-scale structure anomalies decompose into multiple constraint stories with different ε values. The observational reality of anomalies (ε ≈ 0.08, mountain) is upstream; the institutional suppression of model alternatives (ε ≈ 0.52, tangled rope) is downstream. The two stories are linked: institutional suppression exists because the anomalies are real, and because institutional actors benefit from maintaining consensus despite them. The observational story is empirically high-confidence; the extraction story is social-structural and institutional-level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(small_scale_structure_anomalies, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
