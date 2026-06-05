% ============================================================================
% CONSTRAINT STORY: single_cell_measurement_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_single_cell_measurement_integration, []).

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
 *   constraint_id: single_cell_measurement_integration
 *   human_readable: Single Cell Measurement Integration Bottleneck
 *   domain: systems_biology/measurement_integration
 *
 * SUMMARY:
 *   Single-cell measurement integration represents a structural bottleneck in
 *   systems biology: as measurement technologies (flow cytometry, RNA-seq,
 *   protein immunoassay, spatial imaging, metabolomics) have proliferated,
 *   integrating across modalities to answer biological questions has become
 *   technically essential but institutionally fragmented. The constraint
 *   exhibits the full spectrum of DR types depending on observer position.
 *   For experimental biologists, it appears as pure extraction (snare): they
 *   must bear integration costs while vendors capture platform lock-in value.
 *   For systems biology labs, it is mixed coordination and extraction
 *   (tangled_rope): multimodal integration enables genuine scientific
 *   insights alongside technical overhead. For vendors, it is coordination
 *   (rope): fragmentation makes their platform essential. For the
 *   open-standards coalition, it is a temporary problem with a sunset
 *   (scaffold): unified data formats and containerized workflows are building
 *   interoperable ecosystems. For journal peer review, it is a degraded
 *   ritual (piton): methods sections describe integration protocols without
 *   verifying reproducibility. For the analytical observer at civilizational
 *   scale, there is a false mountain: the claim that integration
 *   incompleteness is inherent to biological observation, when it is actually
 *   a contingent artifact of vendor fragmentation and institutional practice.
 *
 * KEY AGENTS:
 *   - Experimental Biologist: Primary victim (powerless/trapped) — must integrate incompatible modalities; bears full integration overhead while lacking control over data formats
 *   - Systems Biology Lab: Secondary victim (moderate/constrained) — benefits from integration insights but develops technical debt maintaining custom pipelines; faces high switching costs
 *   - Measurement Technology Vendor: Primary beneficiary (institutional/arbitrage) — captures value through platform lock-in; experiences fragmentation as coordination benefit (their modality becomes essential integration hub)
 *   - Open Standards Coalition: Organized agent (organized/mobile) — bioinformatics consortia, open-source projects building interoperable ecosystems with visible sunset path
 *   - Journal Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative methods-description requirement without functional verification; theater increased as integration complexity outpaced review capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional fragmentation as inherent incompleteness of biological observation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(single_cell_measurement_integration, 0.52).
domain_priors:suppression_score(single_cell_measurement_integration, 0.58).
domain_priors:theater_ratio(single_cell_measurement_integration, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(single_cell_measurement_integration, extractiveness, 0.52).
narrative_ontology:constraint_metric(single_cell_measurement_integration, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(single_cell_measurement_integration, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(single_cell_measurement_integration, tangled_rope).
narrative_ontology:human_readable(single_cell_measurement_integration, "Single Cell Measurement Integration Bottleneck").
narrative_ontology:topic_domain(single_cell_measurement_integration, "systems_biology/measurement_integration").

domain_priors:requires_active_enforcement(single_cell_measurement_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(single_cell_measurement_integration, measurement_technology_vendors).
narrative_ontology:constraint_beneficiary(single_cell_measurement_integration, data_aggregation_platforms).
narrative_ontology:constraint_victim(single_cell_measurement_integration, experimental_biologists).
narrative_ontology:constraint_victim(single_cell_measurement_integration, biological_inference_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTAL BIOLOGIST (SNARE) — Trapped in incompatible measurement ecosystems. Cannot exit: must integrate flow cytometry, transcriptomics, proteomics, metabolomics, and imaging data to answer biological questions, but integration standards are vendor-controlled and fragmented. High suppression: switching costs between platforms are prohibitive; published protocols lock in to specific measurement modalities; career credit goes to method developers, not integrators. Maximum extraction: the biologist bears the full cost of integration overhead while vendors capture data lock-in value.
constraint_indexing:constraint_classification(single_cell_measurement_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEMS BIOLOGY LAB (TANGLED ROPE) — Benefits from coordination: multi-modal single-cell data integration enables systems-level insights unavailable from any single modality. Also bears extraction: lab must develop custom integration pipelines, maintains technical debt across incompatible formats, loses time to data wrangling rather than biological discovery. Constrained exit: can theoretically switch platforms, but switching costs (retraining, method redevelopment, data reformation) are high; organizational inertia locks in current technology choices. Moderate experienced extraction with genuine coordination benefit.
constraint_indexing:constraint_classification(single_cell_measurement_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEASUREMENT VENDOR (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: fragmentation of measurement modalities means their platform (flow cytometry, qPCR, RNA-seq, spatial imaging, etc.) becomes essential infrastructure that other modalities must integrate with. Captures value through platform lock-in and data standardization around their format. Arbitrage exit: can switch focus to alternative measurement modalities if needed, or support competing standards. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(single_cell_measurement_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS COALITION (SCAFFOLD) — Organized actors (bioinformatics consortia, journal standards committees, open-source projects like Scanpy, Seurat, Bioconductor) see integration bottleneck as a temporary coordination failure with a sunset: unified data formats (H5AD, AnnData, BioFrame), containerized workflows (CWL, Nextflow), and community standardization efforts are building interoperable measurement ecosystems. Low effective extraction because coalition has agency and visible exit path — open standards reduce vendor lock-in. Mobile exit: can contribute to or fork standardization efforts. Sunset: 5-10 years for dominant open-source ecosystems to reach maturity and adoption parity with proprietary platforms.
constraint_indexing:constraint_classification(single_cell_measurement_integration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNAL PEER REVIEW GATEKEEPING (PITON) — The requirement that methods be fully described and reproducible creates performative compliance with integration standards. Papers describe data formats and integration steps in prose, tables, and supplementary documents, but reviewers cannot verify reproducibility of integration pipelines without running code. The ritual persists through institutional inertia: Methods sections describing integration protocols are substantive but not functionally verified. Theater ratio elevated by: mandatory method descriptions (performative), lack of code execution verification, format-agnostic review standards that don't actually test interoperability. Degraded from original coordination function (enabling reproducibility) into theatrical compliance.
constraint_indexing:constraint_classification(single_cell_measurement_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some measurement integration lag is inherent to biological complexity: different modalities measure fundamentally different cellular properties (transcription, translation, metabolic state, morphology, protein localization), and these cannot be fully integrated without loss of information or averaging artifacts. This perspective sees incompleteness of integration as a natural limit on biological observability. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that institutional fragmentation (vendor lock-in, publication practices, career incentives) naturalizes what is actually a contingent integration bottleneck.
constraint_indexing:constraint_classification(single_cell_measurement_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(single_cell_measurement_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(single_cell_measurement_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(single_cell_measurement_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(single_cell_measurement_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(single_cell_measurement_integration, TR),
    TR >= 0.70.

:- end_tests(single_cell_measurement_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Experimental biologists and systems labs bear significant integration overhead — time spent data wrangling, format conversion, pipeline maintenance — while vendors capture lock-in value through proprietary data formats and software ecosystems. The extraction is not maximal because genuine scientific benefits accrue from multimodal integration (coordination component); the extraction is the asymmetry in who bears costs vs who captures value. Suppression (0.58): Moderate-high. Switching costs between measurement platforms include: retraining on new instrumentation, developing new protocols optimized for each platform, reformatting historical data, revalidating results. Career incentives lock researchers into particular measurement modalities through published methods and established expertise. Publication bias rewards method development over integration work. Theater ratio (0.68): Elevated and increasing. The requirement that methods be fully described creates performative compliance with integration standards. Researchers document data formats, preprocessing steps, and integration parameters in prose and supplementary materials, but peer reviewers cannot execute integration code to verify reproducibility. Theater increased over interval because measurement complexity outpaced review capacity: journals still require full methods descriptions (the ritual) without verifying their actual executability (the function).
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as radically different types across perspectives. The powerless experimental biologist perceives maximum extraction (snare): trapped in fragmented ecosystems with no exit option. The moderate systems biology lab perceives mixed coordination and extraction (tangled_rope): genuine scientific benefit alongside technical overhead. The institutional vendor perceives pure coordination (rope): fragmentation makes their platform essential. The organized standards coalition perceives a temporary problem being solved (scaffold): open formats and interoperable tools are building an exit path. The journal system perceives a degraded but persistent ritual (piton): methods descriptions are performed without functional verification. The analytical observer at civilizational scale perceives an immutable natural law (mountain): measurement incompleteness is inherent to biological complexity. This perspectival gap reveals that the 'immutable natural law' view is a false summit — naturalization of what is actually a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position. Vendors with arbitrage exit (can develop alternative modalities) experience low or negative effective extraction — they benefit from fragmentation. Experimental biologists with trapped exit (must integrate multiple modalities to answer questions) experience high extraction — they bear all integration costs. Systems labs with constrained exit (switching is expensive but possible) experience moderate extraction — some agency but high barriers. Organized standards coalitions with mobile exit (can develop alternative standards) experience low extraction — they have visible exit paths. The piton classification derives from high theater (0.68) despite moderate extractiveness: the ritualistic verification gate persists through inertia while its functional verification role has degraded as complexity outpaced review capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that institutional fragmentation (vendor-driven format incompatibility, publication practices rewarding method development over integration, career incentives locking researchers into platforms) is being mistaken for inherent biological incompleteness. The false mountain claim is that single-cell measurement integration is fundamentally incomplete — that different modalities measure incommensurable properties. The structural data reveals this is actually a tangled_rope: genuine coordination benefits exist (multimodal integration enables systems-level insights), but active vendor enforcement of proprietary formats and institutional incentives lock researchers into asymmetric extraction relationships. The scaffold perspective's sunset logic (open standards and interoperable tools) is not aspirational — it represents a real structural shift from vendor-controlled integration to open-source/standards-based integration over a 5-10 year horizon. The natural law view (mountain) risks pre-adjudicating integration incompleteness as inevitable when it is actually contingent on institutional arrangements. Mandatrophy is resolved by distinguishing (1) genuine measurement complementarity (rope: different modalities measure different properties), (2) vendor-induced fragmentation (tangled_rope: barriers to integration that could be removed), and (3) inherent biological incompleteness (mountain: properties that cannot be integrated). These are three different constraints. This story addresses constraint (2).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modality_coupling_fidelity,
    'Are integration artifacts (cell-type misclassification, batch effects, missing-value imputation errors) inherent to multi-modal measurement or products of incompatible data formats and crude integration algorithms?',
    'Comparison of integration fidelity across standardized vs proprietary pipelines using simulated ground-truth single-cell data; analysis of whether gold-standard modality fusion (e.g., spatial transcriptomics validating RNA-seq clusters) requires format standardization or just conceptual alignment',
    'If artifacts inherent: mountain classification justified — incompleteness is natural limit. If artifacts from integration gaps: tangled_rope justified — standardization would reduce extraction overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modality_coupling_fidelity, empirical, 'Whether integration artifacts are inherent or format-induced').

omega_variable(
    vendor_standard_convergence,
    'Are measurement technology vendors genuinely committed to open format standardization (AnnData, BioFrame, Zarr) or are they using standards adoption as greenwashing while maintaining proprietary lock-in through software ecosystems?',
    'Analysis of vendor participation in standards bodies, release of proprietary tools on open platforms, removal of API barriers to competing formats, investment in interoperability features vs proprietary extensions',
    'If genuine convergence: scaffold sunset is real — open standards will mature. If greenwashing: sunset is aspirational, vendors will maintain lock-in through software layer; constraint persists as tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_standard_convergence, empirical, 'Whether vendor commitment to standards is genuine or performative').

omega_variable(
    integration_skill_distribution,
    'Is the integration bottleneck a skill/knowledge problem (biologists lack bioinformatics training) or a structural/technical problem (formats are genuinely incompatible)?',
    'Comparison of integration success rates across sites with high vs low bioinformatics expertise; analysis of whether trained bioinformaticians encounter same integration barriers as experimentalists; longitudinal tracking of whether standard platform training reduces integration overhead',
    'If skill problem: scaffold/rope interventions (training, documentation, communities of practice) sufficient. If technical problem: requires vendor/standards intervention — trained researchers still face integration costs. Distinguishes false summit (skill training solves mountain) from real constraint (technical incompatibility remains).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_skill_distribution, empirical, 'Whether bottleneck is skill-based or technical').

omega_variable(
    single_cell_atomicity,
    'Does a single cell''s state have an objective, modality-independent description, or is each measurement modality generating a partial, incommensurable view?',
    'Theoretical analysis of whether single-cell properties (cell type, cell cycle, metabolic state) are invariant across measurement methods or observer-dependent; empirical investigation of whether multimodal fusion produces coherent or contradictory inference',
    'If objective state exists: integration is an engineering problem (constraint is tangled_rope with sunset). If incommensurable: integration is fundamentally incomplete (constraint approaches mountain — incompleteness is natural limit). False summit detector will flag this as naturalization if it''s actually a contingent technical problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_cell_atomicity, conceptual, 'Whether single-cell state is modality-independent or incommensurable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(single_cell_measurement_integration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scmi_tr_t0, single_cell_measurement_integration, theater_ratio, 0, 0.52).
narrative_ontology:measurement(scmi_tr_t5, single_cell_measurement_integration, theater_ratio, 5, 0.62).
narrative_ontology:measurement(scmi_tr_t10, single_cell_measurement_integration, theater_ratio, 10, 0.68).
narrative_ontology:measurement(scmi_tr_t8, single_cell_measurement_integration, theater_ratio, 8, 0.66).

% Extraction over time
narrative_ontology:measurement(scmi_be_t0, single_cell_measurement_integration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scmi_be_t5, single_cell_measurement_integration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(scmi_be_t10, single_cell_measurement_integration, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(scmi_be_t8, single_cell_measurement_integration, base_extractiveness, 8, 0.49).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(single_cell_measurement_integration, information_standard).
narrative_ontology:affects_constraint(single_cell_measurement_integration, cell_type_classification_stability).
narrative_ontology:affects_constraint(single_cell_measurement_integration, batch_effect_confounding).
narrative_ontology:affects_constraint(single_cell_measurement_integration, biological_inference_reproducibility).

% DUAL FORMULATION NOTE:
% Single-cell measurement integration is downstream of specific measurement modality constraints (RNA-seq accuracy, flow cytometry resolution, imaging fidelity) and upstream of higher-level inference constraints (cell type classification, cell state dynamics). This story focuses on the integration layer: format incompatibility and platform lock-in that couples distinct measurement modalities. Separate stories address measurement accuracy and inference stability; network edges establish dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(single_cell_measurement_integration, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
