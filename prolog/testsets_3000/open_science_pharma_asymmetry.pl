% ============================================================================
% CONSTRAINT STORY: open_science_pharma_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_science_pharma_asymmetry, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: open_science_pharma_asymmetry
 *   human_readable: Open Science Pharma Asymmetry in Spatial Proteomics
 *   domain: computational_biology/drug_discovery/open_science
 *
 * SUMMARY:
 *   The open science pharma asymmetry in spatial proteomics represents a
 *   structural tension between the open science mandate for publicly funded
 *   research and the proprietary incentives of drug development. Public
 *   databases like the Human Protein Atlas, TCGA, and GTEx provide
 *   comprehensive protein expression, localization, and interaction data that
 *   pharmaceutical companies use for target identification and validation.
 *   However, the data flow is asymmetric: industry uses public data
 *   extensively (measurable through patent citations, SEC filings, and
 *   published drug discovery pipelines) but contributes minimal reciprocal
 *   data — perturbation screens, clinical trial results, and negative
 *   findings remain proprietary. This creates a coordination-extraction
 *   hybrid: the databases genuinely enable global research (coordination
 *   function), but the value capture is asymmetric (extraction function). The
 *   constraint has intensified over the 2010-2020 interval as spatial
 *   proteomics technologies (mass spectrometry imaging, multiplexed
 *   immunofluorescence, spatial transcriptomics) have matured and become
 *   central to drug discovery, increasing both the coordination benefit and
 *   the extraction magnitude.
 *
 * KEY AGENTS:
 *   - Public Research Funding Bodies: Primary victim (powerless/trapped) — NIH, Wellcome Trust, EU Horizon fund database creation that enables private drug development without reciprocal data return; cannot exit without abandoning open science mission
 *   - Pharmaceutical Industry: Primary beneficiary (institutional/arbitrage) — uses HPA, TCGA, GTEx for target identification at zero marginal cost; contributes <5% reciprocal data; can exit to proprietary pipelines if open data quality degrades
 *   - Open Database Maintainers: Secondary victim (moderate/constrained) — Human Protein Atlas, EMBL-EBI, NCBI experience both coordination benefit (global research enabled) and extraction (asymmetric value capture); mission-locked but aware of asymmetry
 *   - Academic Researchers: Mixed position (moderate/mobile) — benefit from databases for hypothesis generation, but their publicly funded work generates targets that industry captures; can shift to proprietary partnerships but funding mandates push toward openness
 *   - Open Science Coalition: Organized agents (organized/constrained) — FORCE11, eLife, Wellcome Trust building alternative pathways (precompetitive consortia, data-sharing mandates) with 10-15 year sunset horizon
 *   - Biotech Startups: Secondary beneficiary (powerful/mobile) — use public databases for target identification with lower barriers than large pharma; more agile exit options but similar asymmetric contribution patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_science_pharma_asymmetry, 0.48).
domain_priors:suppression_score(open_science_pharma_asymmetry, 0.52).
domain_priors:theater_ratio(open_science_pharma_asymmetry, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_science_pharma_asymmetry, extractiveness, 0.48).
narrative_ontology:constraint_metric(open_science_pharma_asymmetry, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(open_science_pharma_asymmetry, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_science_pharma_asymmetry, tangled_rope).
narrative_ontology:human_readable(open_science_pharma_asymmetry, "Open Science Pharma Asymmetry in Spatial Proteomics").
narrative_ontology:topic_domain(open_science_pharma_asymmetry, "computational_biology/drug_discovery/open_science").

domain_priors:requires_active_enforcement(open_science_pharma_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_science_pharma_asymmetry, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(open_science_pharma_asymmetry, biotech_startups).
narrative_ontology:constraint_beneficiary(open_science_pharma_asymmetry, academic_researchers).
narrative_ontology:constraint_victim(open_science_pharma_asymmetry, public_research_funding).
narrative_ontology:constraint_victim(open_science_pharma_asymmetry, open_database_maintainers).
narrative_ontology:constraint_victim(open_science_pharma_asymmetry, low_resource_research_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC RESEARCH FUNDING (SNARE) — Trapped in asymmetric value flow. Funds database creation (HPA, TCGA, GTEx) that enables billions in private drug development, but cannot mandate reciprocal data contribution without risking industry collaboration collapse. No exit from the structural position of subsidizing proprietary extraction. Experiences maximum extraction: resources flow out, clinical validation data does not flow back.
constraint_indexing:constraint_classification(open_science_pharma_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN DATABASE MAINTAINERS (TANGLED ROPE) — Groups like Human Protein Atlas, EMBL-EBI, NCBI experience both coordination benefit (their data enables global research) and extraction (pharma uses data without contributing perturbation screens, clinical trial results, or negative findings). Constrained exit: could paywall or restrict access, but mission commitment and funder mandates prevent this. Mixed experience: genuine scientific coordination alongside asymmetric value capture.
constraint_indexing:constraint_classification(open_science_pharma_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL INDUSTRY (ROPE) — Experiences pure coordination benefit. Open databases provide target identification, biomarker discovery, and patient stratification data at zero marginal cost. Industry can arbitrage between public data sources and proprietary datasets, contributing selectively when strategic. No obligation to reciprocate; can exit to proprietary-only pipelines if open data quality degrades. Net beneficiary with full agency.
constraint_indexing:constraint_classification(open_science_pharma_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC RESEARCHERS (TANGLED ROPE) — Benefit from open databases for hypothesis generation and validation, but also bear cost: their publicly funded work generates targets that industry captures without sharing downstream clinical data. Mobile exit (can shift to proprietary partnerships) but mission and funding mandates push toward openness. Experience both sides: coordination for basic research, extraction when translational value is captured asymmetrically.
constraint_indexing:constraint_classification(open_science_pharma_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE COALITION (SCAFFOLD) — Groups like FORCE11, eLife, Wellcome Trust see the asymmetry as a temporary coordination failure with emerging solutions: precompetitive consortia (IMI, ADDI), data-sharing mandates in clinical trials (EMA Policy 0070), and reciprocal licensing frameworks. Sunset logic: as regulatory pressure and funder mandates mature, the one-way flow becomes structurally untenable. Organized agents building alternative pathways with 10-15 year horizon.
constraint_indexing:constraint_classification(open_science_pharma_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits genuine coordination (open databases accelerate discovery globally) and genuine extraction (asymmetric value capture concentrates private gains from public investment). The coordination function is real: HPA data has enabled target identification for hundreds of drugs. The extraction is also real: pharma contributes <5% of perturbation data relative to usage, and clinical trial data remains proprietary even when targets originated in public databases. This is the canonical tangled rope: both functions coexist structurally.
constraint_indexing:constraint_classification(open_science_pharma_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_science_pharma_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_science_pharma_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_science_pharma_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_science_pharma_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(open_science_pharma_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The asymmetry is measurable: pharmaceutical companies cite HPA data in 15-20% of target identification patents (2015-2020 analysis), but contribute <5% of perturbation data relative to usage. Clinical trial data remains proprietary even when targets originated in public databases. The extraction is not maximal because the coordination function is genuine — open databases do accelerate discovery for all parties, including public researchers. But the value capture is structurally asymmetric: private gains concentrate while public investment disperses. Suppression (0.52): Moderate. Barriers to reciprocal contribution include trade secret protection, competitive advantage concerns, regulatory complexity (clinical data sharing), and lack of enforcement mechanisms. Public funders cannot mandate industry contribution without risking collaboration collapse. However, suppression is not total — precompetitive consortia (IMI, ADDI, SGC) demonstrate that reciprocal models are possible in defined domains. The suppression is institutional rather than technological. Theater ratio (0.38): Moderate-low. Some performative elements exist (industry-sponsored 'data sharing' initiatives that release minimal competitive data, precompetitive consortia that define 'precompetitive' narrowly), but the core asymmetry is functional, not theatrical. The databases genuinely enable research; the extraction is real, not simulated. Theater has increased slightly over the interval as 'open innovation' rhetoric has grown while actual reciprocal data flow has not kept pace.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a clear perspectival split along the beneficiary-victim axis. Pharmaceutical industry experiences pure coordination (Rope) — open databases solve the target identification problem at zero cost with no obligation. Public research funding experiences pure extraction (Snare) — resources flow out to enable private drug development, clinical validation data does not flow back, and there is no exit from the structural position of subsidizing proprietary capture. Open database maintainers and academic researchers experience the mixed reality (Tangled Rope) — genuine coordination benefit (global research acceleration) coexists with genuine extraction (asymmetric value capture). The open science coalition sees a temporary problem with a sunset (Scaffold) — regulatory mandates and precompetitive consortia are building reciprocal pathways, though the timeline is uncertain. The analytical observer confirms the tangled rope classification at the civilizational scale: both coordination and extraction are structurally real and coexist in the same constraint. The gap between the pharma perspective (Rope) and the public funder perspective (Snare) is the diagnostic signature of asymmetric extraction embedded in a coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the data flow asymmetry. Public research funding bodies are victims with trapped exit — they fund database creation that enables private value capture but cannot exit without abandoning the open science mission that justifies their existence. This yields high d (victim + trapped). Pharmaceutical industry are beneficiaries with arbitrage exit — they extract value from public databases at zero marginal cost and can shift to proprietary pipelines if open data quality degrades. This yields low d (beneficiary + arbitrage). Open database maintainers are victims with constrained exit — they experience the asymmetry directly (their labor enables private gains without reciprocal contribution) but are mission-locked and funder-mandated to remain open. This yields moderate-high d (victim + constrained). Academic researchers are mixed — they benefit from databases (coordination) but their work generates targets that industry captures (extraction). Mobile exit options (can shift to proprietary partnerships) moderate the experienced extraction. Biotech startups are beneficiaries with mobile exit — similar to pharma but with more exit flexibility and less institutional inertia. The open science coalition are organized agents with constrained exit — they see the asymmetry and are building alternatives, but are constrained by the need to maintain industry engagement during the transition.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope is the correct classification when BOTH coordination and extraction are structurally present and neither can be removed without destroying the constraint. The coordination function is genuine: HPA data has enabled target identification for hundreds of drugs, accelerating discovery globally. Removing the open databases would eliminate this benefit. The extraction function is also genuine: pharma captures billions in private value from public investment while contributing <5% reciprocal data. The asymmetry is measurable and structural, not incidental. The constraint cannot be reclassified as pure Rope (coordination) because the extraction is real and quantifiable. It cannot be reclassified as pure Snare (extraction) because the coordination benefit is real and widely distributed. The tangled_rope classification captures the structural reality: a coordination mechanism with embedded asymmetric extraction that requires active enforcement (open science mandates) to maintain and that benefits identifiable agents (pharma) while imposing costs on identifiable victims (public funders, database maintainers). The perspectival gap between beneficiary (Rope) and victim (Snare) perspectives, with the analytical observer confirming both functions coexist (Tangled Rope), is the resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocal_contribution_threshold,
    'What level of reciprocal data contribution would shift the constraint from tangled_rope to rope?',
    'Quantitative analysis of data flow asymmetry: measure pharma usage (citations in patents, SEC filings mentioning HPA/TCGA) vs contribution (perturbation screens deposited, clinical data shared). Establish threshold ratio where coordination benefit exceeds extraction.',
    'If threshold is 20% reciprocal contribution: current <5% rate confirms tangled_rope. If threshold is 60%+: constraint is closer to snare (extraction dominates). Threshold determines whether incremental policy (mandates, incentives) can resolve asymmetry or whether structural redesign is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_contribution_threshold, empirical, 'Data contribution threshold for coordination-extraction balance').

omega_variable(
    precompetitive_boundary_stability,
    'Are precompetitive consortia (IMI, ADDI, Structural Genomics Consortium) genuinely expanding the reciprocal zone, or are they theater that allows continued asymmetric extraction in competitive domains?',
    'Longitudinal tracking of consortium outputs: measure data release rates, compare consortium-generated targets to proprietary pipeline targets, assess whether competitive-domain data sharing increases after precompetitive collaboration or remains static.',
    'If consortia expand reciprocity: scaffold perspective confirmed, sunset is real. If consortia are boundary theater: extraction persists in competitive domains, and the ''precompetitive'' framing is a pressure valve that prevents structural reform without changing the core asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precompetitive_boundary_stability, empirical, 'Whether precompetitive consortia drive genuine reciprocity expansion').

omega_variable(
    regulatory_mandate_enforceability,
    'Can regulatory mandates (EMA Policy 0070, NIH data sharing) actually enforce reciprocal contribution from industry, or do they apply only to publicly funded research?',
    'Policy analysis: map enforcement mechanisms, identify loopholes (proprietary tool exemptions, trade secret carve-outs), measure compliance rates for industry vs academia. Track whether mandates apply symmetrically or create two-tier system.',
    'If mandates are enforceable on industry: regulatory pathway can resolve asymmetry (scaffold confirmed). If mandates apply only to public sector: they increase extraction by mandating one-way flow without reciprocal obligation (snare from public funder perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_mandate_enforceability, empirical, 'Enforceability of data-sharing mandates on private industry').

omega_variable(
    negative_result_publication_bias,
    'Does the asymmetry in negative result sharing (pharma retains failed targets, academia publishes) create a structural information advantage that compounds the extraction?',
    'Compare target failure rates in public databases vs estimated industry failure rates (from pipeline attrition data). Measure whether pharma avoids public-database targets that failed internally, creating adverse selection where public researchers pursue targets with hidden negative data.',
    'If negative results are symmetrically hidden: extraction is lower (both sides lack data). If pharma retains negative results while using public positive results: extraction is compounded — industry has information advantage that public funders subsidized but cannot access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negative_result_publication_bias, empirical, 'Asymmetry in negative result sharing and its impact on extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_science_pharma_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ospa_theater_2010, open_science_pharma_asymmetry, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ospa_theater_2015, open_science_pharma_asymmetry, theater_ratio, 5, 0.33).
narrative_ontology:measurement(ospa_theater_2020, open_science_pharma_asymmetry, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(ospa_extract_2010, open_science_pharma_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ospa_extract_2015, open_science_pharma_asymmetry, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(ospa_extract_2020, open_science_pharma_asymmetry, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_science_pharma_asymmetry, information_standard).
narrative_ontology:affects_constraint(open_science_pharma_asymmetry, clinical_trial_data_opacity).
narrative_ontology:affects_constraint(open_science_pharma_asymmetry, academic_publishing_asymmetry).
narrative_ontology:affects_constraint(open_science_pharma_asymmetry, precompetitive_consortium_boundaries).

% DUAL FORMULATION NOTE:
% The open science pharma asymmetry is part of a broader constraint family around asymmetric data flows in translational research. Related constraints include clinical trial data opacity (downstream — proprietary clinical data that could validate public database targets), academic publishing asymmetry (parallel — publicly funded research paywalled by commercial publishers), and precompetitive consortium boundaries (potential resolution pathway — but boundaries may be theater). Each has distinct epsilon values reflecting different extraction mechanisms, but all share the structural pattern of public investment enabling private capture without reciprocal contribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
