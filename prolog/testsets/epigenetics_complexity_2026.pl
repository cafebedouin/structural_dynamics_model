% ============================================================================
% CONSTRAINT STORY: epigenetics_complexity_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epigenetics_complexity_2026, []).

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
 *   constraint_id: epigenetics_complexity_2026
 *   human_readable: Epigenetic Regulatory Constraint in Systems Biology
 *   domain: biological_science/regulatory_mechanism
 *
 * SUMMARY:
 *   The discovery that the human genome contains approximately 20,000
 *   protein-coding genes created an explanatory crisis: how could such a
 *   limited genetic toolkit generate the observed biological complexity
 *   across cell types, developmental stages, and disease states? Epigenetics
 *   emerged as the primary mechanism resolving this crisis — the idea that
 *   chemical modifications (DNA methylation, histone acetylation, chromatin
 *   remodeling) regulate gene expression without changing DNA sequence. This
 *   constraint exhibits the characteristic structure of a Tangled Rope: it
 *   combines genuine coordination (explaining biological diversity through
 *   regulatory mechanisms) with asymmetric extraction (imposing measurement
 *   burden, computational complexity, and skill requirements that concentrate
 *   research capacity). The theater_ratio rising from 0.42 to 0.68 over the
 *   interval reflects the pedagogical narrative of 'epigenetics as solution'
 *   becoming increasingly decoupled from the functional reality that
 *   integrated epigenetic prediction remains far below biological accuracy
 *   targets. The constraint demonstrates how an explanatory paradigm can
 *   simultaneously solve an intellectual problem and create an epistemic
 *   bottleneck.
 *
 * KEY AGENTS:
 *   - Predictive Model: Primary victim (powerless/trapped) — cannot achieve biological accuracy without integrating irreducible epigenetic complexity; epistemic burden is non-negotiable
 *   - Experimental Biologist: Secondary victim (moderate/constrained) — faces measurement burden (bisulfite sequencing, ChIP-seq, multi-omics platforms), cost barriers, and skill requirements; also benefits from expanded research programs and funding
 *   - Epigenetics Research Institution: Primary beneficiary (institutional/arbitrage) — captures research prestige, funding streams, and competitive advantage from paradigm shift without bearing full measurement burden
 *   - Molecular Biology Education: Institutional piton (institutional/arbitrage) — maintains theatrical representation of epigenetics as 'the solution to complexity' despite functional prediction limitations; inertia-maintained narrative
 *   - Systems Biology Coalition: Organized agents (organized/constrained) — actively building integrated multi-omics frameworks with explicit sunset logic; seeing constraint as temporary technology/integration challenge
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent epistemic bottleneck as immutable law of biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epigenetics_complexity_2026, 0.38).
domain_priors:suppression_score(epigenetics_complexity_2026, 0.52).
domain_priors:theater_ratio(epigenetics_complexity_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epigenetics_complexity_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(epigenetics_complexity_2026, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(epigenetics_complexity_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epigenetics_complexity_2026, tangled_rope).
narrative_ontology:human_readable(epigenetics_complexity_2026, "Epigenetic Regulatory Constraint in Systems Biology").
narrative_ontology:topic_domain(epigenetics_complexity_2026, "biological_science/regulatory_mechanism").

domain_priors:requires_active_enforcement(epigenetics_complexity_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epigenetics_complexity_2026, complexity_reducing_narrative).
narrative_ontology:constraint_beneficiary(epigenetics_complexity_2026, epigenetic_research_institutions).
narrative_ontology:constraint_victim(epigenetics_complexity_2026, predictive_model_reliability).
narrative_ontology:constraint_victim(epigenetics_complexity_2026, reductionist_biology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREDICTIVE MODEL (SNARE) — Trapped in the epigenetic complexity bottleneck. Cannot exit the requirement to account for methylation, histone modifications, chromatin remodeling, phase separation, and non-coding RNA regulation simultaneously. Genomic sequencing alone is insufficient; models must integrate multi-dimensional epigenetic states. This epistemic burden is irreversible — the field cannot unsee the complexity. Maximum extraction: predictive capacity is constrained by requirements that cannot be simplified or bypassed without sacrificing biological accuracy.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXPERIMENTAL BIOLOGIST (TANGLED ROPE) — Constrained by epigenetic measurement requirements (bisulfite sequencing, ChIP-seq, ATAC-seq, CUT&RUN) and cost barriers, yet benefits from the expanded explanatory framework. The epigenetic paradigm provides tools and grants for research programs that would not exist without the complexity narrative. Real coordination (explaining biological diversity) mixed with real extraction (measurement burden, sample requirements, interdisciplinary skill ceiling). Significant agency but also significant constraint.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EPIGENETICS RESEARCH INSTITUTION (ROPE) — Institutional beneficiary with arbitrage capacity. The epigenetic paradigm shift created entire research domains, funding streams, and career pathways. Institutions that embraced epigenetics (Broad Institute, MRC, Max Planck) gained competitive advantage and research prestige. The constraint operates as pure coordination: communication that epigenetics is the key to complexity generates collaboration, funding, and validation. Extraction runs toward this agent — they benefit from the narrative without bearing its epistemic costs.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MOLECULAR BIOLOGY TEXTBOOK (PITON) — The pedagogical representation of epigenetics as 'the solution to the complexity problem' is largely performative. Most teaching emphasizes conceptual epigenetic mechanisms (methyl marks, histone states, chromatin domains) rather than their integrated function. The theatrical function (establishing intellectual legitimacy) dominates the functional performance (actually predicting cellular states from epigenetic data). Textbooks maintain the epigenetics narrative through institutional inertia despite the reality that integrated epigenetic prediction remains far below biological accuracy targets. Theater ratio high because the conceptual narrative persists even as empirical integration lags.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SYSTEMS BIOLOGY COALITION (SCAFFOLD) — Organized agents (SEQC consortium, Data Coordination Center, single-cell multi-omics projects) are building integrated epigenetic frameworks with explicit sunset logic: as machine learning models incorporate sufficient multi-scale epigenetic data, the transitional phase of 'epigenetics as explanatory bottleneck' resolves into 'epigenetics as integrated regulatory layer.' The coalition sees the constraint as temporary — complexity is real, but decomposable through systematic multi-omics mapping and computational integration. High agency, explicit pathway to functional resolution.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, biological complexity with limited protein-coding genes is inherent to eukaryotic organization: regulation at the epigenetic level is not contingent but necessary. Gene expression variation across tissues and developmental stages requires regulatory mechanisms beyond the transcriptional code; epigenetic mechanisms (methylation, chromatin state, phase separation) are structural necessities, not institutional artifacts. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of what is actually a contingent epistemic bottleneck masquerading as immutable biological law.
constraint_indexing:constraint_classification(epigenetics_complexity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epigenetics_complexity_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epigenetics_complexity_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(epigenetics_complexity_2026, TR),
    TR >= 0.70.

:- end_tests(epigenetics_complexity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The epigenetic complexity requirement genuinely constrains predictive capacity, but the extraction is not total — experimental biologists can conduct meaningful research with partial epigenetic data, and systems biology approaches are decomposing the problem into tractable sub-mechanisms. The value reflects that the bottleneck is real but partially solvable through technology and methodology. Suppression (0.52): Moderate-high. Significant barriers include equipment costs (ChIP-seq, ATAC-seq), computational infrastructure, specialized bioinformatic skills, and the sheer dimensionality of the epigenetic feature space. However, suppression is declining as single-cell multi-omics platforms mature and costs decrease. Theater ratio (0.68): High. The pedagogical representation of epigenetics as 'the key to complexity' is substantially performative — most teaching emphasizes conceptual mechanisms rather than their integrated predictive function. Textbooks maintain the narrative despite the reality that epigenetic integration lags biological requirements. The theater has increased as the complexity of empirical findings has made simplified narratives less adequate but more pedagogically necessary.
 *
 * PERSPECTIVAL GAP:
 *   The Snare perspective (predictive model) sees only burden; the Rope perspective (research institution) sees only benefit; the Tangled Rope perspective (experimental biologist) sees both simultaneously. This gap is unbridgeable from within individual perspectives — only the indexed classification system can show that all perspectives are valid descriptions of the same structural constraint. The false summit at the mountain perspective is particularly instructive: naturalizing epigenetic complexity as 'inherent to biology' conceals the contingent institutional choices (measurement focus, funding concentration, skill requirements) that created and maintain the bottleneck.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Beneficiaries (research institutions, funding agencies) experience low or negative effective extraction — they gain prestige and resources from the epigenetic paradigm without bearing full measurement burden. Victims (predictive models, experimental capacity) experience high extraction — they must incorporate irreducible complexity without commensurate explanatory gain. The experimental biologist occupies a middle position: constrained by measurement requirements but also enabled by research programs and funding. The composition of beneficiaries and victims reveals that the constraint concentrates research capacity (toward well-funded institutions with epigenetic expertise) while distributing burden (across the entire field through measurement requirements and complexity standards). This asymmetry is the structure of the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that 'epigenetics as explanatory bottleneck' and 'epigenetics as genuine regulatory mechanism' are structurally distinct claims that should decompose into separate stories. The genuine regulatory claim (epigenetic mechanisms cause gene expression variation) has moderate extractiveness and may be a Mountain or Mountain-bordered constraint depending on empirical resolution of omega_epigenetic_causality_direction. The bottleneck claim (integrated epigenetic prediction is required for biological accuracy) has moderate-to-high extractiveness and benefits from Tangled Rope classification because it simultaneously solves (coordinates biology across cell types) and constrains (imposes measurement and computational burden). The mandatrophy is resolved by refusing to collapse these into a single explanation: accept both the genuine regulatory mechanism and the contingent epistemic bottleneck without forcing one to be 'really true' and the other 'really false.' The field benefits from both simultaneously — the genuine mechanism provides explanatory power; the bottleneck provides research focus and funding rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epigenetic_causality_direction,
    'To what extent are epigenetic states causal regulators of gene expression versus correlative reporters of cell state?',
    'Perturbational experiments (targeted demethylation, histone modification induction, chromatin remodeling); tracking of epigenetic state changes prior to phenotypic change; causal inference from time-series multi-omics data',
    'If primarily causal: epigenetics is a genuine regulatory layer requiring independent complexity accounting. If primarily correlative: epigenetic states are readable outputs of upstream signaling; complexity resides elsewhere. Shifts classification from Snare (inevitable burden) to false bottleneck.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epigenetic_causality_direction, empirical, 'Whether epigenetic states are causal or correlative').

omega_variable(
    integrative_prediction_ceiling,
    'What is the maximum predictive accuracy achievable from complete epigenetic data (all marks, modifications, chromatin states) for cell type, developmental stage, and disease status in a given tissue?',
    'Benchmarking studies on well-characterized tissues (primary lymphocytes, cultured cell lines, embryonic stages); machine learning on comprehensive epigenetic feature sets; comparison against simpler genomic baselines',
    'If ceiling exceeds 85% accuracy: epigenetics is genuinely necessary explanatory layer. If ceiling near 60%: epigenetics explains modest additional variance; other regulatory mechanisms dominate. Determines whether complexity extraction is justified or overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integrative_prediction_ceiling, empirical, 'Maximum predictive ceiling from complete epigenetic data').

omega_variable(
    measurement_resolution_sufficiency,
    'Do current epigenetic measurement technologies (whole-genome bisulfite sequencing, ChIP-seq, ATAC-seq, CUT&RUN) adequately capture functional epigenetic state, or is substantial regulatory information lost to measurement resolution limits?',
    'Comparison of functional outcomes from single-cell multi-omics studies versus bulk averaging; identification of cell-state-specific epigenetic patterns missed by bulk assays; functional validation of inferred epigenetic states in single-cell systems',
    'If adequately captured: current measurement burden is justified. If substantial loss: the epigenetic bottleneck is partly an artifact of measurement limitations rather than biological necessity. Shifts classification toward scaffold (temporary technology constraint) rather than snare (inherent complexity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_resolution_sufficiency, empirical, 'Whether current epigenetic measurements capture functional state').

omega_variable(
    decomposition_sufficiency,
    'Can the ''epigenetic complexity problem'' be decomposed into more specific sub-mechanisms (cis-regulatory element function, nucleosome positioning, phase-separated transcription hubs) that each have tractable predictive models?',
    'Systematic review of decomposed models; assessment of predictive accuracy when sub-mechanisms are modeled independently versus as integrated system; identification of genuine inter-mechanism dependencies versus apparent coupling from measurement averaging',
    'If decomposable: complexity is manageable through modular approaches. Current ''epigenetics'' label conflates multiple constraints that could be separated. If genuine integration: epigenetics is an irreducible multi-scale system. Determines whether the constraint is a single Tangled Rope or a constraint family requiring network decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decomposition_sufficiency, conceptual, 'Whether epigenetic complexity is decomposable into modular sub-mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epigenetics_complexity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epigenetics_tr_t0, epigenetics_complexity_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(epigenetics_tr_t5, epigenetics_complexity_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(epigenetics_tr_t10, epigenetics_complexity_2026, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(epigenetics_be_t0, epigenetics_complexity_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(epigenetics_be_t5, epigenetics_complexity_2026, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(epigenetics_be_t10, epigenetics_complexity_2026, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epigenetics_complexity_2026, information_standard).
narrative_ontology:affects_constraint(epigenetics_complexity_2026, genomic_complexity_gap).
narrative_ontology:affects_constraint(epigenetics_complexity_2026, single_cell_measurement_integration).
narrative_ontology:affects_constraint(epigenetics_complexity_2026, developmental_trajectory_prediction).

% DUAL FORMULATION NOTE:
% The epigenetic constraint family decomposes into three distinct mechanisms: (1) genuine epigenetic causality in gene regulation (lower extractiveness, may be Mountain); (2) the epistemic bottleneck created by requiring integrated epigenetic prediction (this story, Tangled Rope, ε=0.38); (3) the measurement technology constraint in capturing sufficient epigenetic resolution (Scaffold, temporary). Each has distinct ε values and decomposed stories. This story focuses on the epistemic bottleneck structure rather than the underlying biological mechanism or measurement technology limitations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
