% ============================================================================
% CONSTRAINT STORY: genomic_complexity_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genomic_complexity_gap, []).

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
 *   constraint_id: genomic_complexity_gap
 *   human_readable: Genomic Complexity Gap: Prediction vs. Phenotype
 *   domain: genetics/molecular_biology/systems_biology
 *
 * SUMMARY:
 *   The genomic complexity gap is a structural constraint in modern biology:
 *   the systematic failure of genomic data to predict phenotypic outcomes in
 *   complex traits despite decades of investment in sequencing and
 *   association studies. This constraint operates as a tangled
 *   coordination-extraction hybrid where genuine advances in genomic
 *   technology and methodology combine with institutional lock-in around
 *   reductionist frameworks that promise more predictive power than the data
 *   supports. The gap has widened as a theater phenomenon even as sequencing
 *   costs have dropped — the promise of personalized precision medicine based
 *   on genetic profiles has become decoupled from demonstrated clinical
 *   utility. The constraint manifests differently across institutional
 *   positions: for clinical medicine it appears as a snare (impossible
 *   demands with no exit), for reductionist research programs it appears as
 *   pure coordination (enabling mechanism), for systems biology coalitions it
 *   appears as a temporary scaffold (building alternative frameworks), for
 *   the genomic determinism narrative it appears as an inert piton
 *   (maintained by institutional momentum despite functional decay). The
 *   analytical observer risks naturalizing this as a fundamental limit of
 *   biological complexity rather than recognizing it as a contingent
 *   institutional arrangement.
 *
 * KEY AGENTS:
 *   - Clinical Medicine: Primary victim (powerless/trapped) — demands for genomically-guided treatment exceed what genotype-phenotype correlations support; cannot exit
 *   - Translational Researchers: Secondary victim (moderate/constrained) — pressured to bridge gap with products that overpromise; can shift research direction but constrained by funding and institutional expectations
 *   - Reductionist Genomics Programs: Primary beneficiary (institutional/arbitrage) — funding, publications, and technology markets expand regardless of phenotypic prediction success; can arbitrage the gap itself
 *   - Integrative Systems Biology Coalition: Organized agent (organized/constrained) — building multi-omics and environmental integration frameworks to bypass reductionist lock-in; perceives sunset horizon
 *   - Genomic Determinism Narrative: Institutional actor (institutional/arbitrage) — maintains rhetorical commitment despite empirical disconnect; theater-dominated rather than function-driven
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional arrangement as biological fundamental
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genomic_complexity_gap, 0.52).
domain_priors:suppression_score(genomic_complexity_gap, 0.58).
domain_priors:theater_ratio(genomic_complexity_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genomic_complexity_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(genomic_complexity_gap, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(genomic_complexity_gap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genomic_complexity_gap, tangled_rope).
narrative_ontology:human_readable(genomic_complexity_gap, "Genomic Complexity Gap: Prediction vs. Phenotype").
narrative_ontology:topic_domain(genomic_complexity_gap, "genetics/molecular_biology/systems_biology").

domain_priors:requires_active_enforcement(genomic_complexity_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genomic_complexity_gap, reductionist_research_programs).
narrative_ontology:constraint_beneficiary(genomic_complexity_gap, pharmaceutical_industry).
narrative_ontology:constraint_victim(genomic_complexity_gap, integrative_systems_understanding).
narrative_ontology:constraint_victim(genomic_complexity_gap, translational_medicine_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLINICAL MEDICINE (SNARE) — Trapped in the gap between genomic data and therapeutic efficacy. Clinicians cannot exit: patients demand precision medicine promises based on genomic profiles, but genotype-phenotype predictions fail systematically for complex traits. Bears full cost of false precision — wasted treatments, false hope, resource misallocation. No coordination benefit; maximum extraction.
constraint_indexing:constraint_classification(genomic_complexity_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSLATIONAL RESEARCHERS (TANGLED ROPE) — Constrained by funding incentives and institutional expectations to bridge the gap. Benefit from coordination: genomic datasets enable large-scale studies and biomarker discovery. Also extracted from: pressure to produce clinical applications that exceed what the data supports. Moderate agency and some exit (can shift to pure basic research) but constrained by career path and grant dependencies.
constraint_indexing:constraint_classification(genomic_complexity_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REDUCTIONIST GENOMICS PROGRAMS (ROPE) — Institutional beneficiary. Experiences constraint as pure coordination: genomic sequences enable hypothesis generation, large-scale association studies, and technological advancement. Can arbitrage the gap itself — sell precision medicine tools, generate publications from association studies, expand sequencing technology markets. Net beneficiary with high exit capacity.
constraint_indexing:constraint_classification(genomic_complexity_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATIVE SYSTEMS BIOLOGY COALITION (SCAFFOLD) — Organized movement (network biology, multi-omics, machine learning) building alternative frameworks that incorporate non-genetic factors (epigenetics, environment, developmental context). See the genomic reductionism as temporary institutional lock-in with a sunset clause: as computational biology matures and multi-scale models replace single-variable prediction, the false promise of genomic determinism will fade. Theater is declining (alternative validation pathways emerging) and sunset is structurally foreseeable (10-20 year horizon).
constraint_indexing:constraint_classification(genomic_complexity_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GENOMIC DETERMINISM RHETORIC (PITON) — The institutional narrative that 'the genome is destiny' or 'genotype determines phenotype' persists as the dominant framing despite 25+ years of failed prediction. Theater ratio is high: grant applications invoke determinism, press releases claim precision medicine breakthroughs, textbooks teach the central dogma. The actual predictive function of genomic data for complex traits is weak (heritability explains 10-40% of phenotypic variance; SNP-based prediction explains 1-5%). The mismatch between rhetoric and function indicates piton: institutional inertia maintains the deterministic framing because the alternative (acknowledging systems complexity, environmental embeddedness, developmental contingency) threatens existing research structures.
constraint_indexing:constraint_classification(genomic_complexity_gap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPLEXITY VIEW (MOUNTAIN) — From a civilizational/universal analytical position, the gap between genomic information and phenotypic outcome is a consequence of irreducible biological complexity: non-linear interactions, developmental history, environmental dependence, and stochastic effects create a principled epistemic limit on genetic prediction. This perspective sees the gap as a natural law rather than an institutional failure. However, the base properties contradict this — suppression (0.58), theater (0.68), and asymmetric extraction reveal that the 'natural law' framing naturalizes what is actually a contingent institutional commitment to reductionism.
constraint_indexing:constraint_classification(genomic_complexity_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genomic_complexity_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genomic_complexity_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genomic_complexity_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genomic_complexity_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genomic_complexity_gap, TR),
    TR >= 0.70.

:- end_tests(genomic_complexity_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The original research community captures technology development benefits, publication advantage from large association studies, and prestige from big genomics programs. The extraction is real but not maximal because genuine advances in sequence annotation and population genomics do occur. The measurement trajectory from 0.28 to 0.52 reflects increasing theater (performative precision medicine promises) layered over stable underlying methodological gains. Suppression (0.58): Moderate-high. Significant barriers to exit from the reductionist framework include: (a) institutional infrastructure (sequencing pipelines, genomic databases) optimized for single-variant analysis, (b) funding concentration in genome-scale projects, (c) textbook and training curricula built on genetic determinism, (d) publication bias toward positive associations, (e) industry investment in genomic testing products. These barriers are surmountable but substantial. Theater ratio (0.68): High and rising. Press releases claim precision medicine breakthroughs despite weak SNP-based prediction. Grant language invokes genetic determinism despite explicit knowledge that complex traits involve gene-environment interaction. GWAS papers report associations that explain 1-5% of phenotypic variance while claiming clinical relevance. The theater has increased over the interval as precision medicine marketing has accelerated despite stagnating prediction accuracy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range of indexical classification. Clinical medicine sees a snare (trapped in the gap, no exit, pure extraction). Translational researchers see tangled rope (genuine coordination through data access plus extraction through unmet promises). Reductionist programs see pure rope (enabling mechanism with no cost). Systems biology sees a temporary scaffold (sunset horizon as alternatives mature). The genomic determinism narrative sees itself as inevitable piton (persisting through institutional momentum). The analytical observer risks seeing mountain (natural complexity limit). The perspectival gaps are not measurement artifacts — they reflect genuine structural differences in exit options, power positions, and extraction flows. The clinical agent truly cannot walk away without abandoning patients. The researcher truly faces career consequences for rejecting genomic frameworks. The institutional program truly benefits from expansion. The gap is not ambiguous when viewed from each position; it becomes ambiguous only when trying to force a single type across all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality flow reveals extraction from clinical medicine toward research infrastructure. Clinical agents are trapped (high d → high f(d) → high chi) and bear costs of false precision. Reductionist programs are institutional beneficiaries with arbitrage options (low d → low/negative f(d)) and experience the constraint as enabling. Translational researchers are constrained by career path dependence (moderate exit cost, victim status) producing moderate d and moderate chi. Systems biology coalition is organized with partial exit capacity (building alternatives) producing lower chi despite victim perception. The institutional narrative uses low-d institutional positioning (arbitrage: can move between genomics and adjacent biotech fields) to maintain deterministic framing despite empirical contradiction. The analytical observer with analytical exit options and observational detachment produces neutral d, risking false mountain classification if the natural-law frame isn't scrutinized.
 *
 * MANDATROPHY ANALYSIS:
 *   THERAPEUTIC RESOLUTION: This constraint resolves mandatrophy by showing how the same structural phenomenon yields different types depending on agent position. The mandatrophy question is not 'is this reductionism extraction or coordination?' but 'for whom?' For research institutions, genomic analysis is pure coordination (enabling). For clinical medicine, it is extraction (false promises with no exit). For systems biology, it is temporary (sunset visible). The resolution is not to collapse all perspectives into one type but to recognize that the constraint operates through differential extraction — it benefits some agents while imposing costs on others in an asymmetric way that requires active enforcement (institutional commitment to deterministic framing despite empirical failure). The false mountain classification (naturalizing as biological limit) is detected by observing: (1) suppression is institutional not fundamental, (2) theater is rising despite technical progress, (3) significant organized resistance (systems biology coalition) sees a sunset, (4) asymmetric extraction toward powerless clinical agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heritability_vs_predictability,
    'Does high heritability of a trait guarantee high genomic predictability?',
    'Systematic comparison across traits: identify traits with high twin/family heritability but low SNP-based prediction; identify rare genetic variants with large effect sizes that explain heritability gap; conduct missing heritability studies',
    'If heritability and predictability are independent: reductionist research programs are pursuing an inherently unachievable goal (fundamental limit, not empirical gap). If they correlate: the gap is resolvable through better sequencing/imputation (temporary coordination problem, not structural extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heritability_vs_predictability, empirical, 'Relationship between heritability and genomic predictability').

omega_variable(
    polygenicity_threshold,
    'What is the threshold number of causal variants above which genomic prediction becomes structurally intractable?',
    'Simulation studies of prediction accuracy as function of causal variant count and effect size distribution; empirical data from polygenic traits (height, BMI) showing saturation of prediction as more variants are sequenced',
    'If threshold < 100 variants: many complex traits are fundamentally unpredictable from genomics alone. If threshold > 100,000 variants: prediction may improve with genomic coverage (optimism justified). Impacts whether gap is a natural law (mountain) or institutional arrangement (snare/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polygenicity_threshold, empirical, 'Threshold for polygenicity-driven prediction intractability').

omega_variable(
    non_genetic_variance_fraction,
    'What fraction of phenotypic variance in complex traits comes from non-genetic factors (environment, epigenetics, developmental stochasticity)?',
    'Twin studies with environmental isolation; epigenetic screening across development; multi-omics integration; machine learning models incorporating non-genetic data and measuring variance explained improvement',
    'If > 70%: genomic reductionism is fundamentally misaligned with biology (snare perspective confirmed). If < 30%: genomics is appropriately central (rope perspective justified). The gap is either a natural consequence of biological systems or an institutional choice to ignore the dominant variance source.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_genetic_variance_fraction, empirical, 'Fraction of variance from non-genetic sources').

omega_variable(
    environmental_coupling_measurement,
    'Can non-genetic factors be systematically measured and incorporated into prediction models?',
    'Development of environmental phenotyping standards; integration of EHR, lifestyle, and exposure data into polygenic models; comparison of prediction accuracy with vs without environmental covariates',
    'If yes: the gap is organizational (we can integrate multi-scale data but don''t). This supports the scaffold sunset thesis. If no: the gap reflects inherent measurement barriers and supports mountain/natural law view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_coupling_measurement, empirical, 'Feasibility of systematic environmental factor measurement').

omega_variable(
    institutional_incentive_lock,
    'Do funding, career, and publication structures create path dependence toward genomic reductionism regardless of predictive utility?',
    'Historical analysis of research funding flows; career trajectory mapping for genomics vs systems biology researchers; bibliometric analysis of citation patterns and journal gatekeeping',
    'If yes: the constraint is extractive and maintained by institutional structure (tangled rope/snare perspective). If no: the constraint reflects genuine epistemological commitments (rope or mountain perspective). Addresses whether the gap is science-driven or incentive-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_lock, empirical, 'Role of institutional incentives in maintaining reductionist commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genomic_complexity_gap, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genomic_tr_t0, genomic_complexity_gap, theater_ratio, 0, 0.38).
narrative_ontology:measurement(genomic_tr_t5, genomic_complexity_gap, theater_ratio, 5, 0.52).
narrative_ontology:measurement(genomic_tr_t10, genomic_complexity_gap, theater_ratio, 10, 0.68).
narrative_ontology:measurement(genomic_tr_t15, genomic_complexity_gap, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(genomic_be_t0, genomic_complexity_gap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(genomic_be_t5, genomic_complexity_gap, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(genomic_be_t10, genomic_complexity_gap, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(genomic_be_t15, genomic_complexity_gap, base_extractiveness, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genomic_complexity_gap, resource_allocation).
narrative_ontology:affects_constraint(genomic_complexity_gap, precision_medicine_promise_gap).
narrative_ontology:affects_constraint(genomic_complexity_gap, gene_environment_interaction_suppression).
narrative_ontology:affects_constraint(genomic_complexity_gap, polygenicity_measurement_intractability).

% DUAL FORMULATION NOTE:
% The genomic complexity gap is distinct from specific failed predictions (precision medicine promise) and from the general problem of gene-environment interaction. The gap is the structural constraint maintaining commitment to genomic reductionism despite known limitations. It affects downstream constraints by providing the institutional scaffold that enables precision medicine hype and suppresses integrative biology frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genomic_complexity_gap, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
