% ============================================================================
% CONSTRAINT STORY: causal_inference_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_causal_inference_gap, []).

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
 *   constraint_id: causal_inference_gap
 *   human_readable: Causal Inference Gap in Complex Biological Systems
 *   domain: systems_biology/epistemology/translational_medicine
 *
 * SUMMARY:
 *   The causal inference gap in complex biological systems represents a
 *   structural disconnect between statistical association (what GWAS
 *   identifies) and causal mechanism (what translational medicine requires).
 *   Since the first wave of genome-wide association studies in 2007, the
 *   field has identified tens of thousands of disease-associated genetic
 *   variants, yet mechanistic validation rates remain below 15% and
 *   therapeutic translation rates below 5%. Missing heritability estimates
 *   (the gap between SNP-based heritability and pedigree-based heritability)
 *   range from 40% to 80% for most complex diseases, indicating that the
 *   majority of genetic architecture remains statistically invisible or
 *   mechanistically opaque. This constraint exhibits strong extraction
 *   accumulation over its 15-year interval: as GWAS sample sizes grew from
 *   thousands to millions, the marginal effect sizes of new discoveries
 *   decreased (winner's curse exhaustion), polygenicity increased (more
 *   variants of smaller effect), and the proportion of associations with
 *   plausible biological mechanisms declined. The theater ratio has increased
 *   correspondingly: polygenic risk scores are marketed as precision medicine
 *   despite limited clinical utility, and association publications continue
 *   to dominate funding and prestige despite poor downstream conversion
 *   rates. The constraint is downstream of two structural features:
 *   perturbation_epistemology (the mountain-level difficulty of inferring
 *   causation in non-perturbable complex systems) and methodological_lock_in
 *   (the tangled_rope of career incentives and infrastructure investment in
 *   association-based approaches).
 *
 * KEY AGENTS:
 *   - Translational Medicine Programs: Primary victim (powerless/trapped) — cannot convert associations to therapeutic targets; 85-90% failure rate in mechanistic validation; no exit from GWAS paradigm at biographical timescale
 *   - Patients Awaiting Treatments: Ultimate victim (powerless/trapped) — diseases with known genetic associations but no validated mechanisms remain untreatable; missing heritability represents biological understanding that cannot be acted upon clinically
 *   - Drug Development Pipelines: Secondary victim (moderate/constrained) — 90% Phase II failure rate for GWAS-derived targets; can partially exit via phenotypic screening but at significant cost; also benefit from association data as search-space filter
 *   - GWAS Research Groups: Primary beneficiary (institutional/arbitrage) — capture citation, funding, and career advancement from association discoveries regardless of mechanistic validation; inference gap is invisible from this position
 *   - Statistical Genomics Platforms: Primary beneficiary (institutional/arbitrage) — UK Biobank, All of Us, FinnGen capture sustained funding via association generation; causal gap is structurally advantageous (prevents one-time value extraction)
 *   - Biomarker Discovery Programs: Institutional actor (institutional/constrained) — maintain degraded ritual of polygenic risk scores with limited clinical utility; theater ratio >0.70
 *   - Functional Genomics Consortium: Organized agents (organized/mobile) — ENCODE, CRISPR screening consortia, single-cell atlases building alternative association-to-mechanism pipelines with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine epistemic limitation (emergent properties of complex systems) and extractive institutional arrangement (career incentives misaligned with mechanistic validation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(causal_inference_gap, 0.68).
domain_priors:suppression_score(causal_inference_gap, 0.72).
domain_priors:theater_ratio(causal_inference_gap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(causal_inference_gap, extractiveness, 0.68).
narrative_ontology:constraint_metric(causal_inference_gap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(causal_inference_gap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(causal_inference_gap, snare).
narrative_ontology:human_readable(causal_inference_gap, "Causal Inference Gap in Complex Biological Systems").
narrative_ontology:topic_domain(causal_inference_gap, "systems_biology/epistemology/translational_medicine").

domain_priors:requires_active_enforcement(causal_inference_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(causal_inference_gap, gwas_research_groups).
narrative_ontology:constraint_beneficiary(causal_inference_gap, statistical_genomics_platforms).
narrative_ontology:constraint_beneficiary(causal_inference_gap, biomarker_discovery_programs).
narrative_ontology:constraint_victim(causal_inference_gap, translational_medicine_programs).
narrative_ontology:constraint_victim(causal_inference_gap, drug_development_pipelines).
narrative_ontology:constraint_victim(causal_inference_gap, precision_medicine_initiatives).
narrative_ontology:constraint_victim(causal_inference_gap, patients_awaiting_treatments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSLATIONAL MEDICINE PROGRAMS (SNARE) — Trapped in a cycle where statistical associations cannot be converted to therapeutic targets. Cannot exit the GWAS paradigm because funding, infrastructure, and career paths are locked into association-based discovery. Bears maximum extraction: invests resources pursuing statistically significant variants that fail mechanistic validation at 85-90% rates. No alternative pathway available at biographical timescale.
constraint_indexing:constraint_classification(causal_inference_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PATIENTS AWAITING TREATMENTS (SNARE) — Ultimate victims with zero exit options. The gap between association and mechanism directly translates to therapeutic delay: diseases with identified genetic associations but no validated causal pathways remain untreatable. Missing heritability (40-80% for most complex diseases) represents biological understanding that exists statistically but cannot be acted upon clinically.
constraint_indexing:constraint_classification(causal_inference_gap, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DRUG DEVELOPMENT PIPELINES (TANGLED ROPE) — Constrained by high failure rates (90% of targets from GWAS fail Phase II) but also benefit from the association data as a starting filter. Experience both coordination (GWAS narrows search space from 20,000 genes to hundreds of candidates) and extraction (most candidates fail mechanistic validation, wasting years and capital). Can partially exit by pivoting to phenotypic screening or model organism approaches, but at significant cost.
constraint_indexing:constraint_classification(causal_inference_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GWAS RESEARCH GROUPS (ROPE) — Primary beneficiaries. Capture citation advantage, funding priority, and career advancement from association discoveries regardless of downstream mechanistic validation. The inference gap is invisible from this position: publishing statistically significant associations is the terminal goal, not a step toward mechanism. Experience the constraint as pure coordination: communicating genetic architecture to the field.
constraint_indexing:constraint_classification(causal_inference_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATISTICAL GENOMICS PLATFORMS (ROPE) — Benefit from infrastructure lock-in. UK Biobank, All of Us, FinnGen, and similar platforms capture sustained funding and institutional prestige by generating associations. The causal inference gap is structurally advantageous: if associations translated directly to mechanisms, the platform's value would be one-time rather than continuous. Arbitrage exit: can pivot to other statistical phenotypes (proteomics, metabolomics) when genetic associations saturate.
constraint_indexing:constraint_classification(causal_inference_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: BIOMARKER DISCOVERY PROGRAMS (PITON) — Institutional actors maintaining a degraded ritual. Polygenic risk scores (PRS) are marketed as precision medicine tools but have limited clinical utility due to the causal inference gap: a high PRS indicates statistical risk but provides no mechanistic insight for intervention. The program persists through institutional inertia and commercial interest despite theater ratio >0.70 — most PRS applications are performative risk stratification rather than actionable clinical guidance.
constraint_indexing:constraint_classification(causal_inference_gap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: FUNCTIONAL GENOMICS CONSORTIUM (SCAFFOLD) — Organized agents (ENCODE, Roadmap Epigenomics, CRISPR screening consortia, single-cell atlases) building alternative pathways that bypass pure association. Massively parallel reporter assays, perturb-seq, and spatial transcriptomics are creating direct association-to-mechanism pipelines. Sunset logic: as functional screening costs drop and throughput increases, the GWAS-first paradigm loses necessity. Estimated sunset: 15-25 years for functional-first approaches to become standard in complex disease research.
constraint_indexing:constraint_classification(causal_inference_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the gap represents both genuine epistemic limitation (complex systems have emergent properties not reducible to component associations) and extractive institutional arrangement (career incentives reward association discovery over mechanistic validation). The constraint coordinates research effort (GWAS provides systematic genome-wide coverage) while extracting from translational goals (most associations never yield mechanisms). Analytical classification as tangled_rope rather than mountain reflects that the gap's magnitude is contingent on methodological choices and resource allocation, not purely on inherent biological complexity.
constraint_indexing:constraint_classification(causal_inference_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(causal_inference_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(causal_inference_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(causal_inference_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(causal_inference_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(causal_inference_gap, TR),
    TR >= 0.70.

:- end_tests(causal_inference_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from translational medicine and patients by creating a structural trap: statistical associations are necessary for funding and publication but insufficient for therapeutic development. GWAS research groups capture career benefits during the association-discovery phase, but 85-90% of associations never yield validated mechanisms. The extraction has increased over the interval as effect sizes decreased and polygenicity increased — recent GWAS hits are statistically robust but mechanistically more opaque than early discoveries. Suppression (0.72): High. Barriers to bridging the gap include: (1) methodological — complex diseases involve emergent network properties not reducible to single-variant effects; (2) resource — functional validation requires expensive model systems and long timelines; (3) institutional — career incentives reward association discovery over mechanistic follow-up; (4) epistemic — perturbation experiments in humans are often impossible, and model organisms introduce their own inference gaps. Translational programs cannot exit because funding, infrastructure, and expertise are locked into the GWAS paradigm. Theater ratio (0.65): Moderate-high. Polygenic risk scores are the primary theatrical output: marketed as precision medicine tools but provide minimal clinical utility due to lack of mechanistic insight. A high PRS indicates statistical risk but offers no actionable intervention pathway. The theater has increased as PRS applications proliferated despite stagnant therapeutic conversion rates. Association publications continue to dominate high-impact journals despite poor downstream validation, sustained by the ritual of statistical significance rather than mechanistic understanding.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence driven by structural position relative to the association-mechanism pipeline. GWAS research groups and statistical genomics platforms see pure coordination (rope) — they are solving the legitimate problem of systematically mapping genetic architecture, and the inference gap is invisible because mechanistic validation is not their goal. Functional genomics consortia see a temporary problem with a sunset (scaffold) — high-throughput functional screens are building direct association-to-mechanism pathways that will bypass the GWAS paradigm within 15-25 years. Biomarker discovery programs see a degraded ritual (piton) — polygenic risk scores persist through commercial interest and institutional inertia despite limited clinical utility. Drug development pipelines see mixed coordination and extraction (tangled_rope) — GWAS provides valuable search-space filtering but most candidates fail mechanistic validation. Translational medicine programs and patients see pure extraction (snare) — the gap between association and mechanism directly translates to therapeutic delay and wasted resources, with no exit available at biographical timescale. The analytical observer sees both genuine epistemic limitation (some fraction of complex disease heritability may be irreducible to variant-level mechanisms due to emergent network properties) and extractive institutional arrangement (career incentives misaligned with mechanistic validation), yielding tangled_rope rather than mountain. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' — the presheaf over observation sites captures the full constraint structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Translational medicine programs and patients are victims with trapped exit options, yielding high directionality values and maximum experienced extraction. They bear the full cost of the inference gap: resources invested in pursuing associations that fail mechanistic validation, and therapeutic delays for diseases with known genetic architecture but no validated causal pathways. Drug development pipelines are victims with constrained exit (can pivot to phenotypic screening or model organisms but at significant cost), yielding moderate-high directionality. They experience both extraction (90% target failure rate) and coordination benefit (GWAS narrows search space). GWAS research groups and statistical genomics platforms are beneficiaries with arbitrage exit options, yielding low directionality values and low or negative experienced extraction. They capture career and funding benefits from association discovery regardless of downstream mechanistic validation. The inference gap is structurally invisible from their position — publishing associations is the terminal goal. Biomarker discovery programs are institutional actors with constrained exit (locked into PRS paradigm by commercial investment and regulatory pathways), but their piton classification derives from the theater gate rather than from high experienced extraction. Functional genomics consortia are organized agents with mobile exit options (can pivot across screening modalities and biological systems), yielding moderate directionality and experiencing the constraint as a temporary coordination problem with a sunset. The analytical observer uses analytical exit and sees both genuine epistemic limitation and extractive institutional arrangement, yielding moderate directionality and tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION INCOMPLETE (extractiveness 0.68 requires resolution; omega variables identify key empirical uncertainties but do not yet provide definitive classification boundaries). The constraint risks mandatrophy collapse if the analytical observer's tangled_rope classification is misread as 'the' answer, obscuring the snare experienced by translational medicine and patients. The critical mandatrophy question is: what fraction of the causal inference gap is genuine epistemic limitation (mountain — irreducible complexity of emergent biological systems) vs methodological lock-in (tangled_rope — contingent on current approaches and resource allocation) vs pure extraction (snare — career incentives misaligned with therapeutic goals)? Omega variable 'emergent_property_fraction' directly addresses this: if >50% of complex disease heritability arises from irreducible network properties, the association-to-mechanism paradigm is categorically wrong and the gap is a mountain (requiring paradigm shift to network causation). If <20%, the gap is methodological and the snare/tangled_rope classifications dominate. Omega variable 'therapeutic_validation_lag' provides temporal resolution: if validation rates are converging (gap closing as methods improve), scaffold logic applies; if diverging (gap widening as easy targets exhaust), snare is confirmed for remaining heritability. The mandatrophy is resolved by recognizing that different fractions of the heritability landscape may occupy different constraint types: monogenic and oligogenic traits (already largely validated) were temporary scaffolds; highly polygenic traits with small effect sizes and no biological prior are snares from the translational perspective; traits involving emergent network properties may be mountains requiring paradigm shift. The constraint as a whole is a snare because the dominant extraction flow (from translational medicine and patients to GWAS research groups and platforms) is structural and increasing, but the omega variables preserve the possibility that some fraction of the gap is irreducible epistemic limitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    missing_heritability_resolution,
    'Is missing heritability (40-80% for most complex diseases) a measurement artifact, a consequence of rare variant burden below detection thresholds, or evidence of non-genetic inheritance mechanisms?',
    'Whole-genome sequencing of large cohorts with deep phenotyping; family-based designs capturing shared environment; epigenetic and microbiome heritability estimates; comparison of SNP-heritability vs pedigree-heritability across populations',
    'If measurement artifact: GWAS paradigm is fundamentally sound, gap is temporary (scaffold from more perspectives). If rare variant burden: current platforms are structurally inadequate (snare confirmed). If non-genetic mechanisms: association-to-mechanism pipeline is categorically wrong for a large fraction of heritability (mountain — irreducible epistemic limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(missing_heritability_resolution, empirical, 'Nature of missing heritability and implications for association-mechanism gap').

omega_variable(
    polygenicity_threshold,
    'At what level of polygenicity (number of causal variants) does the association-to-mechanism pipeline become structurally intractable rather than merely resource-limited?',
    'Simulation studies of mechanistic validation success rates as function of variant count; empirical tracking of validation rates for monogenic vs oligogenic vs highly polygenic traits; cost-benefit analysis of functional follow-up for traits with >1000 associated loci',
    'If threshold is low (<100 variants): most complex diseases are beyond mechanistic reach via GWAS (snare confirmed, mountain from some perspectives). If threshold is high (>1000 variants): gap is resource constraint, not epistemic limit (tangled_rope or scaffold from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(polygenicity_threshold, empirical, 'Polygenicity threshold beyond which mechanistic validation becomes intractable').

omega_variable(
    functional_screening_sufficiency,
    'Do high-throughput functional screens (CRISPR, perturb-seq, MPRA) actually bridge the causal inference gap, or do they introduce their own association-mechanism disconnect via context-dependence and model system artifacts?',
    'Cross-platform validation: compare mechanistic conclusions from cell-line screens, organoid screens, and in vivo models for the same variants; track therapeutic success rates for targets identified via functional screening vs traditional GWAS follow-up',
    'If sufficient: scaffold perspective confirmed — functional genomics provides genuine sunset for GWAS paradigm. If insufficient: the gap persists at a different level — we replace statistical association with experimental association, but mechanism remains elusive (snare from more perspectives, potential mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_screening_sufficiency, empirical, 'Whether functional genomics screens provide true mechanistic resolution').

omega_variable(
    emergent_property_fraction,
    'What fraction of complex disease heritability arises from emergent network properties (epistasis, gene-environment interaction, developmental contingency) that are irreducible to single-variant mechanisms?',
    'Network modeling of validated disease mechanisms; quantification of epistatic variance in model organisms with complete genetic control; comparison of additive vs non-additive genetic architecture across traits; systems biology approaches to pathway-level rather than variant-level causation',
    'If fraction is low (<20%): gap is methodological, not fundamental (tangled_rope or scaffold). If fraction is high (>50%): the association-to-mechanism paradigm is categorically wrong for most complex diseases — the causal structure is not decomposable into variant-level effects (mountain — irreducible epistemic limit requiring paradigm shift to network causation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergent_property_fraction, conceptual, 'Fraction of heritability arising from irreducible emergent properties').

omega_variable(
    therapeutic_validation_lag,
    'What is the empirical distribution of time-to-therapeutic-validation for GWAS-identified associations, and does it show convergence (gap closing over time) or divergence (gap widening as easy targets are exhausted)?',
    'Longitudinal tracking of GWAS hits from discovery to clinical trial; survival analysis of association-to-drug timelines; comparison of validation rates for early GWAS (2007-2012) vs recent GWAS (2018-2023); stratification by effect size, allele frequency, and biological prior knowledge',
    'If converging: gap is temporary resource constraint (scaffold). If diverging: gap is structural feature of the paradigm — low-hanging fruit (large effect, known biology) are exhausted, leaving associations that are statistically robust but mechanistically intractable (snare confirmed, potential mountain for remaining heritability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(therapeutic_validation_lag, empirical, 'Temporal trajectory of association-to-therapeutic validation rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(causal_inference_gap, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2007, causal_inference_gap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_2012, causal_inference_gap, theater_ratio, 5, 0.48).
narrative_ontology:measurement(theater_2017, causal_inference_gap, theater_ratio, 10, 0.58).
narrative_ontology:measurement(theater_2022, causal_inference_gap, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(extract_2007, causal_inference_gap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(extract_2012, causal_inference_gap, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(extract_2017, causal_inference_gap, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(extract_2022, causal_inference_gap, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(causal_inference_gap, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of perturbation_epistemology (mountain — inherent difficulty of causal inference in non-perturbable complex systems) and methodological_lock_in (tangled_rope — career incentives and infrastructure investment in association-based approaches). The causal_inference_gap represents the specific instantiation of these upstream constraints in the domain of complex disease genetics. The gap's extractiveness (0.68) is higher than methodological_lock_in would predict alone, indicating that perturbation_epistemology contributes genuine epistemic difficulty on top of the institutional lock-in. However, the gap is not purely a mountain (as perturbation_epistemology might suggest) because functional genomics approaches are building alternative pathways, and the magnitude of the gap is contingent on resource allocation and methodological choices. The network decomposition separates the irreducible epistemic limit (perturbation_epistemology, ε=0.08) from the institutional arrangement (methodological_lock_in, ε=0.52) from the specific domain instantiation (causal_inference_gap, ε=0.68), allowing precise tracking of which component drives extraction in complex disease genetics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
