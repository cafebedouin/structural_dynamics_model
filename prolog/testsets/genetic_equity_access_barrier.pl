% ============================================================================
% CONSTRAINT STORY: genetic_equity_access_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_equity_access_barrier, []).

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
 *   constraint_id: genetic_equity_access_barrier
 *   human_readable: Genetic Equity Access Barrier in Precision Medicine
 *   domain: healthcare/biomedics/social_equity
 *
 * SUMMARY:
 *   The genetic equity access barrier emerges from the structural
 *   concentration of sequenced human genomes in high-income populations with
 *   predominantly European ancestry. This creates a feedback loop: genetic
 *   reference databases are 80%+ European ancestry, clinical decision support
 *   is calibrated to European variant frequencies, and research funding
 *   concentrates studies in high-income institutions. Patients and healthcare
 *   systems serving underrepresented populations face cascading barriers:
 *   unaffordable testing costs, poor prediction accuracy on their genetic
 *   variants (often missing rare variants specific to their ancestry), and
 *   clinical tools validated only on European genotypes. The constraint
 *   exhibits all six DR types from different perspectives. From the
 *   low-income patient's view, it is a snare: trapped by cost and knowledge
 *   exclusion. From the research institution's view, it is a rope: they are
 *   solving a genuine coordination problem (establishing variant standards).
 *   From the equity coalition's view, it is a scaffold: decentralized
 *   genomics projects with sunset logic (building population-specific
 *   databases). From the clinical guidelines board's view, it is a piton:
 *   rhetorical equity commitments without material resource reallocation.
 *   From the analytical civilizational view, it risks appearing as a
 *   mountain: sample size effects and rare variant discovery are presented as
 *   natural laws of genetics. But the structural data reveals this as false
 *   naturalization — the distribution of sequencing investment is a policy
 *   choice, not a law of nature.
 *
 * KEY AGENTS:
 *   - Low-income patients with non-European ancestry: Primary victims (powerless/trapped) — denied precision medicine options due to cost and prediction inaccuracy; forced into generic protocols
 *   - Public health systems in middle-income and low-income countries: Secondary victims (moderate/constrained) — face licensing costs and infrastructure dependencies; benefit from genetic coordination while bearing extraction costs
 *   - Genetic testing companies and reference labs: Primary beneficiaries (institutional/arbitrage) — control access to well-curated European ancestry data; set pricing and licensing terms; can exit or renegotiate
 *   - High-income ancestry groups (predominantly European): Secondary beneficiaries (powerful/mobile) — benefit from precision medicine, clinical trials designed on their genetic data, treatment options optimized for their variants
 *   - Clinical genetics guidelines boards: Institutional actors (institutional/arbitrage) — maintain performative equity language while anchoring to European-derived standards; persist through inertia
 *   - Global genetic equity coalition: Organized agents (organized/constrained) — building decentralized multi-ancestry databases; see exit path through data infrastructure investment
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent research funding patterns as inherent scientific limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_equity_access_barrier, 0.58).
domain_priors:suppression_score(genetic_equity_access_barrier, 0.72).
domain_priors:theater_ratio(genetic_equity_access_barrier, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_equity_access_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(genetic_equity_access_barrier, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(genetic_equity_access_barrier, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_equity_access_barrier, tangled_rope).
narrative_ontology:human_readable(genetic_equity_access_barrier, "Genetic Equity Access Barrier in Precision Medicine").
narrative_ontology:topic_domain(genetic_equity_access_barrier, "healthcare/biomedics/social_equity").

domain_priors:requires_active_enforcement(genetic_equity_access_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_equity_access_barrier, high_income_ancestry_groups).
narrative_ontology:constraint_beneficiary(genetic_equity_access_barrier, genetic_testing_companies).
narrative_ontology:constraint_beneficiary(genetic_equity_access_barrier, research_institutions_with_funding).
narrative_ontology:constraint_victim(genetic_equity_access_barrier, underrepresented_populations).
narrative_ontology:constraint_victim(genetic_equity_access_barrier, health_equity_outcomes).
narrative_ontology:constraint_victim(genetic_equity_access_barrier, global_south_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME PATIENT WITH NON-EUROPEAN ANCESTRY (SNARE) — Trapped by economic barriers (testing costs $500-5000), genetic reference databases 80% European ancestry (rendering prediction accuracy <50% for African/Asian/Indigenous genomes), and clinical skepticism about non-European genetic variants. No exit mechanism. Bears full extraction cost: denied precision treatment options, excluded from clinical trials, forced into generic medicine protocols. Maximum experienced suppression — cannot afford testing, cannot access knowledge, cannot exit the constraint.
constraint_indexing:constraint_classification(genetic_equity_access_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEM IN MIDDLE-INCOME COUNTRY (TANGLED ROPE) — Experiences genuine coordination function (genetic databases enable rare disease diagnosis, improving outcomes for connected patients) alongside asymmetric extraction (licensing costs for validated panels exceed annual genomics budget; data infrastructure concentrated in high-income institutions). Constrained by resource dependencies and standards set externally. Mixed: benefits from genetic knowledge coordination while bearing extraction costs of data colonialism and technology access monopolies.
constraint_indexing:constraint_classification(genetic_equity_access_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GENETIC TESTING COMPANY (ROPE) — Experiences the constraint primarily as a coordination problem they help solve: establishing standards, curating variant databases, training clinicians. Benefits from their institutional position (monopolistic access to well-curated European ancestry data). Can arbitrage: negotiate licensing, set pricing, control data access. Net beneficiary. Sees the constraint as coordination that they enable and that benefits patients — framing that naturalizes their extraction position.
constraint_indexing:constraint_classification(genetic_equity_access_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLINICAL GENETICS GUIDELINES BOARD (PITON) — Maintains performative equity language (boards include diversity statements, include underrepresented populations in recommendations) while actual practice remains oriented to European reference data. Guidelines declare aspirational inclusion but lack enforcement mechanisms or funding redirects. Theater ratio reflects: guidelines published, diversity committees formed, but material resource allocation to population-specific variant databases remains minimal. Function is degraded — the board's process no longer meaningfully improves outcomes for non-European populations; it persists through institutional inertia.
constraint_indexing:constraint_classification(genetic_equity_access_barrier, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL GENETIC EQUITY COALITION (SCAFFOLD) — Organized agents (All of Us Research Program, African Genome Variation Project, Trans-Omics for Precision Medicine in Indigenous populations) building decentralized genetic reference databases with explicit sunset logic: as population-specific variant databases mature and integrate into clinical tools, the access barrier from European-only reference data decreases. Sees the constraint as a temporary coordination failure being solved through data infrastructure investment. Low effective extraction because coalition has agency and sees exit path — estimated sunset: 10-15 years for integrated multi-ancestry databases to reach clinical utility equivalence.
constraint_indexing:constraint_classification(genetic_equity_access_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, variant annotation accuracy necessarily depends on reference population size. Rare variant interpretation requires statistical power that naturally concentrates in high-income/large-population genetic studies. This perspective sees the access barrier as an immutable consequence of how genetic knowledge works: underrepresented populations structurally have fewer sequenced genomes, fewer rare variants documented, weaker statistical basis for clinical interpretation. This is presented as a natural law of genetics — 'sample size effects are inherent.' But the structural data contradicts the mountain classification — the engine will identify this as false naturalization of a policy choice: the distribution of sequencing investment is contingent (decisions about where to fund studies, whose populations to include), not inherent.
constraint_indexing:constraint_classification(genetic_equity_access_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_equity_access_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_equity_access_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_equity_access_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genetic_equity_access_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genetic_equity_access_barrier, TR),
    TR >= 0.70.

:- end_tests(genetic_equity_access_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The barrier combines three extraction mechanisms: (1) testing cost ($500-5000) excludes low-income patients; (2) prediction inaccuracy on non-European variants reduces clinical utility for 50%+ of global population; (3) research design bias (studies powered only for European variants) perpetuates the gap. Extractiveness has increased from 0.42 to 0.58 over the interval as precision medicine has become a therapeutic category — the gap between those who can access it and those excluded has widened. Suppression (0.72): High. Multiple barriers reinforce the constraint: economic (testing unaffordable), knowledge (reference databases don't contain your variants), institutional (clinical protocols skeptical of non-European variant interpretations), and epistemic (genetic variation itself is defined through European-derived frameworks). Suppression persists because each barrier alone might be surmountable, but combined they create a lock-in. Theater ratio (0.45): Moderate-low. The constraint's performative content is lower than some; equity language is present (diversity statements, inclusion initiatives) but material resource allocation remains concentrated. Theater is not the primary mechanism — extraction operates through genuine coordination problems (variant databases do require standards) combined with asymmetric access. The theater that does exist (equity commitments without funding) functions as a suppression mechanism: mutes pressure for reform by creating false impression of progress.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (powerless patient) sees pure extraction with no coordination benefit — genetic knowledge exists but is priced and designed for others. The rope perspective (testing company) sees genuine coordination: they enable variant classification, establish standards, improve outcomes for connected patients. The scaffold perspective (equity coalition) sees a temporary problem with a real solution path — building population-specific databases reduces extraction within a 10-15 year horizon. The piton perspective (guidelines board) sees a degraded process — equity commitments without enforcement, performative inclusion without material change. The mountain perspective (analytical civilizational) sees an immutable limit — rare variant interpretation requires large sample sizes, which naturally concentrate in high-income populations. The gap between snare and rope is about experienced vs actual extraction: the company genuinely solves a coordination problem AND extracts asymmetrically. The gap between scaffold and mountain is about whether the constraint is policy-contingent (fixable) or scientifically inherent (not fixable). The structural data supports the scaffold reading: as non-European sample sizes increase, prediction accuracy should equalize — this would be impossible if the barrier were a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. Low-income patients bear the full cost (d ≈ 0.95, high f(d)) with no exit — trapped. High-income groups benefit (d ≈ 0.15, low f(d)) with multiple exits — mobile or arbitrage. Testing companies benefit with arbitrage options (d ≈ 0.20, low f(d)). Public health systems in middle-income countries are partially captured — they benefit from genetic coordination but bear extraction costs (d ≈ 0.60, mid f(d)). The equity coalition has real agency and exit paths (d ≈ 0.40, moderate f(d)). Clinical guidelines boards maintain beneficiary status without high d because they have institutional insulation and arbitrage (they can shift focus without material loss). The directionality computation reveals why the constraint persists: those most harmed have zero options; those benefiting have multiple exits.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy through structural decomposition. The constraint is NOT 'genetic access is a snare or a rope depending on perspective.' The constraint is a TANGLED ROPE: it performs genuine coordination (establishing variant databases, standardizing interpretation) AND it extracts asymmetrically (concentrating benefits to high-income ancestry groups). The mandatrophy is resolved by recognizing that all six perspectival readings are correct — they are not alternative interpretations of a single underlying fact, but rather different agents' actual experiences of the same structural phenomenon. The powerless patient actually experiences a snare (extraction with no coordination benefit for them). The testing company actually experiences a rope (their coordination function is genuine). The equity coalition actually experiences a scaffold (they have agency and exit paths). The guidelines board actually experiences a piton (performative process). The mountain perspective is a false naturalization of a policy choice — the engine's false summit detector flags this as naturalization of contingency. The analytical observer's challenge is to recognize that the constraint's type varies by perspective because agents occupy different structural positions, not because the constraint is ambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sample_size_equity_threshold,
    'At what sample size threshold does variant prediction accuracy equalize across ancestry groups?',
    'Empirical: compare current European N~500k to non-European N~50k-100k; model prediction accuracy curves as non-European N approaches 500k; track ongoing cohort growth rates',
    'If threshold reachable in 10 years: scaffold perspective confirmed, sunset is real. If threshold requires 50+ years or is asymptotic: access barrier is structural (snare persists), equity becomes intractable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sample_size_equity_threshold, empirical, 'Sample size threshold for ancestry-equitable variant prediction accuracy').

omega_variable(
    licensing_and_data_sovereignty_constraints,
    'Are licensing costs and data-sharing restrictions (patents, export controls) structural barriers or symptoms of upstream sequencing equity failure?',
    'Policy analysis: track cost reductions as open-source variant databases (gnomAD, ClinVar) mature; correlate countries'' data-access delays with licensing restrictions vs genomics infrastructure gaps',
    'If licensing is primary barrier: policy reform (open licensing) can reduce extraction in 2-3 years. If infrastructure is primary barrier: cost reduction requires 10-15 year investment cycle. Different barriers drive different constraint types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_and_data_sovereignty_constraints, empirical, 'Relative impact of licensing vs infrastructure equity barriers').

omega_variable(
    clinical_utility_lag_for_non_european_variants,
    'Do non-European ancestry variants show measurably lower clinical validity for treatment recommendations, or is the disparity driven by lower genetic study diversity (sampling bias) rather than true biological difference?',
    'Meta-analysis: compare clinical validity effect sizes (odds ratios, explained variance) for same genes across ancestry-stratified cohorts; control for sample size and experimental design quality',
    'If true biological difference exists: access barrier is partly insurmountable (mountain for some specific claims). If disparity is sampling bias: barrier is policy/equity choice (snare with fixable extraction). Classification depends on root cause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clinical_utility_lag_for_non_european_variants, empirical, 'Whether ancestry-based variant clinical utility disparities reflect biology or sampling bias').

omega_variable(
    identity_lock_in_research_epistemology,
    'Do genetic researchers in high-income institutions perceive non-European genetic diversity as scientifically inferior (identity-locked to European reference paradigm) or merely acknowledge economic constraints on data collection?',
    'Discourse analysis: publications, grant reviews, editorial decisions; survey of researcher perceptions of non-European variant databases; correlation between institution wealth and willingness to invest in population-specific research',
    'If identity-locked: researchers would resist non-European data even if funding were available (cognitive barrier). If constrained: removing financial barriers would shift research focus rapidly (structural barrier). Affects whether scaffold perspective is achievable or if epistemic capture must be overcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_research_epistemology, conceptual, 'Whether genetic research epistemology is identity-locked to European reference paradigm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_equity_access_barrier, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneq_tr_t0, genetic_equity_access_barrier, theater_ratio, 0, 0.38).
narrative_ontology:measurement(geneq_tr_t5, genetic_equity_access_barrier, theater_ratio, 5, 0.42).
narrative_ontology:measurement(geneq_tr_t10, genetic_equity_access_barrier, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(geneq_be_t0, genetic_equity_access_barrier, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(geneq_be_t5, genetic_equity_access_barrier, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(geneq_be_t10, genetic_equity_access_barrier, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_equity_access_barrier, information_standard).
narrative_ontology:boltzmann_floor_override(genetic_equity_access_barrier, 0.12).
narrative_ontology:affects_constraint(genetic_equity_access_barrier, precision_medicine_access_disparity).
narrative_ontology:affects_constraint(genetic_equity_access_barrier, rare_disease_diagnosis_equity).
narrative_ontology:affects_constraint(genetic_equity_access_barrier, clinical_trial_representation_bias).
narrative_ontology:affects_constraint(genetic_equity_access_barrier, genomic_data_colonialism).

% DUAL FORMULATION NOTE:
% The genetic equity access barrier is downstream of research funding allocation patterns and upstream of clinical outcomes disparities. Separate constraint stories should address: (1) research funding concentration (why European populations are oversequenced), (2) licensing and data-sharing restrictions (separate from sequencing equity), and (3) clinical trial representation bias (separate but coupled). Each has distinct epsilon; linked via network affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genetic_equity_access_barrier, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
