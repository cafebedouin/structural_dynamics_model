% ============================================================================
% CONSTRAINT STORY: clinical_translation_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clinical_translation_gap, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: clinical_translation_gap
 *   human_readable: Clinical Translation Gap in Germline Base Editing
 *   domain: biomedical_research/genome_editing/reproductive_medicine
 *
 * SUMMARY:
 *   Base editing technology eliminated the most severe genotoxic risks of
 *   germline genome editing — double-strand breaks, large deletions,
 *   chromosomal rearrangements — that made earlier CRISPR applications
 *   categorically unsafe for human reproduction. The technical advance was
 *   substantial: base editors chemically convert single nucleotides without
 *   cutting DNA, reducing off-target damage by orders of magnitude. Yet
 *   clinical translation remains blocked nearly a decade after the
 *   technology's development. The gap persists due to four unresolved
 *   technical problems (mosaicism variance, off-target effects in vivo,
 *   delivery toxicity, unknown long-term consequences) and two regulatory
 *   barriers (requirement for naturally occurring variants, institutional
 *   conservatism). This constraint exhibits tangled rope structure: genuine
 *   coordination (safety assessment is necessary) coexists with asymmetric
 *   extraction (regulatory delay exceeds evidence-based risk and preserves
 *   incumbent industry market share). The translation gap extracts from
 *   couples with monogenic disease risk who have limited embryos or face
 *   age-dependent fertility decline, while benefiting the PGT industry,
 *   academic labs sustained by research funding, and regulatory agencies
 *   avoiding political controversy. The constraint's theater ratio (0.38)
 *   reflects that much regulatory activity is substantive safety assessment,
 *   but a significant fraction is performative risk amplification and
 *   naturally-occurring-variant restriction that naturalizes genetic
 *   determinism.
 *
 * KEY AGENTS:
 *   - Couples with Monogenic Disease Risk: Primary victim (powerless/trapped) — face biological constraints and regulatory barriers; technical capability exists but remains inaccessible
 *   - Prospective Parents with Limited Embryos: Secondary victim (moderate/constrained) — partial access to PGT-M alternatives but forced into imperfect choices
 *   - Preimplantation Genetic Testing Industry: Primary beneficiary (institutional/arbitrage) — translation gap preserves PGT-M market share; can pivot to base editing when approved
 *   - Academic Genome Editing Labs: Secondary beneficiary (institutional/arbitrage) — extended research window sustains funding and careers
 *   - Regulatory Agencies: Mixed position (institutional/constrained) — coordinate legitimate safety assessment while extracting through conservatism exceeding evidence-based risk
 *   - Patient Advocacy Organizations: Organized agents (organized/mobile) — push for regulatory reform; see gap as temporary with clear sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies genuine coordination and genuine extraction coexisting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clinical_translation_gap, 0.52).
domain_priors:suppression_score(clinical_translation_gap, 0.68).
domain_priors:theater_ratio(clinical_translation_gap, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clinical_translation_gap, extractiveness, 0.52).
narrative_ontology:constraint_metric(clinical_translation_gap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(clinical_translation_gap, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(clinical_translation_gap, accessibility_collapse, 0.12).
narrative_ontology:constraint_metric(clinical_translation_gap, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clinical_translation_gap, tangled_rope).
narrative_ontology:human_readable(clinical_translation_gap, "Clinical Translation Gap in Germline Base Editing").
narrative_ontology:topic_domain(clinical_translation_gap, "biomedical_research/genome_editing/reproductive_medicine").

domain_priors:requires_active_enforcement(clinical_translation_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clinical_translation_gap, preimplantation_genetic_testing_industry).
narrative_ontology:constraint_beneficiary(clinical_translation_gap, academic_genome_editing_labs).
narrative_ontology:constraint_beneficiary(clinical_translation_gap, regulatory_agencies).
narrative_ontology:constraint_victim(clinical_translation_gap, couples_with_monogenic_disease_risk).
narrative_ontology:constraint_victim(clinical_translation_gap, prospective_parents_with_limited_embryos).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUPLES WITH MONOGENIC DISEASE RISK (SNARE) — Trapped by biological constraints (limited embryos, age-dependent fertility decline) and regulatory barriers. The technical capability exists but remains inaccessible. Base editing eliminates the severe genotoxic risks that made germline editing categorically unsafe, yet the translation gap persists through regulatory conservatism and industry capture. Maximum experienced extraction: the constraint extracts reproductive autonomy and forces acceptance of disease transmission or childlessness.
constraint_indexing:constraint_classification(clinical_translation_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROSPECTIVE PARENTS WITH LIMITED EMBRYOS (TANGLED ROPE) — Constrained by the same regulatory barriers but with partial access to coordination benefits: PGT-M screening exists as an alternative, though it requires multiple IVF cycles and may fail when all embryos carry the variant. The constraint coordinates access to genetic screening while extracting through the forced choice between imperfect alternatives. Mixed experience: some benefit from the existing testing infrastructure, substantial cost from the translation delay.
constraint_indexing:constraint_classification(clinical_translation_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PGT INDUSTRY (ROPE) — Primary beneficiary. The translation gap preserves the PGT-M market: as long as base editing remains clinically unavailable, couples must use embryo screening. The constraint coordinates a genuine service (genetic testing) while the regulatory delay protects market position. Arbitrage exit: industry actors can pivot to base editing services when regulatory approval arrives, capturing both markets sequentially. Net beneficiary with low experienced extraction.
constraint_indexing:constraint_classification(clinical_translation_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC GENOME EDITING LABS (ROPE) — Benefit from the extended research window. The translation gap sustains funding for mechanistic studies, off-target characterization, and delivery optimization. The constraint coordinates legitimate scientific investigation while the delay provides career stability for researchers. Arbitrage exit: labs can transition to clinical partnership when translation occurs. Low experienced extraction.
constraint_indexing:constraint_classification(clinical_translation_gap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PATIENT ADVOCACY ORGANIZATIONS (SCAFFOLD) — Organized agents pushing for regulatory reform and expanded access. See the translation gap as a temporary coordination failure with a clear sunset: as long-term safety data accumulates and regulatory frameworks adapt, the gap will close. The constraint coordinates safety assessment (legitimate function) while the delay is transitional. Mobile exit: advocacy groups can shift focus to implementation equity once approval is achieved.
constraint_indexing:constraint_classification(clinical_translation_gap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGULATORY AGENCIES (TANGLED ROPE) — Constrained by institutional mandate (protect public health) and political pressure (avoid germline editing controversy). The constraint coordinates genuine safety assessment while extracting through regulatory conservatism that exceeds evidence-based risk. Mixed experience: agencies benefit from avoiding catastrophic approval errors but bear reputational cost when delays are perceived as captured or ideological. Constrained exit: cannot easily revise standards without political authorization.
constraint_indexing:constraint_classification(clinical_translation_gap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The translation gap exhibits genuine coordination (safety assessment, long-term monitoring, regulatory framework development) and genuine extraction (industry capture of regulatory delay, reproductive autonomy constrained beyond evidence-based risk). The constraint is not a false summit: the unsolved technical problems (mosaicism variance, off-target rates in vivo, unknown long-term effects) are real, but the regulatory response amplifies them through institutional inertia and industry influence. The naturally-occurring-variant restriction is a constructed barrier, not a technical necessity.
constraint_indexing:constraint_classification(clinical_translation_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clinical_translation_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(clinical_translation_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(clinical_translation_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(clinical_translation_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(clinical_translation_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts reproductive autonomy from couples with monogenic disease risk and limited embryos, forcing acceptance of disease transmission, childlessness, or repeated IVF cycles with uncertain outcomes. The extraction is substantial but not maximal because PGT-M provides a partial alternative for some couples. The increasing trajectory (0.38 → 0.52 over 9 years) reflects accumulating evidence that base editing's safety profile is acceptable while regulatory barriers persist, widening the gap between technical capability and clinical access. Suppression (0.68): High. Regulatory barriers are severe: germline editing is prohibited in most jurisdictions, clinical trials require extensive preclinical data, and the naturally-occurring-variant restriction eliminates most therapeutic applications. Biological constraints (limited embryos, age-dependent fertility decline) compound regulatory suppression. The increasing trajectory (0.55 → 0.68) reflects regulatory frameworks hardening as political controversy intensifies, despite improving safety data. Theater ratio (0.38): Moderate. Much regulatory activity is substantive: long-term safety monitoring, off-target characterization, and mosaicism studies address real unknowns. However, significant performative content exists: the naturally-occurring-variant restriction has weak safety justification and functions primarily as ideological boundary maintenance; repeated calls for 'more data' without specifying evidentiary thresholds; and regulatory agency risk amplification that exceeds evidence-based assessment. The increasing trajectory (0.25 → 0.38) reflects growing performative content as technical problems are progressively solved but regulatory timelines do not adjust proportionally.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across structural positions. Couples with monogenic disease risk experience pure extraction (Snare): the technical capability exists but remains inaccessible through regulatory barriers that exceed evidence-based risk. Prospective parents with limited embryos experience mixed coordination and extraction (Tangled Rope): PGT-M provides partial benefit while the translation gap forces imperfect choices. The PGT industry experiences coordination (Rope): the constraint preserves their market while coordinating genetic screening services. Academic labs experience coordination (Rope): the research window sustains legitimate scientific investigation. Patient advocacy organizations see a temporary problem with sunset logic (Scaffold): as safety data accumulates and regulatory frameworks adapt, the gap will close. Regulatory agencies experience mixed coordination and extraction (Tangled Rope): they coordinate genuine safety assessment while extracting through conservatism. The analytical observer identifies the constraint as Tangled Rope at the civilizational scale: genuine coordination (safety assessment is necessary given unsolved technical problems) coexists with genuine extraction (regulatory delay exceeds evidence-based risk and preserves incumbent industry position). The perspectival gap is not a measurement error — it reflects real differences in structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Couples with monogenic disease risk are full victims with trapped exit options, yielding maximum directionality (d ≈ 1.0) and maximum experienced extraction. They cannot exit the constraint: biological fertility windows are fixed, alternative reproductive options are limited, and regulatory barriers are jurisdictionally uniform. Prospective parents with limited embryos are victims with constrained exit (d ≈ 0.7): PGT-M provides partial access to genetic screening, reducing but not eliminating extraction. The PGT industry is the primary beneficiary with arbitrage exit (d ≈ 0.1): the translation gap directly preserves their market, and they can pivot to base editing services when regulatory approval arrives, capturing both markets sequentially. Academic genome editing labs are secondary beneficiaries with arbitrage exit (d ≈ 0.2): the extended research window sustains funding, and they can transition to clinical partnerships when translation occurs. Regulatory agencies occupy a mixed position (d ≈ 0.5): they coordinate legitimate safety assessment (beneficiary aspect) while bearing reputational cost from delays perceived as captured or ideological (victim aspect). Their constrained exit reflects institutional mandate and political pressure. Patient advocacy organizations have mobile exit (d ≈ 0.3): they can shift focus to implementation equity once approval is achieved, and their organized power reduces experienced extraction. The analytical observer's directionality is neutral (d = 0.5) by definition, yielding the base extractiveness value without amplification or damping.
 *
 * MANDATROPHY ANALYSIS:
 *   The clinical translation gap resolves mandatrophy by demonstrating that tangled rope classification is stable across multiple perspectives when genuine coordination and genuine asymmetric extraction coexist. The constraint is not mislabeled coordination (it extracts substantially from trapped victims) and not mislabeled pure extraction (it coordinates legitimate safety assessment). The naturally-occurring-variant restriction is the clearest extraction signal: it has weak safety justification (synthetic variants with equivalent pathogenicity profiles are excluded) and functions primarily to naturalize genetic determinism, limiting reproductive autonomy through ideological boundary maintenance rather than evidence-based risk assessment. The regulatory conservatism that exceeds proportional response to unsolved technical problems (mosaicism, off-target variance) is the second extraction mechanism: industry influence and institutional inertia amplify risk beyond evidence, preserving PGT market share. Yet the coordination function is genuine: base editing does have unsolved technical problems, long-term effects are unknown, and regulatory frameworks for germline modification require careful development. The constraint exhibits the tangled rope signature: beneficiaries declared (PGT industry, academic labs, regulatory agencies), victims declared (couples with monogenic disease risk, prospective parents with limited embryos), active enforcement required (regulatory prohibition, clinical trial restrictions), and the analytical perspective confirms mixed structure rather than resolving to pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mosaicism_resolution_timeline,
    'What timeline is required to resolve mosaicism variance to clinically acceptable levels, and is the current regulatory delay proportional to that timeline?',
    'Longitudinal embryo editing studies with single-cell sequencing; comparison of mosaicism rates across delivery methods and developmental stages; regulatory risk-benefit modeling',
    'If mosaicism can be resolved within 5 years: current delay is extractive regulatory conservatism. If resolution requires 15+ years: delay is proportional safety assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mosaicism_resolution_timeline, empirical, 'Timeline for mosaicism resolution vs regulatory proportionality').

omega_variable(
    naturally_occurring_variant_restriction,
    'Is the regulatory requirement that edits must match naturally occurring variants a legitimate safety constraint or a constructed barrier that naturalizes genetic determinism?',
    'Comparative analysis of variant pathogenicity: naturally occurring vs synthetic; examination of regulatory justification documents; cross-jurisdictional policy comparison',
    'If naturally occurring variants are demonstrably safer: restriction is coordination. If safety profiles are equivalent: restriction is extraction mechanism that limits reproductive autonomy through naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturally_occurring_variant_restriction, conceptual, 'Whether naturally-occurring-variant restriction is safety-based or ideological').

omega_variable(
    pgt_industry_regulatory_capture,
    'To what extent do PGT industry actors influence regulatory timelines through advisory board membership, funding of safety studies, or lobbying?',
    'Financial disclosure analysis; regulatory advisory board composition; industry-funded research publication patterns; comparative approval timelines across jurisdictions with different industry influence',
    'If industry influence is negligible: translation gap is legitimate safety coordination. If industry influence is substantial: translation gap is partially extractive, preserving PGT market share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pgt_industry_regulatory_capture, empirical, 'Degree of PGT industry capture of regulatory process').

omega_variable(
    off_target_clinical_significance,
    'Are the remaining off-target effects of base editing clinically significant at population scale, or are they within the background mutation rate and therefore negligible?',
    'Whole-genome sequencing of base-edited embryos vs controls; population genetics modeling of mutation load; long-term health outcome tracking in model organisms',
    'If off-target effects exceed background: regulatory caution is warranted coordination. If off-target effects are within background: regulatory delay is extractive risk amplification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(off_target_clinical_significance, empirical, 'Clinical significance of base editing off-target effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clinical_translation_gap, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_initial, clinical_translation_gap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clin_trans_tr_t3, clinical_translation_gap, theater_ratio, 3, 0.3).
narrative_ontology:measurement(clin_trans_tr_t6, clinical_translation_gap, theater_ratio, 6, 0.35).
narrative_ontology:measurement(clin_trans_tr_t9, clinical_translation_gap, theater_ratio, 9, 0.38).

% Extraction over time
narrative_ontology:measurement(extract_initial, clinical_translation_gap, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clin_trans_be_t3, clinical_translation_gap, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(clin_trans_be_t6, clinical_translation_gap, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(clin_trans_be_t9, clinical_translation_gap, base_extractiveness, 9, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppress_initial, clinical_translation_gap, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clin_trans_su_t3, clinical_translation_gap, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(clin_trans_su_t6, clinical_translation_gap, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(clin_trans_su_t9, clinical_translation_gap, suppression_requirement, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clinical_translation_gap, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The clinical translation gap is downstream of four technical constraints (dna_repair_substrate_difference, delivery_modality_toxicity, guide_rna_off_target_variance, genetic_mosaicism_timing) but represents a distinct regulatory and institutional constraint. The upstream constraints have their own extractiveness values reflecting the technical barriers; the translation gap has its own extractiveness reflecting regulatory conservatism, industry capture, and ideological boundary maintenance that exceed evidence-based risk assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(clinical_translation_gap, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
