% ============================================================================
% CONSTRAINT STORY: clinical_need_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clinical_need_threshold, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: clinical_need_threshold
 *   human_readable: Clinical Need Threshold for Germline Genetic Modification
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The clinical need threshold for germline genetic modification (GGM)
 *   establishes criteria for when GGM is medically indicated: cases where
 *   preimplantation genetic diagnosis (PGD) cannot select an unaffected
 *   embryo (both parents homozygous for recessive disease alleles), where
 *   polygenic disease risk cannot be adequately reduced via PGD (which can
 *   select against single variants but not optimize across multiple loci), or
 *   where protective alleles that neither parent carries could prevent
 *   serious disease. This threshold operationalizes the
 *   therapeutic/enhancement distinction that regulatory frameworks depend on.
 *   The constraint coordinates access to GGM by distinguishing medical
 *   necessity from enhancement applications, solving a genuine collective
 *   action problem: without clear criteria, jurisdictions face pressure to
 *   permit enhancement applications, and prospective parents face ethical and
 *   legal ambiguity about when GGM is legitimate. The threshold's
 *   extractiveness is low (0.18) because it does not extract rents from those
 *   it governs — it establishes access criteria that benefit prospective
 *   parents with genuine medical need. Suppression is low (0.25) because
 *   alternatives (PGD, donor gametes, adoption) remain available for cases
 *   that do not meet the threshold. Theater ratio is low (0.15) because the
 *   threshold is operationalized through measurable criteria (genetic test
 *   results, polygenic risk scores, donor availability data) rather than
 *   performative review. The modest upward drift in all three metrics
 *   reflects increasing complexity as genomic research identifies more
 *   protective alleles and polygenic risk models improve, creating boundary
 *   cases where the therapeutic/enhancement distinction becomes harder to
 *   operationalize.
 *
 * KEY AGENTS:
 *   - Prospective Parents (Both Homozygous): Primary beneficiary (powerless/trapped) — the threshold legitimates their access to GGM when PGD cannot help
 *   - Families with Polygenic Disease Burden: Primary beneficiary (moderate/constrained) — the threshold legitimates polygenic risk reduction via GGM when PGD is inadequate
 *   - Rare Disease Communities: Primary beneficiary (organized/mobile) — the threshold protects their access claims without opening enhancement door
 *   - Clinical Genetics Institutions: Beneficiary (institutional/mobile) — the threshold provides clear practice boundaries and reduces liability risk
 *   - Regulatory Bodies: Beneficiary (institutional/arbitrage) — the threshold operationalizes the therapeutic/enhancement distinction that regulation depends on
 *   - Analytical Observer: Sees coordination function (analytical/analytical) — the threshold solves a real collective action problem with minimal extractive overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clinical_need_threshold, 0.18).
domain_priors:suppression_score(clinical_need_threshold, 0.25).
domain_priors:theater_ratio(clinical_need_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clinical_need_threshold, extractiveness, 0.18).
narrative_ontology:constraint_metric(clinical_need_threshold, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(clinical_need_threshold, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clinical_need_threshold, rope).
narrative_ontology:human_readable(clinical_need_threshold, "Clinical Need Threshold for Germline Genetic Modification").
narrative_ontology:topic_domain(clinical_need_threshold, "bioethics/reproductive_medicine/genetic_engineering").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clinical_need_threshold, prospective_parents_with_homozygous_conditions).
narrative_ontology:constraint_beneficiary(clinical_need_threshold, families_with_polygenic_disease_burden).
narrative_ontology:constraint_beneficiary(clinical_need_threshold, rare_disease_communities).
narrative_ontology:constraint_vindicates(clinical_need_threshold, medical_necessity_doctrine).
narrative_ontology:constraint_vindicates(clinical_need_threshold, therapeutic_enhancement_distinction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE PARENTS WITH HOMOZYGOUS CONDITIONS (ROPE) — Trapped by genetic reality (both parents carry identical disease alleles, PGD cannot select unaffected embryo) but experience the clinical need threshold as coordination: it establishes legitimate access to GGM when no alternative exists. The threshold protects their reproductive autonomy by distinguishing medical necessity from enhancement. Net beneficiary despite trapped exit — the constraint enables rather than extracts.
constraint_indexing:constraint_classification(clinical_need_threshold, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILIES WITH POLYGENIC DISEASE BURDEN (ROPE) — Constrained by the complexity of polygenic risk (PGD can select against single variants but cannot optimize across multiple loci; donor gametes may not carry protective alleles the family seeks). The clinical need threshold coordinates access to GGM for polygenic risk reduction that PGD cannot achieve. Moderate power through advocacy organizations; constrained exit because alternatives exist but are inadequate. Experience as coordination — the threshold legitimates their case.
constraint_indexing:constraint_classification(clinical_need_threshold, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLINICAL GENETICS INSTITUTIONS (ROPE) — Mobile exit (can decline to offer GGM services) but experience the threshold as coordination: it provides clear criteria for when GGM is medically indicated versus experimental or enhancement-oriented. The threshold reduces liability risk and ethical ambiguity. Institutional beneficiaries — the constraint enables legitimate practice boundaries.
constraint_indexing:constraint_classification(clinical_need_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: RARE DISEASE ADVOCACY ORGANIZATIONS (ROPE) — Organized agents with mobile exit (can advocate for alternative research priorities) but experience the threshold as coordination: it establishes that GGM is appropriate when disease burden is severe and alternatives are unavailable. The threshold legitimates their communities' access claims without opening the door to enhancement applications they may oppose. Net beneficiaries — the constraint protects their framing.
constraint_indexing:constraint_classification(clinical_need_threshold, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY BODIES (ROPE) — Arbitrage exit (can set different thresholds across jurisdictions) and experience the constraint as coordination: the clinical need threshold operationalizes the therapeutic/enhancement distinction that regulatory frameworks depend on. The threshold solves a genuine collective action problem — without it, jurisdictions face a race to the bottom on enhancement applications. Net beneficiaries — the constraint enables coherent regulation.
constraint_indexing:constraint_classification(clinical_need_threshold, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — The clinical need threshold coordinates access to GGM by distinguishing cases where no acceptable alternative exists (both parents homozygous for recessive disease, polygenic risk reduction unachievable via PGD, protective alleles unavailable in donor pool) from cases where PGD or donor gametes suffice. The threshold is a coordination mechanism with minimal extractive overhead: it solves the real problem of operationalizing medical necessity without suppressing alternatives or creating identifiable victims. Extractiveness is low because the constraint does not extract rents — it establishes access criteria. Suppression is low because alternatives (PGD, donor gametes, adoption) remain available for cases that do not meet the threshold.
constraint_indexing:constraint_classification(clinical_need_threshold, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clinical_need_threshold_tests).
:- end_tests(clinical_need_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The clinical need threshold does not extract rents from prospective parents or institutions — it establishes access criteria that benefit those with genuine medical need. The modest extractiveness reflects administrative overhead (genetic counseling, ethics review, documentation requirements) and the possibility that some families near the threshold boundary experience the criteria as arbitrary gatekeeping. The upward drift (0.12 → 0.18) reflects increasing complexity as genomic research expands the boundary cases. Suppression (0.25): Low. The threshold does not suppress alternatives — PGD, donor gametes, and adoption remain available for cases that do not meet the clinical need criteria. The modest suppression reflects that some prospective parents may experience the threshold as a barrier when they value genetic parenthood and their case is near the boundary. The upward drift (0.20 → 0.25) reflects that as GGM technology matures, the threshold increasingly functions as a gate rather than a coordination mechanism. Theater ratio (0.15): Low. The threshold is operationalized through measurable criteria: genetic test results showing both parents homozygous, polygenic risk scores quantifying disease burden, donor availability data for specific conditions. Review processes are functional rather than performative. The modest upward drift (0.10 → 0.15) reflects that as boundary cases increase, some ethics review becomes more about institutional liability management than substantive evaluation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all six perspectives classify as rope. The uniformity reflects that the clinical need threshold solves a genuine coordination problem (operationalizing medical necessity) without creating identifiable victims or asymmetric extraction. The modest extractiveness (0.18) and suppression (0.25) are experienced similarly across perspectives because they reflect administrative overhead and boundary ambiguity rather than structural extraction. The key structural feature is that the threshold is a gate that opens (enables access for those with medical need) rather than a gate that closes (extracts from or suppresses those it governs). Prospective parents with homozygous conditions are trapped by genetic reality but experience the threshold as enabling rather than extractive. Families with polygenic disease burden are constrained by PGD's limitations but experience the threshold as legitimating rather than suppressing. The analytical observer sees the same coordination function that the governed agents experience. The omega variables identify the boundary cases where the rope classification could degrade: if the polygenic threshold is set too high, families with substantial disease burden are excluded and the constraint becomes a snare from their perspective; if donor gametes are treated as always-acceptable alternatives, the threshold becomes extractive from the perspective of prospective parents who value genetic parenthood; if adding protective alleles is classified as enhancement rather than therapy, the therapeutic/enhancement distinction becomes arbitrary and the coordination function collapses.
 *
 * DIRECTIONALITY LOGIC:
 *   All six perspectives classify as rope because all agents are net beneficiaries of the coordination function. Prospective parents with homozygous conditions are trapped by genetic reality but experience the threshold as enabling — it legitimates their access to GGM when no alternative exists. Their directionality is low (near 0.0) because they are primary beneficiaries despite trapped exit. Families with polygenic disease burden are constrained by the inadequacy of PGD but experience the threshold as coordination — it legitimates polygenic risk reduction via GGM. Their directionality is low because they benefit from the access criteria. Clinical genetics institutions are mobile (can decline to offer GGM) but experience the threshold as coordination — it provides clear practice boundaries. Their directionality is low because they benefit from reduced liability risk. Rare disease advocacy organizations are organized with mobile exit but experience the threshold as coordination — it protects their communities' access claims. Their directionality is low because they benefit from the legitimation. Regulatory bodies have arbitrage exit (can set different thresholds across jurisdictions) and experience the threshold as coordination — it operationalizes the therapeutic/enhancement distinction. Their directionality is near 0.0 because they are primary beneficiaries. The analytical observer sees pure coordination with minimal extractive overhead. No victims are declared because no agent bears asymmetric costs — the threshold establishes access criteria that benefit those with medical need without extracting from others.
 *
 * MANDATROPHY ANALYSIS:
 *   The clinical need threshold resolves the mandatrophy by distinguishing coordination (establishing access criteria for medical necessity) from extraction (gatekeeping that benefits some at others' expense). The constraint is rope rather than snare because it does not create victims — prospective parents who do not meet the threshold have alternatives (PGD, donor gametes, adoption) and are not trapped by the constraint itself. The constraint is rope rather than tangled_rope because it does not exhibit asymmetric extraction — the administrative overhead and boundary ambiguity are experienced similarly by all governed agents, not concentrated on a victim class. The constraint is rope rather than scaffold because it does not carry a sunset — the need to distinguish medical necessity from enhancement is not temporary. The constraint is rope rather than piton because the threshold is operationalized through functional criteria (genetic test results, polygenic risk scores) rather than performative review. The constraint is rope rather than mountain because it is not a natural law — the specific criteria for clinical need are socially constructed and vary across jurisdictions. The analytical observer's rope classification matches the governed agents' experience, confirming that the coordination function is genuine rather than a cover story for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    polygenic_threshold_ambiguity,
    'What level of polygenic risk reduction justifies GGM when PGD can select against some but not all risk alleles?',
    'Quantitative modeling of polygenic risk reduction achievable via PGD versus GGM; cost-benefit analysis of incremental risk reduction; patient preference studies on acceptable risk thresholds',
    'If threshold is set too high: families with substantial but not maximal polygenic burden are excluded from GGM access, and the constraint becomes extractive (snare) from their perspective. If threshold is set too low: enhancement applications enter through the polygenic door, and the therapeutic/enhancement distinction collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polygenic_threshold_ambiguity, empirical, 'Polygenic risk threshold for GGM access').

omega_variable(
    donor_gamete_availability_assumption,
    'Does the clinical need threshold assume donor gametes are always an acceptable alternative, or does it recognize genetic parenthood as a legitimate interest?',
    'Ethical analysis of reproductive autonomy; empirical data on donor gamete acceptance rates across cultural contexts; legal precedents on genetic parenthood rights',
    'If donor gametes are treated as always-acceptable alternative: the threshold becomes extractive (snare) from the perspective of prospective parents who value genetic parenthood (see upstream constraint genetic_parenthood_valuation). If genetic parenthood is recognized as legitimate interest: the threshold expands to include cases where donor gametes are technically available but not acceptable to the prospective parents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(donor_gamete_availability_assumption, preference, 'Whether donor gametes count as acceptable alternative').

omega_variable(
    protective_allele_discovery_rate,
    'As genomic research identifies more protective alleles (e.g., APOE2 for Alzheimer''s, PCSK9 loss-of-function for cardiovascular disease), does the clinical need threshold expand to include adding protective alleles parents lack?',
    'Longitudinal tracking of protective allele discoveries; regulatory decisions on whether adding protective alleles counts as therapy or enhancement; public deliberation on acceptable scope of GGM',
    'If adding protective alleles is classified as therapy: the clinical need threshold expands substantially, and the constraint''s coordination function remains intact. If adding protective alleles is classified as enhancement: the threshold remains narrow, but the therapeutic/enhancement distinction becomes increasingly arbitrary as the line between correcting deficiency and adding protection blurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_allele_discovery_rate, conceptual, 'Whether adding protective alleles counts as meeting clinical need').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clinical_need_threshold, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clin_need_theater_t0, clinical_need_threshold, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clin_need_theater_t3, clinical_need_threshold, theater_ratio, 3, 0.12).
narrative_ontology:measurement(clin_need_theater_t6, clinical_need_threshold, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(clin_need_extract_t0, clinical_need_threshold, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(clin_need_extract_t3, clinical_need_threshold, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(clin_need_extract_t6, clinical_need_threshold, base_extractiveness, 6, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(clin_need_suppress_t0, clinical_need_threshold, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(clin_need_suppress_t3, clinical_need_threshold, suppression_requirement, 3, 0.23).
narrative_ontology:measurement(clin_need_suppress_t6, clinical_need_threshold, suppression_requirement, 6, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clinical_need_threshold, resource_allocation).

% DUAL FORMULATION NOTE:
% The clinical need threshold is downstream of genetic_parenthood_valuation: if genetic parenthood is recognized as a legitimate interest, the threshold expands to include cases where donor gametes are technically available but not acceptable to prospective parents. The two constraints are structurally linked but have distinct extractiveness values — genetic_parenthood_valuation reflects the cultural and legal status of genetic parenthood claims; clinical_need_threshold reflects the operationalization of medical necessity criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
