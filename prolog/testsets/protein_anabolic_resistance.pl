% ============================================================================
% CONSTRAINT STORY: protein_anabolic_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_protein_anabolic_resistance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: protein_anabolic_resistance
 *   human_readable: Age-Related Anabolic Resistance to Dietary Protein
 *   domain: biomedical/aging_biology/nutritional_biochemistry
 *
 * SUMMARY:
 *   Aging muscle tissue exhibits reduced sensitivity to dietary protein
 *   stimulus, requiring higher per-meal leucine doses (~2.5-3g, typically
 *   30-40g total protein) to activate mTOR signaling and achieve protein
 *   synthesis rates equivalent to younger muscle on lower doses. This
 *   constraint is measured via stable isotope tracer methodology and is
 *   documented across independent research groups. Commercial actors benefit
 *   from the constraint's existence by marketing optimized protein products
 *   to older adults, but they did not construct the underlying biology. The
 *   constraint is claimed as mountain (a discovered biological limit) while
 *   carrying identifiable beneficiaries, triggering FSM evaluation.
 *
 * KEY AGENTS:
 *   - aging_adults: Primary targets (powerless/trapped) — bear the constraint's nutritional requirement regardless of intervention
 *   - protein_supplement_manufacturers: Beneficiaries (organized/mobile) — market premium products addressing the constraint
 *   - sports_nutrition_industry: Beneficiaries (organized/mobile) — expand market into aging population
 *   - geriatric_nutrition_consultants: Beneficiaries (moderate/mobile) — provide optimization services
 *   - muscle_physiology_researchers: Analytical observers (institutional/analytical) — characterize the constraint
 *   - public_health_nutritionists: Analytical observers (institutional/analytical) — translate research to guidelines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(protein_anabolic_resistance, 0.08).
domain_priors:suppression_score(protein_anabolic_resistance, 0.03).
domain_priors:theater_ratio(protein_anabolic_resistance, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(protein_anabolic_resistance, extractiveness, 0.08).
narrative_ontology:constraint_metric(protein_anabolic_resistance, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(protein_anabolic_resistance, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(protein_anabolic_resistance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(protein_anabolic_resistance, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(protein_anabolic_resistance, mountain).
narrative_ontology:human_readable(protein_anabolic_resistance, "Age-Related Anabolic Resistance to Dietary Protein").
narrative_ontology:topic_domain(protein_anabolic_resistance, "biomedical/aging_biology/nutritional_biochemistry").

domain_priors:emerges_naturally(protein_anabolic_resistance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(protein_anabolic_resistance, protein_supplement_manufacturers).
narrative_ontology:constraint_beneficiary(protein_anabolic_resistance, sports_nutrition_industry).
narrative_ontology:constraint_beneficiary(protein_anabolic_resistance, geriatric_nutrition_consultants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(protein_anabolic_resistance, aging_adults).
narrative_ontology:constraint_vindicates(protein_anabolic_resistance, mtor_leucine_threshold_hypothesis).
narrative_ontology:constraint_vindicates(protein_anabolic_resistance, protein_distribution_optimization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience declining muscle protein synthesis efficiency with age regardless of intervention. Must consume higher per-meal protein doses (~30-40g vs 20-25g in youth) to achieve equivalent anabolic response. The constraint operates through cellular signaling machinery degradation; no dietary pattern fully restores youthful efficiency. Exit means accepting accelerated sarcopenia.
narrative_ontology:constraint_stakeholder(protein_anabolic_resistance, aging_adults, payer,
    powerless, biographical, trapped, universal).

% Market products specifically formulated for older adults at premium pricing, citing the leucine threshold research. The constraint creates sustained demand for concentrated protein sources and leucine-enriched formulations. They fund some of the research establishing dosing thresholds but do not control the underlying biology.
narrative_ontology:constraint_stakeholder(protein_anabolic_resistance, protein_supplement_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Expand market beyond athletes into aging population by positioning high-protein products as medical nutrition. The constraint's documentation in peer-reviewed literature provides scientific legitimacy for marketing claims. They benefit from the constraint's existence but did not construct it.
narrative_ontology:constraint_stakeholder(protein_anabolic_resistance, sports_nutrition_industry, beneficiary,
    organized, biographical, mobile, global).

% Provide professional services optimizing protein intake timing and dosing for older clients. The constraint creates a technical optimization problem requiring specialized knowledge. Their expertise derives from the constraint's complexity, but the constraint would persist without their practice.
narrative_ontology:constraint_stakeholder(protein_anabolic_resistance, geriatric_nutrition_consultants, beneficiary,
    moderate, biographical, mobile, national).

% Measure the constraint via stable isotope tracer studies, document leucine thresholds, and investigate upstream mechanisms (mitochondrial signaling, ribosomal capacity, insulin sensitivity). They characterize the constraint but do not benefit materially from its existence; their funding comes from aging biology grants, not industry.
narrative_ontology:constraint_stakeholder(protein_anabolic_resistance, muscle_physiology_researchers, observer,
    institutional, generational, analytical, global).

% Translate the research into dietary guidelines for older populations. They must balance the constraint's reality against practical barriers (cost, access, cultural food patterns). They see both the biological necessity and the socioeconomic inequity it creates.
narrative_ontology:constraint_stakeholder(protein_anabolic_resistance, public_health_nutritionists, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is a biological constraint, not a coordination mechanism. The constraint describes how aging muscle tissue responds to dietary protein stimulus.
% TRANSFER_FUNCTION: No intentional transfer. The constraint imposes higher nutritional requirements on older adults; commercial actors capture value by meeting that requirement, but the requirement itself is not constructed to enable the capture.
% ABSENT_VOICES: Low-income older adults who cannot afford optimized protein intake are structurally excluded from the discourse. The research literature and commercial products assume purchasing power; the constraint's biological reality is universal but the mitigation strategies are economically gated.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — if aging muscle suddenly regained youthful anabolic sensitivity — older adults would maintain muscle mass on lower protein intake, supplement demand would collapse, and geriatric nutrition would simplify. But the biological processes (mitochondrial decline, ribosomal dysfunction, insulin resistance) that produce the constraint are intrinsic to mammalian aging. The constraint is a measurement of those processes, not a constructed rule.
% FOUNDING_PROBLEM: Not applicable — this is a discovered biological phenomenon, not a designed solution to a problem. The constraint was characterized through isotope tracer studies in the 1990s-2000s as researchers investigated why older adults lose muscle mass despite adequate total protein intake.
% FOUNDING_PROBLEM_CORROBORATION: The constraint is documented across independent research groups in multiple countries using standardized stable isotope methodology. Muscle biopsy studies, leucine kinetics, and mTOR phosphorylation assays all converge on the same leucine threshold shift. No party constructed this constraint; it was discovered.
narrative_ontology:disappearance_verdict(protein_anabolic_resistance, world_unchanged).
narrative_ontology:founding_problem_status(protein_anabolic_resistance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(protein_anabolic_resistance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(protein_anabolic_resistance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(protein_anabolic_resistance_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(protein_anabolic_resistance, ExtMetricName, E),
    domain_priors:suppression_score(protein_anabolic_resistance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(protein_anabolic_resistance),
    narrative_ontology:constraint_metric(protein_anabolic_resistance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(protein_anabolic_resistance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(protein_anabolic_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because the constraint is a biological phenomenon, not a constructed extraction mechanism. The modest extraction that exists comes from commercial actors capturing value by meeting a real biological need, not from the constraint itself. Suppression is near-zero (0.03) because no party enforces the constraint; it operates through cellular machinery degradation. Theater is negligible (0.02) because the research characterizing the constraint is genuine stable-isotope physiology, not performance. Accessibility collapse is very high (0.92) because once the constraint is understood, no dietary pattern fully restores youthful anabolic efficiency — the alternatives (lower protein intake, different timing) simply fail. Resistance is very low (0.04) because the constraint is a measurement of biological reality; disputing it means disputing isotope tracer data. The slight upward drift in extractiveness over the 50-year interval reflects growing commercialization of the research findings, not changes in the underlying biology.
 *
 * PERSPECTIVAL GAP:
 *   From the aging adult seat, the constraint operates as an unavoidable biological tax — higher food costs, more planning, no escape. From the supplement manufacturer seat, the same constraint is a market opportunity created by nature. From the researcher seat, it is a measurable phenomenon with upstream mechanisms to investigate. The engine should compute these seats differently: the powerless/trapped seat experiences high effective extraction despite the low base ε, while the organized/mobile beneficiary seats experience the constraint as subsidy.
 *
 * DIRECTIONALITY LOGIC:
 *   Aging adults are full targets (d → 1.0): the constraint extracts from them (higher nutritional cost, no exit) regardless of who benefits commercially. Supplement manufacturers and nutrition consultants are beneficiaries (d → 0.0-0.2): they collect from meeting the constraint's requirement but do not control its existence. Researchers are analytical observers (d = 0.5): they characterize the constraint without being subject to it or profiting from it materially.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT mandatrophy despite having beneficiaries. The founding problem (sarcopenia in aging) is still live; the constraint is a discovered biological limit, not a vestigial coordination mechanism. The beneficiaries did not construct the constraint and could not remove it if they wanted to. The FSM signature should fire (mountain + beneficiaries), but the omega variables document why this is a genuine natural law that happens to create commercial opportunities, not a false summit masquerading as biology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_commercial_construction,
    'Is anabolic resistance an intrinsic feature of mammalian aging biology, or a constructed constraint that benefits supplement manufacturers?',
    'Cross-species comparative biology: if the leucine threshold shift appears in non-commercial model organisms (rodents, primates) under controlled conditions with no industry funding, the constraint is natural. If it only appears in industry-funded human studies, suspect construction.',
    'If natural, the constraint is a genuine mountain and the beneficiaries are incidental. If constructed, it is a false summit and should reclassify to tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_commercial_construction, empirical, 'Whether the constraint is discovered biology or commercial construction.').

omega_variable(
    leucine_threshold_universality,
    'Is the ~2.5-3g leucine threshold universal across populations, or does it vary with genetics, prior training, or dietary history in ways that industry-funded research systematically ignores?',
    'Population studies in non-Western cohorts with different baseline protein intakes and genetic backgrounds. If the threshold is stable across populations, it is a biological constant. If it varies substantially, the ''universal'' threshold is a statistical artifact of WEIRD subject pools.',
    'A variable threshold would mean the constraint is less absolute than presented, and the commercial dosing recommendations are overcalibrated to maximize product consumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leucine_threshold_universality, empirical, 'Whether the leucine threshold is a biological constant or a population-specific finding.').

omega_variable(
    mitochondrial_causality,
    'Is anabolic resistance downstream of mitochondrial demand signal deficiency, or are they parallel consequences of aging with no causal link?',
    'Interventional studies: if restoring mitochondrial function (via exercise, NAD+ precursors, or mitochondrial-targeted therapies) reduces the leucine threshold, the constraints are causally linked. If anabolic resistance persists despite mitochondrial restoration, they are parallel.',
    'If causally linked, fixing mitochondrial signaling could reduce or eliminate anabolic resistance, making the protein supplementation strategy a symptomatic treatment rather than a fundamental solution. If parallel, both constraints must be addressed independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitochondrial_causality, empirical, 'Whether anabolic resistance is caused by mitochondrial dysfunction or is an independent aging process.').

omega_variable(
    socioeconomic_access_inequality,
    'Does the constraint''s mitigation strategy (high per-meal protein intake) create a de facto nutritional inequality where low-income older adults cannot afford to maintain muscle mass?',
    'Epidemiological analysis: if sarcopenia rates are significantly higher in low-income older populations even after controlling for other health factors, the constraint''s biological reality is creating a socioeconomic health disparity through its mitigation cost.',
    'If confirmed, the constraint is natural but its consequences are mediated by economic access, making it a mountain that operates differently across power gradients. This would not change the constraint''s classification but would document a structural injustice in how biological limits interact with economic systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(socioeconomic_access_inequality, empirical, 'Whether the constraint''s mitigation cost creates measurable health inequality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(protein_anabolic_resistance, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prot_tr_t0, protein_anabolic_resistance, theater_ratio, 0, 0.01).
narrative_ontology:measurement_basis(prot_tr_t0, observed).
narrative_ontology:measurement(prot_tr_t10, protein_anabolic_resistance, theater_ratio, 10, 0.01).
narrative_ontology:measurement_basis(prot_tr_t10, observed).
narrative_ontology:measurement(prot_tr_t20, protein_anabolic_resistance, theater_ratio, 20, 0.015).
narrative_ontology:measurement_basis(prot_tr_t20, observed).
narrative_ontology:measurement(prot_tr_t30, protein_anabolic_resistance, theater_ratio, 30, 0.018).
narrative_ontology:measurement_basis(prot_tr_t30, observed).
narrative_ontology:measurement(prot_tr_t40, protein_anabolic_resistance, theater_ratio, 40, 0.02).
narrative_ontology:measurement_basis(prot_tr_t40, observed).
narrative_ontology:measurement(prot_tr_t50, protein_anabolic_resistance, theater_ratio, 50, 0.02).
narrative_ontology:measurement_basis(prot_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(prot_be_t0, protein_anabolic_resistance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(prot_be_t0, observed).
narrative_ontology:measurement(prot_be_t10, protein_anabolic_resistance, base_extractiveness, 10, 0.06).
narrative_ontology:measurement_basis(prot_be_t10, observed).
narrative_ontology:measurement(prot_be_t20, protein_anabolic_resistance, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(prot_be_t20, observed).
narrative_ontology:measurement(prot_be_t30, protein_anabolic_resistance, base_extractiveness, 30, 0.075).
narrative_ontology:measurement_basis(prot_be_t30, observed).
narrative_ontology:measurement(prot_be_t40, protein_anabolic_resistance, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(prot_be_t40, observed).
narrative_ontology:measurement(prot_be_t50, protein_anabolic_resistance, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(prot_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(prot_su_t0, protein_anabolic_resistance, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(prot_su_t0, observed).
narrative_ontology:measurement(prot_su_t10, protein_anabolic_resistance, suppression_requirement, 10, 0.02).
narrative_ontology:measurement_basis(prot_su_t10, observed).
narrative_ontology:measurement(prot_su_t20, protein_anabolic_resistance, suppression_requirement, 20, 0.025).
narrative_ontology:measurement_basis(prot_su_t20, observed).
narrative_ontology:measurement(prot_su_t30, protein_anabolic_resistance, suppression_requirement, 30, 0.028).
narrative_ontology:measurement_basis(prot_su_t30, observed).
narrative_ontology:measurement(prot_su_t40, protein_anabolic_resistance, suppression_requirement, 40, 0.03).
narrative_ontology:measurement_basis(prot_su_t40, observed).
narrative_ontology:measurement(prot_su_t50, protein_anabolic_resistance, suppression_requirement, 50, 0.03).
narrative_ontology:measurement_basis(prot_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(protein_anabolic_resistance, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of mitochondrial_demand_signal_deficiency in the aging biology network. Mitochondrial dysfunction may causally contribute to reduced anabolic sensitivity, but the causal link is not yet definitively established (see omega: mitochondrial_causality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
