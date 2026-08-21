% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__critical_psychiatry_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Categories as Pharmaceutical Market Construction (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint story represents the 'critical psychiatry' reading of the
 *   DSM taxonomic kernel. From this perspective, DSM categories are not
 *   objective reflections of disease but are reverse-engineered from
 *   available pharmaceutical treatments. This process constructs markets for
 *   psychotropic drugs, medicalizing human distress and expanding the scope
 *   of psychiatric intervention primarily for profit. The claimed type is
 *   'snare' because the coordination function (standardized diagnosis) serves
 *   as a cover for substantial, actively enforced extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.7).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.8).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Categories as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '36c27489-f73a-4370-b758-e645afdc01a8').
narrative_ontology:cs_kernel_codification('36c27489-f73a-4370-b758-e645afdc01a8', formalized).
narrative_ontology:cs_authority_grounding('36c27489-f73a-4370-b758-e645afdc01a8', extraction).
narrative_ontology:cs_interpretation_layer_present('36c27489-f73a-4370-b758-e645afdc01a8').
narrative_ontology:cs_reading_relation('36c27489-f73a-4370-b758-e645afdc01a8', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('36c27489-f73a-4370-b758-e645afdc01a8', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('36c27489-f73a-4370-b758-e645afdc01a8', foundational, psychiatric_categories_are_socially_constructed_for_markets).
narrative_ontology:cs_axiom_status(psychiatric_categories_are_socially_constructed_for_markets, holdable).
narrative_ontology:cs_axiom_grounding('36c27489-f73a-4370-b758-e645afdc01a8', psychiatric_categories_are_socially_constructed_for_markets, empirically_contingent).
narrative_ontology:cs_axiom('36c27489-f73a-4370-b758-e645afdc01a8', secondary, pharmaceutical_profit_drives_diagnostic_expansion).
narrative_ontology:cs_axiom_status(pharmaceutical_profit_drives_diagnostic_expansion, holdable).
narrative_ontology:cs_axiom_grounding('36c27489-f73a-4370-b758-e645afdc01a8', pharmaceutical_profit_drives_diagnostic_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('36c27489-f73a-4370-b758-e645afdc01a8', biomedical_hegemony_of_diagnosis).
narrative_ontology:cs_drift_state('36c27489-f73a-4370-b758-e645afdc01a8', contemporary_critical_challenge_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('36c27489-f73a-4370-b758-e645afdc01a8', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, biomedical_researchers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_companies).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_with_adverse_drug_effects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drives the expansion of diagnostic categories and the development of psychotropic drugs, directly profiting from increased prescription rates and market share. Actively influences research and professional guidelines.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from research grants, speaking fees, and professional status tied to the biomedical model of mental illness and its pharmaceutical treatments. Their practice is heavily influenced by DSM categories and drug availability.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary,
    powerful, biographical, constrained, national).

% Receive diagnoses and prescriptions that may not be appropriate for their distress, leading to unnecessary drug use, dependency, and a medicalized understanding of their experiences. Often lack access to alternative treatments.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription, payer,
    powerless, immediate, trapped, local).

% Suffer negative health consequences, including severe side effects, withdrawal symptoms, and long-term health issues, from psychotropic medications prescribed based on DSM diagnoses.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_with_adverse_drug_effects, payer,
    powerless, immediate, trapped, local).

% Analyze and critique the DSM's role in medicalizing distress, promoting pharmaceutical solutions, and serving commercial interests. They advocate for alternative models of understanding and treating mental health.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists_and_advocates, observer,
    moderate, biographical, constrained, national).

% Receive funding and prestige for research aligned with DSM categories and pharmaceutical targets, reinforcing the biomedical paradigm and its associated market structures.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, biomedical_researchers, beneficiary,
    institutional, generational, constrained, global).

% Rely on DSM diagnoses for billing and reimbursement, which streamlines claims processing and reinforces the medical model, even if it leads to over-medicalization and higher overall costs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_companies, beneficiary,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized system of diagnostic labels, facilitating communication among clinicians, researchers, and insurance providers, and enabling large-scale epidemiological studies and treatment protocols.
% TRANSFER_FUNCTION: Transfers significant financial resources from patients (via drug purchases and insurance premiums) and public healthcare systems to pharmaceutical companies, industry-funded researchers, and associated medical professionals, in exchange for diagnostic labels and pharmaceutical interventions.
% ABSENT_VOICES: Patients seeking non-pharmacological, holistic, or social solutions; practitioners of alternative therapies; those who view distress as a normal human experience or social problem rather than a medical illness; and indigenous healing traditions. These voices are marginalized or excluded by the dominant biomedical framework.
% DISAPPEARANCE_RATIONALE: If the DSM's categorical system and its associated enforcement mechanisms vanished overnight, the current model of psychiatric diagnosis, pharmaceutical prescription, and insurance billing would collapse. This would force a radical reorganization of mental healthcare, research funding, and pharmaceutical markets, shifting towards more diverse and potentially less medicalized approaches.
% FOUNDING_PROBLEM: To standardize psychiatric diagnoses, improve communication among clinicians, facilitate research into mental disorders, and provide a common language for mental health professionals.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (biomedical psychiatrists, pharmaceutical industry) claim the problem of diagnostic consistency and research facilitation is still live. Critics (critical psychiatrists, patient advocates) argue that the original problem is substantially solved or superseded by the DSM's role in market creation and medicalization, supported by historical analysis of DSM revisions, pharmaceutical development cycles, and industry influence on diagnostic criteria.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the system generates significant profits for pharmaceutical companies and associated professionals, often at the expense of patients receiving unnecessary or harmful treatments. Suppression (0.8) is high due to the institutional power of psychiatry, the medicalization of distress, and the limited availability or recognition of non-pharmacological alternatives. Theater ratio (0.4) reflects that while some diagnostic work is genuine, a substantial portion of the system's activity is performative, justifying existing categories and treatments rather than objectively discovering new ones. The increasing trends in extractiveness and suppression over the interval reflect the growing influence of the pharmaceutical industry and the expansion of diagnostic criteria since DSM-III.
 *
 * PERSPECTIVAL GAP:
 *   The critical psychiatry reading fundamentally diverges from the biomedical reading. While the biomedical perspective sees the DSM as a scientific tool for objective diagnosis, the critical psychiatry perspective views it as a market-generating mechanism. This gap means that what one seat perceives as necessary coordination, another perceives as enforced extraction. The engine's per-seat classification will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical capital and industry-funded psychiatrists are clear beneficiaries, directly profiting or gaining status from the system. Biomedical researchers and insurance companies also benefit from the streamlined, biomedical framework. Patients subjected to overprescription and adverse drug effects are the primary victims, bearing the costs of unnecessary treatment and side effects. Critical psychiatrists and advocates act as observers, challenging the system's underlying assumptions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''critical_psychiatry_reading'' of the ''dsm_taxonomy_kernel''?',
    'Comparison with canonical texts and arguments within critical psychiatry literature.',
    'If misaligned, the analysis of the kernel''s contested nature would be flawed, potentially misrepresenting the structural delta between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verification of the specific reading''s fidelity to its intellectual tradition.').

omega_variable(
    biomedical_vs_market_construction,
    'To what extent do DSM categories reflect objective neurobiological disease entities versus being constructed to facilitate pharmaceutical markets?',
    'Longitudinal studies tracking diagnostic expansion relative to drug development, independent analysis of DSM revision processes, and meta-analysis of industry funding in psychiatric research.',
    'If categories are primarily objective, the extractiveness of this constraint is overstated; if primarily constructed, the snare classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biomedical_vs_market_construction, empirical, 'Structural delta between critical psychiatry and biomedical readings.').

omega_variable(
    pathologization_vs_market_construction,
    'Is the primary function of DSM categories to pathologize natural human variation, or to construct markets for psychotropic drugs?',
    'Sociological analysis of diagnostic trends in relation to social norms versus economic analysis of pharmaceutical market growth and lobbying efforts.',
    'If pathologization is primary, the neurodiversity reading gains explanatory power; if market construction is primary, this reading is reinforced. Both can coexist, but their relative weight affects the overall understanding of the constraint''s function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathologization_vs_market_construction, conceptual, 'Structural delta between critical psychiatry and neurodiversity readings.').

omega_variable(
    extent_of_pharmaceutical_influence,
    'What is the precise quantitative extent of pharmaceutical industry influence on DSM revisions and psychiatric prescribing practices?',
    'Disclosure of financial ties for DSM panel members, analysis of drug sales data correlated with diagnostic changes, and independent audits of pharmaceutical marketing expenditures.',
    'Higher quantified influence would strengthen the ''snare'' classification and the extractiveness metric; lower influence would suggest a more complex, less purely extractive dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_pharmaceutical_influence, empirical, 'Quantification of industry''s role in shaping psychiatric taxonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(dsm__tr_t1988, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(dsm__tr_t1996, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1996, 0.35).
narrative_ontology:measurement(dsm__tr_t2004, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2004, 0.38).
narrative_ontology:measurement(dsm__tr_t2012, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2012, 0.39).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(dsm__be_t1988, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1988, 0.58).
narrative_ontology:measurement(dsm__be_t1996, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1996, 0.64).
narrative_ontology:measurement(dsm__be_t2004, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2004, 0.68).
narrative_ontology:measurement(dsm__be_t2012, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2012, 0.69).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(dsm__su_t1988, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1988, 0.7).
narrative_ontology:measurement(dsm__su_t1996, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1996, 0.75).
narrative_ontology:measurement(dsm__su_t2004, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2004, 0.78).
narrative_ontology:measurement(dsm__su_t2012, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2012, 0.79).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
