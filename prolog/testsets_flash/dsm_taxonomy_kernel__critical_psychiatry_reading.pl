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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   This constraint represents the 'critical psychiatry' reading of the DSM's
 *   role, asserting that its diagnostic categories are significantly shaped
 *   by the availability of pharmaceutical treatments, thereby constructing
 *   markets for psychotropic drugs. This reading views the DSM not as a
 *   neutral scientific taxonomy, but as a tool influenced by economic
 *   interests, leading to over-medicalization and profit extraction. The
 *   claimed type is 'tangled_rope' because it still provides a coordination
 *   function (common diagnostic language) but is deeply intertwined with
 *   asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.7).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.6).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Categories as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '6131cfbc-2ea4-465f-9498-649fcab5a92d').
narrative_ontology:cs_kernel_codification('6131cfbc-2ea4-465f-9498-649fcab5a92d', formalized).
narrative_ontology:cs_authority_grounding('6131cfbc-2ea4-465f-9498-649fcab5a92d', extraction).
narrative_ontology:cs_interpretation_layer_present('6131cfbc-2ea4-465f-9498-649fcab5a92d').
narrative_ontology:cs_reading_relation('6131cfbc-2ea4-465f-9498-649fcab5a92d', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('6131cfbc-2ea4-465f-9498-649fcab5a92d', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('6131cfbc-2ea4-465f-9498-649fcab5a92d', foundational, diagnostic_categories_are_socially_constructed).
narrative_ontology:cs_axiom_status(diagnostic_categories_are_socially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('6131cfbc-2ea4-465f-9498-649fcab5a92d', diagnostic_categories_are_socially_constructed, empirically_contingent).
narrative_ontology:cs_axiom('6131cfbc-2ea4-465f-9498-649fcab5a92d', foundational, pharmaceutical_profit_drives_diagnostic_expansion).
narrative_ontology:cs_axiom_status(pharmaceutical_profit_drives_diagnostic_expansion, holdable).
narrative_ontology:cs_axiom_grounding('6131cfbc-2ea4-465f-9498-649fcab5a92d', pharmaceutical_profit_drives_diagnostic_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('6131cfbc-2ea4-465f-9498-649fcab5a92d', pre_pharmaceutical_influence_taxonomy).
narrative_ontology:cs_drift_state('6131cfbc-2ea4-465f-9498-649fcab5a92d', contemporary_dsm_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6131cfbc-2ea4-465f-9498-649fcab5a92d', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_with_industry_ties).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, general_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Funds research, marketing, and lobbying efforts that influence the definition and promotion of psychiatric diagnoses, ensuring alignment with their drug pipelines. Benefits directly from increased prescriptions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive funding for research, speaking engagements, and consulting from pharmaceutical companies. Their professional standing and income are often enhanced by the diagnostic framework that supports psychotropic drug use.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_with_industry_ties, beneficiary,
    powerful, biographical, mobile, national).

% Are diagnosed with categories that may not accurately reflect their distress, leading to prescriptions for psychotropic drugs. They bear the costs of medication, potential side effects, and the medicalization of their experiences, with limited access to alternative explanations or treatments.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription, payer,
    powerless, biographical, constrained, local).

% Often rely on DSM categories for diagnosis and treatment guidelines, leading them to prescribe psychotropic drugs even when alternative, non-pharmacological interventions might be more appropriate. They face pressure from patients, specialists, and pharmaceutical marketing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, general_practitioners, payer,
    moderate, biographical, constrained, local).

% Analyze and critique the DSM's development and its relationship with the pharmaceutical industry, advocating for alternative models of mental distress and care. They operate within academic and advocacy spaces, often challenging mainstream psychiatric practice.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists_and_academics, observer,
    organized, generational, analytical, global).

% Offer non-pharmacological approaches to mental health but often find their methods marginalized or not covered by insurance due to the dominance of the DSM-driven, biomedical model. They are excluded from mainstream diagnostic and treatment pathways.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, alternative_therapists_and_counselors, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language and framework for diagnosing mental health conditions, facilitating communication among clinicians, researchers, and insurance providers, and guiding treatment decisions.
% TRANSFER_FUNCTION: Transfers significant financial resources from patients and healthcare systems to pharmaceutical companies and associated medical professionals, in exchange for diagnostic labels and psychotropic drug treatments.
% ABSENT_VOICES: Patients seeking non-pharmacological solutions, alternative mental health practitioners, and researchers advocating for psychosocial or systemic explanations of distress are largely absent from the DSM's definitional process. They would argue for a broader, less medicalized understanding of mental health.
% DISAPPEARANCE_RATIONALE: If the DSM categories, as currently constructed and enforced, vanished overnight, the pharmaceutical market for psychotropic drugs would face a severe crisis of legitimacy and demand. Diagnostic practices would fragment, insurance reimbursement would be disrupted, and there would be a significant shift towards alternative explanatory models and treatment modalities, fundamentally reorganizing mental healthcare.
% FOUNDING_PROBLEM: To create a common nomenclature for mental disorders, improve diagnostic reliability, and facilitate research into etiology and treatment.
% FOUNDING_PROBLEM_CORROBORATION: The American Psychiatric Association and pharmaceutical companies assert the problem is live, citing the need for consistent diagnosis and treatment. Critical psychiatrists, patient advocacy groups, and independent researchers argue that while a common language is useful, the current system has been co-opted, and the founding problem of reliable diagnosis is now secondary to market construction. This is supported by historical analyses of DSM revisions and pharmaceutical marketing strategies.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).

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
 *   Extractiveness is high (0.7) due to the substantial profits generated by pharmaceutical sales driven by DSM diagnoses. Suppression (0.6) arises from the institutional dominance of the biomedical model, which marginalizes alternative explanations and treatments, limiting patient and practitioner exit options. The theater ratio (0.4) reflects that while some diagnostic refinement is genuine, a significant portion of the taxonomic work serves to legitimize and expand drug markets. Accessibility collapse (0.4) is moderate because while the biomedical model is dominant, alternative perspectives and therapies do exist, albeit often marginalized. Resistance (0.7) is high, driven by patient advocacy groups, critical academics, and alternative practitioners who actively challenge the DSM's influence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pharmaceutical capital and aligned psychiatrists, the DSM is a 'rope' or 'scaffold' that coordinates research and treatment, leading to better patient outcomes. From the 'critical psychiatry' perspective, it functions as a 'tangled_rope' or 'snare,' coordinating diagnosis while extracting profits and suppressing alternative approaches. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical capital and psychiatrists with industry ties are clear beneficiaries, as the system directly generates revenue and professional opportunities for them. Patients subjected to overprescription and general practitioners are payers, bearing the costs of medication and the limitations of a medicalized framework. Critical psychiatrists and alternative therapists are observers or excluded, actively resisting or marginalized by the dominant paradigm.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to provide a reliable diagnostic system. This reading argues that the mandate has drifted, becoming entangled with market construction. The 'tangled_rope' classification captures this hybridity, preventing mislabeling it as pure coordination (ignoring extraction) or pure extraction (ignoring its residual coordination function). The contested status of the founding problem further supports this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_influence_quantification,
    'What is the precise causal weight of pharmaceutical industry funding and lobbying on specific DSM diagnostic criteria revisions, relative to purely scientific evidence?',
    'Independent, transparent audits of DSM task force conflicts of interest, detailed financial disclosures, and comparative analysis of diagnostic expansion against drug patent expirations and new drug approvals.',
    'Higher quantified influence would strengthen the ''tangled_rope'' or ''snare'' classification by demonstrating a more direct link between extraction and diagnostic categories; lower influence would shift it closer to a ''rope'' or ''scaffold'' with a more benign coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_quantification, empirical, 'Quantifying the extent of pharmaceutical industry influence on DSM revisions.').

omega_variable(
    diagnostic_utility_vs_harm,
    'Does the DSM''s coordination function (e.g., diagnostic reliability for research) outweigh the harms of over-medicalization, overprescription, and the suppression of alternative paradigms?',
    'Longitudinal studies comparing patient outcomes under DSM-guided care versus alternative, non-pharmacological, or non-DSM-based approaches, including measures of iatrogenic harm and patient-reported quality of life.',
    'If harms significantly outweigh utility, the classification would shift more definitively towards ''snare''; if utility is found to be substantial despite harms, it would remain a ''tangled_rope'' or even shift towards a ''rope'' for certain applications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diagnostic_utility_vs_harm, preference, 'Balancing the DSM''s utility against its potential harms.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''critical psychiatry'' reading of the DSM kernel, or is it an overstatement of pharmaceutical influence that obscures the DSM''s legitimate scientific aims?',
    'Consensus among independent historians of psychiatry and medical sociologists on the historical trajectory of DSM revisions and their correlation with pharmaceutical market dynamics, accounting for both scientific and economic drivers.',
    'If the reading is found to be an overstatement, the constraint''s extractiveness and suppression metrics would be re-evaluated downwards, potentially shifting its classification towards a ''rope'' or ''scaffold'' (closer to the biomedical reading). If corroborated, the current metrics and classification are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity regarding the validity and scope of the critical psychiatry reading itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
