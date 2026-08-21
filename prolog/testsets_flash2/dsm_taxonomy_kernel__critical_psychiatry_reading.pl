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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the critical psychiatry reading of the DSM
 *   taxonomy, where diagnostic categories are seen as reverse-engineered from
 *   available pharmaceutical treatments to construct markets for psychotropic
 *   drugs. It is a Tangled Rope because it provides a coordination function
 *   (standardized diagnosis) but is deeply intertwined with asymmetric
 *   extraction (pharmaceutical profits). The metrics reflect a system that
 *   has become increasingly extractive and reliant on active enforcement
 *   (e.g., through insurance reimbursement rules tied to DSM diagnoses) over
 *   time, with a growing theatrical component where scientific legitimacy
 *   masks commercial interests.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.65).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, 'fee5c512-6708-441a-9e9f-8680082d602b').
narrative_ontology:cs_kernel_codification('fee5c512-6708-441a-9e9f-8680082d602b', formalized).
narrative_ontology:cs_authority_grounding('fee5c512-6708-441a-9e9f-8680082d602b', extraction).
narrative_ontology:cs_interpretation_layer_present('fee5c512-6708-441a-9e9f-8680082d602b').
narrative_ontology:cs_reading_relation('fee5c512-6708-441a-9e9f-8680082d602b', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fee5c512-6708-441a-9e9f-8680082d602b', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('fee5c512-6708-441a-9e9f-8680082d602b', foundational, psychiatric_categories_are_social_constructs).
narrative_ontology:cs_axiom_status(psychiatric_categories_are_social_constructs, holdable).
narrative_ontology:cs_axiom_grounding('fee5c512-6708-441a-9e9f-8680082d602b', psychiatric_categories_are_social_constructs, empirically_contingent).
narrative_ontology:cs_axiom('fee5c512-6708-441a-9e9f-8680082d602b', foundational, pharmaceutical_profit_drives_diagnostic_expansion).
narrative_ontology:cs_axiom_status(pharmaceutical_profit_drives_diagnostic_expansion, holdable).
narrative_ontology:cs_axiom_grounding('fee5c512-6708-441a-9e9f-8680082d602b', pharmaceutical_profit_drives_diagnostic_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('fee5c512-6708-441a-9e9f-8680082d602b', pre_pharmaceutical_era_holistic_understanding).
narrative_ontology:cs_drift_state('fee5c512-6708-441a-9e9f-8680082d602b', contemporary_pharmaceutical_dominance, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fee5c512-6708-441a-9e9f-8680082d602b', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_with_industry_ties).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_experiencing_adverse_drug_effects).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, general_public_via_medicalized_distress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Funds research, marketing, and lobbying efforts that influence the development and adoption of DSM categories, particularly those for which psychotropic drugs are available. Benefits directly from increased diagnoses and prescriptions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive funding for research, speaking engagements, and consulting from pharmaceutical companies. Their professional practice is shaped by DSM categories, which often lead to pharmaceutical interventions. They benefit from the perceived legitimacy and efficacy of these categories.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_with_industry_ties, beneficiary,
    powerful, biographical, constrained, national).

% Are diagnosed with conditions that may be over-medicalized or for which non-pharmaceutical alternatives are downplayed. They bear the financial costs of medication and therapy, as well as the burden of a medicalized identity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription, payer,
    powerless, immediate, trapped, local).

% Suffer from side effects of psychotropic medications, often without adequate recognition or alternative treatment options. Their suffering is a direct cost of the pharmaceutical-driven diagnostic system.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_experiencing_adverse_drug_effects, payer,
    powerless, immediate, trapped, local).

% Experiences a societal shift towards medicalizing normal human distress and variation, leading to increased reliance on pharmaceutical solutions and a diminished capacity for non-medical coping strategies. Bears the indirect costs of a medicalized society.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, general_public_via_medicalized_distress, payer,
    moderate, generational, constrained, national).

% Analyze and critique the DSM's role in medicalizing distress and promoting pharmaceutical solutions. They advocate for alternative models of mental health that prioritize social, psychological, and existential factors over biological ones. Their work aims to expose the constraint's extractive nature.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists_and_academics, observer,
    organized, generational, analytical, global).

% Offer alternative, non-pharmacological treatments but often find their approaches marginalized or less reimbursed within a system heavily influenced by pharmaceutical models and DSM categories. They are excluded from the primary diagnostic and treatment pathways.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, non_pharmaceutical_therapists, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language for psychiatric diagnosis, facilitating communication among clinicians, researchers, and insurance providers, and guiding treatment decisions within a biomedical framework.
% TRANSFER_FUNCTION: Transfers significant financial resources from patients and healthcare systems to pharmaceutical companies and associated medical professionals, in exchange for diagnostic labels and corresponding drug treatments.
% ABSENT_VOICES: Patients advocating for non-medicalized understandings of distress, indigenous healing traditions, and alternative therapeutic modalities are largely absent from the DSM's development and mainstream psychiatric discourse. They would challenge the underlying assumptions of pathologization and pharmaceutical necessity.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy and its associated enforcement mechanisms vanished overnight, the pharmaceutical market for psychotropic drugs would face immediate disruption, diagnostic practices would fragment, and there would be a rapid re-evaluation of mental health interventions, leading to a significant reorganization of psychiatric care and funding.
% FOUNDING_PROBLEM: To create a common nomenclature for mental disorders, improve diagnostic reliability, and guide research and treatment, moving psychiatry towards a more scientific and evidence-based discipline.
% FOUNDING_PROBLEM_CORROBORATION: The American Psychiatric Association and many clinicians attest the problem is still live, citing the need for diagnostic consistency. Critical psychiatrists, patient advocacy groups, and independent researchers attest that while a common language is useful, the current taxonomy has been co-opted by pharmaceutical interests, and its original problem-solving function is now secondary to market construction; this is supported by analyses of DSM revisions correlating with drug development cycles.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) due to the vast profits generated by psychotropic drug sales, driven by DSM diagnoses. Suppression (0.65) is significant, as alternative diagnostic frameworks or non-pharmaceutical treatments are marginalized by the dominant biomedical model and its institutional backing. The theater ratio (0.45) reflects the ongoing performance of scientific objectivity and patient-centered care, which increasingly serves to legitimize pharmaceutical market expansion. Accessibility collapse (0.60) indicates that while some alternatives exist, the dominant paradigm makes them difficult to access or legitimize. Resistance (0.70) is high, driven by patient advocacy groups and critical academics challenging the system.
 *
 * PERSPECTIVAL GAP:
 *   The critical psychiatry reading fundamentally diverges from the biomedical reading, which would frame the DSM as a neutral scientific tool. From the perspective of pharmaceutical capital, the DSM is a highly effective coordination mechanism for market expansion. From the patient's perspective, it is an extractive system that pathologizes normal life and pushes medication. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical capital and psychiatrists with industry ties are clear beneficiaries, as they directly profit from the system. Patients, particularly those subjected to overprescription or adverse effects, are the primary victims, bearing the costs in terms of health, finances, and medicalized identities. The general public also bears diffuse costs through the medicalization of normal distress. Critical psychiatrists and non-pharmaceutical therapists act as observers or excluded parties, challenging the system from the margins.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to standardize diagnosis for scientific and clinical benefit. This reading argues that the mandate has atrophied, becoming a cover for market construction. The classification as Tangled Rope, with high extractiveness and suppression, prevents mislabeling this as pure coordination by highlighting the asymmetric benefits and active enforcement required to maintain the pharmaceutical-driven diagnostic system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_influence_quantification,
    'What is the precise financial and structural influence of pharmaceutical capital on the DSM revision process and psychiatric education?',
    'Independent audits of funding flows, disclosure of conflicts of interest for DSM panel members, and longitudinal studies tracking pharmaceutical marketing spend against diagnostic prevalence.',
    'Higher quantified influence would strengthen the ''extraction'' component of the Tangled Rope classification, potentially pushing it closer to a Snare if the coordination function is found to be negligible compared to the market-driven agenda.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_quantification, empirical, 'Quantifying the extent of pharmaceutical industry influence on psychiatric taxonomy.').

omega_variable(
    medicalization_of_distress_boundary,
    'Where is the boundary between genuine psychiatric illness and normal human distress or variation that has been medicalized by the DSM?',
    'Cross-cultural studies of distress, longitudinal studies of diagnostic stability without pharmaceutical intervention, and patient-reported outcome measures that prioritize lived experience over symptom checklists.',
    'A clearer boundary would reduce the victim set by identifying individuals who are currently over-diagnosed, thereby reducing the measured extractiveness and suppression for those individuals. If a large portion of current diagnoses are found to be medicalized distress, the constraint''s overall extractiveness would be re-evaluated upwards for the remaining ''genuine'' illness categories, as the ''coordination'' function would be seen as even more tenuous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medicalization_of_distress_boundary, conceptual, 'Distinguishing genuine illness from medicalized normal human experience.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., insurance rules, lack of alternative treatment access) or internalized (e.g., patients internalizing a medicalized identity, belief in drug necessity)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-stigma, continued belief in medical necessity) after structural barriers are removed (e.g., access to alternative therapies), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true exit more difficult and amplifying the constraint''s power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in psychiatric diagnosis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, identity_coordination).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_patent_system).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, health_insurance_reimbursement_rules).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dsm_taxonomy_kernel'. It is linked to the 'biomedical_reading' and 'neurodiversity_reading' as sibling interpretations of the same core commitment, each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
