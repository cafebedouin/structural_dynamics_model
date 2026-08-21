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
 *   drugs. It is one reading of the 'dsm_taxonomy_kernel'. This reading
 *   highlights the role of pharmaceutical capital and industry-tied
 *   psychiatrists as beneficiaries, while patients and the general public
 *   bear the costs of over-medicalization and adverse drug effects. The
 *   metrics reflect a system with substantial extraction and suppression,
 *   maintained by active enforcement of diagnostic and treatment paradigms.
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
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e7fa6432-ad0a-4626-81a7-7459c36e9def').
narrative_ontology:cs_kernel_codification('e7fa6432-ad0a-4626-81a7-7459c36e9def', formalized).
narrative_ontology:cs_authority_grounding('e7fa6432-ad0a-4626-81a7-7459c36e9def', extraction).
narrative_ontology:cs_interpretation_layer_present('e7fa6432-ad0a-4626-81a7-7459c36e9def').
narrative_ontology:cs_reading_relation('e7fa6432-ad0a-4626-81a7-7459c36e9def', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7fa6432-ad0a-4626-81a7-7459c36e9def', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('e7fa6432-ad0a-4626-81a7-7459c36e9def', foundational, diagnostic_categories_are_social_constructs).
narrative_ontology:cs_axiom_status(diagnostic_categories_are_social_constructs, holdable).
narrative_ontology:cs_axiom_grounding('e7fa6432-ad0a-4626-81a7-7459c36e9def', diagnostic_categories_are_social_constructs, empirically_contingent).
narrative_ontology:cs_axiom('e7fa6432-ad0a-4626-81a7-7459c36e9def', foundational, psychiatric_medicalization_serves_capital).
narrative_ontology:cs_axiom_status(psychiatric_medicalization_serves_capital, holdable).
narrative_ontology:cs_axiom_grounding('e7fa6432-ad0a-4626-81a7-7459c36e9def', psychiatric_medicalization_serves_capital, empirically_contingent).
narrative_ontology:cs_reference_frame('e7fa6432-ad0a-4626-81a7-7459c36e9def', pre_pharmaceutical_era_descriptive_psychiatry).
narrative_ontology:cs_drift_state('e7fa6432-ad0a-4626-81a7-7459c36e9def', contemporary_pharmaceutical_dominance, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e7fa6432-ad0a-4626-81a7-7459c36e9def', '').
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

% Funds research, marketing, and lobbying efforts that influence the development and adoption of DSM categories, particularly those that align with existing or pipeline psychotropic drugs. Benefits directly from increased drug prescriptions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive funding for research, speaking engagements, and consulting from pharmaceutical companies. Their professional practice is shaped by DSM categories, which often lead to pharmaceutical interventions. Their professional standing is tied to the existing diagnostic framework.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_with_industry_ties, beneficiary,
    powerful, biographical, constrained, national).

% Are diagnosed with conditions that may be over-medicalized or for which non-pharmaceutical alternatives are downplayed, leading to unnecessary or prolonged drug regimens. Bear the financial cost and potential health risks of medication.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription, payer,
    powerless, immediate, trapped, local).

% Suffer from side effects of psychotropic medications prescribed based on DSM diagnoses, often without adequate information about risks or alternatives. Their ability to exit treatment is constrained by dependency and lack of perceived alternatives.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_experiencing_adverse_drug_effects, payer,
    powerless, immediate, trapped, local).

% Internalizes a framework where common human experiences of distress are pathologized and medicalized, leading to a societal over-reliance on pharmaceutical solutions and a diminished capacity for non-medical coping strategies. Bears the diffuse costs of a medicalized society.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, general_public_via_medicalized_distress, payer,
    moderate, generational, constrained, national).

% Analyze and critique the DSM's role in medicalizing distress and promoting pharmaceutical solutions. They advocate for alternative models of mental health that prioritize social, psychological, and existential factors over purely biological ones. Their influence is primarily academic and advocacy-based.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists_and_academics, observer,
    organized, generational, analytical, global).

% Argue that many DSM categories pathologize natural human variation and that the focus on 'disorder' obscures the strengths and unique perspectives of neurodivergent individuals. Their perspectives are often marginalized within mainstream psychiatric discourse.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language for psychiatric diagnosis, facilitating communication among clinicians, researchers, and insurance providers, and guiding treatment decisions within a medical model.
% TRANSFER_FUNCTION: Transfers significant financial resources from patients (and healthcare systems) to pharmaceutical companies through the prescription of psychotropic drugs, driven by the diagnostic categories.
% ABSENT_VOICES: Patients who feel misdiagnosed or harmed by psychiatric medication, and advocates for non-medical approaches to distress, are often marginalized in the process of DSM revision and implementation. Neurodiversity advocates are also largely excluded from the core definitional process.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the pharmaceutical market for psychotropic drugs would face immediate disruption, diagnostic practices would fragment, and the medical-industrial complex around mental health would be forced to fundamentally reorganize, shifting power dynamics and treatment paradigms.
% FOUNDING_PROBLEM: To create a common nomenclature for mental disorders, improve diagnostic reliability, and facilitate research into etiology and treatment, moving away from purely psychoanalytic or descriptive approaches.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream psychiatry and pharmaceutical companies assert the problem of diagnostic reliability and treatment guidance remains live. Critical psychiatrists and patient advocacy groups argue that while a common language is useful, the current taxonomy has been co-opted, and its original problem-solving function is now secondary to market construction; independent sociological and historical analyses corroborate this shifted function.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) due to the significant profits generated by pharmaceutical sales driven by DSM diagnoses. Suppression (0.65) reflects the institutional power of mainstream psychiatry and pharmaceutical marketing in shaping public and professional understanding of mental health, limiting perceived alternatives to drug treatment. Theater ratio (0.45) indicates that while some diagnostic and research functions are genuine, a substantial portion of the system's activity serves to legitimize and expand pharmaceutical markets. The increasing trend in extractiveness and suppression over time reflects the growing influence of pharmaceutical interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pharmaceutical capital and industry-tied psychiatrists, the DSM taxonomy is a necessary tool for identifying and treating illness, justifying the associated costs and profits. From the critical psychiatry reading, the same structure is a mechanism for market construction and profit extraction, with the coordination function serving as a cover. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical capital and psychiatrists with industry ties are clear beneficiaries (low directionality), as they directly profit or gain professional standing from the current system. Patients subjected to overprescription and adverse drug effects are primary targets (high directionality), bearing the direct costs and harms. The general public is also a target, experiencing the diffuse costs of medicalized distress. Critical psychiatrists and neurodiversity advocates act as observers or excluded parties, challenging the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_influence_quantification,
    'What is the precise quantitative impact of pharmaceutical funding on DSM category development and diagnostic criteria?',
    'Independent, transparent audits of all funding flows to DSM task force members, coupled with analysis of changes in diagnostic criteria correlating with drug development cycles.',
    'Strong correlation would solidify the ''market construction'' claim, potentially leading to reclassification towards a Snare. Weak correlation would challenge the core premise of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_quantification, empirical, 'Quantifying the direct influence of pharmaceutical capital on DSM taxonomy.').

omega_variable(
    alternative_treatment_efficacy,
    'What is the comparative efficacy and accessibility of non-pharmaceutical interventions (e.g., psychotherapy, social support, lifestyle changes) for conditions currently managed by psychotropic drugs?',
    'Large-scale, independent comparative effectiveness research studies, and public health initiatives promoting non-pharmaceutical options.',
    'Demonstrated high efficacy and accessibility of alternatives would expose the suppression of options, strengthening the extractive nature of the constraint. Low efficacy/accessibility would partially vindicate the current system''s necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_treatment_efficacy, empirical, 'Assessing the viability and suppression of non-pharmaceutical alternatives.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the DSM taxonomy fundamentally a scientific classification system, a medical practice guide, or a market-shaping instrument?',
    'Analysis of the historical evolution of the DSM, its funding sources, and its practical effects on clinical practice and pharmaceutical sales, weighed against its stated scientific goals.',
    'If primarily a market instrument, this reading''s classification as Tangled Rope (or Snare) is reinforced. If primarily scientific, the ''biomedical_reading'' gains credence, and this reading''s claims become a critique of implementation rather than core structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the fundamental purpose and nature of the DSM taxonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, resource_allocation).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_patent_system).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, medical_insurance_reimbursement_rules).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_training_curricula).

% DUAL FORMULATION NOTE:
% This constraint is the 'critical_psychiatry_reading' of the 'dsm_taxonomy_kernel'. It highlights the market-construction function, contrasting with the 'biomedical_reading' (objective disease entities) and 'neurodiversity_reading' (pathologizing variation). Each reading is a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
