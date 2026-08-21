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
 *   human_readable: DSM Categories as Pharmaceutical Market Constructs (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint story represents the 'critical psychiatry' reading of the
 *   DSM taxonomy kernel. From this perspective, DSM categories are not
 *   objective reflections of disease but are actively reverse-engineered from
 *   available pharmaceutical treatments to construct and expand markets for
 *   psychotropic drugs. The system, while claiming to standardize diagnosis,
 *   primarily functions as a mechanism for profit extraction, with patients
 *   bearing the costs and risks. The claimed type is 'snare' because the
 *   coordination story (diagnostic standardization) is seen as a cover for a
 *   fundamentally extractive operation.
 *
 * KEY AGENTS:
 *   - Pharmaceutical_companies: Primary agenda-setter and beneficiary (institutional/arbitrage)
 *   - Industry_funded_psychiatrists: Beneficiary (powerful/constrained)
 *   - Patients: Primary target/payer (powerless/trapped)
 *   - Insurance_companies: Beneficiary (institutional/arbitrage)
 *   - Critical_psychiatrists: Observer (moderate/mobile)
 *   - Alternative_therapists: Excluded (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.85).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Categories as Pharmaceutical Market Constructs (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '05ea163e-a2be-432f-88a8-b69ef0a25659').
narrative_ontology:cs_kernel_codification('05ea163e-a2be-432f-88a8-b69ef0a25659', formalized).
narrative_ontology:cs_authority_grounding('05ea163e-a2be-432f-88a8-b69ef0a25659', extraction).
narrative_ontology:cs_interpretation_layer_present('05ea163e-a2be-432f-88a8-b69ef0a25659').
narrative_ontology:cs_reading_relation('05ea163e-a2be-432f-88a8-b69ef0a25659', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('05ea163e-a2be-432f-88a8-b69ef0a25659', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('05ea163e-a2be-432f-88a8-b69ef0a25659', foundational, diagnostic_categories_market_constructs).
narrative_ontology:cs_axiom_status(diagnostic_categories_market_constructs, holdable).
narrative_ontology:cs_axiom_grounding('05ea163e-a2be-432f-88a8-b69ef0a25659', diagnostic_categories_market_constructs, empirically_contingent).
narrative_ontology:cs_reference_frame('05ea163e-a2be-432f-88a8-b69ef0a25659', objective_disease_taxonomy).
narrative_ontology:cs_drift_state('05ea163e-a2be-432f-88a8-b69ef0a25659', contemporary_pharmaceutical_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('05ea163e-a2be-432f-88a8-b69ef0a25659', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_companies).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, alternative_therapists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heavily influence the development and promotion of DSM categories, often aligning them with the indications for their psychotropic drugs. They profit directly from increased prescriptions driven by these categories.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_companies, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from research grants, speaking fees, and consulting arrangements with pharmaceutical companies. They apply DSM categories in clinical practice, often leading to pharmacotherapy, reinforcing the market-driven system.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary,
    powerful, biographical, constrained, national).

% Are diagnosed using DSM categories, often leading to prescriptions for psychotropic drugs. They bear the financial costs of medication, potential adverse side effects, and the social stigma of a medicalized diagnosis, with limited access to non-pharmacological alternatives.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients, payer,
    powerless, immediate, trapped, local).

% Benefit from the standardization of diagnoses for billing and reimbursement purposes, even as they pay for psychotropic drugs. The DSM provides a clear framework for categorizing conditions, simplifying claims processing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_companies, beneficiary,
    institutional, biographical, arbitrage, national).

% Challenge the validity and utility of DSM categories, arguing for their social and economic construction. They face professional marginalization for questioning the dominant paradigm but continue to advocate for alternative approaches.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists, observer,
    moderate, biographical, mobile, national).

% Offer non-pharmacological treatments (e.g., psychotherapy, counseling, holistic approaches) that often do not align with DSM-driven diagnostic pathways. They struggle for insurance reimbursement and professional legitimacy within a system dominated by medicalized diagnoses.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, alternative_therapists, excluded,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature for psychiatric conditions, facilitating communication among clinicians, guiding research, and enabling insurance billing for mental health services.
% TRANSFER_FUNCTION: Transfers significant financial resources from patients (through drug purchases and insurance premiums) to pharmaceutical companies and associated medical professionals, in exchange for diagnostic labels and pharmacotherapy.
% ABSENT_VOICES: Patients whose experiences do not fit neatly into DSM categories, advocates for non-pharmacological or social interventions, and researchers challenging the biological reductionism inherent in the DSM's application. Their perspectives are often marginalized by the dominant biomedical model.
% DISAPPEARANCE_RATIONALE: If the DSM categories and their enforcement vanished overnight, the current system of psychiatric diagnosis, pharmaceutical marketing, and insurance reimbursement would collapse. This would force a radical reorganization of mental healthcare, research, and the pharmaceutical industry's engagement with psychiatry.
% FOUNDING_PROBLEM: To standardize psychiatric diagnoses, moving away from idiosyncratic descriptions and enabling systematic research and clinical practice.
% FOUNDING_PROBLEM_CORROBORATION: The American Psychiatric Association (APA) and pharmaceutical industry assert the founding problem of diagnostic standardization is still live, citing ongoing needs for reliable classification. Critical psychiatry scholars, patient advocacy groups, and independent historical and economic analyses argue that the original problem is largely solved or has been distorted, with the system now primarily serving market construction rather than objective diagnosis.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.78) because the system generates substantial profits for pharmaceutical companies and associated medical professionals, often at the expense of patient well-being and alternative care options. Suppression is very high (0.85) due to the medical authority of the DSM, the marginalization of alternative frameworks, and the vulnerability of patients within the healthcare system. Theater ratio is moderate (0.45): while some genuine diagnostic and research functions exist, a significant portion of the system's activity is performative, maintaining the illusion of objective science while serving market interests. The metrics show a clear trend of increasing extractiveness, suppression, and theatricality over the interval, reflecting the growing influence of pharmaceutical capital.
 *
 * PERSPECTIVAL GAP:
 *   The 'critical psychiatry' reading fundamentally diverges from the 'biomedical' reading, which views DSM categories as mapping to objective neurobiological entities. From the perspective of pharmaceutical companies and many mainstream psychiatrists, the DSM is a necessary scientific tool for patient care and research. From the critical perspective, it is a tool of market construction and social control. The engine's classification as 'snare' reflects the structural asymmetry and extraction identified by this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical companies and industry-funded psychiatrists are clear beneficiaries, directly profiting from the system. Insurance companies also benefit from standardized billing. Patients are the primary victims, bearing the costs and risks of medicalized diagnoses and pharmacotherapy. Critical psychiatrists and alternative therapists are marginalized or excluded, representing resistance to the dominant paradigm. The 'trapped' exit option for patients reflects their limited ability to opt out of the system once diagnosed, due to medical authority, insurance requirements, and lack of accessible alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the DSM as a 'rope' (pure coordination) or 'scaffold' (temporary support). The critical psychiatry reading argues that the coordination function (standardized diagnosis) is largely a cover, and the system is not temporary. The 'snare' classification highlights the coercive and extractive nature, where the persistence depends on suppressing alternatives and identifiable victims bear the costs, even if a superficial coordination story is maintained. The 'contested' status of the founding problem further supports this, indicating a potential drift from original mandate to a new, extractive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_function_ambiguity,
    'Is the primary function of DSM categories to objectively classify mental illness, or to construct markets for psychotropic drugs?',
    'Longitudinal studies comparing diagnostic trends with pharmaceutical sales and R&D pipelines, and historical analysis of DSM revision processes for industry influence.',
    'If market construction is primary, the ''snare'' classification is strongly reinforced. If objective classification is primary, the constraint might lean towards ''tangled_rope'' or even ''rope'' (if extraction is incidental to coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_function_ambiguity, empirical, 'Ambiguity regarding the DSM''s true functional purpose.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative mental health frameworks structural (e.g., insurance, licensing) or internalized by patients and practitioners (e.g., belief in biomedical model''s superiority)?',
    'Analysis of patient and practitioner choices in systems with mandated coverage for diverse therapies: if non-pharmacological options remain underutilized despite coverage, internalized suppression is higher.',
    'If internalized, the constraint''s effective suppression is higher and more resistant to structural reforms, as individuals carry the suppression with them even when external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative mental health approaches.').

omega_variable(
    separability_of_standardization_from_extraction,
    'Can the genuine coordination function of diagnostic standardization be achieved without the current level of pharmaceutical market construction and extraction?',
    'Examination of mental health systems in countries with different regulatory approaches to pharmaceutical marketing and diagnostic development, or the development of independent, non-industry-funded diagnostic systems.',
    'If separable, the ''snare'' classification is strongly validated, as the extraction is not inherent to the coordination. If inseparable, some extraction might be reclassified as a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_standardization_from_extraction, conceptual, 'Whether diagnostic standardization and pharmaceutical market construction are inherently linked.').


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
narrative_ontology:measurement(dsm__tr_t2004, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2004, 0.4).
narrative_ontology:measurement(dsm__tr_t2012, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2012, 0.43).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(dsm__be_t1988, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1988, 0.62).
narrative_ontology:measurement(dsm__be_t1996, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1996, 0.69).
narrative_ontology:measurement(dsm__be_t2004, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2004, 0.74).
narrative_ontology:measurement(dsm__be_t2012, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2012, 0.77).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(dsm__su_t1988, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1988, 0.72).
narrative_ontology:measurement(dsm__su_t1996, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1996, 0.78).
narrative_ontology:measurement(dsm__su_t2004, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2004, 0.82).
narrative_ontology:measurement(dsm__su_t2012, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2012, 0.84).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_patent_system).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, medical_insurance_reimbursement_rules).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_research_funding_priorities).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dsm_taxonomy_kernel'. This 'critical_psychiatry_reading' focuses on the market-driven construction of categories, contrasting with the 'biomedical_reading' (objective disease entities) and the 'neurodiversity_reading' (pathologization of natural variation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
