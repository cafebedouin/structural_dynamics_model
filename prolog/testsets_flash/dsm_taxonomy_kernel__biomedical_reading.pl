% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__biomedical_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Taxonomy as Objective Biomedical Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the 'biomedical reading' of the DSM taxonomy,
 *   asserting that its categories correspond to objective neurobiological
 *   disease entities. This reading underpins the current psychiatric
 *   paradigm, enabling medical interventions and shaping public understanding
 *   of mental health. The high extractiveness reflects the costs borne by
 *   individuals diagnosed under this system, including involuntary treatment,
 *   stigma, and the medicalization of normal human variation. The high
 *   suppression is due to the institutional power of the psychiatric
 *   establishment and pharmaceutical industry in defining and enforcing this
 *   paradigm, often marginalizing alternative perspectives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Taxonomy as Objective Biomedical Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, 'eded1f61-ec5b-42c4-99a2-77b494a42e05').
narrative_ontology:cs_kernel_codification('eded1f61-ec5b-42c4-99a2-77b494a42e05', formalized).
narrative_ontology:cs_authority_grounding('eded1f61-ec5b-42c4-99a2-77b494a42e05', lineage).
narrative_ontology:cs_interpretation_layer_present('eded1f61-ec5b-42c4-99a2-77b494a42e05').
narrative_ontology:cs_reading_relation('eded1f61-ec5b-42c4-99a2-77b494a42e05', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_reading_relation('eded1f61-ec5b-42c4-99a2-77b494a42e05', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('eded1f61-ec5b-42c4-99a2-77b494a42e05', foundational, mental_disorders_are_brain_diseases).
narrative_ontology:cs_axiom_status(mental_disorders_are_brain_diseases, holdable).
narrative_ontology:cs_axiom_grounding('eded1f61-ec5b-42c4-99a2-77b494a42e05', mental_disorders_are_brain_diseases, empirically_contingent).
narrative_ontology:cs_axiom('eded1f61-ec5b-42c4-99a2-77b494a42e05', foundational, dsm_categories_reflect_objective_reality).
narrative_ontology:cs_axiom_status(dsm_categories_reflect_objective_reality, holdable).
narrative_ontology:cs_axiom_grounding('eded1f61-ec5b-42c4-99a2-77b494a42e05', dsm_categories_reflect_objective_reality, empirically_contingent).
narrative_ontology:cs_reference_frame('eded1f61-ec5b-42c4-99a2-77b494a42e05', biomedical_disease_model).
narrative_ontology:cs_drift_state('eded1f61-ec5b-42c4-99a2-77b494a42e05', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eded1f61-ec5b-42c4-99a2-77b494a42e05', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_criteria).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, disseminates, and enforces the DSM diagnostic criteria. Benefits from the authority and funding associated with identifying and treating 'diseases.' Their professional identity is deeply tied to the biomedical model.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Develops and markets psychotropic medications. Benefits directly from the expansion of diagnostic categories that create new markets for their products. Actively funds research and advocacy that supports the biomedical model.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Schools, workplaces, and legal systems that use DSM diagnoses to manage or exclude individuals whose behavior deviates from norms. Benefits from the medicalization of non-conformity, which provides a framework for intervention or removal.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    organized, biographical, mobile, national).

% Are labeled with a disease entity, often leading to involuntary treatment, loss of legal capacity, social stigma, and lifelong reliance on medication. Their identity can become fused with the diagnosis, making exit from the medical system difficult.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_criteria, payer,
    powerless, biographical, identity_locked, local).

% Bear the emotional, financial, and social costs of managing a family member's 'illness.' Often pressured to accept the biomedical model and its prescribed treatments, with limited access to alternative frameworks or support.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed_individuals, payer,
    moderate, generational, constrained, local).

% Challenge the biomedical model, arguing that diagnoses are social constructs serving institutional interests. Their perspectives are often marginalized within mainstream psychiatric discourse and funding structures.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_advocates, excluded,
    organized, generational, constrained, global).

% Argue that many DSM categories pathologize natural human variation. Seek to reframe conditions as differences rather than diseases, challenging the fundamental premise of the biomedical reading. Their voices are often excluded from diagnostic manual revisions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature for mental health conditions, enabling communication among clinicians, researchers, and insurance providers, and guiding treatment decisions based on a shared understanding of 'disease' entities.
% TRANSFER_FUNCTION: Transfers authority over mental distress and behavioral variation from individuals and communities to medical professionals, enabling the flow of resources (funding, prescriptions) to the psychiatric and pharmaceutical industries, and imposing costs (stigma, treatment, loss of autonomy) on diagnosed individuals.
% ABSENT_VOICES: Neurodiversity advocates and critical psychiatry scholars are largely excluded from the official diagnostic process; they would argue that the categories are arbitrary, harmful, or socially constructed, rather than objective biomedical facts.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy as objective biomedical entities vanished overnight, the entire edifice of psychiatric diagnosis, pharmaceutical treatment, insurance billing, and legal frameworks for mental health would collapse. Clinical practice would become highly fragmented, research funding would be reallocated, and the social understanding of mental distress would undergo a radical transformation, likely leading to a re-evaluation of what constitutes 'illness' versus 'variation.'
% FOUNDING_PROBLEM: The problem of inconsistent and unreliable diagnosis in psychiatry, leading to difficulties in research, treatment, and communication among clinicians.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment and pharmaceutical industry attest that the problem of diagnostic reliability and validity remains live, justifying ongoing research and revisions. However, critical psychiatry and neurodiversity advocates contest this, arguing that the problem has shifted from diagnostic inconsistency to the reification of categories that lack clear biological markers, and that the 'solution' has become the problem itself. Independent meta-analyses of diagnostic reliability and validity studies offer mixed corroboration, often showing lower inter-rater reliability than claimed.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading enables significant transfers of autonomy, resources, and social control from individuals to the medical-industrial complex. Suppression (0.90) is also very high, as the institutional authority of psychiatry, backed by legal and economic power, actively suppresses alternative explanations for distress and non-conformity. The theater ratio (0.20) is relatively low, as the system genuinely aims to identify and treat 'diseases,' but a portion of its activity is performative, maintaining the illusion of objective biomedical discovery where empirical evidence is often lacking. Accessibility collapse (0.75) is substantial, as once a diagnosis is applied, alternative explanations or non-medical paths to understanding distress become difficult to access or legitimize. Resistance (0.40) is moderate, coming from patient advocacy groups, critical scholars, and neurodiversity movements, but it faces significant institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the psychiatric establishment, this is a necessary framework for scientific progress and patient care (claimed as a Rope or even Mountain of objective science). From the perspective of diagnosed individuals and critical observers, it operates as a Snare, extracting autonomy and resources under the guise of medical necessity. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment, pharmaceutical industry, and institutions requiring conformity are clear beneficiaries, gaining authority, market share, and social control. Individuals meeting diagnostic criteria and their families are the primary payers, bearing the direct costs of diagnosis and treatment, and often experiencing identity-lock. Critical psychiatry and neurodiversity advocates are excluded, as their perspectives challenge the foundational premise of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validation_of_categories,
    'To what extent do DSM categories map to objective, empirically verifiable neurobiological disease entities?',
    'Longitudinal neuroimaging studies, genetic research, and biomarker discovery that consistently identify distinct biological underpinnings for each diagnostic category, independent of behavioral presentation.',
    'If strong empirical validation emerges, the extractiveness might be re-evaluated as a necessary cost of treating genuine disease, potentially shifting the classification towards a Tangled Rope or even Rope. If validation remains weak, the Snare classification is reinforced, highlighting the constructed nature of the categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validation_of_categories, empirical, 'Uncertainty regarding the objective biological reality of DSM categories.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional power, legal frameworks) or internalized (diagnosed individuals internalizing the ''sick'' identity)?',
    'Post-diagnosis trajectory of individuals offered non-medical frameworks and support: if suppression persists after the medical-institutional mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true liberation more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in psychiatric diagnosis.').

omega_variable(
    framing_of_distress,
    'Is mental distress fundamentally a biomedical illness, a social construct, or a form of human variation?',
    'This is a conceptual and preference-based question, not empirically resolvable. Resolution would require a societal shift in values and epistemic frameworks regarding human experience.',
    'A shift towards social construction or neurodiversity framings would fundamentally alter the perceived legitimacy and necessity of the biomedical reading, likely reducing its extractiveness and suppression in the long term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_distress, conceptual, 'Fundamental conceptual disagreement on the nature of mental distress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.1).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, mental_health_insurance_coverage_rules).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_commitment_laws).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_research_funding_priorities).

% DUAL FORMULATION NOTE:
% This is one reading of the 'dsm_taxonomy_kernel'; other readings (neurodiversity, critical psychiatry) offer alternative structural analyses of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
