% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: DSM Taxonomy — Critical Psychiatry Reading: Categories Reverse-Engineered from Pharma Treatments
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual of Mental Disorders) functions
 *   as the authoritative taxonomy for psychiatric diagnosis in the US and
 *   globally. The critical psychiatry reading holds that DSM categories are
 *   not discovered natural kinds but are reverse-engineered from available
 *   pharmaceutical treatments: diagnostic boundaries expand to match drug
 *   indications, thresholds lower to enlarge treatable populations, and new
 *   categories emerge when novel compounds need markets. This reading
 *   identifies pharmaceutical capital and industry-funded psychiatrists as
 *   beneficiaries who extract profit via expanded prescribing, while patients
 *   subjected to overprescription and adverse drug effects are the victims.
 *   The coordination function (shared diagnostic language for clinical
 *   communication, insurance, research) is real but subordinate to the
 *   market-construction function. The constraint requires active enforcement
 *   through institutional mandates (insurance requires DSM codes),
 *   publication gatekeeping, and professional socialization. The timeline
 *   (0–40) tracks DSM-III (1980) through DSM-5-TR (2022), showing rising
 *   extractiveness as pharma influence grew, rising theater as the biomedical
 *   veneer thickened, and rising suppression as alternatives were
 *   marginalized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy — Critical Psychiatry Reading: Categories Reverse-Engineered from Pharma Treatments").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '1c0cff0f-1dc6-49c4-8280-c2363543d9b4').
narrative_ontology:cs_kernel_codification('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', formalized).
narrative_ontology:cs_authority_grounding('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', extraction).
narrative_ontology:cs_interpretation_layer_present('1c0cff0f-1dc6-49c4-8280-c2363543d9b4').
narrative_ontology:cs_reading_relation('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', foundational, diagnostic_categories_follow_marketable_treatments).
narrative_ontology:cs_axiom_status(diagnostic_categories_follow_marketable_treatments, holdable).
narrative_ontology:cs_axiom_grounding('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', diagnostic_categories_follow_marketable_treatments, empirically_contingent).
narrative_ontology:cs_axiom('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', secondary, biomedical_model_serves_commercial_interests).
narrative_ontology:cs_axiom_status(biomedical_model_serves_commercial_interests, holdable).
narrative_ontology:cs_axiom_grounding('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', biomedical_model_serves_commercial_interests, instrumental).
narrative_ontology:cs_reference_frame('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', dsm_iii_reliability_solution).
narrative_ontology:cs_drift_state('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', dsm_5_tr_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1c0cff0f-1dc6-49c4-8280-c2363543d9b4', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, adverse_effect_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, non_industry_clinicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Funds DSM workgroups, key opinion leaders, CME, and patient advocacy groups; profits from expanded diagnostic markets for psychotropics. Can redirect capital to other therapeutic areas if psychiatry becomes unprofitable — arbitrage-grade exit from this specific constraint.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Chair DSM workgroups, lead clinical trials, write guidelines, control residency curricula. Receive consulting fees, speaking fees, research funding, and career advancement from pharma. Exit means leaving academic psychiatry — career path dependence makes exit constrained, not mobile.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary).

% Receive diagnoses with lowered thresholds (e.g., MDD, ADHD, GAD) and corresponding prescriptions. The diagnosis becomes fused with self-concept ("I have a chemical imbalance"), making rejection of the framework existentially dislocating. Insurance, disability, and workplace accommodations require DSM codes — structural trapping compounds identity lock.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer).

% Suffer metabolic syndrome, tardive dyskinesia, sexual dysfunction, emotional blunting, withdrawal syndromes from medications prescribed for expanded diagnoses. No compensation mechanism exists; informed consent processes minimize risks. Exit means stopping medication — which risks relapse, withdrawal, and loss of disability/insurance benefits tied to the diagnosis.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, adverse_effect_patients, payer,
    powerless, biographical, trapped, national).

% Community psychiatrists, therapists, primary care doctors who use DSM codes for reimbursement but lack industry ties. Face professional pressure to follow guidelines, use approved medications, document DSM diagnoses. Can partially resist (off-label, therapy-first) but cannot opt out of the coding system.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, non_industry_clinicians, payer,
    moderate, biographical, constrained, national).

% Argue that autism, ADHD, and other categories pathologize natural variation. Excluded from DSM workgroups, NIMH funding, and mainstream clinical guidelines. Their objection is that the coordination function (shared language) serves institutional norms, not human flourishing. They operate from outside the constraint's enforcement structure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, neurodiversity_advocates, excluded,
    organized, generational, analytical, global).

% Produce the evidence base for this reading: historical analyses of DSM-pharma entanglement, critiques of trial methodology, documentation of diagnostic inflation. Neither collect nor pay — they analyze. Their work is cited in litigation and policy but does not change the constraint's operation directly.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_scholars, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic language enabling clinical communication, insurance reimbursement, research standardization, and legal/administrative decision-making across the mental health system.
% TRANSFER_FUNCTION: Moves prescription volume and revenue from patients/insurers/public health systems to pharmaceutical companies via expanded diagnostic criteria that match drug indications; moves professional status and research funding to industry-aligned psychiatrists.
% ABSENT_VOICES: Patients who recover without medication and reject the diagnostic framework; clinicians who practice non-biomedical models (psychodynamic, somatic, community-based) and are excluded from guideline-setting; indigenous and traditional healers whose frameworks are invisible to the DSM system. They are absent because the constraint's enforcement structure (insurance codes, licensing, publication) requires DSM compliance.
% DISAPPEARANCE_RATIONALE: If the DSM vanished overnight, insurance reimbursement would collapse, pharmaceutical marketing would lose its diagnostic infrastructure, clinical communication would fragment, and disability/legal systems would lose their psychiatric taxonomy. The mental health system would reorganize — possibly around ICD codes, dimensional assessments, or non-diagnostic service models. The world rearranges because the constraint is load-bearing for the current arrangement.
% FOUNDING_PROBLEM: Pre-DSM-III psychiatry suffered from diagnostic unreliability: clinicians disagreed on diagnoses, research could not replicate, insurance could not standardize reimbursement. The APA built DSM-III (1980) as an atheoretical, descriptive system to solve the reliability problem.
% FOUNDING_PROBLEM_CORROBORATION: The reliability problem was substantially solved by DSM-III's operational criteria — this is attested by research methodologists and epidemiologists outside the benefiting parties (e.g., Robins & Guze criteria, NIMH ECA studies). However, the arrangement persisted and expanded beyond reliability into validity claims that serve commercial interests. The critical psychiatry reading (this constraint) and the neurodiversity reading corroborate that the founding problem is dead but the constraint persists. The biomedical reading claims the problem is live (validity remains unfinished) — this is self-asserted by the benefiting parties.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.72: the revenue flow from expanded diagnosis to pharma profit is large and decoupled from marginal clinical benefit. Suppression 0.68: alternative frameworks (psychodynamic, social, neurodiversity) face structural exclusion from funding, journals, and training. Theater 0.45: the DSM presents as empirical science (field trials, reliability studies) but the decision criteria for category inclusion/thresholds track commercial opportunity. Accessibility_collapse 0.62: once a diagnosis enters the DSM, it becomes the default framework — alternatives require active resistance. Resistance 0.58: critical psychiatry, neurodiversity, and service user movements contest the taxonomy but lack institutional power. The claimed type tangled_rope reflects genuine coordination (clinicians need shared language) combined with asymmetric extraction (pharma profits, patients pay).
 *
 * PERSPECTIVAL GAP:
 *   From the pharmaceutical/psychiatric establishment seat, the DSM is a rope: it solves the genuine coordination problem of diagnostic reliability and treatment matching. From the patient seat, it is a snare: the coordination story is cover for extraction. From the critical clinician seat, it is a tangled rope: the coordination is real but the extraction is structural and growing. The engine computes this seat divergence from the structural data (beneficiary/victim declarations, power, exit_options) — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical capital (beneficiary, institutional power, arbitrage exit) collects the extraction — d near 0.0 (full beneficiary). Industry-funded psychiatrists (beneficiary/agenda_setter, institutional power, constrained exit via career dependence) administer and benefit — d ~ 0.15. Overprescribed patients (victim, powerless, identity_locked exit) bear costs with no exit — d near 1.0 (full target). Adverse effect patients (victim, powerless, trapped exit) suffer harms with no recourse — d near 1.0. Non-industry clinicians (payer/moderate, constrained exit) face professional pressure to conform — d ~ 0.65. Neurodiversity advocates (excluded, organized, analytical exit) are structurally excluded — their objection is the coordination function's blind spot.
 *
 * MANDATROPHY ANALYSIS:
 *   The DSM's original mandate (DSM-III, 1980) was diagnostic reliability for research and clinical communication — a genuine coordination problem. Over 40 years, the mandate has atrophied: reliability gains plateaued while diagnostic expansion accelerated, tracking pharma pipelines. The arrangement persists because pharma capital captures the gains, industry-funded psychiatrists administer the taxonomy, and no party bears enough concentrated cost to force revision. This is mandatrophy: the coordination function is now the cover for the extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the DSM taxonomy a genuine discovery of natural disease kinds, or a constructed instrument that expands diagnostic boundaries to match available treatments?',
    'Longitudinal analysis of DSM revision history against pharmaceutical patent timelines, FDA approval patterns, and industry marketing campaigns; testimony from DSM workgroup members on decision criteria.',
    'If the critical psychiatry reading is structurally true, the taxonomy operates as a market-construction mechanism (tangled_rope/snare) rather than a biomedical classification (mountain). This reading''s ε, beneficiaries, and victims are the descriptive reality; the biomedical reading''s claims are the cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading (critical_psychiatry_reading) of the contested kernel dsm_taxonomy_kernel. Sibling readings: biomedical_reading (categories map to objective neurobiological entities), neurodiversity_reading (categories pathologize natural variation). The structural delta for this reading: victim set = patients subjected to overprescription/adverse effects; beneficiary = pharmaceutical capital + industry-funded psychiatrists; extractiveness = moderate-high (profit extraction via drug sales).').

omega_variable(
    diagnostic_inflation_mechanism,
    'Is diagnostic threshold lowering driven by genuine clinical insight or by commercial pressure to expand treatable populations?',
    'Comparative analysis of threshold changes across DSM editions correlated with new drug indications, patent extensions, and key opinion leader financial disclosures.',
    'If commercial pressure is the primary driver, the constraint''s extraction is structural and its coordination function (clinical communication) is subordinate. If clinical insight drives thresholds, the extraction is incidental to genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_inflation_mechanism, empirical, 'Whether diagnostic expansion is clinically warranted or commercially constructed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-biomedical frameworks structural (funding gatekeeping, publication bias, institutional mandates) or internalized (clinicians genuinely believing the biomedical model exhausts psychiatric reality)?',
    'Post-exit trajectory of clinicians who leave academic psychiatry: if suppression persists after leaving the institutional context, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after exit. This affects directionality for the clinician seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative psychiatric frameworks.').

omega_variable(
    patient_identity_lock,
    'To what degree are patients identity-locked into the diagnostic framework through self-concept fusion ("I am my diagnosis") versus structurally trapped (insurance, disability, legal systems require DSM codes)?',
    'Longitudinal qualitative study of patients who reject their diagnosis: do they face material barriers only, or also existential dislocation?',
    'If identity_locked dominates, patient exit_options is identity_locked rather than constrained — directionality shifts toward full target (d → 1.0) and effective extraction amplifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_identity_lock, conceptual, 'Identity-lock dynamics in patient relationship to DSM categories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, information_standard).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.05).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_marketing_regulation).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_reimbursement_codes).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_research_funding).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% The dsm_taxonomy_kernel decomposes into three constraint stories with distinct ε, beneficiaries, and victims: biomedical_reading (low ε, mountain-like), critical_psychiatry_reading (moderate-high ε, tangled_rope), neurodiversity_reading (moderate ε, snare/tangled_rope). They share the same institutional infrastructure (DSM editions, APA governance, insurance codes) but instantiate different constraints because their ε referents differ: the biomedical reading assesses the taxonomy as a scientific classification (low extraction), the critical psychiatry reading assesses it as a market-construction mechanism (high extraction), the neurodiversity reading assesses it as a pathologization apparatus (moderate extraction with identity harm). Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, institutional, 0.1).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, powerless, 0.95).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, moderate, 0.65).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
