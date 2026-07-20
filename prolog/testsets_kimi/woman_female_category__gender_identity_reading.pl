% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Self-Identification as Determinant of Woman/Female Category Membership
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The constraint under examination is the legal and social rule that
 *   membership in the category woman/female is determined exclusively by
 *   internal self-identification with that gender category, operating
 *   independently of biological sex characteristics. It is one reading of the
 *   contested kernel woman_female_category. The rule functions as a
 *   coordination mechanism for transgender individuals seeking recognition
 *   and protection, while simultaneously extracting dignity and recognition
 *   costs from those who inhabit the category, including trans women in
 *   female-only spaces where their presence becomes contested. The
 *   agenda-setting institutions enforce this boundary through
 *   anti-discrimination law, administrative policy, and social sanction,
 *   actively suppressing alternative sex-based categorizations. The
 *   structural asymmetry between the beneficiary seat and the payer seat is
 *   the axis along which the engine computes divergent per-seat
 *   classifications.
 *
 * KEY AGENTS:
 *   - transgender_individuals: Primary beneficiary (moderate/identity_locked) â gains recognition and access through self-identification
 *   - self_identified_women: Primary payer (organized/constrained) â bears dignity and recognition harms from contested category boundaries
 *   - gender_policy_institutions: Agenda setter (institutional/analytical) â administers and enforces the self-identification criterion
 *   - sex_based_rights_advocates: Excluded voice (organized/constrained) â argues for biological sex boundary but kept out of institutional deliberation
 *   - clinical_medicine: Analytical observer (institutional/analytical) â tracks clinical tension between sex and identity categorization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.72).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.68).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Self-Identification as Determinant of Woman/Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '87f400f2-e23f-4e63-a2a1-1b7b8d123274').
narrative_ontology:cs_kernel_codification('87f400f2-e23f-4e63-a2a1-1b7b8d123274', distributed).
narrative_ontology:cs_authority_grounding('87f400f2-e23f-4e63-a2a1-1b7b8d123274', expertise).
narrative_ontology:cs_interpretation_layer_present('87f400f2-e23f-4e63-a2a1-1b7b8d123274').
narrative_ontology:cs_reading_relation('87f400f2-e23f-4e63-a2a1-1b7b8d123274', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('87f400f2-e23f-4e63-a2a1-1b7b8d123274', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('87f400f2-e23f-4e63-a2a1-1b7b8d123274', foundational, self_identification_determines_womanhood).
narrative_ontology:cs_axiom_status(self_identification_determines_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('87f400f2-e23f-4e63-a2a1-1b7b8d123274', self_identification_determines_womanhood, deontological).
narrative_ontology:cs_reference_frame('87f400f2-e23f-4e63-a2a1-1b7b8d123274', self_determination_baseline).
narrative_ontology:cs_drift_state('87f400f2-e23f-4e63-a2a1-1b7b8d123274', contemporary_backlash_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('87f400f2-e23f-4e63-a2a1-1b7b8d123274', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, self_identified_women).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, gender_identity_distinct_from_sex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose gender identity differs from their sex assigned at birth. Under this constraint they gain legal and social category membership by self-declaration, enabling access to sex-protected spaces, services, and identity documents without medical gatekeeping. Their access depends on the constraint's enforcement against competing sex-based definitions.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, national).

% Individuals who identify as women, encompassing both cisgender and transgender women. They inhabit the category woman which under this constraint is determined solely by self-identification. Trans women in female-only spaces face particular scrutiny and dignity costs; cis women experience category dissolution concerns and potential loss of sex-based protections. All bear recognition harms when the category becomes politically contested.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, self_identified_women, payer,
    organized, biographical, constrained, national).

% Government bodies, human rights tribunals, and administrative agencies that write and enforce guidelines determining that self-identification is the sole criterion for woman/female category membership. They process identity claims, adjudicate disputes, and sanction service providers who use biological sex as a criterion.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_policy_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Advocacy groups and individuals who maintain that biological sex is material to the category woman/female and that sex-based protections require sex-determined boundaries. They are routinely excluded from policy consultations where self-identification is adopted and their positions are characterized as discriminatory or exclusionary by dominant institutional actors.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, sex_based_rights_advocates, excluded,
    organized, generational, constrained, national).

% Hospitals, clinics, and medical researchers who record patient sex for clinical purposes while navigating legal requirements to recognize gender identity. They observe divergent outcomes when biological sex is masked by identity-based categorization in screening, diagnosis, and epidemiology, but do not set the legal category rule.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, clinical_medicine, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for transgender individuals to obtain legal and social recognition of their gender identity without medical gatekeeping, enabling access to sex-protected spaces, services, and anti-discrimination coverage based on self-declared category membership.
% TRANSFER_FUNCTION: Moves legal status, spatial access, and recognition rights from a biologically-based categorical system to one governed by subjective self-identification, concentrating dignity and recognition costs on those who inhabit or depend on the category woman.
% ABSENT_VOICES: Sex-based rights advocates and gender-critical feminists who argue that biological sex is immutable and relevant for certain protections; also medical professionals who maintain that sex is binary and clinically significant for health outcomes. They are excluded from progressive policy tables and their objections are treated as discriminatory.
% DISAPPEARANCE_RATIONALE: If the rule vanished, transgender individuals would lose identity-based legal protections and access mechanisms, rearranging their world. Simultaneously, sex-based categorical boundaries would reassert in law and policy, rearranging the world of women who currently experience category dissolution. The parties dispute which rearrangement counts as harm or restoration.
% FOUNDING_PROBLEM: Transgender individuals historically faced exclusion from legal recognition, healthcare, and anti-discrimination protections due to medicalized gatekeeping and biological sex requirements.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations and legal scholars outside the directly benefiting population attest to historical exclusion; however, gender-critical feminists and some clinicians outside the beneficiary set contest that the founding problem justifies the current constraint, arguing it erodes sex-based rights.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, contested).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint generates substantial dignity and recognition harms for individuals in the woman category; the category becomes a site of contestation rather than protection, and the extraction is amplified for trans women in female-only spaces where invasive scrutiny is highest. Suppression (0.68) reflects the active legal and social suppression of sex-based alternative definitions, including hate speech regulation and administrative penalties for sex-based exclusion. Theater ratio (0.55) is moderate-high: much institutional activity around the constraint is symbolic or declarative (pronoun policies, statement-based inclusion) rather than material changes to safety or resource distribution. Accessibility collapse (0.65) captures that once the self-identification rule is adopted, biological sex alternatives become socially and legally inaccessible in progressive institutional contexts. Resistance (0.70) is high due to sustained gender-critical feminist opposition, legislative pushback, and public contestation. The measurement series show extraction and theater accumulating over a twenty-year interval as the rule spread from activist demands to institutional policy and then encountered backlash, hardening enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional agenda-setter seat, the constraint is a necessary human rights coordination mechanism protecting a vulnerable minority. From the self-identified women payer seat, the same constraint dissolves a category they depend on for safety and recognition, replacing it with a contested boundary that exposes them to scrutiny and harm. Transgender beneficiaries experience the constraint as protective coordination, yet trans women in female-only spaces also appear in the victim set because the constraint's operation makes their presence the focal point of public contestation, generating dignity harms that the coordination benefit does not fully offset. The engine computes these divergences from the structural data rather than from any authored reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals are declared beneficiaries: the constraint is structurally designed to subsidize their access to recognition and spaces, so their directionality sits near the beneficiary pole. Self-identified women are declared victims: the constraint extracts dignity and recognition from their categorical position, particularly in spaces where boundaries are contested, pushing their directionality toward the target pole. Gender policy institutions sit near analytical because they administer the constraint without being personally governed by it. Sex-based rights advocates are excluded from the arrangement's benefits and deliberations, giving them high directionality as external targets of suppression. The engine will compute effective extraction accordingly: damped for beneficiaries, amplified for trapped or constrained targets.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the declared victim set, this constraint might compute as a rope: it coordinates a genuine protection problem for transgender individuals. Without the declared beneficiary set, it might compute as a snare: it enforces a category boundary that extracts from women. The Tangled Rope classification is mandated by the simultaneous presence of both a coordination function (identity-based protections) and asymmetric extraction (dignity/recognition harms on category members). The active enforcement requirement prevents misreading informal social norms as structural extraction; the constraint persists only because institutions actively suppress sex-based alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the gender_identity_reading logically foreclose its sibling readings (sex_biology_reading, hybrid_contextual_reading) or merely coexist with them as competing political frameworks?',
    'Analysis of whether any single legal framework can simultaneously hold self-identification and biological sex as determinants; comparative reading of the sibling constraint stories.',
    'If foreclosed, the kernel is a zero-sum political contest; if coexistent, hybrid arrangements are structurally possible and the constraint''s extraction profile may be context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Committer-frame ambiguity about the logical relationship between kernel readings.').

omega_variable(
    dignity_harm_mechanism,
    'Are the dignity and recognition harms experienced by self-identified women intrinsic to the self-identification rule itself, or are they caused by social backlash and incomplete institutionalization of the rule?',
    'Cross-jurisdictional comparison: jurisdictions with strong self-identification enforcement and low backlash versus jurisdictions with high backlash.',
    'If intrinsic, extraction is structurally inseparable from the constraint and epsilon is inherent; if extrinsic, the constraint may be reformable toward lower extraction without abandoning the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_harm_mechanism, empirical, 'Whether dignity harms are endogenous to the rule or caused by external resistance.').

omega_variable(
    enforcement_dependency,
    'Does category membership by self-identification require ongoing active legal and social enforcement to persist, or would it stabilize as a self-sustaining social convention if enforcement ceased?',
    'Observation of informal social recognition patterns in communities with minimal legal enforcement versus those with strong institutional mandates.',
    'If enforcement-dependent, the constraint is confirmed as tangled_rope; if self-sustaining, classification shifts toward rope and the victim set may represent transient adjustment costs rather than structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependency, empirical, 'Whether the constraint''s persistence depends on active suppression of alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woman_female_gi_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(woman_female_gi_tr_t5, woman_female_category__gender_identity_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(woman_female_gi_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(woman_female_gi_tr_t15, woman_female_category__gender_identity_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(woman_female_gi_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(woman_female_gi_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(woman_female_gi_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(woman_female_gi_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(woman_female_gi_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(woman_female_gi_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woman_female_gi_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woman_female_gi_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(woman_female_gi_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(woman_female_gi_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(woman_female_gi_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'woman_female_category'. The kernel decomposes into three structurally distinct claims about category membership determinants: gender identity (this file), biological sex (sex_biology_reading), and hybrid contextual determination (hybrid_contextual_reading). Each reading has a different beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
