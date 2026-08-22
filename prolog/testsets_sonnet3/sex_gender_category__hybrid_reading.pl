% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Medical-Gatekeeping (Hybrid Biology + Transition) Reading of Sex/Gender Category Membership
 *   domain: social_ontology/legal_classification/medicine
 *
 * SUMMARY:
 *   Under the medical gatekeeping model, whether a trans woman is legally and
 *   socially recognized as a woman depends on completing a clinically
 *   supervised transition pathway — hormone therapy, often surgery,
 *   psychiatric evaluation, and a formal diagnosis of gender dysphoria —
 *   administered by gender clinics and certified by licensed medical
 *   professionals. This reading positions itself as a compromise between
 *   purely biological and purely self-identified accounts of sex/gender
 *   category membership: it offers conditional inclusion to trans women who
 *   complete the process while continuing to exclude anyone who has not,
 *   cannot, or does not wish to medicalize their transition. The arrangement
 *   functions as a genuine coordination mechanism for institutions needing an
 *   administrable, third-party-verifiable threshold, but it also concentrates
 *   gatekeeping authority in medical institutions that profit from and depend
 *   on the arrangement's persistence, and it imposes severe, unevenly
 *   distributed costs on applicants.
 *
 * KEY AGENTS:
 *   - gender_clinic_institutions: agenda_setter/beneficiary (institutional/arbitrage) — sets and administers the diagnostic threshold
 *   - transitioned_binary_conforming_trans_women: beneficiary/payer (moderate/constrained) — gains conditional recognition after absorbing gatekeeping costs
 *   - non_transitioning_trans_individuals: payer (powerless/trapped) — remains excluded regardless of stated identity
 *   - nonbinary_people: payer (powerless/trapped) — has no membership pathway under the two-box hybrid model
 *   - family_courts_and_civil_registries: observer (institutional/analytical) — applies the standard without authoring it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.61).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.68).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical-Gatekeeping (Hybrid Biology + Transition) Reading of Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/legal_classification/medicine").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '240e4646-fc72-4f67-94f0-ad2504218060').
narrative_ontology:cs_kernel_codification('240e4646-fc72-4f67-94f0-ad2504218060', distributed).
narrative_ontology:cs_authority_grounding('240e4646-fc72-4f67-94f0-ad2504218060', expertise).
narrative_ontology:cs_interpretation_layer_present('240e4646-fc72-4f67-94f0-ad2504218060').
narrative_ontology:cs_reading_relation('240e4646-fc72-4f67-94f0-ad2504218060', sex_gender_category__biology_reading, influences).
narrative_ontology:cs_reading_relation('240e4646-fc72-4f67-94f0-ad2504218060', sex_gender_category__identity_reading, influences).
narrative_ontology:cs_axiom('240e4646-fc72-4f67-94f0-ad2504218060', foundational, category_membership_requires_verified_transition).
narrative_ontology:cs_axiom_status(category_membership_requires_verified_transition, holdable).
narrative_ontology:cs_axiom_grounding('240e4646-fc72-4f67-94f0-ad2504218060', category_membership_requires_verified_transition, instrumental).
narrative_ontology:cs_axiom('240e4646-fc72-4f67-94f0-ad2504218060', secondary, clinical_diagnosis_is_legitimate_gatekeeping_authority).
narrative_ontology:cs_axiom_status(clinical_diagnosis_is_legitimate_gatekeeping_authority, holdable).
narrative_ontology:cs_axiom_grounding('240e4646-fc72-4f67-94f0-ad2504218060', clinical_diagnosis_is_legitimate_gatekeeping_authority, conventional).
narrative_ontology:cs_reference_frame('240e4646-fc72-4f67-94f0-ad2504218060', clinical_diagnostic_threshold_standard).
narrative_ontology:cs_drift_state('240e4646-fc72-4f67-94f0-ad2504218060', post_informed_consent_model_spread, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('240e4646-fc72-4f67-94f0-ad2504218060', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, gender_clinic_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_licensing_bodies).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, transitioned_binary_conforming_trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, category_administering_states).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, nonbinary_people).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, low_income_trans_applicants).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_denied_diagnosis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transitioned_binary_conforming_trans_women).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, gradated_category_membership_doctrine).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_authority_over_sex_classification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer diagnostic protocols (gender dysphoria assessment, real-life test requirements, hormone/surgical thresholds) that determine when an applicant crosses from 'biological male/female' into recognized category membership. They set the criteria, control the paperwork trans people need for legal and medical recognition, and derive institutional funding, referral pipelines, and professional authority from being the gatekeepers of the transition process.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_clinic_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, gender_clinic_institutions, beneficiary).

% Certify which clinicians may diagnose gender dysphoria and authorize transition-related care, entrenching a professional monopoly over category adjudication. They benefit from the arrangement's persistence regardless of whether it serves patients well, since the licensing structure itself is what confers their relevance.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_licensing_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Have completed hormone therapy and/or surgery and obtained diagnostic sign-off, and are thereby granted conditional recognition as women under the hybrid standard. They benefit from finally being legible to law and medicine, but only after absorbing years of gatekeeping costs, financial burden, and invasive evaluation they had no power to negotiate away.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transitioned_binary_conforming_trans_women, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, transitioned_binary_conforming_trans_women, payer).

% Use the hybrid standard to issue amended identity documents, adjudicate sex-segregated facility and sport access, and resolve legal disputes, gaining a workable administrative bright line (medical sign-off) without having to adjudicate contested philosophical questions about identity or biology directly.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, category_administering_states, beneficiary,
    institutional, generational, analytical, national).

% Identify as trans but cannot or choose not to undergo medical transition (health contraindications, cost, non-binary identification, or personal preference). Under this reading they remain categorized by natal biology regardless of stated identity, excluded from legal and social recognition in their identified category, and bear the constraint's costs without any path to relief that does not require submitting to medicalization.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Do not seek binary transition and have no category to transition into under a two-box hybrid model. The framework has no membership pathway for them at all; they are simply left outside both recognized categories, bearing exclusion the arrangement was never built to address.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, nonbinary_people, payer,
    powerless, biographical, trapped, national).

% Cannot afford the hormone therapy, surgery, therapy letters, and repeated clinical visits the gatekeeping model requires to cross the recognition threshold. Their legal and social category membership is effectively gated by wealth even though the standard is presented as medical rather than economic.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, low_income_trans_applicants, payer,
    powerless, biographical, trapped, national).

% Seek clinical sign-off but are denied by gatekeeping clinicians applying subjective or shifting diagnostic criteria (insufficient 'passing,' ambivalence about surgery, comorbid conditions read as disqualifying). They remain formally uncategorized as women despite identifying and living as such, entirely at the discretion of institutions they cannot appeal to outside the same medical hierarchy.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_denied_diagnosis, payer,
    powerless, biographical, constrained, national).

% Have views on where the line for female-category access to sex-segregated spaces should sit, but are not the ones setting the diagnostic criteria — clinics and licensing bodies set the standard, and this group's objections or endorsements enter only as external political pressure on legislatures, not as an institutional seat at the gatekeeping table.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_womens_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% Apply the hybrid standard when adjudicating identity document amendments, marriage validity, and inheritance disputes, taking medical certification as dispositive evidence without independently assessing its fairness. They observe and enforce the classification rather than authoring it.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, family_courts_and_civil_registries, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides administrators, courts, and third parties (employers, sports bodies, prisons) with a single legible bright-line test for category membership that avoids requiring lay adjudicators to evaluate contested philosophical claims about identity directly — deferring instead to medical sign-off as a proxy.
% TRANSFER_FUNCTION: Moves the cost of legibility from institutions onto individual trans and nonbinary people: applicants must absorb years of clinical evaluation, financial cost, and invasive procedures to be granted recognition, while institutions receive a low-friction, defensible administrative rule and clinics/licensing bodies receive gatekeeping authority and fee-generating referral pipelines.
% ABSENT_VOICES: Non-transitioning trans people and nonbinary people are structurally absent from the standard's design — the hybrid model was built around a two-box binary-transition template and has no mechanism to even register their objection that the category structure itself excludes them, not merely that the criteria are too strict.
% DISAPPEARANCE_RATIONALE: If the hybrid gatekeeping standard vanished overnight, courts and registries would need a new adjudication rule (falling back to either pure biology or pure self-identification, each a different constraint with a different victim set); clinics would lose a major source of institutional authority and referral revenue; and currently-excluded non-transitioning and nonbinary people would either gain recognition under a self-ID standard or remain excluded under a biology standard — the world does not stay the same, it reorganizes around whichever sibling reading fills the vacuum.
% FOUNDING_PROBLEM: Legal and medical systems needed an administrable rule for handling people whose lived gender diverged from natal sex, at a time when neither pure biological assignment nor pure self-identification was politically or institutionally acceptable to sex-segregated systems (prisons, sports, records, marriage law) — medical transition sign-off offered a documented, third-party-verifiable threshold.
% FOUNDING_PROBLEM_CORROBORATION: Clinics and licensing bodies attest the diagnostic threshold remains necessary to prevent misuse of legal reclassification. Independent bioethicists and disability-rights-adjacent scholars, along with several national human rights commissions and non-transitioning trans advocacy organizations (sources outside the beneficiary set), attest the medical necessity premise has weakened as self-ID jurisdictions report no measurable increase in bad-faith reclassification, suggesting the gatekeeping function now serves institutional and professional interests more than the originally cited verification problem.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects that a substantial share of the arrangement's operation is not verification cost but rent: clinics and licensing bodies capture referral pipelines, fee income, and professional authority whose scale exceeds what pure diagnostic accuracy would require, and the threshold has drifted (per the temporal series) from a narrower verification function toward a broader gatekeeping apparatus. Suppression (0.68) is high because exit from the arrangement is not available to those it excludes — a non-transitioning trans person or nonbinary person cannot simply opt out of being denied recognition; the suppression is structural (no institutional pathway exists) rather than merely a matter of enforcement against resistance. Theater ratio (0.42) captures that a meaningful fraction of the gatekeeping ritual (extended waiting periods, redundant evaluations, real-life test requirements in jurisdictions with weak clinical justification for them) has become performative compliance-signaling rather than functional diagnostic necessity, and this ratio has risen over the interval as evidence accumulated that the strict protocols do not measurably improve outcomes. Accessibility collapse is moderate (0.5): for those with resources, an alternative path (identity_reading jurisdictions, informal social recognition) sometimes exists in parallel, so alternatives have not collapsed completely the way they would under a pure mountain claim. Resistance is high (0.72): the excluded classes and increasingly parts of the medical establishment itself actively contest the standard's necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Gender clinics, licensing bodies, and administering states sit near the beneficiary end: they set the criteria, derive institutional authority and revenue from administering them, and are not structurally exposed to the standard's costs. Transitioned binary-conforming trans women occupy a genuinely mixed position — they are beneficiaries of the recognition eventually granted but payers of the process that gates it, which is why they carry a secondary payer role; their directionality sits closer to symmetric than either pure beneficiary or pure victim groups. Non-transitioning trans individuals, nonbinary people, low-income applicants, and those denied diagnosis sit at the full-target end: they bear the classification's costs (exclusion, financial burden, invasive evaluation, discretionary denial) with no institutional lever to alter the criteria and, for the powerless/trapped exit profile, no realistic alternative category to move to.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is neither purely coordination nor purely extraction, which is exactly what the tangled_rope classification is built to register: it does solve a real coordination problem (institutions needing a workable, documentable threshold for sex-segregated systems) while simultaneously running an asymmetric extraction machine (clinics and licensing bodies profiting from gatekeeping scaled beyond diagnostic necessity, applicants absorbing costs with no negotiating power). Classifying it as a pure snare would erase the genuine administrability problem it solves for courts and registries; classifying it as a pure rope would erase the documented drift toward theatrical, cost-inflating gatekeeping and the structural exclusion of non-transitioning and nonbinary people who have no coordination benefit at all — for them, there is no rope, only the wall.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_hybrid,
    'Is the medical-gatekeeping standard the correct resolution of the sex/gender category kernel, or is it an unstable compromise that will collapse toward either the biology_reading or the identity_reading as political and clinical consensus shifts?',
    'Track jurisdictional drift over time: count jurisdictions moving from hybrid standards toward pure self-ID (identity_reading) versus toward reinstated biological criteria (biology_reading) as courts and legislatures revisit the standard.',
    'If jurisdictions are converging toward self-ID, this reading''s victim set (non-transitioning and nonbinary people) shrinks as the identity_reading absorbs them; if converging toward biology, the reading''s beneficiary set (transitioned trans women) shrinks as the biology_reading revokes their conditional inclusion. Either shift changes which sibling constraint is doing the work this one currently does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_hybrid, conceptual, 'Whether the hybrid reading is a stable resolution or a transitional compromise between sibling kernel readings.').

omega_variable(
    diagnostic_necessity_vs_gatekeeping_capture,
    'How much of the current diagnostic threshold reflects genuine clinical necessity (preventing regret, ensuring informed consent) versus institutional capture by clinics and licensing bodies whose authority depends on the threshold remaining high?',
    'Compare outcome data (regret rates, satisfaction, harm) between jurisdictions with strict gatekeeping protocols and jurisdictions that have moved to informed-consent models with minimal gatekeeping; a null difference would indicate the gatekeeping component beyond informed consent is capture, not necessity.',
    'If outcome data show no meaningful difference, the extractiveness score understates the constraint''s true rent-seeking share; the gatekeeping apparatus would be exposed as substantially theatrical/extractive beyond its stated coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_necessity_vs_gatekeeping_capture, empirical, 'Whether the medical threshold''s stringency reflects clinical necessity or institutional self-interest.').

omega_variable(
    partial_victim_overlap_with_biology_reading,
    'To what extent does the victim set of this hybrid reading overlap with the victim set of the sibling biology_reading, given that transitioned trans women exit the biology_reading''s victim class but non-transitioning trans people remain victims under both?',
    'Cross-reference the beneficiary/victim declarations of both sibling constraint files directly; the overlap is a structural fact about the two authored stories, resolvable by comparison rather than further empirical study.',
    'Establishes that this reading is not simply ''less extractive'' than biology_reading in aggregate — it redistributes who is harmed (medicalizing exclusion for the willing/able, while leaving those unable or unwilling to medicalize just as excluded as under pure biology) rather than uniformly reducing harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partial_victim_overlap_with_biology_reading, conceptual, 'Structural comparison of victim sets across sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__hybrid_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__hybrid_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__hybrid_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(sex__tr_t32, sex_gender_category__hybrid_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__hybrid_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__hybrid_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__hybrid_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__hybrid_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(sex__be_t32, sex_gender_category__hybrid_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__hybrid_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__hybrid_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__hybrid_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__hybrid_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(sex__su_t32, sex_gender_category__hybrid_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__hybrid_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'sex/gender category membership' per the ε-invariance principle: sex_gender_category__biology_reading (immutable biological criteria), sex_gender_category__hybrid_reading (this file — medical gatekeeping/combined criteria), and sex_gender_category__identity_reading (self-identification criteria). Each reading has a distinct beneficiary/victim structure and a distinct ε — they are not three measurements of one constraint but three structurally different constraints sharing a contested kernel. The hybrid reading's ε (0.61) sits structurally between what a pure biology reading and a pure identity reading would show for their respective operations, but this is not an averaging relationship — it reflects this reading's own distinct gatekeeping-cost mechanism, which neither sibling authors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
