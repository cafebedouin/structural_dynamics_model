% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Medical Gatekeeping Model of Sex/Gender Category Membership (Hybrid Reading)
 *   domain: social ontology/legal classification/identity politics
 *
 * SUMMARY:
 *   Under the medical gatekeeping model, category membership is determined by
 *   a combination of natal biology and medically certified social transition:
 *   a trans person is conditionally admitted to the category after completing
 *   a professionally administered pathway of diagnosis, hormone therapy, and
 *   often surgery, certified by medical institutions and honored by legal
 *   recognition authorities. The model solves a real adjudication problem
 *   (uniform determination of contested membership claims) while charging a
 *   heavy toll for passage and excluding outright those who do not or cannot
 *   traverse the pathway. KEY AGENTS (by structural relationship):
 *   medical_gatekeeping_professions: agenda-setting administrator and
 *   principal collector ([institutional]/[arbitrage]);
 *   legal_recognition_authorities: secondary agenda-setter
 *   ([institutional]/[mobile]); transitioning_trans_persons: primary
 *   cost-bearing participant with conditional benefit
 *   ([moderate]/[identity_locked]); non_transitioning_trans_individuals:
 *   excluded cost-bearer ([powerless]/[trapped]); cisgender_category_members:
 *   incidental beneficiary ([organized]/[mobile]);
 *   trans_advocacy_organizations: organized resistance seat
 *   ([organized]/[analytical]); international_human_rights_bodies: external
 *   pressure seat ([institutional]/[analytical]). This story instantiates ONE
 *   reading of the sex_gender_category kernel; the sibling readings are
 *   separate constraints (see kernel_context and
 *   network.dual_formulation_note). The claimed type and the metrics are
 *   independent authored facts: I claim tangled_rope because I judge the
 *   adjudication function genuine and the extraction asymmetric and enforced;
 *   the metrics record what I take to be descriptively true of the
 *   arrangement's operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.64).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.56).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical Gatekeeping Model of Sex/Gender Category Membership (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social ontology/legal classification/identity politics").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '45e14b94-f93f-4a7b-b886-dc6a180f8350').
narrative_ontology:cs_kernel_codification('45e14b94-f93f-4a7b-b886-dc6a180f8350', formalized).
narrative_ontology:cs_authority_grounding('45e14b94-f93f-4a7b-b886-dc6a180f8350', expertise).
narrative_ontology:cs_interpretation_layer_present('45e14b94-f93f-4a7b-b886-dc6a180f8350').
narrative_ontology:cs_reading_relation('45e14b94-f93f-4a7b-b886-dc6a180f8350', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('45e14b94-f93f-4a7b-b886-dc6a180f8350', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('45e14b94-f93f-4a7b-b886-dc6a180f8350', foundational, category_entry_requires_medical_certification).
narrative_ontology:cs_axiom_status(category_entry_requires_medical_certification, holdable).
narrative_ontology:cs_axiom_grounding('45e14b94-f93f-4a7b-b886-dc6a180f8350', category_entry_requires_medical_certification, conventional).
narrative_ontology:cs_axiom('45e14b94-f93f-4a7b-b886-dc6a180f8350', secondary, medical_screening_safeguards_category_stability).
narrative_ontology:cs_axiom_status(medical_screening_safeguards_category_stability, holdable).
narrative_ontology:cs_axiom_grounding('45e14b94-f93f-4a7b-b886-dc6a180f8350', medical_screening_safeguards_category_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('45e14b94-f93f-4a7b-b886-dc6a180f8350', medically_certified_category_entry).
narrative_ontology:cs_drift_state('45e14b94-f93f-4a7b-b886-dc6a180f8350', contemporary_informed_consent_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('45e14b94-f93f-4a7b-b886-dc6a180f8350', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_professions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_category_members).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transitioning_trans_persons).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, transitioning_trans_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Psychiatrists, endocrinologists, and gender clinics define the diagnostic criteria, run the assessment pathways, and issue the certificates that recognition authorities accept. Multi-year assessment periods, repeated sessions, and specialist referrals route every applicant through their services, with evaluation and treatment billed along the way. Their professional bodies write and revise the standards that determine what the pathway requires. Leaving is frictionless: the same credentials and practices transfer to any adjacent clinical market.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_professions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, medical_gatekeeping_professions, beneficiary).

% Courts, civil registries, and government departments apply the certification standard when deciding whose documents, records, and facility assignments change. They did not originate the medical requirement but administer and defend it, and they can amend or replace it by ordinary legislation or case law, as several jurisdictions have done by moving to statutory self-declaration.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_recognition_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Trans women and men who seek recognition under this model must obtain diagnoses, undergo hormone therapy and often surgical procedures, and sustain years of documented assessment before their category membership is accepted. They pay in money, bodily intervention, delay, and disclosure risk. The recognition they seek is dispensed by the same pathway that charges them, so walking away means remaining unrecognized in the category they live as; their attachment to the category is not optional, it is the thing they are asking to be admitted to.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transitioning_trans_persons, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, transitioning_trans_persons, beneficiary).

% Trans people who do not access or do not want medical transition, for reasons of health, income, conviction, or unwillingness, have no route to recognition under this model at all. No amount of testimony, documentation, or lived experience substitutes for the medical certificate. They bear the model's burdens, including misdocumented records, facility exclusion, and disclosure risk, while being ineligible for what the pathway dispenses.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% People assigned the category at birth and never questioned in it. The model maintains a boundary around their category that is adjudicated by medicine rather than declaration; some members explicitly value that boundary, others regard it as irrelevant to them or unjust to those excluded. Their own membership never depends on the pathway, so they can ignore it, oppose it, or defend it at no personal cost.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_category_members, beneficiary,
    organized, generational, mobile, national).

% Campaign and legal organizations pressing for depathologization, shorter pathways, and self-declaration statutes. They litigate, lobby, and document pathway harms; they collect nothing from the pathway and bear its costs only through the people they represent.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_advocacy_organizations, observer,
    organized, generational, analytical, continental).

% Strasbourg and treaty-body jurists and rapporteurs who have reviewed compulsory-sterilization and forced-treatment requirements, found them rights violations, and pressured national models toward shorter, less invasive pathways. They hold no domestic enforcement power; their leverage is jurisprudential and reputational.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_gatekeeping_professions).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform adjudication procedure for contested category-membership claims across documents, civil registries, facilities, and records: an applicant's claim is decided by a medically certified transition standard rather than by case-by-case dispute.
% TRANSFER_FUNCTION: Moves money (assessment and treatment fees), time (multi-year pathways), and decision authority over bodies and identities from trans persons seeking recognition to medical institutions; moves boundary certainty and a defensible decision procedure to recognition authorities and category incumbents.
% ABSENT_VOICES: Non-transitioning trans individuals are structurally absent from standard-setting panels: the pathway is designed by the professions that administer it and the authorities that honor it, and the people it excludes outright are not seated when criteria are written. Intersex persons and detransitioned former applicants are likewise rarely consulted. Their objection, that membership should not require medical intervention, is recorded here as commentary only.
% DISAPPEARANCE_RATIONALE: If the certified-pathway standard vanished overnight, registries, document systems, facility rules, and sports and custodial classifications would all need a replacement determination rule immediately; the referral pipelines feeding gender clinics would lose their legal mandate, and thousands of pending applications would convert into either automatic recognitions or fresh contests. The arrangements of every seated party depend on the standard's existence.
% FOUNDING_PROBLEM: Mid-twentieth-century clinical management of transsexualism: how to gate access to scarce, irreversible, poorly understood medical interventions, and how to open a legal recognition channel consistent with then-unquestioned binary categories, without flooding either the clinics or the registries.
% FOUNDING_PROBLEM_CORROBORATION: The scarcity-gating half of the founding problem is attested dead by sources outside the benefiting parties: European Court of Human Rights jurisprudence (Goodwin v UK 2002; A.P., Garcon et Nicot v France 2017, striking compulsory sterilization), treaty-body findings, and WPATH's own successive standards revisions, which abandoned the gatekeeping framing the professions once enforced. The recognition-channel half is attested live by the same bodies and by trans-led scholarship documenting continued demand. No party inside the medical beneficiary set originally made these attestations; they were compelled by litigation and external review.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.64: the pathway's costs (fees, multi-year waits, interventions some applicants do not clinically want, disclosure exposure) are decoupled from any service the applicant independently sought, and the excluded non-transitioning population bears the model's burdens with zero offsetting benefit. Suppression is 0.56 as a raw structural property, unscaled by power or scope: refusal of recognition is the enforcement instrument, and alternative determination rules are suppressed within hybrid-model jurisdictions even though they operate elsewhere. Theater ratio is 0.40 and rising across the series: as informed-consent practice spread, a growing share of pathway activity became hoop-jumping (box-ticking sessions, waiting-list triage functioning as de facto rationing, documentary rituals) rather than substantive clinical judgment, a Goodhart drift signature. Accessibility collapse is 0.55: alternatives (self-declaration statutes, social transition without legal change) demonstrably persist in some jurisdictions, so the constraint does not present as natural law, but within a hybrid jurisdiction the certified pathway is nearly the sole road to recognition. Resistance is 0.62: sustained litigation, campaigning, and external human-rights pressure have visibly bent the arrangement (sterilization requirements struck down, pathways shortened) without displacing it. The measurement series run on one shared seven-point grid (1979-2026) with all three tracked metrics authored at every point. The trajectories are cyclical rather than monotonic: extraction and suppression declined steadily from 1979 to 2017 under depathologization and rights pressure, then re-hardened after roughly 2020 as backlash politics, tightened eligibility scrutiny, and renewed gatekeeping rhetoric reversed part of the drift. The oscillation is partly an extraction mechanism in itself: periodic loosening recruits applicants into the pathway, and subsequent tightening raises the sunk-cost stakes of those already inside it. Suppression_requirement is tracked deliberately because enforcement capacity genuinely changed over the interval, from near-total clinical discretion, through codified panel review, toward informed-consent practice, and back toward intensified scrutiny; the series traces that machinery, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the medical professions' seat the arrangement is therapeutic stewardship they built and revise in good faith; from the transitioning applicant's seat the same structure operates as a toll booth positioned on the only road to a good they cannot abandon; from the non-transitioning person's seat it is a locked door with the toll sign still posted. The identity-lock dynamic is specific here: transitioning applicants are identity_locked not by ideology but by the object of the transaction itself, since the category membership they seek is constitutive of the life they are trying to live, and exiting the pathway means exiting the claim. Were that frame to break (if recognition were decoupled from the pathway), the lock dissolves instantly, which is precisely why pathway defense concentrates on preserving the coupling. Coalition potential among the powerless is real but indirect: non-transitioning trans individuals hold little individual leverage, and their effective power arrives aggregated through advocacy organizations and external juridical bodies rather than through any seat they occupy directly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: medical_gatekeeping_professions sit near the beneficiary pole (they collect fees and authority and face no cost), cisgender_category_members nominally so, transitioning_trans_persons near the target pole, non_transitioning_trans_individuals at the full-target extreme (all burden, no benefit, no exit). Two overrides are warranted because the structural derivation misses dual-positioned reality. First, transitioning_trans_persons: a victim-primary derivation would place them near d=0.9, but they are also the arrangement's intended recipients, receiving the recognition good the pathway dispenses; their true position is target-weighted but materially subsidized at the far end, so d=0.70. Second, cisgender_category_members: a beneficiary derivation would place them near d=0.15, but their benefit is diffuse, unrequested, and actively contested by many of them, and they bear none of the pathway's costs while collecting none of its revenues; d=0.35 reflects a weaker, partly nominal subsidy than the derivation assumes. Scope amplification of effective extraction (national scope, verification difficulty) is computed by the engine from the scope atoms; suppression enters unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate had two halves: gating scarce, irreversible interventions, and operating a recognition channel. The first half is substantially dead, interventions are less scarce, understanding has matured, and informed-consent practice demonstrates that gatekeeping rigor was never proportionate to clinical necessity, yet the gate's authority persists, increasingly theatrical in jurisdictions where assessment has thinned into ritual (hence the rising theater_ratio). The second half remains live: recognition demand is real and someone must adjudicate it. The classification prevents symmetric mislabeling: reading the arrangement as pure extraction erases the genuine adjudication service every jurisdiction needs; reading it as pure coordination hides the toll and the excluded population. The R5 mismatch consumer will read founding_problem_status=contested against disappearance_verdict=world_rearranges: the arrangement is not a zombie (its recognition function is load-bearing) but its gatekeeping form is mandate-surplus, which is exactly the tangled-rope tension the corpus exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates the hybrid_reading of the sex_gender_category kernel; are its victim set, authority concentration, and epsilon stable properties of the arrangement, or artifacts of instantiating this reading rather than a sibling?',
    'Comparative compilation of the sibling stories (biology_reading, identity_reading) and cross-reading audit of victim-set overlap and epsilon deltas against the declared structural expectations.',
    'Adopting the biology_reading collapses the victim set to all trans persons and removes the medical beneficiary seat entirely; adopting the identity_reading empties the gatekeeping-cost victim class and dissolves the medical authority concentration. Classification of THIS file is invariant, but family-level conclusions rotate with reading selection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame omega: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    medical_necessity_vs_toll,
    'Are the pathway''s required interventions and assessment periods substantively therapeutic for the applicant, or a toll extracted as the price of recognition?',
    'Outcomes research comparing recognized cohorts who completed mandated components with matched cohorts recognized without them, plus cost accounting of assessment against comparable non-gatekept clinical services.',
    'If the requirements are a toll, the extractive component is rent layered on a coordination service and the tangled-rope reading hardens toward the snare boundary; if substantially therapeutic, part of the measured extraction is the price of care the applicants independently need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_toll, empirical, 'Whether gatekeeping requirements track clinical necessity or act as a passage toll.').

omega_variable(
    exclusion_boundary_necessity,
    'Does excluding non-transitioning trans individuals serve a genuine category-coordination need, or does it chiefly enforce medical authority over the membership boundary?',
    'Compare boundary-integrity outcomes in self-declaration jurisdictions (where the excluded class gains membership without medical certification) against hybrid jurisdictions on the metrics boundary-defenders actually cite: facility incidents, sports fairness disputes, registry error rates.',
    'If exclusion survives no functional test, the excluded victim class is produced by extraction rather than coordination, and the constraint''s coordination claim narrows to the transitioning population alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_boundary_necessity, conceptual, 'Whether the outright-excluded victim set is intrinsic to the coordination function or to the gatekeeping form.').

omega_variable(
    backlash_ratchet_vs_cycle,
    'Is the post-2017 re-hardening of pathways and eligibility scrutiny a durable enforcement ratchet or one phase of a recurring loosen-tighten cycle?',
    'Longitudinal tracking of legislative amendments, judicial rulings, and standards revisions across at least two further electoral and standards cycles.',
    'A ratchet trajectory pushes the arrangement toward the snare boundary (rising suppression on a captured gain flow); a cyclical trajectory keeps it a tangled rope whose extraction oscillates with political weather, and dates any type transition differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backlash_ratchet_vs_cycle, empirical, 'Whether the observed U-shaped extraction curve is ratcheting or oscillating.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is structural (legal refusal of recognition, documentary exclusion) versus internalized (shame and self-pathologization carried by applicants from the pathologization era, persisting even where rules relax)?',
    'Post-reform suppression trajectory: compare help-seeking, disclosure, and recognition-claim rates in cohorts entering after depathologization reforms against earlier cohorts; persistent deficits after barrier removal indicate internalized residue.',
    'If a substantial share is internalized, effective suppression exceeds the structural measure and relaxations of the written rules will under-deliver relief; the constraint''s hold on the trapped seat is then partly carried in the targets themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the suppression scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 1979, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t1979, sex_gender_category__hybrid_reading, theater_ratio, 1979, 0.22).
narrative_ontology:measurement(sex__tr_t1990, sex_gender_category__hybrid_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement(sex__tr_t2002, sex_gender_category__hybrid_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(sex__tr_t2007, sex_gender_category__hybrid_reading, theater_ratio, 2007, 0.33).
narrative_ontology:measurement(sex__tr_t2011, sex_gender_category__hybrid_reading, theater_ratio, 2011, 0.36).
narrative_ontology:measurement(sex__tr_t2017, sex_gender_category__hybrid_reading, theater_ratio, 2017, 0.39).
narrative_ontology:measurement(sex__tr_t2026, sex_gender_category__hybrid_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t1979, sex_gender_category__hybrid_reading, base_extractiveness, 1979, 0.7).
narrative_ontology:measurement(sex__be_t1990, sex_gender_category__hybrid_reading, base_extractiveness, 1990, 0.69).
narrative_ontology:measurement(sex__be_t2002, sex_gender_category__hybrid_reading, base_extractiveness, 2002, 0.66).
narrative_ontology:measurement(sex__be_t2007, sex_gender_category__hybrid_reading, base_extractiveness, 2007, 0.63).
narrative_ontology:measurement(sex__be_t2011, sex_gender_category__hybrid_reading, base_extractiveness, 2011, 0.59).
narrative_ontology:measurement(sex__be_t2017, sex_gender_category__hybrid_reading, base_extractiveness, 2017, 0.57).
narrative_ontology:measurement(sex__be_t2026, sex_gender_category__hybrid_reading, base_extractiveness, 2026, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t1979, sex_gender_category__hybrid_reading, suppression_requirement, 1979, 0.78).
narrative_ontology:measurement(sex__su_t1990, sex_gender_category__hybrid_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(sex__su_t2002, sex_gender_category__hybrid_reading, suppression_requirement, 2002, 0.68).
narrative_ontology:measurement(sex__su_t2007, sex_gender_category__hybrid_reading, suppression_requirement, 2007, 0.62).
narrative_ontology:measurement(sex__su_t2011, sex_gender_category__hybrid_reading, suppression_requirement, 2011, 0.57).
narrative_ontology:measurement(sex__su_t2017, sex_gender_category__hybrid_reading, suppression_requirement, 2017, 0.53).
narrative_ontology:measurement(sex__su_t2026, sex_gender_category__hybrid_reading, suppression_requirement, 2026, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, identity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sex/gender category rule' decomposes, per the epsilon-invariance principle, into three structurally distinct determination rules: biology_reading (immutable reproductive biology decides; victim set: all trans persons; negligible gatekeeping infrastructure), hybrid_reading (THIS story: biology plus medically certified transition; victim set partially overlapping, high gatekeeping costs, authority concentrated in medical institutions), and identity_reading (self-identification decides; victim set: those contesting self-declared boundaries). Each has its own epsilon, beneficiaries, and enforcement profile; measuring one with another's observable produces a different epsilon and therefore a different constraint. Family linkage: biology_reading is upstream (historically prior; its biological component is cited as this reading's foundation), and this reading exerts downstream pressure on identity_reading debates by supplying the 'reasonable middle' against which self-declaration proposals are argued. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, moderate, 0.7).
constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
