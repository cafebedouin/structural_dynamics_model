% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Medical-Gatekeeping (Hybrid) Reading of Sex/Gender Category Membership
 *   domain: social ontology / identity politics / legal classification
 *
 * SUMMARY:
 *   Under the medical-gatekeeping model, sex/gender category membership for
 *   legal, athletic, and institutional purposes is granted conditionally: an
 *   individual assigned one sex at birth may be recognized in the other
 *   category only after satisfying a threshold of medical transition
 *   (typically some combination of hormone therapy duration and/or surgical
 *   intervention) certified by medical institutions. This compromise position
 *   is often adopted by courts, sports federations, and legislatures seeking
 *   an administrable middle ground between an immutable-biology standard and
 *   a pure self-identification standard. It creates a hierarchy of
 *   recognition: transitioned individuals gain conditional membership;
 *   non-transitioning trans individuals, those priced out of care, those
 *   geographically without access, and intersex individuals whose bodies
 *   don't fit the binary transition narrative are left outside the recognized
 *   category regardless of their identity or lived circumstance.
 *
 * KEY AGENTS:
 *   - medical_gatekeeping_institutions: agenda_setter (institutional/arbitrage) — certifies transition status, collects fees, controls the threshold
 *   - transitioned_trans_women: beneficiary/payer (moderate/constrained) — gains conditional recognition at high personal cost
 *   - non_transitioning_trans_individuals: payer (powerless/trapped) — categorically excluded regardless of identity
 *   - low_income_trans_applicants: payer (powerless/trapped) — excluded by cost of the threshold, not by choice
 *   - sex_segregated_institutions_seeking_compromise: beneficiary/agenda_setter (organized/constrained) — adopts hybrid rule as litigation-resistant compromise
 *   - legislatures_and_courts: observer (institutional/analytical) — imports medical standards absent independent expertise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.61).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.58).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical-Gatekeeping (Hybrid) Reading of Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social ontology / identity politics / legal classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '1ceac116-db37-4e3f-913f-9e6aa0600916').
narrative_ontology:cs_kernel_codification('1ceac116-db37-4e3f-913f-9e6aa0600916', distributed).
narrative_ontology:cs_authority_grounding('1ceac116-db37-4e3f-913f-9e6aa0600916', expertise).
narrative_ontology:cs_interpretation_layer_present('1ceac116-db37-4e3f-913f-9e6aa0600916').
narrative_ontology:cs_reading_relation('1ceac116-db37-4e3f-913f-9e6aa0600916', sex_gender_category__biology_reading, influences).
narrative_ontology:cs_reading_relation('1ceac116-db37-4e3f-913f-9e6aa0600916', sex_gender_category__identity_reading, influences).
narrative_ontology:cs_axiom('1ceac116-db37-4e3f-913f-9e6aa0600916', foundational, medical_transition_as_legitimate_threshold).
narrative_ontology:cs_axiom_status(medical_transition_as_legitimate_threshold, holdable).
narrative_ontology:cs_axiom_grounding('1ceac116-db37-4e3f-913f-9e6aa0600916', medical_transition_as_legitimate_threshold, instrumental).
narrative_ontology:cs_axiom('1ceac116-db37-4e3f-913f-9e6aa0600916', foundational, graduated_conditional_membership_permissible).
narrative_ontology:cs_axiom_status(graduated_conditional_membership_permissible, holdable).
narrative_ontology:cs_axiom_grounding('1ceac116-db37-4e3f-913f-9e6aa0600916', graduated_conditional_membership_permissible, conventional).
narrative_ontology:cs_reference_frame('1ceac116-db37-4e3f-913f-9e6aa0600916', clinical_transition_certification_standard).
narrative_ontology:cs_drift_state('1ceac116-db37-4e3f-913f-9e6aa0600916', post_self_id_jurisdiction_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1ceac116-db37-4e3f-913f-9e6aa0600916', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, transitioned_trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, sex_segregated_institutions_seeking_compromise).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, low_income_trans_applicants).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, intersex_individuals_with_ambiguous_presentation).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_individuals_in_medical_deserts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transitioned_trans_women).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_transition_as_legitimating_threshold).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, graduated_category_membership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Endocrinologists, surgeons, gender clinics, and the diagnostic apparatus (informed by frameworks like WPATH standards) that certify whether an individual has transitioned 'enough' to qualify for reclassification. They set diagnostic criteria, control access to hormones and surgery, write letters that courts and agencies rely on, and collect fees and referral revenue throughout the process. Their institutional survival and authority depend on the gatekeeping function persisting rather than being replaced by self-identification or by a fixed-biology rule that would eliminate the need for their certification.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals who have completed hormone therapy and/or surgery and obtain conditional legal and social recognition as women under this reading. They benefit relative to the biology reading (which would exclude them entirely) but pay substantially in money, time, bodily risk, and psychological cost to meet the threshold, and their recognition can be revisited or challenged (in sport, prisons, shelters) even after transition. Exit from the requirement is not available if they want the recognition; their only path runs through the gatekeeping apparatus.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transitioned_trans_women, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, transitioned_trans_women, payer).

% People who identify outside their birth-assigned category but cannot or choose not to pursue medical transition — for reasons of cost, health contraindication, non-binary identity, age, or personal choice. Under the hybrid reading they are categorically excluded from recognition that the identity reading would grant them, while bearing all the social and legal costs of nonconformity. They have no route into the recognized category short of undergoing procedures they may not want or cannot access.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Trans individuals for whom the medical threshold is nominally available but practically unreachable: surgery costs tens of thousands of dollars, insurance frequently excludes it, and waitlists for gender clinics run years. They are held in a state of conditional non-membership not because they reject the hybrid criterion but because they cannot afford to clear it, making the gatekeeping cost itself a wealth-based exclusion mechanism layered on top of the biology/identity dispute.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, low_income_trans_applicants, payer,
    powerless, biographical, trapped, national).

% People whose bodies do not sort cleanly under either a biology-only or transition-based criterion. The hybrid reading's medical framework, built around a binary transition narrative (assigned sex A, transition to sex B), has no clean procedural slot for ambiguous or non-binary intersex presentations, leaving this group to negotiate case-by-case exceptions with the same gatekeeping institutions that were not designed with them in mind.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, intersex_individuals_with_ambiguous_presentation, payer,
    powerless, biographical, trapped, national).

% Trans people living in rural areas or jurisdictions with few or no gender-affirming care providers. The hybrid criterion requires access to a specific medical infrastructure that simply does not exist within reasonable travel distance for them, converting a geographic accident into exclusion from legal recognition regardless of their actual commitment to transition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_individuals_in_medical_deserts, payer,
    powerless, biographical, trapped, regional).

% Sports federations, prisons, shelters, and other sex-segregated institutions that adopt the hybrid criterion (often keyed to hormone levels or surgical status) as an administrable compromise between the biology and identity readings. They benefit from having a defensible, litigation-resistant middle position that lets them claim procedural fairness while still controlling admission, and they help set the specific thresholds (hormone levels, waiting periods) that operationalize the reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, sex_segregated_institutions_seeking_compromise, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, sex_segregated_institutions_seeking_compromise, agenda_setter).

% Women in sex-segregated spaces (shelters, prisons, sports) governed by the hybrid criterion but not consulted on where the medical threshold should sit. Some object that the threshold is too permissive relative to safety or fairness concerns they hold; they are affected by where institutions draw the transition-completion line but are not parties to the medical-institutional determination of that line.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_women_in_shared_spaces, excluded,
    moderate, biographical, constrained, national).

% Bodies that adjudicate disputes arising from the hybrid criterion — birth certificate amendment rules, sports eligibility litigation, prison placement challenges — often by importing medical gatekeeping standards wholesale because courts lack independent expertise to construct an alternative and defer to the credentialed institutions that produced the criterion.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides administrable institutions (courts, sports bodies, prisons, insurers) a bright-enough line to apply without adjudicating contested metaphysical or identity questions themselves: 'has this person completed a recognized medical process' is procedurally checkable in a way that 'is this person really a woman' is not. It also offers a middle path that lets institutions avoid the political cost of endorsing either pure-biology or pure-self-identification rules outright.
% TRANSFER_FUNCTION: Moves the power to certify category membership from courts, legislatures, and individuals themselves to medical institutions, and moves the cost of category access from a matter of declaration (identity reading) or birth fact (biology reading) to a matter of sustained medical, financial, and bureaucratic labor borne by the individual seeking recognition — with those costs falling hardest on people who cannot afford or access the required care.
% ABSENT_VOICES: Non-transitioning trans people and non-binary individuals who reject the binary transition narrative entirely are not represented in the standard-setting process (WPATH-style bodies, legislative hearings dominated by clinical voices); intersex individuals whose bodies don't map onto the assigned-sex-to-transitioned-sex narrative are largely absent from the criterion's design; low-income applicants who cannot afford the threshold have no seat in institutions that set cost-insensitive medical requirements.
% DISAPPEARANCE_RATIONALE: Medical institutions, some sex-segregated institutions, and some transitioned individuals would say the world rearranges badly — without a gatekeeping threshold, they argue, either an unworkable case-by-case biology dispute or an unbounded self-identification standard would replace a workable compromise. Non-transitioning trans advocates and disability/access advocates would say the world is substantially unchanged for the people currently excluded by cost or geography, since they gain nothing from the hybrid rule's disappearance unless it is replaced by an identity-based standard — the verdict depends entirely on which reading of the underlying kernel replaces this one.
% FOUNDING_PROBLEM: Legal and administrative systems needed a way to process transgender legal recognition requests (name/sex marker changes, sports eligibility, facility placement) that predated and lacked any settled self-identification framework, and needed something more administrable than birth-certificate biology once transition became medically possible and socially visible.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and sympathetic courts attest the founding problem (administrability without a settled alternative) remains live. Non-transitioning trans advocates, disability-access researchers, and some legal scholars outside the medical-gatekeeping apparatus attest that the problem has shifted: administrability could now be achieved via self-attestation (as several jurisdictions have implemented without documented harm), making continued medical gatekeeping a persistence of institutional control rather than a live necessity — this is corroborated by comparative jurisdictional data from self-ID jurisdictions, not solely by advocacy assertion.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, contested).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.61) reflects the resource, time, and bodily-risk cost extracted from applicants seeking recognition, and the exclusionary cost borne by those who cannot meet the threshold — set below a pure snare because genuine institutional coordination value exists (administrability, avoidance of ad hoc adjudication) and some applicants genuinely benefit. Suppression (0.58) is substantial because the criterion actively forecloses alternative recognition paths (self-attestation) once adopted by an institution, but it is not near-total because self-ID jurisdictions coexist elsewhere, showing the suppression is not physically necessary. Theater ratio (0.42) captures that a meaningful share of the gatekeeping apparatus's stated function (protecting institutional integrity) has drifted toward defending the gatekeeping institutions' own role rather than serving applicants or the institutions they interface with — evidenced by rising procedural requirements without corresponding rise in decision quality. Accessibility collapse is moderate (0.5): once inside the medical process, alternatives narrow sharply, but people can and do exit to self-ID advocacy or biology-based challenge, so collapse is not total. Resistance is high (0.68) reflecting sustained activist, legal, and clinical pushback from multiple directions (both from those who find the threshold too permissive and those who find it too restrictive).
 *
 * DIRECTIONALITY LOGIC:
 *   Medical gatekeeping institutions sit at the clear beneficiary end: they administer, certify, and profit from the process, and their institutional relevance depends on the criterion's persistence rather than replacement by a bright-line rule (biology) or a no-institution-needed rule (self-ID). Transitioned trans women are dual-positioned — real beneficiaries relative to exclusion, but they pay heavily in cost and risk and their recognition remains conditional and contestable, which is why they carry both beneficiary and payer roles. Non-transitioning trans individuals, low-income applicants, geographically isolated applicants, and ambiguous-presentation intersex individuals are structural targets: the hybrid criterion's line-drawing falls entirely on them, and they have no route to alter their own directionality short of undergoing procedures they may not want, cannot afford, or cannot access.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids two mislabeling failures: treating the medical threshold as pure coordination (ignoring that it produces documented victims among non-transitioning and low-income trans people) and treating it as pure extraction (ignoring that it provides real administrability value relative to unresolved disputes, and that some applicants are genuine beneficiaries of the conditional recognition it grants). The tangled_rope classification holds both: coordination function for institutions seeking a workable standard, and asymmetric extraction concentrated on those the medical threshold excludes or prices out. This is why requires_active_enforcement, beneficiaries, and victims are all populated — the constraint fails the tangled_rope gate without all three, and the structural data show all three are genuinely present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_kernel_position,
    'Is the medical-gatekeeping (hybrid) reading a principled middle position between the biology and identity readings, or is it an unstable compromise that inherits the exclusionary victim set of the biology reading (for non-transitioners) while adding new victims (cost/access-excluded applicants) not present in either sibling reading?',
    'Compare victim-set overlap across all three sibling constraint files: if the hybrid reading''s victims are a strict superset of the biology reading''s victims plus a novel cost/access-excluded population, the hybrid reading is structurally more extractive than either pure alternative, not a genuine midpoint.',
    'If the hybrid reading is shown to inherit rather than reduce exclusion, its tangled_rope classification would tilt toward snare on review, since the coordination benefit would be shown to accrue mainly to institutions rather than to applicants broadly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_kernel_position, conceptual, 'Whether the hybrid reading is a genuine compromise or a compounding of exclusions from the sibling readings.').

omega_variable(
    medical_threshold_arbitrariness,
    'Is the specific medical threshold (hormone duration, surgical completion) that operationalizes this reading a principled marker of category-relevant change, or an arbitrary line whose main function is to give institutions a defensible cutoff regardless of its substantive connection to the properties (athletic performance, safety, etc.) the threshold is invoked to protect?',
    'Empirical review of whether outcomes the threshold purports to track (e.g., athletic performance parameters) actually track the specific threshold chosen, versus tracking other variables (age of transition onset, individual physiology) not captured by the binary threshold.',
    'If the threshold is shown to be poorly correlated with the properties it is invoked to protect, the coordination-function claim weakens substantially and the theater_ratio should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_threshold_arbitrariness, empirical, 'Whether the specific medical criteria used are substantively justified or administratively arbitrary.').

omega_variable(
    authority_concentration_ambiguity,
    'Should the concentration of category-determination authority in medical institutions (rather than courts, legislatures, or individuals) be read as an appropriate deference to clinical expertise, or as an extraction of adjudicatory power by institutions with a financial and professional stake in the gatekeeping function persisting?',
    'Track whether medical institutions'' own recommended thresholds have moved toward or away from applicant-favorable standards over time as their financial exposure to gatekeeping services has changed, and whether jurisdictions with fully separated (non-financially-interested) medical advisory bodies produce different thresholds than jurisdictions where the certifying and treating institutions are one and the same.',
    'If certifying and treating institutions are structurally the same profit-interested party, the authority_grounding classification of ''expertise'' becomes harder to distinguish from ''extraction'', which would shift the cs_structure interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_concentration_ambiguity, conceptual, 'Whether medical authority in this domain is disinterested expertise or self-interested gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__hybrid_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__hybrid_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__hybrid_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__hybrid_reading, base_extractiveness, 25, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__hybrid_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__hybrid_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'sex/gender category membership' per the ε-invariance principle. The biology_reading (category fixed at birth, no transition path — different ε, different victim set including all trans individuals regardless of transition status) and the identity_reading (category by self-attestation — different ε, different victim set limited to those contesting self-ID's legitimacy in specific institutional contexts) are separate constraints, each with independently authored metrics. This hybrid_reading file's ε (0.61) sits structurally between measuring a different arrangement than either sibling — it is not an average or blend of their values but reflects this reading's own distinct gatekeeping-cost and partial-exclusion structure. All three files are linked via affects_constraints because a shift in institutional adoption of one reading changes the legitimacy and resource base of the others (e.g., growing adoption of self-ID standards reduces the perceived necessity of the medical-gatekeeping compromise, exerting downstream pressure captured in this reading's drift_state).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
