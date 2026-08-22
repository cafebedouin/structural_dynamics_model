% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Legal Sex Category Membership Determined by Immutable Reproductive Biology
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint story instantiates the biology_reading of the contested
 *   sex_gender_category kernel. It asserts that legal membership in the
 *   category 'woman' (and 'man') is determined exclusively by immutable
 *   reproductive biology observed at birth (chromosomes, gametes, anatomy).
 *   This reading forecloses gender identity as a criterion, excludes trans
 *   women from the 'woman' category, forces trans men into it, and compels
 *   intersex and nonbinary people into a binary they do not fit. The
 *   constraint is claimed as tangled_rope: it performs a genuine coordination
 *   function (bright-line administrability for sex-segregated institutions)
 *   while extracting severely from trans, intersex, and nonbinary people
 *   through active enforcement (legislation, litigation, administrative
 *   policy). The sibling readings (identity_reading, hybrid_reading) are
 *   separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - cisgender_women: Primary beneficiary (organized/constrained) — rely on sex-segregated protections
 *   - sex_based_rights_advocates: Primary agenda_setter (institutional/arbitrage) — architect the legal rule
 *   - institutional_gatekeepers: Secondary agenda_setter/beneficiary (institutional/constrained) — implement the rule
 *   - transgender_women: Primary payer (moderate/identity_locked) — excluded from woman category
 *   - transgender_men: Primary payer (moderate/identity_locked) — forced into woman category
 *   - intersex_individuals: Primary payer (powerless/trapped) — forced into binary by biology
 *   - nonbinary_individuals: Primary payer (powerless/trapped) — no category exists for them
 *   - medical_gatekeepers: Excluded (organized/constrained) — clinical judgment overridden
 *   - human_rights_bodies: Observer (institutional/analytical) — monitor compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.78).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.82).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Legal Sex Category Membership Determined by Immutable Reproductive Biology").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '99df67a6-605c-4e84-a0d4-a1e6a9240a94').
narrative_ontology:cs_kernel_codification('99df67a6-605c-4e84-a0d4-a1e6a9240a94', formalized).
narrative_ontology:cs_authority_grounding('99df67a6-605c-4e84-a0d4-a1e6a9240a94', extraction).
narrative_ontology:cs_interpretation_layer_present('99df67a6-605c-4e84-a0d4-a1e6a9240a94').
narrative_ontology:cs_reading_relation('99df67a6-605c-4e84-a0d4-a1e6a9240a94', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('99df67a6-605c-4e84-a0d4-a1e6a9240a94', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('99df67a6-605c-4e84-a0d4-a1e6a9240a94', foundational, legal_sex_is_determined_by_immutable_biology_at_birth).
narrative_ontology:cs_axiom_status(legal_sex_is_determined_by_immutable_biology_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('99df67a6-605c-4e84-a0d4-a1e6a9240a94', legal_sex_is_determined_by_immutable_biology_at_birth, deontological).
narrative_ontology:cs_axiom('99df67a6-605c-4e84-a0d4-a1e6a9240a94', foundational, women_s_sex_based_rights_require_exclusion_of_male_bodied_people).
narrative_ontology:cs_axiom_status(women_s_sex_based_rights_require_exclusion_of_male_bodied_people, holdable).
narrative_ontology:cs_axiom_grounding('99df67a6-605c-4e84-a0d4-a1e6a9240a94', women_s_sex_based_rights_require_exclusion_of_male_bodied_people, deontological).
narrative_ontology:cs_reference_frame('99df67a6-605c-4e84-a0d4-a1e6a9240a94', sex_class_legal_formalism).
narrative_ontology:cs_drift_state('99df67a6-605c-4e84-a0d4-a1e6a9240a94', post_self_id_legal_recognition_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('99df67a6-605c-4e84-a0d4-a1e6a9240a94', '').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cisgender_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, sex_based_rights_advocates).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, institutional_gatekeepers).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, transgender_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, transgender_men).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, nonbinary_individuals).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_is_binary_and_immutable).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, sex_based_protections_require_biological_criteria).
narrative_ontology:constraint_vindicates(sex_gender_category__biology_reading, women_s_rights_are_sex_based_not_identity_based).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on sex-segregated spaces (prisons, shelters, sports, changing rooms) and sex-based legal protections (Title IX, equality law). View biology-based categorization as essential to preserving these protections. Exit options constrained because alternative frameworks (self-ID) are experienced as erasing the material basis of their rights; relocation or legal challenge is costly and socially penalized.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cisgender_women, beneficiary,
    organized, generational, constrained, global).

% Advocate for and draft legislation, policy, and litigation that enshrine biological sex as the sole criterion for legal category membership. Includes feminist organizations, legal groups, and some religious institutions. Hold institutional power through courts, legislatures, and policy networks. Can shift jurisdiction or strategy (arbitrage exit) but are invested in the current framing.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, sex_based_rights_advocates, agenda_setter,
    institutional, generational, arbitrage, global).

% Administrators of prisons, sports federations, schools, hospitals, and bureaucracies who implement sex classification rules. Benefit from clear, administrable bright-line rules (biology at birth) that reduce discretion and liability. Constrained exit: their roles require implementing whatever the legal standard is; dissent risks professional sanction.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, institutional_gatekeepers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, institutional_gatekeepers, beneficiary).

% Excluded from 'woman' legal category regardless of transition, social recognition, or lived experience. Denied access to women's spaces, services, and protections; face heightened violence, discrimination, and bureaucratic erasure. Identity-locked exit: gender identity is constitutive of self; 'opting out' of womanhood is not a live option. The constraint extracts dignity, safety, and legal recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, transgender_women, payer,
    moderate, biographical, identity_locked, global).

% Assigned female at birth but identify as men. Forced into 'woman' legal category by biology-based rule, despite male presentation and social recognition. Lose access to male spaces; face forced disclosure, misgendering, and exclusion from men's protections. Identity-locked exit: manhood is constitutive; cannot 'choose' to be women.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, transgender_men, payer,
    moderate, biographical, identity_locked, global).

% Biological sex characteristics do not fit binary male/female categories. Forced into binary classification at birth (often via non-consensual surgery) and locked into that assignment legally. No exit: biology itself resists the binary, but the constraint imposes it. Trapped by medical-legal history and lack of recognition pathways.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, global).

% Identify outside the man/woman binary entirely. Biology-based rule forces binary classification with no third option. Denied legal recognition, forced misgendering on documents, excluded from both men's and women's spaces where sex segregation applies. Trapped: no biological or legal pathway to recognized status.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, nonbinary_individuals, payer,
    powerless, biographical, trapped, global).

% Clinicians who assess gender transition (hormones, surgery) under a medical-gatekeeping model (the hybrid_reading). Excluded from legal category determination under biology_reading — their professional judgment is overridden by birth assignment. Would argue for clinical nuance; constrained by licensing and institutional norms.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_gatekeepers, excluded,
    organized, biographical, constrained, national).

% UN treaty bodies, regional courts, NHRIs monitoring compliance with non-discrimination, privacy, and bodily autonomy standards. Increasingly interpret sex-based discrimination to include gender identity. Analytical seat: they observe and opine but do not directly administer the constraint in domestic law.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, administratively simple rule for assigning legal sex category at birth, enabling sex-segregated institutions (prisons, sports, shelters, bathrooms) and sex-based statistical monitoring to operate without case-by-case adjudication of identity claims.
% TRANSFER_FUNCTION: Transfers the costs of boundary enforcement (exclusion, misgendering, violence, legal invisibility) from cisgender women and institutional gatekeepers onto transgender, intersex, and nonbinary individuals. The biology rule externalizes the harm of rigid classification onto those whose bodies or identities do not fit it.
% ABSENT_VOICES: Transgender youth (especially pre-pubertal), detransitioners (whose experiences are weaponized by both sides), global majority trans communities (especially in criminalizing jurisdictions), and intersex infants subjected to early surgery — none are present in the legislative/litigation rooms where biology-based rules are crafted.
% DISAPPEARANCE_RATIONALE: If biology-based legal sex classification vanished overnight, every sex-segregated institution would need new eligibility rules; anti-discrimination law would shift to gender identity or mixed criteria; data collection on sex would require redesign; the legal architecture of 'women's rights' would be contested and rebuilt. The world rearranges because the constraint structures institutions, not just perceptions.
% FOUNDING_PROBLEM: Historical need for a clear, objective, administratively tractable criterion to operationalize sex-based protections (voting, property, education, employment) and sex-segregated institutions in a pre-genetic, pre-hormonal-therapy era where birth anatomy was the only available proxy.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and feminist scholars outside the beneficiary set (e.g., Joan Scott, Katherine Franke, Dean Spade) document that sex classification was always a legal technology of governance, not a natural fact; the 'bright line' was constructed to serve state administrative capacity and patriarchal family law. The beneficiary set (sex-based rights advocates) asserts the founding problem (male violence against females) remains live and requires biological criteria.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) is high because the constraint transfers severe harms (exclusion from protections, violence, bureaucratic erasure, forced medicalization) onto structurally vulnerable populations while the coordination benefit (administrative simplicity) could be achieved with less extractive designs (self-declaration with safeguards). Suppression (0.82) is very high: the constraint's persistence depends on active legislative bans, litigation, administrative refusal, and social enforcement against alternatives (self-ID, medical gatekeeping). Theater ratio (0.45) is moderate-high: the 'bright line' justification is real but increasingly performative as the administrative burden of alternatives (e.g., self-ID with sports policy carve-outs) is demonstrated manageable elsewhere. Accessibility collapse (0.72) is high: once the biology rule is understood as the legal standard, alternatives (medical transition, social recognition, identity documents) are legally foreclosed — but not conceptually collapsed, as evidenced by functioning alternative systems in other jurisdictions. Resistance (0.68) is substantial: trans rights movements, human rights bodies, medical associations, and some feminist formations actively contest the rule.
 *
 * PERSPECTIVAL GAP:
 *   The biology_reading computes as tangled_rope from the payer seats (trans, intersex, nonbinary people) because extraction is high, suppression is active, and coordination benefit to them is near zero. From the agenda_setter/beneficiary seats (cis women, sex-based rights advocates, institutional gatekeepers), the same constraint computes closer to rope or scaffold: the coordination function (protecting female-only space) is experienced as genuine and necessary, extraction is not felt (they are not the targets), and suppression is experienced as defense rather than offense. The engine computes this seat divergence from the structural data — the declared beneficiaries, victims, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from declared beneficiaries and victims. Cisgender women and sex-based rights advocates are beneficiaries: they collect the protective value of sex-segregated spaces and legal categories without bearing the exclusion costs. Institutional gatekeepers are dual-positioned: they benefit from administrative simplicity but are constrained implementers. Transgender women, transgender men, intersex individuals, and nonbinary individuals are victims: they bear the full extraction (exclusion, misclassification, violence, erasure) with identity-locked or trapped exit options — they cannot 'opt out' of their gender/sex identity. Medical gatekeepers are excluded: their professional role (assessing transition readiness) is structurally displaced by the biology rule. Human rights bodies are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative tractability of sex classification for sex-based protections) is contested: the original administrative necessity (birth anatomy as only proxy) has been obsoleted by medical transition, legal self-declaration models, and decoupled policy design (e.g., sports eligibility by hormone levels, not birth certificates). Yet the constraint persists and intensifies (rising extractiveness, suppression, theater). This is mandatrophy: the mandate (protect women via biology) has outlived its administrative function but is maintained because it now serves as a boundary-making device for political coalition and identity. The classification prevents mislabeling: without the tangled_rope frame, the coordination function (bright-line administrability) would be mistaken for pure extraction (snare) or the extraction would be dismissed as necessary coordination cost (rope). The tangling is the point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_framing_kernel_reading,
    'Is the biology_reading a genuine reading of a shared kernel (sex_gender_category) or a distinct constraint masquerading as a reading?',
    'Compare structural cores: do all three readings share the same referent (legal sex category assignment) and differ only in the criterion (biology vs identity vs hybrid)? If yes, they are readings of one kernel. If the biology_reading''s referent is ''sex'' and identity_reading''s referent is ''gender'', they are different kernels.',
    'If distinct kernels, the network.affects_constraints links are mis-specified; each should stand alone. If shared kernel, the reading_relations and axioms in cs_structure correctly model the committer frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_kernel_reading, conceptual, 'Whether the three declared readings share a single kernel or constitute three separate constraints.').

omega_variable(
    coordination_extractiveness_separability,
    'Can the coordination function (bright-line administrability for sex-segregated institutions) be achieved without the extractive biology rule?',
    'Empirical study of jurisdictions with self-declaration legal gender recognition (Argentina, Malta, Ireland, New Zealand, 15+ US states): do sex-segregated institutions collapse, or do they adapt via policy carve-outs (e.g., sport by testosterone, prisons by risk assessment)?',
    'If coordination is separable from biology rule, the biology_reading''s tangled_rope classification is confirmed — extraction is not necessary for coordination. If inseparable, the biology_reading may be closer to rope (extraction as price of coordination) or scaffold (transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extractiveness_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    intersex_suppression_mechanism,
    'Is the suppression of intersex individuals under the biology rule structural (legal-binary enforcement) or internalized (medicalization as ''correction'')?',
    'Longitudinal study of intersex adults in jurisdictions with vs. without third-sex/nonbinary legal options: does suppression (forced assignment, non-consensual surgery, legal erasure) persist after legal binary enforcement ends?',
    'If internalized, the constraint''s effective suppression is higher than structural measure suggests — intersex individuals carry the suppression after legal exit. Affects omega-weighted classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intersex_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for intersex individuals.').

omega_variable(
    identity_locked_vs_trapped_differentiation,
    'Do transgender women/men (identity_locked exit) and intersex/nonbinary individuals (trapped exit) experience materially different effective extraction under the same biology rule?',
    'Compare outcomes: legal recognition access, violence rates, mental health, economic status across the four victim groups in biology-rule jurisdictions. Identity-locked agents may have some mitigation (medical transition, social recognition) even under legal exclusion; trapped agents have none.',
    'If effective extraction differs significantly, the engine''s directionality derivation (which treats identity_locked and trapped as distinct exit_options) is validated. If not, the exit_options taxonomy needs refinement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_trapped_differentiation, empirical, 'Whether identity_locked and trapped exit options produce measurably different extraction experiences under the same constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex_gender_bio_tr_t1970, sex_gender_category__biology_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(sex_gender_bio_tr_t1990, sex_gender_category__biology_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(sex_gender_bio_tr_t2000, sex_gender_category__biology_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(sex_gender_bio_tr_t2010, sex_gender_category__biology_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(sex_gender_bio_tr_t2015, sex_gender_category__biology_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(sex_gender_bio_tr_t2020, sex_gender_category__biology_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(sex_gender_bio_tr_t2025, sex_gender_category__biology_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(sex_gender_bio_be_t1970, sex_gender_category__biology_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(sex_gender_bio_be_t1990, sex_gender_category__biology_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(sex_gender_bio_be_t2000, sex_gender_category__biology_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(sex_gender_bio_be_t2010, sex_gender_category__biology_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(sex_gender_bio_be_t2015, sex_gender_category__biology_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(sex_gender_bio_be_t2020, sex_gender_category__biology_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(sex_gender_bio_be_t2025, sex_gender_category__biology_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sex_gender_bio_su_t1970, sex_gender_category__biology_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(sex_gender_bio_su_t1990, sex_gender_category__biology_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(sex_gender_bio_su_t2000, sex_gender_category__biology_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(sex_gender_bio_su_t2010, sex_gender_category__biology_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(sex_gender_bio_su_t2015, sex_gender_category__biology_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(sex_gender_bio_su_t2020, sex_gender_category__biology_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(sex_gender_bio_su_t2025, sex_gender_category__biology_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel decomposes into three constraint stories: biology_reading (this file), identity_reading, hybrid_reading. They share the referent 'legal sex/gender category membership criterion' but have distinct ε (biology_reading: 0.78 high extraction; identity_reading: ~0.15 low extraction; hybrid_reading: ~0.45 moderate), distinct beneficiary/victim sets, and distinct types. biology_reading forecloses identity_reading within any single legal framework (mutually exclusive criteria); biology_reading influences hybrid_reading (biology rule makes medical gatekeeping irrelevant for legal category); identity_reading and hybrid_reading coexist_with each other (different jurisdictions adopt different models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__biology_reading, organized, 0.15).
constraint_indexing:directionality_override(sex_gender_category__biology_reading, institutional, 0.1).
constraint_indexing:directionality_override(sex_gender_category__biology_reading, moderate, 0.9).
constraint_indexing:directionality_override(sex_gender_category__biology_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
