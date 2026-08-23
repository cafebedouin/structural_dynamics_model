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
 *   human_readable: Sex/Gender Category Membership via Medical Gatekeeping
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   The hybrid reading of sex/gender category membership holds that category
 *   inclusion is determined by a combination of biological sex and completed
 *   medical transition. Trans women who have undergone specified medical
 *   interventions (hormones, surgery, legal documentation changes) are
 *   recognized as women for legal and social purposes; those who have not are
 *   classified by birth sex. This reading emerged as a legal and
 *   institutional compromise between biology-only and identity-only models,
 *   concentrating authority in medical gatekeeping institutions that verify
 *   transition completion. The constraint operates as a tangled rope: it
 *   coordinates access to single-sex spaces and legal protections (benefiting
 *   cisgender women and legal administrators) while extracting high
 *   compliance costs from trans individuals who must navigate medical
 *   gatekeeping, and excluding non-binary individuals entirely. Extraction
 *   has risen over the measurement interval as medical requirements have
 *   expanded, wait times have increased, and the identity-reading challenge
 *   has prompted defensive enforcement.
 *
 * KEY AGENTS:
 *   - cisgender_women_claiming_protected_space: Primary beneficiary (institutional/moderate) — gains coordinated single-sex provisions
 *   - medical_gatekeeping_institutions: Agenda setter/beneficiary (institutional/arbitrage) — controls the transition verification pathway, collects professional authority and resource flows
 *   - legal_administrators_of_single_sex_provisions: Agenda setter (institutional/analytical) — implements and enforces the classification rule
 *   - trans_women_pre_medical_transition: Primary victim (powerless/trapped) — excluded from category despite identity, cannot access gate
 *   - trans_women_denied_medical_access: Victim (powerless/trapped) — medically unable to satisfy gate requirements
 *   - non_binary_individuals_excluded_from_both_categories: Victim (powerless/identity_locked) — structurally unrepresentable in binary system
 *   - trans_women_post_medical_transition: Conditional beneficiary/payer (moderate/constrained) — gained inclusion after bearing full gatekeeping cost
 *   - gender_critical_feminists: Observer/excluded (organized/constrained) — reject hybrid model as insufficient, advocate biology reading
 *   - trans_rights_advocates: Observer/excluded (organized/constrained) — reject hybrid model as extractive, advocate identity reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.78).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership via Medical Gatekeeping").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'a66f4b8b-4485-46b0-9611-f2bdac170a46').
narrative_ontology:cs_kernel_codification('a66f4b8b-4485-46b0-9611-f2bdac170a46', formalized).
narrative_ontology:cs_authority_grounding('a66f4b8b-4485-46b0-9611-f2bdac170a46', extraction).
narrative_ontology:cs_interpretation_layer_present('a66f4b8b-4485-46b0-9611-f2bdac170a46').
narrative_ontology:cs_reading_relation('a66f4b8b-4485-46b0-9611-f2bdac170a46', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('a66f4b8b-4485-46b0-9611-f2bdac170a46', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('a66f4b8b-4485-46b0-9611-f2bdac170a46', foundational, medical_transition_modifies_sex_category).
narrative_ontology:cs_axiom_status(medical_transition_modifies_sex_category, holdable).
narrative_ontology:cs_axiom_grounding('a66f4b8b-4485-46b0-9611-f2bdac170a46', medical_transition_modifies_sex_category, instrumental).
narrative_ontology:cs_axiom('a66f4b8b-4485-46b0-9611-f2bdac170a46', foundational, binary_categories_are_administratively_necessary).
narrative_ontology:cs_axiom_status(binary_categories_are_administratively_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a66f4b8b-4485-46b0-9611-f2bdac170a46', binary_categories_are_administratively_necessary, conventional).
narrative_ontology:cs_reference_frame('a66f4b8b-4485-46b0-9611-f2bdac170a46', medical_gatekeeping_compromise).
narrative_ontology:cs_drift_state('a66f4b8b-4485-46b0-9611-f2bdac170a46', contemporary_identity_politics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a66f4b8b-4485-46b0-9611-f2bdac170a46', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_women_claiming_protected_space).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, legal_administrators_of_single_sex_provisions).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_pre_medical_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_denied_medical_access).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_binary_individuals_excluded_from_both_categories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, trans_women_post_medical_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_post_medical_transition).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, biological_sex_is_determinative_except_where_medically_modified).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, category_membership_requires_institutional_verification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on single-sex provisions (shelters, prisons, sports, changing rooms) for safety and fairness. The hybrid model gives them a verifiable boundary: only those who have completed medical transition are included. They did not design the medical gate but benefit from its exclusionary function. Their exit is constrained — they cannot individually opt out of the classification system, and collective political exit requires overcoming the institutional compromise.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cisgender_women_claiming_protected_space, beneficiary,
    organized, generational, constrained, national).

% Control the pathway to category reassignment through diagnostic criteria, hormone prescription, surgical referral, and legal documentation support. This authority generates professional status, resource flows (clinical revenue, research funding), and institutional legitimacy. They can reform the gate from within (and have, e.g., reducing surgical requirements in some jurisdictions) but face pressure from both biology and identity flanks. Their exit is arbitrage-grade: they could shift to informed-consent models without losing professional authority.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, beneficiary).

% Implement and enforce the classification rule across prisons, schools, sports bodies, public facilities, and identity documents. They need a clear, administrable rule — the hybrid model provides a documentary threshold (gender recognition certificate, amended birth certificate) that is legally defensible. They are constrained by legislation and litigation; their exit would require legislative change they do not control.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_administrators_of_single_sex_provisions, agenda_setter,
    institutional, generational, constrained, national).

% Identify as women but are classified as men under the hybrid model until they complete medical transition. They bear the full cost of the gate (wait times, medical gatekeeping, financial cost, bodily risk) while receiving none of the category benefits. Their exit is trapped: they cannot access the category without the gate, and the gate is controlled by institutions that do not answer to them. Many face heightened vulnerability in male-classified spaces (prisons, shelters) while awaiting transition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_pre_medical_transition, payer,
    powerless, biographical, trapped, national).

% Medically unable to satisfy transition requirements (health contraindications, financial barriers, geographic lack of providers, age restrictions). They are permanently excluded from category recognition regardless of identity or social transition. The constraint extracts their social and legal misclassification as the price of maintaining the gate's integrity. No exit exists within the binary system.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_denied_medical_access, payer,
    powerless, biographical, trapped, national).

% Do not identify as men or women, but the hybrid model (like both sibling readings) only recognizes binary categories. They are structurally unrepresentable — no medical pathway exists to a non-binary legal category under this model. Their exclusion is not a side effect but a structural necessity of the binary frame. Exit is identity-locked: accepting the binary frame would require denying their identity; rejecting it means permanent legal invisibility.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_binary_individuals_excluded_from_both_categories, payer,
    powerless, biographical, identity_locked, national).

% Have completed the medical gate and now hold legal recognition as women. They benefit from the coordination (access to single-sex provisions, correct documentation) but bear the historical extraction cost (years of gatekeeping, medical risk, financial burden). Their situation is dual: they are now beneficiaries of the constraint they once paid into. Exit is constrained — they could detransition but would lose the recognition they paid for; they cannot reform the gate for those behind them without risking their own status.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_post_medical_transition, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, trans_women_post_medical_transition, payer).

% Advocate the biology_reading: sex category is immutable and the hybrid model concedes the principle. They are excluded from the compromise — their position is that no medical intervention can change sex category. They organize politically to roll back the hybrid model. Exit is constrained: they operate within a legal system that has adopted the hybrid model; changing it requires legislative victories against medical and trans-rights coalitions.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_critical_feminists, excluded,
    organized, generational, constrained, national).

% Advocate the identity_reading: self-identification should suffice for category membership. They are excluded from the compromise — their position is that medical gatekeeping is a human rights violation. They organize politically to replace the hybrid model with self-ID. Exit is constrained: they operate within a legal system that has adopted the hybrid model; changing it requires legislative victories against medical and gender-critical coalitions.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally administrable, medically verifiable boundary for single-sex provisions that acknowledges trans existence without abandoning biological reference — coordinates access to shelters, prisons, sports, documentation across jurisdictions and institutions.
% TRANSFER_FUNCTION: Moves the cost of category verification from institutions to trans individuals (who must undergo medical transition), and moves category recognition from trans individuals to medical/legal gatekeepers (who control the verification pathway). Non-binary individuals pay the cost of structural invisibility.
% ABSENT_VOICES: Trans youth blocked from medical transition by age restrictions; trans people in jurisdictions without medical transition access; intersex individuals whose biology doesn't fit the binary the hybrid model modifies; detransitioners whose category status becomes ambiguous under the model. These voices are structurally excluded because the model only recognizes binary categories with a medical gateway.
% DISAPPEARANCE_RATIONALE: If the hybrid model vanished overnight, single-sex provisions would lose their administrable boundary. Biology-reading jurisdictions would revert to birth-sex classification; identity-reading jurisdictions would adopt self-ID. Trans women post-transition would lose legal recognition in some jurisdictions and retain it in others. Medical gatekeeping institutions would lose their adjudicative authority over category membership. The entire legal architecture of gender recognition certificates would collapse.
% FOUNDING_PROBLEM: Legal systems needed a rule for trans people's category membership that preserved single-sex provisions for cisgender women while acknowledging that some trans people live permanently in a gender different from birth assignment. The biology reading excluded all trans people; the identity reading (not yet politically viable) included all self-identified trans people. The hybrid model was built as the enactable compromise: medical transition as the threshold.
% FOUNDING_PROBLEM_CORROBORATION: The hybrid model's founding compromise is attested by the legislative history of the UK Gender Recognition Act 2004, the Spanish Ley Trans 2023 (which moved from hybrid to self-ID), and the WPATH Standards of Care evolution. Corroboration from outside the beneficiary set: the European Court of Human Rights (Christine Goodwin v. UK) recognized the need for legal recognition but left the gatekeeping threshold to states; the UN Independent Expert on SOGI has criticized medical gatekeeping as a human rights violation. No neutral observer attests the founding problem is solved — both flanking readings remain live political forces.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.78) is high because the constraint demands extensive medical, legal, and social compliance from trans individuals as the price of category recognition — costs that are not reciprocally borne by cisgender members. Suppression (0.72) is high because the constraint's persistence depends on active enforcement of medical gatekeeping (controlling access to hormones/surgery, policing documentation, litigating edge cases) and on suppressing the identity-reading alternative that would dissolve the gate. Theater ratio (0.41) is moderate: the medical verification process has genuine clinical components, but a growing share of the requirement structure serves to legitimate the gate rather than serve clinical necessity (e.g., mandatory psychiatric evaluations, arbitrary wait times, surgical requirements not medically indicated). Accessibility collapse (0.68) is substantial: once the medical-gatekeeping frame is accepted, alternatives (self-ID, biology-only) appear as category errors rather than policy choices. Resistance (0.63) is high and rising: trans rights movements, medical professional bodies, and human rights institutions increasingly contest the gatekeeping model.
 *
 * PERSPECTIVAL GAP:
 *   From the cisgender women / legal administrator seat, the constraint appears as a rope: it coordinates single-sex provisions with a clear, verifiable boundary that balances inclusion and protection. From the trans women pre-transition seat, it is a snare: the gate is the barrier, the coordination story is cover for exclusion, and the cost of passage (medical transition) is extracted from those who did not choose the condition. From the post-transition trans woman seat, it is a tangled rope: they paid the extraction price and now benefit from the coordination, but recognize the gate as extractive for those behind them. From the non-binary seat, it is a mountain of a different kind: the binary itself is the unevaluated premise that makes them structurally invisible — neither gate nor key fits.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: cisgender women (protected-space coordination), medical institutions (gatekeeping authority), legal administrators (clear rule). Victims: pre-transition trans women (excluded by gate), medically-denied trans women (structurally barred), non-binary individuals (unrepresentable). Post-transition trans women occupy a dual seat: they are conditional beneficiaries who paid the full extraction price — their d-value is near symmetric (0.5) because they bear the historical cost and collect the current benefit. Medical institutions are agenda_setters with arbitrage exit (they control the gate and can reform it). The directionality derivation from these declarations captures the asymmetry: extraction flows from trans individuals toward the institutional maintenance of the binary category system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was coordinating single-sex provisions in a way that acknowledges trans existence without abandoning biological reference points. That problem remains contested: biology-reading advocates say the compromise concedes too much; identity-reading advocates say it concedes too little. The constraint persists not because the founding problem is solved, but because no coalition has formed to replace it — biology and identity readings are mutually foreclosed in legislative forums, leaving the hybrid as the only enactable position. This is mandatrophy: the arrangement's mandate (a stable compromise) has outlived its function (neither side accepts it), but the constraint persists because the cost of replacing it exceeds what any single coalition can bear. The theater ratio rise tracks this: more enforcement energy goes into defending the compromise against both flanks than into the coordination function itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid reading of sex/gender category membership a genuine coordination compromise or an unstable truce that structurally forecloses both biology and identity readings?',
    'Track legislative and judicial adoption patterns: if hybrid model becomes the settled legal standard across jurisdictions, it functions as coordination; if it is litigated out of existence or replaced by identity model, it was an unstable truce.',
    'If genuine coordination, the constraint may stabilize as rope or tangled_rope with lower extraction over time. If unstable truce, extraction will concentrate on the excluded groups while the model persists only through active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading represents a structural compromise or a transitional arrangement that forecloses coherent alternatives').

omega_variable(
    medical_gatekeeping_necessity,
    'Is medical transition a structurally necessary gate for category membership, or does it function as an arbitrary barrier that extracts compliance from trans individuals?',
    'Compare outcomes in jurisdictions with self-identification models vs. medical gatekeeping models on metrics of fraud, safety, and social integration. If self-ID jurisdictions show no increase in harms the gate purports to prevent, medical gatekeeping is extractive.',
    'If medically unnecessary, the gate is pure extraction (snare component of tangled_rope dominates). If necessary for coordination function, the extraction is the price of the compromise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medical_gatekeeping_necessity, empirical, 'Whether the medical transition requirement serves a genuine coordination function or is an extractive barrier').

omega_variable(
    cisgender_women_benefit_structure,
    'Do cisgender women materially benefit from the hybrid model''s exclusion of non-transitioned trans women, or is the benefit primarily symbolic/identity-protective?',
    'Measure material outcomes (safety incidents, resource access, institutional trust) in single-sex spaces under hybrid model vs. self-ID model. If material outcomes don''t differ, benefit is symbolic.',
    'If benefit is primarily symbolic, the extraction from trans individuals is not offset by material coordination gains for the beneficiary class — shifts classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cisgender_women_benefit_structure, empirical, 'Nature of the beneficiary class''s gain from the constraint''s operation').

omega_variable(
    non_binary_exclusion_necessity,
    'Is the exclusion of non-binary individuals from both male and female categories a structural necessity of binary classification, or an arbitrary imposition that could be resolved by adding categories?',
    'Examine jurisdictions with legal third-gender or non-binary recognition: if administrative systems function without collapse, exclusion is not structurally necessary.',
    'If exclusion is unnecessary, the constraint extracts from non-binary individuals without coordination justification — strengthens snare component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_binary_exclusion_necessity, conceptual, 'Whether binary category structure is a genuine coordination requirement or an extractive imposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t6, sex_gender_category__hybrid_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__hybrid_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(sex__tr_t18, sex_gender_category__hybrid_reading, theater_ratio, 18, 0.37).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__hybrid_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sex__be_t6, sex_gender_category__hybrid_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__hybrid_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(sex__be_t18, sex_gender_category__hybrid_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__hybrid_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sex__su_t6, sex_gender_category__hybrid_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__hybrid_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(sex__su_t18, sex_gender_category__hybrid_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__hybrid_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__hybrid_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, single_sex_space_access_rules).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, anti_discrimination_law_application).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, medical_transition_access_protocols).

% DUAL FORMULATION NOTE:
% Part of the sex_gender_category constraint family. This hybrid reading occupies the legislative middle ground between biology_reading (Mountain-claimed, extractive in practice) and identity_reading (Rope-claimed, coordination function contested). The three readings share the same referent (legal category membership) but differ in ε by ~0.4 across readings. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, powerless, 0.9).
constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
