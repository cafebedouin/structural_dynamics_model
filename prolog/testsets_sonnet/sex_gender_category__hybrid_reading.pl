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
 *   human_readable: Medical Gatekeeping Model of Sex/Gender Category Membership
 *   domain: social_ontology/legal_classification/medical_institutions
 *
 * SUMMARY:
 *   This story instantiates the HYBRID READING of the sex/gender category
 *   kernel: category membership is granted conditionally, contingent on
 *   completing a medically supervised transition pathway administered by
 *   clinical gatekeeping institutions. This is not a
 *   compromise-in-the-abstract between the biology reading and the identity
 *   reading — it is a distinct constraint with its own beneficiary and victim
 *   sets, its own authority structure (medical institutions rather than birth
 *   registries or self-report), and its own gatekeeping costs. The hybrid
 *   model conditionally includes transitioned trans women (a group the
 *   biology reading excludes entirely) while excluding non-transitioning
 *   trans individuals and nonbinary people (groups the identity reading would
 *   include). Its distinctive extractive mechanism is the gatekeeping cost
 *   itself: time, money, medical risk, and documentation burden imposed as
 *   the price of admission, borne disproportionately by those with fewer
 *   resources or less access to medical infrastructure.
 *
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
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Medical Gatekeeping Model of Sex/Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social_ontology/legal_classification/medical_institutions").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb').
narrative_ontology:cs_kernel_codification('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', distributed).
narrative_ontology:cs_authority_grounding('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', expertise).
narrative_ontology:cs_interpretation_layer_present('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb').
narrative_ontology:cs_reading_relation('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', sex_gender_category__biology_reading, influences).
narrative_ontology:cs_reading_relation('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', sex_gender_category__identity_reading, influences).
narrative_ontology:cs_axiom('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', foundational, category_membership_transformable_through_verified_medical_process).
narrative_ontology:cs_axiom_status(category_membership_transformable_through_verified_medical_process, holdable).
narrative_ontology:cs_axiom_grounding('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', category_membership_transformable_through_verified_medical_process, conventional).
narrative_ontology:cs_axiom('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', secondary, medical_completion_is_legitimate_gate_for_legal_recognition).
narrative_ontology:cs_axiom_status(medical_completion_is_legitimate_gate_for_legal_recognition, holdable).
narrative_ontology:cs_axiom_grounding('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', medical_completion_is_legitimate_gate_for_legal_recognition, instrumental).
narrative_ontology:cs_reference_frame('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', clinical_gatekeeping_consensus_era).
narrative_ontology:cs_drift_state('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', post_informed_consent_model_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea2e84e2-c8e2-4609-83a6-d8f8b9f5cadb', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, transitioned_trans_women_seeking_legal_recognition).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, sex_segregated_institutions_seeking_bright_line_rule).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, low_income_trans_applicants_unable_to_afford_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, nonbinary_and_genderqueer_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_individuals_in_medical_treatment_deserts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transitioned_trans_women_seeking_legal_recognition).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_authority_over_category_boundaries_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Endocrinologists, surgeons, and licensed gender clinics administer the diagnostic and procedural criteria (hormone duration, surgical requirements, letters of readiness) that determine when a person crosses from one legal/social category into another. They control the pace, cost, and availability of the pathway, and their sign-off is what courts, sports bodies, and record-keepers treat as the operative threshold. They are also positioned as the entities who could loosen or tighten the criteria but bear none of the downstream cost of gatekeeping delay.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Have completed or substantially completed a medically supervised transition and are conditionally admitted to female-designated categories (documents, facilities, some sports tiers) once they clear the gatekeeping threshold. They benefit relative to the biology-only reading, but they paid heavily in time, money, and invasive documentation to reach admission, and remain subject to re-scrutiny if their paperwork or history is challenged.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, transitioned_trans_women_seeking_legal_recognition, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, transitioned_trans_women_seeking_legal_recognition, payer).

% Prisons, shelters, sports federations, and licensing bureaus want an administrable line they can point to and defend in litigation. The medical-completion threshold gives them a defensible, externally-validated criterion instead of having to adjudicate identity or biology claims themselves, which is why they favor deferring to medical sign-off even when it produces harsh individual outcomes.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, sex_segregated_institutions_seeking_bright_line_rule, beneficiary,
    institutional, generational, constrained, national).

% Are trans by identity and often by social presentation but have not undergone (or cannot undergo, or choose not to undergo) the specific medical steps the gatekeeping model requires. They are categorically excluded from the recognition the hybrid model offers, placed in an ambiguous or hostile position relative to both the original-sex category and the destination category, and have no route to inclusion short of medical procedures they may not want, need, or be able to obtain.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Want and would pursue the medical pathway but cannot afford surgery, hormone therapy, or the associated clinical documentation and travel. The gatekeeping model formally offers them a route to recognition that is materially unreachable, converting what looks like an open criterion into a wealth-gated one.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, low_income_trans_applicants_unable_to_afford_transition, payer,
    powerless, biographical, trapped, national).

% Do not seek a destination category of male or female at all, and so have no possible route through a model built entirely around binary endpoint admission. Their situation is structurally invisible to the hybrid framework, which was not built to represent them and offers them no completion criterion of any kind.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, nonbinary_and_genderqueer_individuals, excluded,
    powerless, biographical, trapped, national).

% Live in regions with no accessible gender clinics, few specialist providers, or hostile local medical systems. They face effective exclusion from the gatekeeping pathway not because of choice or cost alone but because the infrastructure the model depends on does not reach them, regardless of desire or ability to pay.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_individuals_in_medical_treatment_deserts, payer,
    powerless, biographical, trapped, regional).

% Reject the hybrid model's premise that social/medical transition can ever change category membership at all, arguing categories should track birth biology exclusively. They are not gatekept by this constraint but object to its existence in principle; they are excluded from this story's stakeholder set only in the sense that they occupy the sibling biology_reading constraint, not this one, though their advocacy exerts pressure on how strictly medical institutions apply the hybrid criteria.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, biology_only_advocates, excluded,
    organized, generational, mobile, national).

% Adjudicate disputes when the medical-completion threshold is challenged, litigated, or legislated against, and can shift the constraint toward the biology reading or the identity reading depending on jurisdiction and era. They take testimony from clinicians, plaintiffs, and defendant institutions and can rewrite the threshold's stringency.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides sex-segregated and sex-classified institutions (prisons, sports, records bureaus, shelters) with a single administrable, externally-validated threshold for category membership, avoiding the need for each institution to independently adjudicate contested identity or biology claims on a case-by-case basis.
% TRANSFER_FUNCTION: Moves the cost of categorical legibility from institutions onto individual applicants: institutions get a bright-line rule they can defend in litigation and staff without specialized training, while applicants absorb the time, money, medical risk, and documentation burden of proving completion of a medically defined transition pathway.
% ABSENT_VOICES: Non-transitioning trans people, financially excluded applicants, people in medical deserts, and nonbinary/genderqueer people would object that the threshold is either unreachable or irrelevant to their actual situation, but none of them participate in setting the clinical criteria, which are negotiated primarily between medical associations, litigators, and sports/legal bodies.
% DISAPPEARANCE_RATIONALE: If the medical-completion threshold vanished overnight, institutions would have to adopt either a pure biology criterion or a pure self-identification criterion (the sibling readings), each with a different victim set; medical gatekeeping infrastructure built around transition certification would lose its regulatory function, and the current population of conditionally-admitted individuals would have to be reclassified under whichever successor rule replaced it.
% FOUNDING_PROBLEM: Sex-segregated institutions needed an administrable line to resolve category disputes in litigation and daily operation once self-identification claims and biological-essentialist claims both proved contestable and neither commanded uncontested legal consensus on its own.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and defendant sex-segregated institutions attest the threshold remains necessary as a defensible legal standard. Trans advocacy organizations and independent legal scholars outside the medical-institutional beneficiary set attest the founding problem has shifted from 'how do we adjudicate a genuine dispute' to 'how do we ration access to categories using medicalization as a filter,' and that the current model serves institutional liability management more than any coordination need the excluded groups recognize as legitimate.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.61) reflects the gatekeeping toll levied on applicants: clinical fees, travel to specialist providers, years of hormone-duration requirements, and the risk of rejection at any stage. Suppression (0.68) reflects that alternatives to going through the medical pathway are foreclosed by the same institutions that administer sex-segregated facilities and records — there is no route to recognition except through the sanctioned threshold. Theater ratio (0.42, rising over the interval) reflects a growing share of gatekeeping activity that functions to demonstrate institutional diligence and liability management (extensive letters, waiting periods, redundant evaluations) rather than to serve a clinical necessity, particularly as informed-consent models have demonstrated that much of the traditional gatekeeping apparatus is not medically required. Accessibility collapse (0.5) is moderate rather than high: unlike a pure natural-law mountain, alternative paths (self-identification, informed consent models) are visibly available and actively argued for by advocates and some clinicians, so alternatives have not fully collapsed, but the model as currently practiced in most jurisdictions treats the medical threshold as the only legitimate access point. Resistance (0.72) is high: this reading is the single most actively contested of the three, challenged in litigation from multiple directions and rejected outright by advocates of both sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical gatekeeping institutions and the sex-segregated institutions that defer to them sit near the beneficiary end: they receive either direct fees (clinics) or a defensible administrable rule (institutions) without bearing the downstream cost of exclusion. Transitioned trans women sit in an intermediate position — genuine beneficiaries of inclusion relative to the biology reading, but they paid a steep entry cost, so their directionality is not purely favorable; the story treats them as beneficiary-with-payer secondary role rather than pure beneficiary. Non-transitioning trans individuals, financially excluded applicants, and those in medical deserts sit at the target end: the same threshold that admits the first group categorically excludes them, and their exit options are trapped because the categories they need access to (facilities, legal sex markers, sports tiers) are gated by the same authority with no alternative route. Nonbinary and genderqueer individuals are structurally outside the model's endpoint logic entirely, which is a distinct exclusion mechanism from active gatekeeping — they are not failing a threshold, they are outside its domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — institutions needing a legally defensible administrable line — was real when self-identification and biology-only claims were both legally untested. Whether that problem remains live is contested: proponents (medical institutions, defendant institutions) maintain the threshold is still functionally necessary for litigation risk management; critics argue the actual clinical justification for many gatekeeping requirements (real-life test periods, mandatory surgery for legal recognition, lengthy waiting periods) has been substantially undermined by informed-consent-model clinical practice, meaning the apparatus increasingly serves institutional risk-management theater rather than the coordination function it was built for. This divergence between claimed function (careful clinical judgment protecting patients and institutions) and computed structure (tangled_rope: real coordination benefit for institutions bundled with concentrated extraction from those who cannot clear or do not want the threshold) is exactly the kind of claim/metric gap the framework is built to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_institutional_liability_shield,
    'Are the specific gatekeeping requirements (waiting periods, mandatory procedures, documentation burdens) clinically necessary safeguards, or primarily a liability-management apparatus for institutions deferring to medical sign-off?',
    'Comparative outcome studies between jurisdictions using informed-consent models versus traditional gatekeeping models; if outcomes are equivalent or better under informed consent, the additional gatekeeping burden in the traditional model is evidentially unjustified by clinical necessity.',
    'If gatekeeping is substantially non-necessary, the extractiveness attributed to ''medical caution'' should be recharacterized as institutional rent/liability-shielding, strengthening the tangled_rope classification over any softer coordination-only reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_institutional_liability_shield, empirical, 'Whether gatekeeping stringency tracks clinical necessity or institutional risk management.').

omega_variable(
    hybrid_reading_stability_under_litigation,
    'Is the hybrid reading a stable equilibrium, or a transitional compromise that will collapse toward either the biology reading or the identity reading as litigation and legislation continue?',
    'Track jurisdictional drift over a multi-decade window: if more jurisdictions converge toward informed-consent/self-attestation models, the hybrid reading is transitional toward identity_reading; if more jurisdictions add stricter biological criteria on top of medical requirements, it is transitional toward biology_reading.',
    'If the hybrid reading is inherently transitional, it may be better modeled with a sunset-clause structure (scaffold) at a future point rather than as a persistent tangled_rope; this omega marks that reclassification as an open empirical question rather than settling it now.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_stability_under_litigation, conceptual, 'Whether the hybrid reading is a stable kernel reading or a way-station between the sibling readings.').

omega_variable(
    coalition_power_of_excluded_groups,
    'Do non-transitioning trans individuals, financially excluded applicants, and nonbinary individuals have any realistic coalition path to jointly contest the medical threshold, given their otherwise low individual power?',
    'Observe litigation and advocacy coalition patterns: do these groups organize jointly with transitioned beneficiaries (who have institutional standing) to challenge the threshold''s stringency, or does the conditional inclusion of transitioned individuals split the coalition by giving one subgroup a stake in defending the current threshold?',
    'If the threshold''s conditional inclusion structurally divides potential coalition partners (transitioned beneficiaries have incentive to defend the threshold that admitted them), the powerless victim groups have less coalition leverage than a naive powerless-victims analysis would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_of_excluded_groups, empirical, 'Whether conditional inclusion fractures potential resistance coalitions among excluded and marginally-included groups.').


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
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__hybrid_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(sex__tr_t18, sex_gender_category__hybrid_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(sex__tr_t24, sex_gender_category__hybrid_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex__be_t6, sex_gender_category__hybrid_reading, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__hybrid_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(sex__be_t18, sex_gender_category__hybrid_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(sex__be_t24, sex_gender_category__hybrid_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sex__su_t6, sex_gender_category__hybrid_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__hybrid_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(sex__su_t18, sex_gender_category__hybrid_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(sex__su_t24, sex_gender_category__hybrid_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__hybrid_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sex_gender_category kernel. biology_reading, hybrid_reading (this story), and identity_reading each instantiate a structurally distinct constraint with a different authority structure, different beneficiary/victim sets, and different ε — per the ε-invariance principle these are NOT the same constraint measured three ways, but three constraints that share a contested kernel (what determines category membership) and partially overlapping victim populations. The hybrid reading's victim set partially overlaps both siblings: it shares the exclusion of non-transitioning trans people with biology_reading's more totalizing exclusion, and shares the inclusion of transitioned individuals with identity_reading's inclusion logic, but reaches each via a different mechanism (medical completion vs. birth fact vs. self-attestation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
