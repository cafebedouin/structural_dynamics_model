% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Identity as Basis for Woman/Female Category Membership
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines 'woman' or 'female' category membership based on
 *   internal self-identification with a gender category, independent of
 *   biological sex. It is a specific reading of the broader
 *   'woman_female_category' kernel. This reading prioritizes gender identity
 *   for social and legal recognition, leading to the inclusion of trans women
 *   in female-designated spaces and categories. The constraint is actively
 *   enforced through legal frameworks and social pressure, with identifiable
 *   beneficiaries (transgender individuals, gender identity advocates) and
 *   victims (cisgender women, gender-critical feminists).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.65).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity as Basis for Woman/Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '001a60b9-f43f-4246-840a-ae45545b9555').
narrative_ontology:cs_kernel_codification('001a60b9-f43f-4246-840a-ae45545b9555', formalized).
narrative_ontology:cs_authority_grounding('001a60b9-f43f-4246-840a-ae45545b9555', lineage).
narrative_ontology:cs_interpretation_layer_present('001a60b9-f43f-4246-840a-ae45545b9555').
narrative_ontology:cs_reading_relation('001a60b9-f43f-4246-840a-ae45545b9555', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('001a60b9-f43f-4246-840a-ae45545b9555', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('001a60b9-f43f-4246-840a-ae45545b9555', foundational, gender_identity_is_primary_determinant_of_gender).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_gender, holdable).
narrative_ontology:cs_axiom_grounding('001a60b9-f43f-4246-840a-ae45545b9555', gender_identity_is_primary_determinant_of_gender, deontological).
narrative_ontology:cs_axiom('001a60b9-f43f-4246-840a-ae45545b9555', foundational, self_identification_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('001a60b9-f43f-4246-840a-ae45545b9555', self_identification_is_sufficient_for_category_membership, conventional).
narrative_ontology:cs_reference_frame('001a60b9-f43f-4246-840a-ae45545b9555', identity_based_gender_recognition).
narrative_ontology:cs_drift_state('001a60b9-f43f-4246-840a-ae45545b9555', contemporary_contestation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('001a60b9-f43f-4246-840a-ae45545b9555', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women_in_single_sex_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, medical_professionals).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, medical_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from legal and social recognition of their self-identified gender, allowing access to spaces and categories aligning with their identity. Exit is not an option as it involves denying their core identity.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promotes and legislates for gender identity as the primary determinant of gender category, shaping public discourse and institutional policy. They benefit from the expansion of identity-based rights.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Experience a redefinition of 'woman' or 'female' that includes individuals with male biology, leading to perceived loss of privacy, safety, or dignity in spaces previously designated for biological females. Their concerns are often dismissed or reframed as transphobic.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women_in_single_sex_spaces, payer,
    powerless, immediate, constrained, local).

% Advocates for sex-based rights and the retention of 'woman' as a category defined by biological sex. They bear the cost of social ostracization, professional repercussions, and legal challenges for expressing their views. Their exit options are limited by the social and professional costs of dissent.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_feminists, payer,
    organized, generational, constrained, national).

% Implement and enforce laws and policies that codify gender identity as the basis for legal sex/gender. They navigate conflicting rights claims and public pressure, often prioritizing identity-based recognition.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Are increasingly required to align medical language and practice with gender identity, which can create tension with biological realities in certain contexts (e.g., reproductive health). They benefit from clear legal frameworks but may face ethical dilemmas.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, medical_professionals, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, medical_professionals, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal recognition of gender identity, providing a consistent framework for individuals to live in accordance with their self-identified gender.
% TRANSFER_FUNCTION: Transfers social and legal recognition, dignity, and access to gendered spaces from a biological sex-based definition of 'woman' to an identity-based definition, benefiting transgender individuals and their advocates.
% ABSENT_VOICES: Many cisgender women, particularly those concerned about single-sex spaces, feel their voices are absent or actively suppressed in policy discussions, often being labeled as bigoted or hateful. Their concerns are often not formally acknowledged in policy-making bodies.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, legal and social frameworks would revert to or emphasize biological sex as the primary determinant of 'woman' or 'female' categories. This would significantly alter the rights and recognition of transgender individuals, leading to widespread social and legal reorganization.
% FOUNDING_PROBLEM: The historical and ongoing marginalization and discrimination faced by transgender individuals, particularly trans women, who were denied recognition of their gender identity in social and legal contexts.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy groups and human rights organizations consistently attest to the ongoing problem of discrimination and the need for identity-based recognition. While some gender-critical groups contest the scope or nature of the problem, the existence of historical and current marginalization is widely corroborated by independent human rights reports and sociological studies.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).

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
 *   The extractiveness (0.65) reflects the perceived loss of sex-based protections and dignity for cisgender women, and the social/professional costs borne by those who dissent from the identity-based definition. Suppression (0.70) is high due to legal mandates and social pressure that actively discourage or penalize challenges to the identity-based definition. The theater ratio (0.20) is relatively low, as the constraint's primary function (identity recognition) is genuinely pursued, though some enforcement may be performative in dismissing dissenting views. The rising extractiveness and suppression over time reflect the increasing institutionalization and enforcement of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and gender identity advocates, this constraint is a necessary coordination mechanism for dignity and recognition, with minimal extraction. From the perspective of cisgender women concerned about single-sex spaces and gender-critical feminists, it is a highly extractive and suppressive mechanism that redefines fundamental categories and erodes sex-based rights. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals and gender identity advocates are clear beneficiaries (d near 0.0) as the constraint directly affirms their identity and expands their rights. Cisgender women in single-sex spaces and gender-critical feminists are targets (d near 1.0) as they bear the costs of category redefinition and suppression of their views. Legal systems and medical professionals act as agenda-setters, enforcing the constraint and navigating its implications.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (addressing transgender marginalization) is still live. However, the specific mechanism (identity as sole determinant) is contested, leading to a 'tangled rope' classification. The analysis prevents mislabeling it as a pure rope by acknowledging the significant extraction and suppression experienced by dissenting parties, while also recognizing its genuine coordination function for beneficiaries. It is not a snare because it genuinely solves a coordination problem for a significant group, but the asymmetric costs make it tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_identity_vs_sex,
    'What are the appropriate contexts where gender identity should take precedence over biological sex, and vice versa?',
    'Empirical studies on the impact of identity-based vs. sex-based categorization in different domains (e.g., sports, healthcare, prisons, shelters) on safety, fairness, and dignity for all parties.',
    'If identity-based categorization is found to cause disproportionate harm in certain contexts, the constraint''s scope would need to be limited, potentially shifting it towards a ''hybrid_contextual_reading'' and reducing its extractiveness on cisgender women.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_identity_vs_sex, empirical, 'Ambiguity regarding the domain-specific applicability of gender identity vs. biological sex.').

omega_variable(
    dignity_harm_measurement,
    'How can the ''dignity harm'' experienced by cisgender women from the redefinition of ''woman'' be objectively measured and weighed against the ''dignity harm'' experienced by transgender individuals from non-recognition?',
    'Development of validated psychosocial metrics for dignity and recognition, coupled with deliberative democratic processes to establish societal weighting of competing harms.',
    'If the dignity harm to cisgender women is found to be substantial and unmitigated, it would increase the measured extractiveness and suppression of this constraint, potentially pushing it closer to a ''snare'' classification from their perspective. If the harms are found to be negligible, it would support the ''rope'' framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_harm_measurement, conceptual, 'Difficulty in objectively measuring and comparing competing dignity harms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (social ostracization, professional repercussions) structural (external barriers) or internalized (cognitive patterns that persist after barrier removal) for gender-critical feminists?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism (e.g., legal penalties, social media bans) is removed, reclassify as partially internalized. Longitudinal studies on the psychological impact of public dissent.',
    'If internalized, the constraint''s effective suppression on gender-critical feminists is higher than the structural measure suggests — they carry the suppression with them after exit, making exit less effective. This would amplify the perceived extractiveness from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting voices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1990, woman_female_category__gender_identity_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(woma_tr_t2000, woman_female_category__gender_identity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(woma_tr_t2010, woman_female_category__gender_identity_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__gender_identity_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(woma_tr_t2020, woman_female_category__gender_identity_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__gender_identity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t1990, woman_female_category__gender_identity_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(woma_be_t2000, woman_female_category__gender_identity_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(woma_be_t2010, woman_female_category__gender_identity_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(woma_be_t2015, woman_female_category__gender_identity_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(woma_be_t2020, woman_female_category__gender_identity_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__gender_identity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1990, woman_female_category__gender_identity_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(woma_su_t2000, woman_female_category__gender_identity_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(woma_su_t2010, woman_female_category__gender_identity_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(woma_su_t2015, woman_female_category__gender_identity_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(woma_su_t2020, woman_female_category__gender_identity_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__gender_identity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, single_sex_spaces_policy).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, gender_affirming_care_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'woman_female_category' kernel. Its structural properties and classification differ significantly from the 'sex_biology_reading' and 'hybrid_contextual_reading' due to differing definitions of category membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
