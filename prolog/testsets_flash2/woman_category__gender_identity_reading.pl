% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Woman Category: Gender Identity Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint represents the reading of the category 'woman' as
 *   determined by internal gender identity, inclusive of transgender women.
 *   It is one reading of a contested kernel, 'woman_category'. This reading
 *   asserts that self-identification is the primary criterion for womanhood,
 *   leading to policies that grant transgender women access to women's spaces
 *   and services. The metrics reflect the ongoing contestation and the
 *   perceived costs borne by those advocating for sex-based rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.65).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Woman Category: Gender Identity Reading").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, 'ef45f5cd-e173-4b97-bd55-103dbb43b954').
narrative_ontology:cs_kernel_codification('ef45f5cd-e173-4b97-bd55-103dbb43b954', formalized).
narrative_ontology:cs_authority_grounding('ef45f5cd-e173-4b97-bd55-103dbb43b954', practice).
narrative_ontology:cs_interpretation_layer_present('ef45f5cd-e173-4b97-bd55-103dbb43b954').
narrative_ontology:cs_reading_relation('ef45f5cd-e173-4b97-bd55-103dbb43b954', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef45f5cd-e173-4b97-bd55-103dbb43b954', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('ef45f5cd-e173-4b97-bd55-103dbb43b954', foundational, gender_identity_is_primary_determinant_of_womanhood).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('ef45f5cd-e173-4b97-bd55-103dbb43b954', gender_identity_is_primary_determinant_of_womanhood, deontological).
narrative_ontology:cs_axiom('ef45f5cd-e173-4b97-bd55-103dbb43b954', foundational, inclusion_of_transgender_women_is_moral_imperative).
narrative_ontology:cs_axiom_status(inclusion_of_transgender_women_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('ef45f5cd-e173-4b97-bd55-103dbb43b954', inclusion_of_transgender_women_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('ef45f5cd-e173-4b97-bd55-103dbb43b954', inclusive_gender_identity_framework).
narrative_ontology:cs_drift_state('ef45f5cd-e173-4b97-bd55-103dbb43b954', contemporary_social_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ef45f5cd-e173-4b97-bd55-103dbb43b954', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_based_rights_advocates).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, cisgender_women_in_sex_segregated_spaces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in the category 'woman' based on their internal gender identity, affirming their self-identification. This provides access to spaces and services designated for women, but also exposes them to public debate and potential backlash.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the understanding of 'woman' as inclusive of transgender women, advocating for policies and legal frameworks that enshrine gender identity as the primary determinant of category membership. They shape public discourse and institutional policy.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, constrained, global).

% Bear the cost of perceived erosion of sex-based protections and spaces. They argue that conflating sex and gender identity undermines the ability to advocate for the specific needs and rights of biological women, particularly in areas like sports, prisons, and shelters. They face significant social and political pressure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_advocates, payer,
    organized, generational, constrained, national).

% Experience the direct impact of this reading in sex-segregated spaces (e.g., changing rooms, shelters, sports). They may feel their privacy, safety, or fair competition is compromised, but often lack institutional power to challenge the policies directly, facing social ostracization if they voice concerns.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women_in_sex_segregated_spaces, payer,
    powerless, immediate, constrained, local).

% Are tasked with translating this reading into law and policy, balancing competing rights claims. They face pressure from both gender identity advocates and sex-based rights advocates, often leading to complex and contested legislative processes.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Are often marginalized in the binary debate between sex and gender identity, as their lived experience challenges both strict biological definitions and purely identity-based ones. Their specific needs for recognition and accommodation are frequently overlooked.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, intersex_individuals, excluded,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal recognition of gender identity as the primary determinant of womanhood, facilitating inclusion and affirming the self-identification of transgender women within the category 'woman'.
% TRANSFER_FUNCTION: Transfers social and legal recognition of 'woman' from a sex-based definition to a gender-identity-based definition, granting access and affirmation to transgender women, while potentially transferring perceived risks or loss of sex-specific protections from cisgender women.
% ABSENT_VOICES: Intersex individuals, whose experiences challenge binary understandings of sex and gender, are often excluded from the core debate, as are many cisgender women who feel unable to voice concerns about sex-segregated spaces without being labeled transphobic.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal and social landscape regarding gender recognition would immediately revert to sex-based definitions, impacting identity documents, access to spaces, and anti-discrimination laws. Transgender women would lose legal recognition of their womanhood, and the political debate would shift dramatically.
% FOUNDING_PROBLEM: The historical exclusion and discrimination faced by transgender women, who identify as women but were assigned male at birth, from social and legal recognition within the category 'woman'.
% FOUNDING_PROBLEM_CORROBORATION: Transgender women and gender identity advocates attest that the problem of exclusion and discrimination is still live. Sex-based rights advocates contest the solution, arguing it creates new problems for cisgender women, but generally acknowledge the historical discrimination faced by transgender individuals.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-to-high, reflecting the perceived costs to cisgender women in terms of privacy, safety, and fair competition in sex-segregated spaces, as well as the social pressure on those who dissent. Suppression (0.70) is high due to active enforcement of policies based on this reading and the social penalties for challenging it. Theater ratio (0.20) is low, as the policies are genuinely intended to achieve inclusion, though their justification is contested. Resistance (0.75) is high, indicating significant ongoing opposition from sex-based rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender women and gender identity advocates, this constraint is a necessary coordination mechanism for inclusion and affirmation. From the perspective of sex-based rights advocates and many cisgender women, it is an extractive mechanism that erodes sex-based protections. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and gender identity advocates are beneficiaries, gaining recognition and policy alignment. Sex-based rights advocates and cisgender women in sex-segregated spaces are payers, experiencing perceived losses of rights or safety. Policy makers act as agenda-setters, mediating and implementing the policies. Intersex individuals are excluded, as their specific concerns are often not central to this binary-focused debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impact_on_sex_based_protections,
    'Does the gender identity reading of ''woman'' genuinely erode sex-based protections for cisgender women, or are these concerns unfounded/exaggerated?',
    'Empirical studies on the incidence of harm in mixed-sex spaces, analysis of legal outcomes in jurisdictions with gender identity-based laws, and longitudinal studies on women''s participation in sports.',
    'If erosion is demonstrated, the extractiveness and suppression metrics for cisgender women would be re-evaluated upwards, potentially shifting the constraint towards a Snare for that seat. If concerns are unfounded, the current metrics might be overestimates of actual extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_sex_based_protections, empirical, 'Uncertainty regarding the actual impact of gender identity-based definitions on the rights and safety of cisgender women.').

omega_variable(
    identity_vs_material_reality,
    'Is gender identity a sufficient basis for category membership when material sex-based differences (e.g., in sports, physical vulnerability) are relevant?',
    'Conceptual analysis of the philosophical underpinnings of identity vs. material reality, and policy outcomes in areas where these conflict. This is a deeply contested conceptual question.',
    'If material reality is deemed paramount in specific contexts, the constraint''s application in those areas would be reclassified as highly extractive or a Snare for cisgender women. If identity is consistently prioritized, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_vs_material_reality, conceptual, 'The conceptual tension between self-identified gender and biological sex in defining social categories and rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/policy barriers) or internalized (social pressure, fear of ostracization) for those who dissent from this reading?',
    'Post-exit suppression trajectory: if dissent persists after formal barriers are removed, reclassify as partially internalized. Qualitative sociological studies on self-censorship and social consequences for dissenters.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests for dissenters — they carry the suppression with them after formal barriers are removed. This would amplify the effective extraction for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissent against gender identity-based definitions of womanhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t5, woman_category__gender_identity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(woma_tr_t10, woman_category__gender_identity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(woma_tr_t15, woman_category__gender_identity_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(woma_be_t5, woman_category__gender_identity_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(woma_be_t10, woman_category__gender_identity_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(woma_be_t15, woman_category__gender_identity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(woma_su_t5, woman_category__gender_identity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(woma_su_t10, woman_category__gender_identity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(woma_su_t15, woman_category__gender_identity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'gender_identity_reading' of the 'woman_category' kernel. It is structurally distinct from the 'sex_biology_reading' and 'intersex_accommodation_reading' of the same kernel, which define womanhood differently and would yield different extractiveness values and stakeholder sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
