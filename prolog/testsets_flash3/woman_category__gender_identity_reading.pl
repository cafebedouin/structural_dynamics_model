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
 *   This constraint defines 'woman' based on internal gender identity,
 *   including transgender women. It is a reading of the broader
 *   'woman_category' kernel, which is highly contested. This reading aims to
 *   coordinate social and legal recognition for transgender women but
 *   generates significant friction and perceived extraction for those
 *   advocating for sex-based rights, particularly cisgender women in
 *   sex-segregated spaces. The metrics reflect the ongoing enforcement
 *   required to maintain this definition against resistance.
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
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '03a70173-f0fc-416a-a34b-15913fff9dff').
narrative_ontology:cs_kernel_codification('03a70173-f0fc-416a-a34b-15913fff9dff', formalized).
narrative_ontology:cs_authority_grounding('03a70173-f0fc-416a-a34b-15913fff9dff', practice).
narrative_ontology:cs_interpretation_layer_present('03a70173-f0fc-416a-a34b-15913fff9dff').
narrative_ontology:cs_reading_relation('03a70173-f0fc-416a-a34b-15913fff9dff', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('03a70173-f0fc-416a-a34b-15913fff9dff', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('03a70173-f0fc-416a-a34b-15913fff9dff', foundational, gender_identity_is_primary_determinant_of_womanhood).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('03a70173-f0fc-416a-a34b-15913fff9dff', gender_identity_is_primary_determinant_of_womanhood, deontological).
narrative_ontology:cs_axiom('03a70173-f0fc-416a-a34b-15913fff9dff', foundational, inclusion_of_transgender_women_is_moral_imperative).
narrative_ontology:cs_axiom_status(inclusion_of_transgender_women_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('03a70173-f0fc-416a-a34b-15913fff9dff', inclusion_of_transgender_women_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('03a70173-f0fc-416a-a34b-15913fff9dff', gender_identity_inclusive_framework).
narrative_ontology:cs_drift_state('03a70173-f0fc-416a-a34b-15913fff9dff', contemporary_social_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('03a70173-f0fc-416a-a34b-15913fff9dff', '').
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

% Gain recognition and inclusion in the category 'woman' based on their internal gender identity, affirming their self-perception. This provides access to spaces and services designated for women. Their identity is central to this claim.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the understanding of 'woman' as inclusive of transgender women, advocating for policies and legal frameworks that reflect this definition. They shape public discourse and institutional policy.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Bear the cost of redefined categories, particularly in areas like sports, prisons, and changing rooms, where they argue sex-based protections for cisgender women are eroded. They face legal and social pressure when asserting sex-based definitions.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_advocates, payer,
    organized, generational, constrained, national).

% Experience the direct impact of this definition in spaces historically segregated by sex (e.g., changing rooms, shelters, sports). They may feel their privacy, safety, or fair competition is compromised, with limited recourse to challenge the policy.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women_in_sex_segregated_spaces, payer,
    powerless, immediate, constrained, local).

% Are tasked with implementing legal and policy definitions of 'woman' that align with this reading, often balancing competing rights claims. They face political pressure from both sides of the debate.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Are often overlooked in the binary 'sex vs. gender identity' debate, despite their biological realities challenging simplistic sex definitions. Their specific needs for accommodation are not central to this reading's framework.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, intersex_individuals, excluded,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal recognition for individuals who identify as women, ensuring their inclusion in the category 'woman' and associated rights and spaces, thereby affirming gender identity as a primary determinant of social category.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to sex-segregated spaces, and identity affirmation to transgender women. It transfers costs in terms of perceived erosion of sex-based protections and definitional clarity for cisgender women.
% ABSENT_VOICES: Intersex individuals, whose biological realities complicate both sex- and gender-identity-based definitions, are often marginalized in this debate. They would advocate for a more nuanced understanding of biological variation.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal and social landscape for transgender women would fundamentally shift, potentially revoking access to women's spaces and services, leading to a significant reorganization of identity recognition and rights frameworks. Conversely, sex-based rights frameworks would reassert themselves.
% FOUNDING_PROBLEM: The historical exclusion and discrimination faced by transgender women, who identify as women but were assigned male at birth, from social and legal recognition within the category 'woman'.
% FOUNDING_PROBLEM_CORROBORATION: Transgender women and gender identity advocates attest that the problem of exclusion and misrecognition remains live. Sex-based rights advocates contest the framing of the problem, arguing that the solution creates new problems for cisgender women; however, the historical exclusion of transgender individuals is widely documented by human rights organizations and medical bodies.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) due to the perceived costs borne by sex-based rights advocates and cisgender women, who feel their protections are diminished. Suppression (0.70) is also high, reflecting the active social and legal pressure applied to enforce this definition and marginalize dissenting views. Resistance (0.75) is substantial, indicating an ongoing, active contestation of this definition. Theater ratio (0.20) is low, as the enforcement is genuinely aimed at achieving the stated goal of inclusion, not merely performance.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiaries (transgender women, gender identity advocates) experience this as a necessary coordination mechanism for identity affirmation and inclusion. Payers (sex-based rights advocates, cisgender women) experience it as an extractive imposition that erodes their existing protections. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and gender identity advocates are clear beneficiaries, gaining recognition and shaping policy. Sex-based rights advocates and cisgender women in sex-segregated spaces are the primary payers, experiencing the costs of redefined categories. Policy makers act as agenda-setters, mediating and implementing these definitions. Intersex individuals are largely excluded from the central debate, despite its relevance to their lived experience.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_scope_ambiguity,
    'Is the concept of ''woman'' fundamentally a social/gender identity or a biological/sex category?',
    'Conceptual clarification through philosophical consensus or legal precedent that explicitly prioritizes one framework over the other for all contexts, or clearly delineates context-dependent definitions.',
    'If resolved as primarily biological, this reading''s extractiveness would be re-evaluated as higher, as it would be seen as imposing a non-biological definition. If resolved as primarily social/identity, the resistance would be re-evaluated as less legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Ambiguity over the fundamental basis of the category ''woman''.').

omega_variable(
    impact_on_sex_based_protections,
    'To what extent does the inclusion of transgender women in ''woman'' categories genuinely erode or merely reconfigure sex-based protections for cisgender women?',
    'Empirical studies on safety, privacy, and fairness outcomes in sex-segregated spaces (e.g., prisons, sports, shelters) in jurisdictions that have adopted this reading, compared to those that have not.',
    'If significant erosion is empirically demonstrated, the extractiveness and suppression metrics for sex-based rights advocates would be validated as accurate or even understated. If no significant erosion, the perceived extraction would be re-evaluated as lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_protections, empirical, 'Empirical impact of gender identity definition on sex-based protections.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of sex-based rights advocacy structural (legal/policy barriers) or internalized (social pressure/self-censorship)?',
    'Post-policy-change suppression trajectory: if advocacy for sex-based rights persists and gains traction after legal challenges to gender identity definitions are removed, reclassify as partially internalized. If legal barriers remain the primary obstacle, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — advocates carry the suppression with them. If purely structural, removing legal barriers would significantly reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for sex-based rights advocacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t5, woman_category__gender_identity_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(woma_tr_t10, woman_category__gender_identity_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(woma_tr_t15, woman_category__gender_identity_reading, theater_ratio, 15, 0.18).
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
% This constraint is one reading of the 'woman_category' kernel, focusing on gender identity. It is structurally distinct from the 'sex_biology_reading' and 'intersex_accommodation_reading' due to differing definitions of the category 'woman' and their implications for rights and access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
