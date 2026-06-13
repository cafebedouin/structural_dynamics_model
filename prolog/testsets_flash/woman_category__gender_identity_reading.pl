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
 *   irrespective of assigned sex at birth. It is a reading of the
 *   'woman_category' kernel, which is highly contested. This reading aims to
 *   ensure inclusivity for transgender women but generates significant
 *   friction with those advocating for sex-based rights, particularly in
 *   areas like sports and sex-segregated spaces. The constraint is actively
 *   enforced through legal and social mechanisms, leading to rising
 *   extractiveness and suppression for those who challenge it.
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
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '13ac54de-f666-43c5-a53f-34e9f9eb6f9a').
narrative_ontology:cs_kernel_codification('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', distributed).
narrative_ontology:cs_authority_grounding('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', practice).
narrative_ontology:cs_interpretation_layer_present('13ac54de-f666-43c5-a53f-34e9f9eb6f9a').
narrative_ontology:cs_reading_relation('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', foundational, gender_identity_is_primary_determinant_of_womanhood).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', gender_identity_is_primary_determinant_of_womanhood, deontological).
narrative_ontology:cs_axiom('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', secondary, sex_assigned_at_birth_is_irrelevant_to_womanhood).
narrative_ontology:cs_axiom_status(sex_assigned_at_birth_is_irrelevant_to_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', sex_assigned_at_birth_is_irrelevant_to_womanhood, conventional).
narrative_ontology:cs_reference_frame('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', gender_identity_as_self_evident_truth).
narrative_ontology:cs_drift_state('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('13ac54de-f666-43c5-a53f-34e9f9eb6f9a', '').
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

% Gain recognition and inclusion in the category 'woman', aligning legal and social identity with their internal gender identity. This provides access to spaces and services designated for women. Exit is not an option as it would mean denying their identity.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the definition of 'woman' based on gender identity, influencing policy, law, and social norms. They benefit from the expansion of gender identity as the primary determinant of social categories.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, mobile, global).

% Bear the cost of losing sex-based protections and categories, particularly in areas like sports, prisons, and domestic violence shelters, where they argue biological sex is relevant. They face social and legal pressure for 'exclusionary' views.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_advocates, payer,
    organized, generational, constrained, national).

% Experience a redefinition of 'woman' that includes individuals with male biology, leading to concerns about privacy, safety, and fairness in sex-segregated spaces and competitive sports. Their concerns are often dismissed or framed as discriminatory.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, cisgender_women_in_sex_segregated_spaces, payer,
    powerless, biographical, constrained, local).

% Are tasked with drafting and implementing laws and policies that reflect this definition, often navigating intense public debate and legal challenges. They face pressure from both gender identity advocates and sex-based rights advocates.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, policy_makers, agenda_setter,
    institutional, immediate, constrained, national).

% Analyze the legal implications and philosophical underpinnings of defining 'woman' by gender identity, contributing to jurisprudence and public discourse without directly enforcing the constraint.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal recognition of gender identity, ensuring that individuals who identify as women are consistently recognized as such across various social and institutional contexts, thereby reducing discrimination and promoting inclusivity.
% TRANSFER_FUNCTION: Transfers social and legal recognition from a sex-based definition of 'woman' to a gender-identity-based definition. This transfers access rights to transgender women and transfers the burden of adapting to new definitions onto cisgender women and institutions previously organized by sex.
% ABSENT_VOICES: Children and adolescents, particularly girls, whose developing understanding of sex and gender is shaped by these definitions, and who may be directly impacted in areas like sports and single-sex spaces, are largely absent from the policy-making discourse.
% DISAPPEARANCE_RATIONALE: If the gender identity reading of 'woman' vanished overnight, legal frameworks, social norms, and institutional practices would revert to sex-based definitions. This would significantly alter the rights and recognition of transgender women, and reshape debates around sex-segregated spaces and sports eligibility.
% FOUNDING_PROBLEM: The historical and ongoing exclusion and discrimination faced by transgender individuals, particularly transgender women, who were not recognized in their affirmed gender, leading to social marginalization and legal vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations and human rights bodies attest that discrimination against transgender individuals remains a live problem. Sex-based rights advocates contest that the proposed solution creates new problems for cisgender women, but do not deny the historical problem of transgender discrimination itself.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).

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
 *   The extractiveness (0.65) is moderate-to-high because while it provides benefits to transgender women, it imposes costs on cisgender women by redefining categories that were historically based on biological sex, leading to perceived loss of protections and spaces. Suppression (0.7) is high due to social and legal pressure against 'gender-critical' views, often framed as transphobic. The theater ratio (0.2) is low, as the enforcement is genuinely aimed at achieving the stated goal of gender identity recognition, though the underlying coordination function is increasingly strained by the costs imposed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender women and gender identity advocates, this constraint is a necessary rope for inclusion and equality. From the perspective of sex-based rights advocates and many cisgender women, it operates as a snare, eroding sex-based protections under the guise of inclusion. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and gender identity advocates are clear beneficiaries, gaining recognition and advancing their policy goals (low d). Sex-based rights advocates and cisgender women in sex-segregated spaces are targets, bearing the costs of redefined categories and perceived loss of protections (high d). Policy makers are agenda-setters, navigating the implementation and enforcement of this definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_relevance,
    'Is biological sex a relevant category for certain rights, protections, or spaces, even when gender identity is the primary legal/social determinant of ''woman''?',
    'Empirical studies on fairness in sports, safety in single-sex spaces, and the efficacy of sex-segregated services (e.g., domestic violence shelters) when gender identity is the sole criterion.',
    'If biological sex is found to be relevant in specific contexts, the constraint''s extractiveness on cisgender women would be re-evaluated as higher, potentially leading to a reclassification towards Snare in those specific contexts. If not, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_relevance, empirical, 'The relevance of biological sex in specific contexts where gender identity is the primary determinant of ''woman''.').

omega_variable(
    identity_vs_material_reality,
    'To what extent can legal and social categories based on self-declared identity override material biological realities without creating new forms of harm or injustice?',
    'Philosophical and legal analysis of the limits of self-identification in public policy, combined with long-term social impact assessments in jurisdictions that have fully adopted gender identity as the sole determinant.',
    'If a significant disjunction is found to cause systemic harm, the ''gender_identity_reading'' might be seen as structurally unstable or requiring substantial compensatory mechanisms, potentially increasing its computed extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_vs_material_reality, conceptual, 'The conceptual tension between identity-based categories and material biological realities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of sex-based rights advocates structural (legal/institutional barriers) or internalized (social pressure/self-censorship)?',
    'Analysis of legal outcomes in free speech cases related to gender identity, and sociological studies on self-censorship among academics and professionals expressing gender-critical views.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the structural measure suggests, as dissent is stifled even without explicit legal prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views on gender identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_category__gender_identity_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(woma_tr_t2008, woman_category__gender_identity_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(woma_tr_t2016, woman_category__gender_identity_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(woma_tr_t2024, woman_category__gender_identity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_category__gender_identity_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(woma_be_t2008, woman_category__gender_identity_reading, base_extractiveness, 2008, 0.45).
narrative_ontology:measurement(woma_be_t2016, woman_category__gender_identity_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(woma_be_t2024, woman_category__gender_identity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_category__gender_identity_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(woma_su_t2008, woman_category__gender_identity_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(woma_su_t2016, woman_category__gender_identity_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(woma_su_t2024, woman_category__gender_identity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'woman_category' kernel. Its structural properties differ significantly from the 'sex_biology_reading' and 'intersex_accommodation_reading', particularly in its beneficiary/victim sets and the nature of its enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
