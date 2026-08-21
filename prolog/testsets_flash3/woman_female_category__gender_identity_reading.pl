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
    narrative_ontology:epsilon_provenance/5,
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
 *   an individual's internal self-identification with that gender,
 *   independent of their biological sex. It is a specific reading of the
 *   broader 'woman_female_category' kernel, which is highly contested. This
 *   reading aims to provide legal and social recognition for transgender
 *   individuals, particularly trans women, within female-designated spaces
 *   and categories. The constraint is actively enforced through policy and
 *   social pressure, leading to benefits for those whose identities are
 *   affirmed and costs for those who believe it erodes sex-based protections.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.65).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity as Basis for Woman/Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '26574fe4-03a6-42c1-aa83-7983f43459f1').
narrative_ontology:cs_kernel_codification('26574fe4-03a6-42c1-aa83-7983f43459f1', formalized).
narrative_ontology:cs_authority_grounding('26574fe4-03a6-42c1-aa83-7983f43459f1', lineage).
narrative_ontology:cs_interpretation_layer_present('26574fe4-03a6-42c1-aa83-7983f43459f1').
narrative_ontology:cs_reading_relation('26574fe4-03a6-42c1-aa83-7983f43459f1', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('26574fe4-03a6-42c1-aa83-7983f43459f1', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('26574fe4-03a6-42c1-aa83-7983f43459f1', foundational, gender_identity_is_primary_determinant_of_gender).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_gender, holdable).
narrative_ontology:cs_axiom_grounding('26574fe4-03a6-42c1-aa83-7983f43459f1', gender_identity_is_primary_determinant_of_gender, deontological).
narrative_ontology:cs_axiom('26574fe4-03a6-42c1-aa83-7983f43459f1', foundational, self_identification_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('26574fe4-03a6-42c1-aa83-7983f43459f1', self_identification_is_sufficient_for_category_membership, conventional).
narrative_ontology:cs_reference_frame('26574fe4-03a6-42c1-aa83-7983f43459f1', gender_identity_affirmation_framework).
narrative_ontology:cs_drift_state('26574fe4-03a6-42c1-aa83-7983f43459f1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('26574fe4-03a6-42c1-aa83-7983f43459f1', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, gender_critical_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, biological_sex_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, public_institutions).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, public_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal and social recognition of their self-identified gender, allowing access to spaces and categories aligning with their identity. Exit from this framework would mean denying their identity, which is not a viable option.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce policies that define 'woman' or 'female' based on gender identity. They see this as essential for human rights and dignity. Their efforts shape legal and institutional frameworks.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, constrained, national).

% Experience a perceived loss of sex-based rights, spaces, and language when gender identity is prioritized over biological sex. They bear the cost of having their sex-based category redefined and often face social and professional repercussions for expressing their views. Exit options are limited to forming alternative communities or legal challenges.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_women, payer,
    organized, biographical, constrained, national).

% Advocate for the recognition of biological sex as the primary determinant of 'woman' or 'female' categories, particularly in contexts like sports, prisons, and single-sex spaces. They bear the cost of being marginalized in public discourse and policy-making.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, biological_sex_advocates, payer,
    moderate, generational, constrained, national).

% Are tasked with interpreting and implementing laws that define gender and sex. They are under pressure from both sides of the debate and their decisions actively enforce one reading over another, shaping societal norms.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% Implement policies based on gender identity, often facing logistical challenges, public backlash, and internal dissent. They benefit from appearing progressive and inclusive, but bear the costs of navigating complex and contested definitions.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, public_institutions, payer,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, public_institutions, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal recognition for transgender individuals, ensuring their self-identified gender is affirmed across various contexts, thereby reducing discrimination and promoting inclusion.
% TRANSFER_FUNCTION: Transfers social and legal recognition, dignity, and access to gender-affirming spaces from a biological-sex-based definition to a gender-identity-based definition. This redefines who belongs to the category 'woman' or 'female'.
% ABSENT_VOICES: Children and adolescents who are navigating gender identity questions, and who may be subject to irreversible medical interventions based on this framework, are largely absent from the policy-making discourse. Their long-term interests and potential for desistance are often not fully represented.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and social landscape around gender recognition would immediately revert to a sex-based or more ambiguous system. Transgender individuals would lose current protections and recognition, while gender-critical voices would regain ground. Public institutions would face immediate pressure to redefine policies.
% FOUNDING_PROBLEM: The historical and ongoing discrimination, marginalization, and lack of recognition faced by transgender individuals, leading to significant psychological distress and social exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and their advocates attest that the problem of discrimination is still live. While gender-critical groups acknowledge historical discrimination, they contest whether this specific solution (gender identity over biological sex) is the appropriate or only means to address it, citing harms to women's sex-based rights.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the perceived cost to gender-critical women and biological sex advocates, who experience a redefinition of their category and spaces. Suppression (0.70) is high due to social and institutional pressure to conform to this definition, often leading to professional and social repercussions for dissenters. Theater ratio is low (0.10) as the constraint is actively and genuinely enforced, not merely performative. Accessibility collapse is moderate (0.40) as alternative frameworks (like sex-based definitions) are still debated and pursued, but often at significant personal cost.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals, this constraint is a necessary rope, coordinating recognition and dignity. From the perspective of gender-critical women, it is a snare, extracting sex-based rights and protections. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals and gender identity advocates are clear beneficiaries, gaining recognition and protection (low d). Gender-critical women and biological sex advocates are targets, bearing the costs of category redefinition and social pressure (high d). Legal systems and public institutions act as agenda-setters, enforcing the constraint, but also bear costs in terms of public contention and implementation challenges.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_vs_safety_tradeoff,
    'Is the dignity and recognition afforded by gender identity-based category membership necessarily in conflict with the safety and privacy concerns of biological women in single-sex spaces?',
    'Empirical studies on the impact of gender identity policies on single-sex spaces, combined with legal analysis of competing rights frameworks. Resolution would require a framework that can reconcile or prioritize these values.',
    'If a conflict is inherent, the constraint''s extractiveness on gender-critical women is irreducible. If not, alternative coordination mechanisms could reduce extraction while maintaining dignity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_vs_safety_tradeoff, conceptual, 'Whether the core values promoted by this reading inherently conflict with those of sibling readings.').

omega_variable(
    social_vs_legal_enforcement_balance,
    'What is the relative proportion of social pressure (e.g., ''cancel culture'') versus formal legal enforcement in maintaining this constraint?',
    'Sociological studies tracking instances of social sanction versus legal action related to gender identity definitions. Analysis of public discourse and institutional policy implementation.',
    'If social pressure is the dominant enforcement mechanism, the suppression metric might be higher than formal legal structures alone suggest, indicating a more diffuse and harder-to-resist form of coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_vs_legal_enforcement_balance, empirical, 'Balance of social vs. legal enforcement mechanisms.').

omega_variable(
    identity_lock_mechanism,
    'For transgender individuals, is ''identity_locked'' exit primarily due to internal psychological factors (self-concept) or external social/medical pressures (affirmation pathways)?',
    'Longitudinal studies of detransition rates, experiences of individuals who question their gender identity, and the influence of social and medical affirmation models.',
    'If external pressures are dominant, the ''identity_locked'' status is more a product of the constraint''s social enforcement than an intrinsic psychological state, potentially increasing effective extraction from those who might otherwise exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized components of identity-locked exit for transgender individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__gender_identity_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__gender_identity_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(woma_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(woma_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, single_sex_spaces_policy).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, gender_affirming_healthcare_access).

% DUAL FORMULATION NOTE:
% This constraint is the 'gender_identity_reading' of the 'woman_female_category' kernel. Its structural properties and classification differ significantly from the 'sex_biology_reading' and 'hybrid_contextual_reading' of the same kernel, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
