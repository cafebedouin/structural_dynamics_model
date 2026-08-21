% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership (Gender Identity Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint defines membership in gendered categories (e.g., 'woman',
 *   'man') based on an individual's subjective gender identity and
 *   self-declaration. It is a reading of the broader
 *   'gendered_category_membership' kernel. This reading asserts that an
 *   individual's internal sense of gender is the primary determinant of their
 *   category, leading to the inclusion of transgender individuals in
 *   categories aligned with their identity. The constraint is actively
 *   enforced through social pressure, institutional policy, and legal
 *   frameworks, often positioning those who resist this definition as
 *   perpetrators of exclusion. The metrics reflect the costs borne by those
 *   adapting to or resisting this framework, and the active suppression of
 *   alternative definitions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.6).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership (Gender Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, 'fd253c91-fa04-48ba-a624-94514edd4e44').
narrative_ontology:cs_kernel_codification('fd253c91-fa04-48ba-a624-94514edd4e44', formalized).
narrative_ontology:cs_authority_grounding('fd253c91-fa04-48ba-a624-94514edd4e44', practice).
narrative_ontology:cs_interpretation_layer_present('fd253c91-fa04-48ba-a624-94514edd4e44').
narrative_ontology:cs_reading_relation('fd253c91-fa04-48ba-a624-94514edd4e44', gendered_category_membership__biological_sex_reading, influences).
narrative_ontology:cs_reading_relation('fd253c91-fa04-48ba-a624-94514edd4e44', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('fd253c91-fa04-48ba-a624-94514edd4e44', foundational, gender_is_self_declared).
narrative_ontology:cs_axiom_status(gender_is_self_declared, holdable).
narrative_ontology:cs_axiom_grounding('fd253c91-fa04-48ba-a624-94514edd4e44', gender_is_self_declared, deontological).
narrative_ontology:cs_axiom('fd253c91-fa04-48ba-a624-94514edd4e44', foundational, gender_identity_trumps_sex_in_social_categories).
narrative_ontology:cs_axiom_status(gender_identity_trumps_sex_in_social_categories, holdable).
narrative_ontology:cs_axiom_grounding('fd253c91-fa04-48ba-a624-94514edd4e44', gender_identity_trumps_sex_in_social_categories, conventional).
narrative_ontology:cs_reference_frame('fd253c91-fa04-48ba-a624-94514edd4e44', gender_self_identification_as_norm).
narrative_ontology:cs_drift_state('fd253c91-fa04-48ba-a624-94514edd4e44', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fd253c91-fa04-48ba-a624-94514edd4e44', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cisgender_women_resisting_inclusion).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, institutions_adapting_policies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition and inclusion in gendered categories aligned with their self-declared identity, which is crucial for their well-being and social integration. Exit from this framework would mean denying their identity, which is not a viable option.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the understanding of gendered categories based on self-declaration. They benefit from the expansion of this framework and the social and legal changes it brings. Their exit options are constrained by their commitment to the cause.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, constrained, global).

% Experience a redefinition of categories they previously understood as sex-based, leading to concerns about single-sex spaces, data collection, and political representation. They bear the social cost of being labeled as exclusionary if they resist the gender identity framework.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cisgender_women_resisting_inclusion, payer,
    moderate, biographical, constrained, local).

% Are compelled to revise policies, language, and facilities to align with gender identity principles, often facing legal challenges or public pressure if they do not. They bear the administrative and social costs of this adaptation.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, institutions_adapting_policies, payer,
    institutional, immediate, constrained, national).

% Are largely excluded from mainstream discourse and policy-making bodies that adopt the gender identity framework. Their arguments for sex-based categories are often dismissed or actively suppressed, limiting their ability to influence the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, biological_sex_advocates, excluded,
    organized, generational, trapped, global).

% Analyze the implications of both biological sex and gender identity framings, often highlighting the role of social performance and recognition in category formation. They are not directly subject to the constraint but observe its effects.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, social_role_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent framework for social and legal recognition of individuals' gender, facilitating their integration into society according to their self-declared identity.
% TRANSFER_FUNCTION: Transfers social and institutional legitimacy for gendered category membership from biological sex or social role to subjective gender identity, from those who resist to those who advocate for it.
% ABSENT_VOICES: Advocates for biological sex as the primary determinant of gendered categories are often marginalized or actively silenced in spaces where the gender identity reading is dominant; they would argue for the preservation of sex-based distinctions.
% DISAPPEARANCE_RATIONALE: If this reading of gendered category membership vanished, the social and legal landscape for transgender individuals would fundamentally shift, leading to a loss of recognition and rights. Institutions would revert to sex-based or more ambiguous categorizations, causing widespread social reorganization.
% FOUNDING_PROBLEM: The historical exclusion and misgendering of transgender individuals, leading to significant social distress, discrimination, and lack of legal recognition for their lived gender.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and human rights organizations universally attest to the ongoing problem of misgendering and exclusion. Legal scholars and medical professionals also corroborate the need for frameworks that affirm gender identity, from outside the primary beneficiary group.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).
:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs imposed on institutions and individuals who must adapt to or are penalized for not adhering to this definition, as well as the redefinition of existing categories. Suppression (0.6) is significant due to active social and institutional pressure to conform, and the marginalization of dissenting voices. Theater ratio (0.2) is relatively low, as the constraint's function of affirming gender identity is largely genuine, though some performative aspects exist in institutional compliance. The constraint is claimed as a Tangled Rope because it genuinely coordinates social recognition for transgender individuals (beneficiaries) but does so by extracting costs from those who must adapt or are suppressed (victims).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and advocates, this constraint is a necessary coordination mechanism for social justice and recognition. From the perspective of cisgender women resisting inclusion, it is an extractive redefinition of their categories, imposing costs and suppressing their concerns. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals and gender identity advocates are beneficiaries, experiencing affirmation and expanded rights (low d). Cisgender women who resist inclusion and institutions adapting policies are payers, bearing social, administrative, and sometimes legal costs (high d). Biological sex advocates are largely excluded, facing suppression of their views (high d, but structurally outside the coordination function). Social role theorists are observers, analyzing the dynamics without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_gendered_categories,
    'Which specific categories (e.g., ''woman'', ''mother'', ''female athlete'') are subject to the gender identity reading, and which remain primarily sex-based?',
    'Analysis of legal precedents and institutional policies across different domains (e.g., sports, healthcare, legal identity) to map the boundaries of application.',
    'If the reading''s scope is limited to legal identity but not, for example, competitive sports, the extractiveness and suppression on cisgender women in those specific domains would be lower. If universal, the current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_gendered_categories, empirical, 'Ambiguity regarding the specific domains where gender identity is the sole determinant of category membership.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression experienced by those resisting the gender identity reading primarily structural (e.g., legal penalties, institutional mandates) or internalized (e.g., fear of social ostracization, self-censorship)?',
    'Sociological studies on the mechanisms of social pressure and self-censorship, combined with legal analysis of formal penalties for non-compliance.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the structural measure suggests, as resistance is preempted. If primarily structural, formal remedies might be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Distinguishing between structural and internalized components of suppression for dissenting voices.').

omega_variable(
    coordination_extraction_balance,
    'At what point does the coordination benefit for transgender individuals outweigh the costs imposed on other groups, or vice versa?',
    'Comprehensive social impact assessments, cost-benefit analyses, and ethical frameworks that weigh the well-being of all affected parties, potentially leading to policy adjustments.',
    'A finding that costs significantly outweigh benefits would challenge the constraint''s legitimacy as a coordination mechanism, pushing it closer to a Snare. A finding of net benefit would reinforce its Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_balance, preference, 'The normative balance between the coordination function and the extractive costs of the gender identity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__gender_identity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__gender_identity_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__gender_identity_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__gender_identity_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__gender_identity_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__gender_identity_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, single_sex_spaces_policy).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gender_affirming_healthcare_access).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gendered_category_membership' kernel. Its operation directly influences the viability and legitimacy of alternative readings and related policies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
