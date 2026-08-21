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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gendered Category Membership via Gender Identity
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint describes the framework for gendered category membership
 *   (e.g., 'woman', 'man') when grounded in subjective gender identity and
 *   self-declaration. It is one reading of the broader
 *   'gendered_category_membership' kernel. This reading asserts that an
 *   individual's internal sense of gender is the primary determinant for
 *   their membership in gendered categories, leading to the inclusion of
 *   transgender individuals based on self-ID. The constraint functions to
 *   coordinate social recognition and access but imposes costs on those who
 *   adhere to alternative definitions, particularly those based on biological
 *   sex.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.68).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.75).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gendered Category Membership via Gender Identity").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '351c8723-2a75-4c2e-8d7e-29863f00bc09').
narrative_ontology:cs_kernel_codification('351c8723-2a75-4c2e-8d7e-29863f00bc09', formalized).
narrative_ontology:cs_authority_grounding('351c8723-2a75-4c2e-8d7e-29863f00bc09', practice).
narrative_ontology:cs_interpretation_layer_present('351c8723-2a75-4c2e-8d7e-29863f00bc09').
narrative_ontology:cs_reading_relation('351c8723-2a75-4c2e-8d7e-29863f00bc09', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('351c8723-2a75-4c2e-8d7e-29863f00bc09', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('351c8723-2a75-4c2e-8d7e-29863f00bc09', foundational, gender_is_internal_sense).
narrative_ontology:cs_axiom_status(gender_is_internal_sense, holdable).
narrative_ontology:cs_axiom_grounding('351c8723-2a75-4c2e-8d7e-29863f00bc09', gender_is_internal_sense, deontological).
narrative_ontology:cs_axiom('351c8723-2a75-4c2e-8d7e-29863f00bc09', foundational, self_declaration_is_sufficient_for_membership).
narrative_ontology:cs_axiom_status(self_declaration_is_sufficient_for_membership, holdable).
narrative_ontology:cs_axiom_grounding('351c8723-2a75-4c2e-8d7e-29863f00bc09', self_declaration_is_sufficient_for_membership, conventional).
narrative_ontology:cs_reference_frame('351c8723-2a75-4c2e-8d7e-29863f00bc09', inclusive_self_identification).
narrative_ontology:cs_drift_state('351c8723-2a75-4c2e-8d7e-29863f00bc09', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('351c8723-2a75-4c2e-8d7e-29863f00bc09', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_critical_feminists).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cisgender_women_in_sex_segregated_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, institutional_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from recognition and inclusion in gendered categories and spaces that align with their self-declared gender identity. Their identity is deeply tied to this recognition, making exit from the framework (e.g., by accepting alternative categorizations) highly constrained.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Actively promote and enforce the principle of gender identity as the primary basis for category membership. They work to shape public discourse, institutional policy, and legal frameworks to reflect this reading, often facing significant opposition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, agenda_setter,
    organized, generational, constrained, global).

% Bear the costs of redefining gendered categories, particularly 'woman', to include transgender individuals. They argue this undermines sex-based rights and protections, and face social and professional ostracization for resisting the gender identity reading. Their exit options are limited to forming alternative communities or engaging in sustained, often penalized, dissent.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_feminists, payer,
    moderate, generational, constrained, global).

% Experience the direct impact of sex-segregated spaces (e.g., changing rooms, shelters, sports) becoming gender-segregated. They may feel their privacy, safety, or fair competition is compromised, but have limited power to resist institutional policies that adopt the gender identity reading.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cisgender_women_in_sex_segregated_spaces, payer,
    powerless, immediate, constrained, local).

% Governments, corporations, and NGOs that adopt policies based on the gender identity reading. They benefit from appearing inclusive and avoiding legal challenges, but bear the costs of implementing new policies and managing internal and external dissent. Their exit is constrained by social pressure and legal precedent.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, institutional_bodies, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, institutional_bodies, beneficiary).

% Analyze the conceptual coherence, ethical implications, and social consequences of different approaches to gendered category membership. They are not directly subject to the constraint but provide critical commentary and theoretical frameworks.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, analytical_philosophers_and_ethicists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a consistent and inclusive framework for gendered category membership based on an individual's internal sense of gender, facilitating social recognition and access to spaces and resources for transgender individuals.
% TRANSFER_FUNCTION: Transfers social recognition, definitional authority over gendered categories, and access to gendered spaces/resources from traditional biological/social definitions to individual self-declaration. It also transfers the burden of adaptation and potential social costs (e.g., perceived loss of sex-based protections) to those who adhere to alternative definitions.
% ABSENT_VOICES: Those who believe gendered categories are exclusively tied to immutable biological sex or traditional social roles are often marginalized or dismissed as discriminatory within this framework. Their perspectives are structurally excluded from mainstream policy-making and public discourse that adopts the gender identity reading.
% DISAPPEARANCE_RATIONALE: If the principle of gender identity as the sole or primary basis for gendered category membership vanished overnight, social categories, legal frameworks, and institutional policies would need to be fundamentally re-evaluated. This would lead to significant rearrangement of social norms, access to spaces, and legal protections, particularly for transgender individuals and those who previously resisted the gender identity reading.
% FOUNDING_PROBLEM: Historical exclusion, discrimination, and misgendering of transgender individuals from gendered categories and spaces, leading to significant social and psychological harm, and a lack of legal and social recognition for their identities.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and their allies attest to the ongoing problem of exclusion and misrecognition. Human rights organizations and some cisgender allies corroborate the historical and ongoing harm. Opponents, however, contest the proposed solution's impact on other groups and the nature of the 'problem' itself.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is substantial because the redefinition of categories, while beneficial for some, imposes significant social and conceptual costs on others, particularly cisgender women who may feel their sex-based rights are eroded. Suppression (0.75) is high due to active social and institutional pressure to conform to this reading, often framing dissent as discriminatory. The theater ratio (0.25) is moderate; while there's genuine intent for inclusion, some enforcement may be performative to avoid backlash rather than deeply integrated. Resistance (0.70) is high, reflecting ongoing public and academic debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and gender identity advocates, this constraint is a necessary 'rope' for inclusion and recognition, solving a fundamental problem of misgendering and exclusion. From the perspective of gender-critical feminists and some cisgender women, it operates as a 'snare' or 'tangled rope', extracting from their existing rights and categories while coordinating a new social order that they perceive as harmful. The engine's computation of per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals and gender identity advocates are clear beneficiaries (low d) as the constraint directly affirms their identities and facilitates their inclusion. Gender-critical feminists and cisgender women in sex-segregated spaces are targets (high d) as they bear the costs of category redefinition and may experience perceived loss of protections. Institutional bodies act as agenda-setters, balancing perceived benefits of inclusivity with the costs of managing dissent.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to ensure inclusion for transgender individuals remains live. However, the analysis reveals that the mechanism of achieving this (self-declaration as the sole basis for category membership) has generated significant extraction and suppression for other groups. This prevents mislabeling it as a pure 'rope' (which would ignore the victims) or a pure 'snare' (which would ignore the genuine coordination function for beneficiaries). The 'tangled_rope' classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_scope_ambiguity,
    'Is ''gender'' in ''gender identity'' referring to a social role, an internal sense, or a legal status, and how does this scope affect category boundaries?',
    'Conceptual clarification through philosophical analysis and legal precedent, distinguishing between different referents of ''gender'' and their implications for category membership.',
    'If ''gender'' is primarily a social role, the ''social_role_reading'' gains more traction. If it''s purely an internal sense, the ''gender_identity_reading'' is strengthened but may face challenges in practical application. If it''s a legal status, the ''biological_sex_reading'' might be foreclosed in law but persist in social practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Ambiguity in the definition of ''gender'' and its impact on category membership.').

omega_variable(
    impact_on_sex_based_rights,
    'Does the inclusion of transgender individuals in gendered categories via self-ID genuinely undermine sex-based rights and protections for cisgender women, or are these concerns unfounded?',
    'Empirical studies on the impact of gender-inclusive policies on women''s safety, privacy, and opportunities in sex-segregated spaces, combined with legal analysis of rights frameworks.',
    'If undermining is demonstrated, the extractiveness and suppression for cisgender women are higher, strengthening the ''snare'' aspect for that seat. If concerns are unfounded, the extractiveness is lower, and the constraint leans more towards a ''rope'' for all, with only conceptual disagreement remaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_rights, empirical, 'Empirical impact of gender identity reading on sex-based rights.').

omega_variable(
    internalized_suppression_of_dissent,
    'To what extent is the suppression of dissent against the gender identity reading internalized by individuals, beyond structural barriers?',
    'Sociological studies on self-censorship, social pressure, and identity-fusion mechanisms among those who might otherwise dissent, particularly in professional or academic contexts.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, making exit options like ''constrained'' or ''mobile'' less viable in practice, pushing agents closer to ''identity_locked'' or ''trapped'' due to social costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_dissent, empirical, 'Structural vs. internalized suppression mechanism for dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__gender_identity_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__gender_identity_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__gender_identity_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__gender_identity_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__gender_identity_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__gender_identity_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sex_segregated_spaces_policy).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, legal_gender_recognition_laws).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, sports_eligibility_rules).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gendered_category_membership' kernel. This reading focuses on gender identity as the primary determinant, while sibling readings focus on biological sex or social role. Each reading has distinct beneficiaries, victims, and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
