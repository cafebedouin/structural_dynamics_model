% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category (Sex-Biology Reading)
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint defines 'woman' based on chromosomal, anatomical, and
 *   reproductive biology (typically XX chromosomes and female reproductive
 *   anatomy). It is a reading of the broader 'woman_category' kernel, which
 *   is contested by gender identity and intersex accommodation readings. This
 *   reading aims to secure sex-based protections and resources for biological
 *   females, but in doing so, it excludes transgender women and creates
 *   ambiguity for intersex individuals. The constraint is claimed as a Rope
 *   by its proponents, emphasizing its coordination function for
 *   female-specific rights, but its operation is substantially extractive and
 *   suppressive for those it excludes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.65).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.7).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category (Sex-Biology Reading)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'd5c9bb7b-8629-4050-8c04-d0e4522dcb31').
narrative_ontology:cs_kernel_codification('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', formalized).
narrative_ontology:cs_authority_grounding('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', practice).
narrative_ontology:cs_interpretation_layer_present('d5c9bb7b-8629-4050-8c04-d0e4522dcb31').
narrative_ontology:cs_reading_relation('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', foundational, sex_based_rights_are_essential).
narrative_ontology:cs_axiom_status(sex_based_rights_are_essential, holdable).
narrative_ontology:cs_axiom_grounding('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', sex_based_rights_are_essential, deontological).
narrative_ontology:cs_reference_frame('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', traditional_sex_based_categories).
narrative_ontology:cs_drift_state('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', contemporary_gender_identity_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d5c9bb7b-8629-4050-8c04-d0e4522dcb31', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_segregated_sports_organizations).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, women_s_shelters_and_services).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, gender_critical_advocates).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_individuals_with_atypical_sex_characteristics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define eligibility criteria for women's sports categories based on biological sex, aiming to ensure fair competition. They enforce rules that exclude transgender women and may require biological verification, facing legal challenges and public pressure.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_segregated_sports_organizations, agenda_setter,
    institutional, generational, constrained, national).

% Provide services exclusively to biological women, citing safety and specific needs. They advocate for policies that define 'woman' by sex, facing funding threats and accusations of discrimination.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, women_s_shelters_and_services, agenda_setter,
    organized, biographical, constrained, local).

% Actively promote and defend the sex-based definition of 'woman' in law and policy. They benefit from the clarity and perceived protection this definition offers to biological females, organizing politically and through media.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_critical_advocates, beneficiary,
    organized, generational, mobile, global).

% Are excluded from sex-segregated spaces and services designated for 'women' under this definition. They bear the social and practical costs of non-recognition, facing discrimination and barriers to full participation in society. Their identity is deeply tied to their self-identification as women.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, immediate, identity_locked, national).

% May be ambiguously categorized or excluded from 'woman' spaces if their biology does not strictly conform to typical XX chromosomes and female reproductive anatomy, even if they identify as women. They bear the burden of biological scrutiny and potential exclusion, often facing medical gatekeeping.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_individuals_with_atypical_sex_characteristics, payer,
    powerless, biographical, identity_locked, national).

% Analyze the legal and social implications of sex-based definitions of 'woman,' particularly concerning equality law, non-discrimination, and the rights of marginalized groups. They provide critical analysis of the constraint's operation and its impact.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, feminist_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social and legal categories to ensure sex-based protections and resources for biological females, particularly in contexts like sports, intimate spaces, and data collection for health and violence prevention.
% TRANSFER_FUNCTION: Transfers exclusive access to certain spaces, resources, and legal recognition to biological females, while denying that access to transgender women and potentially intersex individuals who do not meet strict biological criteria.
% ABSENT_VOICES: Transgender women and intersex advocates are often excluded from policy-making bodies that define 'woman' in sex-exclusive terms, despite being directly impacted. They would argue for inclusive definitions based on gender identity or a broader understanding of sex.
% DISAPPEARANCE_RATIONALE: If the sex-biology definition of 'woman' vanished overnight, categories for sports, single-sex spaces, and legal protections would need immediate redefinition, leading to significant social and legal reorganization. The concept of sex-based rights would be fundamentally altered.
% FOUNDING_PROBLEM: The need to protect and provide for biological females as a distinct class, particularly from male-pattern violence and to ensure fair competition in sports.
% FOUNDING_PROBLEM_CORROBORATION: Many women's rights organizations and biological sex advocates attest that the problem of sex-based discrimination and the need for female-specific protections remain live. This is corroborated by ongoing data on violence against women and physiological differences in sports, from sources outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) arises from the denial of access and recognition to transgender women and some intersex individuals in spaces and categories designated for 'women.' Suppression (0.70) is high due to active enforcement of sex-based criteria in sports, services, and legal definitions, often through policy, legal challenges, and social pressure. Theater ratio is low (0.10) as the enforcement is generally direct and functional, not performative. Resistance is high (0.80) from excluded groups and their allies. Accessibility collapse is moderate (0.40) as alternative definitions exist and are actively advocated for, but this reading actively works to collapse them in specific contexts.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a necessary coordination mechanism for female rights and safety, with minimal extraction. Those excluded experience it as a highly extractive and suppressive snare, denying their identity and access. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizations and advocates who define 'woman' by sex biology are beneficiaries and agenda-setters, as they gain clarity and exclusive access to resources/protections. Transgender women and intersex individuals with atypical sex characteristics are payers, bearing the costs of exclusion and non-recognition. Their exit options are identity_locked, as their self-identity as women is fundamental, making 'exit' from the category impossible without profound personal cost. Feminist legal scholars act as observers, analyzing the structural impacts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_definition_ambiguity,
    'How precisely can ''biological sex'' be defined for legal and policy purposes, especially concerning intersex variations?',
    'Development of clear, consistent, and widely accepted medical and legal criteria for sex classification that accommodate biological diversity without undermining the category''s purpose.',
    'If a precise, inclusive definition is found, it could reduce the victim set for intersex individuals. If not, the ambiguity will continue to generate extraction for those who don''t fit the ''typical'' case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_definition_ambiguity, empirical, 'Ambiguity in defining biological sex beyond typical XX/XY, particularly for intersex individuals.').

omega_variable(
    necessity_of_sex_segregation,
    'To what extent is sex-segregation (e.g., in sports, shelters) genuinely necessary to achieve its stated goals (fairness, safety), and could these goals be achieved through less exclusionary means?',
    'Empirical studies on performance differences in sports, safety outcomes in shelters, and the effectiveness of alternative, inclusive policies.',
    'If sex-segregation is found to be non-essential or achievable through other means, the justification for the constraint''s extractiveness would weaken, potentially reclassifying it towards a Snare. If found essential, it would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_sex_segregation, empirical, 'Whether sex-segregation is a necessary or merely convenient mechanism for its stated goals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/policy barriers) or internalized (social stigma, fear of reprisal) for transgender women?',
    'Post-policy-change trajectory: if suppression persists after legal barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(woma_be_t5, woman_category__sex_biology_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(woma_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(woma_be_t15, woman_category__sex_biology_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(woma_su_t5, woman_category__sex_biology_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(woma_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(woma_su_t15, woman_category__sex_biology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, transgender_rights_legislation).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, women_s_health_policy).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, sports_governance_rules).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
