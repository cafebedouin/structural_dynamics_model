% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'intersex accommodation' reading
 *   of the 'woman_category' kernel. It defines 'woman' to include individuals
 *   with typical female biology and intersex variations that do not fit the
 *   male category, acknowledging biological sex as a spectrum. This reading
 *   aims to provide recognition and reduce harm for intersex individuals,
 *   challenging rigid binary definitions. The metrics reflect a relatively
 *   low extractiveness and suppression, as its primary function is
 *   coordination and inclusion, though it faces resistance from other
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.15).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.25).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '41e4936c-5470-4976-b42c-975f6c3f4d70').
narrative_ontology:cs_kernel_codification('41e4936c-5470-4976-b42c-975f6c3f4d70', distributed).
narrative_ontology:cs_authority_grounding('41e4936c-5470-4976-b42c-975f6c3f4d70', expertise).
narrative_ontology:cs_interpretation_layer_present('41e4936c-5470-4976-b42c-975f6c3f4d70').
narrative_ontology:cs_reading_relation('41e4936c-5470-4976-b42c-975f6c3f4d70', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('41e4936c-5470-4976-b42c-975f6c3f4d70', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('41e4936c-5470-4976-b42c-975f6c3f4d70', foundational, biological_sex_is_a_spectrum).
narrative_ontology:cs_axiom_status(biological_sex_is_a_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('41e4936c-5470-4976-b42c-975f6c3f4d70', biological_sex_is_a_spectrum, empirically_contingent).
narrative_ontology:cs_axiom('41e4936c-5470-4976-b42c-975f6c3f4d70', foundational, inclusion_of_intersex_individuals_is_ethical_imperative).
narrative_ontology:cs_axiom_status(inclusion_of_intersex_individuals_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('41e4936c-5470-4976-b42c-975f6c3f4d70', inclusion_of_intersex_individuals_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('41e4936c-5470-4976-b42c-975f6c3f4d70', scientifically_informed_inclusive_sex_categories).
narrative_ontology:cs_drift_state('41e4936c-5470-4976-b42c-975f6c3f4d70', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('41e4936c-5470-4976-b42c-975f6c3f4d70', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_individuals).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, medical_professionals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, women_with_typical_female_biology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from a category definition that acknowledges their biological reality, reducing medical pathologization and social exclusion. Their identity is intrinsically linked to their biological sex characteristics, which are not strictly binary.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_individuals, beneficiary,
    powerless, biographical, identity_locked, global).

% This group benefits from a more nuanced and scientifically accurate understanding of sex, aligning medical practice with biological diversity rather than rigid binary definitions. It supports ethical care and research for intersex conditions.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_professionals, beneficiary,
    institutional, generational, mobile, global).

% Responsible for crafting policies that reflect this understanding, particularly in areas like healthcare, legal recognition, and anti-discrimination. They face pressure from various advocacy groups but are structurally positioned to implement inclusive definitions.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% This group benefits from a more accurate and inclusive scientific understanding of sex, which can lead to better health outcomes and reduced stigma for all women. They are not directly targeted by this reading but are part of the broader category definition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, women_with_typical_female_biology, beneficiary,
    organized, biographical, mobile, global).

% This group's primary focus is on gender identity as the determinant of 'woman' category. While not directly opposed to intersex accommodation, their framework prioritizes self-identification over biological variation, leading to potential tension in policy application.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_advocates, excluded,
    organized, biographical, constrained, global).

% This group advocates for a strict binary definition of sex based on typical reproductive biology. They view intersex variations as deviations from the norm, which this reading challenges by integrating them into the 'woman' category, leading to direct conceptual conflict.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a more scientifically accurate and inclusive understanding of biological sex within the 'woman' category, facilitating appropriate medical care, legal recognition, and social inclusion for intersex individuals.
% TRANSFER_FUNCTION: Transfers social and medical recognition, as well as legal protections, to intersex individuals who might otherwise be excluded or pathologized by strictly binary sex definitions. It also transfers a more nuanced understanding of sex to broader society.
% ABSENT_VOICES: While not entirely absent, the voices of those advocating for strictly binary sex definitions or those prioritizing gender identity over biological sex may feel their concerns are not fully addressed within this specific reading's framework, particularly in contexts like sports where biological differences are highly salient.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, intersex individuals would revert to being categorized solely within a binary framework, leading to increased medical pathologization, social exclusion, and legal ambiguity. Policies would fail to accommodate their specific needs, and scientific understanding would regress.
% FOUNDING_PROBLEM: The historical and ongoing exclusion, pathologization, and lack of recognition for individuals whose biological sex characteristics do not fit a strict male/female binary, leading to significant harm and human rights violations.
% FOUNDING_PROBLEM_CORROBORATION: Intersex advocacy organizations, human rights bodies, and medical ethics committees consistently corroborate the ongoing nature of this problem, citing continued discrimination, non-consensual medical interventions, and lack of legal recognition for intersex individuals globally.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).
:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading primarily seeks to include and recognize, rather than extract from, any group. Suppression (0.25) is present due to the need to actively counter prevailing binary norms and resist alternative readings that would exclude intersex individuals. Theater ratio is low (0.05) as the constraint is genuinely functional in its aim for inclusion. Accessibility collapse is moderate (0.3) because while it offers a clear path for intersex individuals, it doesn't fully collapse the alternatives of binary or gender-identity-based definitions for other contexts. Resistance (0.1) comes from advocates of other readings who see this as either too narrow (gender identity) or too broad (sex biology).
 *
 * PERSPECTIVAL GAP:
 *   While this reading is largely beneficial for intersex individuals, it creates a conceptual tension with both the strict 'sex_biology_reading' and the 'gender_identity_reading'. From the perspective of sex_biology_advocates, this reading might be seen as diluting the category of 'woman'. From gender_identity_advocates, it might be seen as overly focused on biology. This story focuses on the benefits and coordination function of the intersex accommodation reading itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Intersex individuals and medical professionals are clear beneficiaries, gaining recognition and a more accurate framework. Policy makers are agenda-setters, tasked with implementing this understanding. Advocates for other readings are excluded, as their frameworks are not prioritized here, leading to conceptual tension rather than direct extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_definition_ambiguity,
    'How precisely can ''intersex variations that do not fit male category'' be defined without creating new exclusions or ambiguities at the margins?',
    'Development of internationally recognized medical and legal criteria for intersex variations, coupled with case law establishing precedents for inclusion.',
    'If the boundary remains ambiguous, the reading''s effectiveness in providing clear recognition is reduced, potentially leading to continued exclusion or inconsistent application. If clear, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_definition_ambiguity, conceptual, 'Ambiguity in defining the precise scope of intersex variations within the ''woman'' category.').

omega_variable(
    sports_policy_tension,
    'How does this reading''s accommodation of biological spectrum interact with policies in competitive sports, where performance advantages related to sex characteristics are highly salient?',
    'Specific policy frameworks developed by sports governing bodies that integrate intersex accommodation with fair competition principles, potentially involving separate categories or specific eligibility criteria.',
    'In elite sports, this reading''s low extractiveness could become high if it leads to exclusion based on biological advantage, or if it is perceived to undermine fair competition for typical females. Its classification could shift to a Tangled Rope in that specific domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sports_policy_tension, preference, 'Tension between inclusive category definition and fair competition in sports.').

omega_variable(
    coexistence_with_gender_identity_reading,
    'Can this biologically-grounded reading of ''woman'' coexist harmoniously with a ''gender_identity_reading'' in all social and legal contexts, or do they create irreconcilable conflicts?',
    'Empirical observation of policy implementation in jurisdictions attempting to integrate both frameworks; conceptual analysis of logical consistency across different domains (e.g., single-sex spaces vs. legal identity).',
    'If irreconcilable, the ''coexists_with'' relation might shift to ''forecloses'' in specific contexts, leading to a more extractive outcome for one reading''s beneficiaries. If harmonious, the overall social coordination around ''woman'' is enhanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_with_gender_identity_reading, conceptual, 'Potential for conflict between intersex accommodation and gender identity definitions of ''woman''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(woma_tr_t5, woman_category__intersex_accommodation_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(woma_tr_t10, woman_category__intersex_accommodation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(woma_tr_t15, woman_category__intersex_accommodation_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(woma_tr_t20, woman_category__intersex_accommodation_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(woma_be_t5, woman_category__intersex_accommodation_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(woma_be_t10, woman_category__intersex_accommodation_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(woma_be_t15, woman_category__intersex_accommodation_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(woma_be_t20, woman_category__intersex_accommodation_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(woma_su_t5, woman_category__intersex_accommodation_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(woma_su_t10, woman_category__intersex_accommodation_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(woma_su_t15, woman_category__intersex_accommodation_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(woma_su_t20, woman_category__intersex_accommodation_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'woman_category' kernel, focusing on intersex accommodation. It is linked to other readings (sex_biology_reading, gender_identity_reading) through the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
