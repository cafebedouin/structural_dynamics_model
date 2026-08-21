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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category: Sex-Biology Definition
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint defines 'woman' as an adult human female with XX
 *   chromosomes and typical female reproductive anatomy, serving as a
 *   foundational category in various social, legal, and policy contexts. It
 *   is a reading of the broader 'woman_category' kernel. While presented by
 *   proponents as a natural and necessary distinction, its application in
 *   policy actively enforces boundaries, leading to significant extraction
 *   from those who do not fit this strict biological definition, particularly
 *   transgender women. The high extractiveness and suppression reflect the
 *   real-world consequences of this definition in areas like sports,
 *   healthcare, and access to sex-segregated spaces.
 *
 * KEY AGENTS:
 *   - advocates_for_sex_based_rights: Primary agenda setter (organized/constrained) — defends the definition
 *   - cisgender_women: Primary beneficiary (moderate/mobile) — benefits from sex-segregated spaces/policies
 *   - transgender_women: Primary target/victim (powerless/identity_locked) — excluded from spaces/policies
 *   - some_intersex_individuals: Secondary target/victim (powerless/identity_locked) — ambiguously included/excluded
 *   - sports_governing_bodies: Institutional agenda setter (institutional/constrained) — enforces definition in sports
 *   - policy_makers: Institutional agenda setter (institutional/constrained) — implements definition in law
 *   - gender_identity_advocates: Excluded voice (organized/constrained) — would object to the definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.75).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.8).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category: Sex-Biology Definition").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2').
narrative_ontology:cs_kernel_codification('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', formalized).
narrative_ontology:cs_authority_grounding('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', expertise).
narrative_ontology:cs_interpretation_layer_present('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2').
narrative_ontology:cs_reading_relation('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', foundational, biological_sex_is_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_binary, holdable).
narrative_ontology:cs_axiom_grounding('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', biological_sex_is_binary, empirically_contingent).
narrative_ontology:cs_axiom('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', foundational, sex_matters_for_social_categories).
narrative_ontology:cs_axiom_status(sex_matters_for_social_categories, holdable).
narrative_ontology:cs_axiom_grounding('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', sex_matters_for_social_categories, instrumental).
narrative_ontology:cs_reference_frame('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', biological_sex_dichotomy).
narrative_ontology:cs_drift_state('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', contemporary_gender_identity_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6425c9eb-20a9-4c84-b3b2-6888ffc1f8b2', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, advocates_for_sex_based_rights).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, cisgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, some_intersex_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and defend the definition of 'woman' based on biological sex in policy, law, and public discourse. They see this as essential for protecting the rights and spaces of biological females.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, advocates_for_sex_based_rights, agenda_setter,
    organized, generational, constrained, global).

% Benefit from sex-segregated spaces (e.g., changing rooms, shelters, sports categories) and policies designed specifically for biological females, which this definition underpins. They may also bear costs if the definition is seen as divisive.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, cisgender_women, beneficiary,
    moderate, biographical, mobile, global).

% Are excluded from spaces and policies defined by biological sex, leading to denial of recognition, access, and specific protections. Their identity is often invalidated by this definition in public discourse.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, immediate, identity_locked, global).

% May be ambiguously included or excluded depending on the specific biological criteria applied (e.g., chromosomes vs. reproductive anatomy), leading to uncertainty, medical gatekeeping, and denial of full inclusion in either sex category.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, some_intersex_individuals, payer,
    powerless, biographical, identity_locked, global).

% Implement and enforce rules for sex-segregated sports categories based on biological sex, often citing fairness and competitive integrity. They face pressure from both advocates for sex-based rights and transgender inclusion.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Draft and enact legislation and policies that rely on a biological definition of 'woman' for various purposes, including data collection, healthcare, and legal protections. They navigate intense public and political debate.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for definitions of 'woman' based on gender identity, arguing that biological definitions are discriminatory and harmful. They are often excluded from policy discussions that adopt the sex-biology reading.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, advocates_for_sex_based_rights).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, biologically-defined category for 'woman' to enable sex-segregated spaces, data collection, and policies intended to protect or serve biological females.
% TRANSFER_FUNCTION: Transfers access, recognition, and resources (e.g., eligibility for specific protections, participation in sex-segregated sports) to individuals who meet the biological definition, while denying these to those who do not.
% ABSENT_VOICES: Transgender women, gender identity advocates, and some intersex advocates are often excluded from policy-making spaces where this definition is applied, and would argue for broader, more inclusive definitions.
% DISAPPEARANCE_RATIONALE: If this definition vanished overnight, policies and spaces currently relying on it (e.g., sex-segregated sports, women's shelters, specific legal protections) would lose their foundational basis, leading to significant re-evaluation and reorganization of social and legal categories.
% FOUNDING_PROBLEM: To establish a clear and consistent definition of 'woman' for legal, social, and biological purposes, particularly to ensure protections and rights specific to biological females and for scientific clarity.
% FOUNDING_PROBLEM_CORROBORATION: Biologists and medical professionals corroborate the biological basis of sex. Advocates for sex-based rights corroborate the ongoing need for sex-specific protections and categories. Opponents argue the founding problem is framed too narrowly, ignoring gender identity and intersex variations.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.75) because the definition, when applied in policy, denies access and recognition to individuals who identify as women but do not meet the biological criteria. Suppression is also high (0.80) due to active enforcement mechanisms (e.g., legal challenges, policy mandates, social pressure) that maintain the boundary and exclude alternatives. The theater ratio is low (0.10) because the definition is functionally applied with real-world consequences, not merely performative. Resistance is high (0.70) reflecting the intense social and political contestation surrounding this definition. The claimed type is 'tangled_rope' because it provides a coordination function (clear categories for specific purposes) but also involves significant asymmetric extraction and requires active enforcement to maintain its boundaries.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this definition (e.g., advocates_for_sex_based_rights, cisgender_women) experience it as a necessary coordination mechanism for fairness and protection. Those excluded (e.g., transgender_women, some_intersex_individuals) experience it as a highly extractive and suppressive barrier. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Advocates for sex-based rights and cisgender women are beneficiaries, as the constraint secures spaces and resources for biological females. Transgender women and some intersex individuals are victims, as they are excluded or ambiguously positioned. Sports governing bodies and policy makers act as agenda setters, enforcing the definition. Gender identity advocates are excluded, representing a voice that would challenge the constraint's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (clear biological definition for specific purposes) is still live, but its application has shifted from a largely uncontested biological fact to a highly contested social and political boundary. The high extractiveness and suppression indicate that while a coordination function exists, it is heavily intertwined with exclusion and rent-seeking (in terms of maintaining social power and access). This prevents mislabeling it as a pure Rope or a Piton, as it is actively maintained and highly impactful.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a stable, independent definition, or one reading of a contested kernel?',
    'Analysis of public discourse, legal challenges, and academic debate surrounding the definition of ''woman''. The presence of multiple, actively debated alternative definitions confirms its status as a kernel reading.',
    'If it were a stable, independent definition, its classification would be less sensitive to external contestation. As a kernel reading, its stability and classification are inherently linked to the dynamics of the broader ''woman_category'' kernel and its sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''sex_biology_reading'' of the ''woman_category'' kernel.').

omega_variable(
    definition_scope_ambiguity,
    'To what extent is the biological definition of ''woman'' intended to apply universally across all contexts (social, legal, medical), versus being context-specific (e.g., only for sports or reproductive health)?',
    'Examination of legislative intent, judicial rulings, and policy documents that apply this definition. Divergent applications across contexts would suggest a more nuanced, context-dependent interpretation.',
    'If the definition is intended as universal, the measured extractiveness and suppression are broadly applicable. If it is context-specific, the impact on excluded groups might be localized, potentially lowering the effective extractiveness in other domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_scope_ambiguity, empirical, 'Ambiguity in the intended scope of the biological definition of ''woman''.').

omega_variable(
    intersex_inclusion_ambiguity,
    'How are various intersex conditions accommodated or excluded by the ''typical case'' clause in the sex-biology definition?',
    'Detailed analysis of medical guidelines, legal precedents, and policy applications concerning specific intersex variations. This would clarify which intersex individuals are included/excluded and under what criteria.',
    'If intersex individuals are consistently excluded or forced into binary categories that do not fit their biology, the effective extractiveness for this group is higher. If accommodations are made, it could reduce the measured suppression and extraction for this specific victim group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'Clarity on the inclusion/exclusion of intersex individuals within the sex-biology definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_category__sex_biology_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(woma_tr_t2005, woman_category__sex_biology_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(woma_tr_t2010, woman_category__sex_biology_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(woma_tr_t2015, woman_category__sex_biology_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(woma_tr_t2020, woman_category__sex_biology_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(woma_tr_t2025, woman_category__sex_biology_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_category__sex_biology_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(woma_be_t2005, woman_category__sex_biology_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(woma_be_t2010, woman_category__sex_biology_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(woma_be_t2015, woman_category__sex_biology_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(woma_be_t2020, woman_category__sex_biology_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(woma_be_t2025, woman_category__sex_biology_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_category__sex_biology_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(woma_su_t2005, woman_category__sex_biology_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(woma_su_t2010, woman_category__sex_biology_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(woma_su_t2015, woman_category__sex_biology_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(woma_su_t2020, woman_category__sex_biology_reading, suppression_requirement, 2020, 0.79).
narrative_ontology:measurement(woma_su_t2025, woman_category__sex_biology_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_category' kernel. Its definition of 'woman' based on biological sex directly impacts the scope and legitimacy of the 'gender_identity_reading' and 'intersex_accommodation_reading' by setting a strict boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
