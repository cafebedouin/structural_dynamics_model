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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Category 'Woman' Defined by Sex Biology
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint defines 'woman' based on sex biology (XX chromosomes,
 *   female reproductive anatomy), serving as a foundational premise for
 *   sex-segregated policies and protections. It is one reading of a contested
 *   kernel, where other readings propose definitions based on gender identity
 *   or intersex accommodation. This reading aims to protect the interests of
 *   biological females but, in doing so, excludes or ambiguously includes
 *   other groups, leading to significant social and political contestation.
 *   The metrics reflect the increasing enforcement and resistance this
 *   definition faces as alternative readings gain prominence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.6).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.7).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Category 'Woman' Defined by Sex Biology").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'ddebacef-96cf-4932-a8c7-5b33d60d6db3').
narrative_ontology:cs_kernel_codification('ddebacef-96cf-4932-a8c7-5b33d60d6db3', formalized).
narrative_ontology:cs_authority_grounding('ddebacef-96cf-4932-a8c7-5b33d60d6db3', lineage).
narrative_ontology:cs_interpretation_layer_present('ddebacef-96cf-4932-a8c7-5b33d60d6db3').
narrative_ontology:cs_reading_relation('ddebacef-96cf-4932-a8c7-5b33d60d6db3', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('ddebacef-96cf-4932-a8c7-5b33d60d6db3', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('ddebacef-96cf-4932-a8c7-5b33d60d6db3', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('ddebacef-96cf-4932-a8c7-5b33d60d6db3', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('ddebacef-96cf-4932-a8c7-5b33d60d6db3', foundational, sex_based_rights_are_essential).
narrative_ontology:cs_axiom_status(sex_based_rights_are_essential, holdable).
narrative_ontology:cs_axiom_grounding('ddebacef-96cf-4932-a8c7-5b33d60d6db3', sex_based_rights_are_essential, deontological).
narrative_ontology:cs_reference_frame('ddebacef-96cf-4932-a8c7-5b33d60d6db3', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('ddebacef-96cf-4932-a8c7-5b33d60d6db3', contemporary_gender_identity_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ddebacef-96cf-4932-a8c7-5b33d60d6db3', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_segregated_women).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, women_in_sports).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, women_in_violence_shelters).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, some_intersex_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the preservation of sex-segregated spaces, services, and data collection, which they argue are necessary for safety, fairness, and accurate policy-making. They actively advocate for this definition.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_segregated_women, beneficiary,
    organized, generational, constrained, global).

% Benefit from the exclusion of transgender women from women's sports categories, citing biological performance advantages. They perceive this definition as essential for fair competition and equal opportunity in sports.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, women_in_sports, beneficiary,
    organized, biographical, constrained, global).

% Benefit from policies that restrict access to women-only spaces (like shelters) based on biological sex, citing safety and trauma-informed care. They rely on this definition for their sense of security and privacy.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, women_in_violence_shelters, beneficiary,
    moderate, immediate, constrained, local).

% Are excluded from categories and protections designated for 'women' under this definition, leading to social, legal, and physical marginalization. They bear the cost of non-recognition and lack of access to gender-affirming spaces.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, identity_locked, global).

% May find their identities and experiences ambiguously or inadequately accommodated by a strict binary biological definition, particularly if their biology does not fit typical XX/XY categories but they identify as women. They bear the cost of definitional ambiguity and potential exclusion.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, some_intersex_individuals, payer,
    powerless, biographical, identity_locked, global).

% Are tasked with codifying and enforcing definitions of 'woman' in law and policy, balancing competing claims. They face political pressure from various advocacy groups and legal challenges regardless of the definition adopted.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for inclusive definitions of 'woman' that encompass gender identity, arguing that biological essentialism violates human rights and perpetuates discrimination. They are often excluded from policy-making processes that adopt strict biological definitions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, human_rights_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social and legal categories around a clear, empirically verifiable biological referent for 'woman', providing a stable basis for sex-segregated policies, data collection, and protections.
% TRANSFER_FUNCTION: Transfers definitional authority and access to sex-segregated resources and protections to individuals meeting specific biological criteria, while denying or restricting them for those who do not.
% ABSENT_VOICES: Transgender women and their allies, as well as many intersex advocates, are often excluded from the foundational discussions that establish this definition, or their perspectives are dismissed. They would argue for definitions based on gender identity or a more nuanced understanding of biological sex.
% DISAPPEARANCE_RATIONALE: If this definition vanished overnight, the legal and social landscape around sex-segregated spaces, sports, and data collection would be thrown into immediate disarray, requiring new frameworks to manage access and categorization. Policies based on this definition would cease to function.
% FOUNDING_PROBLEM: The need for clear, unambiguous categories to protect and advocate for the specific interests and vulnerabilities of biological females, particularly in contexts like sports, healthcare, and safety from male violence.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for sex-based rights and feminist organizations attest that the problem of protecting biological females' interests is ongoing and exacerbated by challenges to sex-based categories. This is corroborated by data on sex-specific health outcomes and violence against women, though the interpretation of how best to address these issues is contested.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).

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
 *   Extractiveness (0.6) is substantial as this definition grants or denies access to resources and recognition based on biological criteria, imposing costs on those excluded. Suppression (0.7) is high due to active efforts to enforce this definition in law and policy, often against strong resistance from those advocating for broader definitions. Resistance (0.8) is very high, reflecting the intense social and political struggle over this category. Accessibility collapse (0.4) is moderate; while this definition is widely used, alternative conceptualizations of 'woman' persist and are actively promoted. Theater ratio (0.2) is low, as the definition is actively debated and enforced, not merely maintained for show.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of biological women who advocate for this definition, it is a necessary coordination mechanism to protect their specific interests and ensure fairness. From the perspective of transgender women and many intersex individuals, it is an extractive and suppressive constraint that denies their identity and access to essential resources. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Biological women who benefit from sex-segregated spaces and policies are beneficiaries (d near 0.0). Transgender women and some intersex individuals are victims (d near 1.0) as they are excluded or marginalized by this definition. Policy makers are agenda-setters, navigating the enforcement of this definition amidst competing claims. Human rights advocates are excluded, actively resisting the constraint from outside the formal decision-making process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_determinism_vs_social_construction,
    'Is the category ''woman'' fundamentally determined by immutable biological facts, or is it a socially constructed category with flexible boundaries?',
    'Philosophical consensus on the nature of social categories, or empirical data on the malleability of gender roles and identities across cultures and time.',
    'If primarily biological, this reading''s claims to naturalness and necessity are strengthened; if primarily social, its extractive and suppressive aspects become more salient as a constructed rather than inherent constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determinism_vs_social_construction, conceptual, 'The fundamental nature of the category ''woman''.').

omega_variable(
    intersex_inclusion_ambiguity,
    'How does this sex-biology reading specifically accommodate or exclude individuals with intersex variations who identify as women but do not fit typical XX chromosomal or anatomical definitions?',
    'Detailed policy guidelines and legal precedents explicitly addressing various intersex conditions within this definitional framework.',
    'If intersex individuals are consistently excluded or marginalized, the victim set expands and the extractiveness of the constraint increases; if specific accommodations are made, the constraint''s scope and impact on this group are mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'Clarity of intersex inclusion within the sex-biology definition.').

omega_variable(
    sports_fairness_empirical_basis,
    'What is the precise empirical evidence for biological performance advantages of transgender women in women''s sports, and how does it vary by sport, level of competition, and duration of hormone therapy?',
    'Comprehensive, longitudinal scientific studies on athletic performance outcomes across diverse sports and populations, with transparent methodology and peer review.',
    'Strong, consistent evidence of significant, unmitigable advantage would bolster the ''fairness'' argument for exclusion, potentially reducing the perceived extractiveness for biological women. Weak or inconsistent evidence would undermine this justification, increasing the perceived extractiveness for transgender women.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_fairness_empirical_basis, empirical, 'Empirical basis for fairness claims in sports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1970, woman_category__sex_biology_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(woma_tr_t1990, woman_category__sex_biology_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(woma_tr_t2010, woman_category__sex_biology_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(woma_tr_t2024, woman_category__sex_biology_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t1970, woman_category__sex_biology_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(woma_be_t1990, woman_category__sex_biology_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(woma_be_t2010, woman_category__sex_biology_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(woma_be_t2024, woman_category__sex_biology_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1970, woman_category__sex_biology_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(woma_su_t1990, woman_category__sex_biology_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(woma_su_t2010, woman_category__sex_biology_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(woma_su_t2024, woman_category__sex_biology_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, intersex_accommodation_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, transgender_sports_participation_rules).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, violence_against_women_act_definitions).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'woman_category' kernel, focusing on sex biology. It is linked to sibling readings that define 'woman' by gender identity or intersex accommodation, as well as downstream policies that rely on these definitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
