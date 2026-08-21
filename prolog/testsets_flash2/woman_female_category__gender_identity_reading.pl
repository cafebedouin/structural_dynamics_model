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
 *   This constraint story instantiates the 'gender_identity_reading' of the
 *   'woman_female_category' kernel. It describes the structural arrangement
 *   where membership in the category 'woman' or 'female' is determined by
 *   internal self-identification, independent of biological sex. This reading
 *   aims to provide legal and social recognition for transgender individuals,
 *   particularly trans women, aligning with their gender identity. However,
 *   it generates significant contestation and perceived harms for cisgender
 *   women and gender-critical feminists who argue for sex-based categories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.65).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity as Basis for Woman/Female Category Membership").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '0213b7cb-909c-40e7-81d6-0e4d5d459cca').
narrative_ontology:cs_kernel_codification('0213b7cb-909c-40e7-81d6-0e4d5d459cca', formalized).
narrative_ontology:cs_authority_grounding('0213b7cb-909c-40e7-81d6-0e4d5d459cca', lineage).
narrative_ontology:cs_interpretation_layer_present('0213b7cb-909c-40e7-81d6-0e4d5d459cca').
narrative_ontology:cs_reading_relation('0213b7cb-909c-40e7-81d6-0e4d5d459cca', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('0213b7cb-909c-40e7-81d6-0e4d5d459cca', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('0213b7cb-909c-40e7-81d6-0e4d5d459cca', foundational, gender_identity_is_primary_determinant_of_gender).
narrative_ontology:cs_axiom_status(gender_identity_is_primary_determinant_of_gender, holdable).
narrative_ontology:cs_axiom_grounding('0213b7cb-909c-40e7-81d6-0e4d5d459cca', gender_identity_is_primary_determinant_of_gender, deontological).
narrative_ontology:cs_axiom('0213b7cb-909c-40e7-81d6-0e4d5d459cca', secondary, sex_assigned_at_birth_is_irrelevant_to_gender_identity).
narrative_ontology:cs_axiom_status(sex_assigned_at_birth_is_irrelevant_to_gender_identity, holdable).
narrative_ontology:cs_axiom_grounding('0213b7cb-909c-40e7-81d6-0e4d5d459cca', sex_assigned_at_birth_is_irrelevant_to_gender_identity, deontological).
narrative_ontology:cs_reference_frame('0213b7cb-909c-40e7-81d6-0e4d5d459cca', self_identification_as_gender_truth).
narrative_ontology:cs_drift_state('0213b7cb-909c-40e7-81d6-0e4d5d459cca', contemporary_contestation, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0213b7cb-909c-40e7-81d6-0e4d5d459cca', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_based_protections).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women_in_female_only_spaces).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal and social recognition of their self-identified gender, allowing access to spaces and categories aligning with their identity. Exit options are limited by the deeply personal nature of gender identity and the social/legal consequences of non-recognition.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_based_protections, beneficiary,
    moderate, biographical, identity_locked, global).

% Experience a redefinition of 'female-only' spaces, which they may perceive as eroding privacy, safety, or the specific purpose of those spaces. Their concerns are often dismissed or framed as discriminatory, limiting their ability to advocate for sex-segregated spaces.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women_in_female_only_spaces, payer,
    moderate, biographical, constrained, local).

% Advocate for the recognition of sex-based rights and categories, arguing that gender identity cannot fully replace biological sex in all contexts. They face significant social and professional pressure, often being labeled as transphobic, which constrains their ability to organize and express their views.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_feminists, payer,
    organized, generational, constrained, national).

% Are tasked with drafting and implementing laws and policies that define gender and sex categories. They navigate competing demands from various advocacy groups and legal precedents, often under pressure to align with contemporary human rights frameworks.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_and_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Support the gender identity reading as an advancement of human rights and non-discrimination. They benefit from the expansion of identity-based protections and work to ensure legal frameworks reflect this understanding.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent framework for legal and social recognition of an individual's self-identified gender, aiming to reduce discrimination and ensure dignity for transgender individuals.
% TRANSFER_FUNCTION: Transfers social and legal recognition, access to gender-specific spaces, and affirmation of identity to transgender individuals, while potentially transferring perceived loss of sex-based protections or definitional clarity to cisgender women and gender-critical advocates.
% ABSENT_VOICES: Children and vulnerable adults who may be impacted by changes to sex-segregated spaces, but whose voices are often mediated through adult advocates, are largely absent from direct policy discussions.
% DISAPPEARANCE_RATIONALE: If the principle of gender identity as the sole basis for category membership vanished overnight, legal and social frameworks would revert to sex-based definitions, leading to significant disruption for transgender individuals who would lose current protections and recognition. Advocacy efforts would re-focus on establishing new frameworks for identity recognition.
% FOUNDING_PROBLEM: The historical and ongoing discrimination, misgendering, and exclusion of transgender individuals from social and legal categories that align with their internal sense of self.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations, human rights bodies, and numerous personal testimonies from transgender individuals consistently corroborate the ongoing nature of discrimination and the need for identity-based recognition. This is attested from outside the immediate beneficiary group by international human rights law and medical professional bodies.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is high due to the perceived dignity and recognition harms experienced by those who believe sex-based categories are being eroded, and the social/legal costs imposed on those who dissent. Suppression (0.70) is also high, reflecting the active social and institutional pressure to conform to the gender identity framework, often through accusations of transphobia or discrimination, which limits open debate and the ability to advocate for alternative framings. The constraint is claimed as a 'tangled_rope' because it genuinely coordinates identity recognition for one group (transgender individuals) while simultaneously extracting costs from another (cisgender women and gender-critical feminists) through the same structure, requiring active enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and human rights advocates, this constraint is a 'rope' or 'scaffold' that provides essential coordination for identity recognition and reduces discrimination. From the perspective of cisgender women and gender-critical feminists, it operates as a 'snare' or 'tangled_rope' that extracts sex-based protections and imposes social costs for dissent. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals are clear beneficiaries (d near 0.0) as the constraint directly affirms their identity and grants access. Cisgender women and gender-critical feminists are targets (d near 1.0) as they bear the costs of category redefinition and suppression of their views. Legal and policy makers act as agenda-setters, mediating and enforcing the evolving definitions. Human rights advocates are beneficiaries, aligning with the expansion of identity-based protections.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to ensure dignity and prevent discrimination for transgender individuals. However, the high extractiveness and suppression indicate that the mechanism for achieving this mandate (self-identification as the sole criterion) is generating significant costs and resistance, suggesting a potential for mandatrophy where the means (unqualified self-ID) may be overshadowing the original end (dignity and non-discrimination for transgender people). The 'tangled_rope' classification prevents mislabeling it as pure coordination by highlighting the asymmetric extraction and active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_self_identification,
    'To what extent should self-identification be the sole determinant of category membership across all contexts (e.g., sports, medical, legal, social)?',
    'Empirical studies on fairness and safety in specific contexts (e.g., sports performance, medical data accuracy), and legal precedents from jurisdictions adopting contextual approaches.',
    'If self-identification is found to be problematic in certain contexts, the constraint''s scope would narrow, potentially leading to a ''hybrid_contextual_reading'' classification. If it holds across all contexts, the current reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_self_identification, empirical, 'Ambiguity regarding the universal applicability of self-identification for category membership.').

omega_variable(
    dignity_vs_sex_based_rights,
    'Is there an irreducible conflict between the dignity and recognition of transgender individuals and the sex-based rights of cisgender women, or can these be reconciled within a single framework?',
    'Conceptual analysis and legal scholarship exploring frameworks that simultaneously uphold both identity-based recognition and sex-based protections, or empirical evidence of successful policy implementations that achieve both.',
    'If an irreducible conflict exists, the constraint will remain highly extractive for one group, regardless of benefits to the other. If reconciliation is possible, the extractiveness could be reduced, potentially shifting the classification towards a ''rope'' or ''scaffold'' for all parties.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_vs_sex_based_rights, conceptual, 'Whether the core normative claims of identity-based recognition and sex-based rights are fundamentally incompatible.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gender-critical views structural (e.g., legal restrictions, institutional policies) or internalized (e.g., self-censorship due to social pressure)?',
    'Analysis of legal challenges to free speech, institutional policies on academic freedom, and surveys on self-censorship among academics and activists. If suppression persists after formal barriers are removed, it suggests internalized mechanisms.',
    'If primarily structural, removing legal/institutional barriers could significantly reduce suppression. If primarily internalized, the effective suppression is higher than structural measures suggest, and requires different interventions (e.g., fostering open dialogue, protecting dissenting voices).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t2000, woman_female_category__gender_identity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(woma_tr_t2005, woman_female_category__gender_identity_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(woma_tr_t2010, woman_female_category__gender_identity_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(woma_tr_t2015, woman_female_category__gender_identity_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement(woma_tr_t2020, woman_female_category__gender_identity_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(woma_tr_t2024, woman_female_category__gender_identity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t2000, woman_female_category__gender_identity_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(woma_be_t2005, woman_female_category__gender_identity_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(woma_be_t2010, woman_female_category__gender_identity_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(woma_be_t2015, woman_female_category__gender_identity_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(woma_be_t2020, woman_female_category__gender_identity_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(woma_be_t2024, woman_female_category__gender_identity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t2000, woman_female_category__gender_identity_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(woma_su_t2005, woman_female_category__gender_identity_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(woma_su_t2010, woman_female_category__gender_identity_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(woma_su_t2015, woman_female_category__gender_identity_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(woma_su_t2020, woman_female_category__gender_identity_reading, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement(woma_su_t2024, woman_female_category__gender_identity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'woman_female_category' kernel. Its sibling readings are 'sex_biology_reading' and 'hybrid_contextual_reading', which offer alternative definitions of category membership. This reading's high extractiveness and suppression are distinct from the other readings, which have different beneficiary/victim structures and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
