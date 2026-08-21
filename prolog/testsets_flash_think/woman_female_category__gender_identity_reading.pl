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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Woman/Female Category: Gender Identity Reading
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines membership in the 'woman' or 'female' category
 *   based on internal self-identification with that gender, independent of
 *   biological sex. It is one reading of the contested
 *   'woman_female_category' kernel. While it aims to coordinate
 *   identity-based protections for transgender individuals, it generates
 *   significant extraction in the form of dignity and recognition harms for
 *   those who prioritize sex-based definitions, particularly women concerned
 *   about sex-segregated spaces. The constraint requires active social and
 *   legal enforcement to maintain its definition against resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.7).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.65).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Woman/Female Category: Gender Identity Reading").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '41d40a58-c99a-4c7d-be5c-a41a8270e469').
narrative_ontology:cs_kernel_codification('41d40a58-c99a-4c7d-be5c-a41a8270e469', formalized).
narrative_ontology:cs_authority_grounding('41d40a58-c99a-4c7d-be5c-a41a8270e469', practice).
narrative_ontology:cs_interpretation_layer_present('41d40a58-c99a-4c7d-be5c-a41a8270e469').
narrative_ontology:cs_reading_relation('41d40a58-c99a-4c7d-be5c-a41a8270e469', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('41d40a58-c99a-4c7d-be5c-a41a8270e469', woman_female_category__hybrid_contextual_reading, coexists_with).
narrative_ontology:cs_axiom('41d40a58-c99a-4c7d-be5c-a41a8270e469', foundational, gender_identity_is_self_determined).
narrative_ontology:cs_axiom_status(gender_identity_is_self_determined, holdable).
narrative_ontology:cs_axiom_grounding('41d40a58-c99a-4c7d-be5c-a41a8270e469', gender_identity_is_self_determined, deontological).
narrative_ontology:cs_axiom('41d40a58-c99a-4c7d-be5c-a41a8270e469', secondary, social_categories_are_fluid).
narrative_ontology:cs_axiom_status(social_categories_are_fluid, holdable).
narrative_ontology:cs_axiom_grounding('41d40a58-c99a-4c7d-be5c-a41a8270e469', social_categories_are_fluid, conventional).
narrative_ontology:cs_reference_frame('41d40a58-c99a-4c7d-be5c-a41a8270e469', gender_identity_as_primary_social_determinant).
narrative_ontology:cs_drift_state('41d40a58-c99a-4c7d-be5c-a41a8270e469', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('41d40a58-c99a-4c7d-be5c-a41a8270e469', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_protections).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, individuals_prioritizing_sex_based_categories).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, women_concerned_about_sex_segregated_spaces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_studies_academics).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, legal_advocacy_organizations).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, gender_identity_as_primary_social_category).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, self_determination_of_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from social and legal recognition of their self-identified gender, which aligns with their internal sense of self. This recognition is crucial for their dignity and access to gender-affirming spaces and services. Exit from this identity is not an option.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals_seeking_identity_protections, beneficiary,
    moderate, biographical, identity_locked, global).

% Bear the cost of having their preferred definition of 'woman' or 'female' (based on biological sex) challenged or overridden in various social and legal contexts. They may feel their concerns about sex-based rights or data are dismissed.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, individuals_prioritizing_sex_based_categories, payer,
    moderate, biographical, constrained, global).

% Experience a perceived loss of sex-segregated spaces (e.g., changing rooms, shelters, sports categories) as exclusive to biological females, leading to concerns about privacy, safety, and fair competition. Their ability to advocate for sex-based spaces is often constrained by social pressure.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, women_concerned_about_sex_segregated_spaces, payer,
    moderate, biographical, constrained, local).

% Are instrumental in shaping and disseminating the theoretical framework that grounds gender identity as primary. Their academic work and advocacy benefit from the adoption of this framework, reinforcing their professional identity and influence.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_studies_academics, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, gender_studies_academics, beneficiary).

% Actively lobby for and litigate to establish legal precedents and policies that enshrine gender identity as the basis for category membership. Their organizational mission and funding often depend on advancing these legal and social changes.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legal_advocacy_organizations, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, legal_advocacy_organizations, beneficiary).

% Are often marginalized or excluded from mainstream discourse and policy-making processes when they attempt to assert biological sex as the primary determinant of 'woman' or 'female'. Their arguments are frequently dismissed as discriminatory or transphobic.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, biological_sex_advocates, excluded,
    powerless, biographical, constrained, global).

% Are tasked with drafting and implementing laws and policies that define gender categories. They observe the ongoing social and legal debates and must navigate competing claims, often under significant public pressure.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, policy_makers, observer,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, policy_makers, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate social recognition and legal protections for individuals based on their self-identified gender, ensuring their inclusion in gender-specific categories and spaces.
% TRANSFER_FUNCTION: Transfers the authority to define 'woman' or 'female' from a biological sex-based framework to an identity-based framework, thereby transferring social and legal recognition, and access to gender-specific spaces, to transgender individuals.
% ABSENT_VOICES: Those who believe biological sex is the sole determinant of 'woman' or 'female' in all contexts, particularly in discussions about sex-segregated spaces, are often excluded or silenced in dominant public and institutional discourse.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the social and legal landscape around gender recognition would fundamentally shift. Categories like 'woman' or 'female' would revert to or be re-debated under a sex-based definition, leading to new forms of exclusion or re-negotiation of rights for transgender individuals, and a reorganization of gender-affirming policies.
% FOUNDING_PROBLEM: Historical and ongoing exclusion, discrimination, and lack of recognition for transgender individuals, leading to significant harms to their dignity, safety, and well-being.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy groups, human rights organizations, and many legal scholars attest to the ongoing problem of discrimination and the need for identity-based protections. Critics, however, argue that the scope of the problem or the proposed solution has shifted beyond its original intent, leading to new harms.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) due to the significant dignity and recognition harms experienced by individuals whose understanding of 'woman' or 'female' is based on biological sex, and who feel their concerns are dismissed. Suppression is also high (0.65) as alternative definitions are often actively marginalized or deemed unacceptable in many institutional and social contexts. The theater ratio is low (0.1) because the constraint is actively debated and enforced, not merely maintained for show. Resistance is very high (0.8) reflecting the intense public and academic contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and their advocates, this constraint is a necessary coordination mechanism for recognition and inclusion. From the perspective of those prioritizing sex-based categories, it is an extractive imposition that redefines fundamental social categories and impacts sex-based rights. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals seeking identity-based protections are the primary beneficiaries (low d) as the constraint directly affirms their identity and grants access. Individuals prioritizing sex-based categories and women concerned about sex-segregated spaces are targets (high d) as they bear the costs of redefined categories and perceived loss of sex-exclusive spaces. Gender studies academics and legal advocacy organizations act as agenda-setters and beneficiaries, actively shaping and enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_woman_female,
    'Is the definition of ''woman'' or ''female'' primarily a matter of self-identified gender or biological sex?',
    'Societal consensus shift, legal precedent from higher courts, or scientific consensus on the nature of gender and sex.',
    'If resolved towards biological sex, this constraint''s extractiveness would be inverted (benefiting sex-based definitions) and its claimed type would shift. If resolved towards identity, its coordination function would be universally recognized, reducing perceived extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_woman_female, conceptual, 'Fundamental conceptual ambiguity regarding the definition of ''woman''/''female''.').

omega_variable(
    impact_on_sex_based_rights,
    'Does the inclusion of transgender women in ''woman'' or ''female'' categories genuinely undermine sex-based rights or protections for biological women, or are these concerns unfounded?',
    'Empirical studies on the impact of inclusive policies on sex-segregated spaces, sports, and data collection, combined with legal analysis of rights frameworks.',
    'If genuine harms are demonstrated, the extractiveness of this constraint would be further amplified for biological women, potentially shifting its classification towards a Snare. If harms are disproven, the perceived extraction would decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_sex_based_rights, empirical, 'Empirical ambiguity regarding the impact of gender identity definitions on sex-based rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative views structural (institutional policies, legal frameworks) or internalized (social pressure, fear of reprisal)?',
    'Post-policy-change discourse analysis: if suppression of sex-based arguments persists after institutional barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them, making resistance harder to organize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative definitions.').


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
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(woma_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(woma_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel. Each reading represents a distinct structural claim about category membership, with different beneficiaries, victims, and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
