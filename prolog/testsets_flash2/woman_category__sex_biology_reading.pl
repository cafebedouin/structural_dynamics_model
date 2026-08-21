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
 *   This constraint represents the 'sex-biology' reading of the category
 *   'woman', defining it primarily by chromosomal, anatomical, and
 *   reproductive biology. It is one reading of a contested kernel,
 *   'woman_category', which also includes 'gender_identity_reading' and
 *   'intersex_accommodation_reading'. This reading's structural delta is that
 *   it defines the victim set as transgender women and some intersex
 *   individuals, while benefiting organizations and advocates who prioritize
 *   sex-segregated spaces and data. The metrics reflect the active
 *   enforcement and resistance this definition generates.
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
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'b05522f4-bb18-40da-b679-6c7b96ded7ac').
narrative_ontology:cs_kernel_codification('b05522f4-bb18-40da-b679-6c7b96ded7ac', formalized).
narrative_ontology:cs_authority_grounding('b05522f4-bb18-40da-b679-6c7b96ded7ac', practice).
narrative_ontology:cs_interpretation_layer_present('b05522f4-bb18-40da-b679-6c7b96ded7ac').
narrative_ontology:cs_reading_relation('b05522f4-bb18-40da-b679-6c7b96ded7ac', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b05522f4-bb18-40da-b679-6c7b96ded7ac', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('b05522f4-bb18-40da-b679-6c7b96ded7ac', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('b05522f4-bb18-40da-b679-6c7b96ded7ac', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('b05522f4-bb18-40da-b679-6c7b96ded7ac', foundational, sex_based_rights_are_necessary).
narrative_ontology:cs_axiom_status(sex_based_rights_are_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b05522f4-bb18-40da-b679-6c7b96ded7ac', sex_based_rights_are_necessary, deontological).
narrative_ontology:cs_reference_frame('b05522f4-bb18-40da-b679-6c7b96ded7ac', biological_sex_as_foundational_category).
narrative_ontology:cs_drift_state('b05522f4-bb18-40da-b679-6c7b96ded7ac', contemporary_gender_identity_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b05522f4-bb18-40da-b679-6c7b96ded7ac', '2024-07-30T12:00:00Z').
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

% Provide services exclusively to biological women, citing safety and privacy concerns. They advocate for policies that define 'woman' by sex at birth, often facing funding cuts or legal action for non-inclusion of transgender women.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, women_s_shelters_and_services, agenda_setter,
    organized, biographical, constrained, local).

% Actively promote and defend the sex-based definition of 'woman', arguing for the importance of sex-based rights and protections. They benefit from the constraint's persistence as it aligns with their ideological framework and policy goals.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_critical_advocates, beneficiary,
    moderate, biographical, mobile, global).

% Are excluded from spaces, services, and categories designated for 'women' under this reading, experiencing discrimination and denial of their gender identity. Their identity is locked, making 'exit' from their self-identification impossible, and their options are to challenge the constraint or live with exclusion.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, immediate, identity_locked, global).

% May be ambiguously included or excluded depending on the specific biological criteria applied, often facing invasive scrutiny or being forced into categories that do not fully represent their lived experience. They bear the cost of rigid binary definitions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_individuals_with_atypical_sex_characteristics, payer,
    powerless, biographical, constrained, global).

% Analyze the legal and social implications of sex-based definitions, examining their historical context, impact on various groups, and potential for both protection and exclusion. They contribute to the intellectual discourse surrounding the constraint.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, feminist_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social, legal, and sporting categories by a clear, empirically verifiable biological criterion, providing a stable basis for sex-segregated spaces, data collection, and protective measures for biological females.
% TRANSFER_FUNCTION: Transfers access, recognition, and protection within 'woman' categories to individuals meeting biological criteria, while denying these to those who do not, particularly transgender women.
% ABSENT_VOICES: Transgender rights organizations and advocates for intersex inclusion are actively campaigning against this reading, but their perspectives are often dismissed or marginalized within the discourse that upholds this constraint, particularly in policy-making bodies influenced by gender-critical groups.
% DISAPPEARANCE_RATIONALE: If the sex-biology reading of 'woman' disappeared overnight, categories like women's sports, single-sex spaces, and sex-disaggregated data would lose their foundational definition, leading to a rapid reorganization of policies and practices around gender identity or more complex biological spectra. This would fundamentally alter the landscape of sex-based rights and protections.
% FOUNDING_PROBLEM: To establish clear, objective criteria for defining 'woman' for legal, social, and scientific purposes, particularly to ensure protections and opportunities for biological females based on their distinct needs and experiences.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for sex-based rights and many women's organizations attest that the problem of defining 'woman' for protective purposes remains live, citing ongoing concerns about fairness in sports, safety in single-sex spaces, and accurate data collection. This is corroborated by legislative debates and policy proposals in various jurisdictions seeking to codify sex-based definitions.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is substantial because this reading imposes significant costs on transgender women and some intersex individuals by denying them access to 'woman' categories. Suppression (0.70) is high due to active legal and social enforcement mechanisms that exclude those not meeting the biological criteria. Theater ratio is low (0.10) as the constraint is actively and genuinely enforced, not merely performative. Accessibility collapse is moderate (0.40) as alternatives (like self-identification) exist but are actively suppressed, and resistance is high (0.80) due to strong opposition from affected groups and their allies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sex-segregated sports organizations and women's services, this constraint is a necessary coordination mechanism to protect biological females. From the perspective of transgender women, it is a snare that extracts their identity and access. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizations and advocates who uphold the sex-biology definition are beneficiaries (low d) as it aligns with their goals and provides a clear framework for their operations. Transgender women are primary targets (high d) as they are directly excluded and bear significant costs. Intersex individuals with atypical sex characteristics are also targets, experiencing ambiguity and potential exclusion. The constraint subsidizes the beneficiaries by providing a clear, enforceable boundary for their activities, while extracting from targets by denying them access and recognition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting biological females) is still considered live by its proponents, preventing a clear mandatrophy resolution. However, the high extractiveness and suppression, coupled with significant resistance, suggest that while the mandate persists, its implementation is highly contested and imposes substantial costs on specific groups, indicating a 'tangled rope' rather than a 'rope' or 'mountain'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_determinism_vs_social_construction,
    'Is the category ''woman'' fundamentally determined by immutable biological facts, or is it a socially constructed category with evolving definitions?',
    'Philosophical and sociological consensus on the nature of gender and sex, or a legal precedent that definitively establishes one framework over the other.',
    'If biologically determined, the constraint''s ''naturalness'' (emerges_naturally) would be higher, potentially shifting it towards a mountain or rope. If socially constructed, its constructed nature and potential for extraction would be more evident, reinforcing its ''tangled rope'' or ''snare'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determinism_vs_social_construction, conceptual, 'Ambiguity regarding the foundational nature of the category ''woman''.').

omega_variable(
    intersex_inclusion_ambiguity,
    'How does this sex-biology reading consistently accommodate the diversity of intersex conditions, particularly those that do not fit a strict binary definition?',
    'Development of clear, consistent, and non-invasive criteria within the sex-biology framework for intersex inclusion, or a shift towards a more spectrum-based biological understanding.',
    'Inconsistent or exclusionary accommodation of intersex individuals would increase the constraint''s extractiveness and suppression for this group, potentially pushing it closer to a ''snare'' for them. Clear, inclusive accommodation would reduce these metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_inclusion_ambiguity, empirical, 'Uncertainty regarding the consistent application of sex-biology criteria to intersex individuals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/policy barriers) or internalized (social stigma, fear of reprisal) for transgender women?',
    'Post-exit suppression trajectory: if transgender women continue to experience exclusion or discrimination even after legal/policy barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — transgender women carry the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for transgender women.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t5, woman_category__sex_biology_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(woma_tr_t10, woman_category__sex_biology_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(woma_tr_t15, woman_category__sex_biology_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(woma_be_t5, woman_category__sex_biology_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(woma_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(woma_be_t15, woman_category__sex_biology_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(woma_su_t5, woman_category__sex_biology_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(woma_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(woma_su_t15, woman_category__sex_biology_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
