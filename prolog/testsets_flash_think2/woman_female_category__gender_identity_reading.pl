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
 *   human_readable: Gender Identity as Determinant for Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines membership in the 'woman' or 'female' category
 *   based on an individual's internal self-identification with that gender,
 *   independent of their biological sex. It is one reading of the broader
 *   'woman_female_category' kernel, which is highly contested. This reading
 *   aims to ensure the inclusion and recognition of transgender individuals,
 *   particularly trans women, within gendered categories and spaces. However,
 *   it generates significant 'dignity/recognition harms' for cisgender women
 *   who perceive an erosion of sex-based boundaries and protections, leading
 *   to high extractiveness and active resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.75).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.7).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Identity as Determinant for Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, '8a255028-fa1c-46ea-ac6d-e75ff021dc1f').
narrative_ontology:cs_kernel_codification('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', distributed).
narrative_ontology:cs_authority_grounding('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', expertise).
narrative_ontology:cs_interpretation_layer_present('8a255028-fa1c-46ea-ac6d-e75ff021dc1f').
narrative_ontology:cs_reading_relation('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', foundational, gender_identity_is_internal_and_self_determined).
narrative_ontology:cs_axiom_status(gender_identity_is_internal_and_self_determined, holdable).
narrative_ontology:cs_axiom_grounding('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', gender_identity_is_internal_and_self_determined, deontological).
narrative_ontology:cs_axiom('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', foundational, gender_identity_trumps_biological_sex_for_category).
narrative_ontology:cs_axiom_status(gender_identity_trumps_biological_sex_for_category, holdable).
narrative_ontology:cs_axiom_grounding('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', gender_identity_trumps_biological_sex_for_category, conventional).
narrative_ontology:cs_reference_frame('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', identity_based_recognition_framework).
narrative_ontology:cs_drift_state('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', contemporary_gender_wars, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a255028-fa1c-46ea-ac6d-e75ff021dc1f', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, cisgender_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from social and legal recognition of their self-identified gender, gaining access to spaces and categories aligned with their identity. Their identity is deeply personal and not easily changed, making exit from this framework difficult if it were to shift.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).

% Experience perceived dignity and recognition harms as sex-based categories and spaces are redefined by gender identity. They bear the cost of losing sex-specific protections or definitions, with limited options to opt out of the broader social and legal framework.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, cisgender_women, payer,
    organized, generational, constrained, global).

% Often advocate for and legitimize the gender identity framework through research, teaching, and public discourse. They benefit from the intellectual and institutional prominence of this perspective.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_studies_academics, agenda_setter,
    institutional, generational, mobile, global).

% Actively campaign for the legal and social adoption of gender identity as the primary determinant for gender categories, providing legal and social support to transgender individuals. They are key enforcers of this constraint.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, lgbtq_advocacy_groups, agenda_setter,
    organized, biographical, constrained, national).

% Actively resist the redefinition of 'woman' based on gender identity, advocating for sex-based definitions and protections. Their voices are often marginalized or actively suppressed in mainstream discourse, making them excluded from the agenda-setting process.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, feminist_gender_critical_groups, excluded,
    organized, generational, constrained, global).

% Are tasked with translating social norms into legal frameworks, often navigating intense public debate. They implement policies that either affirm or challenge the gender identity framework, facing political costs for either choice.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, policymakers, agenda_setter,
    institutional, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social recognition and inclusion for individuals whose gender identity differs from their sex assigned at birth, by establishing self-identification as the primary determinant for gender category membership.
% TRANSFER_FUNCTION: Transfers social recognition, access to gendered spaces, and legal protections based on self-identified gender, from a framework primarily based on biological sex to one primarily based on internal identity. This transfer is from cisgender women (who lose sex-based boundaries) to transgender individuals (who gain identity-based recognition).
% ABSENT_VOICES: Feminist gender-critical groups and individuals who advocate for sex-based rights and definitions are often excluded from policy-making and mainstream media discussions, despite their organized resistance. They would argue for the primacy of biological sex in defining 'woman' and for sex-segregated spaces.
% DISAPPEARANCE_RATIONALE: If self-identification ceased to be the determinant for gender category membership overnight, legal and social frameworks would revert to sex-based definitions. This would significantly alter the rights, recognition, and social standing of transgender individuals, particularly trans women, and would fundamentally reorganize the structure and purpose of gendered spaces and categories.
% FOUNDING_PROBLEM: The historical and ongoing exclusion, misgendering, and lack of recognition for transgender individuals within social and legal categories primarily defined by biological sex, leading to significant dignity and human rights harms.
% FOUNDING_PROBLEM_CORROBORATION: Transgender individuals and LGBTQ+ advocacy groups consistently attest to the ongoing problem of misgendering and exclusion. While the proposed solution (gender identity as determinant) is contested, the existence of the problem of transgender exclusion and lack of recognition is widely acknowledged by human rights organizations and many social scientists outside the direct beneficiary groups.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (identity recognition and inclusion for transgender individuals) but simultaneously involves asymmetric extraction. The 'dignity/recognition harms' experienced by cisgender women, who feel their sex-based category is being undermined, constitute this extraction. The high suppression (0.70) reflects the active marginalization or silencing of dissenting views, particularly from gender-critical perspectives. Resistance is very high (0.80) due to the ongoing and intense public debate. Theater ratio is low (0.10) as the constraint is a subject of active social and legal contestation, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transgender individuals and their advocates, this constraint is a necessary recognition of human dignity and identity, a form of coordination that rectifies historical exclusion. From the perspective of many cisgender women, particularly gender-critical feminists, the same constraint is an extractive force that erodes sex-based rights and protections, leading to significant harms. The engine's computation of per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender individuals are the primary beneficiaries, gaining recognition and access to categories aligned with their identity (low directionality). Cisgender women are the primary targets/payers, experiencing the 'dignity/recognition harms' and perceived loss of sex-based boundaries (high directionality). Gender studies academics and LGBTQ+ advocacy groups act as agenda-setters and beneficiaries, shaping and enforcing the constraint. Feminist gender-critical groups are excluded, actively resisting the constraint but often marginalized from the discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_dignity_harms,
    'Are the ''dignity/recognition harms'' experienced by cisgender women a direct, unavoidable consequence of the gender identity framework, or are they a result of specific implementations or discursive practices?',
    'Empirical studies on the lived experiences of cisgender women in jurisdictions with strong gender identity protections, distinguishing between theoretical and practical harms, and identifying mitigating factors.',
    'If harms are unavoidable, the extractiveness of this reading is intrinsic. If harms are implementation-dependent, the extractiveness could be reduced by policy adjustments without abandoning the core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_dignity_harms, empirical, 'Distinguishing intrinsic vs. contingent dignity/recognition harms.').

omega_variable(
    contestability_of_self_identification,
    'Is internal self-identification a sufficiently robust and universally accepted basis for defining social and legal categories, or does its subjective nature render it inherently contestable for public policy?',
    'Cross-cultural comparative legal analysis and philosophical inquiry into the foundations of identity and category formation in diverse societies.',
    'If inherently contestable, the constraint''s stability and legitimacy are perpetually fragile, increasing its suppression requirement to maintain. If robust, its coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contestability_of_self_identification, conceptual, 'The philosophical and social robustness of self-identification as a categorical determinant.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gender-critical voices structural (e.g., institutional policies, funding biases) or internalized (e.g., self-censorship due to social pressure)?',
    'Content analysis of institutional policies, funding flows, and public discourse, alongside qualitative studies of individuals'' experiences of expressing dissenting views.',
    'If primarily structural, removing institutional barriers could reduce suppression. If primarily internalized, the constraint''s effective suppression is higher than structural measures suggest, as individuals carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting views.').


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
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.75).

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

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'woman_female_category' kernel, alongside 'sex_biology_reading' and 'hybrid_contextual_reading'. Each reading represents a distinct structural claim about category membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
