% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Reading of Woman/Female Category
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint represents a 'hybrid contextual' reading of the
 *   'woman/female' category, where biological sex is used for medical,
 *   sports, and safety contexts, while gender identity is used for social and
 *   legal recognition. It is one reading of the broader
 *   'woman_female_category' kernel. The constraint attempts to mediate
 *   between competing claims, leading to a moderate level of extraction as
 *   both transgender and cisgender women experience some form of
 *   subordination of their preferred definition in specific contexts.
 *   Institutional actors benefit from this compromise by minimizing direct
 *   conflict, but bear the overhead of managing complex, context-dependent
 *   rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.45).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.3).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Reading of Woman/Female Category").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '0718dd6d-0285-4db6-815d-f47110443f37').
narrative_ontology:cs_kernel_codification('0718dd6d-0285-4db6-815d-f47110443f37', formalized).
narrative_ontology:cs_authority_grounding('0718dd6d-0285-4db6-815d-f47110443f37', practice).
narrative_ontology:cs_interpretation_layer_present('0718dd6d-0285-4db6-815d-f47110443f37').
narrative_ontology:cs_reading_relation('0718dd6d-0285-4db6-815d-f47110443f37', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0718dd6d-0285-4db6-815d-f47110443f37', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_axiom('0718dd6d-0285-4db6-815d-f47110443f37', foundational, contextual_relevance_of_sex_and_gender).
narrative_ontology:cs_axiom_status(contextual_relevance_of_sex_and_gender, holdable).
narrative_ontology:cs_axiom_grounding('0718dd6d-0285-4db6-815d-f47110443f37', contextual_relevance_of_sex_and_gender, conventional).
narrative_ontology:cs_axiom('0718dd6d-0285-4db6-815d-f47110443f37', foundational, balancing_competing_rights_and_interests).
narrative_ontology:cs_axiom_status(balancing_competing_rights_and_interests, holdable).
narrative_ontology:cs_axiom_grounding('0718dd6d-0285-4db6-815d-f47110443f37', balancing_competing_rights_and_interests, deontological).
narrative_ontology:cs_reference_frame('0718dd6d-0285-4db6-815d-f47110443f37', evolving_social_consensus).
narrative_ontology:cs_drift_state('0718dd6d-0285-4db6-815d-f47110443f37', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0718dd6d-0285-4db6-815d-f47110443f37', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, transgender_women).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, cisgender_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, gender_identity_advocates).
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, sex_biology_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government bodies, sports federations, and medical organizations attempting to balance competing claims by applying different criteria for 'woman' or 'female' across various contexts (e.g., sex for sports, gender for legal ID). They benefit from reduced direct conflict but bear the cost of ongoing definitional ambiguity and enforcement overhead.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, institutional_actors_seeking_conflict_minimization, agenda_setter,
    institutional, generational, constrained, national).

% Recognized as women in social and legal contexts, but excluded from some sex-segregated spaces (e.g., elite sports, some medical contexts) based on biological sex. They bear the cost of inconsistent recognition and the emotional toll of exclusion in specific domains.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, transgender_women, payer,
    moderate, biographical, identity_locked, local).

% Recognized as women based on biological sex, but may feel their sex-based needs are deprioritized in social/legal contexts that prioritize gender identity. They bear the cost of perceived erosion of sex-based protections or categories in certain domains.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, cisgender_women, payer,
    moderate, biographical, constrained, local).

% Advocate for gender identity as the primary determinant of 'woman' status across all contexts. They benefit from the partial recognition of gender identity in social and legal domains under this hybrid reading, but find the sex-based exceptions to be a limitation.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_advocates, beneficiary,
    organized, generational, mobile, national).

% Advocate for biological sex as the primary determinant of 'woman' status across all contexts. They benefit from the recognition of sex in medical, sports, and safety contexts under this hybrid reading, but find the gender identity-based inclusions to be a limitation.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_biology_advocates, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate social and legal recognition of gender identity with the biological realities of sex, by applying different criteria for 'woman' or 'female' depending on the specific context (e.g., medical, sports, social).
% TRANSFER_FUNCTION: Transfers social and legal recognition (status, access) to transgender women in some contexts, while preserving sex-based categories (access, safety) for cisgender women in others. The 'cost' is borne by both groups through inconsistent application and definitional ambiguity.
% ABSENT_VOICES: Those who believe that 'woman' is an immutable, sex-based category across all contexts, and those who believe gender identity should universally define 'woman' status, are both partially excluded from full recognition of their claims within this hybrid framework. Their voices are present in public discourse but not fully accommodated by the constraint.
% DISAPPEARANCE_RATIONALE: If this hybrid contextual reading vanished, the legal and social landscape would immediately revert to either a purely sex-based or purely gender identity-based definition of 'woman,' leading to significant re-categorization, legal challenges, and social upheaval as institutions would be forced to adopt a single, consistent definition.
% FOUNDING_PROBLEM: The problem of reconciling the social and legal recognition of gender identity with the biological realities of sex, particularly in contexts where sex differences are material (e.g., sports, medicine, safety).
% FOUNDING_PROBLEM_CORROBORATION: Institutional actors and advocates from both sides attest that the tension between sex and gender identity remains a live problem, requiring ongoing attempts at reconciliation. The persistence of policy debates and legal challenges corroborates this.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).
:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because this reading attempts a compromise, but both groups (transgender and cisgender women) experience some cost due to the inconsistent application of categories. Suppression (0.3) is present as the framework actively enforces its contextual distinctions, limiting the ability of either side to impose a universal definition. Theater ratio is low (0.1) as the distinctions, while contested, are genuinely applied and enforced by institutions. The constraint is claimed as a Tangled Rope because it serves a coordination function (mediating conflict) but involves asymmetric extraction from both groups it seeks to coordinate.
 *
 * PERSPECTIVAL GAP:
 *   Institutional actors view this as a necessary and pragmatic compromise, a coordination mechanism to manage complex social realities. However, both transgender and cisgender women, as the direct subjects of these categorizations, experience it as a form of extraction, where their full recognition or protection is contingent and context-dependent. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional actors seeking conflict minimization are the primary beneficiaries, as this reading provides a framework to manage complex social and legal issues, reducing direct confrontation. Transgender women are payers in contexts where their gender identity is subordinated to biological sex. Cisgender women are payers in contexts where their sex-based categories are broadened to include gender identity. Advocates for both gender identity and sex biology are partial beneficiaries, as their claims are partially recognized, but also bear costs where their preferred universal application is denied.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contextual_coherence_ambiguity,
    'Is the contextual application of ''woman''/''female'' coherent and stable, or does it lead to unmanageable complexity and further contestation?',
    'Longitudinal study of legal challenges, policy implementation, and social acceptance in jurisdictions adopting this hybrid approach. If legal challenges and social friction increase over time, it suggests incoherence.',
    'If incoherent, the constraint''s effective extractiveness and suppression would be higher than measured, as the system would generate more friction and require more enforcement to maintain. If coherent, it would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_coherence_ambiguity, empirical, 'Whether the contextual distinctions are practically sustainable.').

omega_variable(
    beneficiary_cost_distribution,
    'Are the ''benefits'' of conflict minimization for institutional actors genuinely outweighing the ''costs'' borne by transgender and cisgender women, or is the compromise primarily serving institutional convenience?',
    'Qualitative research on the lived experiences of both transgender and cisgender women under this framework, compared to the administrative burden and public perception of institutional actors.',
    'If institutional convenience is the primary driver, the constraint''s extractiveness would be re-evaluated upward, as the coordination story would be revealed as cover for administrative ease at the expense of affected parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_cost_distribution, preference, 'Whether the compromise serves affected parties or institutional convenience.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint a genuine attempt at reconciliation, or a temporary political compromise that merely defers a more fundamental definitional conflict?',
    'Analysis of legislative and judicial trends over a longer time horizon (e.g., 20-30 years). If the contextual distinctions erode or are replaced by a universal definition, it suggests a deferred conflict.',
    'If a deferred conflict, the constraint''s stability is lower, and its long-term viability as a coordination mechanism is questionable, potentially leading to a reclassification towards a more temporary (Scaffold) or unstable (Tangled Rope) type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the hybrid approach is a stable solution or a temporary deferral of conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__hybrid_contextual_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__hybrid_contextual_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(woma_be_t5, woman_female_category__hybrid_contextual_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(woma_be_t10, woman_female_category__hybrid_contextual_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(woma_su_t5, woman_female_category__hybrid_contextual_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(woma_su_t10, woman_female_category__hybrid_contextual_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
