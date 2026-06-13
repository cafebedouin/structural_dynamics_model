% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 Authority (Codified Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of the Hindu Marriage Act 1955,
 *   as interpreted by civil courts, over marriage and family law for the
 *   Hindu community in India. It is one reading of the broader
 *   'marriage_authority_kernel' which encompasses various religious and
 *   secular legal frameworks. This reading establishes uniform rules within
 *   the Hindu community, with state courts as adjudicators, and aims for
 *   moderate gender equity, often seen as an improvement over traditional
 *   customary laws but less comprehensive than a fully secular code. The
 *   claimed type is 'tangled_rope' because it genuinely coordinates legal
 *   certainty for the Hindu community while also extracting from those
 *   seeking more progressive or interfaith options.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 Authority (Codified Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'd162d31a-5d96-4d04-9e2e-589c7d9453f7').
narrative_ontology:cs_kernel_codification('d162d31a-5d96-4d04-9e2e-589c7d9453f7', formalized).
narrative_ontology:cs_authority_grounding('d162d31a-5d96-4d04-9e2e-589c7d9453f7', lineage).
narrative_ontology:cs_interpretation_layer_present('d162d31a-5d96-4d04-9e2e-589c7d9453f7').
narrative_ontology:cs_reading_relation('d162d31a-5d96-4d04-9e2e-589c7d9453f7', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d162d31a-5d96-4d04-9e2e-589c7d9453f7', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d162d31a-5d96-4d04-9e2e-589c7d9453f7', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('d162d31a-5d96-4d04-9e2e-589c7d9453f7', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('d162d31a-5d96-4d04-9e2e-589c7d9453f7', foundational, hindu_personal_law_state_enforceable).
narrative_ontology:cs_axiom_status(hindu_personal_law_state_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('d162d31a-5d96-4d04-9e2e-589c7d9453f7', hindu_personal_law_state_enforceable, conventional).
narrative_ontology:cs_axiom('d162d31a-5d96-4d04-9e2e-589c7d9453f7', foundational, gender_equity_within_religious_framework).
narrative_ontology:cs_axiom_status(gender_equity_within_religious_framework, holdable).
narrative_ontology:cs_axiom_grounding('d162d31a-5d96-4d04-9e2e-589c7d9453f7', gender_equity_within_religious_framework, deontological).
narrative_ontology:cs_reference_frame('d162d31a-5d96-4d04-9e2e-589c7d9453f7', post_independence_hindu_law_reform).
narrative_ontology:cs_drift_state('d162d31a-5d96-4d04-9e2e-589c7d9453f7', contemporary_uniform_civil_code_debate, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('d162d31a-5d96-4d04-9e2e-589c7d9453f7', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_courts).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_equal_rights).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the balance between reform and tradition; while it improved women's rights, it still falls short of full equality in some areas, creating costs for those pushing for further change. Suppression (0.6) is significant because individuals within the Hindu community are largely bound by this law, with limited options for opting out without social or legal penalties, especially for interfaith marriages. Theater ratio (0.1) is low, as the Act's provisions are genuinely enforced and serve their stated purpose, with minimal performative maintenance. Accessibility collapse (0.7) is high because for Hindu individuals, this is the primary and often only legally recognized path for marriage, making alternatives difficult to access or socially costly. Resistance (0.3) is moderate, coming from women's rights groups and secular reformers who seek further amendments or a uniform civil code.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hindu community members, the HMA 1955 is a beneficial coordination mechanism that provides legal clarity and preserves cultural identity. From the perspective of Hindu women seeking full equality or interfaith couples, it functions as an extractive and suppressive mechanism that limits their choices and rights. Civil courts, as agenda-setters, balance these perspectives in their interpretations, but their primary role is to enforce the existing codified law.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu community members are beneficiaries due to legal certainty and cultural preservation. Civil courts are agenda-setters, enforcing the law and benefiting from their institutional role. Hindu women seeking equal rights are payers, bearing the costs of incomplete equity. Interfaith couples are victims, as the Act does not accommodate their unions and pushes them to a less socially accepted secular alternative. Secular legal reformers are observers, analyzing and advocating for change.
 *
 * MANDATROPHY ANALYSIS:
 *   The HMA 1955's mandate to reform and codify Hindu personal law is still live, but its status is contested regarding the extent of gender equity achieved. The classification as a 'tangled_rope' prevents mislabeling it as a 'rope' (ignoring the extraction from certain groups) or a 'snare' (ignoring its genuine coordination function for the majority of the community). The ongoing contestation over its 'founding_problem_status' (live vs. dead) is central to understanding its current operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_gap,
    'To what extent does the Hindu Marriage Act 1955, as currently interpreted, fall short of full gender equity compared to a hypothetical uniform civil code?',
    'Comparative legal analysis of HMA provisions against a model uniform civil code, coupled with empirical studies on women''s legal outcomes under the Act.',
    'If the gap is substantial, the extractiveness metric for Hindu women would be higher, potentially shifting the constraint closer to a Snare for that seat. If negligible, the coordination aspect would be emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_gap, empirical, 'The actual degree of gender inequality embedded in the HMA 1955.').

omega_variable(
    religious_identity_vs_individual_rights,
    'Is the preservation of religion-specific personal laws (like the HMA 1955) a legitimate form of identity coordination, or does it primarily serve to suppress individual rights in the name of communal identity?',
    'Conceptual analysis of legal philosophy regarding group rights vs. individual rights, and empirical study of how individuals experience these laws in practice.',
    'If primarily suppressive, the suppression metric would be re-evaluated upwards, and the constraint''s classification for individuals might shift towards Snare. If a genuine balance, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_vs_individual_rights, conceptual, 'The balance between communal identity preservation and individual rights within the HMA.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''hindu_codified_reading'' of the marriage_authority_kernel, or is it better understood as a ''state_enforced_religious_law'' that primarily serves state interests in managing religious communities?',
    'Analysis of legislative intent, judicial interpretations, and the political economy of religious personal laws in India. If state interests consistently override community autonomy or individual rights, the latter framing is stronger.',
    'If reclassified as ''state_enforced_religious_law'', the ''authority_grounding'' in cs_structure might shift from ''lineage'' to ''extraction'', and the ''claimed_type'' might lean more towards ''snare'' due to the state''s instrumental use of religious law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in whether the HMA primarily serves Hindu community identity or state control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.05).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.3).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2010, 0.59).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel'. Its structural properties and metrics are distinct from other readings (e.g., Muslim Shariat, Christian Canonical, Secular Civil) due to differing legal sources, enforcement mechanisms, and equity outcomes. All readings are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
