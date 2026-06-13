% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Sovereign Legitimacy
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the 'republican reading' of sovereign
 *   legitimacy, where authority originates from the people and is delegated
 *   through consent, typically via democratic processes. It is grounded in
 *   popular sovereignty and social contract theory. This reading emphasizes
 *   accountability and the right of the governed to choose their rulers, but
 *   also acknowledges the potential for majoritarian extraction from
 *   minorities or those excluded from the franchise. The constraint is
 *   claimed as a Rope due to its genuine coordination function in
 *   establishing stable governance, but its moderate extractiveness and
 *   suppression reflect the ongoing costs and exclusions inherent in its
 *   operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.3).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, 'ae252cde-00f2-45db-8a99-f63115a3cf24').
narrative_ontology:cs_kernel_codification('ae252cde-00f2-45db-8a99-f63115a3cf24', formalized).
narrative_ontology:cs_authority_grounding('ae252cde-00f2-45db-8a99-f63115a3cf24', lineage).
narrative_ontology:cs_interpretation_layer_present('ae252cde-00f2-45db-8a99-f63115a3cf24').
narrative_ontology:cs_reading_relation('ae252cde-00f2-45db-8a99-f63115a3cf24', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('ae252cde-00f2-45db-8a99-f63115a3cf24', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ae252cde-00f2-45db-8a99-f63115a3cf24', foundational, legitimacy_from_popular_consent).
narrative_ontology:cs_axiom_status(legitimacy_from_popular_consent, holdable).
narrative_ontology:cs_axiom_grounding('ae252cde-00f2-45db-8a99-f63115a3cf24', legitimacy_from_popular_consent, deontological).
narrative_ontology:cs_axiom('ae252cde-00f2-45db-8a99-f63115a3cf24', foundational, government_by_delegated_power).
narrative_ontology:cs_axiom_status(government_by_delegated_power, holdable).
narrative_ontology:cs_axiom_grounding('ae252cde-00f2-45db-8a99-f63115a3cf24', government_by_delegated_power, conventional).
narrative_ontology:cs_reference_frame('ae252cde-00f2-45db-8a99-f63115a3cf24', enlightenment_social_contract).
narrative_ontology:cs_drift_state('ae252cde-00f2-45db-8a99-f63115a3cf24', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ae252cde-00f2-45db-8a99-f63115a3cf24', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_voting_rights).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_from_franchise).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, political_minorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while the system aims for collective benefit, it inherently extracts compliance and resources from all, and can impose costs on political minorities. Suppression is low (0.3) as the system relies on consent, but active enforcement is required to maintain order and suppress challenges to its foundational principles. Theater ratio is low (0.1) as the mechanisms of consent (elections, constitutional adherence) are generally functional, though their fairness and representativeness are subject to ongoing debate. Accessibility collapse is moderate (0.6) as alternatives to this system (e.g., monarchy, anarchy) are generally seen as less legitimate or viable within this framework, but not entirely impossible to conceive or advocate for. Resistance is low (0.2) because the system generally enjoys broad, if sometimes grudging, acceptance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry and elected representatives, this constraint is a legitimate and beneficial framework for self-governance. From the perspective of excluded groups or political minorities, it can appear as a system that legitimizes their marginalization or the suppression of their interests, even if it is not a pure snare. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry with voting rights and elected representatives are primary beneficiaries, as they are the source and wielders of legitimate power, respectively. Those excluded from the franchise and political minorities are victims, as they bear the costs of governance without full participation or representation. The directionality for beneficiaries is low (subsidized by the system's stability), while for victims it is high (targeted by its exclusionary or majoritarian aspects).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to provide legitimate, accountable governance) remains live. The classification as a Rope, rather than a Snare, prevents mislabeling genuine coordination as pure extraction, while acknowledging the real costs and exclusions. The ongoing need for active enforcement and the potential for majoritarian tyranny prevent it from being a pure Mountain, as its persistence is not entirely natural or self-sustaining without human action and vigilance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majoritarian_tyranny_risk,
    'To what extent does the republican reading''s emphasis on majority rule lead to the systematic extraction from or suppression of political minorities, effectively turning it into a Snare for those groups?',
    'Empirical analysis of legislative outcomes and judicial protections for minority rights over time, particularly in systems with weak constitutional checks on majoritarian power.',
    'If majoritarian tyranny is consistently demonstrated, the effective classification for political minorities would shift towards Snare, indicating a higher degree of extraction and suppression than the overall Rope classification suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_risk, empirical, 'Assesses the risk of majoritarian rule becoming extractive for minorities.').

omega_variable(
    consent_vs_coercion_boundary,
    'At what point does ''delegated consent'' become indistinguishable from ''coerced compliance'' for those with limited exit options or suppressed voices within the system?',
    'Comparative analysis of political systems with varying degrees of electoral freedom, civil liberties, and protections for dissent. Examination of historical periods where franchise was restricted.',
    'If the boundary is frequently crossed, the overall suppression metric for the constraint would be higher, and its classification would lean more towards Tangled Rope or Snare, as the ''consent'' narrative would be revealed as cover for coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_coercion_boundary, conceptual, 'Examines the line between genuine consent and coerced compliance in republican systems.').

omega_variable(
    republican_vs_monarchical_legitimacy,
    'Is the republican reading of sovereign legitimacy fundamentally incompatible with the monarchical reading, or can elements of both coexist within a single political framework?',
    'Analysis of constitutional monarchies and historical transitions from monarchy to republic. Conceptual analysis of the logical coherence of combining inherited and delegated authority.',
    'If fundamentally incompatible, the ''forecloses'' relation to the monarchical reading is strengthened. If coexistence is robust, it supports the ''coexists_with'' or ''influences'' relations, suggesting a more complex, hybrid political reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(republican_vs_monarchical_legitimacy, conceptual, 'Examines the compatibility of republican and monarchical legitimacy claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1789, sovereign_legitimacy__republican_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(sove_tr_t1850, sovereign_legitimacy__republican_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__republican_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(sove_tr_t1950, sovereign_legitimacy__republican_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sove_be_t1789, sovereign_legitimacy__republican_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(sove_be_t1850, sovereign_legitimacy__republican_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__republican_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(sove_be_t1950, sovereign_legitimacy__republican_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1789, sovereign_legitimacy__republican_reading, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement(sove_su_t1850, sovereign_legitimacy__republican_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__republican_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(sove_su_t1950, sovereign_legitimacy__republican_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'sovereign_legitimacy' kernel. The 'republican_reading' emphasizes popular sovereignty and delegated consent, contrasting with monarchical and hybrid constitutional models. Each reading represents a distinct structural claim about the source and flow of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
