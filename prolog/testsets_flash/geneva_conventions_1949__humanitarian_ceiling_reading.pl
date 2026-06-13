% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions (1949): Humanitarian Ceiling Reading
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'humanitarian ceiling' reading of the 1949
 *   Geneva Conventions, which posits that the Conventions establish absolute,
 *   non-reciprocal humanitarian minimums that constrain state violence
 *   regardless of adversary compliance or security rationales. This reading
 *   emphasizes expansive civilian and detainee protections, including basic
 *   rights for irregular combatants, and places an asymmetric burden of
 *   restraint on state militaries. It stands in contrast to readings that
 *   emphasize reciprocity or prioritize state security.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.3).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions (1949): Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'b68eeaa1-b70a-459f-b16b-d0de2047046d').
narrative_ontology:cs_kernel_codification('b68eeaa1-b70a-459f-b16b-d0de2047046d', fixed_text).
narrative_ontology:cs_authority_grounding('b68eeaa1-b70a-459f-b16b-d0de2047046d', lineage).
narrative_ontology:cs_interpretation_layer_present('b68eeaa1-b70a-459f-b16b-d0de2047046d').
narrative_ontology:cs_reading_relation('b68eeaa1-b70a-459f-b16b-d0de2047046d', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('b68eeaa1-b70a-459f-b16b-d0de2047046d', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('b68eeaa1-b70a-459f-b16b-d0de2047046d', foundational, humanitarian_protections_are_absolute).
narrative_ontology:cs_axiom_status(humanitarian_protections_are_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b68eeaa1-b70a-459f-b16b-d0de2047046d', humanitarian_protections_are_absolute, deontological).
narrative_ontology:cs_axiom('b68eeaa1-b70a-459f-b16b-d0de2047046d', foundational, non_reciprocity_does_not_abrogate_duty).
narrative_ontology:cs_axiom_status(non_reciprocity_does_not_abrogate_duty, holdable).
narrative_ontology:cs_axiom_grounding('b68eeaa1-b70a-459f-b16b-d0de2047046d', non_reciprocity_does_not_abrogate_duty, deontological).
narrative_ontology:cs_reference_frame('b68eeaa1-b70a-459f-b16b-d0de2047046d', post_wwii_universal_humanitarian_consensus).
narrative_ontology:cs_drift_state('b68eeaa1-b70a-459f-b16b-d0de2047046d', contemporary_asymmetric_conflict_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b68eeaa1-b70a-459f-b16b-d0de2047046d', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, medical_personnel).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_planners).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants_seeking_total_war).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the real operational costs and constraints placed on state militaries. Suppression (0.7) is high because this reading actively suppresses arguments for 'military necessity' or 'reciprocity' as justifications for violating humanitarian norms. Theater ratio (0.2) is low, as the commitment to these principles is generally genuine, though often challenged in practice. The increasing suppression over time reflects the hardening of international legal norms against derogation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state military planners, this reading imposes significant constraints and costs, potentially hindering operational effectiveness. From the perspective of civilians and humanitarian actors, it is a vital protective framework. The international courts uphold this reading, often against state resistance, creating a tension between legal interpretation and perceived national security interests.
 *
 * DIRECTIONALITY LOGIC:
 *   State military planners are payers, bearing the costs of operational restraint. Civilians, detained combatants, and medical personnel are clear beneficiaries, receiving protection. Irregular combatants seeking total war are victims, as their preferred tactics are explicitly constrained. International courts act as agenda-setters, enforcing this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_absolute_duty,
    'To what extent is state compliance with the Geneva Conventions truly independent of adversary reciprocity, or does perceived non-reciprocity erode adherence to this humanitarian ceiling reading?',
    'Empirical study of state practice in asymmetric conflicts where one party consistently violates IHL: does the ''humanitarian ceiling'' reading persist in state doctrine and practice, or does it degrade towards a ''conditional reciprocity'' model?',
    'If reciprocity proves a strong determinant, the ''humanitarian ceiling'' reading is less robust than claimed, and its effective extractiveness from state militaries might be lower (as they find justifications for non-compliance), while the suppression of security rationales is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_absolute_duty, empirical, 'The tension between the absolute duty claimed by this reading and the practical pressures of non-reciprocal warfare.').

omega_variable(
    security_necessity_vs_humanitarian_ceiling,
    'Is the ''humanitarian ceiling'' reading genuinely capable of suppressing ''military necessity'' arguments in all contexts, or does a ''security maximization'' rationale inevitably reassert itself in high-stakes conflicts?',
    'Analysis of judicial decisions and state legal opinions during periods of perceived existential threat: are humanitarian protections consistently upheld, or are they systematically reinterpreted to permit actions previously deemed illegal?',
    'If security maximization consistently overrides the humanitarian ceiling, this reading''s suppression of security rationales is weaker than claimed, and its effective classification might shift towards a ''tangled rope'' or ''snare'' for those it purports to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_vs_humanitarian_ceiling, conceptual, 'The conceptual contest between absolute humanitarianism and perceived state security imperatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1960, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(gene_tr_t1975, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(gene_tr_t2005, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.2).
narrative_ontology:measurement(gene_be_t1960, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(gene_be_t1975, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(gene_be_t2005, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1960, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(gene_su_t1975, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(gene_su_t2005, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 1949 Geneva Conventions kernel. This 'humanitarian ceiling' reading emphasizes absolute, non-reciprocal humanitarian minimums, contrasting with 'conditional reciprocity' and 'security maximization' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
