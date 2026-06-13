% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__creditor_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__creditor_coordination_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint models structural adjustment conditionalities from the
 *   perspective of international creditors and financial institutions,
 *   viewing them as necessary coordination mechanisms to ensure fiscal
 *   sustainability and maintain market confidence in debtor nations. It is
 *   one reading of the 'structural_adjustment_conditionalities' kernel,
 *   focusing on the coordination problem solved and the benefits to the
 *   international financial system and future taxpayers, with 'inefficient
 *   state sectors' as the primary victims of necessary reforms. The claimed
 *   type is 'rope' because, from this perspective, the benefits of
 *   coordination outweigh the extraction, which is seen as a necessary cost
 *   of adjustment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.4).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '05670b19-3801-4d04-98de-9f1c1623cbeb').
narrative_ontology:cs_kernel_codification('05670b19-3801-4d04-98de-9f1c1623cbeb', formalized).
narrative_ontology:cs_authority_grounding('05670b19-3801-4d04-98de-9f1c1623cbeb', expertise).
narrative_ontology:cs_interpretation_layer_present('05670b19-3801-4d04-98de-9f1c1623cbeb').
narrative_ontology:cs_reading_relation('05670b19-3801-4d04-98de-9f1c1623cbeb', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('05670b19-3801-4d04-98de-9f1c1623cbeb', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('05670b19-3801-4d04-98de-9f1c1623cbeb', foundational, fiscal_discipline_is_foundational_for_growth).
narrative_ontology:cs_axiom_status(fiscal_discipline_is_foundational_for_growth, holdable).
narrative_ontology:cs_axiom_grounding('05670b19-3801-4d04-98de-9f1c1623cbeb', fiscal_discipline_is_foundational_for_growth, empirically_contingent).
narrative_ontology:cs_axiom('05670b19-3801-4d04-98de-9f1c1623cbeb', foundational, market_confidence_is_a_prerequisite_for_investment).
narrative_ontology:cs_axiom_status(market_confidence_is_a_prerequisite_for_investment, holdable).
narrative_ontology:cs_axiom_grounding('05670b19-3801-4d04-98de-9f1c1623cbeb', market_confidence_is_a_prerequisite_for_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('05670b19-3801-4d04-98de-9f1c1623cbeb', post_bretton_woods_consensus).
narrative_ontology:cs_drift_state('05670b19-3801-4d04-98de-9f1c1623cbeb', contemporary_global_south_critiques, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('05670b19-3801-4d04-98de-9f1c1623cbeb', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, private_creditors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_debtor_nations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).
:- end_tests(structural_adjustment_conditionalities__creditor_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is seen as coordination and risk reduction, with extraction being a byproduct of necessary reforms rather than an end in itself. Suppression is moderate (0.4) as debtor nations have limited alternatives to international financing, but the enforcement is primarily policy-based rather than overtly coercive. Theater ratio is low (0.1) as the conditionalities are genuinely intended to achieve fiscal stability and market confidence, with minimal performative elements from this perspective. The metrics reflect the view that conditionalities are a functional, albeit sometimes difficult, tool for global financial stability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of debtor nations and affected populations (as in the 'debtor_extraction_reading' sibling), the same conditionalities would compute as highly extractive and suppressive. This reading emphasizes the systemic benefits and the 'tough love' aspect of necessary reforms, leading to a 'rope' classification, while other readings would highlight the asymmetric power dynamics and social costs, leading to 'snare' or 'tangled_rope' classifications. The divergence is rooted in whether the 'adjustment' is seen as a shared burden for collective good or an imposed cost for creditor benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and private creditors are beneficiaries (d near 0.0) as they gain from reduced risk and a stable lending environment. Debtor nation governments are payers (d near 0.7) as they bear the political and social costs of implementing reforms. Inefficient state sectors are victims (d near 1.0) as they are directly dismantled. Future taxpayers are beneficiaries (d near 0.1) as they are expected to benefit from long-term stability. International capital markets are beneficiaries (d near 0.0) due to systemic risk reduction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_adjustment,
    'What is the true social and economic cost of implementing conditionalities in debtor nations, and how does it compare to the benefits of financial stability?',
    'Comprehensive, independent ex-post evaluations of structural adjustment programs, including social impact assessments and counterfactual analyses of alternative development paths.',
    'If the social costs significantly outweigh the long-term benefits, it would challenge the ''rope'' classification and push towards a ''tangled_rope'' or ''snare'' by revealing higher effective extraction and victimhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_adjustment, empirical, 'Assessing the net impact of conditionalities on debtor nations beyond fiscal metrics.').

omega_variable(
    alternative_coordination_mechanisms,
    'Are there alternative, less extractive mechanisms for achieving international financial coordination and fiscal sustainability that would not require such extensive conditionalities?',
    'Comparative analysis of different international financial architectures and development aid models, including those emphasizing debt relief, grants, or non-conditional budget support.',
    'The existence of viable, less extractive alternatives would weaken the ''rope'' classification by demonstrating that the current level of extraction and suppression is not structurally necessary for coordination, potentially reclassifying it as a ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, conceptual, 'Exploring the necessity of conditionalities for international financial coordination.').

omega_variable(
    naturalness_of_market_confidence,
    'To what extent is ''market confidence'' a natural, objective phenomenon, and to what extent is it a constructed social fact influenced by creditor preferences and power dynamics?',
    'Sociological and political economy studies analyzing the formation and manipulation of ''market sentiment'' and ''investor confidence'' in the context of international finance.',
    'If market confidence is largely a social construct reflecting creditor power, the ''naturalness'' of conditionalities as a response to ''market needs'' would be undermined, increasing the perceived extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_market_confidence, conceptual, 'Examining the constructed nature of ''market confidence'' as a driver for conditionalities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'creditor_coordination_reading' of the 'structural_adjustment_conditionalities' kernel. It focuses on the coordination function and benefits to the international financial system. Sibling readings ('debtor_extraction_reading', 'hybrid_selectivity_reading') offer alternative perspectives on the same underlying policy instrument, emphasizing extraction or selective application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
