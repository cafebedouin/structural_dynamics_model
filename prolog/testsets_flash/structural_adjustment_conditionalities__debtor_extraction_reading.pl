% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities (Debtor Extraction Reading)
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This constraint represents the 'debtor_extraction_reading' of structural
 *   adjustment conditionalities, which views them as coercive instruments
 *   imposed by international financial institutions (IFIs) and creditor
 *   nations on indebted developing countries. The conditionalities, often
 *   requiring privatization, deregulation, and cuts to public spending, are
 *   seen as dismantling social contracts and extracting wealth for the
 *   benefit of transnational capital and creditor banks, rather than
 *   fostering genuine development or fiscal stability in the debtor nations.
 *   This reading emphasizes the neo-colonial power dynamics and the violent
 *   impact on domestic populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.92).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.95).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities (Debtor Extraction Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '507377b3-27be-4ba6-b50e-cd66fdb65351').
narrative_ontology:cs_kernel_codification('507377b3-27be-4ba6-b50e-cd66fdb65351', formalized).
narrative_ontology:cs_authority_grounding('507377b3-27be-4ba6-b50e-cd66fdb65351', extraction).
narrative_ontology:cs_interpretation_layer_present('507377b3-27be-4ba6-b50e-cd66fdb65351').
narrative_ontology:cs_reading_relation('507377b3-27be-4ba6-b50e-cd66fdb65351', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('507377b3-27be-4ba6-b50e-cd66fdb65351', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('507377b3-27be-4ba6-b50e-cd66fdb65351', foundational, debt_is_a_tool_of_control).
narrative_ontology:cs_axiom_status(debt_is_a_tool_of_control, holdable).
narrative_ontology:cs_axiom_grounding('507377b3-27be-4ba6-b50e-cd66fdb65351', debt_is_a_tool_of_control, deontological).
narrative_ontology:cs_axiom('507377b3-27be-4ba6-b50e-cd66fdb65351', foundational, austerity_harms_development).
narrative_ontology:cs_axiom_status(austerity_harms_development, holdable).
narrative_ontology:cs_axiom_grounding('507377b3-27be-4ba6-b50e-cd66fdb65351', austerity_harms_development, empirically_contingent).
narrative_ontology:cs_reference_frame('507377b3-27be-4ba6-b50e-cd66fdb65351', neo_colonial_power_structure).
narrative_ontology:cs_drift_state('507377b3-27be-4ba6-b50e-cd66fdb65351', contemporary_debt_crisis_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('507377b3-27be-4ba6-b50e-cd66fdb65351', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, international_financial_institutions).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_industries).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.92) reflects the transfer of public assets to private (often foreign) hands, the redirection of state revenue towards debt servicing, and the suppression of domestic industries. Suppression (0.95) is extremely high due to the debtor states' lack of viable alternatives to IFI loans, making compliance with conditionalities effectively mandatory despite severe social costs. The low theater ratio (0.15) indicates that the 'development' or 'stability' narrative is largely a cover for the underlying extractive function, with little genuine coordination or support provided to the debtor states. Accessibility collapse is high (0.88) because alternative financing or development paths are systematically foreclosed by the global financial architecture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of debtor state populations, this is a clear Snare, violently extracting resources and dismantling social protections. From the perspective of creditor banks and transnational capital, it is a highly beneficial mechanism for securing investments and ensuring debt repayment. IFIs, as agenda-setters, present it as a necessary Rope for global financial stability, but this reading strongly contests that framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Debtor state populations, public sector workers, and domestic industries are clear victims (high d) as they bear the direct costs of austerity and privatization. Transnational capital, creditor banks, and international financial institutions are beneficiaries (low d) as they profit from the new market opportunities, debt repayment, and enhanced financial control. The debtor state governments themselves are in a complex position, acting as agenda-setters for the conditionalities within their own borders, but as targets of the IFIs (d is high for them relative to IFIs, but lower relative to their own populations).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a coercive extraction regime as a benign coordination mechanism. The 'debtor_extraction_reading' highlights that the original mandate of 'development' or 'stability' has atrophied, replaced by a function of wealth transfer and control, making it a Snare rather than a Rope. The persistence is due to the power asymmetry and lack of alternatives for debtor states, not a genuine collective benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine Snare, or a necessary coordination mechanism for fiscal sustainability?',
    'Analysis of counterfactuals where debt restructuring occurred without conditionalities, and comparison of economic outcomes for debtor states.',
    'If the ''creditor_coordination_reading'' is correct, the constraint would be reclassified as a Rope or Tangled Rope, with lower extractiveness and higher coordination function. If the ''hybrid_selectivity_reading'' is correct, the constraint''s application would be seen as contingent on geopolitical factors, not universal economic principles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''debtor_extraction_reading'' of the ''structural_adjustment_conditionalities'' kernel, which views conditionalities as extractive neo-colonial instruments.').

omega_variable(
    causality_of_economic_distress,
    'To what extent is the economic distress in debtor nations a result of pre-existing internal governance failures versus the direct impact of conditionalities?',
    'Detailed econometric studies comparing economic trajectories of states with and without conditionalities, controlling for internal factors and external shocks.',
    'If internal factors are dominant, the extractiveness attributed to conditionalities might be overstated. If conditionalities are the primary driver, the Snare classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_of_economic_distress, empirical, 'Distinguishing the causal impact of conditionalities from other factors in debtor state economic outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 15, 0.15).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 5, 0.88).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 15, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 5, 0.92).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 10, 0.94).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 15, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, debt_sustainability_frameworks).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, global_financial_governance).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, sovereign_debt_restructuring).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'structural_adjustment_conditionalities' kernel. Its ε value is high, reflecting its extractive nature, in contrast to the 'creditor_coordination_reading' which would have a lower ε, and the 'hybrid_selectivity_reading' which would emphasize contingent application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
