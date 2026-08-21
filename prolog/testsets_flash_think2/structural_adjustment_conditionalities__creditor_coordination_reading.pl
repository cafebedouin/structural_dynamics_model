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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'creditor coordination' reading of
 *   structural adjustment conditionalities. From this perspective,
 *   conditionalities are essential mechanisms for coordinating the fiscal
 *   policies of debtor nations with the expectations of international
 *   creditors, thereby ensuring fiscal sustainability and maintaining market
 *   confidence. The constraint is framed as a Rope, solving a genuine
 *   collective action problem with relatively low extraction, where 'victims'
 *   are primarily inefficient state sectors rather than the broader populace.
 *   The metrics reflect this framing, showing moderate suppression (necessary
 *   for policy enforcement) and low extraction (seen as a legitimate cost of
 *   coordination).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.45).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '356e9ba0-4baa-4348-b572-0731966f9504').
narrative_ontology:cs_kernel_codification('356e9ba0-4baa-4348-b572-0731966f9504', formalized).
narrative_ontology:cs_authority_grounding('356e9ba0-4baa-4348-b572-0731966f9504', expertise).
narrative_ontology:cs_interpretation_layer_present('356e9ba0-4baa-4348-b572-0731966f9504').
narrative_ontology:cs_reading_relation('356e9ba0-4baa-4348-b572-0731966f9504', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('356e9ba0-4baa-4348-b572-0731966f9504', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('356e9ba0-4baa-4348-b572-0731966f9504', foundational, fiscal_discipline_ensures_macroeconomic_stability).
narrative_ontology:cs_axiom_status(fiscal_discipline_ensures_macroeconomic_stability, holdable).
narrative_ontology:cs_axiom_grounding('356e9ba0-4baa-4348-b572-0731966f9504', fiscal_discipline_ensures_macroeconomic_stability, empirically_contingent).
narrative_ontology:cs_axiom('356e9ba0-4baa-4348-b572-0731966f9504', foundational, market_confidence_is_prerequisite_for_sustainable_development).
narrative_ontology:cs_axiom_status(market_confidence_is_prerequisite_for_sustainable_development, holdable).
narrative_ontology:cs_axiom_grounding('356e9ba0-4baa-4348-b572-0731966f9504', market_confidence_is_prerequisite_for_sustainable_development, empirically_contingent).
narrative_ontology:cs_reference_frame('356e9ba0-4baa-4348-b572-0731966f9504', sound_macroeconomic_management_framework).
narrative_ontology:cs_drift_state('356e9ba0-4baa-4348-b572-0731966f9504', contemporary_development_finance_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('356e9ba0-4baa-4348-b572-0731966f9504', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_debtor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_nation_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., IMF, World Bank) design, negotiate, and enforce conditionalities. They benefit from the stability these measures bring to the international financial system and their role as central coordinators.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the increased confidence and reduced risk that conditionalities are intended to bring to debtor nations, making their investments safer and more predictable. They are not directly involved in enforcement but rely on its effects.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, biographical, mobile, global).

% Agree to conditionalities in exchange for loans or debt relief. They bear the political cost of implementing unpopular reforms and lose some policy autonomy, but gain access to crucial financing and market confidence.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_nation_governments, payer,
    moderate, immediate, constrained, national).

% These sectors (e.g., state-owned enterprises, public services) are often targeted for reform, privatization, or budget cuts under conditionalities, leading to job losses or reduced services. They are direct 'victims' of the efficiency drive.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_debtor_nations, payer,
    powerless, immediate, trapped, national).

% Are expected to benefit from the long-term fiscal sustainability, reduced debt burden, and economic growth that conditionalities are designed to achieve, though these benefits are often diffuse and delayed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_nations, beneficiary,
    powerless, generational, identity_locked, national).

% Often advocate for social safety nets, poverty reduction, and protection of vulnerable populations, which can be negatively impacted by austerity measures. Their voices are typically not central to the negotiation of conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, civil_society_organizations_debtor_nations, excluded,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align the fiscal and economic policies of debtor nations with the expectations and requirements of international creditors, ensuring fiscal sustainability, debt repayment, and continued access to global capital markets.
% TRANSFER_FUNCTION: Transfers policy influence and decision-making authority from debtor nation governments to international financial institutions, in exchange for financial assistance and the 'seal of approval' that unlocks further private investment.
% ABSENT_VOICES: Civil society organizations, labor unions, and local communities in debtor nations, who often bear the immediate social costs of structural adjustment policies and would advocate for alternative, more socially protective approaches.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished, international lending to developing nations would become significantly riskier due to moral hazard and lack of credible commitment mechanisms. This would likely lead to a sharp reduction in capital flows, increased sovereign defaults, and a more volatile and fragmented international financial system.
% FOUNDING_PROBLEM: The problem of sovereign debt crises, fiscal irresponsibility, and economic mismanagement in developing nations, which led to instability, inability to repay debts, and a breakdown of trust with international lenders.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions, credit rating agencies, and mainstream financial economists consistently corroborate the ongoing relevance of fiscal discipline and market confidence as critical issues in global development finance.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.28, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.28) and theater ratio (0.15) reflect the view that conditionalities are primarily functional and serve a legitimate coordination purpose, with costs being necessary for achieving efficiency and stability. Suppression (0.45) is moderate, acknowledging that policy changes are often mandated, but framed as a necessary enforcement of agreed-upon reforms. The slight fluctuations in metrics over time reflect evolving approaches to conditionalities, with a general trend towards lower extraction and suppression as the mechanisms matured and became more accepted, from this reading's perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor coordination perspective, the constraint is a Rope, delivering net benefits. However, other readings (e.g., debtor extraction) would classify it as a Snare or Tangled Rope, highlighting the asymmetric power dynamics and social costs. The engine's per-seat classification will reveal this divergence, with IFIs and capital markets computing as beneficiaries, while debtor governments and state sectors compute as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and capital markets are clear beneficiaries, gaining stability and reduced risk. Future taxpayers in debtor nations are also beneficiaries, as they are theoretically spared from future fiscal crises. Debtor nation governments are payers, trading policy autonomy for financial access. Inefficient state sectors are direct targets of reform, bearing the immediate costs. Civil society organizations are excluded, as their perspectives on social costs are often marginalized in the negotiation process.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading, the mandate of conditionalities (ensuring fiscal sustainability and market confidence) remains live and critical. The constraint is not seen as suffering from mandatrophy; rather, it is a continuously necessary tool in international finance. The low theater ratio supports this, indicating that the function is genuine and not merely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debtor_extraction_ambiguity,
    'Is the primary function of conditionalities genuine coordination for fiscal sustainability, or are they primarily extractive instruments benefiting creditors at the expense of debtor nations?',
    'Longitudinal studies comparing economic outcomes (growth, poverty reduction, inequality) in conditionalized vs. non-conditionalized debtor nations, controlling for initial conditions and external shocks. Analysis of capital flight and profit repatriation patterns.',
    'If primarily extractive, the constraint would reclassify towards Snare or Tangled Rope, with significantly higher extractiveness and suppression metrics, and a different beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debtor_extraction_ambiguity, empirical, 'Ambiguity between coordination and extraction as the dominant function of conditionalities.').

omega_variable(
    selectivity_of_enforcement,
    'Are conditionalities applied universally and consistently based on economic principles, or are they selectively enforced, with weaker states facing harsher terms and geopolitically strategic debtors receiving waivers?',
    'Comparative case studies analyzing the application and enforcement of conditionalities across a diverse range of debtor nations, including those with geopolitical significance, and examining the frequency and rationale for waivers.',
    'If selectively applied, the constraint''s effective suppression and extractiveness would be higher for powerless debtor nations, and the ''creditor coordination'' framing would be undermined by evidence of political rather than purely economic logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_of_enforcement, empirical, 'Whether conditionalities are universally applied or selectively enforced based on political considerations.').

omega_variable(
    social_cost_accounting,
    'Does the ''efficiency'' gained through conditionalities adequately account for the social costs (e.g., increased poverty, reduced public services, job losses) borne by vulnerable populations in debtor nations?',
    'Comprehensive social impact assessments and human rights impact assessments integrated into the evaluation of conditionalities, using methodologies that prioritize the well-being of affected populations.',
    'If social costs are systematically underestimated or externalized, the ''net benefit'' claim of the Rope classification would be challenged, potentially shifting the classification towards Tangled Rope due to unacknowledged victims and asymmetric costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_accounting, conceptual, 'Whether the benefits of conditionalities outweigh their social costs, from a broader welfare perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
