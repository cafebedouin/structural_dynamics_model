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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: structural_adjustment_conditionalities__creditor_coordination_reading
 *   human_readable: Structural Adjustment Conditionalities (Creditor Coordination Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes structural adjustment conditionalities from the
 *   'creditor coordination' perspective, where they are viewed as essential
 *   mechanisms for ensuring fiscal sustainability in debtor nations and
 *   maintaining confidence in international capital markets. The narrative
 *   emphasizes their role in solving collective action problems among
 *   creditors and preventing moral hazard, ultimately benefiting both lenders
 *   and the long-term economic health of borrowing countries. This reading
 *   frames the costs borne by certain sectors as necessary reforms for
 *   overall stability.
 *
 * KEY AGENTS:
 *   - creditor_institutions: Primary agenda-setter and beneficiary (institutional/arbitrage)
 *   - international_capital_markets: Primary beneficiary (institutional/arbitrage)
 *   - debtor_governments: Primary payer, secondary beneficiary (institutional/constrained)
 *   - future_taxpayers_in_debtor_nations: Beneficiary (powerless/trapped)
 *   - inefficient_state_sectors: Payer (organized/constrained)
 *   - corrupt_elites: Payer (powerful/constrained)
 *   - analytical_economists_pro_conditionalities: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.35).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '7d756fcb-5b9e-4b26-8a39-f47e0a429ddb').
narrative_ontology:cs_kernel_codification('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', formalized).
narrative_ontology:cs_authority_grounding('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', expertise).
narrative_ontology:cs_interpretation_layer_present('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb').
narrative_ontology:cs_reading_relation('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', foundational, fiscal_discipline_is_paramount).
narrative_ontology:cs_axiom_status(fiscal_discipline_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', fiscal_discipline_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', foundational, market_confidence_is_essential).
narrative_ontology:cs_axiom_status(market_confidence_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', market_confidence_is_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', washington_consensus_framework).
narrative_ontology:cs_drift_state('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', contemporary_development_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('7d756fcb-5b9e-4b26-8a39-f47e0a429ddb', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_in_debtor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, corrupt_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International Monetary Fund (IMF) and World Bank, which design and enforce conditionalities. They benefit from the stability of the global financial system and the repayment of loans, ensuring their institutional mandate and influence.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, creditor_institutions, beneficiary).

% Private investors, banks, and funds that lend to developing nations. They benefit from the increased confidence, reduced risk, and improved macroeconomic stability in debtor countries, facilitating investment and higher returns.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Governments of developing countries that accept conditionalities in exchange for financial assistance. They bear the political costs of implementing reforms but gain access to crucial finance and improved market credibility.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments, payer,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_governments, beneficiary).

% Citizens in borrowing countries who are expected to benefit from long-term fiscal sustainability, reduced debt burdens, and a more stable economy, but have no direct agency in the negotiation or implementation of conditionalities.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_in_debtor_nations, beneficiary,
    powerless, generational, trapped, national).

% Public enterprises, bureaucracies, or state-subsidized industries targeted for reform, privatization, or budget cuts. They bear the costs of losing rents, jobs, or institutional power due to conditionalities aimed at improving efficiency.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors, payer,
    organized, biographical, constrained, national).

% Individuals or groups within debtor nations who benefit from rent-seeking, illicit capital flows, or patronage networks. Conditionalities often target these practices through governance reforms, leading to a loss of their illicit gains.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, corrupt_elites, payer,
    powerful, biographical, constrained, national).

% Economists and policy analysts who provide theoretical and empirical support for the efficacy and necessity of conditionalities in promoting macroeconomic stability, growth, and poverty reduction.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, analytical_economists_pro_conditionalities, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international creditors and debtor governments to ensure fiscal discipline, reduce moral hazard, and restore market confidence, thereby enabling access to vital development finance and promoting long-term economic stability.
% TRANSFER_FUNCTION: Transfers policy autonomy from debtor governments to creditor institutions in exchange for financial assistance, aiming to transfer long-term economic stability to debtor nations and reduce risk for international capital.
% ABSENT_VOICES: Populist movements, nationalistic factions, and civil society groups in debtor countries who would argue for sovereign autonomy over economic policy and prioritize social welfare over fiscal austerity, but are often marginalized in international financial negotiations.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, international lending would become significantly riskier, leading to higher interest rates or reduced access to capital for developing nations. This would likely result in more frequent sovereign defaults, destabilizing global financial markets and hindering development efforts.
% FOUNDING_PROBLEM: Recurrent sovereign debt crises, moral hazard in international lending, and a perceived lack of fiscal discipline in borrowing nations leading to unsustainable debt burdens and global financial instability.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions (IMF, World Bank) and many mainstream economists attest to the ongoing relevance of these problems, citing continued risks of debt distress and the need for sound macroeconomic management. This perspective is widely supported by academic literature in development economics and international finance.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.25) and suppression (0.35) reflect this reading's view that conditionalities are primarily a coordination mechanism with necessary, but not excessive, costs. The theater ratio is low (0.15) because the reforms are seen as genuinely functional. Accessibility collapse is moderate (0.45) as alternatives to IMF/World Bank financing exist but are often less attractive or come with higher costs. Resistance is low (0.20) because, from this perspective, the overall benefits outweigh the costs, and resistance is often attributed to vested interests rather than systemic flaws.
 *
 * PERSPECTIVAL GAP:
 *   This reading presents a coherent view of conditionalities as a Rope. However, other readings (e.g., 'debtor extraction' or 'hybrid selectivity') would assign significantly higher extractiveness and suppression, reflecting the costs borne by debtor populations or the selective application of rules. The engine's classification for other seats would diverge sharply from this 'Rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditor institutions and international capital markets are clear beneficiaries, gaining stability and reduced risk. Future taxpayers are also beneficiaries, as they avoid future debt crises. Debtor governments are payers in terms of policy autonomy but also beneficiaries of access to finance. Inefficient state sectors and corrupt elites are the primary targets/payers, as the reforms directly dismantle their sources of rent or power. Analytical economists supporting this view act as observers, providing intellectual legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   From this 'creditor coordination' reading, the mandate of conditionalities (ensuring fiscal sustainability and market confidence) is still live and actively addressed. The classification as a Rope prevents mislabeling it as pure extraction, acknowledging its genuine coordination function in a complex global financial system. The low theater ratio indicates that the constraint is not merely performative but actively serves its stated purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_ambiguity,
    'Is the primary function of structural adjustment conditionalities genuine coordination among creditors and for market stability, or is it a cover for asymmetric extraction from debtor nations?',
    'Comparative analysis of long-term economic outcomes in countries that adopted conditionalities versus those that pursued alternative development paths, controlling for initial conditions and external shocks. Also, detailed analysis of the distribution of benefits and costs across different social strata within debtor nations.',
    'If primarily extractive, the constraint would reclassify towards a Snare or Tangled Rope, with significantly higher effective extraction for debtor nations and their populations. If primarily coordination, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, empirical, 'Distinguishing the genuine coordination function from potential extractive elements.').

omega_variable(
    efficiency_vs_social_cost_ambiguity,
    'Are the ''inefficient state sectors'' targeted by conditionalities truly inefficient and detrimental to national development, or do they represent vital public services and social safety nets whose dismantling imposes unacceptable social costs?',
    'Detailed, independent social impact assessments of privatization and austerity measures, including longitudinal studies on poverty, inequality, and access to essential services (health, education) in affected countries.',
    'If vital services are being dismantled, the ''victims'' are not merely ''inefficient sectors'' but vulnerable populations, increasing the perceived extractiveness and suppression from their seat, potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_vs_social_cost_ambiguity, conceptual, 'Ambiguity in defining ''inefficient state sectors'' and their social impact.').

omega_variable(
    market_confidence_beneficiary_ambiguity,
    'Does ''market confidence'' primarily benefit debtor nations by lowering borrowing costs and attracting investment, or does it disproportionately serve the interests of international capital markets by securing returns and reducing risk for creditors?',
    'Analysis of the terms of trade, capital flight patterns, and the net financial flows between debtor nations and international capital markets before and after conditionalities, focusing on who captures the majority of the value created.',
    'If benefits disproportionately accrue to creditors, the ''international_capital_markets'' stakeholder''s directionality would shift closer to full beneficiary, and the overall extractiveness from the debtor''s seat would increase, pushing the constraint towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_confidence_beneficiary_ambiguity, empirical, 'Ambiguity regarding the primary beneficiary of ''market confidence''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(stru_tr_t1986, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1986, 0.12).
narrative_ontology:measurement(stru_tr_t1992, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1992, 0.14).
narrative_ontology:measurement(stru_tr_t1998, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(stru_tr_t2004, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.15).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(stru_be_t1986, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1986, 0.22).
narrative_ontology:measurement(stru_be_t1992, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1992, 0.24).
narrative_ontology:measurement(stru_be_t1998, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(stru_be_t2004, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2004, 0.25).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(stru_su_t1986, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1986, 0.32).
narrative_ontology:measurement(stru_su_t1992, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1992, 0.34).
narrative_ontology:measurement(stru_su_t1998, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(stru_su_t2004, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2004, 0.35).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__creditor_coordination_reading, 0.15).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, international_debt_regime).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, global_financial_stability_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'structural_adjustment_conditionalities' kernel. This 'creditor coordination' reading emphasizes the positive coordination function, while sibling readings focus on extraction or selective enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
