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
 *   This story describes structural adjustment conditionalities from the
 *   perspective of international creditors and financial institutions. In
 *   this reading, conditionalities are primarily a coordination mechanism to
 *   ensure fiscal sustainability in debtor nations and maintain confidence in
 *   international capital markets. The 'victims' are inefficient state
 *   sectors, whose dismantling is seen as a necessary reform for long-term
 *   economic health, benefiting future taxpayers and the global financial
 *   system. This reading emphasizes the genuine collective action problem of
 *   sovereign debt and the role of conditionalities in solving it.
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
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__creditor_coordination_reading, rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__creditor_coordination_reading, "Structural Adjustment Conditionalities (Creditor Coordination Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__creditor_coordination_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__creditor_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, '8f01afe7-eb29-4006-8c07-317fecdc080a').
narrative_ontology:cs_kernel_codification('8f01afe7-eb29-4006-8c07-317fecdc080a', formalized).
narrative_ontology:cs_authority_grounding('8f01afe7-eb29-4006-8c07-317fecdc080a', lineage).
narrative_ontology:cs_interpretation_layer_present('8f01afe7-eb29-4006-8c07-317fecdc080a').
narrative_ontology:cs_reading_relation('8f01afe7-eb29-4006-8c07-317fecdc080a', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f01afe7-eb29-4006-8c07-317fecdc080a', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('8f01afe7-eb29-4006-8c07-317fecdc080a', foundational, fiscal_discipline_is_paramount).
narrative_ontology:cs_axiom_status(fiscal_discipline_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('8f01afe7-eb29-4006-8c07-317fecdc080a', fiscal_discipline_is_paramount, instrumental).
narrative_ontology:cs_axiom('8f01afe7-eb29-4006-8c07-317fecdc080a', foundational, market_confidence_is_essential_for_development).
narrative_ontology:cs_axiom_status(market_confidence_is_essential_for_development, holdable).
narrative_ontology:cs_axiom_grounding('8f01afe7-eb29-4006-8c07-317fecdc080a', market_confidence_is_essential_for_development, empirically_contingent).
narrative_ontology:cs_reference_frame('8f01afe7-eb29-4006-8c07-317fecdc080a', washington_consensus_orthodoxy).
narrative_ontology:cs_drift_state('8f01afe7-eb29-4006-8c07-317fecdc080a', post_asian_financial_crisis_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('8f01afe7-eb29-4006-8c07-317fecdc080a', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_in_debtor_nations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_in_debtor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_nation_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As lenders of last resort, they set the terms of structural adjustment loans, including conditionalities. Their role is to ensure fiscal discipline and market stability, protecting their own capital and the broader international financial system. They benefit from the repayment of loans and the perceived stability of the global financial order.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the increased confidence and reduced risk perception in debtor nations that implement conditionalities. This allows for more stable investment flows and higher returns, as conditionalities are seen as a signal of commitment to sound economic policies. They do not directly enforce the conditionalities but profit from their existence.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, biographical, mobile, global).

% Agree to conditionalities to access crucial financing, but bear the political and social costs of implementing austerity measures, privatization, and other reforms. They are constrained by the need for funds and the lack of viable alternative financing options, making exit from the conditionalities difficult without risking default.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_nation_governments, payer,
    moderate, immediate, constrained, national).

% Are expected to benefit from the long-term fiscal sustainability and economic growth that conditionalities are designed to foster, leading to a more stable economy and reduced future debt burdens. However, they have no direct voice in the negotiation of conditionalities and bear the immediate costs of reforms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_in_debtor_nations, beneficiary,
    powerless, generational, trapped, national).

% Are the direct targets of reforms like privatization and budget cuts, leading to job losses and reduced public services. They bear the immediate costs of structural adjustment, as their previous benefits (e.g., subsidies, job security) are dismantled in the name of efficiency and fiscal health.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_in_debtor_nations, payer,
    moderate, biographical, constrained, national).

% Often advocate for alternative development paths and criticize the social impact of conditionalities, but are largely excluded from the direct negotiation process between IFIs and debtor governments. Their influence is primarily through public protest and lobbying.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, civil_society_organizations_in_debtor_nations, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Conditionalities coordinate the expectations of international lenders and investors with the fiscal policies of debtor nations, ensuring that loans are repaid and that market confidence in the debtor's economy is maintained, thereby facilitating future capital flows.
% TRANSFER_FUNCTION: Conditionalities facilitate the transfer of capital (loans) from international financial institutions to debtor nations, in exchange for the transfer of policy autonomy and the implementation of specific economic reforms (e.g., privatization, austerity) by the debtor.
% ABSENT_VOICES: Civil society organizations, labor unions, and marginalized communities in debtor nations, who bear the brunt of austerity and privatization, are largely absent from the negotiation table. They would argue for more socially equitable and less market-driven development strategies.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, international financial institutions would likely cease or severely restrict lending to high-risk nations without policy assurances. This would lead to widespread sovereign defaults, capital flight, and a significant restructuring of global financial governance, as the mechanism for coordinating lender confidence and debtor policy would be gone.
% FOUNDING_PROBLEM: The founding problem was a series of sovereign debt crises in the 1970s and 80s, where debtor nations faced unsustainable debt burdens and lacked the fiscal discipline to repay, leading to a breakdown of trust with international lenders and capital markets.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and major creditor nations attest that the problem of fiscal instability and the need for policy discipline in debtor nations remains live, citing ongoing global economic volatility and the risk of new debt crises. Independent economic analysts also acknowledge the persistent challenge of sovereign debt management, though they may dispute the efficacy or equity of conditionalities as a solution.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.25) because, from this reading, the primary goal is not to extract rents but to ensure repayment and stability, with any 'extraction' being a necessary cost of reform. Suppression is moderate (0.4) as debtor nations face significant pressure to accept conditionalities due to limited alternative financing, but the coercion is framed as necessary for their own long-term good. Theater ratio is low (0.1) as the conditionalities are genuinely intended to achieve their stated goals of fiscal discipline and market confidence, with minimal performative elements. The time series shows relative stability, reflecting the view that the core function of conditionalities has remained consistent over time.
 *
 * PERSPECTIVAL GAP:
 *   The creditor coordination reading emphasizes the systemic benefits and necessity of conditionalities, while other readings (e.g., debtor extraction) highlight the coercive and exploitative aspects. The engine's classification will reveal how this 'Rope' claim holds up against the structural metrics, especially when compared to the 'Snare' classification expected from the debtor extraction reading. The divergence in extractiveness and suppression will be key.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and capital markets are clear beneficiaries (low d) as they receive loan repayments and benefit from market stability. Future taxpayers in debtor nations are also beneficiaries, as they are shielded from future debt crises. Debtor nation governments and inefficient state sectors are payers (higher d), bearing the immediate costs of reform. Civil society organizations are excluded, as their perspectives are not central to this coordination-focused reading.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading, mandatrophy is not a significant concern because the founding problem (sovereign debt crises and fiscal instability) is considered 'live'. The conditionalities are seen as an ongoing, necessary response to a persistent coordination problem, rather than an atrophied mechanism. The low theater ratio also supports this, indicating that the constraint's function is still largely genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine coordination mechanism, or is it primarily an instrument of extraction and power projection?',
    'Comparative analysis with sibling readings (debtor_extraction_reading, hybrid_selectivity_reading) and empirical studies on the long-term developmental impacts of conditionalities, disaggregated by social class and geopolitical context.',
    'If the debtor_extraction_reading or hybrid_selectivity_reading is found to be more structurally accurate, this constraint would reclassify from Rope to Tangled Rope or Snare, indicating a shift from coordination to asymmetric extraction or pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between coordination and extraction as the primary function of conditionalities.').

omega_variable(
    efficacy_of_conditionalities,
    'Do conditionalities consistently lead to fiscal sustainability and economic growth in debtor nations, or do they often exacerbate economic hardship and inequality?',
    'Longitudinal empirical studies comparing economic outcomes in nations that implemented conditionalities versus those that pursued alternative development strategies, controlling for confounding factors.',
    'If conditionalities are found to be consistently ineffective or counterproductive, the justification for their ''coordination'' function would weaken, potentially shifting the classification towards a more extractive type, as the claimed benefits would not materialize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_conditionalities, empirical, 'Empirical uncertainty regarding the actual economic impact and effectiveness of conditionalities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (debtor nations'' limited exit options) a natural consequence of their fiscal situation, or is it actively maintained by the structural power of international financial institutions?',
    'Analysis of alternative financing mechanisms and their accessibility, as well as the historical evolution of IFI lending practices and their impact on debtor sovereignty.',
    'If suppression is found to be actively maintained by IFI power rather than purely a consequence of debtor fiscal weakness, the constraint''s effective suppression would be higher, pushing it closer to a Tangled Rope or Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. actively maintained suppression mechanism in conditionalities.').


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

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
