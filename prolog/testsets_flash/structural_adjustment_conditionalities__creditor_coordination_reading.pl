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
 *   This constraint story analyzes structural adjustment conditionalities
 *   from the perspective of international creditors, viewing them as
 *   essential coordination mechanisms. In this reading, conditionalities
 *   solve a genuine collective action problem in international finance by
 *   ensuring fiscal sustainability and maintaining market confidence,
 *   benefiting both creditors and future taxpayers in debtor states. The
 *   'victims' are identified as inefficient state sectors that are reformed
 *   or dismantled for the greater good of economic stability. This reading
 *   acknowledges some extraction (0.2) and suppression (0.4) as necessary
 *   costs of coordination, but frames them as proportionate to the benefits
 *   of a stable global financial system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__creditor_coordination_reading, 0.2).
domain_priors:suppression_score(structural_adjustment_conditionalities__creditor_coordination_reading, 0.4).
domain_priors:theater_ratio(structural_adjustment_conditionalities__creditor_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__creditor_coordination_reading, extractiveness, 0.2).
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
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'b0d2db17-4863-4441-853b-feeb49914b0a').
narrative_ontology:cs_kernel_codification('b0d2db17-4863-4441-853b-feeb49914b0a', formalized).
narrative_ontology:cs_authority_grounding('b0d2db17-4863-4441-853b-feeb49914b0a', lineage).
narrative_ontology:cs_interpretation_layer_present('b0d2db17-4863-4441-853b-feeb49914b0a').
narrative_ontology:cs_reading_relation('b0d2db17-4863-4441-853b-feeb49914b0a', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0d2db17-4863-4441-853b-feeb49914b0a', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('b0d2db17-4863-4441-853b-feeb49914b0a', foundational, fiscal_sustainability_is_paramount).
narrative_ontology:cs_axiom_status(fiscal_sustainability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b0d2db17-4863-4441-853b-feeb49914b0a', fiscal_sustainability_is_paramount, instrumental).
narrative_ontology:cs_axiom('b0d2db17-4863-4441-853b-feeb49914b0a', foundational, market_liberalization_drives_growth).
narrative_ontology:cs_axiom_status(market_liberalization_drives_growth, holdable).
narrative_ontology:cs_axiom_grounding('b0d2db17-4863-4441-853b-feeb49914b0a', market_liberalization_drives_growth, empirically_contingent).
narrative_ontology:cs_reference_frame('b0d2db17-4863-4441-853b-feeb49914b0a', washington_consensus_framework).
narrative_ontology:cs_drift_state('b0d2db17-4863-4441-853b-feeb49914b0a', contemporary_post_crisis_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b0d2db17-4863-4441-853b-feeb49914b0a', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_in_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, global_financial_markets).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_in_debtor_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide loans to debtor states, conditional on policy reforms. They benefit from the increased likelihood of repayment and the stability of the international financial system. They set the terms of conditionalities to ensure fiscal discipline and market-oriented reforms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditors, agenda_setter,
    institutional, generational, mobile, global).

% Receive loans but must implement often unpopular policy reforms, such as privatization, deregulation, and austerity measures. They bear the political cost of these reforms but gain access to crucial financing.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_state_governments, payer,
    moderate, biographical, constrained, national).

% Are targeted for reform or dismantling under conditionalities, leading to job losses, reduced subsidies, and loss of political influence. They bear the direct costs of structural adjustment.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_in_debtor_states, payer,
    powerless, immediate, trapped, local).

% Benefit from improved fiscal health, reduced national debt, and a more stable economy in the long run, theoretically leading to lower taxes and better public services. Their benefit is diffuse and delayed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_in_debtor_states, beneficiary,
    powerless, generational, identity_locked, national).

% Benefit from the perceived stability and predictability that conditionalities bring to international lending, reducing systemic risk and encouraging capital flows. They are a diffuse beneficiary of the coordination.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, global_financial_markets, beneficiary,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate lending and borrowing behavior between international creditors and debtor states, ensuring that loans are used to promote fiscal sustainability and market confidence, thereby preventing moral hazard and ensuring repayment.
% TRANSFER_FUNCTION: Transfers financial capital from international creditors to debtor states, in exchange for policy commitments (conditionalities) that transfer control over economic policy decisions from debtor states to a framework aligned with creditor interests.
% ABSENT_VOICES: Labor unions, civil society organizations, and marginalized communities in debtor states, who often bear the brunt of austerity measures and privatization, would argue for alternative development models that prioritize social welfare over market liberalization.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished, international lending to developing countries would likely become more volatile and less predictable, potentially leading to increased defaults, reduced market confidence, and a breakdown in the current system of development finance. Debtor states would lose a key source of financing, but also regain full policy autonomy.
% FOUNDING_PROBLEM: The problem of moral hazard in international lending: debtor states might undertake unsustainable fiscal policies, expecting bailouts, and creditors might lend recklessly, expecting to be repaid regardless of debtor performance. This led to cycles of debt crises and instability.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions (IMF, World Bank) and major creditor nations consistently attest that the problem of moral hazard and the need for fiscal discipline remain live. Independent economic analyses, while critical of specific conditionalities, generally acknowledge the underlying coordination problem in international finance.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(structural_adjustment_conditionalities__creditor_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.2) reflects the view that conditionalities primarily serve a coordination function, with any extraction being a necessary cost of ensuring fiscal discipline and market confidence. Suppression (0.4) is present as active enforcement is required to ensure compliance, but it's not seen as excessive given the stakes of international financial stability. The low theater ratio (0.1) indicates that the stated purpose of conditionalities (fiscal sustainability) is largely aligned with their actual operation in this reading. Accessibility collapse (0.6) is moderate, as debtor states have some, albeit constrained, alternatives, and resistance (0.3) is present but not overwhelming.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor coordination perspective, the constraint is a Rope, solving a critical coordination problem. However, from the perspective of debtor states or affected populations, the same conditionalities might be experienced as a Snare or Tangled Rope due to perceived coercion and asymmetric extraction. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International creditors and global financial markets are clear beneficiaries, as conditionalities reduce their risk and stabilize the system. Future taxpayers in debtor states are also beneficiaries, as they inherit a more fiscally sound economy. Debtor state governments are payers, bearing the political costs of implementing reforms. Inefficient state sectors are direct victims, as they are targeted for restructuring. The directionality for creditors is low (beneficiary), for debtor governments and state sectors it is high (target), and for future taxpayers and global markets it is low (beneficiary).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_power_imposition,
    'To what extent do conditionalities genuinely solve a coordination problem versus imposing the will of powerful creditors on weaker debtors?',
    'Analysis of counterfactual scenarios where debtor states successfully implement reforms without conditionalities, or where conditionalities are negotiated under more symmetric power conditions.',
    'If primarily power imposition, the extractiveness and suppression metrics would be re-evaluated upwards, potentially reclassifying the constraint as a Tangled Rope or Snare. If genuine coordination, the Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_power_imposition, conceptual, 'Ambiguity between coordination and power dynamics.').

omega_variable(
    long_term_benefits_vs_short_term_costs,
    'Are the long-term benefits of conditionalities (fiscal sustainability, market confidence) consistently realized and do they outweigh the short-term social and economic costs borne by debtor populations?',
    'Longitudinal empirical studies comparing economic and social development outcomes in states that underwent structural adjustment versus those that did not, controlling for other factors.',
    'If long-term benefits are not consistently realized or do not outweigh costs, the ''future_taxpayers_in_debtor_states'' might shift from beneficiary to payer, increasing overall extractiveness and challenging the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_benefits_vs_short_term_costs, empirical, 'Uncertainty about the net long-term impact of conditionalities.').

omega_variable(
    selectivity_of_enforcement,
    'Is the enforcement of conditionalities applied universally based on economic principles, or is it selectively applied based on geopolitical interests, as suggested by the ''hybrid_selectivity_reading''?',
    'Comparative analysis of conditionalities applied to geopolitically strategic versus non-strategic debtor states with similar economic profiles.',
    'If enforcement is found to be highly selective, it would undermine the claim of universal coordination and suggest a more extractive or politically motivated function, pushing the classification towards Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selectivity_of_enforcement, empirical, 'Whether conditionalities are applied consistently or selectively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.21).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(stru_su_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(stru_su_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__creditor_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, international_debt_management_regime).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__creditor_coordination_reading, global_capital_flow_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
