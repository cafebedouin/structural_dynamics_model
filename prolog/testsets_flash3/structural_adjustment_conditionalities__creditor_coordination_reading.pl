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
 *   This constraint story analyzes structural adjustment conditionalities
 *   from the 'creditor coordination' reading, where they are viewed as
 *   necessary mechanisms to ensure fiscal sustainability and maintain market
 *   confidence in debtor states. The narrative emphasizes their role in
 *   solving collective action problems between multiple creditors and
 *   preventing moral hazard on the part of borrowing governments. The victims
 *   are identified as inefficient state sectors, whose restructuring is seen
 *   as a necessary cost for broader economic health. This reading frames the
 *   constraint as a Rope, facilitating mutually beneficial coordination.
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
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__creditor_coordination_reading, 'e57cc844-ac10-4f2e-b028-c6c92affbfb3').
narrative_ontology:cs_kernel_codification('e57cc844-ac10-4f2e-b028-c6c92affbfb3', formalized).
narrative_ontology:cs_authority_grounding('e57cc844-ac10-4f2e-b028-c6c92affbfb3', lineage).
narrative_ontology:cs_interpretation_layer_present('e57cc844-ac10-4f2e-b028-c6c92affbfb3').
narrative_ontology:cs_reading_relation('e57cc844-ac10-4f2e-b028-c6c92affbfb3', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e57cc844-ac10-4f2e-b028-c6c92affbfb3', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('e57cc844-ac10-4f2e-b028-c6c92affbfb3', foundational, fiscal_sustainability_is_paramount).
narrative_ontology:cs_axiom_status(fiscal_sustainability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('e57cc844-ac10-4f2e-b028-c6c92affbfb3', fiscal_sustainability_is_paramount, instrumental).
narrative_ontology:cs_axiom('e57cc844-ac10-4f2e-b028-c6c92affbfb3', foundational, market_confidence_is_essential_for_development).
narrative_ontology:cs_axiom_status(market_confidence_is_essential_for_development, holdable).
narrative_ontology:cs_axiom_grounding('e57cc844-ac10-4f2e-b028-c6c92affbfb3', market_confidence_is_essential_for_development, empirically_contingent).
narrative_ontology:cs_reference_frame('e57cc844-ac10-4f2e-b028-c6c92affbfb3', washington_consensus_framework).
narrative_ontology:cs_drift_state('e57cc844-ac10-4f2e-b028-c6c92affbfb3', post_global_financial_crisis_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e57cc844-ac10-4f2e-b028-c6c92affbfb3', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__creditor_coordination_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditors).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_debtor_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_states_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provide loans to debtor states, conditioned on reforms aimed at fiscal sustainability. They benefit from repayment and a stable international financial system. They set the terms of the conditionalities and monitor compliance.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_creditors, agenda_setter,
    institutional, generational, mobile, global).

% Agree to conditionalities to secure financing, implementing reforms that can be politically costly. They bear the direct responsibility for implementing the reforms and managing the domestic political fallout.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, debtor_states_governments, payer,
    powerful, biographical, constrained, national).

% Are the direct targets of reforms, facing privatization, budget cuts, or restructuring. They bear the immediate costs of adjustment, including job losses or reduced public services, but are seen as necessary sacrifices for long-term stability.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, inefficient_state_sectors_debtor_states, payer,
    powerless, immediate, trapped, national).

% Benefit from improved fiscal health, reduced debt burdens, and a more stable economy in the long run, avoiding future crises and higher taxes. Their benefits are diffuse and delayed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, future_taxpayers_debtor_states, beneficiary,
    powerless, generational, analytical, national).

% Gain confidence in debtor states' ability to repay, leading to lower borrowing costs and increased investment flows. Conditionalities signal commitment to sound economic management, reducing perceived risk.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, international_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Analyze the effectiveness of conditionalities in promoting economic growth and stability, often from a perspective that emphasizes market-oriented reforms and fiscal discipline. Their analysis informs policy recommendations.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__creditor_coordination_reading, development_economists_creditor_aligned, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectations of international creditors and debtor states, ensuring that financing is provided in exchange for credible commitments to fiscal and structural reforms, thereby preventing moral hazard and ensuring repayment capacity.
% TRANSFER_FUNCTION: Transfers financial resources from international creditors to debtor states, in exchange for policy commitments that transfer control over economic policy decisions from debtor state governments to a framework aligned with creditor interests.
% ABSENT_VOICES: Labor unions, civil society organizations, and marginalized communities within debtor states, who often bear the brunt of austerity measures, are largely excluded from the negotiation of conditionalities. They would advocate for social protections and alternative development paths.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, international lending to developing countries would likely dry up or become prohibitively expensive due to increased risk. Debtor states would lose a critical source of financing, and the international financial system would face increased instability and defaults, forcing a reorganization of development finance.
% FOUNDING_PROBLEM: Debtor states faced unsustainable debt burdens and lacked the institutional capacity or political will to implement necessary fiscal and structural reforms, leading to repeated financial crises and a breakdown of trust with international lenders.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and many development economists (outside the immediate creditor institutions) attest that the core problem of fiscal indiscipline and institutional weakness in some debtor states remains, justifying the continued need for conditionalities. However, the specific form and impact of conditionalities are widely debated.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__creditor_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__creditor_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__creditor_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is considered moderate, representing the necessary costs of reform and the 'price' of accessing international capital, rather than pure rent extraction. Suppression (0.40) reflects the pressure on debtor states to comply, but also the perceived necessity of these reforms. The theater ratio (0.10) is low, as the reforms are generally seen as genuinely intended to achieve their stated goals, even if their effectiveness is debated. Accessibility collapse (0.60) is moderate, as debtor states have limited but not zero alternatives to conditional lending. Resistance (0.30) is present from affected sectors but is not sufficient to derail the overall process from this reading's perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international creditors and capital markets, conditionalities are a vital coordination tool, ensuring stability and repayment. From the perspective of inefficient state sectors, they are a direct imposition leading to job losses and service cuts. The engine's per-seat classification will reflect this divergence, with creditors experiencing a Rope-like constraint and affected sectors experiencing a more Snare-like dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   International creditors and capital markets are beneficiaries (low d) as they gain confidence and repayment. Future taxpayers in debtor states are also beneficiaries, as they avoid future crises. Debtor state governments are payers (moderate d) as they implement politically difficult reforms. Inefficient state sectors are victims (high d) as they are directly targeted for restructuring. The 'requires_active_enforcement' flag is true because compliance is monitored and non-compliance can lead to suspension of funds.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that conditionalities prevent mandatrophy by ensuring that the 'mandate' of fiscal sustainability and market confidence remains central. It frames the persistence of conditionalities as a response to ongoing problems, rather than an inertial continuation of an outdated mechanism. The 'founding_problem_status: live' supports this view, suggesting the original mandate is still relevant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_coordination,
    'What portion of the measured extractiveness is a genuine, unavoidable cost of coordinating international finance and what portion is rent-seeking by creditors?',
    'Detailed, independent economic analysis comparing the returns to creditors with the actual risks and administrative costs of lending, and benchmarking against alternative, less conditional financing models.',
    'If a significant portion is found to be rent-seeking, the constraint would shift towards a Tangled Rope or Snare, even from this reading''s perspective, as the coordination function would be revealed as cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_coordination, empirical, 'Distinguishing legitimate coordination costs from excess extraction.').

omega_variable(
    political_will_vs_structural_necessity,
    'To what extent do conditionalities address a genuine lack of political will for reform in debtor states versus imposing a specific, ideologically-driven reform agenda?',
    'Comparative studies of reform outcomes in states with and without conditionalities, controlling for initial conditions and domestic political dynamics, or analysis of alternative policy packages proposed by debtor states.',
    'If conditionalities are primarily imposing an agenda rather than enabling pre-existing political will, the ''coordination'' aspect diminishes, pushing the classification towards a more extractive type. If they genuinely enable reform, the Rope classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_will_vs_structural_necessity, empirical, 'Assessing the underlying drivers of reform and the role of external pressure.').

omega_variable(
    reading_divergence_debtor_extraction,
    'How would the classification change if viewed from the ''debtor_extraction_reading''?',
    'Analyze the same structural data through the lens of the ''debtor_extraction_reading'' (a separate constraint story) to quantify the difference in extractiveness, suppression, and victim identification.',
    'The ''debtor_extraction_reading'' would likely classify this as a Snare or Tangled Rope, with significantly higher extractiveness and suppression, and a broader victim set, highlighting the perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_debtor_extraction, conceptual, 'Documents the structural differences between the creditor coordination and debtor extraction readings of conditionalities.').

omega_variable(
    reading_divergence_hybrid_selectivity,
    'How would the classification change if viewed from the ''hybrid_selectivity_reading''?',
    'Analyze the same structural data through the lens of the ''hybrid_selectivity_reading'' (a separate constraint story) to quantify the difference in extractiveness, suppression, and victim identification, particularly focusing on the uneven application of conditionalities.',
    'The ''hybrid_selectivity_reading'' would likely classify this as a Tangled Rope, emphasizing the coordination function for some (geopolitically strategic debtors) and extraction for others (weaker states), with varying extractiveness and suppression depending on the debtor''s leverage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_hybrid_selectivity, conceptual, 'Documents the structural differences between the creditor coordination and hybrid selectivity readings of conditionalities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__creditor_coordination_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(stru_tr_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(stru_tr_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(stru_tr_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(stru_tr_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(stru_be_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(stru_be_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 1990, 0.25).
narrative_ontology:measurement(stru_be_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(stru_be_t2010, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(stru_be_t2020, structural_adjustment_conditionalities__creditor_coordination_reading, base_extractiveness, 2020, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t1980, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(stru_su_t1990, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(stru_su_t2000, structural_adjustment_conditionalities__creditor_coordination_reading, suppression_requirement, 2000, 0.4).
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
