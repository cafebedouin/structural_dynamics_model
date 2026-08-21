% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Mandate: Climate Risk Integration (Article 127 TFEU)
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint represents the 'climate_incorporation' reading of the
 *   ECB's mandate under Article 127 TFEU, where climate risk integration into
 *   monetary policy operations (asset purchases, collateral frameworks) is
 *   seen as a treaty obligation (Article 11 TFEU). This reading introduces
 *   new beneficiaries (green economy sectors, EU climate policy makers) and
 *   new victims (fossil fuel, carbon-intensive industries) by re-orienting
 *   financial flows. The constraint is claimed as a Tangled Rope due to its
 *   genuine coordination function (addressing systemic climate risk) coupled
 *   with clear asymmetric extraction from carbon-intensive sectors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.75).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate: Climate Risk Integration (Article 127 TFEU)").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '63162df9-2be6-4a84-8827-3c4f401eee22').
narrative_ontology:cs_kernel_codification('63162df9-2be6-4a84-8827-3c4f401eee22', formalized).
narrative_ontology:cs_authority_grounding('63162df9-2be6-4a84-8827-3c4f401eee22', lineage).
narrative_ontology:cs_interpretation_layer_present('63162df9-2be6-4a84-8827-3c4f401eee22').
narrative_ontology:cs_reading_relation('63162df9-2be6-4a84-8827-3c4f401eee22', ecb_mandate_article_127__orthodox_price_stability, influences).
narrative_ontology:cs_reading_relation('63162df9-2be6-4a84-8827-3c4f401eee22', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('63162df9-2be6-4a84-8827-3c4f401eee22', foundational, environmental_integration_principle).
narrative_ontology:cs_axiom_status(environmental_integration_principle, holdable).
narrative_ontology:cs_axiom_grounding('63162df9-2be6-4a84-8827-3c4f401eee22', environmental_integration_principle, deontological).
narrative_ontology:cs_axiom('63162df9-2be6-4a84-8827-3c4f401eee22', foundational, climate_risk_financial_stability_nexus).
narrative_ontology:cs_axiom_status(climate_risk_financial_stability_nexus, holdable).
narrative_ontology:cs_axiom_grounding('63162df9-2be6-4a84-8827-3c4f401eee22', climate_risk_financial_stability_nexus, empirically_contingent).
narrative_ontology:cs_reference_frame('63162df9-2be6-4a84-8827-3c4f401eee22', ecb_mandate_as_climate_integrator).
narrative_ontology:cs_drift_state('63162df9-2be6-4a84-8827-3c4f401eee22', contemporary_policy_implementation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('63162df9-2be6-4a84-8827-3c4f401eee22', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, ecb).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_economy_sectors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, national_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets its mandate to include climate risk integration, leading to adjustments in asset purchase programs and collateral frameworks. Faces pressure from both climate advocates and orthodox economists. Benefits from enhanced legitimacy by aligning with broader EU goals, but bears the cost of internal and external contestation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from the ECB's integration of climate considerations, as it provides a powerful financial lever to support EU climate policy objectives under Article 11 TFEU. They gain an additional enforcement mechanism for their policy goals without direct budgetary cost.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers, beneficiary,
    institutional, generational, mobile, continental).

% Benefit from favorable collateral treatment and potentially increased demand for their assets in ECB purchase programs, reducing their cost of capital and accelerating growth. They are net recipients of the constraint's re-allocation of financial flows.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_economy_sectors, beneficiary,
    organized, biographical, mobile, regional).

% Faces increased financing costs due to collateral haircuts on carbon-intensive assets and potential exclusion from ECB asset purchase programs. They bear the direct financial extraction and are forced to adapt or shrink. Their exit options are limited by the scale of their existing infrastructure and market dependence.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% Similar to the fossil fuel sector, they face higher costs of capital and reduced access to liquidity due to the ECB's climate-integrated policies. They are forced to internalize climate risks that were previously externalized, impacting their profitability and investment decisions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries, payer,
    organized, biographical, constrained, national).

% Implement the ECB's policies at the national level, including adjusting their collateral frameworks and asset purchases. They bear the operational costs and potential political backlash from affected national industries, while having limited autonomy to deviate from the ECB's interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, national_central_banks, payer,
    institutional, generational, constrained, national).

% Argue that the ECB's primary mandate is price stability and that climate policy falls outside its remit, potentially compromising its independence and effectiveness. Their arguments are heard in academic and policy debates but are not currently shaping the ECB's operational decisions on climate integration.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the financial system's response to climate-related financial risks, ensuring that the ECB's monetary policy operations do not inadvertently exacerbate climate change and instead support the EU's broader environmental objectives.
% TRANSFER_FUNCTION: Transfers financial advantage (lower cost of capital, increased liquidity) from carbon-intensive sectors to green economy sectors by adjusting collateral eligibility and asset purchase criteria, effectively re-allocating capital in line with climate goals.
% ABSENT_VOICES: Orthodox economists and financial institutions heavily invested in carbon-intensive assets, who would argue against the ECB's expanded interpretation of its mandate and the financial implications for their portfolios. They are present in public discourse but lack direct influence over the ECB's current policy direction.
% DISAPPEARANCE_RATIONALE: If the ECB's climate integration mandate vanished, the financial system would revert to a 'climate-blind' state, potentially increasing systemic climate risk and removing a significant financial incentive for green transition. Capital flows would re-orient towards carbon-intensive assets, and EU climate policy would lose a powerful, non-fiscal lever.
% FOUNDING_PROBLEM: The recognition that climate change poses systemic financial risks (physical and transition risks) that could impact price stability and the stability of the financial system, coupled with the EU's treaty obligation to integrate environmental protection into all its policies (Article 11 TFEU).
% FOUNDING_PROBLEM_CORROBORATION: The European Commission, the European Parliament, and a broad consensus among climate scientists and financial regulators corroborate the live status of climate-related financial risks. Independent reports from the Network for Greening the Financial System (NGFS) and the IPCC support the urgency of integrating climate considerations into financial policy.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) reflects the significant financial re-allocation from carbon-intensive to green sectors through collateral haircuts and portfolio tilting. Suppression (0.75) is high because the ECB's institutional power enforces these changes, limiting the ability of affected industries to avoid the costs. Theater ratio (0.40) indicates that while there's genuine action, there's also a performative aspect in balancing climate goals with the primary price stability mandate, leading to incremental rather than transformative shifts. The rising trend in extractiveness and suppression reflects the increasing operationalization of climate policy within the ECB's framework.
 *
 * PERSPECTIVAL GAP:
 *   The ECB and EU climate policy makers perceive this as a necessary and legitimate evolution of the mandate, a coordination mechanism to address systemic risk. Conversely, the fossil fuel and carbon-intensive sectors view it as an illegitimate expansion of power leading to direct extraction. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a Rope-like function and victims experiencing a Snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB itself, as the agenda-setter, benefits from enhanced legitimacy and alignment with EU goals (low d). EU climate policy makers and green economy sectors are clear beneficiaries (low d) as the constraint directly supports their objectives and provides financial advantages. The fossil fuel and carbon-intensive industries are direct targets (high d) as they bear the financial costs. National central banks are also targets (high d) as they implement the policies and face operational burdens without direct benefit. Orthodox economists are excluded, their views not directly shaping policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_legitimacy,
    'Is the ECB''s interpretation of its mandate to include climate risk integration a legitimate evolution of its treaty obligations, or an overreach beyond its primary price stability objective?',
    'A ruling by the European Court of Justice on the scope of Article 127 and Article 11 TFEU, or a formal amendment to the ECB''s statutes.',
    'If deemed an overreach, the constraint would lose its legal grounding, reducing its suppression and extractiveness significantly, potentially reclassifying it as a Piton or even dissolving it. If affirmed, its legitimacy would be strengthened, allowing for further integration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_interpretation_legitimacy, conceptual, 'Ambiguity regarding the legal and constitutional scope of the ECB''s mandate concerning climate policy.').

omega_variable(
    effectiveness_of_portfolio_tilting,
    'How effective are the ECB''s climate-integrated asset purchases and collateral frameworks in genuinely reducing systemic climate risk and re-orienting capital flows, versus merely creating a ''green premium'' without real-world impact?',
    'Empirical studies tracking the real-world emissions reductions, investment shifts, and climate risk exposure of the financial system attributable to ECB policies, controlling for other factors.',
    'If ineffective, the constraint''s ''coordination'' function would be revealed as largely theatrical, increasing its theater_ratio and potentially reclassifying it closer to a Snare or Piton. If highly effective, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_portfolio_tilting, empirical, 'Uncertainty about the real-world impact and efficacy of the ECB''s climate-related financial instruments.').

omega_variable(
    price_stability_tradeoff,
    'Does the integration of climate considerations into monetary policy operations create a material tradeoff with the ECB''s primary objective of price stability, or are the two objectives mutually reinforcing?',
    'Longitudinal econometric analysis of inflation dynamics and financial stability under climate-integrated policies, compared to counterfactual scenarios without such integration.',
    'If a significant tradeoff is demonstrated, it would fuel resistance from orthodox economists and potentially lead to a re-evaluation of the policy, reducing its stability. If mutually reinforcing, it would strengthen the constraint''s legitimacy and persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_stability_tradeoff, empirical, 'Potential conflict or synergy between climate integration and price stability objectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ecb__tr_t2, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ecb__tr_t4, ecb_mandate_article_127__climate_incorporation, theater_ratio, 4, 0.35).
narrative_ontology:measurement(ecb__tr_t6, ecb_mandate_article_127__climate_incorporation, theater_ratio, 6, 0.38).
narrative_ontology:measurement(ecb__tr_t8, ecb_mandate_article_127__climate_incorporation, theater_ratio, 8, 0.39).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ecb__be_t2, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(ecb__be_t4, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 4, 0.63).
narrative_ontology:measurement(ecb__be_t6, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(ecb__be_t8, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 8, 0.67).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ecb__su_t2, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(ecb__su_t4, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(ecb__su_t6, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(ecb__su_t8, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_green_deal_regulations).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, esg_investment_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ECB's Article 127 mandate, alongside 'orthodox_price_stability' and 'expansive_secondary_objectives'. Each reading represents a distinct structural claim about the mandate's operational scope and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
