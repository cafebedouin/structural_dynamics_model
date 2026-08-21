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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Mandate: Climate Risk Integration
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint represents the 'climate_incorporation' reading of the
 *   ECB's mandate under Article 127 TFEU, which interprets the mandate as
 *   requiring the integration of climate risk into asset purchases and
 *   collateral frameworks, driven by treaty obligations (Article 11 TFEU).
 *   This reading asserts that climate change poses systemic financial risks
 *   and that the ECB has a duty to support EU climate policy, leading to
 *   active measures that extract from high-carbon assets and benefit
 *   climate-aligned industries. The claimed type is Tangled Rope, reflecting
 *   a genuine coordination function (financial stability, climate alignment)
 *   coupled with asymmetric extraction and active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.7).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.75).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate: Climate Risk Integration").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, 'e738c6b9-e9b5-4d12-9696-0bdc30c153e8').
narrative_ontology:cs_kernel_codification('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', formalized).
narrative_ontology:cs_authority_grounding('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', lineage).
narrative_ontology:cs_interpretation_layer_present('e738c6b9-e9b5-4d12-9696-0bdc30c153e8').
narrative_ontology:cs_reading_relation('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', foundational, environmental_integration_principle).
narrative_ontology:cs_axiom_status(environmental_integration_principle, holdable).
narrative_ontology:cs_axiom_grounding('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', environmental_integration_principle, deontological).
narrative_ontology:cs_axiom('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', foundational, climate_risk_is_financial_risk).
narrative_ontology:cs_axiom_status(climate_risk_is_financial_risk, holdable).
narrative_ontology:cs_axiom_grounding('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', climate_risk_is_financial_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', environmental_integration_principle).
narrative_ontology:cs_drift_state('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', contemporary_eu_policy, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e738c6b9-e9b5-4d12-9696-0bdc30c153e8', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_aligned_industries).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_goals).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, financial_stability_advocates).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, high_carbon_industries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_issuers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, eu_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the central bank of the Eurozone, the ECB interprets and implements its mandate, including integrating climate risk into its operational frameworks for asset purchases and collateral. It actively shapes policy to align with EU climate objectives.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from a more climate-resilient financial system and progress towards EU climate goals. However, some member states with carbon-intensive economies may face economic adjustments and political pressure due to the ECB's policies.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_member_states, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, eu_member_states, payer).

% Faces increased financing costs, reduced access to central bank liquidity, and collateral haircuts as their assets are de-risked or excluded from ECB operations. This directly impacts their business models and investment strategies.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% Benefit from increased capital flows, favorable collateral treatment, and reduced financing costs as the ECB's policies de-risk green investments and incentivize climate transition. This supports their growth and market position.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_aligned_industries, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from a more stable financial system resilient to climate shocks and progress towards a sustainable economy. Indirectly bear costs through potential economic adjustments during the transition, but also benefit from reduced climate-related damages.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_citizens, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, eu_citizens, payer).

% Critically analyze the ECB's climate integration policies, often arguing for a narrower interpretation of the mandate focused solely on price stability. Their influence is primarily through academic discourse and policy recommendations.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_economists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align the Eurosystem's monetary policy operations with the broader EU climate policy agenda, specifically by integrating climate-related financial risks into asset purchases and collateral frameworks, thereby contributing to financial stability and the green transition.
% TRANSFER_FUNCTION: This arrangement shifts financial support and capital away from carbon-intensive assets and towards climate-aligned investments, effectively transferring financial advantage and risk exposure from the fossil fuel sector to green industries and the broader financial system.
% ABSENT_VOICES: Future generations, who are the primary beneficiaries of effective climate action, lack direct representation in the policy-making process. Their interests are mediated through current political and institutional actors.
% DISAPPEARANCE_RATIONALE: If the mandate for climate risk integration vanished, the ECB would likely revert to a more narrow interpretation of its mandate, removing climate considerations from its operations. This would immediately alter financial flows, potentially increasing systemic climate-related financial risks and undermining EU climate policy goals, leading to a significant reorganization of financial markets and climate transition efforts.
% FOUNDING_PROBLEM: The increasing recognition of climate change as a systemic financial risk (physical and transition risks) that could impact financial stability, coupled with the EU's treaty obligation (Article 11 TFEU) to integrate environmental protection requirements into all its policies.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change, reports from international financial bodies (e.g., Network for Greening the Financial System - NGFS), and legal interpretations of EU treaties by independent scholars and the European Parliament corroborate the live status of the founding problem. The ECB's own financial stability reviews also increasingly highlight climate risk.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) due to the direct financial impact on carbon-intensive assets through collateral haircuts and portfolio tilting. Suppression is also high (0.75) as the ECB's institutional power actively restricts market access and liquidity for certain assets. The theater ratio is moderate (0.3), indicating that while there is genuine policy implementation, there is also an element of political signaling and ongoing debate about the extent and pace of integration. The increasing trend in extractiveness and suppression reflects the progressive implementation of these policies since 2018.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (e.g., climate-aligned industries, some EU member states) view it as a necessary and legitimate evolution of the ECB's mandate to address systemic risks and support treaty obligations. Opponents (e.g., fossil fuel sector, orthodox economists) perceive it as an overreach of the mandate, leading to undue extraction and market distortion. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB (agenda_setter) and climate-aligned industries (beneficiary) are positioned to gain from this constraint, experiencing low effective extraction or even subsidy. The fossil fuel sector and high-carbon industries (victims/payers) are direct targets of extraction, facing high effective extraction. EU member states and citizens have mixed positions, benefiting from climate stability but potentially bearing transition costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively evolving, not atrophying. The 'live' status of the founding problem (climate risk) and the 'world_rearranges' disappearance verdict indicate that the mandate is seen as highly relevant and functional by its proponents. The contestation around its status (founding_problem_status: contested) highlights the ongoing debate about its legitimacy and scope, preventing a clear mandatrophy declaration at this time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a legitimate interpretation of the ECB''s mandate, or an overreach?',
    'Legal rulings from the European Court of Justice on the scope of Article 127 TFEU and Article 11 TFEU in monetary policy, or a formal amendment to the ECB''s mandate.',
    'If deemed an overreach, the constraint''s legitimacy would collapse, leading to reclassification as a Snare or Piton from the perspective of those it extracts from. If fully validated, it would solidify its Tangled Rope status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity regarding the legal and institutional scope of the ECB''s mandate concerning climate policy.').

omega_variable(
    orthodox_reading_impact,
    'What would be the full structural impact if the ''orthodox_price_stability'' reading of the ECB mandate were to prevail?',
    'Analysis of counterfactual policy scenarios where climate considerations are entirely removed from ECB operations, modeling financial stability and climate transition outcomes.',
    'If the orthodox reading prevailed, this ''climate_incorporation'' constraint would cease to exist as an active policy, leading to a re-evaluation of financial stability risks and a significant setback for EU climate policy goals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_reading_impact, empirical, 'The structural consequences of an exclusive price stability focus on climate integration.').

omega_variable(
    suppression_mechanism_effectiveness,
    'How effective are portfolio tilting and collateral haircuts in genuinely suppressing high-carbon investments versus merely shifting them to less regulated markets?',
    'Empirical studies tracking capital flows and investment patterns in response to ECB policies, including potential leakage to non-Eurozone or private markets.',
    'If suppression is found to be ineffective or merely displaces carbon, the constraint''s actual impact on climate goals would be lower, potentially increasing its theater_ratio and reducing its overall effectiveness as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_effectiveness, empirical, 'The real-world efficacy of the ECB''s climate-related financial suppression mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2018, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2018, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(ecb__tr_t2019, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(ecb__tr_t2022, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2022, 0.31).
narrative_ontology:measurement(ecb__tr_t2023, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2023, 0.3).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2018, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(ecb__be_t2019, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.65).
narrative_ontology:measurement(ecb__be_t2022, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement(ecb__be_t2023, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2023, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2018, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement(ecb__su_t2019, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(ecb__su_t2022, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2022, 0.73).
narrative_ontology:measurement(ecb__su_t2023, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_green_deal_targets).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, financial_stability_frameworks).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_taxonomy_for_sustainable_activities).

% DUAL FORMULATION NOTE:
% This constraint is the 'climate_incorporation' reading of the 'ecb_mandate_article_127' kernel. It is structurally distinct from the 'orthodox_price_stability' and 'expansive_secondary_objectives' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
