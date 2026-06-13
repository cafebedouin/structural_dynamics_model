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
 *   ECB's mandate under Article 127 TFEU, which requires the integration of
 *   climate risk into asset purchases and collateral frameworks as a treaty
 *   obligation to support EU climate policy. This reading asserts that the
 *   ECB must actively use its monetary policy tools to facilitate the green
 *   transition, even if it introduces novel forms of extraction and
 *   suppression on carbon-intensive sectors. It is a contested
 *   interpretation, with significant implications for the financial landscape
 *   and the ECB's operational independence.
 *
 * KEY AGENTS:
 *   - ecb: Agenda-setter, institutional power, civilizational time horizon, analytical exit options, global scope.
 *   - eu_climate_policy_makers: Beneficiary, institutional power, generational time horizon, analytical exit options, continental scope.
 *   - green_finance_sector: Beneficiary, powerful, biographical time horizon, arbitrage exit options, global scope.
 *   - fossil_fuel_sector: Payer, powerful, biographical time horizon, constrained exit options, global scope.
 *   - carbon_intensive_industries: Payer, organized, biographical time horizon, constrained exit options, global scope.
 *   - orthodox_economists: Excluded, analytical, biographical time horizon, analytical exit options, global scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.65).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.7).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate: Climate Risk Integration").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '1b86e328-c820-4611-a21a-b894c5c6f210').
narrative_ontology:cs_kernel_codification('1b86e328-c820-4611-a21a-b894c5c6f210', fixed_text).
narrative_ontology:cs_authority_grounding('1b86e328-c820-4611-a21a-b894c5c6f210', lineage).
narrative_ontology:cs_interpretation_layer_present('1b86e328-c820-4611-a21a-b894c5c6f210').
narrative_ontology:cs_reading_relation('1b86e328-c820-4611-a21a-b894c5c6f210', ecb_mandate_article_127__orthodox_price_stability, influences).
narrative_ontology:cs_reading_relation('1b86e328-c820-4611-a21a-b894c5c6f210', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('1b86e328-c820-4611-a21a-b894c5c6f210', foundational, climate_risk_is_financial_risk).
narrative_ontology:cs_axiom_status(climate_risk_is_financial_risk, holdable).
narrative_ontology:cs_axiom_grounding('1b86e328-c820-4611-a21a-b894c5c6f210', climate_risk_is_financial_risk, empirically_contingent).
narrative_ontology:cs_axiom('1b86e328-c820-4611-a21a-b894c5c6f210', foundational, article_11_tfeu_is_binding).
narrative_ontology:cs_axiom_status(article_11_tfeu_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('1b86e328-c820-4611-a21a-b894c5c6f210', article_11_tfeu_is_binding, deontological).
narrative_ontology:cs_reference_frame('1b86e328-c820-4611-a21a-b894c5c6f210', ecb_mandate_climate_neutrality_alignment).
narrative_ontology:cs_drift_state('1b86e328-c820-4611-a21a-b894c5c6f210', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1b86e328-c820-4611-a21a-b894c5c6f210', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, ecb).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_finance_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Central Bank, responsible for monetary policy in the Eurozone. Interprets its mandate to include climate risk integration, setting policies for asset purchases and collateral frameworks to support EU climate objectives. Actively enforces these policies.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb, agenda_setter,
    institutional, civilizational, analytical, global).

% EU institutions and bodies (e.g., European Commission, Parliament) responsible for designing and implementing climate policy. They benefit from the ECB's financial leverage being directed towards supporting the green transition, reinforcing their policy goals.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers, beneficiary,
    institutional, generational, analytical, continental).

% Financial institutions and investors focused on green and sustainable assets. They benefit from policies that favor green bonds and climate-aligned collateral, potentially increasing demand and reducing funding costs for their investments.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_finance_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Companies involved in the extraction, production, and distribution of fossil fuels. They face increased financing costs, reduced eligibility for ECB asset purchases, and higher collateral haircuts due to climate risk integration, leading to extraction.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% Industries with high carbon footprints (e.g., heavy manufacturing, aviation). They experience similar financial pressures to the fossil fuel sector, with their assets and operations becoming less attractive in ECB-influenced financial markets.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries, payer,
    organized, biographical, constrained, global).

% Academics and policy advisors who advocate for a strict interpretation of the ECB's mandate, focusing exclusively on price stability. They are excluded from the decision-making process regarding climate integration and would object to what they perceive as mission creep.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_economists, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align the ECB's monetary policy operations with the broader EU objective of combating climate change and facilitating a green transition, by integrating climate risk into financial stability assessments and asset eligibility criteria.
% TRANSFER_FUNCTION: Transfers financial advantage (lower funding costs, greater liquidity) to green assets and sectors, while imposing financial disadvantages (higher funding costs, reduced liquidity) on fossil fuel and carbon-intensive assets and sectors.
% ABSENT_VOICES: Orthodox economists and financial institutions heavily invested in carbon-intensive assets, who would argue against the ECB's expanded mandate and the financial penalties imposed. They are excluded from the interpretive process that led to this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the ECB would revert to a more narrow focus, removing climate considerations from its operations. This would immediately alter financial market incentives, potentially increasing investment in carbon-intensive sectors and undermining EU climate policy goals, requiring a significant rearrangement of policy and market dynamics.
% FOUNDING_PROBLEM: The systemic financial risks posed by climate change (physical risks, transition risks) and the need for all EU institutions to contribute to the Union's environmental objectives as per Article 11 TFEU.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by the ECB itself, the European Commission, and a broad consensus among climate scientists and financial regulators, citing ongoing climate impacts and the urgency of the green transition. This corroboration comes from outside the direct beneficiaries of the financial tilting.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the ECB's monetary policy with broader EU climate objectives (beneficiaries: ECB, EU climate policy makers, green finance sector) while simultaneously extracting from and suppressing carbon-intensive industries (victims: fossil fuel sector, carbon-intensive industries). The extractiveness (0.65) is substantial due to the financial penalties and reduced access to liquidity for non-compliant assets. Suppression (0.70) is high because the ECB's actions can significantly alter market conditions and investment flows, effectively 'tilting' portfolios away from high-carbon assets. The theater ratio (0.20) is relatively low, indicating that the climate integration efforts are genuinely operational, not merely performative, though some symbolic actions may exist. The rising extractiveness and suppression over the interval reflect the increasing operationalization of climate considerations in ECB policy.
 *
 * PERSPECTIVAL GAP:
 *   The ECB and EU climate policy makers perceive this as a necessary and legitimate coordination mechanism to address systemic climate risks and achieve treaty obligations. The fossil fuel sector and carbon-intensive industries, however, experience it as an extractive and suppressive force, leveraging monetary policy for industrial policy goals. Orthodox economists may view it as mission creep, distorting the ECB's primary price stability mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB is a primary beneficiary (d=0.0) as it gains legitimacy and aligns with EU policy. EU climate policy makers (d=0.0) benefit from the ECB's powerful financial tools supporting their agenda. The green finance sector (d=0.1) benefits from favorable collateral treatment and asset purchase programs. The fossil fuel sector (d=0.9) and carbon-intensive industries (d=0.8) are targets, facing increased costs and reduced access to finance. Orthodox economists (d=0.5) are analytical observers, not directly impacted by the extraction but concerned about the mandate's integrity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively evolving, not suffering from mandatrophy. The 'climate_incorporation' reading is a live, contested interpretation of the ECB's mandate, not an atrophied function. The classification as Tangled Rope prevents mislabeling it as a pure Snare by acknowledging its genuine coordination function (aligning monetary policy with climate goals) while also highlighting its asymmetric extraction and suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_incorporation_legitimacy,
    'Is the integration of climate risk into ECB operations a legitimate interpretation of Article 127 TFEU, or an overreach of its mandate?',
    'ECJ ruling on a challenge to ECB climate-related measures, or a formal amendment to the TFEU explicitly granting or denying this power.',
    'If deemed legitimate, the constraint''s stability and enforcement capacity increase; if deemed overreach, its legitimacy erodes, leading to higher resistance and potential reclassification to a Snare or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_incorporation_legitimacy, conceptual, 'Ambiguity regarding the legal basis for climate integration within the ECB''s mandate.').

omega_variable(
    climate_incorporation_vs_price_stability,
    'To what extent does the climate incorporation reading of Article 127 TFEU conflict with the orthodox_price_stability reading, and which takes precedence in practice?',
    'Observation of ECB policy decisions during periods of high inflation or conflicting objectives; explicit guidance from the Governing Council on trade-offs.',
    'If climate objectives are consistently subordinated to price stability, the effective extraction from carbon-intensive sectors is damped; if they are given equal or greater weight, extraction increases, and the orthodox_price_stability reading is further influenced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_incorporation_vs_price_stability, empirical, 'The practical hierarchy between climate objectives and price stability.').

omega_variable(
    novel_suppression_mechanism_efficacy,
    'How effective is portfolio tilting and collateral haircuts as a suppression mechanism for carbon-intensive industries, and what are the unintended consequences?',
    'Empirical studies on capital reallocation and investment decisions in affected sectors, and analysis of market distortions or ''greenwashing'' incentives.',
    'If highly effective, the suppression metric is accurate; if ineffective or counterproductive, the suppression is more theatrical, potentially shifting the constraint towards a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novel_suppression_mechanism_efficacy, empirical, 'Efficacy and side effects of climate-related financial suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__climate_incorporation, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.22).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__climate_incorporation, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_green_deal_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ECB's mandate (ecb_mandate_article_127). Its extractiveness and suppression differ significantly from the 'orthodox_price_stability' and 'expansive_secondary_objectives' readings, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
