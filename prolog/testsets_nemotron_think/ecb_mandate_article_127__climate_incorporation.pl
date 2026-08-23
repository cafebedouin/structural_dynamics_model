% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: ECB Mandate Climate Risk Integration Requirement
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   The ECB's climate incorporation reading of its Article 127 mandate treats
 *   climate risk integration into asset purchases and collateral frameworks
 *   as a treaty obligation under Article 11 TFEU (environmental integration
 *   clause). This reading structurally expands the constraint set to include
 *   climate transition sectors as beneficiaries and fossil fuel sectors as
 *   extraction targets via portfolio tilting and collateral haircuts. The
 *   constraint is actively enforced through the Eurosystem's operational
 *   frameworks (APP, PEPP, collateral eligibility). It presents as a tangled
 *   rope: genuine coordination function (pricing climate risk system-wide)
 *   combined with asymmetric extraction (carbon-intensive sectors bear costs,
 *   green sectors collect benefits).
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Primary agenda setter (institutional/analytical) — defines and enforces the constraint
 *   - climate_transition_sectors: Primary beneficiaries (organized/constrained) — receive preferential treatment
 *   - fossil_fuel_sector: Primary payers (powerful/constrained) — face systematic extraction via haircuts and exclusion
 *   - euro_area_citizens: Dual-positioned beneficiaries/payers (moderate/constrained) — long-term climate beneficiaries, potential monetary policy cost bearers
 *   - national_central_banks: Secondary agenda setters (institutional/constrained) — implement operationally
 *   - european_commission: Observer (institutional/analytical) — provides treaty legitimacy
 *   - climate_scientists: Observer (analytical/analytical) — supply factual premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.65).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.55).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate Climate Risk Integration Requirement").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7').
narrative_ontology:cs_kernel_codification('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', formalized).
narrative_ontology:cs_authority_grounding('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', lineage).
narrative_ontology:cs_interpretation_layer_present('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7').
narrative_ontology:cs_reading_relation('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', foundational, climate_risk_is_financial_stability_risk).
narrative_ontology:cs_axiom_status(climate_risk_is_financial_stability_risk, holdable).
narrative_ontology:cs_axiom_grounding('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', climate_risk_is_financial_stability_risk, empirically_contingent).
narrative_ontology:cs_axiom('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', foundational, article_11_tfeu_binds_ecb_monetary_policy).
narrative_ontology:cs_axiom_status(article_11_tfeu_binds_ecb_monetary_policy, holdable).
narrative_ontology:cs_axiom_grounding('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', article_11_tfeu_binds_ecb_monetary_policy, conventional).
narrative_ontology:cs_reference_frame('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', pre_2021_neutral_market_framework).
narrative_ontology:cs_drift_state('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', post_2021_climate_action_plan, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3dbf5ac3-a270-42f4-a1a8-91bc45e6a0f7', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_sectors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_finance_institutions).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, renewable_energy_projects).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, euro_area_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, euro_area_citizens).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, article_11_tfeu_environmental_integration).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, climate_risk_as_financial_stability_risk).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, eu_climate_neutrality_2050_target).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monetary policy and collateral frameworks for the euro area. Has mandated climate risk integration into asset purchase programmes (APP, PEPP) and collateral eligibility criteria. Justifies this as necessary for risk assessment and alignment with EU climate policy under Article 11 TFEU. Collects seigniorage and influences capital allocation but does not directly profit from the constraint.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, global).

% Renewable energy, energy efficiency, green bond issuers, and sustainable finance institutions. Benefit from preferential access to ECB liquidity, lower collateral haircuts, and inclusion in corporate bond purchase programmes. Their assets are treated as lower risk due to climate alignment, reducing funding costs. Exit options limited by dependence on EU taxonomy and regulatory recognition.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_sectors, beneficiary,
    organized, biographical, constrained, global).

% Banks, asset managers, and financial intermediaries specializing in green finance. Benefit from expanded market for green assets and ECB-backed demand. Also shape standards via EU taxonomy and disclosure regulations. Can redirect capital globally but are constrained by EU regulatory perimeter.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_finance_institutions, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, green_finance_institutions, agenda_setter).

% Oil, gas, coal, and carbon-intensive industrial companies. Face higher collateral haircuts, exclusion from ECB purchase programmes, and increased cost of capital due to climate risk classification. Their assets are systematically devalued in Eurosystem operations. Exit options constrained by stranded asset dynamics and long-lived capital stock; cannot easily switch sectors.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% Steel, cement, chemicals, aviation, and other hard-to-abate sectors. Face similar collateral and purchase programme disadvantages. Some receive transition funding but remain structurally disadvantaged in Eurosystem frameworks. Exit options limited by technological readiness and geographic concentration in EU.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries, payer,
    organized, biographical, constrained, continental).

% Benefit from long-term climate stability and financial stability if climate risks are properly managed. Bear costs if monetary policy effectiveness is reduced by narrowed collateral universe or if transition costs feed into inflation. No direct exit from euro area monetary policy; political exit only via treaty change.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, euro_area_citizens, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, euro_area_citizens, payer).

% Implement ECB collateral and purchase policies operationally. Have some discretion in national implementation but bound by Eurosystem guidelines. Their balance sheets reflect the constraint's portfolio composition. Cannot exit Eurosystem without leaving euro.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, national_central_banks, agenda_setter,
    institutional, generational, constrained, national).

% Guardian of EU treaties and climate legislation. Monitors ECB's compliance with Article 11 TFEU environmental integration obligation. Can initiate infringement proceedings but has no direct monetary policy authority. Provides political legitimacy for the constraint.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, european_commission, observer,
    institutional, generational, analytical, continental).

% Provide the physical risk scenarios and transition pathways that underpin ECB's climate stress tests and risk assessments. Their consensus (IPCC) frames the constraint's factual premises. No direct stake in monetary operations but their work is institutionalized in the constraint's architecture.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the financial system's risk assessment and capital allocation with the physical and transition risks of climate change, creating a common framework for pricing climate externalities across the euro area banking system and capital markets.
% TRANSFER_FUNCTION: Moves implicit subsidy from carbon-intensive assets (via higher haircuts, exclusion from purchase programmes, higher funding costs) to climate-aligned assets (via lower haircuts, purchase programme eligibility, lower funding costs). The transfer operates through the Eurosystem's collateral framework and asset purchase programmes.
% ABSENT_VOICES: Small and medium enterprises in carbon-intensive regions lacking transition pathways; developing country exporters of fossil fuels to EU; future generations not represented in current governance; monetary policy purists who argue climate mandate exceeds Treaty competence.
% DISAPPEARANCE_RATIONALE: If the climate integration mandate vanished overnight, ECB would revert to pre-2021 collateral and purchase frameworks. Carbon-intensive assets would regain full eligibility, green bonds would lose preferential treatment, and the financial system's climate risk pricing would lose its central anchor. EU climate policy would lose its monetary policy flank, altering the political economy of the Green Deal.
% FOUNDING_PROBLEM: The financial system systematically mispriced climate risks, creating systemic financial stability threats and misallocating capital away from the transition required by EU climate law. The ECB's traditional neutral market approach reproduced this blind spot.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: Network for Greening the Financial System (NGFS) central bank coalition; European Systemic Risk Board (ESRB) warnings on climate-related financial stability risks; IPCC physical science basis; European Court of Auditors reports on EU climate finance gaps. Not solely attested by beneficiaries.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects significant but not total transfer: fossil fuel sector loses access to cheap central bank funding but retains market access; green sectors gain but not all transition funding comes from ECB. Suppression (0.55) is moderate: enforcement is through portfolio composition rules, not outright bans; alternatives exist (private markets, non-EU funding) but are costlier. Theater ratio (0.22) is low: the risk management function is real and operationally substantive, though the treaty obligation interpretation is contested. Accessibility collapse (0.4) reflects that carbon-intensive firms can still fund themselves but at higher cost; alternatives are degraded but not eliminated. Resistance (0.5) captures ongoing legal challenges (German Constitutional Court), political pushback, and industry lobbying.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB seat (agenda_setter, analytical exit), the constraint appears as necessary risk management and treaty compliance — a coordination function. From fossil fuel sector seat (payer, constrained exit), it appears as targeted extraction via regulatory fiat — a snare-like experience. From climate transition sectors (beneficiary, constrained exit), it appears as overdue correction of market failure — a rope-like experience. The engine computes these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   ECB governing council sits at d≈0.1 (beneficiary of expanded mandate legitimacy, no direct cost). Climate transition sectors at d≈0.2 (net recipients of subsidy via preferential terms). Fossil fuel sector at d≈0.85 (systematic cost imposition, constrained exit). Carbon-intensive industries at d≈0.75 (similar but some transition funding access). Euro area citizens near symmetric d≈0.5 (diffuse long-term benefits vs. potential monetary policy costs). National central banks at d≈0.3 (implementation burden but institutional alignment). Commission and scientists at analytical (d≈0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by making the coordination function explicit (climate risk as financial stability risk) while acknowledging the extraction asymmetry. The founding problem (systemic climate risk mispricing) remains live per corroborated sources. The constraint is not a snare because the coordination function is genuine and independently validated (NGFS, ESRB, IPCC). It is not a pure rope because the extraction is asymmetric and enforcement is active. Tangled rope captures the hybrid nature: the mandate's climate reading coordinates risk management while extracting from laggards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_climate_incorporation,
    'How does the climate_incorporation reading structurally relate to the orthodox_price_stability and expansive_secondary_objectives readings of the same Article 127 kernel?',
    'Analyze whether the three readings foreclose, coexist with, or influence each other within the ECB''s legal framework. Track ECB communications, court rulings, and Treaty interpretation scholarship.',
    'If climate_incorporation forecloses orthodox_price_stability, the mandate is fundamentally transformed. If they coexist, the ECB operates a hybrid mandate. If influences, the climate reading reshapes the operational space of the others without eliminating them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_climate_incorporation, conceptual, 'Structural relationship between sibling readings of the ECB mandate kernel').

omega_variable(
    suppression_mechanism_portfolio_tilting,
    'Is the suppression exerted via portfolio tilting and collateral haircuts primarily structural (binding operational rules) or does it include internalized suppression (market anticipation of future tightening)?',
    'Compare pre-announcement vs post-announcement yield spreads for affected vs unaffected bonds; survey market participants on expectation formation; track voluntary green issuance vs mandatory classification.',
    'If largely internalized, the constraint''s effective suppression exceeds its formal rules — markets self-discipline beyond ECB action. If structural, suppression is bounded by operational parameters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_portfolio_tilting, empirical, 'Nature of suppression mechanism in climate-incorporated monetary policy').

omega_variable(
    extraction_boundary_climate_risk_vs_policy_choice,
    'How much of the measured extraction reflects genuine climate risk differentials vs. policy-induced redistribution via EU taxonomy and ECB eligibility criteria?',
    'Decompose yield spreads into physical risk premia, transition risk premia, and policy premia using counterfactual modelling (e.g., remove ECB purchase eligibility, observe price impact).',
    'If extraction is mostly policy-induced, the constraint leans snare; if mostly risk-based, it leans rope. The tangled rope classification depends on both being substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_boundary_climate_risk_vs_policy_choice, empirical, 'Decomposition of extraction into risk-based vs policy-based components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2021, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_climate_tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(ecb_climate_tr_t2024, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2024, 0.15).
narrative_ontology:measurement(ecb_climate_tr_t2027, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2027, 0.19).
narrative_ontology:measurement(ecb_climate_tr_t2030, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2030, 0.22).

% Extraction over time
narrative_ontology:measurement(ecb_climate_be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.35).
narrative_ontology:measurement(ecb_climate_be_t2024, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement(ecb_climate_be_t2027, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2027, 0.61).
narrative_ontology:measurement(ecb_climate_be_t2030, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ecb_climate_su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.3).
narrative_ontology:measurement(ecb_climate_su_t2024, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement(ecb_climate_su_t2027, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2027, 0.5).
narrative_ontology:measurement(ecb_climate_su_t2030, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__climate_incorporation, 0.15).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_taxonomy_regulation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_monetary_policy_operational_framework).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_green_deal_financing).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_ets_carbon_pricing).

% DUAL FORMULATION NOTE:
% Part of ecb_mandate_article_127 kernel family. climate_incorporation reading adds climate risk integration to asset purchases and collateral frameworks. Links to orthodox_price_stability (exclusive inflation focus) and expansive_secondary_objectives (discretionary balancing) as sibling readings. All three share the kernel but instantiate different constraints with different beneficiary/victim structures and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
