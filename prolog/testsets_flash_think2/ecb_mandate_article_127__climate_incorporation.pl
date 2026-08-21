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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: ECB Mandate: Climate Risk Integration Reading
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint describes the reading of the ECB's mandate that requires
 *   the integration of climate-related financial risks into its monetary
 *   policy operations, driven by treaty obligations (Article 11 TFEU) and
 *   financial stability concerns. This reading actively shapes asset
 *   purchases and collateral frameworks, tilting financial flows towards
 *   climate-aligned sectors and away from fossil fuel-intensive industries.
 *   It is a contested interpretation of the ECB's primary objective of price
 *   stability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.65).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.75).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.65).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate: Climate Risk Integration Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, 'd4ccd032-827b-473f-b607-bd56199570aa').
narrative_ontology:cs_kernel_codification('d4ccd032-827b-473f-b607-bd56199570aa', fixed_text).
narrative_ontology:cs_authority_grounding('d4ccd032-827b-473f-b607-bd56199570aa', lineage).
narrative_ontology:cs_interpretation_layer_present('d4ccd032-827b-473f-b607-bd56199570aa').
narrative_ontology:cs_reading_relation('d4ccd032-827b-473f-b607-bd56199570aa', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('d4ccd032-827b-473f-b607-bd56199570aa', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('d4ccd032-827b-473f-b607-bd56199570aa', foundational, climate_risk_is_financial_risk).
narrative_ontology:cs_axiom_status(climate_risk_is_financial_risk, holdable).
narrative_ontology:cs_axiom_grounding('d4ccd032-827b-473f-b607-bd56199570aa', climate_risk_is_financial_risk, empirically_contingent).
narrative_ontology:cs_axiom('d4ccd032-827b-473f-b607-bd56199570aa', foundational, article_11_tfeu_binds_ecb).
narrative_ontology:cs_axiom_status(article_11_tfeu_binds_ecb, holdable).
narrative_ontology:cs_axiom_grounding('d4ccd032-827b-473f-b607-bd56199570aa', article_11_tfeu_binds_ecb, conventional).
narrative_ontology:cs_reference_frame('d4ccd032-827b-473f-b607-bd56199570aa', ecb_mandate_holistic_interpretation).
narrative_ontology:cs_drift_state('d4ccd032-827b-473f-b607-bd56199570aa', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d4ccd032-827b-473f-b607-bd56199570aa', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_aligned_sectors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_intensive_industries).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, orthodox_economists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the ECB's mandate, now actively integrating climate-related financial risks into monetary policy operations, including asset purchases and collateral frameworks. Justifies this as fulfilling treaty obligations and ensuring financial stability.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the ECB's monetary policy aligning with EU climate objectives, providing a powerful institutional lever for the green transition. They advocate for stronger climate integration across all EU institutions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers, beneficiary,
    institutional, generational, constrained, global).

% Face increased financing costs, collateral haircuts, and reduced access to ECB liquidity operations as their assets are re-rated for climate risk. They resist these changes, arguing they are outside the ECB's primary mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_intensive_industries, payer,
    powerful, biographical, constrained, regional).

% Benefit from potentially lower financing costs and improved access to ECB liquidity as their assets are favored in collateral frameworks and asset purchase programs. They support the ECB's climate integration efforts.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_aligned_sectors, beneficiary,
    organized, biographical, mobile, regional).

% Bear the intellectual and reputational cost of their preferred interpretation of the ECB's mandate being challenged. They argue that climate policy is outside the ECB's remit and risks 'greenwashing' monetary policy, potentially undermining its independence and price stability focus.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_economists, payer,
    analytical, biographical, analytical, universal).

% Benefit from a more financially stable and climate-resilient economy in the long term. They may also bear indirect costs if climate transition policies lead to short-term economic adjustments or higher prices in certain sectors.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_citizens, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__climate_incorporation, eu_citizens, payer).

% Are indirectly agenda-setters through their role in shaping EU treaties and climate policy. They exert political pressure on the ECB to align with broader EU objectives, including environmental protection.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_member_states, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__climate_incorporation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the powerful financial levers of the European Central Bank with the broader EU objective of environmental protection and climate transition, ensuring financial stability by internalizing climate-related risks that markets might otherwise ignore.
% TRANSFER_FUNCTION: Shifts financial support, risk exposure, and institutional legitimacy from carbon-intensive economic activities towards climate-aligned ones. It also transfers political capital and policy influence from purely monetary objectives to environmental goals.
% ABSENT_VOICES: Future generations, who will bear the full costs of climate inaction, are structurally absent from current policy debates but would strongly advocate for this integration. Small and medium-sized enterprises (SMEs) in carbon-intensive sectors, lacking the lobbying power of larger industries, also have limited voice in shaping these policies.
% DISAPPEARANCE_RATIONALE: If the ECB ceased integrating climate risk, financial markets would likely revert to underpricing these risks, leading to misallocation of capital, increased systemic financial instability from climate shocks, and a significant setback for EU climate policy goals. The entire financial landscape of the Eurozone would need to re-evaluate its risk models and investment strategies.
% FOUNDING_PROBLEM: The failure of financial markets to adequately price climate-related risks, leading to potential systemic financial instability and hindering the EU's ability to meet its climate targets, despite a treaty obligation (Article 11 TFEU) to integrate environmental protection into all Union policies.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, financial stability assessments by the European Systemic Risk Board (ESRB) and the Bank for International Settlements (BIS), and official communications from the European Commission and European Parliament consistently corroborate the urgency of climate risk and the necessity of its integration into financial policy.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.65) because the policy actively re-prices assets and shifts capital, imposing costs on carbon-intensive sectors. Suppression is high (0.75) as the ECB's institutional power enforces these new criteria, limiting alternatives for affected entities. Theater ratio is low (0.20) because the ECB is genuinely implementing these policies, moving beyond mere rhetoric. Resistance is high (0.70) due to significant opposition from affected industries and some economic factions. The claimed type is Tangled Rope because it serves a genuine coordination function (aligning finance with climate goals) but also involves asymmetric extraction from specific sectors.
 *
 * PERSPECTIVAL GAP:
 *   The ECB and EU climate policymakers view this as a necessary and legitimate evolution of the mandate, ensuring financial stability and fulfilling treaty obligations. Conversely, fossil fuel industries and orthodox economists perceive it as an overreach, an illegitimate expansion of the ECB's role, and a form of 'green central banking' that distorts markets and undermines independence. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council and EU climate policy makers are beneficiaries, gaining policy alignment and institutional leverage. Climate-transition-aligned sectors also benefit from favorable financing conditions. Fossil fuel-intensive industries are clear targets, facing increased costs and reduced access to finance. Orthodox economists are targets in a conceptual sense, as their preferred interpretation of the mandate is challenged. EU citizens are diffuse beneficiaries of climate action but may bear some indirect costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Snare, acknowledging the genuine coordination function of aligning financial stability with climate goals. It also prevents mislabeling it as a pure Rope, by highlighting the significant and actively enforced extraction from specific sectors. The 'live' status of the founding problem (climate risk and treaty obligation) indicates it is not a Piton, as its function is actively pursued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_legitimacy,
    'Is the integration of climate risk into monetary policy a legitimate interpretation of the ECB''s mandate under Article 127 and Article 11 TFEU, or an overreach beyond its primary objective?',
    'Legal rulings from the European Court of Justice, or a formal amendment to the EU treaties clarifying the ECB''s environmental mandate.',
    'If deemed an overreach, the constraint''s legitimacy would collapse, reducing its effective suppression and extractiveness. If affirmed, its institutional grounding would strengthen, potentially increasing its long-term impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_interpretation_legitimacy, conceptual, 'Ambiguity regarding the legal and constitutional legitimacy of the ECB''s climate mandate.').

omega_variable(
    policy_effectiveness_measurement,
    'To what extent is the ECB''s climate risk integration policy actually contributing to the decarbonization of the Eurozone economy and mitigating financial climate risk, versus being primarily symbolic?',
    'Empirical studies tracking capital reallocation, emissions reductions in affected sectors, and financial stability metrics specifically linked to climate risk over a multi-year period.',
    'If found to be largely symbolic, the constraint''s theater_ratio would increase, and its effective extractiveness might be lower than perceived. If highly effective, its coordination function would be strongly validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_effectiveness_measurement, empirical, 'Uncertainty about the real-world impact and effectiveness of the climate integration policy.').

omega_variable(
    suppression_mechanism_attribution,
    'How much of the observed suppression on fossil fuel industries is due to the ECB''s direct policy actions (structural) versus broader market shifts and investor preferences (market-driven)?',
    'Counterfactual economic modeling isolating the impact of ECB policy from other market and regulatory forces, or comparative analysis with economies where central banks do not pursue similar policies.',
    'If primarily market-driven, the ECB''s direct suppressive power is lower than measured. If primarily structural, the ECB''s role in shaping market outcomes is more significant, amplifying its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_attribution, empirical, 'Structural vs. market-driven components of suppression on carbon-intensive assets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 2021, 2031).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(ecb__tr_t2023, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2023, 0.25).
narrative_ontology:measurement(ecb__tr_t2025, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2025, 0.2).
narrative_ontology:measurement(ecb__tr_t2027, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2027, 0.18).
narrative_ontology:measurement(ecb__tr_t2029, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2029, 0.16).
narrative_ontology:measurement(ecb__tr_t2031, ecb_mandate_article_127__climate_incorporation, theater_ratio, 2031, 0.15).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(ecb__be_t2023, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2023, 0.59).
narrative_ontology:measurement(ecb__be_t2025, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(ecb__be_t2027, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2027, 0.65).
narrative_ontology:measurement(ecb__be_t2029, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2029, 0.67).
narrative_ontology:measurement(ecb__be_t2031, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 2031, 0.69).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(ecb__su_t2023, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2023, 0.69).
narrative_ontology:measurement(ecb__su_t2025, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement(ecb__su_t2027, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2027, 0.75).
narrative_ontology:measurement(ecb__su_t2029, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2029, 0.77).
narrative_ontology:measurement(ecb__su_t2031, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 2031, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_climate_policy).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_financial_stability_framework).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ECB's mandate (ecb_mandate_article_127). It is linked to sibling readings that represent alternative interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
