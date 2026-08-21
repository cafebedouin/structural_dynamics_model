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
 *   human_readable: ECB Mandate: Climate Risk Integration (Climate Incorporation Reading)
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint represents the 'climate incorporation' reading of the
 *   ECB's mandate under Article 127 TFEU, which interprets the treaty
 *   obligation (Article 11 TFEU) to integrate environmental protection into
 *   all EU policies as requiring the ECB to consider climate risk in its
 *   asset purchases and collateral frameworks. This reading introduces new
 *   beneficiaries (climate transition sectors, EU climate policy makers) and
 *   victims (fossil fuel companies, carbon-intensive industries) by tilting
 *   monetary policy towards green objectives. The constraint is classified as
 *   a Tangled Rope due to its genuine coordination function (financial
 *   stability, climate transition) coupled with asymmetric extraction from
 *   carbon-intensive sectors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.75).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate: Climate Risk Integration (Climate Incorporation Reading)").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '97824d25-e3f1-4993-8b8c-d6967cf6050a').
narrative_ontology:cs_kernel_codification('97824d25-e3f1-4993-8b8c-d6967cf6050a', fixed_text).
narrative_ontology:cs_authority_grounding('97824d25-e3f1-4993-8b8c-d6967cf6050a', lineage).
narrative_ontology:cs_interpretation_layer_present('97824d25-e3f1-4993-8b8c-d6967cf6050a').
narrative_ontology:cs_reading_relation('97824d25-e3f1-4993-8b8c-d6967cf6050a', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('97824d25-e3f1-4993-8b8c-d6967cf6050a', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('97824d25-e3f1-4993-8b8c-d6967cf6050a', foundational, environmental_integration_principle_operational).
narrative_ontology:cs_axiom_status(environmental_integration_principle_operational, holdable).
narrative_ontology:cs_axiom_grounding('97824d25-e3f1-4993-8b8c-d6967cf6050a', environmental_integration_principle_operational, deontological).
narrative_ontology:cs_axiom('97824d25-e3f1-4993-8b8c-d6967cf6050a', foundational, climate_risk_is_financial_stability_risk).
narrative_ontology:cs_axiom_status(climate_risk_is_financial_stability_risk, holdable).
narrative_ontology:cs_axiom_grounding('97824d25-e3f1-4993-8b8c-d6967cf6050a', climate_risk_is_financial_stability_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('97824d25-e3f1-4993-8b8c-d6967cf6050a', ecb_mandate_with_environmental_integration).
narrative_ontology:cs_drift_state('97824d25-e3f1-4993-8b8c-d6967cf6050a', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97824d25-e3f1-4993-8b8c-d6967cf6050a', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_transition_sectors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_companies).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the ECB's mandate, now incorporating climate risk into asset purchase and collateral frameworks. Faces pressure from both climate advocates and traditionalists. Could adjust policy within the interpreted mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from favorable collateral treatment and potentially increased demand for their assets in ECB purchase programs, easing their access to finance and supporting their growth. Their financial stability is enhanced by the ECB's recognition of climate risk.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, climate_transition_sectors, beneficiary,
    organized, generational, mobile, continental).

% Face increased collateral haircuts and reduced eligibility for ECB asset purchases due to climate risk assessments, raising their borrowing costs and potentially limiting access to central bank liquidity. Their business model is directly challenged by this policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_companies, payer,
    powerful, biographical, constrained, continental).

% Similar to fossil fuel companies, they bear the costs of climate risk integration through higher financing costs and reduced access to ECB facilities, pushing them towards decarbonization or financial distress.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_industries, payer,
    organized, biographical, constrained, continental).

% Benefit from the ECB's support for EU climate policy under Article 11 TFEU, as monetary policy tools align with broader environmental objectives, enhancing the effectiveness and legitimacy of EU climate action.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_policy_makers, beneficiary,
    institutional, generational, constrained, continental).

% Argue that the ECB should maintain an exclusive focus on price stability and avoid 'mission creep' into climate policy, which they see as outside the central bank's core mandate and expertise. Their arguments are heard but not decisive in this reading's implementation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates financial markets with EU climate policy objectives by integrating climate risk into monetary operations, aiming to stabilize the financial system against climate-related shocks and support the green transition.
% TRANSFER_FUNCTION: Transfers financial advantage (lower borrowing costs, better collateral treatment) to climate-aligned sectors and imposes costs (higher borrowing costs, reduced collateral eligibility) on carbon-intensive sectors, redirecting capital flows.
% ABSENT_VOICES: Financial institutions heavily invested in fossil fuels, and those who advocate for a strict interpretation of central bank independence, are marginalized in this reading. They would argue for a 'market neutral' approach to asset purchases.
% DISAPPEARANCE_RATIONALE: If the climate incorporation mandate vanished, the ECB would revert to a market-neutral stance, removing the financial incentives for green transition and the disincentives for carbon-intensive activities. Financial markets would re-price climate risk differently, potentially destabilizing the green transition and undermining EU climate goals.
% FOUNDING_PROBLEM: The ECB's mandate, while primarily focused on price stability, exists within the broader framework of EU treaties, which include environmental integration as a horizontal principle (Article 11 TFEU). The problem was how to reconcile these objectives, especially as climate change became a recognized systemic financial risk.
% FOUNDING_PROBLEM_CORROBORATION: EU legal scholars and climate scientists corroborate the necessity of integrating environmental concerns into all EU policies, including monetary policy, citing the systemic nature of climate risk and the explicit treaty obligations. This is attested by legal opinions and scientific consensus reports from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) is substantial because the policy actively re-prices assets based on climate risk, imposing real costs on certain sectors. Suppression (0.75) is high as the ECB's institutional power makes it difficult for affected entities to avoid these new financial conditions. Theater ratio (0.20) is low, indicating that the policy is genuinely being implemented, not merely performative. Accessibility collapse (0.60) is moderate, as alternatives for carbon-intensive industries are constrained but not entirely eliminated (e.g., private financing, decarbonization efforts). Resistance (0.70) is high, reflecting significant pushback from affected industries and some political factions.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council, from its agenda-setter seat, views this as a necessary evolution of its mandate to ensure financial stability in a changing climate. Climate transition sectors see it as a beneficial coordination mechanism. However, fossil fuel and carbon-intensive industries experience it as a direct, enforced extraction, limiting their access to finance and increasing their operational costs. This divergence is central to the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council (agenda_setter) is a beneficiary of this reading as it enhances its legitimacy and relevance in addressing systemic risks. Climate transition sectors and EU climate policy makers are clear beneficiaries. Fossil fuel and carbon-intensive industries are direct victims, facing increased financial burdens. Orthodox economists are excluded, as their arguments for a narrow mandate are not structurally incorporated into this reading's implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it represents an *expansion* of the mandate's interpretation rather than an atrophy of its original function. The contest is over the legitimate scope of the mandate, not its obsolescence. The classification as Tangled Rope prevents mislabeling it as pure extraction by acknowledging its genuine coordination function in addressing climate risk and supporting EU policy, while also recognizing the asymmetric costs imposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_secondary_objectives,
    'Is the integration of climate risk into monetary policy a legitimate exercise of the ECB''s secondary objective under Article 127 TFEU, or does it overstep into fiscal policy?',
    'ECJ ruling on the scope of the ECB''s mandate regarding climate policy, or a clear amendment to the TFEU clarifying the ECB''s role.',
    'If deemed illegitimate, the constraint would be reclassified as a Snare (pure extraction without legal basis) or a Piton (theatrical enforcement). If affirmed, its legitimacy as a Tangled Rope would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_secondary_objectives, conceptual, 'Ambiguity regarding the legal and constitutional boundaries of the ECB''s climate mandate.').

omega_variable(
    effectiveness_of_portfolio_tilting,
    'How effective is the ECB''s climate-related portfolio tilting in genuinely reducing systemic climate risk and fostering the green transition, versus merely shifting financial burdens?',
    'Empirical studies tracking capital reallocation, emissions reductions, and financial stability metrics over time, comparing outcomes with and without the policy.',
    'If ineffective, the constraint''s extractiveness might be re-evaluated as higher (pure burden without benefit), potentially shifting it towards a Snare. If highly effective, its coordination function would be more strongly vindicated, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_portfolio_tilting, empirical, 'Uncertainty about the real-world impact of the ECB''s climate-related monetary policy tools.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (ECB''s institutional power, legal framework) or internalized (companies preemptively decarbonizing due to perceived inevitability of policy)?',
    'Post-policy implementation surveys of corporate decision-makers on their motivations for decarbonization, distinguishing between direct regulatory pressure and anticipatory shifts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the policy''s reach broader. If purely structural, the suppression is limited to direct policy application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in response to ECB climate policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__climate_incorporation, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__climate_incorporation, theater_ratio, 15, 0.18).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__climate_incorporation, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
