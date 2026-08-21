% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__financialization_reading, []).

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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Housing Price Formation as Financial Asset
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the price formation mechanism in housing
 *   markets as primarily driven by financialization: credit expansion,
 *   asset-price feedback loops, and the treatment of housing as a financial
 *   asset. This is one specific reading of the broader
 *   'price_formation_kernel', emphasizing the role of finance over other
 *   factors like natural scarcity, institutional rules, or land value. The
 *   constraint is claimed as a Tangled Rope, reflecting its dual function of
 *   coordinating capital allocation while extracting significant value from
 *   households.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.82).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.78).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation as Financial Asset").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'd6ed0cae-acad-4d9b-8e8e-a4126365e075').
narrative_ontology:cs_kernel_codification('d6ed0cae-acad-4d9b-8e8e-a4126365e075', formalized).
narrative_ontology:cs_authority_grounding('d6ed0cae-acad-4d9b-8e8e-a4126365e075', extraction).
narrative_ontology:cs_interpretation_layer_present('d6ed0cae-acad-4d9b-8e8e-a4126365e075').
narrative_ontology:cs_reading_relation('d6ed0cae-acad-4d9b-8e8e-a4126365e075', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('d6ed0cae-acad-4d9b-8e8e-a4126365e075', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('d6ed0cae-acad-4d9b-8e8e-a4126365e075', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('d6ed0cae-acad-4d9b-8e8e-a4126365e075', foundational, housing_is_a_financial_asset).
narrative_ontology:cs_axiom_status(housing_is_a_financial_asset, holdable).
narrative_ontology:cs_axiom_grounding('d6ed0cae-acad-4d9b-8e8e-a4126365e075', housing_is_a_financial_asset, conventional).
narrative_ontology:cs_axiom('d6ed0cae-acad-4d9b-8e8e-a4126365e075', foundational, credit_expansion_drives_asset_prices).
narrative_ontology:cs_axiom_status(credit_expansion_drives_asset_prices, holdable).
narrative_ontology:cs_axiom_grounding('d6ed0cae-acad-4d9b-8e8e-a4126365e075', credit_expansion_drives_asset_prices, empirically_contingent).
narrative_ontology:cs_reference_frame('d6ed0cae-acad-4d9b-8e8e-a4126365e075', unfettered_capital_mobility).
narrative_ontology:cs_drift_state('d6ed0cae-acad-4d9b-8e8e-a4126365e075', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d6ed0cae-acad-4d9b-8e8e-a4126365e075', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, asset_owners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from credit expansion, mortgage origination, securitization, and transaction volumes in housing markets. Actively lobbies for policies that support housing as an investment vehicle and ensures liquidity for asset trading. Sets lending standards and influences regulatory frameworks.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold housing as an investment, benefiting from asset price appreciation driven by credit availability and speculative demand. Can leverage existing assets for further investment or consumption, reinforcing wealth accumulation. Their exit options are robust, allowing them to divest or reallocate capital with relative ease.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, asset_owners, beneficiary,
    powerful, generational, mobile, global).

% Bear the costs of high housing prices through mortgage debt service or rent, often consuming a large portion of their income. They are exposed to market volatility and crash risk, with limited ability to influence the underlying price formation mechanisms. Their exit options are constrained by financial obligations and lack of affordable alternatives.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, households, payer,
    moderate, biographical, constrained, local).

% Face significant barriers to entry into homeownership due to rapidly escalating prices, often decoupled from local income growth. They are forced into long-term debt or perpetual renting, with their aspirations for stable housing increasingly out of reach. Their options are severely limited, often leading to being trapped in unaffordable rental markets or leaving their communities.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyers, payer,
    powerless, immediate, trapped, local).

% Argue for housing as a human right and a social good, rather than primarily a financial asset. They advocate for policies that would de-financialize housing, such as stricter lending regulations, public housing initiatives, and land value taxes. While organized, their influence is often marginalized by the dominant financial interests.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_advocates, excluded,
    organized, generational, constrained, national).

% Influence credit availability and interest rates, which directly impact housing affordability and asset prices. They aim for financial stability but often face a dilemma between controlling inflation and supporting economic growth, sometimes inadvertently fueling asset bubbles. Their analytical position allows them to observe the system's dynamics but their policy tools have broad, often unintended, consequences.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of capital towards housing as an investment, facilitating liquidity in real estate markets and enabling wealth transfer through asset appreciation.
% TRANSFER_FUNCTION: Transfers wealth from labor income and new entrants (first-time buyers) to existing asset owners and the financial sector, through rising housing prices, mortgage interest, and transaction fees.
% ABSENT_VOICES: Those advocating for housing as a non-financialized social good, including housing rights organizations, urban planners focused on affordability, and communities displaced by speculative investment. Their perspectives are systematically excluded from the dominant policy discourse that prioritizes financial market stability and asset growth.
% DISAPPEARANCE_RATIONALE: If housing were suddenly de-financialized and credit expansion ceased to drive prices, the global financial system would undergo a massive revaluation, asset bubbles would burst, and the wealth distribution would dramatically shift. Housing prices would likely fall to levels closer to construction costs and local incomes, fundamentally altering investment strategies and household balance sheets.
% FOUNDING_PROBLEM: The problem of efficiently allocating capital, providing liquidity for real estate transactions, and enabling wealth creation through investment in tangible assets.
% FOUNDING_PROBLEM_CORROBORATION: The financial sector and asset owners attest that the system efficiently allocates capital and provides investment opportunities, thus the problem is live. Housing advocates and some economists, however, argue that while capital allocation occurs, it has created new problems (affordability crisis, systemic risk) that supersede the original 'problem' and that the current arrangement is primarily extractive; their analysis, often from outside the benefiting parties, corroborates the 'dead' or 'transformed' status of the original problem.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__financialization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__financialization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the system systematically transfers wealth from those needing shelter to those holding assets and providing finance, with prices often decoupled from fundamental value. Suppression (0.78) is high due to the structural barriers to entry for first-time buyers and the limited alternatives to participating in the financialized market. The system is actively enforced through financial regulations, lending standards, and the legal framework of property rights. Theater ratio is low (0.20) as the financial mechanisms are highly functional in their extractive purpose, not merely performative. Accessibility collapse is high (0.85) because non-market alternatives to housing are severely limited or non-existent for most, forcing participation in the financialized market. Resistance (0.60) is moderate, reflecting ongoing advocacy and policy debates, but not a systemic challenge to the core mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the financial sector and asset owners, this system is an efficient mechanism for capital allocation and wealth creation. From the perspective of households and first-time buyers, it is an extractive system that makes basic shelter unaffordable and creates systemic risk. The engine's classification will highlight this divergence, showing a Tangled Rope for the system as a whole, but potentially a Snare for those trapped at the bottom.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial sector and asset owners are clear beneficiaries (low directionality), profiting from the system's operation. Households and first-time buyers are the primary targets (high directionality), bearing the costs of inflated prices and debt. Central banks, while aiming for stability, often act as agenda-setters whose policies enable the financialization process. Housing advocates are structurally excluded from the core mechanisms of price formation, despite their organized efforts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financial_vs_fundamental_value,
    'To what extent are housing prices driven by financial factors (credit, speculation) versus fundamental factors (construction costs, income, scarcity)?',
    'Empirical studies comparing price-to-income ratios, price-to-rent ratios, and credit growth over time, especially across different regulatory regimes or during periods of financial deleveraging.',
    'If financial factors are dominant, the constraint''s extractiveness and suppression are higher, supporting the Tangled Rope/Snare classification. If fundamental factors are primary, the constraint leans more towards a Rope or even Mountain (reflecting genuine scarcity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_vs_fundamental_value, empirical, 'Distinguishing the drivers of housing price formation.').

omega_variable(
    regulatory_capture_extent,
    'To what degree do financial sector interests influence housing and financial regulations, effectively shaping the ''rules of the game'' to their benefit?',
    'Analysis of lobbying expenditures, revolving door appointments, and the legislative history of financial and housing policies, particularly those related to mortgage markets and asset securitization.',
    'Higher regulatory capture would increase the effective extractiveness and suppression, reinforcing the Snare-like qualities of the constraint for households, as the ''enforcement'' is effectively self-serving. Lower capture would suggest a more balanced, albeit still extractive, Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Assessing the influence of financial interests on housing market regulation.').

omega_variable(
    housing_as_right_vs_asset_framing,
    'Is housing fundamentally a human right or a financial asset?',
    'This is a conceptual and preference-based question, not empirically resolvable. Resolution would involve a societal shift in normative priorities, potentially codified through new legal frameworks or international agreements.',
    'If housing is framed primarily as a right, the current financialized price formation mechanism would be reclassified as a Snare due to its violation of fundamental access. If framed as an asset, the current classification as a Tangled Rope (coordination + extraction) remains more consistent with the prevailing economic paradigm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(housing_as_right_vs_asset_framing, preference, 'Conceptual framing of housing''s primary purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__financialization_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__financialization_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__financialization_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__financialization_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__financialization_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__financialization_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__financialization_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__financialization_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__financialization_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__financialization_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__financialization_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__financialization_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__financialization_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__financialization_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__financialization_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, wealth_inequality).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, systemic_financial_risk).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'price_formation_kernel', focusing on the role of financialization. Other readings (naturalist, institutional, georgist) offer alternative explanations for housing price formation, each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
