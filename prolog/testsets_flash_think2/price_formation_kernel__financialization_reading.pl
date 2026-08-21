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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Housing Price Formation by Financialization
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story analyzes housing price formation through the lens
 *   of financialization, where prices are primarily driven by credit
 *   expansion, asset-price feedback loops, and demand for housing as a
 *   financial asset, rather than solely by objective scarcity or shelter
 *   utility. This reading posits that the financial sector and asset owners
 *   are primary beneficiaries, while indebted households, first-time buyers,
 *   and renters bear the costs of inflated prices and increased precarity.
 *   The constraint is claimed as a Tangled Rope, reflecting its dual function
 *   of coordinating capital flow while extracting wealth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.85).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.78).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation by Financialization").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '601e2cd6-0374-4c48-b296-be731f3582ff').
narrative_ontology:cs_kernel_codification('601e2cd6-0374-4c48-b296-be731f3582ff', formalized).
narrative_ontology:cs_authority_grounding('601e2cd6-0374-4c48-b296-be731f3582ff', extraction).
narrative_ontology:cs_interpretation_layer_present('601e2cd6-0374-4c48-b296-be731f3582ff').
narrative_ontology:cs_reading_relation('601e2cd6-0374-4c48-b296-be731f3582ff', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('601e2cd6-0374-4c48-b296-be731f3582ff', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('601e2cd6-0374-4c48-b296-be731f3582ff', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('601e2cd6-0374-4c48-b296-be731f3582ff', foundational, housing_as_financial_asset).
narrative_ontology:cs_axiom_status(housing_as_financial_asset, holdable).
narrative_ontology:cs_axiom_grounding('601e2cd6-0374-4c48-b296-be731f3582ff', housing_as_financial_asset, conventional).
narrative_ontology:cs_axiom('601e2cd6-0374-4c48-b296-be731f3582ff', foundational, credit_expansion_drives_asset_prices).
narrative_ontology:cs_axiom_status(credit_expansion_drives_asset_prices, holdable).
narrative_ontology:cs_axiom_grounding('601e2cd6-0374-4c48-b296-be731f3582ff', credit_expansion_drives_asset_prices, empirically_contingent).
narrative_ontology:cs_reference_frame('601e2cd6-0374-4c48-b296-be731f3582ff', post_deregulation_era).
narrative_ontology:cs_drift_state('601e2cd6-0374-4c48-b296-be731f3582ff', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('601e2cd6-0374-4c48-b296-be731f3582ff', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, asset_owners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, indebted_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from credit expansion, mortgage origination, securitization, and transaction volumes in housing markets. Actively lobbies for policies that support housing as an investment asset and maintains liquidity for mortgage markets. Its profitability is tied to asset price appreciation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold significant housing assets (primary residences, investment properties) whose value appreciates with financialization. They benefit from rising prices and often leverage existing equity for further investment or consumption. Their wealth is directly tied to the asset-price feedback loop.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, asset_owners, beneficiary,
    powerful, biographical, arbitrage, national).

% Bear the burden of high mortgage debt, often at variable rates, making them vulnerable to interest rate hikes and economic downturns. Their housing costs are decoupled from their income growth, leading to financial precarity. Exit means default or selling at a loss.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, indebted_households, payer,
    powerless, biographical, trapped, local).

% Struggle to enter the housing market due to rapidly escalating prices driven by financial demand rather than fundamental shelter value. They face high down payment requirements and affordability crises, often delaying homeownership or being permanently priced out.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyers, payer,
    powerless, immediate, constrained, local).

% Experience rising rental costs as landlords pass on increased property values, taxes, and financing costs. They have limited control over their housing situation and are often displaced by gentrification or redevelopment driven by financial investment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters, payer,
    powerless, immediate, constrained, local).

% Work to highlight the social costs of housing financialization, advocating for policies that prioritize housing as a human right over an investment vehicle. They collect data, organize communities, and lobby policymakers, but face significant institutional inertia.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_advocates, observer,
    organized, generational, analytical, national).

% Are tasked with maintaining financial stability and consumer protection, but often operate within a framework that implicitly or explicitly supports housing as an investment. They set lending standards, monitor financial institutions, and respond to crises, but their actions can also reinforce financialization.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, regulators, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels global capital into local housing markets, providing liquidity for property transactions and investment, and enabling homeowners to leverage their assets.
% TRANSFER_FUNCTION: Transfers wealth from indebted households and future generations (via inflated prices) to the financial sector (via interest and fees) and existing asset owners (via capital gains).
% ABSENT_VOICES: Future generations and those permanently priced out of stable housing are structurally excluded from the policy conversations that shape housing finance. They would advocate for policies that prioritize affordability and de-financialize housing.
% DISAPPEARANCE_RATIONALE: If housing were suddenly de-financialized and treated purely as shelter, the global financial system would experience a massive shock as trillions in asset value evaporated. Mortgage markets would seize up, investment funds would collapse, and the entire economy would undergo a profound, painful reorganization.
% FOUNDING_PROBLEM: The need for efficient capital allocation, investment opportunities for large pools of capital, and mechanisms for individuals to build wealth through property ownership.
% FOUNDING_PROBLEM_CORROBORATION: Financial industry reports and mainstream economic analyses often corroborate the benefits of housing as an investment vehicle. However, housing policy researchers and social justice organizations provide extensive counter-evidence, highlighting the negative social outcomes and arguing that the 'problem' solved is primarily for capital accumulation, not equitable housing provision.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the system systematically transfers wealth from those needing shelter to those holding capital, with prices often decoupled from underlying value. Suppression (0.78) is also high, as regulatory frameworks, lending standards, and market structures actively maintain the financialized status quo, limiting alternatives for non-financialized housing. Theater ratio is low (0.15) because the system is highly functional in its stated goal of facilitating capital investment, even if its social outcomes are contested. Accessibility collapse is moderate (0.65) as alternatives like public housing or cooperative models exist but are marginalized by the dominant financialized market.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the financial sector and asset owners, this system is a legitimate and efficient mechanism for wealth creation and capital allocation. From the perspective of indebted households and first-time buyers, it is an extractive system that denies access to affordable shelter and creates systemic risk. The engine's classification will highlight this divergence, showing a claimed 'Tangled Rope' that operates with high extraction for many seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial sector and asset owners are clear beneficiaries (low directionality), profiting from asset appreciation and transaction fees. Indebted households, first-time buyers, and renters are targets (high directionality), bearing the costs of inflated prices and debt service. Regulators occupy a complex position, often attempting to balance stability with affordability, but frequently reinforcing the financialized structure. Housing advocates act as analytical observers, seeking to expose the underlying mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is housing price formation primarily driven by financialization, or by other factors like natural scarcity, institutional rules, or land value?',
    'Comparative economic modeling across different regulatory regimes and historical periods, isolating the causal impact of financial instruments versus other factors.',
    'If financialization is confirmed as the dominant driver, this reading''s high extractiveness and tangled_rope classification are reinforced. If other factors are dominant, the constraint''s primary mechanism and classification would shift to a sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity over the primary driver of housing price formation.').

omega_variable(
    structural_vs_market_driven_prices,
    'To what extent are housing prices a ''natural'' outcome of supply/demand, versus a structurally engineered outcome of financial and regulatory systems?',
    'Analysis of price elasticity in response to changes in credit availability versus changes in physical housing supply, and the impact of macroprudential policies.',
    'If prices are largely structurally engineered, the suppression and extractiveness metrics are robust. If they are primarily market-driven, the constraint''s extractive nature is less about active enforcement and more about market failures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_market_driven_prices, empirical, 'Distinguishing structural vs. market-driven price dynamics.').

omega_variable(
    housing_as_right_vs_asset,
    'Is housing fundamentally a human right to shelter, or a legitimate investment asset?',
    'This is a preference-based question, resolvable only through societal value shifts and policy choices, not empirical data. Policy interventions (e.g., public housing, rent control, land value taxes) reflect a societal choice.',
    'A societal shift towards ''housing as a right'' would lead to policies that actively dismantle the financialization constraint, reducing its extractiveness and suppression. A continued emphasis on ''housing as an asset'' reinforces the current structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(housing_as_right_vs_asset, preference, 'Normative framing of housing''s primary purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__financialization_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__financialization_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__financialization_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__financialization_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__financialization_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__financialization_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__financialization_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__financialization_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__financialization_reading, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__financialization_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__financialization_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__financialization_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__financialization_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__financialization_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__financialization_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
