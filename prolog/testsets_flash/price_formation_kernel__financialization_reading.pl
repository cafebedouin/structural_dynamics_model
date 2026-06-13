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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Housing Price Formation (Financialization Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes housing price formation as primarily driven by
 *   financial mechanisms: credit expansion, asset-price feedback loops, and
 *   the treatment of housing as a financial asset rather than solely shelter.
 *   This 'financialization reading' of the price formation kernel highlights
 *   how financial sector activity and policy choices amplify price volatility
 *   and extraction, decoupling housing costs from fundamental supply/demand
 *   for shelter. It is a Tangled Rope because it coordinates capital
 *   allocation and investment while extracting wealth from non-asset-owning
 *   households.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.85).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.75).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '593a7328-79c6-4e89-b796-01aa27084bf8').
narrative_ontology:cs_kernel_codification('593a7328-79c6-4e89-b796-01aa27084bf8', implicit).
narrative_ontology:cs_authority_grounding('593a7328-79c6-4e89-b796-01aa27084bf8', extraction).
narrative_ontology:cs_interpretation_layer_present('593a7328-79c6-4e89-b796-01aa27084bf8').
narrative_ontology:cs_reading_relation('593a7328-79c6-4e89-b796-01aa27084bf8', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('593a7328-79c6-4e89-b796-01aa27084bf8', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('593a7328-79c6-4e89-b796-01aa27084bf8', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('593a7328-79c6-4e89-b796-01aa27084bf8', foundational, housing_as_financial_asset).
narrative_ontology:cs_axiom_status(housing_as_financial_asset, holdable).
narrative_ontology:cs_axiom_grounding('593a7328-79c6-4e89-b796-01aa27084bf8', housing_as_financial_asset, conventional).
narrative_ontology:cs_axiom('593a7328-79c6-4e89-b796-01aa27084bf8', foundational, credit_expansion_drives_asset_prices).
narrative_ontology:cs_axiom_status(credit_expansion_drives_asset_prices, holdable).
narrative_ontology:cs_axiom_grounding('593a7328-79c6-4e89-b796-01aa27084bf8', credit_expansion_drives_asset_prices, empirically_contingent).
narrative_ontology:cs_reference_frame('593a7328-79c6-4e89-b796-01aa27084bf8', unfettered_capital_flow_into_housing).
narrative_ontology:cs_drift_state('593a7328-79c6-4e89-b796-01aa27084bf8', contemporary_post_2008_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('593a7328-79c6-4e89-b796-01aa27084bf8', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_institutions).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, asset_owners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, indebted_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, governments_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drive credit expansion, securitize mortgages, and create financial products that treat housing as an investment asset. They profit from transaction fees, interest payments, and market volatility. They actively lobby for policies that support this financialized model.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from rising asset prices, leveraging their existing equity to acquire more property or extract value. They often advocate for policies that protect property values and limit new supply.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, asset_owners, beneficiary,
    powerful, biographical, mobile, national).

% Face escalating prices driven by financial speculation and credit availability, making homeownership increasingly inaccessible. They must take on large debts or remain renters, with limited options to escape the market dynamics.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_homebuyers, payer,
    powerless, biographical, constrained, local).

% Experience rising rents as housing costs increase across the board, driven by investor demand and landlord leverage. They have limited bargaining power and often face displacement.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters, payer,
    powerless, immediate, constrained, local).

% Are vulnerable to interest rate changes and economic downturns, with their primary asset (their home) tied to volatile market dynamics. Their identity as homeowners is often fused with their financial stability, making exit from the debt structure unthinkable.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, indebted_households, payer,
    moderate, biographical, identity_locked, national).

% Set monetary policy (interest rates, credit availability) and regulatory frameworks that enable or constrain financialization. They benefit from economic activity but bear the social and political costs of housing crises, often intervening with bailouts or stimulus measures.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, governments_central_banks, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, governments_central_banks, payer).

% Argue for policies that prioritize housing as a human right and shelter, rather than an investment asset. They are often excluded from the core decision-making processes that shape financial and housing policy.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, financial_institutions).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of capital into housing markets, enabling large-scale investment and the creation of complex financial instruments around housing assets.
% TRANSFER_FUNCTION: Transfers wealth from households (via debt service, rising prices, and rent) to financial institutions and existing asset owners (via interest, fees, and asset appreciation).
% ABSENT_VOICES: Housing advocates, community organizers, and economists critical of financialization are often marginalized in policy debates, which are dominated by financial industry lobbyists and mainstream economic perspectives that normalize these dynamics.
% DISAPPEARANCE_RATIONALE: If the financialization mechanisms vanished overnight, housing prices would likely crash, the financial system would face severe instability, and the entire structure of housing investment and ownership would need to be fundamentally rethought. The economy would undergo a massive, painful reorganization.
% FOUNDING_PROBLEM: The problem of efficiently allocating capital for housing development and investment, and providing mechanisms for wealth accumulation through property ownership.
% FOUNDING_PROBLEM_CORROBORATION: Financial institutions and asset owners claim the problem is still live, requiring robust financial markets. Housing advocates and critical economists, corroborated by historical data on housing bubbles and crises, argue that the original problem has been superseded by a new problem of financial extraction, and the current system exacerbates rather than solves housing needs for many.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).

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
 *   Extractiveness is high (0.85) because the financial sector captures significant gains from transaction volumes, interest payments, and asset appreciation, while households bear the risks of debt and market crashes. Suppression (0.75) is high due to the structural power of financial institutions and the limited alternatives for households to access housing outside the financialized market. Theater ratio is low (0.20) as the mechanisms are genuinely functional for capital accumulation, even if the stated social benefits (e.g., 'wealth creation for all') are often performative. The increasing extractiveness and suppression over time reflect the deepening financialization of housing markets.
 *
 * PERSPECTIVAL GAP:
 *   Financial institutions and asset owners experience this as a functional, wealth-generating system (beneficiary seat), while first-time homebuyers, renters, and indebted households experience it as an extractive, exclusionary system (payer/victim seats). The engine's per-seat classification will reflect this divergence, with beneficiaries computing as Rope-like and victims as Snare-like.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial institutions and asset owners are primary beneficiaries (d near 0.0) as they profit from credit expansion, transaction fees, and asset appreciation. First-time homebuyers, renters, and indebted households are victims/targets (d near 1.0) as they face escalating prices, high debt burdens, and increased precarity. Governments and central banks are complex: they may benefit from economic activity and tax revenue but also bear the social costs of housing crises, leading to a more symmetric directionality (d near 0.5) or even a target role if they are forced to bail out the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate might have been to facilitate homeownership and capital investment. However, under the financialization reading, this mandate has atrophied into a mechanism for wealth transfer and asset speculation. The classification as a Tangled Rope prevents mislabeling it as a pure coordination mechanism (Rope) by highlighting the asymmetric extraction, or as a purely natural process (Mountain) by emphasizing its constructed, policy-driven nature. The persistence is due to the concentrated benefits for the financial sector and asset owners, who actively maintain the system, rather than a genuine collective benefit for all participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a fundamental feature of financialized housing markets, or is it an artifact of a specific analytical reading?',
    'Comparative analysis of housing markets under different regulatory and financial regimes; if price dynamics shift with regime, it supports the ''reading'' interpretation.',
    'If primarily a reading, the constraint is more amenable to policy intervention; if a fundamental feature, interventions may be less effective or have unintended consequences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''financialization_reading'' of the ''price_formation_kernel''.').

omega_variable(
    naturalist_vs_financialization,
    'Does housing price formation primarily reflect natural scarcity and preferences (naturalist_reading), or is it driven by financial mechanisms (financialization_reading)?',
    'Empirical studies disentangling the contribution of credit expansion and asset-price feedback loops from demographic and supply-side factors.',
    'If the naturalist_reading is dominant, the constraint is closer to a Mountain; if the financialization_reading is dominant, it is a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalist_vs_financialization, empirical, 'Contest between financialization and naturalist readings of price formation.').

omega_variable(
    institutional_vs_financialization,
    'To what extent are financialization dynamics themselves products of specific institutional choices (institutional_reading vs. financialization_reading)?',
    'Historical analysis of policy changes (zoning, lending standards, tax) and their impact on financial sector behavior and housing prices.',
    'If institutional factors are primary, policy levers are more direct; if financialization is an emergent property, interventions are more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_financialization, empirical, 'Relationship between institutional settings and financialization dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__financialization_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__financialization_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__financialization_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__financialization_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__financialization_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__financialization_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__financialization_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__financialization_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__financialization_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, wealth_inequality_amplification).

% DUAL FORMULATION NOTE:
% This constraint is the 'financialization_reading' of the 'price_formation_kernel', which also has 'naturalist_reading', 'institutional_reading', and 'georgist_reading' siblings. Each reading represents a distinct structural claim about housing price formation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
