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
 *   human_readable: Housing Price Formation (Financialization Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the 'financialization reading' of
 *   housing price formation, where prices are primarily driven by credit
 *   expansion, asset-price feedback loops, and demand for housing as a
 *   financial asset. This reading posits a system that, while coordinating
 *   capital, also extracts heavily from non-asset-owning households. The
 *   claimed type is 'tangled_rope' because it performs a coordination
 *   function (capital allocation) but with significant asymmetric extraction
 *   and active enforcement of the financialized structure. The metrics
 *   reflect a system that has become increasingly extractive and suppressive
 *   over time, particularly since the 1980s.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.85).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.78).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '174a0d06-2631-408a-beb0-dc9158190021').
narrative_ontology:cs_kernel_codification('174a0d06-2631-408a-beb0-dc9158190021', distributed).
narrative_ontology:cs_authority_grounding('174a0d06-2631-408a-beb0-dc9158190021', extraction).
narrative_ontology:cs_interpretation_layer_present('174a0d06-2631-408a-beb0-dc9158190021').
narrative_ontology:cs_reading_relation('174a0d06-2631-408a-beb0-dc9158190021', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('174a0d06-2631-408a-beb0-dc9158190021', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('174a0d06-2631-408a-beb0-dc9158190021', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('174a0d06-2631-408a-beb0-dc9158190021', foundational, housing_as_financial_asset).
narrative_ontology:cs_axiom_status(housing_as_financial_asset, holdable).
narrative_ontology:cs_axiom_grounding('174a0d06-2631-408a-beb0-dc9158190021', housing_as_financial_asset, conventional).
narrative_ontology:cs_axiom('174a0d06-2631-408a-beb0-dc9158190021', foundational, credit_drives_price_formation).
narrative_ontology:cs_axiom_status(credit_drives_price_formation, holdable).
narrative_ontology:cs_axiom_grounding('174a0d06-2631-408a-beb0-dc9158190021', credit_drives_price_formation, empirically_contingent).
narrative_ontology:cs_reference_frame('174a0d06-2631-408a-beb0-dc9158190021', post_deregulation_financialized_market).
narrative_ontology:cs_drift_state('174a0d06-2631-408a-beb0-dc9158190021', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('174a0d06-2631-408a-beb0-dc9158190021', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, asset_owners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, indebted_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from credit expansion, mortgage origination fees, securitization, and transaction volume. Actively lobbies for policies that favor housing as a financial asset and resists regulations that would curb credit growth or speculative investment. Sets lending standards and influences monetary policy.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% See their wealth grow through rising asset prices, often leveraging existing equity to acquire more property. They benefit from the feedback loops that decouple housing prices from underlying economic fundamentals, and often resist policies that would deflate asset values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, asset_owners, beneficiary,
    powerful, biographical, mobile, national).

% Face increasingly unaffordable housing prices driven by financial speculation and credit availability, requiring larger down payments and higher debt burdens. Their options are to delay homeownership, move to less desirable areas, or take on significant financial risk.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_homebuyers, payer,
    powerless, immediate, constrained, local).

% Experience rising rents as landlords pass on increased property values and debt service costs. They are often trapped in a cycle where saving for a down payment becomes impossible due to high rental costs, perpetuating their exclusion from ownership.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters, payer,
    moderate, immediate, constrained, local).

% Are highly exposed to interest rate fluctuations and economic downturns, with their primary asset (their home) also being their largest liability. Their identity as homeowners is tied to the financial system, making exit from the debt-driven market difficult without significant loss.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, indebted_households, payer,
    moderate, biographical, identity_locked, national).

% Monitor housing market stability and inflation, often balancing mandates for economic growth with financial stability. Their policy tools (interest rates, quantitative easing) directly influence credit expansion and asset prices, but they face political pressure to maintain asset values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation towards housing, enabling large-scale investment and facilitating homeownership through mortgage markets. It also coordinates the valuation of housing as a store of wealth and a tradable asset.
% TRANSFER_FUNCTION: Transfers wealth from first-time homebuyers and renters (via high prices and rents) to the financial sector (via interest and fees) and existing asset owners (via capital gains), driven by credit expansion and asset-price inflation.
% ABSENT_VOICES: Future generations and those permanently priced out of housing are structurally excluded from the policy conversation, as their interests are not represented in the short-term political and economic cycles that drive financialization. They would advocate for housing as a human right, not a speculative asset.
% DISAPPEARANCE_RATIONALE: If the financialization of housing (credit expansion, asset-price feedback loops) vanished overnight, the housing market would undergo a massive revaluation. Prices would likely crash, the financial sector would face a crisis, and the economy would reorient away from housing as a primary investment vehicle, fundamentally altering wealth distribution and access to shelter.
% FOUNDING_PROBLEM: The need to facilitate widespread homeownership and provide a stable asset class for investment, while also enabling efficient capital allocation for housing development.
% FOUNDING_PROBLEM_CORROBORATION: The financial sector and many asset owners argue the system still serves its founding purpose by providing liquidity and investment opportunities. However, housing advocates, economists, and social scientists outside these benefiting parties corroborate that the system has drifted, with the problem of affordable housing now exacerbated by financialization, not solved by it.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) reflects the substantial portion of household income and wealth transferred to the financial sector and asset owners, decoupled from the actual cost of shelter provision. Suppression (0.78) is high due to the structural barriers to entry for first-time buyers, the lack of alternatives to the dominant mortgage market, and the active lobbying against regulations that would de-financialize housing. The theater ratio (0.20) indicates that while some coordination functions (e.g., mortgage liquidity) are real, a growing portion of the system's activity is performative maintenance of the extractive structure. The temporal measurements show a clear trend of increasing extractiveness and suppression, with a peak around the 2008 financial crisis, followed by a slight dip and then a renewed rise.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the financial sector, the system is an efficient mechanism for capital allocation and wealth creation. From the perspective of homebuyers and renters, it is an extractive system that denies access to a basic need. The engine's classification will highlight this divergence, showing a 'tangled_rope' for the system as a whole, but potentially different classifications for individual seats based on their directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The financial sector and asset owners are clear beneficiaries, with their power and exit options allowing them to shape the market to their advantage. First-time homebuyers, renters, and indebted households are the primary victims, facing high costs and limited exit options. Central banks act as observers, attempting to manage the system but often constrained by political and economic pressures to maintain asset values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted significantly. While initially aimed at facilitating homeownership, the financialization reading suggests it now primarily serves to generate profits for the financial sector and asset owners, with homeownership becoming a secondary, often unattainable, outcome for many. The persistence of the system is due to the concentrated benefits for powerful actors and the diffuse, but substantial, costs borne by a large, less organized population. Resolving mandatrophy would require re-aligning housing policy with shelter provision rather than wealth accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financial_vs_fundamental_drivers,
    'What proportion of housing price appreciation is attributable to financial factors (credit expansion, speculation) versus fundamental factors (population growth, construction costs, income growth)?',
    'Econometric studies that disaggregate price drivers, cross-country comparisons with varying financial regulations, and counterfactual modeling of housing markets without specific financial instruments.',
    'If financial factors dominate, the extractiveness and suppression metrics are robust. If fundamental factors are found to be primary, the constraint might be reclassified towards a ''rope'' or ''mountain'' (natural scarcity) for some seats, reducing the perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_vs_fundamental_drivers, empirical, 'Disentangling financial vs. fundamental drivers of housing prices.').

omega_variable(
    policy_vs_market_agency,
    'To what extent are the financialization dynamics a result of deliberate policy choices (e.g., tax incentives, deregulation) versus inherent market forces?',
    'Historical policy analysis, legislative intent studies, and comparative analysis of policy regimes in different jurisdictions. This would clarify the ''requires_active_enforcement'' aspect.',
    'If policy choices are primary, the constraint is more clearly a ''tangled_rope'' or ''snare'' (human-constructed extraction). If inherent market forces are dominant, it leans closer to a ''mountain'' (unavoidable economic reality), though still with extractive consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_vs_market_agency, conceptual, 'Attributing agency for housing financialization to policy or market forces.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (economic barriers, lack of alternatives) or internalized (belief in housing as an investment, fear of missing out)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., through policy changes), reclassify as partially internalized. Surveys of household financial decision-making.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — households carry the suppression with them after exit, making policy interventions more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in housing markets.').


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
narrative_ontology:measurement(pric_tr_t2008, price_formation_kernel__financialization_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(pric_tr_t2016, price_formation_kernel__financialization_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__financialization_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__financialization_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__financialization_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(pric_be_t2008, price_formation_kernel__financialization_reading, base_extractiveness, 2008, 0.9).
narrative_ontology:measurement(pric_be_t2016, price_formation_kernel__financialization_reading, base_extractiveness, 2016, 0.75).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__financialization_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__financialization_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__financialization_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(pric_su_t2008, price_formation_kernel__financialization_reading, suppression_requirement, 2008, 0.85).
narrative_ontology:measurement(pric_su_t2016, price_formation_kernel__financialization_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__financialization_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, wealth_inequality_escalation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, mortgage_backed_securities_market).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'price_formation_kernel'. This 'financialization_reading' emphasizes credit and asset-price feedback loops, distinct from 'naturalist_reading' (scarcity), 'institutional_reading' (zoning/tax), and 'georgist_reading' (land rent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
