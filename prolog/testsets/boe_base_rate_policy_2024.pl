% ============================================================================
% CONSTRAINT STORY: boe_base_rate_policy_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boe_base_rate_policy_2024, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: boe_base_rate_policy_2024
 *   human_readable: Bank of England's 5.25% Base Interest Rate Policy (2024)
 *   domain: economic/monetary_policy
 *
 * SUMMARY:
 *   The Bank of England's 5.25% base rate policy in 2024 exemplifies a
 *   tangled rope constraint: a hybrid coordination-extraction mechanism that
 *   solves a genuine inflation control problem while asymmetrically
 *   distributing costs and benefits. The MPC maintains rates at a 16-year
 *   high to suppress demand and bring inflation back to the 2% target. This
 *   achieves coordination benefit for savers, financial institutions, and
 *   those benefiting from price stability. Simultaneously, it extracts from
 *   mortgage borrowers, young first-time buyers, and small businesses through
 *   elevated debt service costs. The constraint exhibits all six
 *   classification types from different structural positions, revealing the
 *   indexical nature of how economic policy is experienced. The theater ratio
 *   (0.58) reflects that significant MPC communication is ritual performance
 *   — the monthly meeting cycle, forward guidance narratives, and
 *   consensus-building theater — rather than adaptive problem-solving. Yet
 *   the coordination component is genuine: controlling inflation does require
 *   some demand suppression, and the rate tool is a real (if blunt)
 *   mechanism. The constraint's extractiveness (0.52) has risen from 0.35
 *   over 12 months as the accumulated debt burden and housing market impact
 *   accumulate. The suppression component (0.68) is high: borrowers cannot
 *   reduce their exposure, cannot exit the mortgage market, and cannot
 *   arbitrage their way out of rising costs.
 *
 * KEY AGENTS:
 *   - Mortgage Borrowers: Primary victims (powerless/trapped) — bear extraction through higher monthly payments with no exit option
 *   - First-Time Buyers: Primary victims (powerless/trapped) — generationally excluded from wealth-building through housing market; cannot arbitrage into international real estate markets efficiently
 *   - Small Business Owners: Secondary victims (moderate/constrained) — face higher borrowing costs but some can reduce leverage or pass costs to customers; constrained exit
 *   - Savers & Fixed-Income Holders: Primary beneficiaries (institutional/arbitrage) — benefit from elevated deposit rates and bond yields; arbitrage into alternative instruments if UK rates decline
 *   - Financial Institutions: Primary beneficiaries (institutional/arbitrage) — capture margin expansion and deposit inflows; can arbitrage globally across currency zones
 *   - Inflation Control Coalition: Organized actors (organized/constrained) — central bank, government, businesses planning post-inflation recovery; see constraint as temporary with public sunset
 *   - Global Capital Investors: Secondary beneficiaries (powerful/mobile) — arbitrage UK yield advantage; mobile across currency zones; constrained by sterling depreciation risk
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing monetary policy framework as inherent law rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boe_base_rate_policy_2024, 0.52).
domain_priors:suppression_score(boe_base_rate_policy_2024, 0.68).
domain_priors:theater_ratio(boe_base_rate_policy_2024, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, extractiveness, 0.52).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(boe_base_rate_policy_2024, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boe_base_rate_policy_2024, tangled_rope).
narrative_ontology:human_readable(boe_base_rate_policy_2024, "Bank of England's 5.25% Base Interest Rate Policy (2024)").
narrative_ontology:topic_domain(boe_base_rate_policy_2024, "economic/monetary_policy").

domain_priors:requires_active_enforcement(boe_base_rate_policy_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, savers_fixed_income_holders).
narrative_ontology:constraint_beneficiary(boe_base_rate_policy_2024, financial_institutions).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, mortgage_borrowers).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, business_debt_holders).
narrative_ontology:constraint_victim(boe_base_rate_policy_2024, young_first_time_buyers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MORTGAGE BORROWER (SNARE) — Trapped in high-cost debt with no exit. Monthly payments rise sharply; refinancing is not available at previous rates. Cannot reduce exposure to the constraint. Bears maximum extraction cost with zero alternatives.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-TIME BUYER (SNARE) — Structurally excluded from housing market by high borrowing costs. Cannot acquire property; cannot exit the rental market. Generational impact — cohort loses wealth-building window. Pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNER (TANGLED ROPE) — Faces higher borrowing costs for expansion (extraction), but also benefits from coordinated price stability and reduced inflation risk (coordination). Constrained exit — some can reduce leverage, but most cannot exit the constraint entirely. Mixed experience: some gain from inflation control, others bear costs of contraction.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL INSTITUTION (ROPE) — Enjoys margin expansion and deposit inflows. High base rate increases spread between borrowing and lending costs. Can arbitrage between UK and international rates. Net beneficiary experiencing the constraint as coordination mechanism for their funding environment.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FIXED-INCOME SAVER (ROPE) — Benefits from elevated savings rates and bond yields. Primary beneficiary of the policy. Coordination achieved between saver preferences and policy objective. Low suppression of exit options — can move capital to different instruments or geographies if rates fall.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INFLATION CONTROL COALITION (SCAFFOLD) — Sees the high-rate regime as temporary, necessary transition mechanism with sunset: once inflation falls to 2% target, rates will normalize downward. Organized actors (central bank, government, businesses planning post-inflation recovery) experience the constraint as a coordinated effort with explicit endpoint. The sunset is publicly declared (policy target: inflation back to 2%). Theater reflects the ritual of monthly MPC meetings with predetermined messaging around inflation targets.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: GLOBAL CAPITAL INVESTOR (TANGLED ROPE) — Mobile across currency zones; can arbitrage UK rate advantage against alternatives. Benefits from higher UK yields (coordination coordination). Simultaneously faces extraction if sterling depreciates or capital controls emerge. Exit is mobile but not costless. Experiences mixed extraction and coordination at the global institutional level.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a macroeconomic equilibrium perspective, the base rate is an immutable feature of modern capitalism: central banks must have a policy rate, and interest rate spreads are inherent to credit markets. No economy operates without some base rate regime. However, this naturalizes what is actually a contingent policy choice — the rate level (5.25% vs 2% vs 7%) is not a natural law, and monetary policy frameworks are institutional constructs subject to reform.
constraint_indexing:constraint_classification(boe_base_rate_policy_2024, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boe_base_rate_policy_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boe_base_rate_policy_2024, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boe_base_rate_policy_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(boe_base_rate_policy_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The base rate increase transfers wealth from borrowers to savers through the spread mechanism. Over 12 months, the accumulated impact on household debt service and business financing costs reaches 0.52, up from 0.35 at policy inception. The rise reflects that extraction is cumulative — compounding interest costs and mortgage payment shocks. Suppression (0.68): High. Mortgage borrowers and first-time buyers have zero exit options from the constraint — they cannot refinance at old rates, cannot move to alternative credit markets, and cannot escape UK inflation control without migration. Businesses have slightly more exit through deleveraging, but most cannot reduce borrowing below operational needs. Theater ratio (0.58): Moderate-high. The MPC's monthly meetings, forward guidance communications, and inflation-targeting narrative constitute theater — the public ritual of deliberation obscures that the policy is largely predetermined by the inflation trajectory. However, the policy is not pure theater (unlike a piton) because rate changes do materially affect the economy. The theater reflects the gap between the central bank's public positioning (careful, data-dependent, gradual) and the underlying structural constraint (inflation requires demand suppression regardless of implementation method). Claimed type (tangled_rope): The policy solves a genuine coordination problem (controlling inflation benefits everyone through price stability) while asymmetrically extracting from debtors. Enforcement is active — the MPC maintains rates through explicit policy decision against pressure to cut them faster. Beneficiaries are real (savers, financial institutions); victims are real (borrowers, young buyers).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how economic policy can be simultaneously experienced as coordination and extraction depending on structural position. From the beneficiary's view (institutional saver), the policy is pure rope — coordination that benefits them. From the victim's view (mortgage borrower), it is pure snare — extraction with no escape. From the analytical view, it risks appearing as natural law (immutable feature of capitalism) when it is actually a contingent policy choice. The perspectival gap is 6 types wide (mountain to snare), reflecting maximum structural differentiation. This is diagnostic: if all agents experienced the policy the same way, it would be a uniform-type constraint (mountain or rope). The fact that perspectives radically diverge suggests the policy has succeeded in what it intended (distributional effect) while creating structural tensions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's power level, exit options, and beneficiary/victim status. Mortgage borrowers: powerless, trapped, victims → high d (0.90-0.95) → high f(d) (1.42) → maximum experienced extraction. First-time buyers: powerless, trapped, victims → d ≈ 0.95 → f(d) ≈ 1.42 → maximum extraction. Small business owners: moderate power, constrained exit, partially victims → d ≈ 0.65 → f(d) ≈ 1.00 → moderate extraction. Savers/Financial institutions: institutional power, arbitrage exit, beneficiaries → d ≈ 0.10 → f(d) ≈ -0.01 → negative effective extraction (subsidy). Global investors: powerful, mobile exit, beneficiaries with caveats → d ≈ 0.48 → f(d) ≈ 0.60 → moderate positive extraction (benefits with friction). Inflation control coalition: organized, constrained, partially victims/partially beneficiaries → d ≈ 0.50 → f(d) ≈ 0.65 → balanced extraction-coordination. The scope modifier σ(S) = 1.0 for national scope. Effective extraction χ = ε × f(d) × σ(S) ranges from negative (beneficiaries) to 1.82 (powerless victims at maximum d and f(d)). The commentary does not compute these explicitly but they are implicit in the perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE WITH EXTRACTIVE DRIFT: The constraint resolves mandatrophy by explicitly declaring both coordination function (inflation control) and asymmetric extraction (debt burden on borrowers). The MPC solves a genuine problem (inflation at 4-5%, target is 2%) through a mechanism that benefits savers and stabilizes prices. Simultaneously, it extracts from borrowers through higher debt service costs. Both are structural, both are measurable, neither is illusory. The theater ratio (0.58) prevents false classification as pure rope — the policy communication includes significant ritual (monthly meetings, forward guidance) that obscures the underlying constraint. The measurements show extractiveness rising from 0.35 to 0.52 over 12 months, reflecting accumulating burden on borrowers. The scaffold perspective (sunset when inflation returns to 2%) is analytically justified — the policy is explicitly temporary, with a public endpoint. However, the shadow omega (inflation may not fall within timeframe) leaves open the possibility that the constraint drifts toward snare (if high rates persist indefinitely). The analytical observer's mountain classification is identified as a false summit — monetary policy frameworks are institutional constructs, not laws of nature. The mandatrophy resolves through acknowledging the coordination-extraction hybrid as legitimate rather than trying to force it into a single pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_trajectory_uncertainty,
    'Will inflation decline to the 2% target within 18-24 months, validating the rate-hold strategy, or will stagflationary dynamics persist, requiring further tightening?',
    'CPI tracking, wage growth analysis, and comparative international inflation data; evaluation of whether current rate levels are sufficient or excessive',
    'If inflation falls on schedule: scaffold sunset logic confirmed, constraint transitions to temporary. If inflation persists: extraction component grows, constraint drifts toward snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_trajectory_uncertainty, empirical, 'Timeline and certainty of inflation return to 2% target').

omega_variable(
    mortgage_market_resilience,
    'What proportion of UK mortgages will become unsustainable if rates remain at 5.25% for >3 years, and will this trigger broader financial instability?',
    'Household debt service ratio tracking, repossession and default rate analysis, stress tests on banking system exposure to residential mortgage portfolios',
    'If <5% unsustainable: constraint remains tangled rope with manageable extraction. If >15% unsustainable: constraint becomes snare for critical demographic, potentially requiring policy reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortgage_market_resilience, empirical, 'Household mortgage sustainability threshold at sustained 5.25% rate').

omega_variable(
    policy_transmission_effectiveness,
    'Is the 5.25% rate actually controlling inflation through demand suppression, or are other factors (supply-side relief, energy price decline) doing the work, making the rate hold unnecessary?',
    'Empirical decomposition of inflation drivers; counterfactual analysis of inflation trajectory under alternative rate scenarios; international comparison with economies using different rate levels',
    'If rate is essential: policy justified, constraint serves genuine coordination function. If rate is redundant: extraction is pursuing policy theater rather than genuine inflation control, shifting constraint toward piton or pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_transmission_effectiveness, empirical, 'Causal contribution of base rate to inflation reduction').

omega_variable(
    distributional_intent_clarity,
    'Is the high-rate policy explicitly designed to redistribute from debtors to savers (intentional extraction), or is distributional impact incidental to inflation control (unintended byproduct)?',
    'Analysis of MPC communications, policy statements, and internal deliberations; comparison with alternative policy designs (wage subsidy, targeted demand management) that achieve inflation control with lower distributional cost',
    'If intentional: constraint becomes snare with active enforcement (asymmetric extraction). If incidental: constraint remains tangled rope with coordination justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_intent_clarity, conceptual, 'Whether distributional consequences are policy objective or incidental byproduct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boe_base_rate_policy_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boe_rate_tr_t0, boe_base_rate_policy_2024, theater_ratio, 0, 0.42).
narrative_ontology:measurement(boe_rate_tr_t6, boe_base_rate_policy_2024, theater_ratio, 6, 0.53).
narrative_ontology:measurement(boe_rate_tr_t12, boe_base_rate_policy_2024, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(boe_rate_be_t0, boe_base_rate_policy_2024, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(boe_rate_be_t6, boe_base_rate_policy_2024, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(boe_rate_be_t12, boe_base_rate_policy_2024, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boe_base_rate_policy_2024, resource_allocation).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, uk_housing_market_access).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, business_investment_throttle).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, pension_fund_solvency_relief).
narrative_ontology:affects_constraint(boe_base_rate_policy_2024, inflation_expectation_anchoring).

% DUAL FORMULATION NOTE:
% The base rate policy can be decomposed into structural components: (1) inflation suppression mechanism (ε ≈ 0.05, Mountain) — reducing aggregate demand is an inherent feature of demand-side monetary policy; (2) distributional effect (ε ≈ 0.62, Snare) — wealth transfer from borrowers to savers; (3) coordination infrastructure (ε ≈ 0.15, Rope) — price stability enables market coordination. This story treats them as integrated, but the network acknowledges downstream constraints that isolate each component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(boe_base_rate_policy_2024, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
