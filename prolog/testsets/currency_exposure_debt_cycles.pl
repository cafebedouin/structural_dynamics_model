% ============================================================================
% CONSTRAINT STORY: currency_exposure_debt_cycles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_exposure_debt_cycles, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: currency_exposure_debt_cycles
 *   human_readable: Currency Exposure Debt Cycles: Structural Trap for Foreign-Currency-Denominated Borrowers
 *   domain: economic/financial/macroeconomic
 *
 * SUMMARY:
 *   Currency exposure debt cycles represent a structural trap in the
 *   architecture of global finance whereby borrowers in developing economies
 *   or smaller economies incur debt denominated in currencies they do not
 *   control or directly earn, creating compounding vulnerability when home
 *   currencies depreciate. The trap operates through a feedback loop:
 *   external shock (commodity price drop, capital flight, interest rate
 *   increase in reserve currency zone) triggers currency depreciation →
 *   borrower's debt service costs rise in home-currency terms → reduced
 *   capacity to pay → risk premium increases → further currency pressure →
 *   crisis and potential default. The constraint exhibits high extractiveness
 *   (0.68) because the trapped agents bear costs that benefit currency
 *   creditors and speculators, with systematically limited exit options.
 *   Suppression (0.72) is high: borrowers cannot hedging costs are
 *   prohibitive, currency-denominated refinancing requires willing lenders,
 *   and local-currency alternatives remain underdeveloped at scale. Theater
 *   is relatively low (0.38) because the mechanism is structurally
 *   transparent — the extraction operates through demonstrable economic
 *   accounting, not performative ritual. The constraint classifies as Snare
 *   from powerless trapped borrowers' and moderate sovereigns' perspectives,
 *   but reveals genuine coordination function from institutional
 *   perspectives, suggesting the architecture serves real purposes (capital
 *   flows, trade finance) while simultaneously extracting through currency
 *   risk asymmetries. This family of constraints includes upstream components
 *   (reserve currency hegemony, Bretton Woods architecture) and downstream
 *   manifestations (debt crises in specific emerging markets).
 *
 * KEY AGENTS:
 *   - Foreign-Currency Borrowers: Primary victims (powerless/trapped) — firms and individuals borrowing in USD/EUR without matching revenues; face exponential debt service cost increases during currency depreciation cycles
 *   - Developing-Economy Sovereigns: Primary victims (moderate/constrained) — national governments bearing foreign-currency debt for infrastructure and development; cannot exit without triggering debt crisis or severe domestic adjustment
 *   - Foreign-Currency Creditors: Primary beneficiaries (institutional/arbitrage) — international banks, pension funds, hedge funds earning interest spread plus currency risk premium; can hedge exposure or exit position
 *   - Currency Speculators: Secondary beneficiary (powerful/mobile) — entities betting on currency depreciation; profit from the volatility that creates borrower distress
 *   - IMF/World Bank Reformers: Organized actors (organized/constrained) — institutional coalition building exit pathways through local-currency market development and policy reform
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes currency hegemony as historically contingent engineered system, not natural macroeconomic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_exposure_debt_cycles, 0.68).
domain_priors:suppression_score(currency_exposure_debt_cycles, 0.72).
domain_priors:theater_ratio(currency_exposure_debt_cycles, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_exposure_debt_cycles, extractiveness, 0.68).
narrative_ontology:constraint_metric(currency_exposure_debt_cycles, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(currency_exposure_debt_cycles, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_exposure_debt_cycles, snare).
narrative_ontology:human_readable(currency_exposure_debt_cycles, "Currency Exposure Debt Cycles: Structural Trap for Foreign-Currency-Denominated Borrowers").
narrative_ontology:topic_domain(currency_exposure_debt_cycles, "economic/financial/macroeconomic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_exposure_debt_cycles, foreign_currency_creditors).
narrative_ontology:constraint_beneficiary(currency_exposure_debt_cycles, currency_speculators).
narrative_ontology:constraint_victim(currency_exposure_debt_cycles, foreign_currency_borrowers).
narrative_ontology:constraint_victim(currency_exposure_debt_cycles, developing_economy_sovereigns).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED BORROWER (SNARE) — Individual or firm borrowing in foreign currency (USD, EUR, JPY) faces exchange rate depreciation when home currency weakens. Debt service costs rise in home-currency terms without corresponding income increase. Cannot exit: debt is contractual obligation; currency hedging is expensive and available only to large institutions; refinancing requires willing creditors with appetite for currency risk; local-currency financing unavailable for long-term amounts. Maximum experienced extraction — immobilized by debt denominated in currency they do not directly control or earn.
constraint_indexing:constraint_classification(currency_exposure_debt_cycles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING SOVEREIGN (SNARE) — Government bearing foreign-currency debt exposure from national infrastructure, commodity-dependent revenues, or IMF/World Bank financing. Cannot exit easily: currency devaluation triggers debt crisis (rising service costs exceed revenues); currency defense through reserves-burning or rate-hiking damages domestic economy; restructuring triggers loss of access to capital markets. Significant extraction — nation-level agency exists but is severely constrained by debt obligations and macroeconomic feedback loops.
constraint_indexing:constraint_classification(currency_exposure_debt_cycles, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL BANK (TANGLED ROPE) — Large creditor institution experiences currency exposure debt cycles as a genuine coordination mechanism (matching foreign-currency borrowers with lenders) PLUS systematic extraction (capturing currency risk premium in interest rates while maintaining portfolio diversification). The bank can exit or hedge its exposure through financial instruments; borrowers cannot. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(currency_exposure_debt_cycles, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL FINANCIAL SYSTEM (ROPE) — From the vantage of institutional architecture, currency exposure debt cycles are a coordination mechanism enabling capital flows across borders. The system coordinates savings (creditors) with investment opportunities (borrowers) and enables trade financing. Extraction exists but is not the primary function — the system performs genuine coordination. Institutional arbitrage position: central banks and large financial institutions can adjust policy and leverage constraints.
constraint_indexing:constraint_classification(currency_exposure_debt_cycles, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IMF-LED REFORM COALITION (SCAFFOLD) — Organized effort (IMF, World Bank, regional development banks) sees currency exposure debt cycles as a temporary coordination failure addressable through structural reform. Sunset logic: local-currency bond market development, inflation targeting, currency basket borrowing, and hedging instruments are designed to build exit pathways. Extraction is high during implementation (structural adjustment imposes costs on populations) but explicitly sunset through institutional reform. Coalition members face constrained exit but perceive reform pathways.
constraint_indexing:constraint_classification(currency_exposure_debt_cycles, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — The entire architecture of dollar-denominated global reserves and petroleum pricing creates structural exposure to currency depreciation for non-reserve-currency-issuing nations. This is extractive at civilizational scale — no single borrower can exit; exit requires collective coordination to reconstruct the reserve system. The constraint is naturalistically presented ('that is how global finance works') but is historically contingent (Bretton Woods, post-1971 dollar hegemony). Recognition that this is engineered extraction, not immutable law.
constraint_indexing:constraint_classification(currency_exposure_debt_cycles, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_exposure_debt_cycles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_exposure_debt_cycles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_exposure_debt_cycles, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_exposure_debt_cycles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(currency_exposure_debt_cycles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The base measurement reflects that foreign-currency denominated debt systematically transfers wealth from borrowers to creditors when currency depreciates — a direct extraction mechanism. The increase over the interval (0.45 → 0.68) reflects accumulating debt burdens during a period of emerging-market currency stress, rising interest rates in reserve currency zones, and capital outflows that deepen depreciation. The extractiveness is not maximal (would be 1.0) because some borrowers can refinance or service debt despite depreciation, and some creditors absorb losses. But the trajectory is upward, indicating the mechanism is progressively capturing larger wealth transfers. Suppression (0.72): Very high and stable. Borrowers face multiple binding constraints: (1) debt contracts are legally enforced, (2) currency hedging is prohibitively expensive or unavailable, (3) local-currency financing is unavailable at necessary scales or maturities, (4) refinancing requires creditor willingness and market access, (5) currency intervention by central banks is limited by foreign exchange reserves, (6) structural economic adjustment (belt-tightening) damages domestic economy while improving creditor repayment probability. Exit options are severely constrained across the board. Theater ratio (0.38, stable): Relatively low because the extraction mechanism is transparently mathematical — debt service in home currency = principal+interest × (home/foreign exchange rate). No performative ritual is required. The mechanism works through honest accounting. Theater is not zero because some narrative framing occurs (inevitability of depreciation, naturalness of foreign currency financing, claims that reform requires IMF programs), but the extraction does not depend on obscuring the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival disagreement. Trapped borrowers see a snare (pure extraction, no escape). Sovereigns see a snare (immobilized by debt). International banks see tangled rope (genuine capital allocation coordination + currency risk premium extraction). The global financial system sees rope (genuine coordination of savings and investment). Reformers see scaffold (temporary problem addressable through institutional change). The analytical observer sees snare at civilizational scale (engineered currency hegemony). The gap is not measurement noise — it reflects genuine structural differences in exit options and beneficiary/victim status. The bank perspective (tangled rope) is essential for diagnosis: it shows that the mechanism has real coordination content (matching borrowers with lenders) even though it is extractive (borrowers bear currency risk they did not choose). This rules out piton classification (would require theater ≥ 0.70; theater here is 0.38) and points to snare from powerless/trapped perspectives plus tangled rope from powerful/mobile perspectives as the correct mixed classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives based on structural position. Trapped borrowers with no exit (d ≈ 0.95) experience maximum extractiveness. Sovereigns with constrained exit (d ≈ 0.80) experience high extractiveness. International banks with arbitrage options (d ≈ 0.30) experience moderate extraction scaled down by exit mobility. The global financial system as institutional beneficiary (d ≈ 0.10) experiences negative effective extraction — the system benefits from the coordination function of currency exposure. The reform coalition (d ≈ 0.50) experiences moderate extraction because they face real constraints during implementation but also have genuine agency in policy design. The analytical observer (d ≈ 0.72) perceives the extraction as systemic — not dependent on individual agent position but inherent to the reserve currency architecture. The pipeline applies f(d) and scope modifiers: global scope (σ=1.2) amplifies extraction because the mechanism operates across all developing economies simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by establishing that currency exposure debt cycles are NOT natural law (mountain), NOT pure coordination (rope), but genuinely a SNARE with embedded coordination function. The mandatrophy question is: 'Is currency exposure a necessary cost of global finance (coordination) or an extractive structure imposed by hegemonic actors?' Answer: Both. The mechanism performs real coordination (matching savers with borrowers, enabling trade finance, allocating capital across borders) while simultaneously extracting from borrowers through asymmetric currency risk allocation. The snare classification captures this: snares are extractive mechanisms that persist because they have some function. The analytical observer's perspective prevents the constraint from being misclassified as 'just how global finance works' (false mountain) or 'a fair mechanism everyone accepts' (false rope). The constraint is engineered extraction justified by coordination narratives. Recognition that the IMF reform coalition's scaffold perspective represents an actual policy pathway (local-currency market development, currency basket borrowing, hedging infrastructure) prevents the snare from being classified as immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    currency_depreciation_inevitability,
    'Is currency depreciation during debt cycles an inevitable macroeconomic consequence or a policy choice embedded in monetary regime design?',
    'Comparison of inflation-adjusted debt service costs for borrowers in fixed-regime vs managed-float vs dollarized economies; correlation between central bank policy autonomy and depreciation magnitude during external shock',
    'If inevitable: constraint is closer to mountain (natural macroeconomic law). If policy-contingent: constraint is clearly snare (designed extraction). Current evidence suggests policy-contingent dominates, supporting snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(currency_depreciation_inevitability, empirical, 'Whether currency depreciation in debt cycles is inevitable or policy-engineered').

omega_variable(
    local_currency_market_feasibility,
    'Can long-term local-currency bond markets develop at scale sufficient to allow developing nations to refinance infrastructure without foreign currency exposure?',
    'Historical comparison of local-currency market depth in emerging markets vs reserve-currency economies; identification of institutional barriers (capital controls, inflation targeting constraints, institutional investor mandates) vs genuine market constraints',
    'If feasible: scaffold sunset is real (can exit the snare through market development). If infeasible: trap is permanent (snare is immutable for non-reserve-currency nations). Current efforts (Brazil, Mexico, South Africa local-currency development) suggest partial feasibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_currency_market_feasibility, empirical, 'Whether developing nations can sustainably refinance in local currency').

omega_variable(
    hedging_accessibility_asymmetry,
    'Do currency hedging costs create a fundamental barrier that systematically excludes small borrowers and developing sovereigns from cost-effective risk management?',
    'Price comparison of currency derivatives for large vs small counterparties; analysis of hedging costs as percentage of interest savings; examination of whether hedging access correlates with borrower size/creditworthiness',
    'If yes: hedging is pseudo-exit (available theoretically but unaffordable in practice), strengthening snare classification. If no: hedging provides genuine exit option, reducing effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hedging_accessibility_asymmetry, empirical, 'Whether currency hedging remains prohibitively expensive for small borrowers').

omega_variable(
    reserve_currency_alternative,
    'Could a basket-based or commodity-backed reserve system reduce extraction compared to single-currency hegemony?',
    'Simulation of historical debt cycles under alternative reserve regimes (SDR basket, gold standard, multi-currency basket); comparison of volatility and distributional consequences',
    'If yes: current system is engineered choice, not inevitable. Supports civilizational-scale snare classification. If no: currency exposure trap is inherent to any reserve system. Downgrades from engineered extraction to structural coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reserve_currency_alternative, conceptual, 'Whether alternative reserve architectures would reduce debt cycle extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_exposure_debt_cycles, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cedc_theater_t0, currency_exposure_debt_cycles, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cedc_theater_t5, currency_exposure_debt_cycles, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cedc_theater_t10, currency_exposure_debt_cycles, theater_ratio, 10, 0.38).
narrative_ontology:measurement(cedc_theater_t3, currency_exposure_debt_cycles, theater_ratio, 3, 0.34).

% Extraction over time
narrative_ontology:measurement(cedc_extract_t0, currency_exposure_debt_cycles, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cedc_extract_t5, currency_exposure_debt_cycles, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cedc_extract_t10, currency_exposure_debt_cycles, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(cedc_extract_t3, currency_exposure_debt_cycles, base_extractiveness, 3, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_exposure_debt_cycles, resource_allocation).
narrative_ontology:affects_constraint(currency_exposure_debt_cycles, reserve_currency_hegemony).
narrative_ontology:affects_constraint(currency_exposure_debt_cycles, emerging_market_debt_crisis).
narrative_ontology:affects_constraint(currency_exposure_debt_cycles, commodity_price_collapse).

% DUAL FORMULATION NOTE:
% Currency exposure debt cycles decompose into three related constraints: (1) reserve_currency_hegemony (ε ≈ 0.50, constraint_type snare at civilizational scale, defines the structural condition), (2) currency_exposure_debt_cycles (ε ≈ 0.68, constraint_type snare at national/firm scale, the extraction mechanism operating within hegemony), (3) emerging_market_debt_crisis (ε ≈ 0.75, constraint_type snare + piton, institutional performance of debt mechanics during crisis). Each story has distinct measurement trajectories and exit pathways. Currency exposure debt cycles is downstream of reserve currency hegemony (cannot exit currency exposure without changing global monetary architecture) but upstream of acute debt crises (accumulating exposure eventually triggers crisis events).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
