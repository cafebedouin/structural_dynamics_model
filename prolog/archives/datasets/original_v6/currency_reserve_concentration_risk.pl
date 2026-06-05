% ============================================================================
% CONSTRAINT STORY: currency_reserve_concentration_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_reserve_concentration_risk, []).

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
 *   constraint_id: currency_reserve_concentration_risk
 *   human_readable: Currency Reserve Concentration Risk in Global Monetary Systems
 *   domain: monetary_policy/international_finance
 *
 * SUMMARY:
 *   Currency reserve concentration creates a structural asymmetry in global
 *   monetary systems where one currency (the U.S. dollar) provides the
 *   settlement medium, store of value, and unit of account for international
 *   transactions. This concentration enables efficiency for some actors and
 *   imposes extraction costs on others. The constraint exhibits six distinct
 *   classifications from different structural positions: a snare for
 *   powerless nations trapped without alternatives, a rope for the reserve
 *   currency center benefiting from coordination, a tangled rope for emerging
 *   markets caught between capital flows and currency stability, a scaffold
 *   for regional alternatives building exit pathways, a piton for Bretton
 *   Woods institutions persisting through inertia, and a false mountain from
 *   a civilization-scale natural law view. The constraint has intensified
 *   over the 30-year interval measured here: extractiveness has grown from
 *   0.35 to 0.62 as dollar dominance has consolidated post-Cold War, while
 *   theater has remained moderate, indicating that most of the measured
 *   extraction reflects real structural effects rather than performative
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - U.S. Treasury and Federal Reserve: Primary beneficiary (institutional/arbitrage) — captures seigniorage, monetary policy flexibility, persistent deficit financing capacity
 *   - Non-Reserve Currency Central Banks: Primary victim (powerless/trapped) — bears interest rate transmission, currency volatility, reserves adequacy pressure
 *   - Commodity Exporting Economies: Secondary victim (powerless/trapped) — commodity price volatility in dollars, terms-of-trade fragility, persistent current account exposure
 *   - Emerging Market Central Banks: Moderate victim (moderate/constrained) — benefit from capital inflows during stable periods but vulnerable to carry-trade reversals and interest rate shocks
 *   - International Financial Institutions: Beneficiary (institutional/arbitrage) — IMF, World Bank, BIS benefit from dollar standardization in operations and reporting
 *   - Regional Currency Arrangements: Organized challengers (organized/mobile) — BRICS reserve pool, renminbi internationalization, bilateral swap networks building alternative pathways
 *   - Bretton Woods Institutions: Performative residue (institutional/arbitrage) — IMF quota system, gold standard nostalgia maintain theater of alternatives without functional verification
 *   - Analytical Observer: Risks false naturalization (analytical/analytical) — risks interpreting Trilemma as immutable law rather than contingent architectural choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_reserve_concentration_risk, 0.58).
domain_priors:suppression_score(currency_reserve_concentration_risk, 0.65).
domain_priors:theater_ratio(currency_reserve_concentration_risk, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_reserve_concentration_risk, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_reserve_concentration_risk, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(currency_reserve_concentration_risk, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_reserve_concentration_risk, tangled_rope).
narrative_ontology:human_readable(currency_reserve_concentration_risk, "Currency Reserve Concentration Risk in Global Monetary Systems").
narrative_ontology:topic_domain(currency_reserve_concentration_risk, "monetary_policy/international_finance").

domain_priors:requires_active_enforcement(currency_reserve_concentration_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_reserve_concentration_risk, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(currency_reserve_concentration_risk, us_treasury).
narrative_ontology:constraint_beneficiary(currency_reserve_concentration_risk, financial_institutions_dollar_denominated).
narrative_ontology:constraint_victim(currency_reserve_concentration_risk, non_reserve_currency_central_banks).
narrative_ontology:constraint_victim(currency_reserve_concentration_risk, commodity_exporters).
narrative_ontology:constraint_victim(currency_reserve_concentration_risk, developing_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-RESERVE CURRENCY CENTRAL BANKS (SNARE) — Trapped in dollar dependency by absence of alternatives at global scale. Cannot exit without massive coordination failure among 180+ national central banks. Bears full extraction cost: foreign exchange volatility, interest rate shocks transmitted from reserve currency center, inability to defend currency independence without exhausting reserves.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMODITY EXPORTING ECONOMIES (SNARE) — Structurally trapped. Commodity prices denominated in dollars; local currency revenues subject to dollar volatility completely outside their control. Cannot diversify pricing (global markets set in dollars) or easily exit dollar reserves (no alternative reserve asset with global liquidity). Bears compounding extraction: real income volatility, terms-of-trade shocks, persistent current-account fragility.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EMERGING MARKET CENTRAL BANKS (TANGLED ROPE) — Constrained by capital controls, exchange rate stability requirements, and carry-trade dynamics, but also benefit from dollar inflows during risk-off periods and from ability to invest reserves in dollar assets. Genuine coordination function (stabilizing capital flows) coexists with asymmetric extraction (interest rate transmission, sudden reversal risk). Agency exists but costly.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: U.S. TREASURY AND FEDERAL RESERVE (ROPE) — Primary beneficiaries. Experience the constraint as coordination: dollar dominance enables efficient global transaction settlement, provides seigniorage revenue, and grants monetary policy flexibility. Net positive: can borrow in own currency, export inflation when needed, run persistent deficits without currency crisis. Low-cost coordination mechanism for their own interests.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL FINANCIAL INSTITUTIONS (ROPE) — Benefit from dollar-dominated settlement and reserve systems. IMF, World Bank, BIS derive operational convenience from standardized dollar metrics. Experience the constraint as pure coordination: dollar pricing simplifies lending, reserve adequacy assessment, and cross-border settlement. No perceived extraction for institutional actors operating at global scale.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL CURRENCY ARRANGEMENTS (SCAFFOLD) — Organized agents building alternative clearing mechanisms: renminbi internationalization, BRICS reserve pool, bilateral swap agreements. These represent temporary scaffolding with implicit sunset logic: as regional integration deepens and alternative reserve assets mature, the concentration risk diminishes. High suppression (incumbent reserve system carries structural advantage) but visible exit pathway and coordination function during transition period.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: GOLD STANDARD NOSTALGIA / BRETTON WOODS REMNANTS (PITON) — The formal commitment to exchange rate stability and the institutional preference for reserve asset stability persist largely through inertia. The gold standard's functional role (anchor for confidence) has been replaced by Federal Reserve credibility, yet the theater of gold reserves persists in central bank balance sheets. Bretton Woods institutions (IMF quota system, fixed parities) are largely performative — actual monetary coordination happens through other mechanisms. Theater ratio high; functional verification of the traditional constraints low.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep structural perspective, some monetary coordination concentration may be inevitable: the Bretton Woods Trilemma (independent monetary policy, fixed exchange rates, capital mobility — choose two) is a mathematical constraint. Any global monetary system must solve this trilemma; concentration in one currency may be the minimum-cost solution. This perspective risks naturalizing the dollar's dominance as inherent necessity rather than contingent institutional choice.
constraint_indexing:constraint_classification(currency_reserve_concentration_risk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_reserve_concentration_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_reserve_concentration_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_reserve_concentration_risk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_reserve_concentration_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(currency_reserve_concentration_risk, TR),
    TR >= 0.70.

:- end_tests(currency_reserve_concentration_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and growing. The dollar's dominance enables real extraction mechanisms: (1) seigniorage revenue flows to U.S. treasury from global dollar demand; (2) monetary policy transmission shock — Federal Reserve rate changes propagate asymmetrically to emerging markets; (3) carry-trade dynamics where dollar appreciation imposes sudden reversal costs on debtor nations; (4) reserve adequacy pressure requiring other central banks to hold dollar reserves at opportunity cost. The growth trajectory from 0.35 to 0.62 reflects post-Cold War consolidation of dollar dominance and financialization of emerging economies, making them more exposed to dollar shocks. Not as severe as a pure snare (0.80+) because the dollar system does provide real coordination benefits: efficient settlement, global market pricing, transaction cost reduction. Suppression (0.65): Moderately high and structural. Exit barriers include: (1) no alternative currency with sufficient global liquidity and trust (network effects problem); (2) political/institutional commitment to Bretton Woods architecture; (3) sunk investments in dollar-denominated assets and liabilities; (4) coordination failure among 180+ central banks (would require impossible coordination to simultaneous shift). Low political feasibility of rapid change but not absolute — regional alternatives (BRICS, ASEAN, bilateral swaps) show that suppression can erode at margins. Theater ratio (0.48): Moderate, not high. The extraction is substantially real rather than performative: interest rate transmission has measurable economic effects, seigniorage is quantifiable, currency crises are genuine. Theater comes from: (1) Bretton Woods institutions continuing to perform roles (IMF surveillance) now superseded by market mechanisms; (2) gold standard nostalgia in central bank balance sheets; (3) formal commitment to exchange rate stability that actual policy doesn't honor. But most of the constraint's power comes from structural economic effects, not theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence on classification type (snare vs rope vs mountain). The gap reveals that dollar dominance is not an intrinsic property of global finance but a structural outcome of post-WWII institutional choices and accumulated network effects. If the analytical observer correctly identifies this as a false mountain (not an inevitable trilemma solution but a contingent arrangement), then the scaffold and emerging market perspectives become diagnostic evidence that exit pathways are genuinely being built — the constraint is already weakening at the margins, which would be invisible from a natural law perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their experienced extractiveness (chi) through the beneficiary/victim status and exit options. The U.S. Treasury and Federal Reserve are clear beneficiaries with arbitrage options (can choose to allow or prevent dollar dominance changes) — they experience low effective extraction, seeing the constraint as pure coordination (rope). Non-reserve currency central banks are victims with no exit options (trapped) — they experience maximum extraction relative to their power level. Emerging markets occupy intermediate positions: they are partly victims (subject to transmission shocks) and partly beneficiaries (receive capital inflows) — they are constrained rather than trapped, producing tangled rope classification rather than snare. Regional alternatives are organized actors with mobile options (can build alternative clearing mechanisms) — they see extraction but perceive pathways, producing scaffold classification. The directionality pipeline computes chi from these positions; the beneficiary/victim declarations and exit options together determine whether an agent experiences the constraint as coordination, extraction, or mixed.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The six perspectives resolve the apparent mandatrophy (is this a natural law versus an extractive constraint?) by showing that concentration is partly coordinated efficiency (genuine rope function) and partly extraction (genuine snare effects), with the balance shifting between agents. The snare classification for powerless central banks and the rope classification for the U.S. Treasury are not contradictory — they measure the same constraint from positions with opposite directional relationships to the extraction flow. The analytical observer's mountain classification is a false summit: it confuses the mathematical Trilemma (a real constraint on monetary architecture) with evidence that dollar dominance is inevitable. The Trilemma only says 'choose two of three' — it doesn't say 'the U.S. dollar must be the chosen medium.' The scaffold and regional alternatives perspectives provide evidence that the constraint is not immutable: alternative architectures (multipolar reserves, CBDC interoperability, regional clearing) are being constructed. The theater ratio (0.48) confirms this is not purely theater — the extraction is substantially real — but high enough suppression (0.65) that exit remains difficult without coordination. The mandatrophy resolves by recognizing that all six types are legitimate readings of different structural dimensions of the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trilemma_inevitability,
    'Is monetary system concentration in a single reserve currency an inevitable consequence of the Trilemma, or a contingent institutional choice that could be replaced with multipolar or decentralized alternatives?',
    'Comparative institutional analysis of alternative monetary architectures (SDR-based, blockchain-native, multipolar reserve baskets); empirical test of whether emerging alternatives can maintain all three Trilemma properties simultaneously',
    'If inevitable: mountain classification from analytical perspective is correct — concentration is a natural law. If contingent: false summit — the constraint is actually a tangled rope or snare, and policy change is structurally feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trilemma_inevitability, conceptual, 'Whether monetary concentration is inevitable or contingent').

omega_variable(
    alternative_reserve_viability,
    'Can the euro, yuan, SDR, or other alternatives achieve the network effects and liquidity that make the dollar the single reserve asset, or is there a critical mass threshold that prevents competition?',
    'Market liquidity analysis (bid-ask spreads, transaction volumes); network effect modeling; comparison of euro, yuan, SDR market development trajectories over 20-year horizon',
    'If critical mass is insurmountable: snare/piton classification (monopoly lock-in). If alternatives can reach tipping point: scaffold classification (sunset pathway is real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reserve_viability, empirical, 'Whether alternative reserve assets can overcome network effects').

omega_variable(
    extraction_vs_coordination_decomposition,
    'What fraction of the measured extractiveness (0.58) represents necessary coordination cost (clearing, settlement, transaction efficiency) versus genuine extraction (seigniorage, monetary policy transmission, financial system fragility export)?',
    'Comparative analysis of settlement costs and transaction efficiency gains from dollar dominance versus hypothetical multipolar system; measurement of seigniorage flows to U.S. treasury; econometric decomposition of exchange rate transmission mechanisms',
    'If coordination cost is >60% of extractiveness: tangled rope classification is durable. If extraction is >70%: constraint reclassifies as snare from emerging market perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, empirical, 'Decomposition of coordination cost versus extraction').

omega_variable(
    central_bank_digital_currency_disruption,
    'Could central bank digital currencies (CBDCs) operating on interoperable distributed ledgers bypass the concentration risk entirely, or do they replicate the same network effects that produced dollar dominance?',
    'Technical analysis of CBDC interoperability standards (mBridge, Project Dunbar); network effect modeling applied to digital currency architectures; observational period through 2035 for CBDC adoption patterns',
    'If CBDCs can achieve true interoperability: scaffold classification confirmed, sunset accelerates to 10-15 year horizon. If CBDCs replicate concentration: constraint persists in digital form, snare/tangled rope classifications persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(central_bank_digital_currency_disruption, empirical, 'Whether CBDCs can disrupt currency concentration').

omega_variable(
    suppression_mechanism_structural_vs_behavioral,
    'Is the suppression of exit options (0.65) primarily structural (economic/technical barriers to building alternatives) or behavioral/institutional (coordination failure among central banks, sunk institutional commitments)?',
    'Analysis of actual technical barriers versus announced policy constraints; interview data from central bank leadership; comparison of stated preferences for reserve diversification versus actual reserve composition changes',
    'If structural: suppression is durable, snare classification is robust. If behavioral: suppression could shift rapidly with policy coordination, scaffold/rope classifications become more accessible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_behavioral, empirical, 'Suppression mechanism decomposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_reserve_concentration_risk, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crcrisk_tr_t0, currency_reserve_concentration_risk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(crcrisk_tr_t10, currency_reserve_concentration_risk, theater_ratio, 10, 0.41).
narrative_ontology:measurement(crcrisk_tr_t20, currency_reserve_concentration_risk, theater_ratio, 20, 0.48).
narrative_ontology:measurement(crcrisk_tr_t30, currency_reserve_concentration_risk, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(crcrisk_be_t0, currency_reserve_concentration_risk, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crcrisk_be_t10, currency_reserve_concentration_risk, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(crcrisk_be_t20, currency_reserve_concentration_risk, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(crcrisk_be_t30, currency_reserve_concentration_risk, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_reserve_concentration_risk, resource_allocation).
narrative_ontology:affects_constraint(currency_reserve_concentration_risk, currency_carry_trade_instability).
narrative_ontology:affects_constraint(currency_reserve_concentration_risk, central_bank_digital_currency_race).
narrative_ontology:affects_constraint(currency_reserve_concentration_risk, emerging_market_debt_in_foreign_currency).
narrative_ontology:affects_constraint(currency_reserve_concentration_risk, seigniorage_extraction_mechanism).

% DUAL FORMULATION NOTE:
% Currency reserve concentration is upstream of multiple constraint families. The carry-trade instability constraint depends on dollar dominance creating asymmetric funding cost structure. CBDC architectures are downstream alternatives attempting to resolve concentration. Foreign currency debt is a victim-side consequence of this constraint. Seigniorage extraction is a beneficiary-side mechanism within the larger constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_reserve_concentration_risk, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
