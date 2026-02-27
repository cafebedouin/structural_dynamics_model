% ============================================================================
% CONSTRAINT STORY: gold_fomo_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fomo_cycle, []).

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
 *   constraint_id: gold_fomo_cycle
 *   human_readable: The Gold Price 'Fear of Missing Out' Cycle
 *   domain: economic/financial_markets
 *
 * SUMMARY:
 *   The gold price FOMO cycle is a structural constraint that emerges during
 *   periods of sustained rally, typically triggered by macroeconomic
 *   uncertainty (rising inflation expectations, geopolitical risk, monetary
 *   expansion). The constraint operates as follows: (1) Initial price rally
 *   driven by fundamentals (real rate decline, risk-off flows); (2) Media
 *   coverage amplifies the move, generating FOMO narratives ('missing the
 *   rally'); (3) Retail investors enter en masse, often through leveraged
 *   positions, precisely when institutional accumulation has slowed; (4)
 *   Sentiment cascade pushes prices well above fundamental levels; (5)
 *   Institutional positions are liquidated into retail demand at the peak;
 *   (6) Price correction occurs, trapping retail investors in underwater
 *   positions. The constraint exhibits suppression through asymmetric
 *   information (institutions see positioning flows retail cannot access),
 *   time pressure (fear of missing out creates urgency that overrides due
 *   diligence), and limited exit options (retail cannot exit without
 *   realizing losses in a crash, and institutional capacity to absorb retail
 *   supply is limited). Theater is high (68%) because media narratives, chart
 *   patterns, and sentiment indices drive price movement more than
 *   fundamental macroeconomic shifts. The constraint is not invariant across
 *   all observers: institutions experience it as low-cost coordination
 *   (rope), retail experiences it as pure extraction (snare), regulators
 *   experience it as a hybrid problem (tangled rope), and the gold market
 *   institution experiences it as a degraded ritual (piton).
 *
 * KEY AGENTS:
 *   - Retail investors: Primary victim (powerless/trapped) — bear full extraction cost when entering at peak during media-driven enthusiasm; lack access to positioning data and face psychological time pressure
 *   - Financial institutions: Primary beneficiary (institutional/arbitrage) — accumulate during rally onset, exit into retail demand at peak; have informational advantage and exit capacity
 *   - Media and financial information ecosystem: Secondary beneficiary (institutional/arbitrage) — amplify narratives during rallies; increase engagement and advertising during high-volatility periods
 *   - Market price discovery mechanism: Primary victim (powerless/trapped) — accurate fundamental-based price discovery is corrupted by momentum extraction; institutional positions distort signals
 *   - Regulatory authorities: Constrained actor (organized/constrained) — must balance consumer protection with financial stability; leverage restrictions and circuit breakers partially but imperfectly mitigate the cycle
 *   - Gold market institution: Institutional actor (institutional/constrained) — safe-haven narrative persists; actual function has degraded into speculation vehicle; theater masks this degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fomo_cycle, 0.58).
domain_priors:suppression_score(gold_fomo_cycle, 0.62).
domain_priors:theater_ratio(gold_fomo_cycle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fomo_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(gold_fomo_cycle, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fomo_cycle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fomo_cycle, snare).
narrative_ontology:human_readable(gold_fomo_cycle, "The Gold Price 'Fear of Missing Out' Cycle").
narrative_ontology:topic_domain(gold_fomo_cycle, "economic/financial_markets").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, financial_institutions).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, early_entrant_investors).
narrative_ontology:constraint_victim(gold_fomo_cycle, retail_investors).
narrative_ontology:constraint_victim(gold_fomo_cycle, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped between asymmetric information (institutional knowledge of technicals) and time pressure (fear of missing the rally). Entry occurs near the peak when media coverage is loudest. Exit costs are maximized: selling into the crash realizes losses. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.96. Pure extraction with high coercion.
constraint_indexing:constraint_classification(gold_fomo_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARKET PRICE DISCOVERY (SNARE) — The mechanism that should aggregate information efficiently is captured by momentum extraction. Prices are driven by sentiment cascades rather than fundamental changes in real interest rates, geopolitical risk, or supply/demand. The constraint extracts accurate price signals from the market. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(gold_fomo_cycle, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) — See FOMO cycle as a coordination mechanism that aggregates liquidity and enables profitable positioning. They enter early, benefit from retail demand, and exit ahead of the correction. Institutions experience this as low-cost coordination: they solve the problem of capital mobilization during bull markets. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary from the cycle's existence.
constraint_indexing:constraint_classification(gold_fomo_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITIES (TANGLED ROPE) — Must balance multiple mandates: consumer protection (curb retail losses), financial stability (prevent cascade contagion), market function (preserve price discovery). The FOMO cycle is partly coordination (aggregates liquidity during risk-off episodes) and partly extraction (transfers retail capital to institutions). Regulations (position limits, disclosure rules, circuit breakers) have both functions but are imperfectly enforced. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GOLD MARKET INSTITUTION (PITON) — Gold's role as safe-haven asset and inflation hedge has degraded into a pure speculation vehicle for retail FOMO. The fundamental function (store of value, geopolitical insurance) persists in institutional narratives, but the actual constraint is driven by sentiment cascades and technical momentum. Theater ratio=0.68 captures that most price movement is performance (chart patterns, media narratives) rather than response to real monetary or geopolitical shifts. The market ritual persists through inertia.
constraint_indexing:constraint_classification(gold_fomo_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CHALLENGE) — A civilizational analysis might claim that FOMO cycles are a natural law of markets with asymmetric information: whenever information asymmetry and time pressure combine, momentum extraction emerges inevitably. However, the structural data (ε=0.58, suppression=0.62, theater=0.68) contradicts a mountain classification. This is a false summit: FOMO extraction is contingent on specific institutional arrangements (retail credit access, 24/7 media coverage, algorithmic amplification), not a timeless property of markets.
constraint_indexing:constraint_classification(gold_fomo_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fomo_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_fomo_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_fomo_cycle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fomo_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gold_fomo_cycle, TR),
    TR >= 0.70.

:- end_tests(gold_fomo_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The transfer from retail to institutional investors during a typical cycle is substantial—retail entering near peaks can lose 20-40% by the time the correction completes, while institutions positioned ahead of retail accumulation realize these gains. However, it is not maximal (0.70+) because: (a) not all retail investors lose (early retail entrants and leveraged shorts profit), (b) some of the 'extraction' is legitimate volatility risk compensation for institutions, and (c) the cycle eventually resets, allowing retail to re-enter at lower prices. The extractiveness value of 0.58 reflects that this is a severe but not permanent transfer—the constraint repeats rather than accumulates. Suppression (0.62): Moderate-high. Retail investors face three suppression mechanisms: (1) Information asymmetry—institutions have real-time positioning data, retail relies on lagged public information; (2) Time pressure—FOMO psychology creates urgency that suppresses due diligence; (3) Access barriers—leverage is available but carries costs, and exit liquidity is lowest when most needed. Suppression is not total (0.70+) because: (a) retail can observe price history and fundamental indicators, (b) FOMO is triggered by actual market conditions (rising prices), not pure manipulation, and (c) some retail investors do exit profitably or avoid entry entirely. Theater ratio (0.68): High. The narrative structure of the FOMO cycle is substantially performative: chart patterns ('golden cross,' 'breakout'), media headlines ('gold surges on inflation fears'), and sentiment indices drive retail entry more than actual macro shifts. Central banks and geopolitical events create fundamental pressure, but the magnitude of price swings driven by these factors is typically 1/3 to 1/2 of the total move; the remainder is momentum extraction. Theater increases over the interval (0.35 → 0.68) because: (a) price acceleration creates geometric media amplification, (b) retail participation accelerates momentum, and (c) technical analysis becomes self-fulfilling as more traders follow the same indicators.
 *
 * PERSPECTIVAL GAP:
 *   The FOMO cycle produces a stark perspectival gap between institutional and retail actors. Institutions see a coordination mechanism (Rope): the cycle mobilizes liquidity during risk-off periods, allows efficient capital allocation, and enables profitable positioning. The cycle solves the problem of 'how do we efficiently accumulate during uncertainty?' Retail sees pure extraction (Snare): they enter when institutions are exiting, lose on timing, and lack information to avoid the trap. The gap widens because institutions have structural advantages (information, timing capacity, leverage costs) that are not available to retail. Regulatory observers see a partial problem (Tangled Rope): the cycle does aggregate capital efficiently, but it does so by extracting from an unprotected class. The market ritual (Piton) perspective reveals that gold's fundamental function (safe haven) has degraded—price moves are driven by sentiment, not by real geopolitical or monetary shifts. The false mountain perspective (that FOMO is a natural law of markets) is revealed as contingent: FOMO cycles require specific institutional conditions (retail leverage access, 24/7 media, algorithmic amplification, information asymmetry) that could be regulated.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail investors: Victim + trapped → d≈0.92, f(d)≈1.40. High effective extraction. Retail enters at peaks after institutional accumulation, holds through the correction, realizes losses. Trapped because: (a) leverage requires holding through volatility, (b) exiting during a crash locks in losses, (c) re-entry costs (spread, taxes, psychological) are high. Institutional investors: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Net beneficiary (negative extraction). Institutions accumulate ahead of retail, exit into retail demand, and have low transaction costs and tax efficiency. Arbitrage exit option reflects their ability to move capital between markets. Market price discovery: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Price discovery cannot exit from the market; it is corrupted in place by momentum flows. Regulatory authorities: Both beneficiary (stability) and victim (consumer protection) → d≈0.50, f(d)≈0.65. Symmetric extraction. Regulators benefit from market liquidity but bear costs of consumer losses and systemic risk. Constrained exit reflects that regulators cannot simply shut down the gold market. Media ecosystem: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.10. Net beneficiary. Media benefits from high-volatility narratives and engagement; has exit options (can shift coverage to other assets).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CONFIRMATION: The snare classification is robust across the empirical and structural dimensions. Retail investors are the primary victims (trapped, powerless, bearing extraction). Financial institutions are the primary beneficiaries (arbitrage access, informational advantage). The constraint requires no active enforcement beyond market structure (leverage availability, information asymmetry, media amplification)—it is self-enforcing through psychological dynamics and structural imbalances. The cycle repeats because: (a) retail memory is short (each rally feels new), (b) leverage is reoffered after each crash, (c) media narratives re-emphasize FOMO after recovery, (d) macro uncertainty creates periodic risk-off episodes that retrigger the cycle. This is a natural snare, not a tangled rope disguised as one. The presence of a secondary beneficiary (media/information ecosystem) does not change the classification; snares often have multiple tiers of extraction. The regulatory perspective (Tangled Rope) is real but secondary—regulators constrain but do not eliminate the cycle. The piton perspective correctly identifies that gold's institutional narrative (safe haven) is partially degraded by speculation. The false mountain perspective is the key diagnostic: the analytical view that FOMO is 'natural to markets with asymmetric information' naturalizes a contingent institutional arrangement. Breaking the cycle would require either (a) reducing information asymmetry (transparency in institutional positioning), (b) removing leverage access for retail during bubbles, (c) reducing media amplification of speculation, or (d) some combination. These are policy interventions, not laws of nature—the mountain is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    momentum_vs_fundamental_threshold,
    'At what point does price appreciation detach from fundamental drivers (real rates, geopolitical risk, supply dynamics) and become pure momentum extraction?',
    'Regression analysis correlating gold price moves with macro variables; identification of inflection points where correlation breaks; behavioral finance metrics (volatility spikes, sentiment indices, retail flow data)',
    'If threshold occurs early (within 20% rally): retail FOMO is secondary to institutional positioning. If threshold occurs late (>50% rally): FOMO is primary extraction driver. This affects whether the snare classification is structural or temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(momentum_vs_fundamental_threshold, empirical, 'Threshold for detachment of price from fundamentals').

omega_variable(
    retail_vs_institutional_extraction_magnitude,
    'How much of the total transfer from retail to institutions is due to entry timing (institutional front-running) versus exit timing (institutional dumping into retail demand)?',
    'Flow analysis from broker data; timing correlation between institutional positioning changes and retail accumulation; realized vs unrealized losses by account type',
    'If institutional extraction is symmetric: this is closer to Tangled Rope (both tiers extract). If institutional extraction dominates: confirms Snare classification for retail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_vs_institutional_extraction_magnitude, empirical, 'Attribution of extraction to timing vs other factors').

omega_variable(
    media_causality_direction,
    'Does media hype cause retail FOMO demand, or does rapid price appreciation cause media coverage, with retail demand following both?',
    'Granger causality analysis of media volume vs retail order flow; lag analysis of price moves vs news cycle; A/B testing of retail exposure to narratives',
    'If media causes demand: suppression includes information control (media amplification). If price causes media: suppression is primarily time-lag (retail enters late). This affects whether suppression should be higher or lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(media_causality_direction, empirical, 'Direction of causality between media hype and retail demand').

omega_variable(
    credit_availability_constraint_binding,
    'Is the retail FOMO cycle structurally bound by availability of leverage credit, or do other psychological factors (FOMO itself, peer pressure) drive entry even without leverage?',
    'Analysis of cash vs leveraged positions during rallies; correlation between margin availability and retail entry; comparison of retail behavior in markets with and without leverage options',
    'If credit is binding: tightening leverage requirements could break the cycle (policy intervention point). If psychological factors dominate: extraction persists even without leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_availability_constraint_binding, empirical, 'Whether credit availability is the binding constraint on FOMO').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fomo_cycle, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_fomo_tr_t0, gold_fomo_cycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gold_fomo_tr_t2, gold_fomo_cycle, theater_ratio, 2, 0.54).
narrative_ontology:measurement(gold_fomo_tr_t4, gold_fomo_cycle, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(gold_fomo_be_t0, gold_fomo_cycle, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gold_fomo_be_t2, gold_fomo_cycle, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(gold_fomo_be_t4, gold_fomo_cycle, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fomo_cycle, resource_allocation).
narrative_ontology:affects_constraint(gold_fomo_cycle, retail_leverage_trap).
narrative_ontology:affects_constraint(gold_fomo_cycle, asymmetric_market_information).

% DUAL FORMULATION NOTE:
% The FOMO cycle is downstream of two structural constraints: (1) retail_leverage_trap (ε≈0.45, Tangled Rope)—the availability and cost structure of leverage credit that enables retail speculation; (2) asymmetric_market_information (ε≈0.52, Snare)—the information gap between institutional and retail investors regarding positioning and flow. The FOMO cycle emerges when both upstream constraints are activated simultaneously during a rally. Each upstream constraint has its own story; the FOMO cycle integrates them into a temporal dynamic (the four-stage rally/crash cycle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
