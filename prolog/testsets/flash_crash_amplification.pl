% ============================================================================
% CONSTRAINT STORY: flash_crash_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flash_crash_amplification, []).

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
 *   constraint_id: flash_crash_amplification
 *   human_readable: Flash Crash Amplification in Automated Trading Systems
 *   domain: financial_systems/market_microstructure
 *
 * SUMMARY:
 *   Flash crashes represent a structural constraint where high-frequency
 *   trading algorithms create cascading price movements that extract value
 *   from slower market participants. The constraint exhibits eight distinct
 *   DR classifications from different observer positions: retail investors
 *   and pension funds experience pure extraction (snare); HFT firms see
 *   profitable coordination (tangled rope); regulators provide temporary
 *   relief through circuit breakers (scaffold); and the 'market maker'
 *   designation performs institutional theater while providing minimal
 *   crisis-period liquidity (piton). The constraint's extractiveness has
 *   increased from 0.35 to 0.68 over the measurement period as algorithmic
 *   complexity has grown, while theater ratio (regulatory performance) has
 *   simultaneously increased from 0.38 to 0.55, indicating that circuit
 *   breaker interventions have become more visible without proportionally
 *   reducing the underlying extraction mechanism. The May 2010 Flash Crash,
 *   March 2020 volatility spike, and ongoing volatility events in
 *   low-liquidity assets demonstrate that suppression of alternative trading
 *   venues (dark pools, alternative settlement mechanisms) keeps retail and
 *   institutional investors trapped in the flash-crash-vulnerable exchange
 *   system.
 *
 * KEY AGENTS:
 *   - Retail Investor: Primary victim (powerless/trapped) — cannot detect microsecond trading signals or execute arbitrage; forced to transact at worst prices
 *   - Pension Fund Fiduciary: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with equity investment mandate; cannot exit despite knowing extraction occurs
 *   - Institutional Asset Manager (Non-HFT): Secondary victim (moderate/constrained) — faces high costs to circumvent flash crash exposure through alternative venues or strategies
 *   - High-Frequency Trading Firm: Primary beneficiary (powerful/arbitrage) — captures latency spreads and crisis-period dislocation profits; has agency and exit options
 *   - Exchange Operator: Secondary beneficiary (institutional/arbitrage) — collects trading volume fees during flash events; coordinates order flow
 *   - Regulatory Coalition (SEC/FINRA): Organized actor (organized/constrained) — implements circuit breakers and trading halts with sunset trajectory as market structure improves
 *   - Market Maker (Regulatory Role): Institutional theater (institutional/arbitrage) — designated status provides regulatory cover while liquidity provision becomes performative during stress
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing cascades as inevitable without examining contingent regulatory architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flash_crash_amplification, 0.68).
domain_priors:suppression_score(flash_crash_amplification, 0.72).
domain_priors:theater_ratio(flash_crash_amplification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flash_crash_amplification, extractiveness, 0.68).
narrative_ontology:constraint_metric(flash_crash_amplification, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(flash_crash_amplification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flash_crash_amplification, snare).
narrative_ontology:human_readable(flash_crash_amplification, "Flash Crash Amplification in Automated Trading Systems").
narrative_ontology:topic_domain(flash_crash_amplification, "financial_systems/market_microstructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flash_crash_amplification, high_frequency_trading_firms).
narrative_ontology:constraint_beneficiary(flash_crash_amplification, market_makers_with_speed_advantage).
narrative_ontology:constraint_victim(flash_crash_amplification, retail_investors).
narrative_ontology:constraint_victim(flash_crash_amplification, pension_funds).
narrative_ontology:constraint_victim(flash_crash_amplification, market_stability).
narrative_ontology:constraint_victim(flash_crash_amplification, price_discovery_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Cannot exit the market structure; forced to transact at severely degraded prices during flash events. No ability to detect or respond to microsecond-level trading signals. Maximum extraction through information asymmetry and speed disadvantage. Zero degrees of freedom.
constraint_indexing:constraint_classification(flash_crash_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PENSION FUND FIDUCIARY (SNARE) — Structurally mobile (could hold cash or alternative assets) but identity-locked through fiduciary obligation to rebalance and maintain equity exposure. The duty to invest perpetually in equity markets locks the fiduciary into participation despite knowing extraction occurs. Exit would require abandoning the role itself. Bears disproportionate extraction costs for mass populations.
constraint_indexing:constraint_classification(flash_crash_amplification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR (SNARE) — Traditional asset managers face high costs to exit or circumvent flash crash exposure: alternative venues (dark pools) have their own latency vulnerabilities, index tracking forces participation, and exit to non-equity instruments carries opportunity cost. Significant extraction; moderate agency through venue selection and hedging strategies.
constraint_indexing:constraint_classification(flash_crash_amplification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-FREQUENCY TRADING FIRM (TANGLED ROPE) — Primary beneficiary with arbitrage exit options (can relocate to other venues, shift strategies, or exit entirely). Experiences the constraint as coordination mechanism for capturing latency spreads AND as pure extraction opportunity during flash events. Has agency to exploit or restrain; chooses to exploit. Genuine coordination function (liquidity provision) overlaid with extractive capacity.
constraint_indexing:constraint_classification(flash_crash_amplification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXCHANGE OPERATOR (ROPE) — Coordinates order flow and executes trades; experiences constraint as fee collection mechanism during high-volume flash events. Benefits from trading volume surge; has exit option through regulatory accommodation (circuit breakers, trading halts). Net beneficiary — extraction flows toward this actor.
constraint_indexing:constraint_classification(flash_crash_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (SCAFFOLD) — SEC, FINRA, circuit breaker mechanisms, position limits, and trade-halt protocols represent organized intervention with sunset logic. Suppression capacity is real (can halt trading, impose fees, enforce limits) but designed to decline as market structure improves. As automated trading becomes more sophisticated and regulations mature, the crisis-driven extraction opportunity shrinks. Temporary support structure with declining need.
constraint_indexing:constraint_classification(flash_crash_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: MARKET MAKER ROLE (PITON) — The formal 'market maker' designation provides regulatory cover and fee exemptions that enable HFT firms to operate with minimal friction. The role is largely performative — designated market makers withdraw liquidity during stress periods (exactly when needed) and provide it during calm periods (when competition is highest). The regulatory theater of market maker requirements persists through institutional inertia despite contradicting the stated goal of providing liquidity in crisis.
constraint_indexing:constraint_classification(flash_crash_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a computational perspective, flash crashes are an inherent property of threshold-triggered automated systems: when price thresholds trigger coordinated selling algorithms, the feedback loop is mathematically inevitable. Volatility cascades are structural features of coupled dynamical systems. However, the structural data contradicts the mountain classification — the constraint is contingent on specific regulatory architecture (circuit breaker thresholds, position limits, margin requirements) that could be redesigned. The 'inevitable' framing naturalizes what are actually policy choices.
constraint_indexing:constraint_classification(flash_crash_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flash_crash_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(flash_crash_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(flash_crash_amplification, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(flash_crash_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(flash_crash_amplification, TR),
    TR >= 0.70.

:- end_tests(flash_crash_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. HFT firms capture latency arbitrage (spread between bid and offer created by their speed advantage) and also benefit from flash crash price dislocations when slower traders are forced to execute at crisis prices. The measurement shows clear upward trend (0.35 → 0.68) as algorithmic trading has become more sophisticated and retail participation has increased. Suppression (0.72): Very high. Retail investors have zero technical capacity to avoid flash crashes; institutional investors face high costs to migrate to alternative venues (dark pools have separate risks; international venues have timing challenges). Position limits and circuit breaker thresholds create artificial constraints on market access. Theater ratio (0.55): Moderate-high but increasing. Circuit breaker interventions (trading halts, position limits) are highly visible regulatory theater that demonstrates action without fully eliminating the underlying extraction mechanism. The theater increases from 0.38 to 0.55 as regulators add more visible interventions (SEC Rule 10b-5, FINRA restrictions), but extractiveness continues rising, indicating theater is not matching underlying structural change.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between victims who see pure extraction (snare) and beneficiaries who see profitable coordination (tangled rope). Retail investors perceive the system as predatory and inescapable (snare). Pension fiduciaries perceive the system as extractive but necessary given fiduciary obligations (snare via identity lock, not structural trap). HFT firms perceive the system as a legitimate coordination mechanism where they provide liquidity in exchange for speed advantage (tangled rope: coordination + extraction layered). The exchange operator perceives it as coordination and fee collection (rope). Regulators perceive it as a temporary crisis requiring intervention (scaffold) — but the piton perspective reveals that the visible regulatory interventions (circuit breakers, trading halts) are becoming performative theater while the underlying extraction mechanism persists. The mountain perspective (cascades as mathematical inevitability) is a false summit — flash crashes are contingent on specific regulatory architecture (margin requirements, position limits, maker exemptions) that could be radically redesigned. The fact that different venues and countries experience flash crashes at different frequencies proves they are not inevitable natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Retail investors experience maximum extraction (d ≈ 0.95, f(d) ≈ 1.42) because they are pure victims with no exit options and zero information advantage. Pension fund fiduciaries experience high extraction through identity lock (d ≈ 0.89, f(d) ≈ 1.28) because they have structural mobility but cognitive/fiduciary commitment that prevents exit. HFT firms experience minimal extraction from this constraint (d ≈ 0.05, f(d) ≈ -0.12) because they are primary beneficiaries with arbitrage exit options. The exchange operator experiences negative extraction (they are a pure beneficiary, d ≈ 0.10, f(d) ≈ -0.02). Regulatory agents experience moderate extraction as they balance conflicting mandates (d ≈ 0.50, f(d) ≈ 0.65) — they suppress crashes but also maintain market-maker exemptions and trading volume incentives that enable HFT extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves at high extractiveness (0.68 > 0.70 threshold approached) by distinguishing between the genuine coordination function (HFT firms DO provide liquidity during normal trading) and the extraction mechanism (HFT firms WITHDRAW liquidity during stress periods and benefit from price dislocations). The tangled rope classification for HFT firms is correct — they provide real liquidity services (coordination function, lower spreads during normal periods) while simultaneously extracting through latency advantage and crisis-period profit-taking. The snare classification for retail and institutional investors is correct — they experience pure extraction with no offsetting coordination benefit. Mandatrophy resolution clarifies that this is NOT a hidden snare falsely labeled rope (all victims), nor a rope falsely labeled snare (it has real costs for some agents). It is a genuine snare with a secondary tangled rope perspective, where the coordination function (HFT market-making) is real but subordinate to the extraction function (latency premium and crisis-period dislocation profits). The theater increase (0.38 → 0.55) simultaneously indicates rising regulatory performance without proportional extraction reduction, suggesting classification stability at snare rather than drift toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flash_crash_intentionality,
    'Are flash crashes the result of intentional HFT strategy exploitation or emergent system behavior from innocent algorithmic interactions?',
    'Event forensics comparing crashes with and without human circuit breaker interventions; analysis of HFT position changes pre/post crash; intentionality signatures in order placement patterns',
    'If intentional: snare classification confirmed with active malice. If emergent: classification shifts toward tangled_rope (coordination failure with extraction as side effect). Determines whether extraction is predatory or incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flash_crash_intentionality, empirical, 'Whether flash crashes result from intentional strategy or emergent system behavior').

omega_variable(
    latency_advantage_magnitude,
    'What fraction of HFT profitability derives from speed advantage vs from legitimate volatility arbitrage and market-making functions?',
    'Earnings decomposition analysis comparing HFT firms'' trading activity against latency-neutral baseline strategies; measurement of spreads captured through speed vs market-making compensation',
    'If latency premium > 50% of revenue: extraction-dominant classification confirmed. If latency premium < 20%: coordination function more prominent, justifying tangled_rope over pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latency_advantage_magnitude, empirical, 'Fraction of HFT profitability from latency advantage vs legitimate functions').

omega_variable(
    circuit_breaker_effectiveness_paradox,
    'Do circuit breaker interventions reduce flash crash severity or merely redistribute crashes across timeframes and venues, creating hidden tail risk?',
    'Comparison of market volatility distributions pre/post circuit breaker implementation; analysis of crashes triggered specifically at circuit breaker thresholds; cross-venue spillover analysis',
    'If circuit breakers genuinely reduce crashes: scaffold classification validated — regulatory intervention has real sunset trajectory. If circuit breakers merely hide crashes: classification shifts toward piton (performative theater) or snare (concentrated at threshold boundaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circuit_breaker_effectiveness_paradox, empirical, 'Whether circuit breakers reduce crashes or redistribute them').

omega_variable(
    identity_lock_permanence_for_fiduciary,
    'Can pension fund fiduciaries break their identity lock (equity investment mandate) without legal/regulatory barrier?',
    'Legal analysis of fiduciary duty constraints; survey of pension funds that have shifted to alternative asset classes; cost analysis of mandate changes vs cost of accepting flash crash exposure',
    'If lock is purely identity-based (fiduciaries could change but don''t): exit_options correctly classified as identity_locked. If lock is legally binding: reclassify as trapped (structural barrier). Affects victim experience characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_permanence_for_fiduciary, conceptual, 'Whether pension fiduciary lock is identity-based or structural').

omega_variable(
    market_wide_systemic_risk,
    'Do flash crashes in one asset class or venue create systemic contagion to unrelated markets, or does contagion remain contained to linked instruments?',
    'Cross-market correlation analysis during flash crash events; measurement of spillover to foreign exchanges, commodities, and crypto; network analysis of liquidity provider positions across markets',
    'If systemic contagion is real: scope should be global with higher σ(S) modifier, increasing χ. If contained: scope is regional/national, lowering chi. Affects whether victims are limited to trading market participants or include broader economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_wide_systemic_risk, empirical, 'Systemic contagion risk from flash crashes across markets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flash_crash_amplification, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flash_tr_t0, flash_crash_amplification, theater_ratio, 0, 0.38).
narrative_ontology:measurement(flash_tr_t5, flash_crash_amplification, theater_ratio, 5, 0.48).
narrative_ontology:measurement(flash_tr_t10, flash_crash_amplification, theater_ratio, 10, 0.55).
narrative_ontology:measurement(flash_tr_t15, flash_crash_amplification, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(flash_be_t0, flash_crash_amplification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(flash_be_t5, flash_crash_amplification, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(flash_be_t10, flash_crash_amplification, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(flash_be_t15, flash_crash_amplification, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flash_crash_amplification, resource_allocation).
narrative_ontology:affects_constraint(flash_crash_amplification, dark_pool_migration_trap).
narrative_ontology:affects_constraint(flash_crash_amplification, margin_call_cascade).
narrative_ontology:affects_constraint(flash_crash_amplification, volatility_index_manipulation).

% DUAL FORMULATION NOTE:
% Flash crash amplification represents the extraction layer of a constraint family centered on algorithmic trading. The upstream constraints (HFT capacity build-out, algorithmic order routing) enable flash crashes; the downstream constraints (dark pool fragmentation, margin call cascades) are triggered by flash crash events. Separate stories model the latency advantage mechanism (ε=0.45, tangled rope) and the flash crash contagion mechanism (ε=0.68, snare) as structurally distinct constraints with shared institutional actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flash_crash_amplification, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
