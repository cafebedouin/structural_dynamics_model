% ============================================================================
% CONSTRAINT STORY: gold_fomo_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   The gold price FOMO cycle is a market dynamic where rapid price
 *   appreciation during a gold rally triggers intense media coverage ('gold
 *   is surging,' 'get in before it's too late') that creates psychological
 *   pressure on retail investors to enter the market. Institutional holders
 *   and early entrants benefit from this coordinated liquidity inflow, while
 *   retail investors who chase the rally often enter near peaks and suffer
 *   losses when the momentum reverses. The constraint exhibits tangled rope
 *   structure: it provides a genuine coordination function (price signals do
 *   aggregate information about supply, demand, and macroeconomic
 *   conditions), but this coordination is overlaid with an asymmetric
 *   extraction mechanism where information advantages, timing advantages, and
 *   behavioral herd psychology allow institutional actors to capture
 *   disproportionate gains at the expense of late-entering retail traders.
 *   The theater ratio (0.68) is elevated because much of the rally narrative
 *   is driven by sentiment, media hype, and momentum indicators rather than
 *   fundamental changes in inflation expectations or currency valuations.
 *   Retail entrants believe they are responding to real signals; in fact,
 *   they are responding to amplified hype that benefits early actors.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — enter late, lack execution speed and market information, suffer losses when momentum reverses
 *   - Institutional Gold Holders: Primary beneficiaries (institutional/arbitrage) — early entry, information advantage, ability to exit before retail inflows peak
 *   - High-Frequency Traders: Secondary beneficiaries (institutional/arbitrage) — exploit retail order flow and momentum, extract small rents per trade but in high volume
 *   - Financial Media Outlets: Amplifiers and partial beneficiaries (institutional/arbitrage) — generate advertising revenue and audience engagement from FOMO narratives
 *   - Fintech/Retail Brokers: Intermediaries (powerful/arbitrage) — earn transaction fees and bid-ask spreads, may have conflicts of interest in promoting gold trading
 *   - Regulatory Authorities: Organized reform agents (organized/constrained) — attempting to reduce extraction through circuit breakers, position limits, transparency rules
 *   - Price Discovery Mechanism: Abstract victim (powerless/trapped) — signal corruption from retail momentum creates temporary false price signals that misprice gold relative to fundamentals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fomo_cycle, 0.58).
domain_priors:suppression_score(gold_fomo_cycle, 0.65).
domain_priors:theater_ratio(gold_fomo_cycle, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fomo_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(gold_fomo_cycle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gold_fomo_cycle, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fomo_cycle, tangled_rope).
narrative_ontology:human_readable(gold_fomo_cycle, "The Gold Price 'Fear of Missing Out' Cycle").
narrative_ontology:topic_domain(gold_fomo_cycle, "economic/financial_markets").

domain_priors:requires_active_enforcement(gold_fomo_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, institutional_gold_holders).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, media_financial_outlets).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, high_frequency_traders).
narrative_ontology:constraint_victim(gold_fomo_cycle, retail_investors).
narrative_ontology:constraint_victim(gold_fomo_cycle, late_entry_buyers).
narrative_ontology:constraint_victim(gold_fomo_cycle, price_discovery_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Powerless agents with no exit before capital commitment. Media bombardment about gold rallies creates psychological pressure to enter. Once committed, they are trapped: selling at a loss is psychologically painful and financially destructive. They bear full extraction cost as institutional holders and early entrants dump positions into their buying pressure. Maximum experienced extraction.
constraint_indexing:constraint_classification(gold_fomo_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER TRADER (TANGLED ROPE) — Constrained by capital limits and information lag. Experiences both extraction (enters late in rally, often at peak) and coordination benefit (technical analysis and momentum indicators do provide tradeable signals within the cycle). Extraction is significant but not absolute — traders with timing discipline or hedging strategies can limit losses. Some agency exists.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL HOLDERS (ROPE) — Benefit from coordinated price signaling. Large holders can move markets and exit before retail enters. The constraint functions as pure coordination from their perspective: price signals coordinate liquidity, media attention coordinates retail inflows, and they arbitrage out. Arbitrage options mean they experience the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(gold_fomo_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM (SCAFFOLD) — Organized agents (financial regulators, consumer protection advocates, fintech platforms) are building circuit breakers, position limits, and transparency requirements that reduce the FOMO extraction mechanism. These interventions have a sunset logic: as algorithmic trading and retail access tools become more sophisticated, retail traders gain better information, execution speed, and portfolio protection. The constraint diminishes over time as structural reforms take effect.
constraint_indexing:constraint_classification(gold_fomo_cycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GOLD STANDARD MYTHOLOGY (PITON) — Cultural and historical narrative that gold is inherently a safe haven creates persistent theater around gold pricing. Fintech platforms, media outlets, and gold dealers perpetuate the 'gold hedge' mythology through marketing and editorial content, even as the actual correlation to inflation or currency debasement varies widely. The narrative persists through institutional inertia and cultural reinforcement, not because it robustly predicts outcomes. Theater ratio is high (0.68) because much of the rally is driven by sentiment and momentum, not by fundamentals.
constraint_indexing:constraint_classification(gold_fomo_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the FOMO cycle exhibits both coordination function (price discovery through aggregated demand) and extraction mechanism (information asymmetry between institutional and retail actors, momentum feeding retail entrants). The cycle is neither pure coordination nor pure extraction; it is a hybrid mechanism where real price signals are amplified by behavioral herd dynamics. Media hype and algorithmic amplification of retail flows create asymmetric information extraction atop legitimate supply/demand coordination.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fomo_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gold_fomo_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gold_fomo_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The FOMO cycle extracts wealth from retail entrants through information asymmetry, timing disadvantage, and behavioral pressure. However, not all extraction is captured by institutional actors — much is dissipated in transaction costs and volatility. The value reflects that extraction is real and significant (retail losses are substantial during reversals) but not total (some retail traders do profit, and the price signal itself has real information content). Suppression (0.65): High. Barriers to retail exit before losses include: psychological aversion to loss realization, lack of real-time price information for spot gold, limited access to hedging instruments, and asymmetric media coverage (FOMO narratives promote entry, but exit narratives are muted). Suppression is not absolute because retail brokers do provide exit mechanisms; it is 0.65 rather than 0.85. Theater ratio (0.68): Elevated. The FOMO cycle is substantially narrative-driven. During rallies, media outlets emphasize momentum, sentiment, and 'missed opportunity' angles rather than fundamental shifts in inflation expectations or macroeconomic risk. The performative aspect includes: celebrity endorsements of gold, 'expert' predictions of $3000+ gold with weak empirical backing, and fintech ads emphasizing gold as a hedge without discussing correlation breakdowns. This performance increases as the rally matures (timeline shows 0.42 → 0.68), indicating Goodhart drift — the media focus shifts from explaining fundamentals to amplifying hype.
 *
 * PERSPECTIVAL GAP:
 *   The institutional holder and retail investor experience this constraint oppositely. The institutional actor sees coordination (Rope): price signals drive liquidity, they can time exits, arbitrage opportunities reward speed. The retail investor sees extraction (Snare): they receive the price signal late, cannot time exits, and suffer losses from their entry decision. The analytical observer sees the hybrid (Tangled Rope): both perspectives are structurally accurate. Real coordination happens (macroeconomic information is aggregated into price), but an extraction layer sits atop it (behavioral momentum feeds retail entry at higher prices, allowing early actors to exit). The scaffold perspective (regulatory reform) claims the extraction mechanism is weakening: fintech tools, real-time spot prices, and algorithmic guardrails are reducing retail information lag and behavioral traps. If the scaffold is correct, we expect theater to decline over time as fundamentals reassert and hype loses power; if incorrect, theater will remain high. The piton perspective (gold mythology) notes that much FOMO is perpetuated by durable cultural narratives about gold as a safe haven, even when empirical correlation to inflation is weak. This mythology creates sustained theater independent of actual macroeconomic changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position relative to the extraction flow. Retail investors, as trapped agents with no exit before losses crystallize, experience high d (near 1.0) — they are full targets of extraction, bearing costs while institutional actors benefit. Institutional holders, as arbitrage-enabled beneficiaries, experience low d (near 0.0 or negative) — they benefit from the coordination function and can exit before retail enters. Mid-tier traders experience moderate d (near 0.50) because they have some exit optionality and can profit from timing, but face information and capital constraints that limit their advantage. The organizational reform coalition experiences lower d (near 0.30-0.40) because they are building constraints on extraction; their success means d values rise for retail investors over time (better information, faster execution, automated guards). The directionality pipeline computes these values from the power/exit/beneficiary declaration, producing d-dependent chi values. Retail investors with trapped exit and powerless status derive high d → high f(d) → high effective extraction (chi). Institutional beneficiaries with arbitrage exit and powerful status derive low d → low/negative f(d) → low/negative effective extraction (they experience benefits, not costs).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES AS TANGLED ROPE: The constraint is not pure extraction (Snare) because it does provide genuine price discovery and coordination — the gold market does aggregate information about inflation, currency weakness, and geopolitical risk. Retail entry, though behavioral and late, does contribute real demand signals that help equilibrate supply. If it were pure Snare, there would be no coordination benefit; institutional actors would extract pure rents with zero social function. But institutional traders do provide liquidity and reduce bid-ask spreads, benefiting even retail traders who pay transaction costs. RESOLVES NOT AS PURE COORDINATION (Rope) because the extraction layer is real and significant — information asymmetry, timing disadvantage, and behavioral momentum create systematic losses for retail entrants that exceed fair-market compensation for risk-taking. If it were pure Rope, all perspectives would classify it as Rope; instead, retail see Snare and institutional see Rope. The perspectival gap and the presence of both beneficiaries and victims confirms Tangled Rope. The mandatrophy is resolved by showing that both the coordination and extraction components are structurally necessary: price signals create legitimate demand (retail entry has real economic meaning), but institutional positioning allows them to frontrun and exit before retail-driven peaks (extraction is real). Neither component can be explained away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_fundamental_attribution,
    'How much of the gold price rally is driven by fundamental macroeconomic factors (inflation expectations, currency weakness, geopolitical risk) versus behavioral FOMO and momentum trading?',
    'Vector decomposition of price movements; correlation analysis between gold returns and fundamental indices (inflation surprise, USD weakness, VIX) versus sentiment indicators (social media mentions, retail flow data, options implied volatility); comparison of price trajectories in periods with similar fundamentals but different FOMO amplification',
    'If fundamentals dominate (>70%): the cycle is primarily Rope (legitimate price discovery), not Snare. If FOMO dominates (>50%): the cycle is primarily Snare (extraction). If balanced: Tangled Rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_fundamental_attribution, empirical, 'Attribution of rally to fundamentals versus behavioral FOMO').

omega_variable(
    retail_exit_window_duration,
    'What percentage of retail investors who enter during a FOMO rally actually exit at a loss, and what is the typical holding duration before capital loss recognition?',
    'Longitudinal tracking of retail account flows (from retail brokers, fintech platforms) correlated with account closures and loss documentation; analysis of tax-loss harvesting patterns; survey data on retail trader sentiment before/after rallies',
    'If >60% exit at loss within 12 months: Snare classification is validated (extraction is severe). If <30% exit at loss: Tangled Rope classification is strengthened (retail gain some benefit despite late entry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_exit_window_duration, empirical, 'Percentage and timing of retail losses from FOMO entry').

omega_variable(
    information_asymmetry_closure_rate,
    'Is the information asymmetry between institutional and retail actors increasing (as retail tools improve) or decreasing (as institutional access widens)?',
    'Comparison of execution speed metrics (latency) for retail vs institutional traders; analysis of bid-ask spreads for retail vs wholesale gold futures; tracking of retail access to real-time spot prices and derivative instruments; measurement of retail trading volume concentration in time windows relative to institutional flows',
    'If asymmetry is decreasing: Scaffold perspective is correct — extraction mechanism is being weakened structurally. If stable: Snare and Tangled Rope extraction will persist. If increasing: Snare extraction will worsen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_closure_rate, empirical, 'Trend in information asymmetry between retail and institutional traders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fomo_cycle, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_fomo_tr_t0, gold_fomo_cycle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gold_fomo_tr_t3, gold_fomo_cycle, theater_ratio, 3, 0.58).
narrative_ontology:measurement(gold_fomo_tr_t6, gold_fomo_cycle, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(gold_fomo_be_t0, gold_fomo_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gold_fomo_be_t3, gold_fomo_cycle, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(gold_fomo_be_t6, gold_fomo_cycle, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fomo_cycle, resource_allocation).
narrative_ontology:affects_constraint(gold_fomo_cycle, crypto_retail_pump_and_dump).
narrative_ontology:affects_constraint(gold_fomo_cycle, meme_stock_momentum_extraction).
narrative_ontology:affects_constraint(gold_fomo_cycle, commodity_price_momentum_herd).

% DUAL FORMULATION NOTE:
% The gold FOMO cycle is one manifestation of a broader retail/institutional extraction pattern across financial markets. The family includes crypto pump-and-dump (higher ε ~0.72, pure Snare), meme stock momentum (higher suppression, intermediate ε ~0.62), and commodity momentum generally (similar ε ~0.58). All share the same structural property: information asymmetry + behavioral herd + momentum → retail extraction. The gold cycle has slightly lower extractiveness than crypto because fundamental macroeconomic factors do anchor gold pricing; crypto relies almost entirely on sentiment. Link them via network to model contamination: if retail confidence in 'gold as hedge' breaks down (after losses), they may lose confidence in other assets with similar FOMO characteristics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fomo_cycle, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
