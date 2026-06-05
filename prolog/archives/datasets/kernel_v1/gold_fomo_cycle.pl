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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gold_fomo_cycle
 *   human_readable: Gold Price FOMO Cycle
 *   domain: economic/financial_markets
 *
 * SUMMARY:
 *   The gold price FOMO cycle is a recurring market phenomenon where rapid
 *   price appreciation during a rally triggers synchronized media
 *   amplification ('gold is surging,' 'get in before it's too late') that
 *   psychologically pressures retail investors to enter near peaks.
 *   Institutional holders and early entrants benefit from the coordinated
 *   liquidity inflow, while retail investors who chase the rally
 *   systematically enter at unfavorable prices and suffer losses when
 *   momentum reverses. The constraint exhibits tangled rope structure: it
 *   contains a genuine coordination function (price discovery, liquidity
 *   provision) alongside asymmetric extraction (beneficiaries harvest retail
 *   order flow, media amplifies narratives that serve institutional
 *   interests). The measurement trajectory shows extractiveness rising from
 *   baseline (0.30) during the accumulation phase through peak (0.62) as
 *   retail participation saturates, then moderating (0.58) as early
 *   position-taking begins. Theater ratio climbs steeply (0.35 → 0.72) during
 *   the peak media saturation phase, indicating that financial commentary
 *   becomes increasingly performative (narrative is the product, not
 *   analysis). The cycle's key structural feature is the **information lag
 *   and asymmetry**: institutional actors with early positioning and market
 *   insight can time entries and exits based on order flow data, while retail
 *   investors receive market-moving information only after it has been
 *   amplified through public media channels.
 *
 * KEY AGENTS:
 *   - Institutional Holders: Primary beneficiary (institutional/arbitrage) — benefit from coordinated retail liquidity inflow; time exits before retail saturation; face no suppression
 *   - Early Entrants: Primary beneficiary (powerful/arbitrage) — capture first-mover price appreciation and time exits into retail enthusiasm
 *   - Retail Investors Chasing Rally: Primary victim (powerless/trapped) — enter near peaks; face information asymmetry and psychological urgency pressure; systematic losers in cycle
 *   - Financial Media: Secondary beneficiary (institutional/arbitrage) — profit from reader engagement during volatile rallies; incentivize coverage of 'hot' stories (surging prices) rather than rational valuation analysis
 *   - Semi-Informed Retail Traders: Secondary victim (moderate/constrained) — understand FOMO mechanism but lack capital scale to arbitrage it; face emotional and leverage constraints
 *   - Central Banks / Macro Investors: Complex position (organized/constrained) — benefit from retail demand for price support but constrained by mandate to maintain currency stability; experience mixed coordination and extraction
 *   - Market Price Discovery: Victim role (powerless/trapped) — the overall accuracy and efficiency of gold price as signal is degraded by FOMO-driven volume; no agent represents the abstract function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fomo_cycle, 0.58).
domain_priors:suppression_score(gold_fomo_cycle, 0.62).
domain_priors:theater_ratio(gold_fomo_cycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fomo_cycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(gold_fomo_cycle, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fomo_cycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fomo_cycle, tangled_rope).
narrative_ontology:human_readable(gold_fomo_cycle, "Gold Price FOMO Cycle").
narrative_ontology:topic_domain(gold_fomo_cycle, "economic/financial_markets").

domain_priors:requires_active_enforcement(gold_fomo_cycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, institutional_holders).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, early_entrants).
narrative_ontology:constraint_beneficiary(gold_fomo_cycle, financial_media).
narrative_ontology:constraint_victim(gold_fomo_cycle, retail_investors).
narrative_ontology:constraint_victim(gold_fomo_cycle, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped by information asymmetry, FOMO psychology, and the illusion of time-limited opportunity. When media coverage intensifies, the retail investor perceives that entry costs are rising (prices climbing) and urgency is high. Exit is structurally available (they can choose not to buy) but psychologically suppressed by the designed urgency narrative. Once invested near the peak, they face the extracted cost: paper losses when momentum reverses. Maximum experienced extraction.
constraint_indexing:constraint_classification(gold_fomo_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL HOLDERS / EARLY ENTRANTS (ROPE) — Benefit from the coordinated liquidity inflow triggered by retail FOMO. Their timing creates genuine coordination: by holding through the rally and selling into retail demand, they enable price discovery and provide liquidity. They experience the constraint as pure coordination with no suppression — they have full exit options (can sell at any time) and face no information barriers.
constraint_indexing:constraint_classification(gold_fomo_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SEMI-INFORMED RETAIL TRADERS (TANGLED ROPE) — Understand the FOMO cycle mechanism and have access to some technical analysis or trading signals but face capital constraints and emotional pressure. They experience the cycle as both coordination (price discovery through volume) and extraction (their order flow is harvested at unfavorable prices by algorithms). Constrained by account size, leverage rules, and opportunity costs — they cannot simply arbitrage the institutions.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL MEDIA (PITON) — Ostensibly educates retail investors about gold markets. In practice, the media cycle is substantially performative theater: coverage intensifies precisely during rallies (when the news is 'hot'), triggering FOMO, and disappears during consolidation or declines. The media's stated function (inform) has atrophied; the actual function (amplify momentum during rallies) is what persists. Theater ratio is high because the narrative framing ('gold is surging, should you get in?') is the product sold to readers and advertisers, not analysis of whether entry is rational at current prices.
constraint_indexing:constraint_classification(gold_fomo_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANKS / MACRO INVESTORS (TANGLED ROPE) — Hold large gold reserves and benefit from coordinated retail demand increasing spot prices, but are also constrained by geopolitical and currency stability considerations. They experience both coordination (gold as monetary anchor during crises, enabling hedging function for many agents) and extraction (when retail FOMO drives prices to levels disconnected from fundamental value, reducing the hedging efficiency of gold positions). Their exit options are constrained by political economy.
constraint_indexing:constraint_classification(gold_fomo_cycle, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, behavioral finance identifies FOMO and momentum-following as immutable features of human psychology. This perspective sees the cycle as an inherent law of market dynamics — information asymmetry, recency bias, and herd behavior are features of cognition, not contingent institutional arrangements. However, the structural data contradicts this: identifiable beneficiaries (institutions, media) benefit from the cycle's perpetuation through designed narratives, suggesting the 'natural law' framing naturalizes extractive institutions.
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
 *   Extractiveness (0.58): High but not maximum. The institutional beneficiaries capture significant value from the retail order flow (estimated 2-8% extraction per cycle based on timing gaps and slippage analysis), but the extraction is not as severe as pure snare (0.72+) because retail investors retain some agency — they can choose not to chase, and some do learn and adapt over multiple cycles. The moderate extractiveness reflects that the constraint is hybrid (coordination + extraction) rather than pure extraction. Suppression (0.62): High. Multiple suppression layers operate: (1) Information asymmetry — retail investors receive market-moving information only after institutional actors have positioned; (2) Psychological urgency — media narrative creates false time constraint ('get in before it's too late'), reducing rational deliberation; (3) Capital constraints — retail investors cannot scale positions to arbitrage the institutions; (4) Market structure — retail orders are often routed through venues that provide order flow to institutional market makers, creating a structural disadvantage. Suppression rises to 0.68 at peak cycle (maximum FOMO period) then declines to baseline as cycle completes. Theater Ratio (0.65): Moderately high. Financial media during gold rallies is substantially performative: headlines emphasize sentiment and price action ('gold surges to 10-year high') rather than fundamental value analysis ('real yields have fallen, here is the justified target'). The narrative framing is designed to trigger retail engagement, not to educate rational decision-making. Peak theater (0.72) occurs 3-6 months into the rally when media coverage saturates and becomes repetitive, but the performativity is the product sold to readers and advertisers.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is unusually wide and reveals the information architecture of financial markets. The same price rally appears as (1) coordination to those with early access and timing, (2) extraction to those with late access and information delay, (3) hybrid to those with partial information, (4) performative theater to media institutions, and (5) a natural law to analytical observers who naturalize behavioral biases. This is not merely different opinions on the same fact — it is different experienced structures depending on market position. The gap is the gap between market microstructure (who gets information first, who can execute what orders) and how that microstructure is experienced by different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from structural position: Institutional beneficiaries with arbitrage exits get d ≈ 0.05 (near-zero extraction from their perspective) → f(d) ≈ -0.12 → negative chi (they experience coordination, not extraction). Retail victims with trapped exits get d ≈ 0.95 (maximum extraction from their perspective) → f(d) ≈ 1.42 → high chi (they experience severe extraction). Semi-informed traders with constrained exits and moderate power get d ≈ 0.70 → f(d) ≈ 1.10 → moderate-high chi. The directionality values encode the information and timing asymmetries that drive the cycle. No overrides are needed — the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled_rope (hybrid coordination + extraction) rather than snare-only. The coordination function is real: the market mechanism for price discovery and liquidity provision exists and functions. The extraction is also real: this mechanism produces systematic asymmetric outcomes favoring early entrants and institutional actors. A pure snare classification would miss the coordination function; a pure rope classification would miss the extraction. The tangled rope resolves by declaring both beneficiaries (who gain from coordination) and victims (who lose from asymmetric extraction), with active enforcement (the market microstructure that creates information lag and timing advantage). The false summit risk is on the analytical perspective — treating FOMO as a natural law of human psychology rather than as a manufactured institutional dynamic that benefits identifiable agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fomo_psychology_vs_structure,
    'Is the FOMO response an inherent feature of human psychology or a manufactured artifact of media and market design?',
    'Cross-cultural comparison of retail investor behavior; analysis of FOMO intensity before vs after media narrative standardization (pre-1990s vs post-internet); experimental psychology studies isolating narrative-driven vs price-signal-driven investment decisions',
    'If inherent psychology: constraint reclassifies toward mountain (natural law of markets). If manufactured: constraint remains tangled_rope/snare, revealing that suppression is institutional design choice, not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fomo_psychology_vs_structure, empirical, 'Whether FOMO is inherent psychology or manufactured by institutional narrative').

omega_variable(
    retail_vs_institutional_timing_gap,
    'What is the average time lag between institutional entry/exit and retail entry/exit during FOMO cycles?',
    'Order flow analysis from brokerages; correlation of institutional transaction timing with retail volume surges; timestamp analysis of media narrative intensification vs retail order clustering',
    'If lag is systematically 2-4 weeks: institutions have clear information advantage enabling extraction (snare). If lag is <1 week: institutions are not systematically front-running retail (coordinate more than extract). If lag is >4 weeks: retail behavior is delayed learning, not FOMO-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_vs_institutional_timing_gap, empirical, 'Timing gap between institutional and retail entry during rallies').

omega_variable(
    fundamental_value_anchor,
    'Does gold have a stable fundamental value anchor (inflation hedge, real yield floor, currency insurance), or is valuation entirely sentiment-driven?',
    'Long-term correlation analysis (50+ years) of gold prices vs inflation, real interest rates, currency indices, geopolitical uncertainty indices; identification of reversion patterns and anchor mechanisms',
    'If anchor exists: FOMO cycle is temporary deviation from coordination (rope), extractiveness is lower. If no anchor: price is purely momentum-driven, extraction is higher (snare), constraint has no natural reversion mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_value_anchor, empirical, 'Whether gold has stable fundamental value anchor or is sentiment-driven').

omega_variable(
    media_narrative_causation,
    'Does intensified media coverage CAUSE retail entry surges (media drives FOMO) or merely REFLECT retail demand that arises from price momentum itself?',
    'Granger causality test of media sentiment intensity vs retail order flow; identification of coverage initiators (who publishes first, mainstream press or retail communities); timeline analysis of headline clustering vs order clustering',
    'If media causes retail flow: suppression is manufactured by institutional information control (snare). If media reflects retail demand: FOMO is organic market response (rope/coordinate). If bidirectional: tangled_rope model holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(media_narrative_causation, empirical, 'Whether media narratives cause or reflect retail FOMO behavior').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression (inability to exit during FOMO) structural (market/capital barriers) or internalized (belief that missing the train is irreversible loss)?',
    'Post-cycle surveys of retail investors: quantitative analysis of barriers cited (capital constraints, leverage limits, emotional pressure); comparison of stated reasons for entry vs actual material constraints; analysis of retail participation in subsequent cycles (learning or repeat behavior)',
    'If structural: suppression ≥ 0.62 is correct. If internalized: retail investors could escape by recognizing FOMO as cognitive trap (suggest identity_locked exit option instead of trapped). If both: suppression is even higher (structural + internalized layers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression is structural barrier or internalized belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fomo_cycle, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gfc_tr_t0, gold_fomo_cycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gfc_tr_t3, gold_fomo_cycle, theater_ratio, 3, 0.55).
narrative_ontology:measurement(gfc_tr_t6, gold_fomo_cycle, theater_ratio, 6, 0.72).
narrative_ontology:measurement(gfc_tr_t9, gold_fomo_cycle, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(gfc_be_t0, gold_fomo_cycle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gfc_be_t3, gold_fomo_cycle, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(gfc_be_t6, gold_fomo_cycle, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(gfc_be_t9, gold_fomo_cycle, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gfc_su_t0, gold_fomo_cycle, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gfc_su_t6, gold_fomo_cycle, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fomo_cycle, resource_allocation).
narrative_ontology:affects_constraint(gold_fomo_cycle, currency_carry_trade_extraction).
narrative_ontology:affects_constraint(gold_fomo_cycle, retail_futures_leverage_trap).
narrative_ontology:affects_constraint(gold_fomo_cycle, commodity_index_fund_momentum).

% DUAL FORMULATION NOTE:
% The gold FOMO cycle is downstream of commodity price momentum broadly but represents a distinct constraint structure. Related constraints (currency carry, leverage traps, index-driven momentum) all interact via the same information asymmetry and retail-institutional timing gap mechanisms. Each has its own extractiveness value: currency carry (0.45, rope/tangled_rope hybrid), leverage trap (0.72, snare), index momentum (0.50, tangled_rope). The family captures how financial market microstructure produces systematic asymmetric outcomes across multiple asset classes and instruments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
