% ============================================================================
% CONSTRAINT STORY: retail_investor_information_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_retail_investor_information_asymmetry, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: retail_investor_information_asymmetry
 *   human_readable: Retail Investor Information Asymmetry in Equity Markets
 *   domain: financial_markets/information_economics
 *
 * SUMMARY:
 *   Retail investor information asymmetry in equity markets represents a
 *   structural constraint where one set of market participants (institutions,
 *   market makers, high-frequency traders) access information, research, and
 *   execution capabilities that another set (retail investors) systematically
 *   lacks. This asymmetry is foundational to modern market microstructure: it
 *   enables price discovery (a genuine coordination function) while
 *   simultaneously enabling extraction (retail capital systematically
 *   underperforms). The constraint exhibits tangled rope characteristics: the
 *   market genuinely coordinates capital allocation and enables retail
 *   participation, yet the same mechanisms that perform this coordination
 *   extract value through spreads, adverse selection, and timing
 *   disadvantages. Extractiveness has increased from 0.38 to 0.58 over the
 *   measurement interval (20 years covering 2006–2026), driven by algorithmic
 *   trading complexity and market fragmentation. Theater ratio remains
 *   moderate (0.35→0.48) because the asymmetry is partially transparent
 *   (investors know they face spreads and timing risks) rather than purely
 *   performed. The constraint involves active enforcement: market regulations
 *   (Reg FD, tick sizes, circuit breakers) actively maintain the information
 *   hierarchy while nominally protecting retail interests. Regulatory
 *   scaffolding (zero-commission trading, fractional shares, retail education
 *   mandates) represents partial solutions with sunset logic: as technology
 *   democratizes information access, the asymmetry's structural foundation
 *   erodes.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victim (powerless/trapped) — structurally unable to exit participation while bearing full information disadvantage cost
 *   - Institutional Investors: Primary beneficiary (institutional/arbitrage) — leverage information advantage for alpha generation; can exit to alternative markets/strategies
 *   - Broker-Dealers: Secondary beneficiary and enforcer (institutional/constrained) — monetize order flow and spreads while providing coordination service; constrained by regulatory limits
 *   - Market Makers: Beneficiary agent (powerful/arbitrage) — profit from spreads enabled by retail participation; have exit options to other markets
 *   - High-Frequency Traders: Secondary beneficiary (analytical/arbitrage) — extract timing value from information asymmetry; operate at scale beyond retail visibility
 *   - Regulatory Coalition: Organized solver (organized/constrained) — SEC, FINRA, consumer advocates building scaffold through transparency mandates and access democratization
 *   - Analytical Observer: Civilizational structure — sees dual function (coordination + extraction) as simultaneously structural and potentially remediable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(retail_investor_information_asymmetry, 0.58).
domain_priors:suppression_score(retail_investor_information_asymmetry, 0.65).
domain_priors:theater_ratio(retail_investor_information_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(retail_investor_information_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(retail_investor_information_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(retail_investor_information_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(retail_investor_information_asymmetry, tangled_rope).
narrative_ontology:human_readable(retail_investor_information_asymmetry, "Retail Investor Information Asymmetry in Equity Markets").
narrative_ontology:topic_domain(retail_investor_information_asymmetry, "financial_markets/information_economics").

domain_priors:requires_active_enforcement(retail_investor_information_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(retail_investor_information_asymmetry, institutional_investors).
narrative_ontology:constraint_beneficiary(retail_investor_information_asymmetry, market_makers).
narrative_ontology:constraint_beneficiary(retail_investor_information_asymmetry, financial_advisors).
narrative_ontology:constraint_beneficiary(retail_investor_information_asymmetry, broker_dealers).
narrative_ontology:constraint_victim(retail_investor_information_asymmetry, retail_investors).
narrative_ontology:constraint_victim(retail_investor_information_asymmetry, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Structurally trapped in asymmetric information environment. Limited access to real-time data, proprietary research, and algorithmic insights. High suppression: psychological pressure of FOMO, sunk cost fallacy, and complexity. No meaningful exit: participating in markets requires submitting to the information hierarchy. Maximum experienced extraction through timing disadvantage, hidden spreads, and adverse selection.
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RETAIL COLLECTIVE / ORGANIZED GROUPS (TANGLED ROPE) — When organized through forums, social media, or retail investor coalitions, retail actors gain negotiating capacity but remain structurally constrained by capital requirements and access barriers. Experience genuine coordination (pooling research, identifying market inefficiencies) alongside extraction (front-running by institutions, order flow monetization). Constrained exit due to opportunity cost and belief in eventual market participation benefits.
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTORS (ROPE) — Experience the information asymmetry as a pure coordination mechanism: specialized research, direct company access, and real-time data streams enable efficient capital allocation and price discovery. The same constraint that extracts from retail investors creates profitable coordination for institutions. Net beneficiary position with full exit options (arbitrage to other markets, asset classes, geographies).
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BROKER-DEALERS (TANGLED ROPE) — Simultaneously coordinate market liquidity and extract through order flow monetization, payment-for-order-flow, bid-ask spreads, and margin requirements. Genuine coordination function (matching buyers/sellers, enabling retail participation) coexists with asymmetric extraction (selling retail order flow to high-frequency traders, marking up spreads). Constrained by regulatory limits on exploitative practices but retain arbitrage options through regulatory arbitrage.
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — SEC disclosure requirements, Reg FD (Fair Disclosure), payment-for-order-flow restrictions, and retail investor protections represent temporary scaffolding to address information asymmetry. Organized agents (regulators, consumer advocates, some institutional investors) see the constraint as solvable through transparency mandates and access democratization. Sunset clause implicit: as technology enables direct market access and real-time data availability, the asymmetry's structural foundation erodes. Theater remains moderate because reforms have genuine functional content (reducing opacity) rather than pure performance.
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EFFICIENT MARKET HYPOTHESIS (PITON) — The foundational assumption that markets aggregate information efficiently and that retail participation adds value is increasingly theatrical. As algorithmic trading and institutional dominance increase, the EMH becomes a degraded institutional narrative maintained through inertia rather than empirical validity. Market efficiency is announced but not performed; the information asymmetry persists despite efficiency rhetoric. Theater ratio high because market efficiency narratives persist despite mounting evidence of exploitation mechanisms.
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, the information asymmetry serves dual functions: it genuinely enables price discovery and capital allocation coordination (institutional research allocating capital efficiently) while simultaneously enabling extraction (retail investors subsidizing institutional alpha through adverse selection). Both functions are structural, not contingent. The constraint persists because it efficiently (in the economic sense) performs both coordination and extraction simultaneously. Decoupling them would require fundamentally restructuring market microstructure.
constraint_indexing:constraint_classification(retail_investor_information_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(retail_investor_information_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(retail_investor_information_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(retail_investor_information_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(retail_investor_information_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(retail_investor_information_asymmetry, TR),
    TR >= 0.70.

:- end_tests(retail_investor_information_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and increasing over the measurement interval. Retail investor alpha (portfolio returns minus risk-adjusted benchmarks) systematically negative by 1-3 percentage points annually, driven primarily by timing disadvantage (buying high, selling low due to information lag), spreads, and adverse selection. The 0.38→0.58 trajectory reflects algorithmic trading acceleration and market fragmentation increasing the information gap. Not maximal (0.70+) because some extraction is offset by genuine access improvements (zero-commission trading, fractional shares, retail trading platforms). Suppression (0.65): High and structural. Psychological suppression (FOMO, overconfidence bias, complexity-induced paralysis) combines with structural suppression (capital requirements, access barriers to institutional-grade data, technical knowledge thresholds). Exit requires either accepting information disadvantage or withdrawing entirely from markets. Theater ratio (0.48): Moderate. Efficient market hypothesis narratives and market efficiency rhetoric persist despite mounting behavioral evidence and structural disadvantage proofs. But asymmetry is partially transparent — retail investors are aware of spreads and timing risks, reducing pure theatrical component. Theater has increased from 0.35 to 0.48 as complexity has escalated (dark pools, algorithmic execution, fractional-second timing) and regulatory theater has expanded (disclosure mandates that appear protective but preserve core asymmetry mechanisms).
 *
 * PERSPECTIVAL GAP:
 *   The informational asymmetry is transparently present to all actors yet experienced completely differently. Retail investors perceive immutable market structure (mountain-equivalent naivete). Institutions perceive efficient coordination (rope). Broker-dealers perceive legitimate business model (tangled rope). Reformers perceive solvable coordination problem (scaffold). Empirically, the constraint shows signatures of all types: the snare extraction (persistent underperformance), the coordination function (price discovery), the active enforcement (regulatory frameworks preserving spreads), and the theatrical degradation (EMH rhetoric). The perspectival gap reveals the constraint's true nature: it is tangled rope maintained by active enforcement — genuine coordination function coexisting with asymmetric extraction, stabilized by regulatory architecture that preserves the spread/flow-monetization extraction mechanisms while appearing to protect retail interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural data: Beneficiaries are institutional investors, market makers, broker-dealers (extract through information advantage, spreads, order flow). Victims are retail investors (bear information disadvantage, timing costs, hidden spreads) and market price discovery (distorted by retail order patterns and information-driven adverse selection). Retail investors' power is powerless (individual capacity); exit is trapped (participation requires accepting asymmetry). This derives d ≈ 0.88–0.95 (near-maximal target), f(d) ≈ 1.30 (strong extraction coefficient). Institutional investors' power is institutional; exit is arbitrage (can move to alternative markets, strategies, geographies). This derives d ≈ 0.10–0.15 (near-minimal beneficiary), f(d) ≈ -0.05 (negative extraction, they receive benefit). Broker-dealers are institutional but constrained by regulation; their dual role (liquidity provider and order-flow extractor) produces d ≈ 0.35 (moderate target position despite beneficiary classification). Organized retail groups (when coalition capacity exists) shift agent_power to organized and exit_options to constrained, producing d ≈ 0.55, f(d) ≈ 0.80 — moderate extraction rather than snare-level maximum. Scope modifier σ(global)=1.2 amplifies all χ values, making the constraint significant at systemic scale.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint avoids false classification as pure rope (coordination only) or pure snare (extraction only) by explicitly declaring both coordination and extraction functions. Coordination: price discovery, capital allocation efficiency, liquidity provision. Extraction: retail adverse selection, spreads, timing disadvantage, psychological exploitation. Both are structural, not contingent. The constraint is NOT solvable by eliminating information asymmetry entirely — some information lag is inherent to market structure. The constraint IS remediable through reduction: regulatory frameworks that narrow spreads, reduce dark pool opacity, and slow algorithmic execution times lower extractiveness while preserving price discovery. The active enforcement gate (requires_active_enforcement: true) correctly identifies that information asymmetry is not natural emergence but maintained through market microstructure design (circuit breakers, fee structures, order routing rules). The beneficiary/victim split confirms tangled rope: both groups exist, both are real, extraction runs from victims to beneficiaries. Chi formula χ = ε × f(d) × σ(S) = 0.58 × variable(agent-dependent) × 1.2 produces high effective extraction at global scope while remaining below snare thresholds (χ ≥ 0.66 across most perspectives) due to moderate baseline extractiveness and beneficiary presence. Mandatrophy is resolved by recognizing that institutional participation in the market genuinely depends on some information advantage (their alpha requires abnormal returns relative to retail); completely eliminating this advantage would eliminate incentives for institutional research and capital allocation. The equilibrium constraint persists because it efficiently balances coordination and extraction — reducing extraction further would reduce coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retail_access_democratization,
    'Will retail access to real-time data, research tools, and algorithmic trading eliminate the information asymmetry or merely shift extraction mechanisms?',
    'Longitudinal tracking of retail investor alpha/returns post-access democratization; correlation between technology adoption (fractional shares, zero-commission trading, retail trading platforms) and retail portfolio performance relative to benchmarks',
    'If asymmetry eliminates: constraint shifts to rope (pure coordination). If extraction persists: new extraction mechanism emerges (psychological manipulation, complexity escalation, liquidity provision requirements), and constraint reclassifies as snare with changed beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_access_democratization, empirical, 'Whether democratized access eliminates information asymmetry or shifts extraction').

omega_variable(
    behavioral_exploitation_vs_information_advantage,
    'What proportion of retail investor underperformance derives from information asymmetry (can be solved by access) versus behavioral/psychological factors (cannot be solved by information alone)?',
    'Comparative analysis: retail investors with information parity (participating in information-privileged platforms) versus control groups; behavioral analysis of trading patterns; correlation between access to information and portfolio performance controlling for behavioral variables',
    'If high behavioral component: information asymmetry constraint is misspecified — the real constraint is psychological, and expanding access does not address the core extraction mechanism. Reclassify to snare of behavioral manipulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_exploitation_vs_information_advantage, empirical, 'Proportion of underperformance from information asymmetry versus behavior').

omega_variable(
    market_microstructure_sustainability,
    'Can retail participation expand without institutional market-making beneficiary position degrading, or is retail extraction inherent to current market microstructure design?',
    'Analysis of bid-ask spreads, order execution quality, and institutional alpha across markets with varying retail participation rates; comparison of market microstructure in retail-dominated versus institutional-dominated venues',
    'If extraction inherent to design: constraint is tangled rope sustained by active enforcement (microstructure regulations that preserve spreads and order flow value). If not inherent: extraction can be reduced without architectural change, supporting scaffold classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_microstructure_sustainability, empirical, 'Whether retail extraction is inherent to market microstructure design').

omega_variable(
    regulatory_arbitrage_persistence,
    'As retail protections strengthen in regulated markets, will extraction migrate to alternative venues (crypto, offshore, OTC) or will unified regulation eliminate arbitrage opportunities?',
    'Tracking of retail capital flows across regulatory boundaries; correlation between regulatory tightening in primary markets and retail participation in less-regulated alternatives; analysis of extraction mechanism migration',
    'If extraction migrates: regulatory scaffold is illusory — suppression persists through arbitrage. If unified: scaffold genuine and extraction reduces over time. If migration is partial: constraint remains tangled rope with regulatory enforcement preventing but not eliminating extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_persistence, empirical, 'Whether regulatory protection or arbitrage determines retail extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(retail_investor_information_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(riia_tr_t0, retail_investor_information_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(riia_tr_t10, retail_investor_information_asymmetry, theater_ratio, 10, 0.41).
narrative_ontology:measurement(riia_tr_t20, retail_investor_information_asymmetry, theater_ratio, 20, 0.48).
narrative_ontology:measurement(riia_tr_t5, retail_investor_information_asymmetry, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(riia_be_t0, retail_investor_information_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(riia_be_t10, retail_investor_information_asymmetry, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(riia_be_t20, retail_investor_information_asymmetry, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(riia_be_t5, retail_investor_information_asymmetry, base_extractiveness, 5, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(retail_investor_information_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(retail_investor_information_asymmetry, 0.18).
narrative_ontology:affects_constraint(retail_investor_information_asymmetry, high_frequency_trading_extraction).
narrative_ontology:affects_constraint(retail_investor_information_asymmetry, retail_broker_dealer_conflicts_of_interest).
narrative_ontology:affects_constraint(retail_investor_information_asymmetry, dark_pool_opacity).
narrative_ontology:affects_constraint(retail_investor_information_asymmetry, equity_market_price_discovery).

% DUAL FORMULATION NOTE:
% Information asymmetry decomposes into multiple structurally distinct constraints: (1) baseline information lag (time required for public information to propagate) — ε=0.08, mountain, (2) institutional information advantage (proprietary research, early access) — ε=0.35, tangled rope, (3) algorithmic extraction (microsecond timing advantages) — ε=0.42, snare. The retail_investor_information_asymmetry story models the combined effect (ε=0.58). Upstream mountain constraint (information lag) makes institutional advantage possible; downstream snares (HFT, payment-for-order-flow) operationalize extraction. Each decomposed constraint has its own story; all link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(retail_investor_information_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
