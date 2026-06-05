% ============================================================================
% CONSTRAINT STORY: djia_as_economic_barometer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_djia_as_economic_barometer, []).

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
 *   constraint_id: djia_as_economic_barometer
 *   human_readable: The Dow Jones Industrial Average as a primary barometer of national economic health
 *   domain: economic_policy/financial_markets
 *
 * SUMMARY:
 *   The Dow Jones Industrial Average has served as the primary cultural
 *   barometer of U.S. economic health since its inception in 1896, but its
 *   dominance as a metric masks a structural constraint: the DJIA is a
 *   price-weighted index of 30 large-cap equities representing approximately
 *   25% of total U.S. market capitalization and benefiting primarily equity
 *   investors, while excluding the 92 million working-age Americans not
 *   holding significant stock portfolios. This constraint operates as a
 *   Tangled Rope with Snare characteristics — it provides genuine
 *   coordination benefits (price discovery, capital allocation signals) while
 *   simultaneously extracting from labor by subordinating employment outcomes
 *   to equity price targets in monetary and fiscal policy. The constraint
 *   exhibits all of Mandatrophy's archetypal tensions: beneficiaries (equity
 *   investors, asset managers, corporate executives using buyback-driven
 *   stock appreciation) genuinely depend on transparent price signals;
 *   victims (working-class wage earners, small business owners, the labor
 *   market's structural stability) bear extraction costs through suppressed
 *   wage growth, employment volatility, and erosion of labor's policy voice.
 *   The theater_ratio (0.68) reflects that much contemporary DJIA coverage is
 *   performative — financial media reports minute-to-minute price movements
 *   as 'economic health updates' despite zero connection to employment,
 *   production, or real income. The extractiveness (0.58) captures the
 *   systematic policy bias toward equity price targets, quantifiable through
 *   Fed communication analysis and monetary policy transmission studies. The
 *   constraint's lifecycle shows increasing theater and extractiveness as
 *   equity ownership concentration has grown and passive indexing has
 *   decoupled stock prices from fundamental earnings.
 *
 * KEY AGENTS:
 *   - Equity Investors and Asset Managers: Primary beneficiary (institutional/arbitrage) — benefit from index inclusion, passive inflows, and policy bias toward asset price stability
 *   - Working-Class Wage Earners: Primary victim (powerless/trapped) — experience subordination of wage growth to equity price targets; no exit from labor market
 *   - Labor Market Stability: Structural victim (powerless/trapped) — abstract collective good; employment volatility treated as residual cost of 'healthy' markets
 *   - Small Business Owners: Secondary victim (moderate/constrained) — capital access subordinated to equity market sentiment
 *   - The Federal Reserve: Institutional actor with constrained exit (institutional/constrained) — enforces the constraint through dual-mandate prioritization favoring equity prices
 *   - Financial Media: Beneficiary (institutional/arbitrage) — generates engagement through DJIA reporting; incentivized to treat price movements as economic news
 *   - Economic Policymakers: Institutional actor with performative function (institutional/constrained) — continue DJIA focus through political convenience despite acknowledged limitations
 *   - Alternative Metrics Coalition: Organized counter-agents (organized/mobile) — building parallel measurement systems with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(djia_as_economic_barometer, 0.58).
domain_priors:suppression_score(djia_as_economic_barometer, 0.62).
domain_priors:theater_ratio(djia_as_economic_barometer, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(djia_as_economic_barometer, extractiveness, 0.58).
narrative_ontology:constraint_metric(djia_as_economic_barometer, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(djia_as_economic_barometer, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(djia_as_economic_barometer, tangled_rope).
narrative_ontology:human_readable(djia_as_economic_barometer, "The Dow Jones Industrial Average as a primary barometer of national economic health").
narrative_ontology:topic_domain(djia_as_economic_barometer, "economic_policy/financial_markets").

domain_priors:requires_active_enforcement(djia_as_economic_barometer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, equity_investors).
narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, financial_media).
narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, asset_managers).
narrative_ontology:constraint_beneficiary(djia_as_economic_barometer, corporate_executives).
narrative_ontology:constraint_victim(djia_as_economic_barometer, working_class_wage_earners).
narrative_ontology:constraint_victim(djia_as_economic_barometer, small_business_owners).
narrative_ontology:constraint_victim(djia_as_economic_barometer, labor_market_stability).
narrative_ontology:constraint_victim(djia_as_economic_barometer, monetary_policy_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING-CLASS WAGE EARNER (SNARE) — Trapped in labor market dependency. When DJIA rises, this agent experiences wage stagnation, reduced job security through automation investment, and eroded purchasing power through monetary tightening justified by 'healthy markets.' No exit from the labor market constraint. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(djia_as_economic_barometer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR MARKET STABILITY (SNARE) — Abstract victim. The constraint treats labor market health as a subordinate indicator; Fed policy is set to asset price targets, creating employment volatility as a residual. No feedback mechanism for labor collateral damage. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(djia_as_economic_barometer, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNERS (TANGLED ROPE) — Constrained by capital access tied to equity market sentiment. DJIA high = cheaper borrowing, investment opportunities. DJIA low = credit rationing, forced asset sales. Mixed: coordination function (market signals for capital allocation) + extraction (subordination to equity valuations). d≈0.68, f(d)≈1.00, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(djia_as_economic_barometer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EQUITY INVESTORS AND ASSET MANAGERS (ROPE) — Net beneficiaries. DJIA index inclusion provides liquidity, passive capital inflows, and valuation inflation divorced from fundamentals. Arbitrage exit: can reallocate across asset classes, geographies, or strategies. The constraint is primarily a coordination mechanism (price discovery) that also captures extraction benefit. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(djia_as_economic_barometer, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE FEDERAL RESERVE (TANGLED ROPE) — Constrained by dual mandate (employment + price stability) but uses equity prices as primary intermediate target via wealth effect and financial conditions index. The constraint extracts coordination benefit (market monitoring) while also extracting from labor market through monetary tightening justified by 'healthy' equity prices. Active enforcement: Fed communications explicitly reference stock market levels. d≈0.52, f(d)≈0.72, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(djia_as_economic_barometer, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ECONOMIC POLICYMAKERS (PITON) — The DJIA as economic barometer persists through institutional habit and political theater despite well-documented limitations: excludes 90% of US workers, misses income inequality, ignores sectoral divergence, captures equity-weighted not employment-weighted health. Alternatives exist (unemployment rate, wage growth, Gini coefficient) but DJIA persists because it is politically convenient (easy to communicate, headline-grabbing, favors incumbent narratives). theater_ratio≈0.68, indicating high performative content. The policymaker sees DJIA reporting as theater but continues because replacement requires acknowledging labor market subordination.
constraint_indexing:constraint_classification(djia_as_economic_barometer, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ALTERNATIVE METRICS COALITION (SCAFFOLD) — Organized actors (labor economists, progressive policymakers, alternative media) are building parallel measurement systems: job quality indices, wage growth trackers, sectoral employment dashboards, real purchasing power indices. These represent a sunset mechanism: as adoption grows, DJIA's normative authority declines. Exit path visible. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(djia_as_economic_barometer, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSELY CLAIMED MOUNTAIN) — The constraint might appear natural ('stock prices reflect real economic expectations') but structural analysis reveals it is contingent institutional design, not a law of markets. The base properties (ε=0.58, suppression=0.62) contradict mountain requirements. The mountain classification is a false summit — naturalization of a power arrangement.
constraint_indexing:constraint_classification(djia_as_economic_barometer, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(djia_as_economic_barometer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(djia_as_economic_barometer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(djia_as_economic_barometer, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(djia_as_economic_barometer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(djia_as_economic_barometer, TR),
    TR >= 0.70.

:- end_tests(djia_as_economic_barometer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The DJIA subordinates labor market outcomes to equity price targeting through monetary policy transmission and fiscal policy stimulus timing. Quantifiable through correlation analysis: working-age males outside equity markets experience wage suppression during periods of 'healthy' equity gains. The extraction is not total — some equity gains do trickle through wealth effects and corporate employment — but the systematic bias is strong enough to warrant 0.58. The upward trajectory from 0.35 to 0.58 over 40 years reflects increasing equity ownership concentration and passive indexing's amplification of capital's policy voice. Suppression (0.62): Moderate-high. Significant barriers prevent labor outcomes from competing with equity prices in policymaking: (a) institutional lock-in — Fed mandate predates modern income inequality measurement; (b) media ecosystem — DJIA gets headline treatment, unemployment reports are footnotes; (c) cognitive capture — 'stock market = economy' is deeply embedded in political narrative; (d) mathematical separation — DJIA excludes 92M workers not holding appreciable equity. Theater ratio (0.68): High and rising. Financial media treats daily DJIA movements (often 0.1-0.5%) as meaningful economic information despite zero predictive power for employment or wages. The performative content increased as passive indexing and algorithmic trading amplified daily volatility, divorcing price movements from fundamental economic change. The theater serves beneficiaries by maintaining narrative dominance of equity prices.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The equity investor sees pure coordination (Rope) — genuine price discovery enabling capital allocation. The working-class wage earner sees pure extraction (Snare) — policy subordination with no exit. The small business owner sees mixed coordination-and-extraction (Tangled Rope) — capital access improved but constrained by equity sentiment. The Federal Reserve sees constrained institutional coordination (Tangled Rope) — using equity prices as intermediate target while acknowledging labor mandate subordination. Policymakers see degraded theater (Piton) — continuing DJIA focus despite knowing it's performative. The alternative metrics coalition sees a temporary constraint with a sunset (Scaffold) — replacement is technically feasible and politically gaining traction. The analytical observer risks a false summit (claiming DJIA is a natural law of markets) but structural analysis reveals it as contingent institutional design. The gap between beneficiary and victim perspectives is extreme: +0.08 d vs 0.92 d → effective extraction ratios differing by a factor of 10.
 *
 * DIRECTIONALITY LOGIC:
 *   Equity investors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net benefit flows from index inclusion and passive flows. Federal Reserve: Mixed institutional actor, constrained exit, victim of mandate design → d≈0.52, f(d)≈0.72. The Fed extracts from labor through asset price targeting while also extracting coordination benefit from price signals. Working-class wage earners: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no exit from labor market and no voice in policy targets. Labor market stability: Abstract victim + trapped → d≈0.95, f(d)≈1.42. Structural subordination with no feedback mechanism. Small business owners: Victim + constrained → d≈0.68, f(d)≈1.00. Constrained capital access subordinated to equity sentiment; can exit through bankruptcy or relocation but with high costs. Policymakers: Institutional + constrained → d≈0.50, f(d)≈0.65. Constrained by political convenience and media ecosystem; medium-level extraction of policy attention.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves Mandatrophy by revealing that the six types coexist in structural layers. At the beneficiary level (equity investors), the constraint is pure Rope — a coordination mechanism with positive externalities. At the victim level (wage earners), it is pure Snare — extraction with suppressed alternatives. The Fed occupies Tangled Rope: genuine coordination function (price discovery) combined with asymmetric extraction (subordination of employment). Policymakers occupy Piton: the mechanism is degraded (theater ratio 0.68) and maintained through inertia. The alternative metrics coalition occupies Scaffold: organized agents building exit paths with generational sunset logic. The mandatrophy is not 'which type is correct?' but 'how many structural layers are compressed into one metric?' The constraint survives because beneficiaries are concentrated (easy to organize), victims are dispersed (hard to coordinate), and policymakers lack strong feedback signals from labor outcomes. Resolution requires either: (a) institutional redesign (Fed mandate restructure to weight employment equally), (b) metric replacement (legislative shift to alternative barometers), or (c) coalition power (organized labor + progressive economists overcoming dispersed-victim problem). Current trajectory suggests slow drift toward Scaffold (alternatives gaining adoption) rather than rapid Snare→Rope conversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    djia_employment_causation,
    'Does the DJIA measure employment-weighted economic health, or does it primarily measure equity investor sentiment disconnected from labor outcomes?',
    'Correlation analysis: DJIA movements vs employment, wage growth, labor force participation over 20-year rolling windows. Granger causality tests to establish direction of influence.',
    'If DJIA reflects labor outcomes: classification reverts toward Rope (coordination mechanism). If DJIA diverges from labor outcomes: classification confirms Snare/Tangled Rope (extraction mechanism with suppressed feedback).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(djia_employment_causation, empirical, 'Whether DJIA causally links to employment or is sentiment-driven').

omega_variable(
    monetary_policy_transmission,
    'Does Fed targeting of ''healthy equity markets'' as an intermediate variable systematically subordinate employment outcomes through financial conditions transmission?',
    'Event study analysis: Fed communications mentioning equity prices; cross-tabulation with subsequent monetary policy moves and labor market outcomes. Decompose policy shocks into equity-price-driven vs employment-driven components.',
    'If equity prices drive policy: systematic subordination confirmed (Snare classification for labor strengthened). If employment outcomes receive equal weight: Tangled Rope or Rope classification more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_policy_transmission, empirical, 'Whether Fed policy targets equity prices ahead of employment').

omega_variable(
    alternative_barometer_adoption,
    'Are alternative economic indicators (unemployment rate, wage growth, wage share, sectoral employment dashboards) gaining institutional adoption as primary barometers?',
    'Media analysis: frequency of DJIA mentions vs alternatives in policy speeches, central bank communications, mainstream news. Survey policymakers on their primary economic health indicators. Track legislative references over time.',
    'If alternatives gain adoption: scaffold sunset mechanism is real; constraint lifecycle will show declining enforcement. If DJIA remains dominant: constraint persists through institutional inertia (piton) or structural necessity (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_barometer_adoption, empirical, 'Whether alternative economic indicators are replacing DJIA as primary barometer').

omega_variable(
    equity_concentration_bias,
    'Does the constraint systematically favor policies that benefit concentrated equity ownership at the expense of diversified labor income?',
    'Policy analysis: tax treatment of capital gains vs wages, monetary policy responses to equity crashes vs employment crises, regulatory changes following equity volatility vs labor market shocks.',
    'If systematic bias confirmed: extraction mechanism confirmed (Snare/Tangled Rope strengthened). If neutral: classification moves toward pure Rope (coordination mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_concentration_bias, empirical, 'Whether equity price targeting biases policy toward capital vs labor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(djia_as_economic_barometer, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(djia_tr_t0, djia_as_economic_barometer, theater_ratio, 0, 0.45).
narrative_ontology:measurement(djia_tr_t20, djia_as_economic_barometer, theater_ratio, 20, 0.58).
narrative_ontology:measurement(djia_tr_t40, djia_as_economic_barometer, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(djia_be_t0, djia_as_economic_barometer, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(djia_be_t20, djia_as_economic_barometer, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(djia_be_t40, djia_as_economic_barometer, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(djia_as_economic_barometer, information_standard).
narrative_ontology:affects_constraint(djia_as_economic_barometer, monetary_policy_transmission_lag).
narrative_ontology:affects_constraint(djia_as_economic_barometer, equity_ownership_concentration).
narrative_ontology:affects_constraint(djia_as_economic_barometer, labor_voice_suppression).

% DUAL FORMULATION NOTE:
% The DJIA-as-barometer constraint is downstream of broader financial market structure (equity ownership concentration, passive indexing, algorithmic trading) and upstream of monetary policy implementation and labor market outcomes. The constraint's extractiveness increased with passive indexing adoption (post-2010) because algorithmic capital flows amplified equity prices' policy influence while further divorcing them from fundamental economic change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(djia_as_economic_barometer, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
