% ============================================================================
% CONSTRAINT STORY: consumer_purchasing_power_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_purchasing_power_erosion, []).

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
 *   constraint_id: consumer_purchasing_power_erosion
 *   human_readable: Consumer Purchasing Power Erosion Through Inflation and Wage Lag
 *   domain: economic/labor/monetary_policy
 *
 * SUMMARY:
 *   Consumer purchasing power erosion represents a structural extraction
 *   mechanism embedded in modern monetary policy. When inflation outpaces
 *   nominal wage growth, real purchasing power declines for wage earners
 *   while asset holders benefit from nominal appreciation and
 *   inflation-induced demand for real assets. The constraint operates
 *   through: (1) monetary expansion creating new currency that enters the
 *   economy unequally (asset markets and financial institutions first, wage
 *   earners last if at all), (2) suppression of wage growth through labor
 *   market slack, unionization decline, and structural unemployment, and (3)
 *   theater that frames inflation as exogenous or temporary rather than as
 *   policy-driven wealth redistribution. The constraint exhibits all six DR
 *   types depending on the observer's structural position. The wage earner
 *   trapped without exit sees a snare. The central bank authority sees its
 *   inflation-targeting framework as coordination mechanism for price
 *   stability, but the engine's false summit detector reveals this as
 *   naturalization of a policy choice. Asset holders experience pure
 *   coordination and benefit extraction. Organized labor experiences mixed
 *   coordination (collective wage setting) and extraction (structural lag).
 *   The extractiveness trend (0.35 → 0.58) reflects that inflation
 *   acceleration over the measurement interval outpaced wage acceleration,
 *   progressively tightening the trap for powerless agents.
 *
 * KEY AGENTS:
 *   - Wage Earners: Primary victim (powerless/trapped) — nominal wages lag inflation; no exit from labor market participation
 *   - Fixed-Income Retirees: Primary victim (powerless/trapped) — pension erosion; cannot re-enter labor market
 *   - Asset Holders and Equity Investors: Primary beneficiary (institutional/arbitrage) — inflation drives real asset appreciation and nominal leverage benefits
 *   - Commercial Banks and Financial Institutions: Primary beneficiary (institutional/arbitrage) — fixed-rate debt erodes, spreads widen, portfolio rebalancing opportunities
 *   - Labor Unions and Organized Labor: Secondary actor (organized/constrained) — can negotiate collective wage adjustments but face structural deunionization and automation pressures
 *   - Skilled Knowledge Workers: Secondary beneficiary (powerful/mobile) — wages closer to inflation; can arbitrage internationally; still face asset price inflation squeeze
 *   - Central Banking Authority: Institutional actor (institutional/arbitrage) — maintains inflation-targeting framework that operationally enables wealth redistribution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing fiat currency monetary expansion as inevitable rather than policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_purchasing_power_erosion, 0.58).
domain_priors:suppression_score(consumer_purchasing_power_erosion, 0.62).
domain_priors:theater_ratio(consumer_purchasing_power_erosion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_purchasing_power_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(consumer_purchasing_power_erosion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(consumer_purchasing_power_erosion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_purchasing_power_erosion, tangled_rope).
narrative_ontology:human_readable(consumer_purchasing_power_erosion, "Consumer Purchasing Power Erosion Through Inflation and Wage Lag").
narrative_ontology:topic_domain(consumer_purchasing_power_erosion, "economic/labor/monetary_policy").

domain_priors:requires_active_enforcement(consumer_purchasing_power_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_purchasing_power_erosion, asset_holders).
narrative_ontology:constraint_beneficiary(consumer_purchasing_power_erosion, financial_institutions).
narrative_ontology:constraint_beneficiary(consumer_purchasing_power_erosion, fixed_rate_debt_issuers).
narrative_ontology:constraint_victim(consumer_purchasing_power_erosion, wage_earners).
narrative_ontology:constraint_victim(consumer_purchasing_power_erosion, savers).
narrative_ontology:constraint_victim(consumer_purchasing_power_erosion, fixed_income_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped in labor market with nominal wages rising slower than inflation. No exit from the constraint: must participate in economy at depreciating purchasing power. Real wages decline structurally; cannot arbitrage or escape through mobility. Suppression mechanisms include: labor market slack, wage stickiness, organized labor decline, gig economy casualization. Maximum experienced extraction.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIXED-INCOME RETIREE (SNARE) — Purchasing power erodes on fixed pensions and fixed savings with no labor income buffer. Cannot re-enter labor market for cost reasons. Trapped by biological constraints (age, health) and contractual constraints (fixed pension formulas). Inflation directly subtracts from livable standard. Maximum suppression and extraction.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SERVICE WORKER (TANGLED ROPE) — Constrained but not trapped. Can shift between employers, negotiate raises, or move to different sectors — but at significant cost (job search, retraining, relocation). Experiences both coordination (labor market enables income generation) and extraction (nominal wage growth lags inflation). Some agency but substantial barriers. Real purchasing power declines modestly despite nominal mobility.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL INSTITUTIONS (ROPE) — Benefit from controlled inflation via: debt issuance at fixed rates that erode in real terms, higher spreads on lending, and portfolio rebalancing into equities. The constraint is coordination (monetary policy, inflation targeting) that shifts wealth toward financial sector. Institutions can arbitrage between inflation expectations and actual rates. Net beneficiary experiencing the constraint as enabling mechanism.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ASSET HOLDERS (ROPE) — Purchasing power erosion in currency drives demand for real assets: equities, real estate, commodities. Asset holders benefit from inflation through: nominal appreciation of holdings, leverage on borrowed money (debt erodes), and portfolio rebalancing. Can arbitrage between inflation and returns. Experience the constraint as pure coordination mechanism enabling wealth transfer. Net beneficiary.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED LABOR (TANGLED ROPE) — Has some agency through collective negotiation (cost of exit: strikes, credible threat power) but faces structural constraints: automation, globalization, deunionization, gig economy shift. Can achieve some wage coordination but cannot prevent overall trend. Experiences both coordination (collective bargaining) and extraction (structural real wage lag). Suppression through legal/political barriers to unionization.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CENTRAL BANKING AUTHORITY (PITON) — Maintains inflation targeting regime nominally as price stability mechanism, but institutional function has degraded. Theater: inflation framed as exogenous or temporary; actual extractive function (wealth redistribution toward asset holders) treated as side effect. Central bank sees itself as solving coordination problem (price stability) when structural role is enabling extraction. Institutional inertia maintains framework despite functional degradation. Theater ratio high.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: SKILLED KNOWLEDGE WORKERS (TANGLED ROPE) — Mobile labor with exit options: international relocation, career switching, equity compensation. Nominal wage growth closer to inflation for top earners. But still experience modest real wage lag and asset inflation squeeze (housing costs, education costs). Some coordination benefit from knowledge economy participation, but extraction through asset price inflation outpacing wage inflation. Moderate experienced extraction.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal perspective, some erosion of purchasing power is inevitable consequence of fiat currency systems: if currency is not commodity-backed, nominal expansion must occur, and real purchasing power cannot be preserved for all agents simultaneously. This perspective sees the constraint as immutable law of monetary systems. However, structural data contradicts mountain classification — the analytical observer risks naturalizing a policy choice (inflation targeting, unequal distribution of newly created currency) as inherent to currency itself.
constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_purchasing_power_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_purchasing_power_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_purchasing_power_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_purchasing_power_erosion, TR),
    TR >= 0.70.

:- end_tests(consumer_purchasing_power_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from wage earners and fixed-income recipients through purchasing power loss while benefiting asset holders. The mechanism is not coercive monopoly (like snares typically are) but rather asymmetric distribution of newly created currency and structural labor market power imbalance. Extractiveness increased over the measurement interval as inflation accelerated (2021-2024 period), outpacing nominal wage growth even in tight labor markets. Suppression (0.62): Moderate-high. Multiple barriers prevent wage earners from escaping the constraint: (1) structural, must participate in economy; (2) labor market slack or automation reducing alternative job options; (3) organized labor decline reducing collective wage-setting power; (4) gig economy casualization; (5) international wage arbitrage limiting developed-market wage growth. Suppression is not total (some agents can negotiate or relocate) but substantial. Theater ratio (0.55): Moderate. Inflation targeting is presented as technical monetary policy for price stability, obscuring its wealth redistribution function. Central bank communications emphasize 'transitory' inflation or 'supply shocks' rather than endogenous monetary expansion effects. However, the theater is incomplete — economists, some policymakers, and segments of the public increasingly recognize the mechanism. Theater has increased over the interval as alternative explanations became less credible (2022-2024).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural phenomenon (nominal wage lag relative to inflation) classifies as snare for the trapped, rope for the beneficiary, piton for the institution maintaining the framework, and mountain for the analytical observer. The gap reveals that purchasing power erosion is not an exogenous market outcome but a policy-embedded extraction mechanism. The mountain perspective is a false summit: inflation targeting is presented as technical necessity but is actually a policy choice. Different monetary frameworks (NGDP targeting, price-level targeting, or institutionalized wage-indexation) would produce different constraint structures. The snare classification for wage earners is robust across alternative policy regimes — the trap is structural (labor market participation required, nominal wages sticky downward, exit suppressed). The rope classification for asset holders is robust — they benefit from inflation regardless of policy details. The piton classification for central banks reflects that the inflation-targeting framework persists through institutional inertia despite alternative frameworks being technically feasible.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation follows beneficiary/victim declarations through the sigmoid function. Wage earners and retirees are victims of purchasing power loss; asset holders and financial institutions are beneficiaries of inflation-driven asset appreciation and debt erosion. Organized labor occupies an intermediate position: victims of structural wage lag but beneficiaries of coordination mechanisms. Central banks are institutional beneficiaries insofar as inflation targeting centralizes monetary control and enables financial system stability through controlled inflation. The scope modifier σ(S) is high (national scope, σ=1.0) because purchasing power erosion is systemically enforced across the economy. The effective extraction χ = ε × f(d) × σ(S) ranges from near-zero for asset holders (beneficiary d, negative f(d)) to maximum for trapped wage earners (victim d=0.95, high f(d)≈1.42, scope σ=1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATORY CONSTRAINT DEFINITION: The constraint must be classified as tangled_rope (not snare) because it exhibits genuine coordination function alongside extraction. Inflation targeting serves an actual coordination role: prevents deflationary spirals, enables financial system stability, and provides a nominal anchor. Agencies (workers, savers, retirees) that benefit from monetary stability are also victims of purchasing power erosion — the constraint is hybrid. This mandates tangled_rope classification. Misclassifying as pure snare would ignore: (1) that some coordination value is genuinely delivered, (2) that beneficiaries include financial institutions that provide capital allocation services, and (3) that alternative regimes (hyperinflation, deflation) would be worse for many agents. The tangled_rope classification correctly captures the asymmetry: coordination value is broadly distributed, but extraction is concentrated (benefits asset holders and financial institutions, harms wage earners and savers). Mandatrophy is resolved by the perspectival framework: different agents experience the constraint differently because they occupy different structural positions relative to the inflation-driven wealth redistribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_measurement_ambiguity,
    'Does official inflation measurement (CPI basket) accurately capture consumption patterns of wage earners versus asset holders?',
    'Comparison of inflation impact across income deciles using granular expenditure data; differential inflation indices by consumption category',
    'If official CPI understates inflation for wage earners: actual extractiveness is higher than measured (ε should increase 0.05-0.10). If CPI is accurate: the constraint''s extraction rate is measured correctly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_measurement_ambiguity, empirical, 'CPI measurement bias across income groups').

omega_variable(
    wage_lag_structural_versus_cyclical,
    'Is wage lag relative to inflation a structural feature of labor market power imbalance or a cyclical consequence of demand shocks and central bank policy?',
    'Time-series decomposition of wage-inflation gap; comparison of wage dynamics across tight vs slack labor markets; policy counterfactual analysis',
    'If structural: wage earners cannot escape extraction even at full employment (suppression ≥ 0.65). If cyclical: constraint would classify as rope during full-employment periods (suppression drops, extraction shifts to coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_lag_structural_versus_cyclical, empirical, 'Whether wage lag is structural or cyclical').

omega_variable(
    asset_appreciation_substitutability,
    'Can wage earners realistically substitute equity/real asset appreciation for wage income to maintain purchasing power, or does asset appreciation primarily benefit existing asset holders?',
    'Wealth distribution dynamics; comparison of asset returns across income deciles; analysis of who captures asset appreciation gains',
    'If substitutable: rope classification for wage earners widens (they can arbitrage into assets). If not: snare classification confirmed (trapped in nominal wages while wealth redistribution occurs through assets).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asset_appreciation_substitutability, empirical, 'Whether wage earners can access asset appreciation as substitute').

omega_variable(
    monetary_policy_causal_attribution,
    'What proportion of purchasing power erosion is attributable to central bank monetary expansion versus supply shocks, globalization, productivity growth mismatch, or labor market deunionization?',
    'VAR decomposition of inflation; international comparison of wage dynamics in different monetary regimes; historical analysis of inflation periods with different causal drivers',
    'If monetary expansion dominates (>50%): central bank authority''s piton classification is accurate (institutional function is maintaining extractive framework). If supply shocks dominate: constraint is less institutional, more exogenous (partial mountain classification warranted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_policy_causal_attribution, empirical, 'Causal attribution of purchasing power erosion').

omega_variable(
    policy_transmission_mechanism_opacity,
    'Do wage earners understand that inflation erodes their purchasing power intentionally through monetary policy, or is the mechanism treated as exogenous market force?',
    'Survey analysis of public understanding of monetary transmission; media discourse analysis on inflation attribution; comparison of wage negotiations in periods of recognized vs unrecognized monetary expansion',
    'If mechanism is opaque: suppression is higher (agents cannot organize resistance) and theater ratio is higher (institutional narrative obscures causal structure). If transparent: agents can coordinate for cost-of-living adjustments, reducing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_transmission_mechanism_opacity, empirical, 'Public understanding of monetary transmission to inflation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_purchasing_power_erosion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(powe_tr_t0, consumer_purchasing_power_erosion, theater_ratio, 0, 0.4).
narrative_ontology:measurement(powe_tr_t5, consumer_purchasing_power_erosion, theater_ratio, 5, 0.48).
narrative_ontology:measurement(powe_tr_t10, consumer_purchasing_power_erosion, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(powe_be_t0, consumer_purchasing_power_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(powe_be_t5, consumer_purchasing_power_erosion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(powe_be_t10, consumer_purchasing_power_erosion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_purchasing_power_erosion, resource_allocation).
narrative_ontology:affects_constraint(consumer_purchasing_power_erosion, labor_market_deunionization).
narrative_ontology:affects_constraint(consumer_purchasing_power_erosion, asset_price_inflation_housing_unaffordability).
narrative_ontology:affects_constraint(consumer_purchasing_power_erosion, retirement_security_pension_erosion).

% DUAL FORMULATION NOTE:
% Consumer purchasing power erosion is decomposed into three downstream constraints: labor market dynamics (wage suppression), asset market dynamics (housing/real estate price inflation), and retirement system dynamics (pension real value decline). Each constraint has distinct structural features but all are driven by upstream monetary policy expansion and inflation. The unified story captures the coordination/extraction hybrid; the downstream stories capture domain-specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
