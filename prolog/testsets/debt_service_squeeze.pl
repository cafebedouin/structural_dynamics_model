% ============================================================================
% CONSTRAINT STORY: debt_service_squeeze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_service_squeeze, []).

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
 *   constraint_id: debt_service_squeeze
 *   human_readable: Consumer Debt Service Squeeze on Higher-Income Households
 *   domain: economic/consumer_finance
 *
 * SUMMARY:
 *   The consumer debt service squeeze on higher-income households represents
 *   a structural extraction mechanism operating through the credit system and
 *   monetary policy. Households earning $75,000+ were historically considered
 *   'safe' credit risks and were marketed aggressively into auto loans,
 *   credit cards, and mortgage products. When inflation accelerated
 *   (2021-2023) and the Federal Reserve raised policy rates to combat it, the
 *   real burden of prior debt increased sharply while new borrowing became
 *   more expensive. Nominal wages have not kept pace with inflation and
 *   rising debt-service costs, creating a growing squeeze on discretionary
 *   income even for households with above-median earnings. The constraint
 *   operates through institutional structures (lending standards, bankruptcy
 *   law, Federal Reserve policy) that create asymmetric extraction: financial
 *   institutions and asset holders benefit from higher rates and locked-in
 *   spreads, while borrowers bear the service burden. The theater ratio
 *   (0.48) is moderate because consumer finance regulation (credit scores,
 *   disclosure rules) creates the appearance of protection without addressing
 *   the underlying mechanism. The extractiveness value (0.52) reflects that
 *   the squeeze has become severe enough to impact household behavior
 *   (reduced spending, delayed major purchases, increased financial stress),
 *   yet not absolute — many households retain some capacity to service debt
 *   or adjust consumption.
 *
 * KEY AGENTS:
 *   - High-income wage earners ($75k+): Primary victim (powerless/trapped) — bear growing debt-service burden; cannot easily refinance or escape credit system
 *   - Financial institutions and lenders: Primary beneficiary (institutional/arbitrage) — capture higher net interest margins, hold valuable fixed-rate assets, benefit from policy rate increases
 *   - Asset holders and real estate investors: Secondary beneficiary (powerful/arbitrage) — benefit from credit-constrained purchasing power (lower demand for housing/vehicles reduces price pressure), can arbitrage rate environment
 *   - Consumers in aggregate: Collective victim (powerless/trapped) — purchasing power eroded; aggregate demand dampened; consumer spending as share of GDP constrained
 *   - Debt reform coalition: Organized agents (organized/constrained) — advocating for debt forgiveness, rate caps, bankruptcy reform; perceive exit path via policy change
 *   - Federal Reserve and monetary policy makers: Institutional actor (institutional/arbitrage) — set policy rates to address inflation; have agency to create or mitigate squeeze through rate and regulatory decisions
 *   - Credit rating agencies and consumer finance regulators: Institutional actors (institutional/arbitrage) — maintain performative oversight; rules focus on disclosure and lending criteria rather than aggregate debt burden or rate-pass-through mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_service_squeeze, 0.52).
domain_priors:suppression_score(debt_service_squeeze, 0.65).
domain_priors:theater_ratio(debt_service_squeeze, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_service_squeeze, extractiveness, 0.52).
narrative_ontology:constraint_metric(debt_service_squeeze, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(debt_service_squeeze, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_service_squeeze, tangled_rope).
narrative_ontology:human_readable(debt_service_squeeze, "Consumer Debt Service Squeeze on Higher-Income Households").
narrative_ontology:topic_domain(debt_service_squeeze, "economic/consumer_finance").

domain_priors:requires_active_enforcement(debt_service_squeeze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_service_squeeze, financial_institutions).
narrative_ontology:constraint_beneficiary(debt_service_squeeze, asset_holders).
narrative_ontology:constraint_victim(debt_service_squeeze, high_income_wage_earners).
narrative_ontology:constraint_victim(debt_service_squeeze, consumer_purchasing_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SQUEEZED HIGH-INCOME HOUSEHOLD (SNARE) — Trapped by prior debt obligations contracted at lower rates. When inflation and rising interest rates hit, refinancing is expensive or unavailable. Must service existing debt while new borrowing becomes costlier. No exit: cannot discharge debt, cannot easily relocate away from labor market, cannot avoid auto/housing needs. Experiences maximum extraction — debt servicing consumes growing share of real income.
constraint_indexing:constraint_classification(debt_service_squeeze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CREDIT-DEPENDENT WAGE EARNER (TANGLED ROPE) — Benefits from credit access that enables purchasing (vehicle, home, education, emergencies). But constraints on refinancing and rising rates create asymmetric extraction. Can reduce discretionary spending or seek higher income, but cannot easily exit the credit system itself. Mixed: coordination benefit (credit) + extraction (rising service burden).
constraint_indexing:constraint_classification(debt_service_squeeze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) — Net beneficiary. Rising rates increase net interest margin. Existing fixed-rate loans become valuable assets (locked-in spread). New lending captures higher-rate environment. Can arbitrage across markets, refinance their own funding, and diversify portfolios. Experiences the constraint as pure coordination: managing rate environment and customer payment flows. No extraction against them — extraction flows toward them.
constraint_indexing:constraint_classification(debt_service_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEBT REFORM COALITION (SCAFFOLD) — Organized agents (consumer advocates, progressive policy makers, labor unions) see the squeeze as a temporary policy failure addressable via: student debt forgiveness, mortgage rate caps, auto lending regulation, bankruptcy reform. The coalition has agency and perceives a sunset: policy interventions can reset the debt-service ratio and restore purchasing power. Constraint is high-suppression but low theater (problem is real, not performative), with perceived exit path via political reform.
constraint_indexing:constraint_classification(debt_service_squeeze, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDIT RATING / REGULATORY SYSTEM (PITON) — Credit bureaus and consumer finance regulations (Dodd-Frank, Fair Credit Reporting Act) maintain a performative role: they create the appearance of consumer protection through score-based lending criteria and disclosure requirements, but the core mechanism (rising rates as policy tool, inflation eroding real income) is unaddressed by these rules. Regulatory theater persists due to institutional path-dependency even as its functional effectiveness at preventing debt-service squeeze has atrophied.
constraint_indexing:constraint_classification(debt_service_squeeze, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a macroeconomic universality perspective, some debt-service squeeze is inherent to any credit system under inflation and rising policy rates: the real cost of prior borrowing rises, and nominal incomes lag. This perspective naturalizes the squeeze as an inevitable feature of monetary policy. However, the structural data reveals this as false naturalization: the squeeze's severity depends on regulatory choices (predatory lending standards, lack of rate caps, bankruptcy restrictions), not on immutable economic laws.
constraint_indexing:constraint_classification(debt_service_squeeze, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_service_squeeze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_service_squeeze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_service_squeeze, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_service_squeeze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_service_squeeze, TR),
    TR >= 0.70.

:- end_tests(debt_service_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over the interval. Base extractiveness reflects the structural asymmetry: borrowers' real income falls relative to debt obligations (extraction), while lenders' margins expand (benefit flow). The value has risen from 0.28 to 0.52 as policy rates increased and wage growth lagged inflation. Not maximum (0.70+) because households retain some capacity to adjust consumption or seek higher income; refinancing is difficult but not impossible for some. Suppression (0.65): High. Barriers include: inability to refinance profitably, limited bankruptcy protections (student loans exempt, limited discharge), concentrated lending market (few competitors for rates), geographic labor market immobility, essential nature of debt (vehicle, housing). These create a suppression environment where alternatives are severely constrained. Theater ratio (0.48): Moderate. Consumer finance regulation (Fair Credit Reporting Act, Dodd-Frank, credit scoring) creates appearance of consumer protection, but addresses symptoms (individual lending decisions) not mechanisms (aggregate rate environment, debt accumulation necessity). The rules are functional at the disclosure level but have low explanatory power for the aggregate squeeze.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the squeezed household (Snare) and the financial institution (Rope) is maximal. From the household's position (powerless/trapped), the constraint is pure extraction with no coordination benefit — they need credit access but the current system imposes rising costs with no escape. From the institution's position (institutional/arbitrage), the constraint is pure coordination — managing the rate environment and customer payment flows generates steady returns. The household sees debt service as a burden that grows against their will; the institution sees it as a profitable business model. The debt reform coalition (Scaffold) introduces a perspectival layer: they see the squeeze as addressable via policy change (sunset mechanism via debt forgiveness, rate caps, bankruptcy reform). The regulatory system (Piton) claims to address the problem through credit scoring and disclosure rules, but these are performative relative to the actual mechanism (rate pass-through, inflation, wage suppression). The analytical observer's Mountain view (inherent to credit systems under inflation) naturalizes what is actually a contingent institutional arrangement — the squeeze could be mitigated or eliminated by policy choices (rate controls, debt jubilee, wage floor mechanisms), revealing it as structurally constructed rather than naturally inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from power, exit options, and beneficiary/victim declarations. High-income wage earners enter as victims with trapped exit (derived d ≈ 0.95, f(d) ≈ 1.42) — maximum experienced extraction. Financial institutions enter as beneficiaries with arbitrage exit (derived d ≈ 0.05, f(d) ≈ -0.12) — negative effective extraction (they benefit). The organized coalition (debt reform advocates) enters as organized/constrained (derived d ≈ 0.50, f(d) ≈ 0.65) — moderate effective extraction, because they have some agency (advocacy, potential policy wins) but face institutional resistance. The Federal Reserve enters as institutional/arbitrage (canonical d ≈ 0.00, f(d) ≈ -0.12) — they set policy rates and have maximal agency; they do not experience extraction from this constraint but rather deploy it as a tool. Scope modifier σ(national) = 1.0 leaves χ = ε × f(d) × 1.0 for each agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: 'Is the debt service squeeze a necessary feature of credit systems (Mountain/coordination) or an extractive mechanism enabled by institutional choices (Snare/Tangled Rope)?' The resolution requires distinguishing between (a) the mechanical fact that rising policy rates increase service burden on fixed-rate debt (true, but not extractive per se) and (b) the institutional choices that make this burden severe and inescapable: predatory lending standards that encourage over-borrowing, bankruptcy law that exempts student loans, concentrated lending markets that prevent rate competition, wage suppression that prevents nominal income growth, lack of public alternatives (public housing, public transit) that would reduce borrowing necessity. The Tangled Rope classification resolves this: the constraint includes a genuine coordination function (credit access enables consumption smoothing and major purchases) AND asymmetric extraction (lenders benefit from rate environment while borrowers bear burden). Both are real. The classification prevents two errors: (1) calling it pure extraction (Snare) and ignoring that credit access is valuable, and (2) calling it pure coordination (Rope) and ignoring the asymmetry. The theater ratio (0.48) is relatively low because the squeeze is real and measurable, not performative — households' debt-service ratios genuinely rise. The institutional response (regulation) is performative relative to the mechanism, but that's captured in the Piton perspective, not in the base extractiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rate_pass_through_timing,
    'How quickly do rising policy rates transmit to consumer lending rates, and is this transmission uniform across credit types?',
    'Time-series analysis of Federal Funds Rate vs. auto loan APRs, credit card rates, and mortgage rates; control for market concentration and lender funding costs',
    'If transmission is fast and uniform: debt-service burden is mechanical (Mountain-like). If transmission is slow/variable: debt-service squeeze reveals extractive lending practices (Snare-like, not Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rate_pass_through_timing, empirical, 'Rate transmission speed and uniformity across credit products').

omega_variable(
    refinancing_access_barriers,
    'What percentage of high-income households with existing debt can refinance at lower rates, and what structural barriers prevent refinancing?',
    'Survey of credit-eligible households; analysis of denial rates by credit score, debt-to-income ratio, and loan type; institutional lending policy review',
    'If barriers are low: households have genuine exit option (not Snare). If barriers are high: refinancing is illusory, confirming trapped exit (Snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refinancing_access_barriers, empirical, 'Barriers to refinancing and rate-relief access').

omega_variable(
    real_wage_lag_causality,
    'Is nominal wage stagnation (failure to keep pace with inflation) a mechanical consequence of labor-market slack, or is it a structural feature of employer power concentration?',
    'Wage growth decomposition; productivity growth vs. wage growth trends; labor market tightness indicators; union density and bargaining power time series',
    'If mechanical slack: debt squeeze is temporary coordination problem (Scaffold). If structural employer power: wage suppression is extractive mechanism (Snare/Tangled Rope), and squeeze persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_wage_lag_causality, empirical, 'Whether wage lag is mechanical or structural').

omega_variable(
    debt_accumulation_necessity,
    'How much of high-income household debt is truly necessary (housing, education, essential vehicle) vs. discretionary (lifestyle, consumption smoothing)?',
    'Household debt survey with categorization by necessity; analysis of debt growth vs. asset accumulation; comparison with debt levels in income-equal European economies with stronger public housing/transport',
    'If mostly necessary: debt-service squeeze is externally imposed (Snare). If significantly discretionary: households have some agency in debt accumulation (shifts to Tangled Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_accumulation_necessity, empirical, 'Necessity vs. discretionary split in household debt accumulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_service_squeeze, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dss_tr_t0, debt_service_squeeze, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dss_tr_t5, debt_service_squeeze, theater_ratio, 5, 0.41).
narrative_ontology:measurement(dss_tr_t10, debt_service_squeeze, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(dss_be_t0, debt_service_squeeze, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dss_be_t5, debt_service_squeeze, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dss_be_t10, debt_service_squeeze, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_service_squeeze, resource_allocation).
narrative_ontology:affects_constraint(debt_service_squeeze, wage_growth_suppression).
narrative_ontology:affects_constraint(debt_service_squeeze, housing_affordability_crisis).
narrative_ontology:affects_constraint(debt_service_squeeze, consumer_purchasing_power_erosion).

% DUAL FORMULATION NOTE:
% The debt service squeeze is downstream of both monetary policy (Federal Reserve rate decisions) and labor market dynamics (wage suppression). This story focuses on the household-level experience of the credit-rate-wage squeeze. Upstream constraints include wage_growth_suppression (structural employer power limiting nominal income growth) and housing_affordability_crisis (which drives mortgage debt accumulation). The squeeze affects downstream consumer purchasing power and aggregate demand dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_service_squeeze, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
