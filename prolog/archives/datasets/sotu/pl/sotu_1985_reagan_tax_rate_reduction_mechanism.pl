% ============================================================================
% CONSTRAINT STORY: sotu_1985_reagan_tax_rate_reduction_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1985_reagan_tax_rate_reduction_mechanism, []).

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
 *   constraint_id: sotu_1985_reagan_tax_rate_reduction_mechanism
 *   human_readable: Reagan 1985 Tax Rate Reduction Mechanism
 *   domain: economic_policy/fiscal_redistribution
 *
 * SUMMARY:
 *   The 1985 Reagan tax rate reduction mechanism operates as a hybrid
 *   coordination-extraction constraint. The policy simultaneously
 *   accomplishes two structural functions: (1) it coordinates capital
 *   formation incentives by reducing the tax burden on high earners and
 *   corporations, creating alignment between private accumulation and public
 *   revenue targets (coordination function); (2) it extracts from
 *   lower-income households and public service-dependent communities by
 *   reducing government revenue available for social programs,
 *   infrastructure, and public goods (extraction function). The constraint is
 *   legitimated through a supply-side economic hypothesis: that tax rate
 *   reductions stimulate growth sufficiently to recover lost revenue through
 *   expanded economic activity, thereby avoiding the zero-sum redistribution
 *   problem. This hypothesis is empirically contested and becomes the
 *   constraint's critical omega variable. The theater ratio increases over
 *   the interval as the 'growth recovery' narrative persists despite
 *   stagnating revenue, revealing the mechanism's degradation from functional
 *   fiscal coordination into performative revenue forecasting (piton
 *   signature). The extractiveness rises over time as the initial modest
 *   redistribution compounds through deficit accumulation, budget pressure on
 *   public programs, and structural inequality effects. From the capital
 *   holders' perspective, this is pure coordination (rope); from low-income
 *   households' perspective, it is pure extraction (snare); from
 *   middle-income earners' perspective, it is genuine mixing of coordination
 *   and extraction (tangled rope); from the analytical observer's
 *   perspective, the claim that it follows economic law is a false summit
 *   naturalizing contingent institutional choices.
 *
 * KEY AGENTS:
 *   - Capital Holders & High-Income Earners: Primary beneficiaries (institutional/arbitrage) — capture direct tax relief with no suppression; experience mechanism as liberation of 'pent-up power'
 *   - Corporations: Primary beneficiaries (institutional/arbitrage) — reduced tax burden on profits; enhanced capital formation incentives
 *   - Low-Income Households: Primary victims (powerless/trapped) — face service cuts, wage stagnation, reduced public investment; cannot exit dependence on public goods
 *   - Public Service-Dependent Communities: Primary victims (powerless/trapped) — concentrated in rural and post-industrial regions; bear concentrated costs of spending cuts
 *   - Federal Revenue System: Institutional victim (institutional/arbitrage) — functional capacity degraded as revenue stagnates relative to expenditure, triggering deficit accumulation
 *   - Labor Union Collectives: Organized secondary victim (organized/constrained) — possess coordination mechanism but face electoral/economic constraints; extract concessions through labor negotiation under fiscal pressure
 *   - Middle-Income Earners & Small Business Owners: Mixed victims/beneficiaries (powerful/mobile) — receive modest tax relief but face service degradation and infrastructure underinvestment
 *   - Supply-Side Reform Coalition: Organized beneficiary (organized/constrained) — economists, policy entrepreneurs, business groups; see mechanism as temporary with sunset when growth recovers
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1985_reagan_tax_rate_reduction_mechanism, 0.58).
domain_priors:suppression_score(sotu_1985_reagan_tax_rate_reduction_mechanism, 0.45).
domain_priors:theater_ratio(sotu_1985_reagan_tax_rate_reduction_mechanism, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1985_reagan_tax_rate_reduction_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1985_reagan_tax_rate_reduction_mechanism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sotu_1985_reagan_tax_rate_reduction_mechanism, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1985_reagan_tax_rate_reduction_mechanism, tangled_rope).
narrative_ontology:human_readable(sotu_1985_reagan_tax_rate_reduction_mechanism, "Reagan 1985 Tax Rate Reduction Mechanism").
narrative_ontology:topic_domain(sotu_1985_reagan_tax_rate_reduction_mechanism, "economic_policy/fiscal_redistribution").

domain_priors:requires_active_enforcement(sotu_1985_reagan_tax_rate_reduction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1985_reagan_tax_rate_reduction_mechanism, capital_holders).
narrative_ontology:constraint_beneficiary(sotu_1985_reagan_tax_rate_reduction_mechanism, high_income_earners).
narrative_ontology:constraint_beneficiary(sotu_1985_reagan_tax_rate_reduction_mechanism, corporations).
narrative_ontology:constraint_victim(sotu_1985_reagan_tax_rate_reduction_mechanism, public_revenue_base).
narrative_ontology:constraint_victim(sotu_1985_reagan_tax_rate_reduction_mechanism, low_income_households).
narrative_ontology:constraint_victim(sotu_1985_reagan_tax_rate_reduction_mechanism, government_service_dependent_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLDS (SNARE) — Trapped by wage stagnation and service dependencies. Cannot exit public education, healthcare, or transfer programs that are simultaneously their primary service and the target of revenue cuts. Extraction flows from them through reduced public investment in safety nets. No alternative pathways available within biographical horizon.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR UNIONS AND PUBLIC EMPLOYEES (TANGLED ROPE) — Constrained by political fragmentation and budget pressure. Possess organization (union structure) enabling some coordination, but face binary choice: accept service cuts or mobilize electoral resistance at career risk. Genuine coordination function (collectively managing wage negotiations within reduced fiscal space) coexists with extraction mechanism (degraded purchasing power as cuts compound). Effective extraction moderate because organization provides agency.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL HOLDERS & HIGH-INCOME EARNERS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: the tax rate reduction mechanism enables capital formation and income retention. No suppression from this perspective — exit is unrestricted (voluntary tax compliance, investment arbitrage across jurisdictions). The mechanism subsidizes their accumulation. They perceive this as liberation of 'pent-up power' — the constraint removes barriers to enterprise.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MIDDLE-INCOME & SMALL BUSINESS OWNERS (TANGLED ROPE) — Receive modest tax relief but also face service degradation and weakened public infrastructure (transportation, education, research). Tax savings often offset by costs: reduced public investment in roads, schools, research universities that sustain their markets. Coordination function: tax framework enables some small-business formation. Extraction: degraded public goods. Mobile enough to relocate to better-provisioned jurisdictions or to hedge via tax-advantaged vehicles. Perspectival experience is genuinely mixed — some benefit, some cost.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPPLY-SIDE REFORM COALITION (SCAFFOLD) — Economists, policy organizations, business groups, and political parties organized around the hypothesis that tax cuts unlock growth. See the mechanism as temporary: the expectation is that growth will eventually restore revenues (Laffer curve narrative). Suppression is cognitive (ideological commitment to supply-side causality) rather than material. The coalition has agency and sees a sunset: when growth materializes (or fails to materialize), the policy either succeeds and becomes normalized, or fails and triggers reversion. Theater ratio is moderate-high because the causal claim (tax cuts → growth → revenue recovery) is empirically contested and requires continuous rhetorical assertion.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL FISCAL FRAMEWORK (PITON) — The mechanism nominally maintains revenue neutrality through growth, but the functional reality degrades. Initial function: marginal rates calibrated to revenue needs and distributional targets. After tax cuts: the fiscal system is maintained through deficits, bracket creep, and deferred obligations rather than through actual revenue alignment. The 'revenue neutral via growth' frame persists performatively (annual revenue projections include growth assumptions) but the underlying mechanism has atrophied — actual revenue stagnates while expenditure pressures mount. Theater ratio high because ritual revenue forecasting continues despite divorced from fiscal reality.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW PERSPECTIVE (MOUNTAIN) — From a civilizational/universal view, marginal tax rates and capital formation incentives appear to follow economic 'laws': higher tax rates reduce capital investment (inelastic response to policy), lower rates expand it (immutable profit motive). This perspective naturalizes the mechanism as following from human nature and economic scarcity. However, the structural data reveals this as a false summit: the 'natural' capital response to tax cuts is modulated by institutional factors (corporate governance, market power, income concentration, fiscal health) that are contingent, not immutable. Empirically, the claimed natural relationship (tax cuts → capital formation → growth) is weaker and more context-dependent than the mountain framing suggests.
constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1985_reagan_tax_rate_reduction_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1985_reagan_tax_rate_reduction_mechanism, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1985_reagan_tax_rate_reduction_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1985_reagan_tax_rate_reduction_mechanism, TR),
    TR >= 0.70.

:- end_tests(sotu_1985_reagan_tax_rate_reduction_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The mechanism directly reduces tax burden on ~20% of income distribution by ~15-25 percentage points (marginal rates 70% → 35-50%). This is significant wealth transfer. However, it is not maximum (snare-level 0.66+) because: (1) the supply-side hypothesis creates a plausible (if contested) path to growth that could recover revenue; (2) the mechanism is publicly legislated and transparently justified (not hidden); (3) some middle-income households receive modest benefits. The extractiveness increases over the interval (0.32 → 0.68) as the deficit accumulates and the growth-recovery path fails to materialize empirically, revealing the mechanism's true redistribution character. Suppression (0.45): Moderate. Low-income households face real barriers: public service cuts (education, healthcare, infrastructure) reduce exit options, wage stagnation limits mobility, and concentrated service dependencies (public transit, public schools, transfer programs) create path dependency. However, suppression is not maximal (snare-level 0.60+) because: (1) labor organization remains possible; (2) electoral mechanisms exist for policy reversal; (3) alternative jurisdictions offer some exit options (geographic mobility, though costly). Theater ratio (0.62): Moderate-high. The 'growth recovery' narrative is the performative layer — annual revenue projections in budget documents include optimistic growth assumptions that, when compared to actuals, reveal the mechanism's theatrical character. The theater increases over time (0.45 → 0.72) as the gap between projected and actual revenues widens, forcing increasingly complex rhetorical justifications.
 *
 * PERSPECTIVAL GAP:
 *   Capital holders and high-income earners see Rope (pure coordination, no suppression, clear benefit). Low-income households see Snare (pure extraction, high suppression, no exit). Public employees see Tangled Rope (organization provides some agency, genuine service coordination exists alongside extraction). Middle-income earners see Tangled Rope (modest benefit offset by service degradation). Federal fiscal system sees Piton (performative revenue projections persist despite functional degradation). Supply-side coalition sees Scaffold (temporary redistribution with sunset when growth materializes). Analytical observer risks seeing Mountain (economic law) but structural data reveals false summit (contingent policy choices). The perspectival gaps are maximal: the same policy is experienced as liberation by beneficiaries, strangulation by victims, and mixed coordination-extraction by those in between. This is the diagnostic signature of effective tangled rope — genuine coordination coexists with genuine extraction, and the balance point is observer-relative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from beneficiary/victim status, power level, and exit options. Capital holders: beneficiary status + institutional power + arbitrage exit → d ≈ 0.10 (low d, negative effective extraction experienced as coordination). Low-income households: victim status + powerless + trapped → d ≈ 0.92 (high d, maximum effective extraction, snare experience). Labor unions: victim status + organized power + constrained exit → d ≈ 0.55 (moderate d, moderate effective extraction, tangled rope experience). Middle-income: mixed beneficiary/victim + powerful + mobile → d ≈ 0.48 (near-neutral d, mixed extraction/benefit, tangled rope experience). Federal fiscal system: victim status + institutional power + constrained exit (must service debt, cannot exit fiscal obligations) → d ≈ 0.70 (high d). The systematic directionality gradient across perspectives produces the perspectival classification range: Mountain to Snare to Rope to Tangled Rope. None of these perspectives is 'wrong' — each is the accurate experiential classification from that structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by distinguishing genuine coordination from extraction through the structural beneficiary/victim split and the temporal omega (growth recovery). At t=0, the supply-side hypothesis is credible (scaffold/rope readings reasonable) — growth could theoretically recover revenue. By t=10, the growth hypothesis has failed to recover revenue empirically (omega 'revenue_recovery_via_growth' resolves negatively), revealing the mechanism's true character: tangled rope at best (coordination + extraction coexist), snare at worst (if growth never materializes and extraction compounds indefinitely). The mandatrophy is resolved by temporal evidence: the mechanism is only coordination-preserving if the growth assumption holds. If growth fails, the coordination disappears and only extraction remains. The rising theater ratio (0.45 → 0.72) is the diagnostic signal — as the growth hypothesis fails, the policy increasingly relies on performative revenue forecasting rather than actual fiscal coordination. This is the piton pattern: degraded institutional function maintained through theatrical assertion. Mandatrophy-resolved status: Conditional tangled rope → false mountain (analytical) depending on empirical growth outcome. The engine's false summit detector will flag the mountain perspective as naturalization of contingent policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_response_causality,
    'Do marginal tax rate reductions directly cause capital formation and GDP growth, or do they correlate with growth driven by other factors (monetary policy, global trade, demographic/technological shifts)?',
    'Econometric analysis isolating tax policy effect from confounders; comparison of growth rates pre/post tax cuts controlling for money supply, trade conditions, investment climate, and sectoral composition',
    'If tax cuts are primary cause: rope/scaffold perspectival reading confirmed, mechanism is coordination accelerant. If causality is weak/indirect: snare reading strengthened, mechanism is pure redistribution with growth rhetoric as cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_response_causality, empirical, 'Causal relationship between tax rate cuts and economic growth').

omega_variable(
    revenue_recovery_via_growth,
    'Does economic growth following tax cuts generate sufficient new tax revenue to offset the rate reduction, or does the revenue gap persist and accumulate as deficit?',
    'Historical budget data comparing revenue change (actual vs baseline revenue with no tax cut) for 5-10 year period post-enactment; Laffer curve empirical test across OECD economies',
    'If revenue recovers: scaffold sunset logic confirmed, temporary redistribution with recovery path. If revenue gap persists: piton reading confirmed, deficit accumulation reveals degraded fiscal coordination, theater ratio increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_recovery_via_growth, empirical, 'Whether growth-based revenue recovery offsets tax cut revenue loss').

omega_variable(
    capital_redeployment_vs_consumption,
    'When high earners and corporations retain tax revenue, how much is deployed toward productive capital investment (plant, equipment, R&D) versus consumption, financial engineering (M&A, buybacks), or tax arbitrage (offshore placement)?',
    'Capital stock data, sectoral investment rates, M&A activity, corporate buyback trends, offshore capital flow tracking pre/post tax policy change',
    'If capital deployment dominates: coordination mechanism is functional, growth claim is credible. If financial engineering/arbitrage dominates: mechanism is pure wealth transfer with minimal growth contribution, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_redeployment_vs_consumption, empirical, 'How retained capital is redeployed (productive investment vs consumption/financial engineering)').

omega_variable(
    distributional_effect_persistence,
    'Does the wealth concentration effect from tax cuts persist or reverse over time as growth distributes? Or does initial inequality gap expand indefinitely?',
    'Gini coefficient trends, income share data (top 10%, top 1%) for 15+ year period post-enactment; wage share vs capital share evolution',
    'If distribution reverses/compresses: temporary redistribution (scaffold reading), coordination with recovery. If gap expands: structural inequality mechanism (snare reading from low-income perspective, false summit on mountain reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_effect_persistence, empirical, 'Whether tax-cut-driven wealth concentration persists or reverses over time').

omega_variable(
    public_investment_substitution,
    'Can tax-cut revenue loss be fully substituted by private capital formation, or are public goods (infrastructure, research, education) non-substitutable and their degradation creates negative externalities offsetting any private gain?',
    'Sectoral productivity data comparing public vs private investment efficiency; infrastructure quality indices; research commercialization rates; education outcome measures across public/private provision models',
    'If substitution is complete: rope/scaffold reading valid, mechanism works as advertised. If public goods are complementary: snare reading strengthened, apparent coordination (private gain) masks extraction (public degradation, negative externalities).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_investment_substitution, empirical, 'Whether private capital formation substitutes for degraded public investment').

omega_variable(
    deficit_sustainability_temporal_horizon,
    'Over what time horizon does deficit accumulation from tax cuts become unsustainable? Does it trigger eventual fiscal crisis (hard constraint), or can it persist indefinitely via debt refinancing and monetary accommodation?',
    'Debt-to-GDP trajectory simulation; interest rate response to deficit levels; historical episodes of fiscal unsustainability; monetary policy endogeneity analysis',
    'If crisis emerges within 20 years: scaffold sunset confirmed, mechanism has forced temporal boundary. If deficit is indefinitely sustainable: piton reading deepens, degradation can persist without triggering reset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deficit_sustainability_temporal_horizon, conceptual, 'Temporal horizon for deficit sustainability and fiscal crisis emergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1985_reagan_tax_rate_reduction_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reagan_tax_tr_t0, sotu_1985_reagan_tax_rate_reduction_mechanism, theater_ratio, 0, 0.45).
narrative_ontology:measurement(reagan_tax_tr_t3, sotu_1985_reagan_tax_rate_reduction_mechanism, theater_ratio, 3, 0.55).
narrative_ontology:measurement(reagan_tax_tr_t6, sotu_1985_reagan_tax_rate_reduction_mechanism, theater_ratio, 6, 0.62).
narrative_ontology:measurement(reagan_tax_tr_t10, sotu_1985_reagan_tax_rate_reduction_mechanism, theater_ratio, 10, 0.72).

% Extraction over time
narrative_ontology:measurement(reagan_tax_be_t0, sotu_1985_reagan_tax_rate_reduction_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(reagan_tax_be_t3, sotu_1985_reagan_tax_rate_reduction_mechanism, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(reagan_tax_be_t6, sotu_1985_reagan_tax_rate_reduction_mechanism, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(reagan_tax_be_t10, sotu_1985_reagan_tax_rate_reduction_mechanism, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1985_reagan_tax_rate_reduction_mechanism, resource_allocation).
narrative_ontology:affects_constraint(sotu_1985_reagan_tax_rate_reduction_mechanism, deficit_accumulation_trajectory).
narrative_ontology:affects_constraint(sotu_1985_reagan_tax_rate_reduction_mechanism, income_inequality_distribution_dynamics).
narrative_ontology:affects_constraint(sotu_1985_reagan_tax_rate_reduction_mechanism, public_infrastructure_degradation).
narrative_ontology:affects_constraint(sotu_1985_reagan_tax_rate_reduction_mechanism, monetary_accommodation_federal_reserve).

% DUAL FORMULATION NOTE:
% The tax rate reduction mechanism decomposes into three structurally distinct constraints with different ε values: (1) capital_tax_burden_relief (ε=0.15, Rope) — pure coordination of capital formation incentives; (2) redistribution_via_rate_cuts (ε=0.68, Snare/Tangled Rope) — extraction from lower-income households and public revenues; (3) deficit_financing_mechanism (ε=0.72, Piton) — degraded fiscal coordination maintained through deficit absorption and debt refinancing. The stories are linked: the rate cuts' revenue loss flows into deficit, which flows into monetary accommodation. Each story has its own empirical signature and omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1985_reagan_tax_rate_reduction_mechanism, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
