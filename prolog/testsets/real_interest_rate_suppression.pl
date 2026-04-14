% ============================================================================
% CONSTRAINT STORY: real_interest_rate_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_real_interest_rate_suppression, []).

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
 *   constraint_id: real_interest_rate_suppression
 *   human_readable: Real Interest Rate Suppression via Monetary Policy
 *   domain: economic_policy/monetary_finance
 *
 * SUMMARY:
 *   Real interest rate suppression via accommodative monetary policy
 *   represents a structural transfer from savers and wage earners to debt
 *   holders and asset owners. This constraint emerged in response to the 2008
 *   financial crisis and has persisted through successive crises (COVID,
 *   geopolitical shocks), becoming institutionalized within central banking
 *   practice. The constraint exhibits hybrid characteristics: it coordinates
 *   financial stability and employment objectives (rope function) while
 *   simultaneously extracting purchasing power from fixed-income agents who
 *   have few alternatives (snare function). The extractiveness trajectory
 *   shows an initial rise from 0.22 (pre-GFC) to 0.42 (mid-GFC quantitative
 *   easing) to 0.58 (sustained low/negative real rates post-2020), indicating
 *   that what began as temporary crisis response has hardened into persistent
 *   policy regime. The theater ratio (0.48) reflects moderate performativity:
 *   the technical apparatus of inflation targeting and 'data-dependent'
 *   policy decisions consumes institutional effort while obscuring
 *   distributional consequences. Real interest rate suppression is sustained
 *   through forward guidance, quantitative easing, regulatory encouragement
 *   of yield-seeking behavior, and suppression of policy alternatives. The
 *   constraint's durability depends on continued fiscal capacity of
 *   governments to service low-real-rate debt and absence of organized
 *   political movement demanding policy reversal.
 *
 * KEY AGENTS:
 *   - Fixed-Income Savers and Retirees: Primary victims (powerless/trapped) — experience systematic purchasing power erosion with no viable exit options within the constraint regime
 *   - Government Treasuries and Central Banks: Primary beneficiaries (institutional/arbitrage) — reduce debt service burden, maintain financial stability; perceive constraint as legitimate policy coordination
 *   - Wage Earners: Secondary victims (moderate/constrained) — face real wage suppression through inflation, benefit from employment maintenance; constrained by labor market immobility
 *   - Asset Owners and Financial Intermediaries: Secondary beneficiaries (powerful/arbitrage) — benefit from artificially elevated asset valuations; face repricing risk if rates normalize
 *   - Organized Labor and Pension Advocates: Organized agents (organized/constrained) — perceive constraint as temporary with sunset; mobilizing for policy reversal through political pressure
 *   - Central Banking Institutions: Institutional actors (institutional/arbitrage) — maintain performative inflation-targeting regime; see own mandate as drifting toward financial stability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as immutable monetary necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(real_interest_rate_suppression, 0.58).
domain_priors:suppression_score(real_interest_rate_suppression, 0.65).
domain_priors:theater_ratio(real_interest_rate_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(real_interest_rate_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(real_interest_rate_suppression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(real_interest_rate_suppression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(real_interest_rate_suppression, tangled_rope).
narrative_ontology:human_readable(real_interest_rate_suppression, "Real Interest Rate Suppression via Monetary Policy").
narrative_ontology:topic_domain(real_interest_rate_suppression, "economic_policy/monetary_finance").

domain_priors:requires_active_enforcement(real_interest_rate_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(real_interest_rate_suppression, government_treasuries).
narrative_ontology:constraint_beneficiary(real_interest_rate_suppression, large_debt_holders).
narrative_ontology:constraint_beneficiary(real_interest_rate_suppression, asset_price_beneficiaries).
narrative_ontology:constraint_victim(real_interest_rate_suppression, savers_fixed_income).
narrative_ontology:constraint_victim(real_interest_rate_suppression, pensioners_on_fixed_returns).
narrative_ontology:constraint_victim(real_interest_rate_suppression, wage_earners_inflation_eroded).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIXED-INCOME SAVER (SNARE) — Trapped in nominal returns that cannot keep pace with inflation. Cannot exit by accessing alternative currency, cannot flee to negative-yield bonds without severe capital loss. Purchasing power erodes systematically. Suppressed alternatives: international bonds face currency depreciation or capital controls; switching to equities requires risk tolerance incompatible with retiree portfolio constraints. Maximum extraction experienced by those least able to absorb it.
constraint_indexing:constraint_classification(real_interest_rate_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNMENT TREASURIES (ROPE) — Primary beneficiary. Suppressed real rates reduce debt service burden and refinancing pressure. Sees the constraint as essential coordination mechanism: monetary policy balances inflation control, employment, and financial stability. Net beneficiary with substantial arbitrage options (can exit by raising rates, accepting political costs). Experiences constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(real_interest_rate_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WAGE EARNERS (TANGLED ROPE) — Face suppressed real wages paired with inflation. Experience both coordination benefit (employment maintained through accommodative policy) and extraction (real purchasing power declines). Constrained by labor market immobility and regional economic geography. Some benefit from low mortgage rates; significant cost from erosion of wage-denominated savings and future pensions.
constraint_indexing:constraint_classification(real_interest_rate_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ASSET OWNERS (TANGLED ROPE) — Benefit from artificially elevated asset valuations driven by yield-seeking flows into equities and real estate. But face real interest rate risk: if rates normalize, asset repricing could erase gains. Arbitrage option: diversify across currencies and jurisdictions. Mixed experience: substantial benefits during suppression phase; concentrated extraction risk if policy reverses.
constraint_indexing:constraint_classification(real_interest_rate_suppression, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INFLATION TARGETING REGIME (PITON) — Central banking's declared purpose is price stability and employment, but the operational mechanism (suppressing real rates through quantitative easing, forward guidance, asset purchases) has become largely performative in justifying transfers to debt holders. The ritual of 'data-dependent' rate decisions persists despite diminishing real inflation control capacity. Theater ratio reflects that the technical apparatus (DSGE models, Phillips curves, transmission mechanisms) consumes institutional effort while the actual allocation outcomes (who benefits, who pays) are barely discussed. The regime persists through inertia and lack of politically viable alternatives.
constraint_indexing:constraint_classification(real_interest_rate_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED LABOR (SCAFFOLD) — Sees rate suppression as temporary tactical loss in a longer generational struggle for wage and benefit adequacy. Organized agents can exit through political mobilization: wage indexation clauses, inflation-adjusted pensions, collective bargaining for cost-of-living adjustments. Suppression is experienced as a sunset constraint: the current regime of low rates and inflation erosion is unsustainable; political pressure will eventually force policy reversal. This is scaffold logic — high suppression now, but declining suppression if organized agents successfully demand policy change.
constraint_indexing:constraint_classification(real_interest_rate_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risks naturalizing the constraint as an immutable law of modern finance: 'in a world of fiat currency, real interest rates must sometimes be suppressed to prevent deflationary spirals and maintain full employment.' This perspective frames the extraction as physically necessary, like gravity. However, the base properties contradict pure mountain classification: suppression (0.65) and extractiveness (0.58) reflect policy choices, not physical limits. Alternative policies (hard currency, commodity standards, fiscal transfers instead of monetary suppression) exist; they are not chosen for political reasons, not structural impossibility. The engine will flag this as false summit.
constraint_indexing:constraint_classification(real_interest_rate_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(real_interest_rate_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(real_interest_rate_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(real_interest_rate_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(real_interest_rate_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(real_interest_rate_suppression, TR),
    TR >= 0.70.

:- end_tests(real_interest_rate_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, measured as persistent purchasing power transfer from savers to debt holders. Base measure reflects that the constraint operates through policy choice (not physical law) and extracts a measurable flow (the inflation premium that savers give up relative to pre-suppression real rates). The 0.58 value reflects sustained suppression over 15 years rather than temporary crisis measure — extraction has become structural. Suppression (0.65): Moderate-high. Savers face multiple barriers to exit: international investment requires knowledge and capital; inflation-hedging through equities requires risk tolerance; currency switching faces depreciation or capital controls; nominal bond yields are negative in real terms with no legal alternative. These are not absolute barriers (like physical imprisonment) but high-cost barriers that most savers cannot practically overcome. Theater ratio (0.48): Moderate. Central banks expend substantial effort on inflation-targeting communications, DSGE modeling, rate-decision rituals, and forward guidance. This apparatus creates appearance of technical sophistication while distributional consequences remain largely unstated. However, theater ratio is not high (>0.70) because the regime does produce real macroeconomic effects — employment and financial stability are genuinely affected, not pure theater. The operative mechanism (yield-seeking asset flows, debt affordability) is visible to financial professionals even if retail savers don't see it. Claimed type (Tangled Rope): The constraint coordinates financial stability and employment (genuine rope function) while asymmetrically extracting from savers (genuine snare function). Both mechanisms operate simultaneously — the extraction is not separable from the coordination. This meets the tangled rope gate: beneficiaries (treasuries, asset owners), victims (savers), active enforcement (central bank policy operations), both functions necessary to explain structural outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The fixed-income saver perceives a snare: extraction with no exit and no benefit. The treasury perceives a rope: coordination mechanism enabling macro stability. The wage earner perceives tangled rope: mixed employment benefit and wage erosion. The asset owner perceives tangled rope: asset gains offset by repricing risk. Organized labor perceives scaffold: temporary constraint with generational sunset if they mobilize. The central bank perceives piton: their own institutions performing rituals (inflation targeting) that no longer produce advertised results (price stability) but persist through inertia. The analytical observer risks mountain: framing suppression as immutable law of fiat currency. This is maximum perspectival gap in a single constraint. The gap reveals that what appears as 'monetary policy necessity' from institutional perspective is experienced as arbitrary extraction from powerless perspective. The truthful description is tangled rope: real coordination function paired with real extraction, not one or the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from base extractiveness (0.58), their structural power and exit options, and scope. Fixed-income savers (powerless/trapped) experience maximum chi: low f(d) from trapped exit + victim status yields high effective extraction. Government treasuries (institutional/arbitrage) experience minimum or negative chi: arbitrage exit + beneficiary status yields low or inverted extraction (they experience benefit flow toward them). Wage earners (moderate/constrained) experience moderate chi: constrained exit + mixed beneficiary-victim status yields mid-range experienced extraction. Asset owners (powerful/arbitrage) experience low-moderate chi: arbitrage exit + beneficiary status despite repricing risk. Organized labor (organized/constrained) experiences scaffold-level chi: constrained exit but organized exit capacity and visible sunset produce lower experienced extraction than structural metrics alone suggest. The directionality derivation shows why the snare (from powerless perspective) and the rope (from institutional perspective) both describe the same constraint: the structural parameters are shared but experienced extractiveness differs by 3–4x depending on agent's power and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR MANDATE DRIFT: The mandatrophy appears as tension between stated central-banking mandate (price stability, full employment, financial stability) and actual policy outcomes (suppressed real rates, distributional transfer, potential asset-price volatility). The resolution is not 'which is the true mandate?' but recognition that the mandate has drifted in practice while remaining nominally unchanged in law. Post-2008, central banks interpret 'financial stability' to include asset-price support and debt sustainability, subordinating price stability and distributing costs to savers. The constraint resolves mandatrophy by acknowledging the divergence: the regime is Tangled Rope (both coordination and extraction) rather than pure Rope (coordination only) because the extraction to savers is not an accidental side effect but a deliberately operated transfer mechanism, even if not explicitly named as such. The piton perspective (central banks see their own regime as degraded ritual) is evidence that the original mandate (price stability through inflation targeting) no longer governs behavior; the regime persists through institutional inertia and absence of alternatives. Mandatrophy is resolved by separating (a) the stated mandate from (b) the operational policy from (c) the structural outcomes: these three have drifted apart, and recognizing the gaps shows that rate suppression is neither innocent coordination nor immutable necessity, but a contingent institutional choice with clear winners and losers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_causation_ambiguity,
    'Is observed inflation primarily caused by monetary suppression of real rates or by supply shocks independent of monetary policy?',
    'Comparative analysis of rate-suppression episodes with vs without simultaneous supply shocks; cross-country evidence of rate suppression without corresponding inflation; DSGE model counterfactuals isolating monetary transmission',
    'If monetary-driven: suppression is active extraction mechanism (snare/tangled rope for savers). If supply-driven: suppression is coordinating response to exogenous shock (rope for all agents). Classification shifts from Snare (powerless perspective) toward Rope if supply-shock hypothesis dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_causation_ambiguity, empirical, 'Whether inflation results from rate suppression or exogenous supply factors').

omega_variable(
    alternative_policy_availability,
    'Could policymakers achieve the same employment and stability outcomes without suppressing real rates (via fiscal transfers, job guarantees, or different monetary frameworks)?',
    'Counterfactual policy experiments; evidence from countries using alternative frameworks (Japan''s fiscal dominance model, EZB''s different employment mandate); MMT-style policy simulations',
    'If viable alternatives exist: suppression is contingent institutional choice (snare/tangled rope confirmed). If no alternatives available: constraint approximates mountain (monetary suppression is structural necessity). Current consensus: alternatives exist but face political barriers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_policy_availability, preference, 'Whether alternative monetary/fiscal policies could achieve same macroeconomic goals').

omega_variable(
    distributional_intent_vs_side_effect,
    'Is real rate suppression a deliberate policy aimed at debt-holder transfers, or an unintended side effect of inflation-targeting failure and employment focus?',
    'Analysis of central bank communications pre vs post-2008; comparison of stated objectives vs distributional outcomes; interviews with policymakers regarding awareness of transfer effects',
    'If deliberate: classification as Tangled Rope (enforced extraction) confirmed across all perspectives. If side effect: classification may shift toward Scaffold (unintended extraction with sunset as policy corrects). Current evidence: mixed intent, with deliberate debt-relief aspect post-GFC increasing over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_intent_vs_side_effect, empirical, 'Whether rate suppression is deliberate policy or unintended side effect').

omega_variable(
    saver_exit_capacity_under_regime,
    'Do savers genuinely face trapped exit status or do they have constrained-level alternatives (international assets, inflation-hedging, currency diversification)?',
    'Survey of retail saver actual asset allocation decisions; transaction costs and barriers to international investment; regulatory constraints on capital flows by income cohort',
    'If trapped: powerless-perspective classification confirmed (snare). If constrained: upgrade to constrained-exit perspective (tangled rope for savers rather than snare). Current evidence: most savers lack practical knowledge or capital to access international alternatives; exit is materially trapped for lower-income cohorts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(saver_exit_capacity_under_regime, empirical, 'Whether savers face trapped or constrained-level exit barriers').

omega_variable(
    mandate_drift_in_central_banking,
    'Has central banking mandate evolved from price-stability and employment (dual mandate) to implicit financial-stability and asset-price-support?',
    'Analysis of policy actions and communications; correlation of interest-rate decisions with asset-price levels vs inflation/unemployment gaps; comparison of pre-2008 vs post-GFC policy reaction functions',
    'If mandate has drifted: financial-stability justification is post-hoc rationalization of extraction mechanism (piton perspective confirmed). If mandate intact: rate suppression is legitimate policy tool applied within defined mandate. Evidence: significant drift detected in post-GFC asset-purchase programs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_drift_in_central_banking, empirical, 'Whether central bank mandate has drifted toward financial stability and asset support').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(real_interest_rate_suppression, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rirs_tr_t0, real_interest_rate_suppression, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rirs_tr_t5, real_interest_rate_suppression, theater_ratio, 5, 0.35).
narrative_ontology:measurement(rirs_tr_t10, real_interest_rate_suppression, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rirs_tr_t15, real_interest_rate_suppression, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(rirs_be_t0, real_interest_rate_suppression, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rirs_be_t5, real_interest_rate_suppression, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(rirs_be_t10, real_interest_rate_suppression, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rirs_be_t15, real_interest_rate_suppression, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(real_interest_rate_suppression, resource_allocation).
narrative_ontology:boltzmann_floor_override(real_interest_rate_suppression, 0.12).
narrative_ontology:affects_constraint(real_interest_rate_suppression, fiscal_dominance_regime).
narrative_ontology:affects_constraint(real_interest_rate_suppression, asset_price_inflation_feedback).
narrative_ontology:affects_constraint(real_interest_rate_suppression, pension_adequacy_crisis).

% DUAL FORMULATION NOTE:
% Real interest rate suppression is distinct from inflation targeting (the regime that operationalizes suppression), distinct from debt sustainability (the outcome that justifies suppression), and distinct from financial stability mandates (the stated rationale for suppression). Each of these could be authored as separate constraint stories with different ε values. The current story treats suppression as the mechanism; upstream stories would address whether inflation targeting is a mountain (natural law) or piton (degraded ritual), and whether fiscal dominance creates structural necessity for monetary suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(real_interest_rate_suppression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
