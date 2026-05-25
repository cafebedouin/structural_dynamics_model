% ============================================================================
% CONSTRAINT STORY: inflation_targeting_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inflation_targeting_regime, []).

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
 *   constraint_id: inflation_targeting_regime
 *   human_readable: Inflation Targeting Regime
 *   domain: macroeconomic_policy
 *
 * SUMMARY:
 *   Inflation targeting emerged in the 1990s as the dominant framework for
 *   monetary policy in developed economies, with central banks delegated sole
 *   authority to maintain inflation within a narrow band (typically 2%). The
 *   regime coordinates private sector inflation expectations around a
 *   credible nominal anchor, enabling price stability and predictable
 *   monetary policy. However, it simultaneously extracts from wage earners
 *   through nominal wage rigidity, suppresses fiscal policy authority through
 *   'monetary dominance,' and systematically benefits asset holders and
 *   creditors through the credibility premium. The constraint exhibits
 *   tangled rope characteristics: genuine coordination function (expectations
 *   stabilization) combined with asymmetric extraction (redistributive
 *   effects). The theater ratio has risen over the 20-year interval as the
 *   regime's actual mechanisms diverge from its legitimating narratives —
 *   central banks measure inflation through indices that increasingly diverge
 *   from lived price experience, particularly for working-class households
 *   facing rapid price growth in housing, healthcare, and education. The
 *   regime's extractiveness has increased as nominal wage growth consistently
 *   lags the inflation target, creating systematic real wage losses for wage
 *   earners despite the official coordination goal.
 *
 * KEY AGENTS:
 *   - Central Banks: Primary beneficiary (institutional/arbitrage) — gains institutional authority and credibility; experiences regime as pure coordination mechanism
 *   - Wage Earners: Primary victim (powerless/trapped) — face nominal wage rigidity and systematic real wage loss; cannot exit labor markets without severe cost
 *   - Asset Holders and Creditors: Secondary beneficiary (powerful/arbitrage) — benefit from predictable real returns and controlled inflation environment; maximum exit optionality
 *   - Small Debtors: Secondary victim (moderate/constrained) — benefit from inflation-eroding debt in long run but suffer from interest rate volatility; constrained by debt obligations
 *   - Fiscal Authorities: Constrained actor (organized/constrained) — suppressed by monetary dominance doctrine; face political cost of challenging central bank independence
 *   - Price Stability Commons: Abstract victim (powerless/trapped) — nominally the regime's target but receives degraded coordination as measurement diverges from experience
 *   - Analytical Observer: Civilizational perspective — recognizes genuine coordination layered with asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inflation_targeting_regime, 0.52).
domain_priors:suppression_score(inflation_targeting_regime, 0.48).
domain_priors:theater_ratio(inflation_targeting_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inflation_targeting_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(inflation_targeting_regime, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(inflation_targeting_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inflation_targeting_regime, tangled_rope).
narrative_ontology:human_readable(inflation_targeting_regime, "Inflation Targeting Regime").
narrative_ontology:topic_domain(inflation_targeting_regime, "macroeconomic_policy").

domain_priors:requires_active_enforcement(inflation_targeting_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inflation_targeting_regime, central_banks).
narrative_ontology:constraint_beneficiary(inflation_targeting_regime, asset_holders).
narrative_ontology:constraint_beneficiary(inflation_targeting_regime, creditors).
narrative_ontology:constraint_victim(inflation_targeting_regime, wage_earners).
narrative_ontology:constraint_victim(inflation_targeting_regime, debtors).
narrative_ontology:constraint_victim(inflation_targeting_regime, working_poor).
narrative_ontology:constraint_victim(inflation_targeting_regime, price_stability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER (SNARE) — Trapped within labor contracts and wage-setting institutions that adjust slowly to inflation targets. Cannot exit employment market without severe cost (unemployment, relocation, skill loss). Bears full extraction: real wages decline when inflation runs at target while nominal wage growth lags. Maximum suppression through labor market frictions and information asymmetry about long-term inflation expectations.
constraint_indexing:constraint_classification(inflation_targeting_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL DEBTOR (TANGLED ROPE) — Constrained by debt obligations denominated in nominal terms. Benefits from inflation reducing real debt burden, but also suffers from interest rate volatility required to defend the inflation target. The regime coordinates debt-servicing across the economy (genuine function) while extracting through real asset devaluation and rate risk. Significant but not maximum extraction — some agency through debt refinancing and real asset appreciation.
constraint_indexing:constraint_classification(inflation_targeting_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANK (ROPE) — Experiences inflation targeting as pure coordination mechanism: stabilizing inflation expectations enables predictable monetary policy and coordinates private sector expectations. The regime concentrates institutional power and credibility in the central bank (beneficiary position) but this is justified through the coordination function. Maximum arbitrage — can exit the regime by simply changing the target or framework without material cost.
constraint_indexing:constraint_classification(inflation_targeting_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASSET HOLDER / CREDITOR (ROPE) — Benefits from predictable inflation within a bounded range, which enables real asset valuation and reduces default risk on loans. Experiences the regime as coordination (stable nominal returns) with asymmetric benefit (creditor receives fixed nominal payments in an environment where inflation is controlled). Maximum exit optionality — can move assets across currencies, asset classes, and jurisdictions.
constraint_indexing:constraint_classification(inflation_targeting_regime, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FISCAL AUTHORITY / POLITICAL COALITION (SCAFFOLD) — The inflation targeting regime constrains fiscal policy through the implicit 'monetary dominance' doctrine: central banks are delegated sole authority over inflation while fiscal authorities are constrained by inflation-fighting credibility. This is a temporary arrangement with a generational sunset: as central banks integrate climate policy, financial stability, and employment mandates into their framework, the rigid separation between monetary and fiscal authority erodes. Suppression is moderate — fiscal authorities can challenge the regime through political pressure, but costs are real (loss of central bank independence, capital flight).
constraint_indexing:constraint_classification(inflation_targeting_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRICE STABILITY COMMONS (PITON) — The regime claims to coordinate around a shared good (stable prices), but the actual mechanism is increasingly performative. Central banks measure inflation through indices (CPI, PCE) that abstract from heterogeneous price experiences; the 'stability' is statistical, not experiential. Theater ratio rises as the gap widens between headline inflation targets (2%) and lived price experiences (housing, healthcare, food). The commons is nominally a beneficiary but receives degraded coordination as the measurement diverges from reality.
constraint_indexing:constraint_classification(inflation_targeting_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, inflation targeting represents a genuine coordination achievement (expectations stabilization) layered with asymmetric extraction (wage suppression, creditor prioritization, fiscal constraint). The regime is necessary for modern monetary systems but not neutral — it systematically redistributes from debtors to creditors, from wage earners to asset holders, from fiscal authority to monetary authority. The extraction is structural to the regime's design, not an accident. The effective extractiveness depends on the directionality of the observer.
constraint_indexing:constraint_classification(inflation_targeting_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inflation_targeting_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inflation_targeting_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inflation_targeting_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inflation_targeting_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inflation_targeting_regime, TR),
    TR >= 0.70.

:- end_tests(inflation_targeting_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The regime is moderately extractive. The primary extraction mechanism is systematic real wage loss for wage earners whose nominal contracts adjust slowly relative to the inflation target. Secondary extraction flows from asset holders capturing the inflation premium and creditors receiving predictable real returns. The extractiveness value reflects that the regime is not pure coordination (which would require ε ≤ 0.45) but also not high extraction (which would require ε ≥ 0.66). The measurement trajectory shows extractiveness increasing from 0.28 (early adoption, 1990) to 0.52 (mature regime, 2010) as nominal wage rigidity persists and real wage growth decouples from productivity. Suppression (0.48): Moderate. Wage earners face barriers to adjustment (labor market friction, information costs, union weakness in many countries) but are not completely trapped — some can relocate, reskill, or push for union contracts. Fiscal authorities face high suppression (political cost of challenging central bank, capital flight) but can override through political pressure. Creditors and asset holders face minimal suppression. Theater ratio (0.58): Moderate-high. The regime's legitimating narrative (stable prices through technical monetary policy) masks the redistribution mechanism (systematic favor to creditors and asset holders). The inflation target (typically 2%) is presented as scientifically optimal, obscuring that alternative targets (1% or 3%) would produce different distributional outcomes. Central bank communications increasingly emphasize the technical competence and independence of the institution, with less acknowledgment of the distributional consequences. The theater has increased over the interval as measured inflation diverges from experienced inflation for wage-earning households.
 *
 * PERSPECTIVAL GAP:
 *   The regime demonstrates how the same constraint produces opposite classifications depending on observer position. Central banks and creditors see rope (coordination). Wage earners see snare (extraction). The gap is not a measurement problem but structural: the regime genuinely coordinates expectations while simultaneously extracting from wage earners. The tangled rope classification at the analytical level acknowledges both functions. The false summit risk: the regime's legitimating narrative ('price stability is good for all') naturalizes what is actually a contingent institutional choice. Alternative regimes (employment targeting, distributional stability, fiscal dominance) would produce different classifications from the same base structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position. Wage earners are trapped with high d (~0.95) — they bear extraction and have no exit. Central banks have low d (~0.05-0.15) — they are net beneficiaries with arbitrage options. Asset holders have low-to-moderate d (~0.10-0.30) — beneficiaries with exit options. The regime derives d from beneficiary/victim declarations: beneficiaries (central banks, asset holders, creditors) get low d yielding negative or minimal χ; victims (wage earners, debtors) get high d yielding high χ. The institutional analytical perspective derives d from the asymmetry between coordination benefit (real, significant) and extraction cost (real, borne by specific groups). The effective extractiveness χ is scaled by spatial scope (global regime scope σ=1.2 amplifies extraction) and agent power (powerless agents with high d experience maximum χ).
 *
 * MANDATROPHY ANALYSIS:
 *   The inflation targeting regime resolves mandatrophy by acknowledging that 'price stability' is not a neutral good but a policy choice with distributional consequences. The genuine coordination function (expectations stabilization) justifies calling this a hybrid rather than pure extraction. But the tangled rope classification requires demonstrating both the coordination function AND the asymmetric extraction — which the regime exhibits. The central question: could the coordination benefits be achieved with lower extraction? Alternative designs (automatic wage indexation, wealth-inclusive inflation measures, dual monetary-fiscal mandates) might reduce extraction while preserving coordination. The current regime is not the only possible inflation-targeting arrangement; it is one design choice among others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_measurement_divergence,
    'Does the inflation target (typically 2% measured by CPI/PCE) represent actual price stability across consumption baskets and time horizons?',
    'Cross-household inflation measurement; disaggregated price tracking by income quintile, age cohort, and region; comparison of objective inflation (what actually happened to prices) vs subjective inflation (what people experienced)',
    'If significant divergence: the regime is performatively stable while experientially volatile for wage earners. If convergence: the regime genuinely coordinates price stability. This determines whether the piton classification (theater ratio ~0.58) is conservative or optimistic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_measurement_divergence, empirical, 'Gap between inflation measurement and heterogeneous price experience').

omega_variable(
    wage_adjustment_speed_vs_target,
    'Do nominal wage contracts systematically adjust to the inflation target, or do wages lag, creating systematic real wage loss for employees?',
    'Longitudinal wage data controlling for productivity, industry, and tenure; comparison of real wage growth before and after inflation targeting adoption; wage adjustment frequency across labor market segments',
    'If wages track inflation target: snare classification overstates extraction (should be rope). If wages lag: snare classification is conservative. This determines whether powerless agents'' d value (~0.95) correctly represents structural extraction or whether nominal wage rigidity is coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_adjustment_speed_vs_target, empirical, 'Lag between nominal wage adjustment and inflation target').

omega_variable(
    fiscal_constraint_credibility,
    'Is the constraint on fiscal policy authority a real structural feature of the inflation targeting regime, or a political convention that can be overridden by governments?',
    'Comparative analysis of fiscal space across central banking regimes; testing whether countries with explicit inflation targets face stricter fiscal constraints than countries with flexible mandates; identification of countries that have successfully overridden monetary dominance (Japan, US post-2008)',
    'If real structural constraint: scaffold sunset is long (50+ years). If political convention: fiscal authorities can exit at political cost alone (short sunset). This determines the confidence in the scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_constraint_credibility, conceptual, 'Whether monetary dominance is structural or political convention').

omega_variable(
    distributional_asymmetry_necessity,
    'Is the redistributive asymmetry (toward creditors, asset holders, away from wage earners) a necessary feature of inflation targeting or a policy design choice?',
    'Counterfactual comparison: could inflation targeting be implemented with automatic wage indexation, distributional stabilizers, or wealth-inclusive inflation measures? Analysis of central banks that have adopted alternative designs (e.g., employment-inclusive mandates in some countries)',
    'If necessary: the extraction in the tangled_rope classification is unavoidable. If design choice: the regime could be reformed to reduce extraction without losing coordination benefits. This determines whether extractiveness (0.52) could be lowered through policy redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_asymmetry_necessity, preference, 'Whether distributional asymmetry is necessary or chosen').

omega_variable(
    monetary_dominance_vs_fiscal_coordination,
    'Does the inflation targeting regime improve overall macroeconomic coordination relative to alternative frameworks (fiscal targeting, employment targeting, dual mandates), or does it create coordination failures by constraining fiscal policy?',
    'Comparison of macroeconomic volatility, employment stability, and distributional outcomes across countries with different frameworks; analysis of crisis periods (2008, 2020) when the regime was tested; modeling of coordination failures from fiscal-monetary conflict',
    'If improves coordination: the rope classification is stronger, extraction is justified cost. If creates failures: the tangled_rope with high suppression is more accurate. This determines the entire classification at institutional and analytical levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_dominance_vs_fiscal_coordination, empirical, 'Whether inflation targeting improves overall macroeconomic coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inflation_targeting_regime, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infl_tr_t0, inflation_targeting_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(infl_tr_t10, inflation_targeting_regime, theater_ratio, 10, 0.48).
narrative_ontology:measurement(infl_tr_t20, inflation_targeting_regime, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(infl_be_t0, inflation_targeting_regime, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(infl_be_t10, inflation_targeting_regime, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(infl_be_t20, inflation_targeting_regime, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inflation_targeting_regime, resource_allocation).
narrative_ontology:affects_constraint(inflation_targeting_regime, labor_market_wage_rigidity).
narrative_ontology:affects_constraint(inflation_targeting_regime, monetary_fiscal_dominance).
narrative_ontology:affects_constraint(inflation_targeting_regime, creditor_debtor_asymmetry).

% DUAL FORMULATION NOTE:
% Inflation targeting is downstream of multiple structural constraints: labor market institutions that create nominal wage rigidity (upstream), the delegation of central bank independence (upstream), and the creditor-debtor balance sheet distribution (upstream). The three downstream constraints represent the empirical manifestations of the regime: wage earners' real income loss, fiscal policy constraint, and asset holder advantage. Each downstream constraint can be analyzed separately with its own extractiveness value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inflation_targeting_regime, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
