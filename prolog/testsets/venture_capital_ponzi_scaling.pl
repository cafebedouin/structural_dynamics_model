% ============================================================================
% CONSTRAINT STORY: venture_capital_ponzi_scaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venture_capital_ponzi_scaling, []).

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
 *   constraint_id: venture_capital_ponzi_scaling
 *   human_readable: Venture Capital Ponzi Scaling Mechanism
 *   domain: finance/technology/institutional_economics
 *
 * SUMMARY:
 *   Venture capital Ponzi scaling is a structural mechanism where capital is
 *   deployed at exponentially increasing valuations, creating returns for
 *   early-stage investors that depend on finding later-stage investors
 *   willing to pay higher prices, independent of underlying cash generation
 *   or profitability. The constraint exhibits high suppression (opacity of
 *   cap tables, burn rates, and financial forecasts), high theater (narrative
 *   emphasis on TAM size, founder mythology, and exponential growth stories),
 *   and asymmetric extraction (early investors capture most upside while
 *   later investors and employees absorb downside risk). The extractiveness
 *   has increased from 0.35 to 0.68 over the measurement interval as the
 *   venture ecosystem has matured, capital concentration has increased, and
 *   valuation inflation has accelerated. This constraint is classified as a
 *   Snare from the perspectives of trapped employees and constrained
 *   late-stage investors, but as Rope from early-stage funds, Tangled Rope
 *   from founders (mixed coordination and extraction), and Piton from
 *   institutional observers (degraded but persistent through narrative
 *   power).
 *
 * KEY AGENTS:
 *   - Early-Stage Venture Funds: Primary beneficiary (institutional/arbitrage) — capture 100x-1000x returns if exits occur at later valuations; control information flow and pricing
 *   - Founder and Company Leadership: Mixed actor (powerful/arbitrage) — genuine coordination function (enable hiring, product, market expansion) but also asymmetric extraction (board control, liquidation preferences, information advantage)
 *   - Late-Stage Employees: Primary victim (powerless/trapped) — equity locked by vesting, information asymmetry about burn rate, no liquidity pathway except acquisition or IPO
 *   - Series C+ Institutional Investors: Secondary victim (moderate/constrained) — trapped by dilution, follow-on round pressure, information opacity; can exit via secondary markets but at significant cost
 *   - Secondary Debt Creditors (Banks): Structural victim (organized/constrained) — extend credit against inflated valuations, subordinated to equity in downside, regulatory constrained
 *   - Venture Capital Market Structure: Institutional actor (institutional/arbitrage) — persists through inertia despite high theater; alternative capital structures haven't fully matured
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing valuation escalation as inevitable market efficiency rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venture_capital_ponzi_scaling, 0.68).
domain_priors:suppression_score(venture_capital_ponzi_scaling, 0.72).
domain_priors:theater_ratio(venture_capital_ponzi_scaling, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venture_capital_ponzi_scaling, extractiveness, 0.68).
narrative_ontology:constraint_metric(venture_capital_ponzi_scaling, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(venture_capital_ponzi_scaling, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venture_capital_ponzi_scaling, snare).
narrative_ontology:human_readable(venture_capital_ponzi_scaling, "Venture Capital Ponzi Scaling Mechanism").
narrative_ontology:topic_domain(venture_capital_ponzi_scaling, "finance/technology/institutional_economics").

domain_priors:requires_active_enforcement(venture_capital_ponzi_scaling).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venture_capital_ponzi_scaling, early_stage_venture_firms).
narrative_ontology:constraint_beneficiary(venture_capital_ponzi_scaling, founder_insiders).
narrative_ontology:constraint_beneficiary(venture_capital_ponzi_scaling, exit_liquidity_buyers).
narrative_ontology:constraint_victim(venture_capital_ponzi_scaling, later_stage_investors).
narrative_ontology:constraint_victim(venture_capital_ponzi_scaling, employee_equity_holders).
narrative_ontology:constraint_victim(venture_capital_ponzi_scaling, debt_creditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-STAGE EMPLOYEE (SNARE) — Hired at Series B/C with four-year vesting schedule and acceleration triggered by exit event. Cannot exit without losing unvested equity; cannot liquidate held equity without company acquisition or IPO. Suppression is structural: golden handcuffs, information asymmetry about burn rate, contractual lock-in. Trapped agent experiencing pure extraction — the valuation multiplier benefits early insiders, not later employees. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SERIES C INSTITUTIONAL INVESTOR (SNARE) — Invested at $2B valuation expecting exit in 5-7 years. Compounding dilution and follow-on rounds at 3x-5x valuations create downside protection illusion (SAFEs, liquidation preferences) but trap capital at risk. High suppression: information asymmetry about burn rate, opacity of cap table, social pressure to follow or lose portfolio signaling. Exit is constrained not trapped — investor can exit via secondary markets or write-down, but at significant cost. Significant experienced extraction through dilution and timelock.
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY-STAGE VENTURE FUND (ROPE) — Seed and Series A investors have massive returns (100x-1000x) if exits occur at later valuations. The mechanism (raising capital at ever-higher valuations to create downstream returns) is perfectly functional from this position. Arbitrage via follow-on rounds and secondary sales. Suppression is low for this actor — they control information flow and pricing. Experiences the constraint as efficient capital allocation, not extraction. Net beneficiary.
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOUNDER/LEADERSHIP (TANGLED ROPE) — Genuine coordination function: capital raise enables hiring, product development, market expansion. Real benefits to founder (equity upside, company resources, status). But also asymmetric extraction: founder/insiders capture board control, information advantage, liquidation preference ordering. Later employees and investors bear cost of dilution while founder benefits from both capital and control. Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VENTURE CAPITAL MARKET STRUCTURE (PITON) — The institutional machinery of VC funding (pitch decks, SAFEs, term sheets, valuation formulas) persists through inertia and incumbent advantage despite degraded function. Theater ratio extremely high: metrics like 'TAM size,' 'unit economics,' 'burn rate' are presented as precision forecasts but are largely fictional. The entire apparatus maintains itself because alternative capital allocation mechanisms haven't fully matured (SPACs, crowdfunding, equity streaming). Functionally degraded but sustained by institutional lock-in.
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — At civilizational scale, the VC Ponzi structure persists because: (1) it generates real innovations (genuine coordination function), (2) distributed decision-making opacity masks the extraction at early stages (early returns look real), (3) legitimate uncertainty about startup outcomes provides epistemic cover for false confidence. Theater ratio high because the entire VC discourse (unicorn narratives, founder mythology, exponential growth stories) performs as motivation/legitimation rather than as accurate forecasting. Degraded but persistent through narrative power.
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: SECONDARY DEBT CREDITORS (TANGLED ROPE) — Banks extend credit against inflated equity valuations and unprofitable burn rates. Genuine coordination function: capital availability enables company operations. Asymmetric extraction: senior debt priority, collateral requirements, personal guarantees from founders shift downside to junior creditors. Organized actors (banking sector) constrained by regulatory requirements and competitive pressure to lend. Mixed coordination (funding necessary) and extraction (subordinated to equity holders).
constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venture_capital_ponzi_scaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venture_capital_ponzi_scaling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venture_capital_ponzi_scaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venture_capital_ponzi_scaling, TR),
    TR >= 0.70.

:- end_tests(venture_capital_ponzi_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Early-stage investors require later investors to sustain exponentially rising valuations, creating a funding pyramid where sustainability depends on continuous capital inflow at inflated prices. The extractiveness has grown from 0.35 to 0.68 over the measurement interval because: (1) capital concentration has increased (mega-funds have more power to set valuations), (2) unicorn narrative saturation has increased theater-driven capital deployment, (3) employee populations have grown (more trapped agents), (4) M&A acquirer valuations have decoupled from VC round prices (signal collapse). Suppression (0.72): Very high. Cap table opacity is endemic (investors often don't know their dilution until year 2-3 of holding period). Burn rate forecasts in pitch decks bear minimal relationship to actual cash consumption. Senior liquidation preferences are buried in term sheets. Personal guarantees from founders are hidden from other investors. Information asymmetry is maintained through institutional design: lead investors have access to detailed financials while syndicate co-investors see summary metrics. Theater ratio (0.81): Extremely high and increasing. VC discourse relies almost entirely on narrative: founder mythology ('visionary founder who will overcome all obstacles'), TAM size (often fictional or based on SAM confusion), unit economics (presented with high precision despite massive uncertainty), burn rate trajectory (submitted forecasts vs actual). The entire pitch-deck-and-term-sheet apparatus is performative — it motivates capital deployment while obscuring the fundamental question: are downstream acquirers willing to pay exit prices that deliver promised returns? Theater has increased from 0.55 to 0.81 because the narrative complexity has increased (ARR metrics, cohort retention, land-and-expand models) while the underlying forecasting accuracy has declined.
 *
 * PERSPECTIVAL GAP:
 *   Early-stage beneficiaries perceive Rope (efficient coordination, clear incentives). Late-stage victims perceive Snare (pure extraction, no exit). Founders perceive Tangled Rope (real capital enabling real work, but also control asymmetry). Market structure perceives Piton (degraded ritual sustained by inertia). The gap reveals the fundamental extraction mechanism: early investors need later investors to sustain valuations, so the entire institutional apparatus (SAFEs, liquidation preferences, term sheets, pitch-deck metrics) is engineered to extract from later investors and pass returns to earlier ones. The mechanism is hidden in preference ordering, cap table opacity, and information asymmetry — not in explicit coercion. This is why suppression is so high: the extraction is legalized and normalized through institutional design rather than enforced through coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early-stage funds, founders-as-early-insiders): low d, experience negative or low effective extraction through the chi formula. They capture upside without downside lock-in. Victims (late-stage investors, employees, debt creditors): high d, experience high effective extraction. They face dilution, lock-in, and limited exit paths. The preference ordering in cap tables (senior liquidation preferences to early investors, subordination of employee equity and debt) encodes the extraction mathematically. This is extracted through institutional design rather than explicit coercion — the terms are 'reasonable' (VC gets preference because they take early-stage risk), but the cumulative effect across rounds is to transfer wealth from later to earlier investors.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (extractiveness 0.68 > 0.70 threshold). The classification as Snare is robust: (1) High suppression (0.72) — information asymmetry, buried terms, opacity of burn rates and cap tables prevent victims from recognizing extraction until after lock-in. (2) High extractiveness (0.68) — early investors require later investors to sustain valuations; extraction is baked into the preference ordering. (3) Low coordination function — the mechanism generates innovation (genuine coordination of capital to productive uses) but this is a byproduct, not the primary function from the extraction perspective. The mandatrophy is resolved by recognizing that the VC Ponzi operates as TWO SEPARATE CONSTRAINTS: (a) Capital allocation for startup innovation (genuine coordination problem, Rope from social perspective), and (b) Valuation escalation to create early-investor returns (pure extraction mechanism, Snare from late-investor perspective). The two constraints are operationally linked but have different ε values. Write separate stories and link via network.affects_constraints: venture_capital_innovation_coordination (ε=0.25, Rope) upstream of venture_capital_ponzi_scaling (ε=0.68, Snare). The first is a real social benefit; the second is the institutional mechanism that captures that benefit for early investors at later investors' cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valuation_signal_collapse,
    'At what trigger point do late-stage investors recognize that valuation escalation is uncoupled from financial viability?',
    'Empirical analysis of acquirer valuations vs VC round valuations for down rounds and acquihires; comparison of public market multiples vs private market multiples for comparable companies',
    'If trigger is hit (acquisition price < Series B valuation): massive downside realization for C/D round investors. Snare classification confirmed empirically. If never hit: constraint operates as pure Ponzi (indefinite valuation escalation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(valuation_signal_collapse, empirical, 'Valuation signal collapse point for late-stage investors').

omega_variable(
    exit_liquidity_availability,
    'Is there sufficient downstream liquidity (acquirers, IPO capacity, secondaries) to absorb exit demand at claimed valuations?',
    'Ratio of VC-backed startup acquisition volume to VC capital deployed; IPO pre-revenue company count and valuations; secondary market transaction sizes and prices relative to latest round valuations',
    'If liquidity insufficient: winners are determined by exit timing (who exits before cascade). If liquidity adequate: extraction is temporary and lower than base estimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_liquidity_availability, empirical, 'Exit liquidity sufficiency for VC-backed companies').

omega_variable(
    burn_rate_forecasting_accuracy,
    'What is the accuracy of VC-backed startup burn rate forecasts submitted to Series B+ investors vs actual realized burn?',
    'Comparison of submitted financial models vs actual cash consumption for cohort of companies; analysis of forecast error distribution and bias direction',
    'If forecast accuracy > 80%: information asymmetry is lower, suppression overstated. If accuracy < 40%: forecasts are essentially fiction, suppression and extraction are higher than base estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burn_rate_forecasting_accuracy, empirical, 'Accuracy of startup burn rate forecasting').

omega_variable(
    founder_insider_alignment,
    'Do founder financial incentives (salary, liquidation preferences, board control) align with later investor interests?',
    'Analysis of founder compensation relative to employee compensation; liquidation preference ordering; board seat allocation vs equity ownership',
    'If misaligned: tangled rope classification confirmed, extraction mechanism is hidden in preference ordering. If aligned: coordination function is more genuine, Tangled Rope moves toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founder_insider_alignment, empirical, 'Founder-investor incentive alignment').

omega_variable(
    alternative_capital_structures_maturity,
    'How mature are alternative capital allocation mechanisms (equity crowdfunding, revenue-based financing, equity streaming) relative to traditional VC?',
    'Comparison of deployment volume, company success rates, investor returns, and time-to-exit for alternative structures vs VC-backed cohorts',
    'If alternatives mature: piton classification strengthens (VC persists through inertia). If alternatives remain marginal: VC structure may be genuinely optimal despite high theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_capital_structures_maturity, empirical, 'Maturity of alternative capital structures').

omega_variable(
    syndication_herding_mechanism,
    'Do follow-on round co-investors coordinate actual due diligence or copy lead investor signals?',
    'Analysis of due diligence patterns in follow-on rounds; correlation between lead and syndicate valuations; identification of syndicate participation after lead signal vs independent evaluation',
    'If herding: suppression is higher (information asymmetry maintained through signaling cascade). If independent: suppression lower, though still present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syndication_herding_mechanism, empirical, 'Investor syndication herding vs independent evaluation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venture_capital_ponzi_scaling, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vcps_tr_t0, venture_capital_ponzi_scaling, theater_ratio, 0, 0.55).
narrative_ontology:measurement(vcps_tr_t3, venture_capital_ponzi_scaling, theater_ratio, 3, 0.68).
narrative_ontology:measurement(vcps_tr_t6, venture_capital_ponzi_scaling, theater_ratio, 6, 0.78).
narrative_ontology:measurement(vcps_tr_t9, venture_capital_ponzi_scaling, theater_ratio, 9, 0.81).

% Extraction over time
narrative_ontology:measurement(vcps_be_t0, venture_capital_ponzi_scaling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vcps_be_t3, venture_capital_ponzi_scaling, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(vcps_be_t6, venture_capital_ponzi_scaling, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(vcps_be_t9, venture_capital_ponzi_scaling, base_extractiveness, 9, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venture_capital_ponzi_scaling, resource_allocation).
narrative_ontology:affects_constraint(venture_capital_ponzi_scaling, startup_burn_rate_opacity).
narrative_ontology:affects_constraint(venture_capital_ponzi_scaling, employee_equity_illiquidity).
narrative_ontology:affects_constraint(venture_capital_ponzi_scaling, venture_capital_innovation_coordination).

% DUAL FORMULATION NOTE:
% VC Ponzi scaling decomposes into two structurally distinct constraints: (1) venture_capital_innovation_coordination (ε=0.25, Rope) — genuine coordination of capital to productive innovation; (2) venture_capital_ponzi_scaling (ε=0.68, Snare) — valuation escalation mechanism that extracts from later investors to reward early ones. The Ponzi constraint is DOWNSTREAM of innovation coordination in the causal chain: capital coordination enables company building, which enables exit opportunities, which sustains the Ponzi mechanism. Measure (1) as a standalone Rope with beneficiaries=[founders, employees, society] and no victims. Measure (2) as this Snare with beneficiaries=[early_investors, founders_as_insiders] and victims=[late_investors, employees, creditors]. The decomposition reveals that the social benefit (innovation) is uncoupled from the extraction mechanism (valuation escalation), and that the Ponzi could be replaced by alternative capital structures without destroying the innovation function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(venture_capital_ponzi_scaling, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
