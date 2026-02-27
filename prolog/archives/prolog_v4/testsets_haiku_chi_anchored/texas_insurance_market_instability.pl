% ============================================================================
% CONSTRAINT STORY: texas_insurance_market_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_texas_insurance_market_instability, []).

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
 *   constraint_id: texas_insurance_market_instability
 *   human_readable: Texas Insurance Market Instability
 *   domain: economic/regulatory/risk_management
 *
 * SUMMARY:
 *   The Texas homeowners insurance market is undergoing structural
 *   destabilization driven by converging climate, regulatory, and actuarial
 *   pressures. Since 2010, major insurers have reduced market share, rates
 *   have accelerated beyond inflation, and policy cancellations in high-risk
 *   coastal and hail-prone areas have surged. The constraint exhibits a
 *   tangled coordination-extraction hybrid: the market performs a genuine
 *   coordination function (pooling catastrophic risk across homeowners), but
 *   this coordination is increasingly asymmetric, with tail-risk
 *   concentration shifting from insurers toward property owners and residual
 *   pools. Regulatory rate restrictions, intended as consumer protection,
 *   have the paradoxical effect of suppressing price signals, masking the
 *   constraint's extraction mechanisms, and increasing theater (the fiction
 *   that 'normal' insurance still functions for high-risk areas). The Texas
 *   Windstorm Insurance Association, designed as a residual insurer of last
 *   resort, has grown to insure ~2% of coastal properties — a sign that the
 *   primary market has largely retreated from tail-risk coverage. The
 *   constraint's extractiveness has increased over 2010-2026 as
 *   climate-driven disaster frequency has risen while insurance capital
 *   accumulation has lagged. The theater ratio reflects that actuarial models
 *   still use historical climate assumptions, creating a performative
 *   appearance of stability despite underlying uninsurability in marginal
 *   areas.
 *
 * KEY AGENTS:
 *   - Homeowners in High-Risk Coastal/Hail Areas: Primary victims (powerless/trapped) — cannot exit without abandoning property; bear full cost of withdrawal from primary market
 *   - Homeowners in Moderate-Risk Areas: Secondary victims (moderate/constrained) — face rising premiums and coverage restrictions; still able to access primary market at cost
 *   - Surviving Major Insurers (Allstate, State Farm regional affiliates): Primary beneficiaries (institutional/arbitrage) — protected by Lloyds Plan mechanisms and regulatory restrictions that prevent new entrants; benefit from controlled market share
 *   - Texas Department of Insurance Regulators: Mixed institutional actor (organized/constrained) — constrained by legislative mandate to keep rates affordable; benefit from regulatory authority and coordination function but face solvency pressure
 *   - Texas Windstorm Insurance Association (TWIA): Institutional residual actor (institutional/constrained) — absorbs tail-risk losses through post-hoc assessments on surviving carriers; constrained by statutory mandate but growing market share indicates primary market failure
 *   - Insurance Rating Bureaus/Actuarial Models: Legacy institutional system (institutional/arbitrage) — persist through inertia; actuarial assumptions (pre-2010 climate baselines) losing predictive power
 *   - Analytical Observer: Sees market failure where tail-risk has become uninsurable at actuarially sustainable premiums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(texas_insurance_market_instability, 0.58).
domain_priors:suppression_score(texas_insurance_market_instability, 0.68).
domain_priors:theater_ratio(texas_insurance_market_instability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(texas_insurance_market_instability, extractiveness, 0.58).
narrative_ontology:constraint_metric(texas_insurance_market_instability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(texas_insurance_market_instability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(texas_insurance_market_instability, tangled_rope).
narrative_ontology:human_readable(texas_insurance_market_instability, "Texas Insurance Market Instability").
narrative_ontology:topic_domain(texas_insurance_market_instability, "economic/regulatory/risk_management").

domain_priors:requires_active_enforcement(texas_insurance_market_instability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(texas_insurance_market_instability, surviving_major_insurers).
narrative_ontology:constraint_beneficiary(texas_insurance_market_instability, state_insurance_commissioner_office).
narrative_ontology:constraint_victim(texas_insurance_market_instability, homeowners_high_risk_areas).
narrative_ontology:constraint_victim(texas_insurance_market_instability, texas_property_owners).
narrative_ontology:constraint_victim(texas_insurance_market_instability, market_solvency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOMEOWNERS IN HIGH-RISK AREAS (SNARE) — Cannot exit the insurance constraint without abandoning property. Bears full cost of increasing premiums, coverage restrictions, and policy cancellations. Texas-specific geography (hurricane zones, hail-prone areas) creates permanent trap. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(texas_insurance_market_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HOMEOWNERS IN MODERATE-RISK AREAS (TANGLED ROPE) — Constrained by mortgage/property tax requirements and limited alternative carriers. Also benefit from market existence and the coordination function of pooled risk. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.55.
constraint_indexing:constraint_classification(texas_insurance_market_instability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SURVIVING MAJOR INSURERS (ROPE) — Benefit from regulatory protection (Lloyds Plan prevents unlimited rate-raising, State Board rate restrictions create cooperative oligopoly). Experience constraint as coordination mechanism: guaranteed market share through residual insurer system. d≈0.15, f(d)≈0.05, σ=0.9 → χ≈0.03. Net beneficiary via regulatory arbitrage.
constraint_indexing:constraint_classification(texas_insurance_market_instability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: STATE INSURANCE COMMISSIONER/REGULATORS (TANGLED ROPE) — Constrained by legislative mandate to keep rates affordable while ensuring solvency. Benefit from coordination function (preventing market collapse) but also extract through regulatory authority over rates and company operations. Organized institutional actor with measurable agency. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(texas_insurance_market_instability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE RATING BUREAU/LEGACY SYSTEMS (PITON) — Historical actuarial models (pre-2010) were designed for climate stability assumptions now violated by increasing frequency/severity of disasters. System persists through institutional inertia despite losing predictive power. Theater ratio = 0.55: actuaries claim precision while underlying climate assumptions degrade. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.19.
constraint_indexing:constraint_classification(texas_insurance_market_instability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET FAILURE VIEW (SNARE) — From a civilizational perspective, the constraint reflects a fundamental misalignment between insurable risk and insurability: climate change increases disaster frequency faster than premium revenues can accumulate capital. The market structure itself (annual contracts, thin reserve buffers) cannot price true tail risks. This is extraction by physical reality against human institutions. d≈0.88, f(d)≈1.30, σ=1.2 → χ≈0.75.
constraint_indexing:constraint_classification(texas_insurance_market_instability, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(texas_insurance_market_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(texas_insurance_market_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(texas_insurance_market_instability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(texas_insurance_market_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(texas_insurance_market_instability, TR),
    TR >= 0.70.

:- end_tests(texas_insurance_market_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from property owners through (a) rapid premium increases in high-risk areas, (b) policy cancellations forcing migration to TWIA residual pool at higher cost, (c) coverage restrictions (higher deductibles, reduced dwelling coverage), and (d) implicit subsidy where moderate-risk homeowners subsidize tail-risk capital via cross-subsidization within surviving carriers. The extraction is asymmetric: insurers capture pricing authority while homeowners' exit options are limited (property-specific, geographically trapped). However, extractiveness is not maximal (0.70+) because the market still provides real insurance function and surviving carriers genuinely absorb some catastrophic risk. Suppression (0.68): High. Significant barriers to exit include: (a) regulatory requirement for mortgage-backed insurance, (b) TWIA is not a competitive alternative (higher cost, worse coverage), (c) self-insurance is not realistic for most homeowners (capital barriers), (d) information asymmetry (homeowners often don't know climate risk escalation), and (e) limited geographic mobility for property-attached investment. Suppression is not total (100%) because homeowners retain the ultimate exit (sell property), but this is costly and not accessible. Theater ratio (0.55): Moderate. Actuarial models still perform risk assessment based on pre-2010 climate baselines, creating an appearance of precision and stability. Rate filings present historical actuarial confidence while underlying climate assumptions have degraded. However, the market has not become purely theatrical — insurers do accumulate real capital, pay real claims, and operate under genuine solvency constraints. Theater is present but not dominant.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a clear perspectival divergence based on structural position. High-risk homeowners (powerless/trapped) see a snare: they cannot exit and face extraction through rapidly rising costs or forced migration to the residual pool. Moderate-risk homeowners (moderate/constrained) see a tangled rope: they still benefit from insurance coordination but increasingly subsidize tail-risk concentration. Surviving major insurers see a rope: regulatory protection and controlled market share provide stable extraction within acceptable bounds. Regulators see a tangled rope from their perspective (constrained by legislative pressure, benefit from coordination function they manage). Legacy actuarial systems see themselves as pitons (performative precision masking degraded underlying models). The analytical observer sees a pure snare (market structure is unraveling under climate acceleration). The perspectival gap is driven by differential exit options: insurers can arbitrage across national markets; homeowners are geographically trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk homeowners: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit option; bear full cost. Moderate-risk homeowners: Victims + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; some exit (property sale) exists but is costly. Surviving major insurers: Beneficiaries + arbitrage → d≈0.15, f(d)≈0.05. Net beneficiary; can exit (already withdrawn from high-risk markets), benefit from regulatory protection. State regulators: Mixed institutional; victims of legislative mandate + beneficiaries of regulatory authority → d≈0.45, f(d)≈0.45. Moderate extraction experience; constrained but have agency. Actuarial systems: Institutional + arbitrage → d≈0.35, f(d)≈0.35. Piton classification derives from theater gate, not from high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy presents as: Is this market failure a genuine coordination problem (rope/tangled rope with sustainable equilibrium) or an emerging snare (asymmetric extraction predicting market collapse)? The constraint resolves toward snare because: (1) extractiveness has increased monotonically (0.35→0.58) while disaster frequency accelerates, indicating the constraint is not stabilizing; (2) the coordination function (catastrophic risk pooling) is being increasingly bypassed for high-risk areas via residual pools and market withdrawal, suggesting the coordination is failing; (3) regulatory suppression of price signals (theater) is masking rather than solving the underlying misalignment between insurable risk and insurability. The tangled rope classification persists at the aggregate market level because survival insurers do still provide coordination, and moderate-risk areas still experience rope-like pooling benefits. But within high-risk submarket, the classification approaches pure snare. This is a constraint in transition from tangled rope (historical, 2010-2015) toward snare (emerging, 2020-2026), with regulatory theater delaying visibility of the shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_acceleration_tail_risk,
    'Is the Texas hurricane/hail frequency increase a temporary statistical fluctuation or a structurally accelerating tail-risk regime shift?',
    '40-year climate data analysis; attribution modeling of disaster frequency trends; comparison to 20th-century baselines vs 2010-2026 observed rates',
    'If temporary: snare classification is overstated; market stabilization is possible within 5-10 years. If structural regime shift: snare is accurate; current pricing is perpetually inadequate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_acceleration_tail_risk, empirical, 'Whether disaster frequency increase is temporary or structural').

omega_variable(
    insurable_risk_boundary,
    'At what disaster frequency does tail-risk insurance become actuarially impossible (premium required > 50% of property value annually)?',
    'Quantitative actuarial modeling; calculation of required capital buffers for different disaster frequencies; comparison to historical precedent (e.g., Florida 2004-2006)',
    'If boundary already crossed: high-risk Texas areas are uninsurable in principle, making snare classification fundamental. If boundary > 5 years away: current squeeze is temporary, tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurable_risk_boundary, empirical, 'Boundary at which tail-risk insurance becomes actuarially impossible').

omega_variable(
    regulatory_rate_suppression_mechanism,
    'Do Texas Department of Insurance rate restrictions function as genuine consumer protection (rope coordination) or as regulatory capture that shifts tail-risk onto property owners (snare extraction)?',
    'Historical analysis of rate-setting decisions; comparison of Texas premium growth to climate-adjusted risk increase; solvency outcomes of insurers operating under restrictions',
    'If protection: regulatory constraint is legitimate coordination; tangled rope is correct classification with sustainable equilibrium. If capture: regulation is extractive mask; underlying snare is being suppressed by theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_rate_suppression_mechanism, conceptual, 'Whether rate restrictions protect consumers or suppress snare visibility').

omega_variable(
    residual_insurer_sustainability,
    'Can the Texas Windstorm Insurance Association (TWIA, residual pool) indefinitely absorb catastrophic losses through assessments on surviving carriers without triggering market collapse?',
    'TWIA solvency modeling; projection of assessment burdens under accelerating disaster scenarios; analysis of insurers'' exit thresholds (at what assessment level do remaining carriers leave market)',
    'If sustainable: market structure persists; snare is contained within residual system. If unsustainable: chain-reaction collapse risk increases; snare classification is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_insurer_sustainability, empirical, 'Whether residual insurer system can sustain accelerating catastrophic losses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(texas_insurance_market_instability, 2010, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(texas_ins_tr_t0, texas_insurance_market_instability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(texas_ins_tr_t5, texas_insurance_market_instability, theater_ratio, 5, 0.47).
narrative_ontology:measurement(texas_ins_tr_t10, texas_insurance_market_instability, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(texas_ins_be_t0, texas_insurance_market_instability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(texas_ins_be_t5, texas_insurance_market_instability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(texas_ins_be_t10, texas_insurance_market_instability, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(texas_insurance_market_instability, resource_allocation).
narrative_ontology:boltzmann_floor_override(texas_insurance_market_instability, 0.42).
narrative_ontology:affects_constraint(texas_insurance_market_instability, climate_driven_property_value_collapse).
narrative_ontology:affects_constraint(texas_insurance_market_instability, mortgage_credit_availability_cascade).
narrative_ontology:affects_constraint(texas_insurance_market_instability, texas_coastal_development_subsidy).

% DUAL FORMULATION NOTE:
% The Texas insurance market instability is upstream of specific property value effects and mortgage market cascade failures. The constraint has extractiveness 0.58 reflecting direct homeowner cost; downstream constraints show how extraction propagates through real estate and credit markets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(texas_insurance_market_instability, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
