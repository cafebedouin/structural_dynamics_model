% ============================================================================
% CONSTRAINT STORY: wholesale_electricity_market_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wholesale_electricity_market_consolidation, []).

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
 *   constraint_id: wholesale_electricity_market_consolidation
 *   human_readable: Wholesale Electricity Market Consolidation and Regional Monopoly Power
 *   domain: energy_economics/industrial_organization
 *
 * SUMMARY:
 *   The consolidation of wholesale electricity markets in North America over
 *   the past 15 years has produced a structural transformation in how power
 *   is generated, dispatched, and priced. Mergers of utilities, acquisitions
 *   of independent generators by holding companies, and the rise of
 *   vertically integrated megafirms have reduced the number of competitive
 *   participants while increasing the scale and geographic scope of
 *   consolidated operators. This constraint exhibits the characteristic
 *   signature of tangled rope: genuine coordination gains (unified dispatch,
 *   transmission planning, reserve margin optimization) coexist with
 *   asymmetric extraction (reduced competition suppresses independent
 *   generator margins and retail price pressure). The constraint cannot be
 *   classified as pure coordination (rope) because consolidation actively
 *   suppresses alternative market structures. It cannot be classified as pure
 *   extraction (snare) because the operational coordination benefits are real
 *   and material. Extractiveness has increased from 0.32 (early consolidation
 *   phase, relatively competitive remaining independents) to 0.58 (modern
 *   phase with few viable alternatives to consolidated utility service).
 *   Theater ratio remains relatively low (0.48) because wholesale market
 *   auctions maintain genuinely competitive appearances even as structural
 *   consolidation narrows the competitive set.
 *
 * KEY AGENTS:
 *   - Large Incumbent Generators: Primary beneficiary (institutional/arbitrage) — consolidation expands their market power, captures efficiency gains, reduces competitive pricing pressure
 *   - Independent Generators: Primary victim (powerless/trapped) — faces consolidation-driven exit barriers, suppressed dispatch economics, forced acquisitions at unfavorable terms
 *   - Retail Consumers: Secondary victim (powerless/trapped) — faces higher retail rates as wholesale competition declines and utilities expand margins
 *   - Regulatory Authority (FERC/State PUCs): Institutional actor (moderate/constrained) — oversees market while facing genuine coordination problems (grid reliability, renewable integration) that consolidation partially solves but also captures for extraction
 *   - Grid Reliability System: Organized actor (organized/constrained) — benefits from consolidated coordination but trapped by dependence on consolidating utilities for execution
 *   - Competitive Market Model: Institutional actor (institutional/arbitrage) — maintains formal competitive structure through RTO/ISO auctions despite consolidation reducing substantive competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wholesale_electricity_market_consolidation, 0.58).
domain_priors:suppression_score(wholesale_electricity_market_consolidation, 0.65).
domain_priors:theater_ratio(wholesale_electricity_market_consolidation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wholesale_electricity_market_consolidation, extractiveness, 0.58).
narrative_ontology:constraint_metric(wholesale_electricity_market_consolidation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(wholesale_electricity_market_consolidation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wholesale_electricity_market_consolidation, tangled_rope).
narrative_ontology:human_readable(wholesale_electricity_market_consolidation, "Wholesale Electricity Market Consolidation and Regional Monopoly Power").
narrative_ontology:topic_domain(wholesale_electricity_market_consolidation, "energy_economics/industrial_organization").

domain_priors:requires_active_enforcement(wholesale_electricity_market_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wholesale_electricity_market_consolidation, large_incumbent_generators).
narrative_ontology:constraint_beneficiary(wholesale_electricity_market_consolidation, utility_holding_companies).
narrative_ontology:constraint_victim(wholesale_electricity_market_consolidation, independent_generators).
narrative_ontology:constraint_victim(wholesale_electricity_market_consolidation, retail_consumers).
narrative_ontology:constraint_victim(wholesale_electricity_market_consolidation, grid_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT GENERATOR (SNARE) — Small natural gas or renewable generators face consolidation-driven market exit barriers. Cannot compete with vertically integrated utilities on dispatch pricing; cannot exit without abandoning capital investment. Bears extraction cost through suppressed dispatch economics and forced buyouts at depressed valuations.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RETAIL CONSUMER (SNARE) — Faces structural reduction in wholesale competition as independent generators exit market. Trapped in consolidated utility service territory; cannot switch providers or negotiate directly with generators. Bears extraction cost through reduced downward price pressure and higher retail rates.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (TANGLED ROPE) — Constrained by FERC jurisdiction limits, state-level regulatory fragmentation, and complexity of monitoring real-time dispatch. Faces genuine coordination problem (maintaining reliable grid dispatch across consolidated utilities) alongside extraction mechanism (utilities suppressing independent competition to raise barriers to entry). High suppression despite moderate power because regulatory tools are asymmetrical — can monitor prices but cannot mandate competitive entry.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSOLIDATED UTILITY (ROPE) — Experiences the constraint as coordination mechanism: consolidated dispatch, unified transmission planning, and economies of scale in generation portfolio. Net beneficiary with significant arbitrage options (can expand geographically, diversify fuel mix, exit unprofitable segments). Extraction mechanism is obscured by efficiency gains — utilities claim consolidation reduces costs, capturing the efficiency gains while suppressing independent generator access.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPETITIVE MARKET MODEL (PITON) — The deregulated wholesale market structure (RTO/ISOs, locational marginal pricing, competitive dispatch auctions) persists as institutional framework despite degrading function as consolidation reduces effective competition. Theater ratio (0.48) reflects that auction mechanisms maintain formal competitive appearances while market structure concentrates power. Competitive model is maintained through inertia — network effects and sunk infrastructure make alternative governance costly to replace.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GRID RELIABILITY SYSTEM (TANGLED ROPE) — Genuine coordination function (consolidated utilities improve transmission planning and reserve margin coordination) mixed with asymmetric extraction (consolidation reduces competition → reduced pressure for efficiency → higher reliability costs passed to consumers). Organized but constrained by interdependencies with consolidating utilities.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, electricity systems display natural monopoly characteristics in transmission and distribution, and some degree of consolidation may be inevitable given network effects. This perspective risks naturalizing what is actually a policy choice: markets for generation capacity could remain competitive even with consolidated transmission/distribution ownership. False summit detection will identify this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wholesale_electricity_market_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wholesale_electricity_market_consolidation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wholesale_electricity_market_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wholesale_electricity_market_consolidation, TR),
    TR >= 0.70.

:- end_tests(wholesale_electricity_market_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting a genuine mix of coordination and extraction. Early consolidation (T=0, ε=0.32) offered meaningful efficiency gains — united dispatch reduced transmission losses, improved reserve coordination, achieved economies of scale in generation portfolio management. Modern consolidation (T=14, ε=0.58) has extracted more of these gains into utility margins rather than passing them through to consumers. The upward trajectory reflects mounting evidence that consolidation is being used to suppress independent generation rather than to achieve unavoidable technical coordination. Suppression (0.65): High. Barriers to independent generator entry and operation include: (1) consolidated utilities controlling dispatch priority favoring their own generation, (2) long-term capacity contracts that lock out competitors, (3) transmission access and pricing opacity that favors integrated carriers, (4) reserve margin requirements that incentivize utilities to overbuild rather than purchase from independents, (5) regulatory complexity that favors well-resourced incumbents. Theater ratio (0.48): Moderate. RTO/ISO wholesale auctions maintain formal competitive mechanisms (locational marginal pricing, security-constrained economic dispatch, capacity auctions) that create the appearance of robust competition. However, the structural consolidation narrows the competitive set — many auction participants are subsidiaries of the same holding company, reducing substantive diversity. The theater has increased over the interval as the gap between formal mechanisms and substantive market concentration has widened.
 *
 * PERSPECTIVAL GAP:
 *   The independent generator sees pure snare — they face a consolidation machine designed to buy them out or drive them bankrupt, with no escape and no coordination benefit. The retail consumer sees snare from the distribution monopoly layer but also sees the wholesale consolidation effect transmitted through higher utility procurement costs. The consolidated utility sees rope — consolidation genuinely solves their dispatch and planning problems, enabling efficiency gains and operational resilience. The regulatory authority sees tangled rope with high uncertainty about whether the coordination benefits justify the extraction costs. The piton perspective reveals the competitive market model as increasingly performative — auctions continue but consolidation has narrowed the substantive competitive set, theater has risen as the gap between form and substance expands. The analytical observer risks the false summit of seeing consolidation as inevitable (natural monopoly) when it actually reflects policy choices about market structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation flows from the beneficiary/victim structure and exit option constraints. Large utilities as beneficiaries with arbitrage options (can expand, diversify, exit segments) experience low effective extraction — d ≈ 0.15-0.20 — because they can navigate around market constraints. Independent generators as victims with trapped options (cannot exit investment sunk in generation assets, cannot relocate physical infrastructure) experience high extraction — d ≈ 0.85-0.95 — because consolidation directly suppresses their economic viability. The regulatory authority as a constrained institutional actor faces moderate d (≈ 0.50-0.65) because it has nominal power but faces genuine technical coordination problems (grid reliability, renewable integration) that consolidation partly solves, creating moral hazard in oversight. Retail consumers as trapped powerless agents in regional monopolies experience high d (≈ 0.80) despite being structurally smaller actors because they have no exit options and no direct participation in wholesale market decisions that determine their rate base.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by establishing that consolidation exhibits genuine tangled rope structure: (1) Real coordination function — unified dispatch and transmission planning produce measurable operational efficiency gains, validated by engineering analysis and industry data. (2) Asymmetric extraction — these coordination benefits are captured primarily by consolidated utilities through margin expansion rather than passed to consumers or paid to independent generators. (3) Active enforcement requirement — consolidation is maintained through regulatory approval processes (merger reviews, rate base treatment) that could be reformed to prevent it. The constraint is NOT pure extraction (snare) because the coordination benefits are material. The constraint is NOT pure coordination (rope) because consolidation actively suppresses competitive alternatives that could achieve coordination at lower extraction cost. The structure satisfies all three tangled rope gates: beneficiaries (utilities), victims (independent generators, consumers), requires_active_enforcement (merger approval, FERC oversight).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consolidation_efficiency_vs_extraction,
    'Do consolidation-driven efficiency gains (reduced transmission losses, better dispatch coordination, economies of scale) materialize as cost reductions for consumers, or are they captured by utilities through margin expansion?',
    'Time series analysis of average generation costs vs retail electricity prices in consolidated vs competitive markets; comparison of transmission loss rates and reserve margin efficiency pre- and post-consolidation',
    'If efficiency gains are passed through: tangled rope classification confirmed, suppression underestimated. If captured by utilities: snare classification strengthened, extraction understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consolidation_efficiency_vs_extraction, empirical, 'Whether consolidation efficiency gains flow to consumers or are captured by utilities').

omega_variable(
    regulatory_capture_depth,
    'To what extent have consolidated utilities captured FERC and state regulatory processes, constraining regulator power below its nominal authority level?',
    'Analysis of FERC enforcement patterns, abandoned investigations, regulatory agency staffing and revolving door patterns, utility lobbying expenditure correlation with regulatory outcomes',
    'If capture is substantial: regulatory authority''s exit_options should be downgraded from constrained to trapped, elevating suppression. Effective d for regulatory agents increases significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth of regulatory capture by consolidated utilities').

omega_variable(
    independent_generator_coalition_potential,
    'Can independent generators and retail consumers form effective political coalition to resist consolidation, or are they too fragmented for coordinated advocacy?',
    'Analysis of independent generator association capacity, consumer advocacy organization funding and efficacy, historical coalition-building patterns in electricity market politics',
    'If coalition capacity is high: powerless agents could upgrade to organized power level in near future, changing classification outcomes. If coalition capacity is low: snare classification solidifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_generator_coalition_potential, empirical, 'Whether independent generators can form effective political coalition').

omega_variable(
    technological_bypass_possibility,
    'Do distributed renewable generation and storage technologies create genuine bypass pathways for retail consumers, reducing their structural trapedness?',
    'Analysis of DER adoption rates in consolidated vs competitive markets, economics of behind-the-meter battery storage, grid-defection cost trajectories, technical feasibility of microgrids in different geographic contexts',
    'If bypass is technically feasible and economically viable: consumer exit_options upgrade from trapped to constrained or mobile. If blocked by regulatory barriers or economics: trapedness confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_bypass_possibility, empirical, 'Whether DER and storage enable consumer bypass of consolidated utilities').

omega_variable(
    transmission_natural_monopoly_irreducibility,
    'Is transmission system consolidation technically necessary for reliability and coordination, or can competitive generation coexist with consolidated transmission (standard industry model)?',
    'Engineering analysis of operational requirements; comparative study of markets with unbundled generation/transmission (Texas ERCOT, PJM) vs fully vertically integrated systems; identification of dispatch coordination tasks that absolutely require consolidation vs those achievable with contractual coordination',
    'If transmission consolidation alone suffices: generation consolidation is not technically necessary, extracting classification is confirmed. If generation consolidation is technically necessary: coordination function is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_natural_monopoly_irreducibility, empirical, 'Technical necessity of generation consolidation given transmission integration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wholesale_electricity_market_consolidation, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wemc_tr_t0, wholesale_electricity_market_consolidation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wemc_tr_t7, wholesale_electricity_market_consolidation, theater_ratio, 7, 0.42).
narrative_ontology:measurement(wemc_tr_t14, wholesale_electricity_market_consolidation, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(wemc_be_t0, wholesale_electricity_market_consolidation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(wemc_be_t7, wholesale_electricity_market_consolidation, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(wemc_be_t14, wholesale_electricity_market_consolidation, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wholesale_electricity_market_consolidation, resource_allocation).
narrative_ontology:affects_constraint(wholesale_electricity_market_consolidation, renewable_integration_grid_stability).
narrative_ontology:affects_constraint(wholesale_electricity_market_consolidation, utility_rate_base_cost_of_capital).
narrative_ontology:affects_constraint(wholesale_electricity_market_consolidation, transmission_access_independent_generation).

% DUAL FORMULATION NOTE:
% Wholesale consolidation is downstream of individual utility mergers but represents a distinct structural constraint on market organization. Individual merger stories (utility_A_acquisition_utility_B) have higher ε values reflecting more transparent extraction; this meta-constraint story captures the system-level extractiveness emerging from accumulated individual consolidations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wholesale_electricity_market_consolidation, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
