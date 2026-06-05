% ============================================================================
% CONSTRAINT STORY: lng_price_volatility_transmission
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lng_price_volatility_transmission, []).

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
 *   constraint_id: lng_price_volatility_transmission
 *   human_readable: LNG Price Volatility Transmission Mechanism
 *   domain: energy_markets/geopolitics
 *
 * SUMMARY:
 *   LNG price volatility transmission represents a global energy constraint
 *   that couples commodity price shocks to consumer electricity and heating
 *   costs across continents. The mechanism operates through liquefied natural
 *   gas supply chains where physical constraints (limited production
 *   capacity, months-long voyage times, regasification bottlenecks) combine
 *   with financial structures (long-term contracts, spot market pricing,
 *   take-or-pay obligations) to create persistent price transmission from
 *   producers to end consumers. The constraint exhibits properties of both
 *   pure coordination (solving the problem of moving gas from production
 *   sites to distant markets) and asymmetric extraction (price volatility is
 *   concentrated on inelastic consumers while producers and intermediaries
 *   capture arbitrage rents). The mechanism has strengthened since 2010 as
 *   global LNG markets have deregulated, integrated, and expanded, but it is
 *   structurally impermanent — renewable energy and battery storage offer
 *   real exit pathways with sunset horizons of 15-25 years. The constraint's
 *   theater_ratio (0.51) reflects that substantial ceremonial activity
 *   (oil-indexation formulas) persists in contracts while real price
 *   discovery happens in spot and hub markets; the gap indicates a piton-like
 *   degradation pathway in which formal mechanisms have decoupled from
 *   functional ones.
 *
 * KEY AGENTS:
 *   - Price-Sensitive Consumers: Primary victims (powerless/trapped) — households in developing importers bear full cost of volatility with no exit options; grid infrastructure locked in
 *   - Developing Importer Nations: Secondary victims (moderate/constrained) — balance energy security needs against vulnerability to price shocks; limited foreign reserves and negotiating capacity
 *   - LNG Exporting Nations/Companies: Primary beneficiaries (institutional/arbitrage) — capture rents from price volatility through portfolio diversification and spot market participation
 *   - Wealthy Importing Nations: Secondary beneficiaries (powerful/constrained) — experience volatility but can absorb shocks and negotiate favorable contract terms
 *   - Trading Intermediaries: Beneficiaries (institutional/arbitrage) — extract value from price spreads and information asymmetries across regional markets
 *   - Oil-Indexation Formula System: Institutional actor (institutional/arbitrage) — maintains contractual but largely performative coupling to crude oil prices
 *   - Regulatory Reform Coalition: Organized agents (organized/constrained) — IEA, EU, multilateral development banks; building alternative pathways (reserves, hubs, decarbonization)
 *   - Power Grid Operators: Victims (institutional/constrained) — face stability challenges when price volatility triggers fuel switching or demand destruction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lng_price_volatility_transmission, 0.58).
domain_priors:suppression_score(lng_price_volatility_transmission, 0.62).
domain_priors:theater_ratio(lng_price_volatility_transmission, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lng_price_volatility_transmission, extractiveness, 0.58).
narrative_ontology:constraint_metric(lng_price_volatility_transmission, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lng_price_volatility_transmission, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lng_price_volatility_transmission, tangled_rope).
narrative_ontology:human_readable(lng_price_volatility_transmission, "LNG Price Volatility Transmission Mechanism").
narrative_ontology:topic_domain(lng_price_volatility_transmission, "energy_markets/geopolitics").

domain_priors:requires_active_enforcement(lng_price_volatility_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lng_price_volatility_transmission, lng_exporters).
narrative_ontology:constraint_beneficiary(lng_price_volatility_transmission, trading_intermediaries).
narrative_ontology:constraint_beneficiary(lng_price_volatility_transmission, wealthy_importing_nations).
narrative_ontology:constraint_victim(lng_price_volatility_transmission, price_sensitive_consumers).
narrative_ontology:constraint_victim(lng_price_volatility_transmission, developing_importers).
narrative_ontology:constraint_victim(lng_price_volatility_transmission, power_grid_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-SENSITIVE CONSUMER (SNARE) — Trapped in volatile LNG price transmission with no exit. Households and small businesses in developing importers cannot switch fuels (grid infrastructure locked in), cannot renegotiate contracts (take-or-pay clauses), and bear full cost of price spikes. Maximum extraction with zero mitigation capacity.
constraint_indexing:constraint_classification(lng_price_volatility_transmission, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING IMPORTER NATION (TANGLED ROPE) — Constrained by energy security (LNG is essential for industrialization and heating) and limited foreign reserves, but also benefits from LNG access that alternatives (coal, local production) cannot provide. The constraint coordinates energy supply while extracting via price volatility transmission. Significant but not total extraction — some negotiating capacity through long-term contracts.
constraint_indexing:constraint_classification(lng_price_volatility_transmission, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LNG EXPORTER (ROPE) — Institutional actor with arbitrage capacity across markets. Experiences the constraint as pure coordination: liquefaction infrastructure, spot market access, and portfolio diversification enable benefit from volatility transmission. Net beneficiary. Extraction runs toward this agent; they have exit optionality (can redirect cargo, hedge positions).
constraint_indexing:constraint_classification(lng_price_volatility_transmission, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTHY IMPORTING NATION (TANGLED ROPE) — Constrained by grid infrastructure lock-in and long-term contracts, but powerful enough to negotiate hedging mechanisms, negotiate contract terms, and absorb price shocks through monetary policy. Coordinates energy supply (rope function) while experiencing some extraction via volatility (tangled element). Agency is substantial — can shape contract terms and storage mandates.
constraint_indexing:constraint_classification(lng_price_volatility_transmission, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OIL-INDEXED PRICING MECHANISM (PITON) — The historical indexation of LNG prices to crude oil has become largely performative. Modern LNG markets operate on hub-based (TTF, Henry Hub) and spot pricing, but long-term contracts retain oil-indexation clauses as ceremonial artifacts. The mechanism persists through inertia despite being empirically decoupled from actual trading. Theater ratio reflects the gap between formal contract language and real price discovery.
constraint_indexing:constraint_classification(lng_price_volatility_transmission, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, price volatility transmission in finite-supply commodity markets is structurally inevitable. Inelastic demand (energy is non-substitutable short-term), supply constraints (LNG requires months to pivot), and global market integration guarantee that localized shocks propagate globally. This appears as natural market law. However, structural data contradicts the mountain classification — the engine identifies false summit: policy choices (contract indexation, reserve mandates, market regulation) create the transmission mechanism; it is not immutable.
constraint_indexing:constraint_classification(lng_price_volatility_transmission, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (IEA, EU regulators, multilateral development banks) see volatility transmission as a temporary coordination failure with sunset potential. Strategic reserves, regional pricing hubs (AGATE in Asia), long-term contracting standards, and renewable transitions represent exit pathways. The constraint is real but carries internal mechanisms for phasing out. Sunset horizon: 15-25 years as decarbonization reduces LNG demand and storage infrastructure matures.
constraint_indexing:constraint_classification(lng_price_volatility_transmission, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lng_price_volatility_transmission_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lng_price_volatility_transmission, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lng_price_volatility_transmission, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lng_price_volatility_transmission, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lng_price_volatility_transmission, TR),
    TR >= 0.70.

:- end_tests(lng_price_volatility_transmission_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting genuine asymmetric extraction combined with legitimate supply-chain coordination. The constraint extracts because producers and intermediaries capture volatility rents while consumers bear price spikes; suppression is high because exit options are limited (grid lock-in, fuel non-substitutability short-term). However, extraction is not maximal because: (a) long-term contracting dampens some volatility, (b) financial hedging provides partial access for institutional actors, (c) renewable alternatives are maturing. Trajectory shows extractiveness rising from 0.35 (2010, early LNG market integration) to 0.58 (2025, high spot market volatility), indicating constraint has strengthened as markets liberalized. Suppression (0.62): Moderate-high. Consumers cannot exit instantly (grid infrastructure requires years to pivot, fuel switching faces technical barriers), but suppression is not total because: (a) some demand elasticity exists at extreme prices, (b) storage and hedging provide partial mitigation, (c) policy interventions (price caps, strategic reserves) reduce suppression. Theater ratio (0.51): Moderate, reflecting that oil-indexation formulas are largely performative. Long-term contracts formally index to Brent crude (80-90% of global LNG still under long-term contracts as of 2025), but spot prices and regional hubs (TTF, Henry Hub, AGATE) discover prices independently. Renegotiations and price reviews happen when indexed prices deviate too far from spot, indicating formulas are ceremonial constraints rather than binding ones.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between beneficiaries (Rope) and powerless victims (Snare). Exporters genuinely experience the constraint as solving the coordination problem of global gas distribution; the volatility that harms consumers is the mechanism that enables their arbitrage profit. This is not disagreement about facts but fundamentally different structural positions relative to the same causal mechanism. The wealthy importer nation (Tangled Rope) sits between them — enough power to benefit from coordination and negotiate terms, but enough exposure to feel extraction. The Piton perspective (oil-indexation formulas) reveals that the constraint carries ceremonial baggage: formal contractual mechanisms have decoupled from functional price discovery, indicating degradation pathway. The Mountain perspective (natural law) is a false summit — the engine will detect this as naturalization of contingent institutional arrangements (contract structures, market liberalization, capacity limits) as if they were immutable laws of physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) track each agent's structural position relative to extraction flow. Exporters with arbitrage exit experience low d (0.10-0.20) → negative effective extraction. Wealthy importers with powerful status and hedging access experience moderate d (0.45-0.55) → moderate chi. Developing importers with trapped consumers but some negotiating capacity experience high d (0.70-0.80) → high chi. Price-sensitive consumers with zero exit experience maximal d (0.95) → maximum chi. The synthetic d value for the overall constraint (0.72) reflects the weighted position of all agents: exporters benefit but are minority; most exposed agents are victims. Beneficiary/victim declarations anchor the directionality computation: beneficiaries include exporters and intermediaries (extraction flows toward them); victims include consumers and developing nations (extraction flows away from them).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY LINKAGE: LNG price volatility transmission is downstream of three upstream constraints: (1) LNG production capacity constraints (physical extraction limits), (2) Geopolitical chokepoint access (Strait of Malacca, Suez), and (3) Energy security dependence (grid lock-in). The volatility transmission constraint exists in the intersection of these three — without them, volatility could not propagate. MANDATROPHY RESOLUTION: The classification as Tangled Rope is robust because the constraint exhibits both coordination function (solving distance problem in energy distribution) AND asymmetric extraction (volatility rents accrue to producers/intermediaries, costs to consumers). The high suppression (0.62) is justified by grid infrastructure lock-in and fuel non-substitutability short-term. The theater ratio (0.51) is moderate because while some ceremonial activity exists (oil indexation), the constraint's primary mechanism (spot market discovery, regional price hubs) is functionally active. The constraint avoids misclassification as pure Snare because legitimate coordination benefits exist (access to global gas reduces regional scarcity) and some actors (wealthy importers) have real agency. It avoids pure Rope because asymmetric extraction is substantial — the constraint concentrates costs on powerless agents (consumers in developing nations) while concentrating benefits on powerful institutional actors (exporters).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    index_formula_decoupling,
    'Are crude oil indexation formulas in long-term LNG contracts actually determinative of prices, or merely ceremonial with spot market discovery doing the real work?',
    'Regression analysis: price movements explained by formula-implied prices vs hub prices; frequency of price renegotiations despite formula clauses',
    'If formulas determinative: extraction mechanism is partly supply-side coordination (higher ε → lower extraction). If ceremonial: extraction mechanism is pure information asymmetry (ε unchanged but suppression higher).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(index_formula_decoupling, empirical, 'Whether crude oil indexation is determinative or ceremonial').

omega_variable(
    demand_elasticity_threshold,
    'At what price level do LNG importers successfully shift to alternative fuels (LPG, coal, renewables)? Is there a structural floor below which demand becomes responsive?',
    'Historical fuel switching data by region; modeling of substitution elasticity at price levels 2x, 3x, 4x baseline',
    'If threshold < 2x baseline: suppression is lower (alternatives provide real exit). If threshold > 4x baseline: suppression is higher (trapped for extended duration). Directly affects snare classification credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_elasticity_threshold, empirical, 'Price threshold for demand substitution to alternatives').

omega_variable(
    financial_hedging_accessibility,
    'Do financial derivatives markets (LNG forwards, options, swaps) genuinely provide accessible price certainty for all importing actors, or only for institutional players with capital access?',
    'Survey of hedging costs, availability, and uptake by firm size and nation wealth; analysis of basis risk and counterparty exposure',
    'If accessible: suppression is lower (all actors can hedge). If restricted to institutional: suppression is bifurcated (powerful actors constrained, powerless trapped). Affects perspectival differentiation between wealthy and developing importers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_hedging_accessibility, empirical, 'Accessibility of financial hedging across actor classes').

omega_variable(
    storage_infrastructure_bottleneck,
    'Is the inability of developing importers to build regasification and storage infrastructure a constraint on exit options, or a consequence of capital markets and geopolitical chokepoints?',
    'Cost analysis of regasification terminals; comparison of infrastructure timelines in capital-rich vs capital-constrained regions; geopolitical barriers to terminal siting',
    'If primarily capital barrier: architecture is Rope-with-coordination-financing-problem (different constraint story). If primarily geopolitical/sovereign: volatility transmission is embedded in asymmetric infrastructure access (suppression rises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_infrastructure_bottleneck, empirical, 'Whether storage bottleneck is capital or geopolitical').

omega_variable(
    renewable_transition_timeline,
    'Will renewable energy and battery storage reduce LNG demand fast enough to create natural sunset (< 2030 in OECD, < 2040 globally), or will LNG lock-in extend beyond mid-century?',
    'IEA Net Zero Scenario pathways; cost curve projections for renewables + storage vs LNG; policy commitment tracking',
    'If sunset < 2030 OECD: scaffold perspective is credible near-term (high confidence in generational exit). If sunset > 2050: constraint persists across multiple human lifetimes; scaffold becomes aspirational; piton dynamics (ceremonial long-term contracts) become entrenched.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(renewable_transition_timeline, conceptual, 'Timeline for LNG demand collapse via renewable transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lng_price_volatility_transmission, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lng_vol_tr_t0, lng_price_volatility_transmission, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lng_vol_tr_t5, lng_price_volatility_transmission, theater_ratio, 5, 0.45).
narrative_ontology:measurement(lng_vol_tr_t10, lng_price_volatility_transmission, theater_ratio, 10, 0.51).
narrative_ontology:measurement(lng_vol_tr_t15, lng_price_volatility_transmission, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(lng_vol_be_t0, lng_price_volatility_transmission, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lng_vol_be_t5, lng_price_volatility_transmission, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lng_vol_be_t10, lng_price_volatility_transmission, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lng_vol_be_t15, lng_price_volatility_transmission, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lng_price_volatility_transmission, resource_allocation).
narrative_ontology:boltzmann_floor_override(lng_price_volatility_transmission, 0.18).
narrative_ontology:affects_constraint(lng_price_volatility_transmission, lng_production_capacity).
narrative_ontology:affects_constraint(lng_price_volatility_transmission, geopolitical_chokepoint_access).
narrative_ontology:affects_constraint(lng_price_volatility_transmission, energy_security_dependence).

% DUAL FORMULATION NOTE:
% LNG price volatility transmission is a downstream constraint in the energy security constraint family. Upstream constraints (production capacity, geopolitical access, grid lock-in) make volatility transmission possible; this constraint describes how volatility actually propagates through markets and to end consumers. The three constraints are linked: volatility transmission only causes extraction when upstream constraints force demand inelasticity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lng_price_volatility_transmission, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
