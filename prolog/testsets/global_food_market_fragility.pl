% ============================================================================
% CONSTRAINT STORY: global_food_market_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_food_market_fragility, []).

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
 *   constraint_id: global_food_market_fragility
 *   human_readable: Global Food Market Fragility to Correlated Crop Failures
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The global food market integrates staple crop production across
 *   geographically dispersed regions through trade, futures contracts, and
 *   logistics networks. This integration creates both efficiency gains and
 *   structural fragility. The constraint exhibits extraction asymmetry:
 *   commodity traders, large exporters, and agricultural input suppliers
 *   benefit from price volatility and market leverage, while subsistence
 *   farmers, urban poor in net-importing nations, and smaller agricultural
 *   exporters bear the costs of price spikes and supply shocks. The
 *   constraint manifests as a tangled rope from government and moderate-power
 *   perspectives — systems provide real coordination (efficient distribution,
 *   price discovery) alongside genuine extraction (volatility transfer to
 *   powerless agents, vulnerability to correlated failures). From the
 *   powerless perspective (subsistence farmer, urban poor), the constraint is
 *   a snare: dependent on global prices with no exit. The theater_ratio
 *   reflects that agricultural policy apparatus (farm subsidies, buffer
 *   stocks) maintains appearance of protecting domestic producers while
 *   actual protection degrades in face of global market forces.
 *   Extractiveness has increased from 0.32 to 0.58 over the 40-year interval,
 *   driven by financialization of commodity markets (futures proliferation,
 *   index speculation) and concentration of production in vulnerable regions
 *   (climate risk, geopolitical instability).
 *
 * KEY AGENTS:
 *   - Subsistence Farmers: Primary victims (powerless/trapped) — dependent on global commodity prices; lost agricultural autonomy; no exit options
 *   - Urban Poor in Net Food-Importing Nations: Primary victims (powerless/trapped) — spend 50-70% of income on staples; vulnerable to price spikes; politically destabilizing
 *   - Large Agricultural Exporters: Primary beneficiaries (institutional/arbitrage) — gain from market integration and pricing power; can shift production regions and crop mix
 *   - Commodity Traders: Secondary beneficiaries (powerful/arbitrage) — profit from price volatility; access to futures and financial hedging; maximum exit optionality
 *   - Agricultural Input Suppliers: Secondary beneficiaries (institutional/arbitrage) — sell seeds, fertilizer, equipment at prices linked to commodity prices
 *   - Net Food-Importing Governments: Constrained coordinating agents (organized/constrained) — require market-based food access but vulnerable to supply shocks; manage extraction through subsidies and strategic reserves
 *   - Exporting Nations (Medium-sized): Mixed experience (moderate/constrained) — benefit from export revenues but constrained by global price dynamics and larger producers' leverage
 *   - Agricultural Policy Apparatus: Degraded institutions (institutional/arbitrage) — maintain performative farm support while actual vulnerability increases; theater ratio high
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_food_market_fragility, 0.58).
domain_priors:suppression_score(global_food_market_fragility, 0.62).
domain_priors:theater_ratio(global_food_market_fragility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_food_market_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_food_market_fragility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_food_market_fragility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_food_market_fragility, tangled_rope).
narrative_ontology:human_readable(global_food_market_fragility, "Global Food Market Fragility to Correlated Crop Failures").
narrative_ontology:topic_domain(global_food_market_fragility, "economic/geopolitical").

domain_priors:requires_active_enforcement(global_food_market_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_food_market_fragility, commodity_traders).
narrative_ontology:constraint_beneficiary(global_food_market_fragility, large_agricultural_exporters).
narrative_ontology:constraint_beneficiary(global_food_market_fragility, agricultural_input_suppliers).
narrative_ontology:constraint_victim(global_food_market_fragility, net_food_importing_nations).
narrative_ontology:constraint_victim(global_food_market_fragility, subsistence_farmers).
narrative_ontology:constraint_victim(global_food_market_fragility, urban_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE FARMER (SNARE) — Cannot exit global market system; bound to commodity prices set in distant futures markets. Local agricultural autonomy has been systematically dismantled by trade policies and debt structures. Bears full extraction cost when prices spike. No alternatives exist within planning horizon.
constraint_indexing:constraint_classification(global_food_market_fragility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: URBAN POOR IN NET FOOD-IMPORTING NATIONS (SNARE) — Trapped in dependency on food imports; spend 50-70% of income on staples. No exit option when crop failures cascade. Cannot shift to domestic production. Cannot shift to substitute commodities. Subject to volatility in distant markets.
constraint_indexing:constraint_classification(global_food_market_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE AGRICULTURAL EXPORTER (ROPE) — Benefits from integrated global markets; has pricing power and market access. Experiences the constraint as coordination mechanism: standardized contracts, futures markets, and logistics networks enable profitable distribution. Arbitrage options: can shift production regions, diversify crops, hedge on futures markets. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMODITY TRADER (ROPE) — Extractive but coordinates price discovery across regions. Benefits from volatility. Has maximum arbitrage options: can shift between commodities, geographic regions, and futures vs spot markets. Experiences global fragility as a coordination system that creates profit opportunities through sophisticated risk management.
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SMALL-TO-MEDIUM EXPORTING NATION (TANGLED ROPE) — Partially benefits from export markets and foreign exchange revenues. But also constrained by global price dynamics set by larger producers. Experiences extraction through price volatility and market leverage of larger traders. Some exit options (crop diversification, regional trade agreements) but constrained by climate, land, and capital.
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NET FOOD-IMPORTING GOVERNMENT (TANGLED ROPE) — Experiences both coordination and extraction. Global markets enable efficient distribution and consumer choice. But also constrained by vulnerability to supply shocks, political pressure to maintain low food prices, and leverage exerted by exporters and speculators. Enforcement (subsidies, price controls, strategic reserves) required to manage extraction and coordinate domestic stability. Exit option (autarky) extremely costly.
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: AGRICULTURAL POLICY APPARATUS (PITON) — Traditional farm subsidies, buffer stocks, and strategic grain reserves persist through institutional inertia despite evidence of inefficiency. Theater ratio high: policy maintains appearance of protecting farmers and national food security while actual protection degrades as global market forces override domestic policies. Beneficiaries are wealthy agricultural producers and input suppliers, not subsistence farmers.
constraint_indexing:constraint_classification(global_food_market_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a global civilizational view, some concentration in agricultural production is inherent to comparative advantage and specialization. Climate differences and population density create structural asymmetry in food production capacity. However, the current degree of fragility is contingent on institutional arrangements (concentration in commodity production, financialization of futures markets, trade policy), not on physical limits. Engine false summit detection will flag this as naturalization.
constraint_indexing:constraint_classification(global_food_market_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_food_market_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_food_market_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_food_market_fragility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_food_market_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_food_market_fragility, TR),
    TR >= 0.70.

:- end_tests(global_food_market_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The global food market extracts from trapped agents (subsistence farmers, urban poor) through price volatility and dependency. Extraction is not maximal (would require active coercion beyond market mechanisms) but structural — trapped agents have no alternatives. The metric reflects the asymmetry: traders and exporters capture gains from volatility; victims absorb losses. Suppression (0.62): Moderate-high. Barriers to exit include: land tenure insecurity preventing crop diversification; credit systems denominated in global commodity prices; lack of storage and logistics infrastructure for regional markets; policy barriers to autarky (WTO rules, trade agreements); agronomic constraints (climate, soil fertility tied to commodity monoculture). Theater ratio (0.48): Moderate. Agricultural policy maintains appearance of protecting farmers (subsidies, price controls, strategic reserves) while actual protection is eroded by global market forces and financialization. Policy machinery is real but increasingly performative relative to actual farmer outcomes. The metric reflects that policy still partially functions for some actors (large producers benefit from subsidies) but has degraded relative to stated goals (food security, farmer stability).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Large exporters and commodity traders see Rope — an efficient coordination system that enables profitable distribution and price discovery. Their arbitrage options and market power mean they experience integration as coordination. Net-importing governments see Tangled Rope — real coordination benefits (efficient distribution, consumer choice) alongside extraction vulnerability (dependency on external suppliers, exposure to price spikes). Subsistence farmers and urban poor see Snare — trapped in dependency with no escape. The agricultural policy apparatus sees Piton — traditional farm support persists through institutional momentum despite declining effectiveness relative to global market forces. The analytical observer at civilizational scale sees Mountain (specialized production inherent to climate differences) but structural data reveals this as a false summit: current degree of fragility is contingent on institutional arrangements (concentration, financialization, policy design), not on physical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to extraction flow. Commodity traders and large exporters are beneficiaries with arbitrage options — low d, negative or zero χ. Subsistence farmers are victims with trapped exit — high d (~0.95), high f(d) ~ 1.42. Urban poor have trapped exit and victim status — high d. Net-importing governments have constrained exit (autarky very costly) and mixed beneficiary/victim status (coordinating role but vulnerable) — moderate d (~0.50-0.65). The pipeline applies f(d) sigmoid to compute experienced extractiveness per perspective, scaled by scope (global scope σ=1.2 amplifies χ). Beneficiaries experience low effective extraction despite high base ε; victims experience high effective extraction. The suppression value (0.62) is unscaled — structural property of the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification depends on structural position. The claim 'global food markets are pure extraction' (Snare from all perspectives) is false — they coordinate price discovery and distribution (Rope from trader/exporter perspective). The claim 'global food markets are pure coordination' (Rope from all perspectives) is also false — they extract from trapped agents (Snare from subsistence farmer perspective). The tangled_rope classification captures the hybrid: genuine coordination functions alongside asymmetric extraction. The false summit mountain perspective exposes naturalization of contingent fragility. The piton classification reveals policy apparatus degradation. No single type is correct; the presheaf over perspectives IS the answer. The constraint's extractiveness has increased from 0.32 to 0.58 over 40 years, driven by financialization (futures proliferation, index speculation, leverage effects) and production concentration (climate vulnerability, geopolitical risk). This trajectory suggests extraction layered onto coordination — the coordination function persists but increasingly serves as infrastructure for extraction by sophisticated actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    financialization_versus_physical,
    'Is market fragility driven primarily by financialization of commodity futures or by underlying physical constraints in global crop production?',
    'Time-series correlation between price volatility and physical production shocks; comparison of price dynamics before/after financialization (1990s onwards); analysis of basis risk and contango effects in specific crop markets',
    'If primarily financialization: fragility is remediable through derivatives market regulation and price stabilization mechanisms. If primarily physical: solutions require agricultural adaptation, geographical diversification, and storage infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financialization_versus_physical, empirical, 'Whether crop price fragility is driven by financialization or physical constraints').

omega_variable(
    substitution_velocity,
    'How quickly can agricultural production pivot between crops in response to prices or climate shocks?',
    'Historical analysis of crop substitution lags; agronomic modeling of planting-decision timescales; comparison of land-use flexibility across crop types and regions',
    'If fast (< 1 season): markets adjust before price spikes cascade. If slow (> 2 years): lag creates extraction window where downstream users are trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_velocity, empirical, 'Agricultural substitution velocity between crops').

omega_variable(
    strategic_reserve_effectiveness,
    'Can national strategic reserves and buffer stocks meaningfully stabilize prices during correlated crop failures?',
    'Historical case studies (US CCC, India''s NFSA, Egypt''s subsidy system) measuring reserve adequacy and release timing; modeling of reserve depletion under simultaneous regional failures',
    'If effective: government-level coordination mechanisms can reduce extraction. If ineffective: victim populations face structural vulnerability regardless of policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_reserve_effectiveness, empirical, 'Effectiveness of strategic reserves in price stabilization').

omega_variable(
    crop_diversification_feasibility,
    'Can agricultural regions meaningfully diversify away from commodity monocultures given economic and agronomic constraints?',
    'Analysis of crop rotation viability, soil degradation from monoculture, price premium for diversified crops, land tenure security effects on diversification behavior',
    'If feasible: fragility is remediable through decentralized farmer decision-making and extension programs. If constrained: fragility locked in by economic structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crop_diversification_feasibility, empirical, 'Feasibility of crop diversification away from commodity monocultures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_food_market_fragility, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gfmf_tr_t0, global_food_market_fragility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gfmf_tr_t20, global_food_market_fragility, theater_ratio, 20, 0.42).
narrative_ontology:measurement(gfmf_tr_t40, global_food_market_fragility, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(gfmf_be_t0, global_food_market_fragility, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gfmf_be_t20, global_food_market_fragility, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(gfmf_be_t40, global_food_market_fragility, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_food_market_fragility, resource_allocation).
narrative_ontology:affects_constraint(global_food_market_fragility, agricultural_commodity_financialization).
narrative_ontology:affects_constraint(global_food_market_fragility, land_use_monoculture_lock).
narrative_ontology:affects_constraint(global_food_market_fragility, strategic_food_reserves_adequacy).

% DUAL FORMULATION NOTE:
% Global food market fragility decomposes into three structurally distinct constraints: (1) commodity financialization (ε≈0.68, Snare from consumer perspective) — price volatility driven by derivatives speculation; (2) land-use monoculture lock (ε≈0.45, Tangled Rope) — production concentrated in climate-vulnerable regions; (3) strategic reserve adequacy (ε≈0.52, Tangled Rope) — buffers insufficient during correlated failures. Each has distinct resolution mechanisms and institutional actors. This story addresses the integrated system constraint; the three components are separate constraint stories linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_food_market_fragility, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
