% ============================================================================
% CONSTRAINT STORY: global_food_market_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: economic/geopolitical/agricultural
 *
 * SUMMARY:
 *   The global food market represents a complex system for distributing
 *   staple crops (wheat, rice, corn, soy) across 8 billion people with
 *   radically unequal production capacity and consumption patterns. The
 *   system generates coordination benefits through comparative advantage,
 *   economies of scale, and geographic risk diversification — but these
 *   benefits are asymmetrically distributed. Three factors create fragility:
 *   (1) geographic concentration of production in a small number of nations
 *   (Russia, Ukraine, USA, India, Argentina control >60% of global grain
 *   trade); (2) climate correlation across major producing regions driving
 *   synchronized harvest failures; (3) financialization and speculation
 *   amplifying price volatility far beyond underlying supply-demand
 *   fundamentals. The constraint exhibits all six DR types from different
 *   perspectives. Subsistence farmers and urban poor see pure extraction
 *   (Snare): they are trapped by dependence on global markets they did not
 *   choose, with no exit options and extreme vulnerability to price spikes.
 *   Food exporters and commodity traders see coordination (Rope): the global
 *   market is a mechanism for their profit and influence. Net food-importing
 *   nations see mixed coordination and extraction (Tangled Rope): they
 *   benefit from access to diverse food sources but suffer asymmetric
 *   extraction during supply disruptions. International governance bodies see
 *   a degraded mandate (Piton): food security coordination has atrophied
 *   while financialization dominates. Alternative supply chain movements see
 *   a temporary problem with a path forward (Scaffold): localization and
 *   resilience offer a sunset to global dependency. The analytical observer
 *   risks naturalizing institutional fragility as inherent to agriculture
 *   (false Mountain). The constraint's extractiveness has increased from 0.35
 *   to 0.58 over 30 years, driven by concentration, financialization, and
 *   climate volatility. Theater ratio remains relatively low (0.48) because
 *   the market's extraction mechanisms operate through price signals and
 *   supply disruptions rather than performative ritual — the coordination
 *   function is real but increasingly overshadowed by extractive dynamics.
 *
 * KEY AGENTS:
 *   - Subsistence Farmers in Climate-Vulnerable Regions: Primary victims (powerless/trapped) — depend entirely on global market access; no diversification options; existential exposure to supply shocks
 *   - Urban Poor in Net Food-Importing Nations: Primary victims (powerless/trapped) — food security wholly dependent on imported staples; 40-70% of income spent on food; no ability to compensate for price spikes
 *   - Net Food-Importing Nations: Secondary victims (moderate/constrained) — experience extraction during supply disruptions; can implement domestic policies (reserves, subsidies, diversification) but at significant fiscal cost
 *   - Agricultural Commodity Traders: Primary beneficiaries (institutional/arbitrage) — profit from price volatility, spatial arbitrage, and speculation; high exit optionality
 *   - Food Export Nations: Primary beneficiaries (institutional/arbitrage) — capture economic rent from export revenue; gain political leverage through food dependency; high exit optionality
 *   - International Food Security Organizations: Organized responders (organized/constrained) — coordinate early warning systems, reserves, emergency aid; constrained by mandate and capacity limits
 *   - Agricultural Subsidies and Protectionist Policies: Institutional actors (institutional/arbitrage) — nominally stabilizing but functionally degraded; maintained through political inertia
 *   - Alternative Supply Chain Movements: Organized agents (organized/constrained) — building localized and regional alternatives; see clear sunset path through diversification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional fragility as natural agricultural constraint
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
narrative_ontology:topic_domain(global_food_market_fragility, "economic/geopolitical/agricultural").

domain_priors:requires_active_enforcement(global_food_market_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_food_market_fragility, agricultural_commodity_traders).
narrative_ontology:constraint_beneficiary(global_food_market_fragility, food_export_nations).
narrative_ontology:constraint_beneficiary(global_food_market_fragility, grain_storage_monopolies).
narrative_ontology:constraint_victim(global_food_market_fragility, net_food_importing_nations).
narrative_ontology:constraint_victim(global_food_market_fragility, subsistence_farmers).
narrative_ontology:constraint_victim(global_food_market_fragility, urban_poor_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE FARMERS (SNARE) — Depend entirely on global market access for seed inputs, fertilizer, and income stability. Climate volatility, price spikes, and supply chain disruptions are existential threats. No exit options: cannot switch crops without market access, cannot stockpile reserves, cannot hedge against price volatility. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.82 (effective extraction very high).
constraint_indexing:constraint_classification(global_food_market_fragility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: URBAN POOR IN NET FOOD-IMPORTING NATIONS (SNARE) — Food costs consume 40-70% of household income. Global market disruptions translate directly to malnutrition, food insecurity, and mortality. Trapped by dependence on imported staples; no domestic production alternatives; no ability to migrate or diversify income. d≈0.95, f(d)≈1.42, σ=1.1 → χ≈0.86 (effective extraction extreme).
constraint_indexing:constraint_classification(global_food_market_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: NET FOOD-IMPORTING NATIONS (TANGLED ROPE) — Experience the global market as a mixed constraint: coordination benefit from international division of labor and comparative advantage, BUT asymmetric extraction when supply disruptions occur (price spikes, export bans). Can implement strategic grain reserves, diversify suppliers, or subsidize domestic production, but at significant fiscal cost. Constrained exit: switching to autarky is economically destructive. d≈0.65, f(d)≈0.95, σ=1.1 → χ≈0.60 (moderate effective extraction).
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AGRICULTURAL COMMODITY TRADERS (ROPE) — Benefit from market volatility through price speculation and arbitrage opportunities. Experience global fragmentation as coordination mechanism: price discovery, risk hedging (futures markets), and geographic redistribution of surplus. High exit optionality: can shift capital to other commodities or financial instruments. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06 (net beneficiary; negative effective extraction).
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOOD EXPORT NATIONS (ROPE) — Primary beneficiaries of global market integration. Capture economic rent from export revenue, political influence through food leverage, and stable demand. Experience market fragility as coordination advantage: their capacity becomes strategic asset; pricing power increases during supply disruptions. Exit optionality: can redirect exports to preferred partners or implement export restrictions. d≈0.10, f(d)≈-0.09, σ=1.2 → χ≈-0.06 (net beneficiary).
constraint_indexing:constraint_classification(global_food_market_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INT'L GOVERNANCE (FAO, WFP, CGIAR) (TANGLED ROPE) — Organized actors tasked with coordination (early warning systems, reserves management, price monitoring) and extraction mitigation (emergency aid, disaster relief). Constrained exit: mandated by charter; cannot abandon food-insecure regions. Experience dual pressure: coordination function requires maintaining market openness; mitigation function requires compensating for market failures. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.38 (moderate extraction due to capacity limits).
constraint_indexing:constraint_classification(global_food_market_fragility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: AGRICULTURAL SUBSIDIES & PROTECTIONISM (PITON) — Nominally designed to stabilize farmers and protect food security. Functionally degraded: subsidies benefit large-scale industrial producers (96% of EU subsidies to 20% of farms); protectionism blocks market clearing and perpetuates inefficiency. Theater ratio=0.72: appearance of stabilization vs reality of rent-seeking. Maintained through institutional inertia and political pressure from agricultural lobbies despite evidence of minimal food security impact. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(global_food_market_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ALTERNATIVE SUPPLY CHAINS (SCAFFOLD) — Organized agents (local food networks, regional trade agreements, vertical integration) see the global market as a temporary problem with a sunset. Decentralized supply chains, precision agriculture, vertical farming, and regional sourcing partnerships are building independent verification pathways around global fragility. Low extraction: coalition has agency and sees a clear exit path toward diversified, resilient systems. d≈0.35, f(d)≈0.35, σ=0.9 → χ≈0.18 (low effective extraction due to agency and local scope).
constraint_indexing:constraint_classification(global_food_market_fragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL SYSTEM VIEW (MOUNTAIN) — From a civilizational perspective, food market fragility might appear as inherent to agricultural systems: climate volatility, storage decay, perishability, and geographic heterogeneity of growing conditions are natural constraints. However, the structural data (ε=0.58, suppression=0.62, beneficiaries/victims clearly asymmetric) contradicts the mountain classification. The engine will flag this as a false summit: the fragility is amplified by institutional arrangements (concentrated production, financialized markets, just-in-time logistics) not by natural law alone.
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
    constraint_indexing:constraint_classification(global_food_market_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate. The global food market extracts value from vulnerable populations (subsistence farmers, urban poor in importing nations) through multiple channels: price volatility (financialized speculation), concentrated production (geographic monopoly), supply disruptions (climate shocks, export bans), and asymmetric information (traders and exporters know more than consumers). The extraction is not total (0.80+) because market mechanisms also provide genuine coordination benefits (efficient production, risk diversification) and some populations have partial exit options (food-exporting nations, commodity traders). The historical increase from 0.35→0.58 reflects intensifying concentration, financialization, and climate volatility. Suppression (0.62): High-moderate. Multiple barriers prevent exit or mitigation: subsistence farmers cannot switch to alternative livelihoods without market access; importing nations face trade retaliation if they implement autarky policies; urban poor have no bargaining power in commodity markets. Strategic grain reserves, diversified sourcing, and price controls are technically available but politically and fiscally expensive. Climate volatility creates structural suppression (cannot predict or control weather). But suppression is not total (0.80+) because some organized actors (export nations, traders, governance bodies) retain agency and can implement countermeasures. Theater ratio (0.48): Low-moderate. The extraction operates primarily through material scarcity (actual crop failures) and price signals rather than performative ritual. Agricultural subsidies show higher theater (appearance of stabilization, reality of rent-seeking), but market functioning itself is relatively material. The low theater indicates this is a genuine coordination problem with extraction components, not an inertial ritual (Piton).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Subsistence farmers and urban poor see a Snare: they are trapped with no exit, bearing full extraction risk. Net food-importing nations see a Tangled Rope: genuine coordination benefits (diverse suppliers, economies of scale) mixed with asymmetric extraction (vulnerable to supply shocks). Export nations and commodity traders see a Rope: the global market is a mechanism for profit and influence with high optionality. Agricultural subsidies appear as a Piton (degraded ritual maintaining inertia). Alternative supply chains see a Scaffold (temporary problem with a sunset). The analytical observer risks a false Mountain (naturalizing institutional fragility). The perspectival gap reflects radically asymmetric structural positions: some agents control the system's design and can exit; others are trapped within it. The gap narrows only if constraints on exit (capital availability, climate adaptation, political risk) are explicitly addressed — otherwise, the powering atom differences are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Subsistence farmers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction: no exit, no bargaining power, existential vulnerability. Urban poor in importing nations: Victim + trapped → d≈0.95, f(d)≈1.42. Slightly higher d than farmers due to complete dependence on imported staples with zero domestic alternatives. Importing nations: Victim + constrained → d≈0.65, f(d)≈0.95. Moderate extraction: can implement countermeasures but at significant cost; cannot exit without economic damage. Commodity traders: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary: profit from volatility, high exit optionality, control information asymmetries. Export nations: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Net beneficiary: capture economic rent, political leverage, can redirect exports. International governance: Mixed + constrained → d≈0.50, f(d)≈0.65. Moderate: tasked with coordination but capacity-limited; cannot abandon mandate. Alternative supply chains: Organized + constrained → d≈0.35, f(d)≈0.35. Low extraction: have agency, see exit path, local scope reduces dependence on global system.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The global food market fragility decomposes into multiple structurally distinct claims with different ε values: (1) Coordination constraint (pure Rope): Geographic diversification and comparative advantage require interconnected markets. ε≈0.08, no victims, beneficiaries are all participants accessing diverse production. (2) Climate correlation constraint (natural/quasi-Mountain): Synchronized regional droughts create genuine scarcity independent of institutions. ε≈0.20, structural but not extractive. (3) Financialization and speculation (institutional Snare): Commodity futures markets amplify price volatility beyond physical scarcity through margin cascades and algorithmic feedback. ε≈0.65, clear victims (price-sensitive consumers), clear beneficiaries (traders and financial institutions). (4) Concentration constraint (institutional Tangled Rope): Production concentrated in few nations; export restrictions during crises create asymmetric extraction. ε≈0.50, mixed coordination and extraction. The JSON presents an integrated analysis (ε=0.58 as aggregate) because the constraint operates as a tangled system where separating components is analytically clean but practically artificial — market fragility emerges from their interaction. The mandatrophy is resolved by acknowledging that all six perspectives are legitimate readings of this integrated system. The system exhibits real coordination value (rope aspects), real extraction (snare aspects through financialization and concentration), temporary institutional solutions (scaffold aspects through governance), degraded policy (piton aspects through subsidies), and genuine asymmetries (tangled rope aspects). No single type captures it; the presheaf structure over perspectives does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climate_correlation_threshold,
    'At what degree of concurrent climate impact across major producing regions does the market transition from fragile to catastrophically unstable?',
    'Historical analysis of synchronized droughts/extreme weather (2010-2011 Russian drought + US heat wave correlation, 2023 Indian monsoon failure timing); climate model projections of future simultaneous failures',
    'If threshold is low (2-3 regions): current infrastructure assumes stability we cannot maintain. Classification shifts to higher ε→higher χ for all perspectives. If threshold is high (5+ regions): current market structure is more robust than assessed; ε decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_correlation_threshold, empirical, 'Threshold for market transition to catastrophic instability').

omega_variable(
    reserve_adequacy_paradox,
    'Are strategic grain reserves a coordination mechanism (stabilizing price spikes) or an extraction apparatus (enabling hoarding and price manipulation)?',
    'Comparative analysis of reserve release patterns during price spikes; detection of coordinated withholding behavior; price impact analysis (do reserves actually dampen volatility or amplify it through asymmetric information?)',
    'If reserves coordinate: market sees lower suppression (≈0.45), classification softens to Rope from more perspectives. If reserves enable extraction: suppression increases (≈0.70), classification hardens to Snare from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_adequacy_paradox, empirical, 'Whether reserves stabilize or enable extraction').

omega_variable(
    localization_feasibility_at_scale,
    'Can localized and regional food systems actually support current global population levels, or are they aspirational Scaffold structures that would degrade catastrophically if implemented?',
    'Carrying capacity analysis by region; caloric yield data for local production systems vs global supply requirements; transition modeling (how many years to establish independent regional systems?)',
    'If feasible: scaffold sunset is real and well-reasoned; classification supports transition path to distributed systems. If infeasible: scaffold is theater; populations remain trapped in global dependency. Ε stays high for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(localization_feasibility_at_scale, empirical, 'Whether localization can scale to global population').

omega_variable(
    financialization_extraction_mechanism,
    'Is commodity price volatility in global futures markets a feature of fundamental supply-demand mismatch (Mountain/Rope view) or an artifact of financialization and speculation (Snare/Tangled Rope view)?',
    'Decompose price movements into physical scarcity components (actual crop failure, transport disruption) vs financial leverage components (position unwinding, margin calls, algorithmic trading feedback loops)',
    'If primarily physical: market fragility is inherent; suppression is natural constraint (ε, suppression stay high). If primarily financial: suppression is institutional choice (ban or regulate speculation); ε stays high but causation shifts from natural scarcity to engineered scarcity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financialization_extraction_mechanism, empirical, 'Whether volatility stems from physical scarcity or financialization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_food_market_fragility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gfmf_tr_t0, global_food_market_fragility, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gfmf_tr_t15, global_food_market_fragility, theater_ratio, 15, 0.43).
narrative_ontology:measurement(gfmf_tr_t30, global_food_market_fragility, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(gfmf_be_t0, global_food_market_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gfmf_be_t15, global_food_market_fragility, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(gfmf_be_t30, global_food_market_fragility, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_food_market_fragility, resource_allocation).
narrative_ontology:affects_constraint(global_food_market_fragility, agricultural_commodity_concentration).
narrative_ontology:affects_constraint(global_food_market_fragility, climate_driven_yield_volatility).
narrative_ontology:affects_constraint(global_food_market_fragility, agricultural_subsidy_redistribution).

% DUAL FORMULATION NOTE:
% Global food market fragility decomposes into four structurally distinct constraints: (1) Coordination requirement (Rope, ε≈0.08) — geographic diversification and comparative advantage inherently require interconnected markets; (2) Climate correlation (quasi-Mountain, ε≈0.20) — synchronized regional droughts create genuine scarcity; (3) Financialization (Snare, ε≈0.65) — commodity futures speculation amplifies volatility beyond physical scarcity; (4) Production concentration (Tangled Rope, ε≈0.50) — geographic concentration enables export restrictions and geopolitical leverage. The JSON presents ε=0.58 as the integrated constraint where these components interact. Upstream constraints (climate volatility, production concentration) create structural vulnerability; downstream constraint (financialization) amplifies extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_food_market_fragility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
