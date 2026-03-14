% ============================================================================
% CONSTRAINT STORY: agricultural_trade_protectionism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agricultural_trade_protectionism, []).

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
 *   constraint_id: agricultural_trade_protectionism
 *   human_readable: Agricultural Trade Protectionism
 *   domain: economic_policy/international_trade
 *
 * SUMMARY:
 *   Agricultural trade protectionism represents one of the longest-standing
 *   contradictions in the global trade system: every WTO round has included
 *   agricultural liberalization as a stated objective, yet protection has
 *   persisted and deepened. The constraint exhibits classic tangled-rope
 *   structure: it solves a genuine coordination problem (stabilizing rural
 *   incomes and food supply security) while simultaneously extracting from
 *   developing-world exporters and domestic consumers through price elevation
 *   and market exclusion. The theater_ratio (0.64) reflects the growing gap
 *   between the official justification (rural preservation, food security,
 *   environmental stewardship) and the actual distribution mechanism
 *   (production-linked subsidies flowing to large-scale producers and
 *   agribusiness). Base extractiveness has increased from 0.42 to 0.58 over
 *   the 20-year interval as the costs to excluded parties have compounded
 *   while protectionist rents have concentrated in agribusiness
 *   consolidation. The constraint demonstrates how a mixed
 *   coordination-extraction hybrid can persist despite acknowledged
 *   inefficiency — the beneficiaries (domestic producers, agricultural input
 *   suppliers, rural political constituencies) have sufficient political
 *   power to block liberalization, while victims (developing-world exporters,
 *   domestic consumers, global efficiency) lack either organizational
 *   capacity or political salience to force change.
 *
 * KEY AGENTS:
 *   - Domestic Agricultural Producers: Primary beneficiary (institutional/arbitrage) — capture protectionist rents through tariffs, quotas, and subsidies; can arbitrage domestic premium prices against global commodity prices
 *   - Developing-World Agricultural Exporters: Primary victim (powerless/trapped) — blocked from major markets; no viable exit; face structural price ceiling from subsidized competition; comprise the global poor with no political leverage
 *   - Domestic Food Consumers: Secondary victim (moderate/constrained) — bear extraction through elevated food prices and reduced diversity; but receive genuine benefits (price stability, supply security, rural community preservation); face mobility barriers (food is non-discretionary, tariff system is opaque)
 *   - Agricultural Input Suppliers: Beneficiary (institutional/arbitrage) — profit from high protected prices driving input demand; benefit from subsidy capitalization into land values
 *   - Rural Political Constituencies: Beneficiary (organized/constrained) — receive income support and community preservation benefits; organized through agricultural interest groups; face demographic decline reducing their political power
 *   - Trading Blocs / WTO Members: Organized hybrid (organized/constrained) — experience both coordination benefits (rules-based market access) and extraction (carve-outs, bilateral exceptions); constrained by multilateral framework; active enforcement required
 *   - Trade Liberalization Coalition: Organized agent (organized/mobile) — development organizations, free-trade advocates, multinational firms; see protectionism as temporary; pursuing sunset through alternative trade architectures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political equilibrium as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agricultural_trade_protectionism, 0.58).
domain_priors:suppression_score(agricultural_trade_protectionism, 0.52).
domain_priors:theater_ratio(agricultural_trade_protectionism, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agricultural_trade_protectionism, extractiveness, 0.58).
narrative_ontology:constraint_metric(agricultural_trade_protectionism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(agricultural_trade_protectionism, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agricultural_trade_protectionism, tangled_rope).
narrative_ontology:human_readable(agricultural_trade_protectionism, "Agricultural Trade Protectionism").
narrative_ontology:topic_domain(agricultural_trade_protectionism, "economic_policy/international_trade").

domain_priors:requires_active_enforcement(agricultural_trade_protectionism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agricultural_trade_protectionism, domestic_agricultural_producers).
narrative_ontology:constraint_beneficiary(agricultural_trade_protectionism, agricultural_input_suppliers).
narrative_ontology:constraint_beneficiary(agricultural_trade_protectionism, rural_political_constituencies).
narrative_ontology:constraint_victim(agricultural_trade_protectionism, developing_world_agricultural_exporters).
narrative_ontology:constraint_victim(agricultural_trade_protectionism, domestic_food_consumers).
narrative_ontology:constraint_victim(agricultural_trade_protectionism, global_efficiency_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING WORLD AGRICULTURAL EXPORTER (SNARE) — Structurally blocked from major markets by tariffs, quotas, and subsidies. Trapped in commodity price competition with heavily subsidized producers. No viable exit: shifting to non-agricultural export requires capital and infrastructure absent in their context. Maximum experienced extraction.
constraint_indexing:constraint_classification(agricultural_trade_protectionism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC FOOD CONSUMER (TANGLED ROPE) — Bears extraction through artificially elevated food prices and reduced market diversity. But also receives genuine coordination benefits: price stability, domestic supply security, rural community preservation. Constrained exit — can access some imports at tariff-inflated prices, but cannot escape the protectionist system. Mixed experience of coordination and cost.
constraint_indexing:constraint_classification(agricultural_trade_protectionism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC AGRICULTURAL PRODUCER (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: tariffs and quotas enable collective action against global commodity price pressure. Can arbitrage between protected domestic market and global market (selling premium-priced domestic production). Net beneficiary with exit options.
constraint_indexing:constraint_classification(agricultural_trade_protectionism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL TRADING BLOC (TANGLED ROPE) — Organized actors (WTO members, trading blocs, multinational agricultural firms) experience protectionism as coordination mechanism (market access guarantees, rules-based trade) but also face extraction through carve-outs and exclusions. Constrained: can renegotiate terms but cannot exit multilateral framework without cost. Active enforcement required.
constraint_indexing:constraint_classification(agricultural_trade_protectionism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE AGRICULTURAL SUBSIDY REGIME (PITON) — The Common Agricultural Policy (EU), Farm Bill (US), and bilateral protectionist agreements persist through political inertia despite acknowledged inefficiency. Theater ratio high: public justification is rural preservation and food security, but actual function is income redistribution to large-scale producers and agribusiness. Performative rather than functional. Regime has low agency and sees itself as locked-in (requires political coalition to change).
constraint_indexing:constraint_classification(agricultural_trade_protectionism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADE LIBERALIZATION COALITION (SCAFFOLD) — Organized agents (development organizations, free-trade advocates, multinational firms with supply-chain interests) see protectionism as a temporary coordination failure with a structural sunset. Agricultural liberalization has been a stated objective of every GATT/WTO round since 1986. Sunset mechanism: as developing-world trade power grows and domestic agricultural constituencies shrink (demographic/economic transition), political will for protection erodes. Mobile: can pursue alternative trade architectures (bilateral deals, regional blocs).
constraint_indexing:constraint_classification(agricultural_trade_protectionism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, agricultural protectionism appears as an immutable constraint: the political power of rural constituencies in democratic systems makes agricultural protection universally observed; the Heckscher-Ohlin factor endowments create structural pressure for protection in labor-abundant countries. This perspective risks naturalizing what is actually a contingent political-economic equilibrium. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(agricultural_trade_protectionism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agricultural_trade_protectionism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agricultural_trade_protectionism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agricultural_trade_protectionism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agricultural_trade_protectionism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agricultural_trade_protectionism, TR),
    TR >= 0.70.

:- end_tests(agricultural_trade_protectionism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. Agricultural protectionism combines genuine coordination benefits (stabilizing volatile commodity markets, ensuring domestic food security, preserving rural livelihoods) with significant extraction from developing-world exporters and domestic consumers. The coordination function is real — without some price stabilization mechanism, commodity price volatility creates severe rural hardship in both developed and developing countries. But the extraction is also real and increasing: subsidies to large-scale producers in wealthy countries now exceed $700 billion annually (OECD PSE), while developing-world farmers lose estimated $24 billion annually in market access. The ratio reflects that this is neither pure coordination (which would have ε ≤ 0.45) nor pure extraction (ε ≥ 0.66). Suppression (0.52): Moderate-high. Barriers to exit are substantial for developing-world exporters (tariff walls, quota systems, non-tariff barriers like food safety standards); for consumers (tariff opacity, non-discretionary nature of food, political difficulty of reducing subsidies); but not insurmountable for organized actors (trade renegotiation is possible, alternative supply chains exist, technology can offset tariff costs). Theater ratio (0.64): High and rising. Official justification has shifted from simple income support to multi-valued narratives: rural preservation, food security, environmental stewardship, GMO safety, climate adaptation. But actual mechanism is production-linked subsidy capitalization — benefits concentrate in land values and agribusiness consolidation, not rural income stabilization. The gap between stated and actual function has widened as farm consolidation has advanced.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives is structured by exit options and power asymmetry. The beneficiary (domestic producer, institutional/arbitrage) experiences rope because they have genuine exit: they could shift to crop diversification, organic premium markets, or non-agricultural enterprise. Their participation in the protectionist system is a choice (profitable), not an imposition. The victim (developing-world exporter, powerless/trapped) experiences snare because exit is structurally unavailable: capital requirements for export crop diversification are prohibitive; alternative export sectors are equally closed or require different comparative advantages; food production is what their labor, land, and climate afford. The analytical observer risks misclassifying the power asymmetry as immutable (mountain) when it is actually contingent on specific global trade architecture, transport costs, and technology adoption rates. The scaffold perspective reveals that the constraint does have a structural sunset: as developing nations grow and rural constituencies shrink in rich countries, political support for protection erodes. The piton perspective reveals that the constraint is already partially inert — much protectionist theater persists despite acknowledged deadweight loss, maintained by institutional momentum rather than active function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: whether they benefit or bear costs, and what exit options they control. Domestic producers with arbitrage options experience low d (full beneficiary) — estimated d ≈ 0.10. Developing-world exporters with no exit experience high d (full target) — d ≈ 0.95. Food consumers with constrained but non-zero exit (some import access, price search) experience moderate-high d ≈ 0.70. Organized trading blocs with renegotiation capacity experience moderate d ≈ 0.50. The beneficiary's low d feeds into the f(d) sigmoid, producing moderate or negative effective extractiveness from their perspective (rope classification). The victim's high d feeds into the sigmoid, producing high effective extractiveness from their perspective (snare classification). The formula χ = ε × f(d) × σ(S) scales the base extractiveness (0.58) by the directionality function and scope modifier. At local scope (agricultural protectionism operates primarily nationally, σ=0.8), effective extractiveness is dampened for some perspectives; at global scope (σ=1.2), extractiveness amplifies for views of the global efficiency loss.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint is genuinely tangled rope (not mislabeled as coordination or extraction), and the mandatrophy is fully absorbed. The coordination function is real: agricultural protectionism does stabilize commodity prices, does preserve rural communities, does ensure food security — these are legitimate public goods. The extraction is also real: tariffs and subsidies transfer wealth from developing-world exporters and domestic consumers to domestic producers and agribusiness — this is measurable and asymmetric. The constraint cannot be decomposed into pure coordination (rope) and pure extraction (snare) as separate stories because they are structurally entangled: the price stabilization mechanism IS the extraction mechanism. Removing tariffs would eliminate extraction but would also eliminate price stability and community preservation — the two are coupled. The only way to separate them is through alternative mechanisms (crop insurance, direct income support, farmer transition assistance), but these do not exist at scale. Therefore, the tangled_rope classification is correct, and the mandatrophy is resolved by acknowledging that some constraints genuinely combine legitimate coordination with asymmetric extraction, and the political economy question is not 'which type is it?' but 'is the coordination benefit worth the extraction cost to losers, and do the beneficiaries have just title to the extracted value?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_measurement_boundary,
    'What constitutes ''subsidy'' vs ''legitimate public investment''? Do crop insurance, crop research funding, and rural infrastructure count as protectionist extraction or as public goods?',
    'Decomposition of OECD Producer Support Estimate (PSE) into (a) pure transfer components (tariffs, quotas, deficiency payments) vs (b) public-goods components (research, infrastructure). Comparison of extractiveness scores for each sub-type.',
    'If public-goods components are substantial: extractiveness should be downgraded 0.10-0.15; constraint reclassifies toward rope. If extraction is concentrated in transfer mechanisms: extractiveness confirmed; snare classification from developing-world perspective is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_measurement_boundary, empirical, 'Boundary between subsidy extraction and public goods provision').

omega_variable(
    developing_world_coalition_power,
    'Can developing-world agricultural exporters organize sufficient coalition power to shift classification from snare to tangled_rope? (Per Dynamic Coalition extension: if victim count exceeds critical_mass_threshold and organization costs drop, powerless agent reclassifies as organized.)',
    'Analysis of African Union, Alliance for a Green Revolution in Africa (AGRA), and G20 agricultural negotiation bloc effectiveness; measurement of organizational cost decline over time',
    'If coalition reaches critical mass: developing-world perspective reclassifies from snare (chi ≥ 0.66) to tangled_rope (0.40 ≤ chi ≤ 0.90); negotiating power emerges; constraint moves toward renegotiation equilibrium. If coalition fails: snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_world_coalition_power, empirical, 'Coalition power of developing-world agricultural exporters').

omega_variable(
    consumer_awareness_internalization,
    'To what extent have domestic food consumers internalized the protectionist constraint as natural/necessary vs perceiving it as extractive? Does identity-locking occur (consumers adopt ''buy local'' identity reducing exit probability)?',
    'Survey data on consumer willingness to pay for protectionist outcomes vs price; analysis of ''buy local'' movement as identity coordination vs forced preference; price elasticity of demand for protected agricultural products',
    'If strong internalization/identity-locking: consumer perspective reclassifies from constrained exit to identity_locked; perceived immutability increases even if structural barriers lower. Constraint becomes more culturally entrenched. If weak internalization: consumer perspective remains constrained; exit option remains viable if prices fell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_awareness_internalization, empirical, 'Internalization and identity-locking of protectionist norms in consumer consciousness').

omega_variable(
    agricultural_efficiency_gains_realization,
    'How much of the theoretical efficiency gain from agricultural liberalization would actually be captured by developing-world exporters vs accumulated by agribusiness consolidation in importing countries?',
    'Comparative analysis of post-liberalization agricultural sectors (New Zealand dairy, Philippine sugar); measurement of producer price increases vs retail price decreases; concentration analysis of reformed sectors',
    'If gains concentrated in import-country agribusiness: liberalization reduces snare severity for developing-world farmers but converts to new extraction mechanism (monopsony pricing). If gains distributed: snare perspective resolves. Classification shifts to reflect new constraint structure (different ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_efficiency_gains_realization, empirical, 'Distribution of efficiency gains from agricultural liberalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agricultural_trade_protectionism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agtp_tr_t0, agricultural_trade_protectionism, theater_ratio, 0, 0.48).
narrative_ontology:measurement(agtp_tr_t10, agricultural_trade_protectionism, theater_ratio, 10, 0.58).
narrative_ontology:measurement(agtp_tr_t20, agricultural_trade_protectionism, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(agtp_be_t0, agricultural_trade_protectionism, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(agtp_be_t10, agricultural_trade_protectionism, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(agtp_be_t20, agricultural_trade_protectionism, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agricultural_trade_protectionism, resource_allocation).
narrative_ontology:affects_constraint(agricultural_trade_protectionism, global_commodity_price_volatility).
narrative_ontology:affects_constraint(agricultural_trade_protectionism, developing_world_agricultural_debt_trap).
narrative_ontology:affects_constraint(agricultural_trade_protectionism, land_use_pattern_lock_in).

% DUAL FORMULATION NOTE:
% Agricultural trade protectionism is upstream of commodity price volatility (protectionism dampens price shocks that would otherwise cascade) and downstream of structural land-use patterns and debt relationships in developing countries (protectionism compounds these by blocking agricultural export revenue). Each linked constraint has its own ε value reflecting its specific empirical content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agricultural_trade_protectionism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
