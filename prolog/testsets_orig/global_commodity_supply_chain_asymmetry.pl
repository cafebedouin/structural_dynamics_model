% ============================================================================
% CONSTRAINT STORY: global_commodity_supply_chain_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_commodity_supply_chain_asymmetry, []).

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
 *   constraint_id: global_commodity_supply_chain_asymmetry
 *   human_readable: Global Commodity Supply Chain Asymmetry
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The global commodity supply chain represents a system of nested
 *   extraction mechanisms operating across multiple institutional scales.
 *   Primary commodity-producing nations export raw materials at depressed
 *   prices while consuming nations (and the logistics intermediaries who
 *   orchestrate global supply) capture the surplus. The constraint exhibits
 *   the full spectrum of DR classification: extraction workers experience
 *   snare dynamics (trapped by geography and capital scarcity); producing
 *   nations experience mixed coordination-extraction (tangled rope — they
 *   benefit from market access but lose through asymmetric terms of trade);
 *   consuming nations and intermediaries experience pure coordination (rope —
 *   the system solves their allocation problem); organized producers
 *   experience variable constraint (tangled rope → snare depending on cartel
 *   cohesion); and the comparative advantage narrative naturalizes the
 *   structure as immutable (false mountain). The theater ratio (0.58)
 *   reflects that commodity exchanges present themselves as transparent
 *   price-discovery mechanisms while actually encoding market power
 *   asymmetries, information privileges for large traders, and structural
 *   biases toward consuming-nation interests. Over the 40-year interval
 *   (1985-2025), extractiveness has risen from 0.38 to 0.58 as intermediary
 *   consolidation has increased and direct producer-consumer trade has
 *   declined. The theater ratio has similarly risen from 0.38 to 0.58 as
 *   commodity governance institutions (IMF conditionality, futures market
 *   standardization, trading house dominance) have become more elaborate
 *   without increasing actual price fairness or producer agency.
 *
 * KEY AGENTS:
 *   - Extraction Labor Force: Primary victim (powerless/trapped) — miners, agricultural workers, plantation laborers in producing nations bearing occupational hazard, price volatility risk, and environmental externality costs with no exit options
 *   - Primary Producing Nation (State-Level): Secondary victim/constrained beneficiary (moderate/constrained) — experiences both coordination benefit (market access, revenue) and extraction (deteriorating terms of trade, external price-setting, debt dependency)
 *   - Consuming Nation / Distribution Hub: Primary beneficiary (institutional/arbitrage) — captures low-cost commodity access; experiences supply chain as coordination mechanism enabling efficient allocation with full exit optionality
 *   - Logistics Intermediary / Commodity Exchange: Powerful beneficiary (powerful/arbitrage) — captures spread through information asymmetry, market-making, and control over standardized contracts; controls price discovery without bearing commodity risk
 *   - State-Backed Cartels and Producer Cooperatives: Organized agents (organized/constrained) — attempt to reverse asymmetry through collective bargaining; experienced extraction varies dramatically with cartel cohesion
 *   - Commodity Exchange Governance Institutions: Institutional actors (institutional/constrained) — maintain formal coordination infrastructure; increasingly serve as theater masking market power asymmetry rather than functioning price-discovery mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional asymmetry through comparative advantage framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_commodity_supply_chain_asymmetry, 0.58).
domain_priors:suppression_score(global_commodity_supply_chain_asymmetry, 0.65).
domain_priors:theater_ratio(global_commodity_supply_chain_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_commodity_supply_chain_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_commodity_supply_chain_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(global_commodity_supply_chain_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_commodity_supply_chain_asymmetry, tangled_rope).
narrative_ontology:human_readable(global_commodity_supply_chain_asymmetry, "Global Commodity Supply Chain Asymmetry").
narrative_ontology:topic_domain(global_commodity_supply_chain_asymmetry, "economic/geopolitical").

domain_priors:requires_active_enforcement(global_commodity_supply_chain_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_commodity_supply_chain_asymmetry, consuming_nations_distribution_hubs).
narrative_ontology:constraint_beneficiary(global_commodity_supply_chain_asymmetry, logistics_intermediaries).
narrative_ontology:constraint_beneficiary(global_commodity_supply_chain_asymmetry, commodity_exchanges).
narrative_ontology:constraint_victim(global_commodity_supply_chain_asymmetry, primary_producing_nations).
narrative_ontology:constraint_victim(global_commodity_supply_chain_asymmetry, extraction_labor_force).
narrative_ontology:constraint_victim(global_commodity_supply_chain_asymmetry, resource_dependent_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTRACTION LABOR FORCE (SNARE) — Trapped by geographic location, capital scarcity, and lack of alternative income. Bears full cost of price volatility, environmental degradation, and occupational hazard. No exit options except migration with extreme barriers. Maximum extraction experienced — labor provides commodity at lowest possible cost while consuming nation captures surplus.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIMARY PRODUCING NATION (TANGLED ROPE) — Constrained by capital requirements for supply chain infrastructure, transportation networks, and processing capacity. Experiences genuine coordination benefit: access to global markets enables revenue and development. But coordination is asymmetric: terms of trade degrade over time; commodity prices are set externally; import-substitution creates debt dependency. Moderate extraction within mixed coordination framework.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSUMING NATION / DISTRIBUTION HUB (ROPE) — Benefits from access to low-cost commodities; experiences constraint as coordination mechanism: transparent supply chains, standardized contracts, and price discovery enable efficient allocation. Net beneficiary with arbitrage option: can switch suppliers, negotiate terms, or vertically integrate. Experiences constraint as pure coordination with minimal coercive overhead.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOGISTICS INTERMEDIARY / COMMODITY EXCHANGE (TANGLED ROPE) — Powerful actors with significant exit options (can shift commodities, reroute supply chains, diversify between markets). Experience the constraint as coordination: information infrastructure, standardized contracts, and price discovery mechanisms they operate generate genuine coordination benefit. But also extract through margin-taking, information asymmetry, and control over market access. Asymmetric extraction within coordination framework — they capture spread without bearing commodity risk.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMODITY EXCHANGE GOVERNANCE (PITON) — Formal institutions (futures markets, contract standards, price-setting mechanisms) claim to solve coordination through transparency and standardization. But the theatrical component is substantial: published prices often diverge from actual transaction prices; standardized contracts hide complex hedging relationships; price discovery mechanisms serve sophisticated traders more than primary producers. Theater ratio reflects that governance systems maintain the appearance of coordination while performing less discovery function than marketed.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED PRODUCERS / STATE CARTELS (TANGLED ROPE) — Organized agents (OPEC, coffee cartels, grain exporters) attempt to reverse the asymmetry through collective action. Experiences constraint as a structure they are actively resisting. When cohesion is high, extraction is reduced and they experience the constraint as manageable tangled rope. When coordination breaks (price wars, defection), the constraint reverts to snare dynamics. This perspective shows how organization changes experienced extractiveness without changing the base structural property — d shifts from 0.80 to 0.45 when victims organize.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the comparative advantage framework, commodity supply chain asymmetry reflects immutable resource distribution: some nations have endowments, others have capital/consumption. The constraint is naturalized as optimal allocation under scarcity. However, the base properties contradict the mountain classification — suppression 0.65, extractiveness 0.58, and asymmetric beneficiary/victim structure indicate contingent institutional arrangements rather than immutable resource facts. This is a false summit.
constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_commodity_supply_chain_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_commodity_supply_chain_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_commodity_supply_chain_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_commodity_supply_chain_asymmetry, TR),
    TR >= 0.70.

:- end_tests(global_commodity_supply_chain_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Base value reflects that primary producers capture 20-30% of final consumer price while intermediaries and consuming nations capture 70-80%. This is not inevitable — cooperative supply chains (Fair Trade, direct bilateral agreements) show lower extractiveness possible. The rising trajectory reflects accelerating intermediary consolidation (container shipping duopoly, futures market concentration, trading house dominance). Suppression (0.65): High. Multiple barriers lock primary producers into supply chain: capital scarcity (infrastructure investment unfeasible for individual producers), information asymmetry (producers lack price data available to traders in real time), technology dependency (standardized containers, exchange protocols controlled by consuming nations), and institutional lock-in (debt structures, IMF conditionality, contract standardization). Theater ratio (0.58): Moderate-high and rising. Commodity exchanges present themselves as transparent price-discovery mechanisms, but actual prices differ significantly from 'market prices' due to: (1) hedging relationships concentrated among large traders, (2) information delays hitting producers later than traders, (3) standardized contracts that hide actual transaction complexity, (4) producer nations excluded from exchange governance despite being price-takers. The theater has increased as institutional complexity has increased without corresponding transparency increases.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival gap across a single structural system. Extraction workers see snare (trapped, bearing all downside). Producing nations see tangled rope (some coordination benefit but extractive asymmetry). Consuming nations see rope (pure coordination with no extraction burden). Intermediaries see rope from their position (they coordinate without bearing risk). Organized producers see variable constraint (snare when cohesion fails, tangled rope when it holds). The analytics observer risks seeing mountain (comparative advantage is immutable) but structural data reveals false summit. The gap arises because d varies 40-fold across perspectives: labor (d=0.95) to consuming nation (d=0.15) spans almost the full sigmoid range. The snare perspective and rope perspective are not 'different interpretations' — they measure genuinely different experienced constraints for agents in structurally different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) computation depends on beneficiary/victim status and exit options: (1) Extraction labor: victim + trapped → d=0.95 (high), (2) Producing nation: victim + constrained → d=0.70 (moderate-high), (3) Consuming nation: beneficiary + arbitrage → d=0.15 (low), (4) Intermediary: beneficiary + arbitrage → d=0.25 (low), (5) Organized producers: victims with constrained exit normally d=0.70, but when cartel cohesion is high, they shift to organized power atom, which has canonical d=0.40, reducing experienced extraction. The shift from unorganized (powerless, d=0.95) to organized (organized, d=0.40) demonstrates how the same structural constraint produces different classifications when agent power changes through collective action. This is not a perspective shift — it's a real change in structural capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by showing that 'coordination' and 'extraction' are not binary opposites but dimensions of the same system viewed from different structural positions. The global supply chain IS a coordination mechanism (it genuinely solves the allocation problem of connecting producers to consumers across distance). AND it IS an extraction mechanism (it asymmetrically concentrates surplus among intermediaries and consuming nations). The mandatrophy dissolves when we recognize that from the consuming nation perspective, the system coordinates efficiently with minimal extraction. From the extraction worker perspective, the same system extracts maximally. The analytical false summit (mountain) is the belief that comparative advantage makes this asymmetry immutable — it naturalizes what is an engineered institutional structure. Resolution requires recognizing that alternative supply chain architectures (cooperative models, state-run commodity exchanges, direct bilateral trade) would have different extractiveness values for the same underlying resource flows. The constraint is not a law of nature but a choice of institutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    terms_of_trade_degradation_mechanism,
    'Is the persistent decline in commodity terms of trade (primary products vs manufactures) an inherent feature of comparative advantage or a contingent outcome of institutional power asymmetry?',
    'Historical decomposition of terms-of-trade changes into productivity factors vs. market power factors; cross-sectional comparison of commodity supplies managed by organized cartels vs unorganized suppliers',
    'If inherent: commodity constraint is mountain-adjacent (unavoidable). If contingent: constraint is engineered snare (institutional power concentrating extraction). Resolution shifts from accepting asymmetry to redesigning institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terms_of_trade_degradation_mechanism, empirical, 'Whether terms-of-trade degradation is structural or institutional').

omega_variable(
    supply_chain_infrastructure_dependency,
    'How much of the extraction margin is inherent to transportation and logistics costs vs. how much is captured by intermediaries through information asymmetry and market position?',
    'Cost accounting: actual containerization, shipping, insurance, storage costs vs. trading margins; comparison of markup ratios across commodity chains with different intermediary concentration levels',
    'If mostly inherent cost: constraint is coordination problem (rope/tangled rope appropriate). If mostly captured margin: constraint is intermediate monopoly (snare/piton appropriate). Resolution determines whether supply chain reform is technical or political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_infrastructure_dependency, empirical, 'Intermediary margin vs. inherent logistics cost decomposition').

omega_variable(
    alternative_market_viability,
    'Could direct bilateral trade between commodity producers and consuming nations bypass the global supply chain, or is supply chain consolidation a necessary feature of scale?',
    'Case studies of direct trade agreements (producer-consumer state deals); comparison of transaction costs (negotiation, verification, finance, shipping) for direct vs. intermediated supply chains at different volumes',
    'If viable: constraint is institutional choice (piton/snare that could be unwound). If necessary: constraint is coordination requirement (rope/tangled rope legitimate). Resolution determines policy tractability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_market_viability, conceptual, 'Whether direct producer-consumer trade is viable alternative').

omega_variable(
    extraction_asymmetry_reversibility,
    'When a producing nation develops processing capacity or logistics infrastructure (reducing dependency on intermediaries), does the extraction mechanism dissolve or migrate to new domains?',
    'Historical case studies: nations that moved up value chain (vertical integration in coffee, cocoa, lithium extraction). Track what extraction mechanisms emerge after intermediary dependency is reduced.',
    'If extraction dissolves: constraint is remediable through development. If migrates: institutional asymmetry is more fundamental (shifts to different extraction mechanism, requires structural reform not just development).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_reversibility, empirical, 'Whether reducing intermediary dependency dissolves or migrates extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_commodity_supply_chain_asymmetry, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcsa_tr_t0, global_commodity_supply_chain_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gcsa_tr_t20, global_commodity_supply_chain_asymmetry, theater_ratio, 20, 0.48).
narrative_ontology:measurement(gcsa_tr_t40, global_commodity_supply_chain_asymmetry, theater_ratio, 40, 0.58).
narrative_ontology:measurement(gcsa_tr_t10, global_commodity_supply_chain_asymmetry, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(gcsa_be_t0, global_commodity_supply_chain_asymmetry, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gcsa_be_t20, global_commodity_supply_chain_asymmetry, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(gcsa_be_t40, global_commodity_supply_chain_asymmetry, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(gcsa_be_t10, global_commodity_supply_chain_asymmetry, base_extractiveness, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_commodity_supply_chain_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(global_commodity_supply_chain_asymmetry, 0.18).
narrative_ontology:affects_constraint(global_commodity_supply_chain_asymmetry, terms_of_trade_volatility).
narrative_ontology:affects_constraint(global_commodity_supply_chain_asymmetry, primary_commodity_price_externality).
narrative_ontology:affects_constraint(global_commodity_supply_chain_asymmetry, debt_dependency_cycle).

% DUAL FORMULATION NOTE:
% Global commodity supply chain asymmetry decomposes into three structurally distinct constraints: (1) resource_allocation coordination (base ε~0.10, genuine coordination function), (2) information_asymmetry extraction (ε~0.45, intermediary margin capture), (3) institutional_power_consolidation (ε~0.65, macroeconomic asymmetry). This story models the integrated constraint at ε=0.58. Sister stories model the decomposed mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_commodity_supply_chain_asymmetry, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
