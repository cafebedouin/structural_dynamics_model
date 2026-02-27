% ============================================================================
% CONSTRAINT STORY: incumbent_steel_production
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_steel_production, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: incumbent_steel_production
 *   human_readable: Incumbent Blast Furnace Steel Production Method
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The blast furnace method dominates global steel production because it
 *   achieved massive economies of scale during the 20th century industrial
 *   expansion. High-grade iron ore and coking coal are concentrated
 *   geographically, creating lock-in: integrated steelmakers invested
 *   billions in blast furnace capacity, supply chains optimized for blast
 *   furnace inputs, and regulatory standards written around blast furnace
 *   outputs. This constraint exhibits tangled rope structure: genuine
 *   coordination benefits exist (the blast furnace ecosystem enables
 *   efficient large-scale production), but extraction mechanisms have
 *   intensified as technological alternatives emerge (hydrogen-based direct
 *   reduction, electric arc furnaces powered by renewables) and resource
 *   constraints tighten (declining ore grades, climate regulations forcing
 *   coal phase-out). The constraint is neither a natural law nor pure
 *   coordination — it is an institutional arrangement maintained through
 *   capital allocation bias, regulatory accommodation, and supply chain
 *   lock-in, not through technical inevitability. Different stakeholders
 *   experience it differently: incumbent steelmakers see rope (efficiency),
 *   developing steelmakers see snare (trapped by geography), alternative
 *   technologies see tangled rope (suppressed), coal-producing nations see
 *   tangled rope (locked in), and decarbonization coalitions see scaffold
 *   (temporary, with sunset clauses). The extractiveness has increased from
 *   0.42 to 0.58 over the interval as climate regulations force incumbent
 *   steelmakers to defend blast furnace investment against alternatives,
 *   increasing theater and suppression mechanisms.
 *
 * KEY AGENTS:
 *   - Integrated Steelmakers (institutional/arbitrage): Primary beneficiaries — capture economies of scale, lock in supply chains, maintain capital depreciation advantages. Structural relationship: beneficiary. Can exit (have capital to invest in alternatives) but choose not to.
 *   - Coking Coal Producers (organized/constrained): Secondary beneficiaries — export commodity dependent on blast furnace demand. Structural relationship: beneficiary in short term, victim in long term (locked into declining markets). Cannot easily exit resource extraction.
 *   - Iron Ore Miners (institutional/arbitrage): Beneficiaries — depend on blast furnace demand for high-grade ore. Can potentially pivot to alternative mineral extraction but currently locked into iron ore.
 *   - Alternative Steel Technology Developers (moderate/constrained): Victims — suppressed by capital allocation favoring incumbents, market lock-in, regulatory inertia. Genuine technical capability but constrained exit (capital barriers).
 *   - Developing Nations Without Ore Reserves (powerless/trapped): Victims — cannot access high-grade iron ore at competitive prices, forced to purchase at monopolistic terms or import finished steel. No exit option.
 *   - Decarbonization Programs (organized/constrained): Victims of delay — forced to accommodate blast furnace timelines in climate targets, constrained by political economy of steel. But building organized exit pathways (green steel standards, carbon pricing).
 *   - Global Steel Customers (powerful/mobile): Secondary actors with exit capacity — can specify green steel, pressure suppliers to transition, source from alternatives. Mobile exit option (can diversify suppliers) creates potential coalition with decarbonization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_steel_production, 0.58).
domain_priors:suppression_score(incumbent_steel_production, 0.62).
domain_priors:theater_ratio(incumbent_steel_production, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_steel_production, extractiveness, 0.58).
narrative_ontology:constraint_metric(incumbent_steel_production, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(incumbent_steel_production, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_steel_production, tangled_rope).
narrative_ontology:human_readable(incumbent_steel_production, "Incumbent Blast Furnace Steel Production Method").
narrative_ontology:topic_domain(incumbent_steel_production, "technological/economic").

domain_priors:requires_active_enforcement(incumbent_steel_production).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_steel_production, integrated_steelmakers).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, coking_coal_producers).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, iron_ore_miners).
narrative_ontology:constraint_victim(incumbent_steel_production, alternative_steel_technologies).
narrative_ontology:constraint_victim(incumbent_steel_production, developing_nations_without_ore_reserves).
narrative_ontology:constraint_victim(incumbent_steel_production, decarbonization_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING STEEL STARTUP (SNARE) — Small steelmakers lacking access to high-grade ore and coking coal infrastructure cannot exit the blast furnace supply chain. Trapped by resource geography and capital barriers; must purchase inputs at monopolistic prices or abandon steel production entirely. Maximum experienced extraction.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTERNATIVE STEEL TECHNOLOGY DEVELOPER (TANGLED ROPE) — Electric arc furnace (EAF), hydrogen-based direct reduction, and other emerging methods offer genuine technical alternatives, but face suppression through: (1) capital requirements favoring incumbents, (2) market lock-in around blast furnace-optimized supply chains, (3) regulatory acceptance delays. Coordination benefit exists (cleaner production, circular economy potential) but is asymmetrically distributed — incumbents extract rents by delaying transition while appearing to support decarbonization.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTEGRATED STEELMAKER (ROPE) — Major producers benefit from blast furnace infrastructure as a coordination standard. Access to long-term ore and coal contracts, established supply logistics, and production process expertise create genuine efficiency gains. The constraint appears as pure coordination — the blast furnace ecosystem enables scale economies and reliable output. High arbitrage capacity (can shift investment to alternative methods if economics change).
constraint_indexing:constraint_classification(incumbent_steel_production, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECARBONIZATION COALITION (SCAFFOLD) — Policy makers, climate coalitions, and steel customers organizing around net-zero commitments treat blast furnace dominance as a temporary problem with a sunset clause. EU Green Deal, US Inflation Reduction Act, and green steel procurement standards are building economic exit ramps through: (1) carbon border adjustment mechanisms (CBAM), (2) green steel premium markets, (3) subsidies for hydrogen and EAF infrastructure. Experienced extraction is low because the coalition has organized exit pathways with explicit timelines (2030-2050 decarbonization targets). Theater ratio remains moderate because the coalition's mechanisms involve genuine regulatory shifts, not performative compliance.
constraint_indexing:constraint_classification(incumbent_steel_production, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CAPITAL INVESTMENT SYSTEM (PITON) — Blast furnace infrastructure represents tens of billions in sunk capital with typical 30-50 year lifespans. Financial institutions and industrial policy continue privileging blast furnace investment through: (1) favorable depreciation schedules, (2) assumption that existing capacity will remain economically central, (3) low cost of capital for incumbent expansion vs. higher-risk alternatives. This persistence appears to be functional coordination (avoiding stranded assets) but is increasingly performative — the theater lies in asserting technological inevitability when superior alternatives exist. Theater ratio is the primary classification driver here, not high extraction; the system extracts value by delaying transition, but this extraction is theater-driven rather than structurally novel.
constraint_indexing:constraint_classification(incumbent_steel_production, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COKING COAL PRODUCER NATION (TANGLED ROPE) — Nations dependent on coking coal exports (Australia, Mongolia, parts of Africa and Central Asia) experience the blast furnace constraint as both coordination and extraction. Coordination benefit: established export markets, foreign exchange revenue, employment stability. Extraction asymmetry: locked into commodity markets vulnerable to decarbonization shocks, structurally constrained from diversifying economy, dependent on incumbent steelmakers' purchasing power. Exit options are constrained by both resource geography and sunk capital in mining infrastructure. The nation benefits from the status quo but cannot easily exit, creating classic tangled rope dynamics.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, blast furnace dominance is neither a natural law nor a pure coordination mechanism. It is a genuine hybrid: (1) coordination function is real — the blast furnace solved the technical problem of large-scale steel production in the 19th century and created economies of scale that persist. (2) Extraction is real and growing — resource constraints (declining ore grades, coking coal scarcity), climate costs externalized, and technology lock-in mechanisms delay transition to superior alternatives. (3) Enforcement is active — capital allocation favors incumbents, regulatory timelines are accommodating, supply chain lock-in persists. This is a textbook tangled rope: benefits exist, but asymmetrically distributed, and alternatives are suppressed through institutional rather than technical barriers.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_steel_production_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_steel_production, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_steel_production, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_steel_production, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_steel_production, TR),
    TR >= 0.70.

:- end_tests(incumbent_steel_production_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The blast furnace constraint extracts value through multiple mechanisms: (1) geographic concentration of high-grade ore and coking coal enables monopolistic pricing by resource owners, (2) capital barriers to building alternative capacity delay transition and protect incumbent returns, (3) supply chain lock-in forces customers to accept blast furnace-produced steel even at environmental cost, (4) regulatory accommodation (slow carbon pricing, long transition timelines) protects incumbent investment. However, extraction is not as severe as a pure snare (0.75+) because genuine coordination benefits exist — the blast furnace ecosystem does enable efficient production, and some alternative technologies (EAF with renewable power) are approaching cost parity. Suppression (0.62): Moderate-high. Significant barriers to exit: (1) capital requirements for new steel plants ($1-2B for greenfield blast furnace vs. $500M-1B for EAF, but still massive), (2) supply chain inertia — steel customers have specifications and logistics designed around blast furnace outputs, (3) regulatory uncertainty — green steel standards are emerging but not globally uniform, (4) employment and geopolitical lock-in — steel production is strategic, nations defend incumbent capacity. Suppression is not total because alternatives are technically viable and capital is available for transition; the suppression is institutional rather than absolute. Theater ratio (0.48): Moderate. The constraint exhibits mixed theater and function. Real functionality exists — the blast furnace ecosystem does solve genuine coordination problems around scale and efficiency. But theater is increasing: (1) incumbent steelmakers tout decarbonization commitments while expanding blast furnace capacity, (2) regulatory timelines extend far beyond feasibility to protect incumbent investment, (3) green steel standards are performative compliance (minimal carbon reduction) rather than genuine alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is driven by structural position relative to the ore/coal supply chain and the capital requirements for alternatives. Integrated steelmakers (institutional/arbitrage) perceive rope — the constraint is a coordination mechanism enabling efficient production, with high exit capacity via capital allocation. They have agency to shift investment but perceive no economic incentive to do so yet. Developing steelmakers (powerless/trapped) perceive snare — geographic concentration of ore and coal, capital barriers, and supply chain lock-in trap them in dependent positions with no exit. Alternative technology developers (moderate/constrained) perceive tangled rope — genuine technical capability to solve the problem (hydrogen-based DR, EAF) but suppressed by capital allocation bias and market lock-in. They have partial agency (can prove technology works) but constrained exit (cannot build plants without capital from incumbents or governments). Decarbonization coalitions (organized/constrained) perceive scaffold — they see the constraint as a temporary institutional arrangement with a clear sunset clause (2050 net-zero targets, carbon pricing). Exit pathways exist (green steel standards, alternative technologies) but require coordinated investment and regulatory certainty. The analytical observer perceives tangled rope — a genuine hybrid of coordination and extraction, neither natural nor pure, institutional rather than inevitable. Each perspective's classification is accurate from its structural position; the gap is real, not observational noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position in the extraction flow and exit capacity. Beneficiaries with high exit capacity (integrated steelmakers, institutional/arbitrage) derive low d (~0.15) — they benefit from the constraint and can exit (by investing in alternatives), but choose not to. The sigmoid f(d) produces low χ for this perspective even with moderate base extractiveness. Victims with no exit capacity (developing steelmakers, powerless/trapped) derive high d (~0.95) — they bear the constraint's full weight with no option to leave or organize. f(d) produces high χ, magnifying the experienced extraction. Alternative technology developers (moderate/constrained) derive mid-range d (~0.65) — they perceive the constraint as oppositional (it suppresses their alternatives) but have some agency (technical proof of concept, coalitional organizing potential). f(d) produces moderate χ. The coal-producing nation (organized/constrained) derives mid-range d (~0.55) — benefits short-term from blast furnace demand (beneficiary in current regime) but faces lock-in and long-term vulnerability (victim in transition regime). This dual position is captured by the moderate d value, which reflects that the nation is both beneficiary and victim depending on time horizon. No directionality overrides are needed — the structural derivation from beneficiary/victim declarations and exit options produces accurate d values for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED THROUGH STRUCTURAL DECOMPOSITION: The constraint resolves the tangled rope/snare ambiguity by distinguishing between the true structural function (coordination around blast furnace-optimized supply chains and economies of scale) and the extraction mechanisms (capital lock-in, geographic monopoly, supply chain inertia, regulatory accommodation). This is NOT a case of wrongly labeling pure extraction as coordination. Rather: (1) Coordination is genuine and creates real efficiency gains. (2) Extraction is genuine and growing as alternatives become viable. (3) The constraint is hybrid because both functions are essential to explaining why it persists despite superior alternatives existing. If we classified this as pure snare, we would miss the genuine coordinating function that makes the constraint efficient relative to fragmented competitors. If we classified it as pure rope, we would miss the asymmetric extraction that makes alternatives artificially uncompetitive. The tangled rope classification captures the true hybrid: a mechanism that solves real coordination problems (scale economies, supply reliability) while extracting rents through capital lock-in and market dominance. The scaffold perspective (decarbonization coalition) adds crucial temporal information: the constraint is undergoing institutional transition (sunset clause through 2050 net-zero targets). This sunset is real — carbon pricing, green steel standards, and alternative technology deployment create genuine exit pathways with explicit timelines. The piton perspective (legacy capital system) reveals theater increasing over time (theater ratio rises from 0.35 to 0.48 in the interval) — the constraint persists partly through performative defense of incumbent investment, not just functional necessity. Mandatrophy is resolved by acknowledging that the constraint is currently tangled rope but is transitioning toward piton (increasing theater as true functionality declines) and toward mountain for certain resource constraints (ore grade depletion, coal scarcity). The classification is temporally grounded and structurally accurate: the incumbent blast furnace method is a genuine hybrid that extracts asymmetrically while providing coordination benefits, and this hybrid is unstable — either alternatives will break the lock-in (transition to alternative technologies), or resource scarcity will force transition (constraints become physical), or institutional inertia will maintain it as theater (constraint degrades to piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_technology_viability,
    'Will hydrogen-based direct reduction or electric arc furnace technologies achieve cost parity with blast furnaces by 2035?',
    'Tracking capital costs for green steel capacity installations, energy price trajectories, and learning curves for nascent technologies; comparative levelized cost analysis at scale',
    'If yes: blast furnace constraint transitions from tangled_rope to piton (enforcement weakens). If no: tangled_rope status persists; transition extends beyond 2050, affecting mandatrophy classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_technology_viability, empirical, 'Whether alternative steel technologies reach economic viability').

omega_variable(
    iron_ore_reserve_depletion_timeline,
    'At current extraction rates and declining ore grades, when do high-grade iron ore reserves become economically exhausted?',
    'US Geological Survey reserve assessments, grade decline trends in major mining regions, break-even cost analysis for low-grade ore processing',
    'If depletion timeline < 2050: blast furnace constraint becomes mountain (physical scarcity forces transition). If > 2070: constraint remains institutional/economic (alternatives compete, not replace due to shortage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_ore_reserve_depletion_timeline, empirical, 'Timeline for high-grade iron ore reserve exhaustion').

omega_variable(
    carbon_pricing_enforcement,
    'Will carbon border adjustment mechanisms (CBAM) and similar policies actually create sufficient price pressure to shift blast furnace investment patterns?',
    'Monitoring carbon credit prices, green premium actualization in steel procurement markets, capital expenditure shifts in integrated steelmaker five-year plans',
    'If effective: scaffold perspective confirmed — decarbonization coalition has real exit pathway with 2030-2040 sunset. If weak: coal coalition perspective strengthens; constraint remains tangled rope longer than expected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_pricing_enforcement, empirical, 'Whether carbon pricing enforcement creates sufficient transition incentives').

omega_variable(
    supply_chain_lock_in_strength,
    'How deeply embedded are blast furnace-optimized logistics and customer specifications in global supply chains?',
    'Analysis of switching costs for major steel consumers (automotive, construction, appliance manufacturers); feasibility studies for supply chain reconfiguration for alternative steel types',
    'If lock-in is strong: suppression value increases; victims trapped longer. If lock-in is weak: exit paths open faster; constraint weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_lock_in_strength, empirical, 'Degree of supply chain lock-in around blast furnace-optimized specifications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_steel_production, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(steel_tr_t0, incumbent_steel_production, theater_ratio, 0, 0.35).
narrative_ontology:measurement(steel_tr_t10, incumbent_steel_production, theater_ratio, 10, 0.42).
narrative_ontology:measurement(steel_tr_t20, incumbent_steel_production, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(steel_be_t0, incumbent_steel_production, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(steel_be_t10, incumbent_steel_production, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(steel_be_t20, incumbent_steel_production, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_steel_production, resource_allocation).
narrative_ontology:affects_constraint(incumbent_steel_production, coking_coal_supply_chain).
narrative_ontology:affects_constraint(incumbent_steel_production, iron_ore_geographic_concentration).
narrative_ontology:affects_constraint(incumbent_steel_production, green_steel_technology_adoption).
narrative_ontology:affects_constraint(incumbent_steel_production, global_carbon_pricing_harmonization).

% DUAL FORMULATION NOTE:
% The incumbent blast furnace constraint decomposes into several structurally distinct constraints: (1) coking coal supply chain (resource extraction + geopolitics), (2) iron ore geographic concentration (resource monopoly), (3) alternative technology suppression (capital allocation bias), (4) global carbon pricing (regulatory harmonization). Each has its own ε and classification. The blast furnace constraint itself (ε=0.58) represents the coordination function around production scale; downstream constraints represent the mechanisms that lock in that coordination function despite alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
