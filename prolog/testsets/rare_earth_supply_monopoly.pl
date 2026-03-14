% ============================================================================
% CONSTRAINT STORY: rare_earth_supply_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_supply_monopoly, []).

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
 *   constraint_id: rare_earth_supply_monopoly
 *   human_readable: Rare Earth Supply Monopoly and Strategic Chokepoint Extraction
 *   domain: geopolitical_economy/technology_supply_chains
 *
 * SUMMARY:
 *   Rare earth element (REE) supply concentration creates a global chokepoint
 *   in technology manufacturing. A single dominant producer (historically
 *   China, controlling 60-95% of global supply and >95% of processing
 *   capacity) extracts substantial economic rents from dependent
 *   manufacturers while maintaining strategic leverage over technology
 *   development and geopolitical competition. The constraint exhibits snare
 *   characteristics at the dependent manufacturer level (powerless, trapped),
 *   tangled rope characteristics at the organized state coalition level
 *   (genuine coordination benefit alongside extraction), and rope
 *   characteristics for incumbent firms with contracted supply. The
 *   constraint's theater ratio is relatively low (0.35) because the
 *   extraction mechanism is direct and functional — the monopolist controls a
 *   genuinely scarce resource through institutional arrangements rather than
 *   maintaining the constraint through elaborate performance. The key
 *   structural question is whether the scarcity is geological (immutable) or
 *   institutional (changeable through policy and investment). Evidence
 *   strongly suggests institutional: rare earths are not actually rare;
 *   deposits exist globally; processing monopoly is maintained through
 *   regulation, infrastructure control, and first-mover advantage, not
 *   geological fact.
 *
 * KEY AGENTS:
 *   - Dominant Producer State: Primary beneficiary (institutional/arbitrage) — extracts economic rents and wieldes strategic leverage through supply control; maintains monopoly through regulation and processing knowledge
 *   - Dependent Manufacturers (Electronics, Magnets, Catalysts): Primary victims (powerless/trapped) — no practical exit from rare earth requirements; face rising prices, rationing, and strategic blackmail during geopolitical tensions
 *   - Incumbent Contracted Technology Firms: Secondary beneficiary (institutional/arbitrage) — locked-in long-term contracts provide supply security at favorable prices; benefit from monopoly preventing new competitors' access to supply
 *   - Organized Consumer Alliances (Governments, Defense Sectors): Secondary victim-turned-organizer (organized/constrained) — bear coordination costs of collective procurement, strategic reserves, and R&D but have constrained exit options through domestic investment
 *   - Substitute Technology Developers: Structural reformers (organized/constrained) — driving technological substitution (non-rare-earth magnets, improved phosphors, recycling) that creates sunset pathway for the constraint
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional monopoly as geological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_supply_monopoly, 0.58).
domain_priors:suppression_score(rare_earth_supply_monopoly, 0.72).
domain_priors:theater_ratio(rare_earth_supply_monopoly, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_supply_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_supply_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_supply_monopoly, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_supply_monopoly, snare).
narrative_ontology:human_readable(rare_earth_supply_monopoly, "Rare Earth Supply Monopoly and Strategic Chokepoint Extraction").
narrative_ontology:topic_domain(rare_earth_supply_monopoly, "geopolitical_economy/technology_supply_chains").

domain_priors:requires_active_enforcement(rare_earth_supply_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_supply_monopoly, dominant_producer_state).
narrative_ontology:constraint_beneficiary(rare_earth_supply_monopoly, incumbent_technology_firms_with_contracted_supply).
narrative_ontology:constraint_victim(rare_earth_supply_monopoly, dependent_technology_manufacturers).
narrative_ontology:constraint_victim(rare_earth_supply_monopoly, emerging_technology_sectors).
narrative_ontology:constraint_victim(rare_earth_supply_monopoly, downstream_consumer_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT MANUFACTURERS (SNARE) — Technology companies requiring rare earth elements (magnets, phosphors, catalysts) face no practical exit. Substitution is technically infeasible at scale; geographic relocation does not solve supply chain dependency; price rationing ensures high cost of maintaining production. Trapped agents experience maximum effective extraction: prices rise, delivery terms tighten, strategic leverage is wielded through export restrictions and quota allocation.
constraint_indexing:constraint_classification(rare_earth_supply_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COORDINATED CONSUMER ALLIANCES (TANGLED ROPE) — Organized downstream actors (national governments, technology coalitions, defense ministries) benefit from genuine coordination: pooling rare earth procurement, negotiating collective contracts, and developing domestic reserves. But the constraint also extracts: the monopoly producer sustains higher prices through scarcity signaling; coordination costs (diplomatic overhead, stockpiling, R&D for substitutes) are borne collectively while extraction flows to the monopolist. Agents have constrained exit — they can invest in alternatives but at high cost.
constraint_indexing:constraint_classification(rare_earth_supply_monopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT CONTRACTED FIRMS (ROPE) — Established technology firms with long-term supply contracts perceive the monopoly as coordination: the producer guarantees supply in exchange for volume commitments. These firms experience low extraction because their contracts pre-date scarcity, lock in lower prices, and reduce uncertainty. The constraint solves their procurement problem. They have arbitrage options (storage, switching manufacturers) and benefit from the supplier's control preventing new competitors from accessing supply.
constraint_indexing:constraint_classification(rare_earth_supply_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: SUBSTITUTE TECHNOLOGY COALITION (SCAFFOLD) — Investment in non-rare-earth magnets (iron-nitride composites), phosphor substitutes (LED efficiency improvements), and rare-earth recycling represents a sunset mechanism. As these technologies mature (10-30 year horizon), rare earth dependency declines and the monopoly's extraction leverage diminishes. Current constraint extraction is high, but organized actors see a bounded time horizon for exit through technological substitution. Suppression is high but declining over time.
constraint_indexing:constraint_classification(rare_earth_supply_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GEOLOGICAL SUPPLY NARRATIVE (PITON) — The constraint is often justified through narratives of geological scarcity ('rare earths are rare,' 'China has 90% of deposits'). In reality, rare earths are not rare; deposits exist globally; production is concentrated through institutional arrangements (mining regulations, processing knowledge, market control), not geology. The 'geological inevitability' framing is theater (ratio ≈ 0.35, relatively low) — it performs scarcity while the real constraint is institutional. The constraint persists through inertia: the supply monopoly was established through prior market dominance, and alternative supply chains have not fully developed despite geological viability.
constraint_indexing:constraint_classification(rare_earth_supply_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, one might frame rare earth supply concentration as an immutable geopolitical law: some regions have deposits, others do not; processing capacity creates persistent asymmetry; defense needs create permanent strategic value. This perspective risks naturalizing what is actually a contingent institutional arrangement (mining regulations, processing monopolies, contract structures, recycling barriers). The engine's false summit detector will flag this as naturalization of a political choice, not a law of nature.
constraint_indexing:constraint_classification(rare_earth_supply_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_supply_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_supply_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_supply_monopoly, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_supply_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_supply_monopoly, TR),
    TR >= 0.70.

:- end_tests(rare_earth_supply_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The monopolist extracts economic surplus through price markup (2-10x above competitive costs in periods of high demand) and strategic allocation (rationing during supply constraints). The extraction is substantial but not maximal because: (1) some dependent manufacturers can adjust production (reduce output, switch to substitute materials, relocate), (2) geological deposits exist globally and could be developed, and (3) recycling and substitution technologies provide eventual escape routes. The intermediate value reflects that the trap is real but not absolute. Suppression (0.72): High. Barriers to exit include: (1) technical infeasibility of substitutes at current scale, (2) geographic relocation does not solve supply dependency, (3) processing knowledge and infrastructure are monopolized, (4) strategic export restrictions prevent alternative suppliers from developing, (5) incumbent firms' contracted supply creates first-mover advantage preventing new entrants. The high suppression reflects that most agents genuinely cannot exit without bearing extraordinary costs. Theater ratio (0.35): Relatively low. The extraction mechanism is direct — controlling supply, rationing allocation, pricing strategically. There is some theatrical narrative (geological scarcity, strategic importance, national security) but the constraint's power derives from functional control, not performance. Unlike many institutional constraints, rare earth monopoly does not rely heavily on myth-maintenance or legitimation theater.
 *
 * PERSPECTIVAL GAP:
 *   The dominant producer sees coordination (Rope at institutional/arbitrage) — they are solving their own supply chain problem and earning return on capital. Incumbent contracted firms see coordination (Rope at institutional/arbitrage) — their long-term contracts lock in favorable terms. The consumer alliance sees mixed benefit and burden (Tangled Rope at organized/constrained) — genuine coordination of collective procurement alongside extraction of higher prices and constrained exit. Dependent manufacturers see pure extraction (Snare at powerless/trapped) — they experience only the cost side with no coordination benefit. The substitute technology coalition sees temporary constraint (Scaffold at organized/constrained) — organized agents with a sunset pathway through R&D. The geological narrative sees immutable law (Mountain at analytical/analytical) — but this is a false summit; the constraint is institutional, not geological. The perspectival gap reveals how institutional monopoly is experienced as natural law by observers distant from the supply chain but as pure chokepoint extraction by those dependent on the resource.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's position in the extraction flow. The dominant producer state experiences low d (0.05-0.15) as a beneficiary with arbitrage options — they can exit by selling supply or maintain the constraint by restricting it. Dependent manufacturers experience high d (0.90-0.95) as victims trapped by material necessity — they cannot exit without radical technology change. Organized consumer alliances experience moderate d (0.55-0.65) as victims with constrained exit — they can invest in alternatives but face high costs and long timelines. The sigmoid f(d) maps these context positions to effective extractiveness: beneficiaries experience negative or near-zero chi; trapped agents experience high chi near the base extractiveness value; constrained agents experience moderate chi. The chi formula scales extractiveness by scope modifier σ(S): global scope (1.2) amplifies the extraction signal because the monopoly spans the entire manufacturing ecosystem.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through recognizing that the constraint is genuinely a snare at the operational level (dependent manufacturers experience extraction with minimal coordination benefit) while simultaneously being a mixed system at the strategic level (consumer coalitions coordinate around substitute technologies and supply diversification, creating a sunset pathway that transforms the snare into a scaffold over generational timescale). The false mountain perspective (geological inevitability) is correctly identified as naturalization of contingent arrangements. No single classification is wrong — all six are correct readings of different structural positions. The snare dominates because most actors in the system experience it as pure extraction; the scaffold sunset is real but requires organized, long-term investment to materialize. The constraint's future depends on whether substitute technologies reach adoption threshold before the monopolist's political leverage hardens the extraction into institutional permanence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geological_scarcity_vs_institutional_chokepoint,
    'Is rare earth supply concentration driven by geological rarity or by institutional arrangements (mining regulations, processing monopolies, recycling infrastructure)?',
    'Global deposit inventory analysis; historical cost curves for production from non-dominant sources; viability assessment of secondary recovery and recycling; comparison of processing costs across countries',
    'If geological: constraint approaches mountain classification — exit is infeasible. If institutional: constraint is snare/tangled rope with exit pathways through policy and investment. Classification shifts 0.3-0.5 in extractiveness depending on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geological_scarcity_vs_institutional_chokepoint, empirical, 'Whether scarcity is geological or institutional').

omega_variable(
    substitute_technology_timeline,
    'How many years until viable non-rare-earth substitutes (iron-nitride magnets, fluorescenceless phosphors, alternative catalysts) reach cost parity and adoption threshold?',
    'Technology roadmap analysis; manufacturing scale-up trajectories; cost reduction curves; market adoption rates for past substitute technologies; investment capital flowing to substitute R&D',
    'If timeline < 10 years: scaffold classification is accurate, sunset is near. If timeline > 30 years: scaffold classification is aspirational, constraint persists as snare for longer horizon. Affects measurement trajectory and theater_ratio trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitute_technology_timeline, empirical, 'Timeline for technological substitutes reaching viability').

omega_variable(
    recycling_infrastructure_feasibility,
    'Can rare earth recycling (from end-of-life electronics and industrial waste) reach 30-50% of total supply at costs below monopoly producer pricing?',
    'Pilot recycling programs; cost curves for different recovery methods; concentration of rare earths in end-of-life products; logistical barriers to collection and processing',
    'If feasible: creates secondary supply source that bypasses monopoly; reduces effective suppression and extraction. If infeasible: recycling remains theater (politically attractive but functionally marginal); monopoly extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_infrastructure_feasibility, empirical, 'Feasibility of recycling as alternative supply source').

omega_variable(
    strategic_vulnerability_political_will,
    'Will geopolitical competition drive investment in alternative supply chains despite higher current costs?',
    'Government commitments to domestic rare earth development; defense spending on supply security; industrial policy investments in substitute technologies; actual diversion of manufacturing to regions with alternative supply',
    'If strong political will: constraint softens as competing supply chains develop, extraction declines. If weak political will: incumbents accept higher costs rather than invest in alternatives, constraint persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_vulnerability_political_will, preference, 'Political willingness to invest in supply chain alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_supply_monopoly, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rares_tr_t0, rare_earth_supply_monopoly, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rares_tr_t10, rare_earth_supply_monopoly, theater_ratio, 10, 0.32).
narrative_ontology:measurement(rares_tr_t20, rare_earth_supply_monopoly, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(rares_be_t0, rare_earth_supply_monopoly, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rares_be_t10, rare_earth_supply_monopoly, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(rares_be_t20, rare_earth_supply_monopoly, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_supply_monopoly, resource_allocation).
narrative_ontology:boltzmann_floor_override(rare_earth_supply_monopoly, 0.18).
narrative_ontology:affects_constraint(rare_earth_supply_monopoly, semiconductor_supply_chain_dependency).
narrative_ontology:affects_constraint(rare_earth_supply_monopoly, green_energy_magnet_bottleneck).
narrative_ontology:affects_constraint(rare_earth_supply_monopoly, defense_industrial_geopolitical_leverage).

% DUAL FORMULATION NOTE:
% Rare earth supply monopoly decomposes into multiple structurally distinct constraints: (1) geological deposit distribution (mountain-like, minimal extractiveness), (2) processing monopoly establishment (snare, high extractiveness, institutional), (3) technology development barriers (tangled rope, institutional/strategic). This story focuses on constraint 2-3: the institutional monopoly and its strategic leverage. Upstream constraints (geological deposits) are stable; downstream constraints (technology supply chain dependencies) are contingent on rare earth availability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_supply_monopoly, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
