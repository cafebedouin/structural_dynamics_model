% ============================================================================
% CONSTRAINT STORY: grid_reserve_margin_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_grid_reserve_margin_collapse, []).

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
 *   constraint_id: grid_reserve_margin_collapse
 *   human_readable: Grid Reserve Margin Collapse and Capacity Market Extraction
 *   domain: energy_economics/grid_operations
 *
 * SUMMARY:
 *   Grid reserve margin collapse emerges at the intersection of two
 *   structural forces: (1) the rise of variable renewable generation, which
 *   reduces the dispatchable capacity available for balancing, and (2)
 *   incumbent generator capacity market designs that extract rent through
 *   artificially maintained reserve margin requirements. The constraint
 *   operates as tangled rope — it contains a genuine coordination problem
 *   (grids do require spare capacity) alongside asymmetric extraction
 *   (capacity market revenues flow disproportionately to incumbents while
 *   costs are socialized). The theater ratio (0.55) reflects that capacity
 *   market clearing processes, while elaborate, increasingly determine
 *   allocation of revenue to incumbents rather than serving genuine
 *   reliability functions. Reserve margins have risen from 15% (adequate for
 *   coal-dominated grids) to 22-25% in some regions, driven more by market
 *   rules than by physical requirements. Retail consumers bear the cost
 *   through reliability charges in their electricity bills; renewable
 *   generators face discriminatory resource qualification rules; the commons
 *   bears the risk of inadequate alternative pathways if incumbents exit. The
 *   grid modernization coalition perceives a sunset: distributed storage,
 *   demand response, and fast-ramp resources are maturing. Within 10-15
 *   years, the dependence on slow-ramp synchronous generators as the primary
 *   reserve mechanism will decline, making current capacity market extraction
 *   unsustainable.
 *
 * KEY AGENTS:
 *   - Incumbent Coal and Nuclear Generators: Primary beneficiary (institutional/arbitrage) — receive guaranteed capacity revenues and revenue floors; highest bargaining power in market design
 *   - Grid Stability Commons: Primary victim (powerless/trapped) — abstract collective good bearing full cost of margin compression and extraction
 *   - Retail Consumers and Small Operators: Secondary victim (moderate/constrained) — pay reliability charges and face demand-response mandates; cannot exit grid
 *   - Renewable Generators and Distributed Resources: Secondary victim (powerful/mobile) — structurally mobile but face extraction through technical discrimination and reserve margin rules designed for synchronous generators
 *   - Grid Modernization Coalition: Organized agents (organized/constrained) — regulators, battery storage firms, demand-response aggregators building alternative pathways with 10-15 year sunset
 *   - Legacy Capacity Market Operators: Institutional actor (institutional/arbitrage) — maintain revenue-allocation mechanisms that serve incumbents more than grid stability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement (capacity markets) as physical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grid_reserve_margin_collapse, 0.58).
domain_priors:suppression_score(grid_reserve_margin_collapse, 0.68).
domain_priors:theater_ratio(grid_reserve_margin_collapse, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grid_reserve_margin_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(grid_reserve_margin_collapse, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(grid_reserve_margin_collapse, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grid_reserve_margin_collapse, tangled_rope).
narrative_ontology:human_readable(grid_reserve_margin_collapse, "Grid Reserve Margin Collapse and Capacity Market Extraction").
narrative_ontology:topic_domain(grid_reserve_margin_collapse, "energy_economics/grid_operations").

domain_priors:requires_active_enforcement(grid_reserve_margin_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grid_reserve_margin_collapse, incumbent_generators).
narrative_ontology:constraint_beneficiary(grid_reserve_margin_collapse, capacity_market_operators).
narrative_ontology:constraint_victim(grid_reserve_margin_collapse, grid_stability_commons).
narrative_ontology:constraint_victim(grid_reserve_margin_collapse, retail_consumers).
narrative_ontology:constraint_victim(grid_reserve_margin_collapse, renewable_generators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRID STABILITY COMMONS (SNARE) — The abstract collective good of grid stability cannot exit or organize. As reserve margins compress, the cost of maintaining reliability (increasingly expensive capacity payments, demand-response mandates, blackout risk) falls on the commons with no mechanism for collective action. Trapped in dependency on whatever capacity mechanism incumbent operators control.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RETAIL CONSUMERS AND SMALL OPERATORS (TANGLED ROPE) — Face both coordination benefits (the capacity market ensures supply) and extraction (rising reliability charges embedded in electricity prices). Exit options are severely constrained: cannot unilaterally source power, cannot opt out of grid participation, but retain some agency through demand response or distributed generation. Mixed experience of benefit and burden.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT COAL AND NUCLEAR GENERATORS (ROPE) — Primary beneficiaries. Experience the constraint as coordination (capacity markets guarantee payments for maintaining reserve margin). High arbitrage options: can bid strategically, mothball assets temporarily, or switch fuels. Net beneficiary — extraction runs toward this agent through guaranteed capacity revenues and revenue floors.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RENEWABLE GENERATORS AND DISTRIBUTED RESOURCES (SNARE) — Structurally mobile (can exit grid or markets) but face high extraction through capacity market design that excludes or penalizes variable resources. Reserve margin collapse is instrumentalized to justify capacity rules that favor slow-ramp generators. Powerful agents but captured in a snare because market rules extract via technical discrimination despite mobility.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: GRID MODERNIZATION COALITION (SCAFFOLD) — Organized actors (regulators, technology firms, demand-response aggregators) perceive the reserve margin collapse as a temporary coordination failure solvable through grid modernization: fast-ramp resources, storage, advanced forecasting, and demand flexibility. See clear sunset: as these technologies mature and regulatory frameworks evolve, the dependence on large synchronous generators declines. Suppression is declining as alternatives mature.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: LEGACY CAPACITY MARKET STRUCTURE (PITON) — The original capacity market mechanism (designed in 2003-2007 for a coal-dominated grid) persists through institutional inertia despite declining relevance. Theater ratio (0.55) reflects that elaborate bidding and clearing processes now largely determine allocation of revenue to incumbents rather than ensuring genuine reliability. The mechanism's primary function has atrophied, but regulatory inertia maintains it.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL CONSTRAINT VIEW (MOUNTAIN) — From a universal/civilizational perspective, reserve margins are a physical necessity: grids must maintain spare capacity to respond to unexpected failures and demand spikes. This perspective treats reserve margin maintenance as an immutable law of grid physics. However, the structural data reveals this as a false summit: the 'collapse' is not physical but institutional — it reflects market design choices (capacity markets favoring incumbents) and regulatory path dependence, not laws of physics.
constraint_indexing:constraint_classification(grid_reserve_margin_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grid_reserve_margin_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(grid_reserve_margin_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grid_reserve_margin_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(grid_reserve_margin_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(grid_reserve_margin_collapse, TR),
    TR >= 0.70.

:- end_tests(grid_reserve_margin_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. The constraint combines legitimate coordination costs (maintaining reserve capacity) with extractive premium (capacity market rules that set reserve margins above physical necessity and that discriminate against renewable resources). The trajectory shows extractiveness rising from 0.28 to 0.58 over the interval, reflecting that incumbent generators have successfully lobbied for tighter reserve margin definitions and stricter qualification rules, while alternative capacity sources have been systematically excluded. The rise is institutional, not physical. Suppression (0.68): High. Multiple barriers prevent exit: grid participation is unavoidable; capacity market rules create high switching costs; regulatory uncertainty deters investment in alternatives; incumbent generators have incumbent advantage in regulatory proceedings. Suppression is declining as storage technology matures and regulatory frameworks shift, but currently remains high. Theater ratio (0.55): Moderate. Capacity market auctions and clearing processes are elaborate and appear technical, but increasingly serve revenue allocation to incumbents rather than genuine reliability verification. The theater has risen from 0.35 (early years when capacity markets were novel) to 0.55 (current state where outcomes are increasingly predictable) as the mechanism has aged and incumbent dominance has solidified. Claimed type (tangled_rope): Required because the constraint contains both genuine coordination (reserve margins are necessary) and asymmetric extraction (market rules favor incumbents). The coordination function is real but instrumentalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the need for spare grid capacity — appears as a natural law (mountain), a pure extraction mechanism (snare), a coordination problem with alternatives (scaffold), a degraded ritual (piton), a mixed mechanism (tangled rope), a coordination system (rope), and a mixed burden (tangled rope from consumer perspective). The gaps are sharpest between the incumbent beneficiary (rope — they experience genuine coordination and revenue certainty) and the trapped commons (snare — abstract collective good bearing costs with no exit). The renewable generators occupy an intermediate position (snare from a powerful perspective — they could exit but market rules extract through discrimination). The grid modernization coalition disagrees with the analytical observer's mountain view: the coalition sees institutions changing, while the observer risks naturalizing those institutions as immutable. The critical gap is between perspectives that see reserve margin requirements as physical law (mountain, piton) versus those that see them as institutional and changeable (scaffold, tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents with arbitrage options (can delay retirements, mothball assets, participate in multiple markets) experience low effective extraction d ≈ 0.15-0.25. Their exit options are high, their beneficiary status is clear, and they face no suppression — they drive the rules. Consumers with trapped/constrained options (cannot exit grid, cannot coordinate collective negotiation, face regulatory capture) experience high extraction d ≈ 0.85-0.95. Renewable generators with mobile options (can build outside the grid or in other regions) but facing market discrimination experience intermediate extraction d ≈ 0.65-0.75: structurally mobile but extraction vectors target them specifically. The commons has no agent to hold d — it is represented through the snare perspective where the powerless/trapped agent absorbs all costs. The piton perspective's institutional actor has low suppression (operators face no barriers to changing rules) but maintains the status quo through inertia, yielding moderate d. The scaffold coalition's organized/constrained position (have exit paths but face regulatory inertia) yields d ≈ 0.45-0.55.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mislabeling through the tangled_rope classification, which requires BOTH beneficiaries (coordinate the reserve margin function) AND victims (bear extraction costs through market rules). Pure extraction (snare) would understate the genuine coordination value of reserve margins. Pure coordination (rope) would understate the extractive premium capacity markets impose. Tangled rope correctly identifies that the grid has real coordination needs (spare capacity is necessary) but that incumbent generators exploit this need through market design that forces higher margins, discriminates against alternatives, and socializes costs while privatizing benefits. The mandatrophy is resolved by showing that the classification type depends on whether the observer focuses on the coordination problem (legitimate) or the market design solution (extractive): from the beneficiary's view (rope), the mechanism coordinates supply. From the victim's view (snare), it extracts. From the analytical view (tangled rope), it does both, and the institutional design problem is that the extraction exceeds the coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_margin_adequacy_threshold,
    'What reserve margin percentage is physically necessary vs. what margin percentage is institutionally/economically constructed by incumbent capacity rules?',
    'Historical analysis of grid reliability: correlation between reserve margin levels and actual blackout/failure rates across regions with different capacity market designs; engineering studies of minimum spinning reserve requirements',
    'If physical minimum is 15%: current rules (22%+) are extractive premium. If minimum is 25%: current rules are insufficient and collapse is genuine. If minimum varies by technology mix: the problem is not reserve margin collapse but misclassification of resource types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_margin_adequacy_threshold, empirical, 'Physical vs. institutional reserve margin threshold').

omega_variable(
    fast_ramp_sufficiency,
    'Can fast-ramp resources (batteries, demand response, solar+storage) provide grid stability functions previously monopolized by synchronous generators, or are slow-ramp generators still physically irreplaceable?',
    'Engineering studies of frequency response, voltage stability, and inertial support requirements; empirical testing in grids with high variable renewable penetration (Texas, California, Ireland); simulation of stability margins with 70%+ renewable penetration',
    'If fast-ramp sufficient: the sunset perspective is correct and extraction is temporary. If slow-ramp irreplaceable: the snare perspective is correct and collapse is structural. This determination drives the classification between Scaffold and Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fast_ramp_sufficiency, empirical, 'Whether fast-ramp resources can replace synchronous generator functions').

omega_variable(
    capacity_market_design_intentionality,
    'Do incumbent generators consciously exploit capacity market design to extract rent, or is the extraction a side effect of well-intentioned but maladapted regulatory structures?',
    'Analysis of capacity market design proposals and lobbying positions; comparison of outcomes under different bidding rules; examination of generator behavior when rules were neutral vs. favorable',
    'If conscious exploitation: pure snare. If unintended side effect: tangled rope. If externally mandated: rope. This affects whether classification should emphasize intentional extraction or institutional coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_market_design_intentionality, empirical, 'Whether capacity market extraction is intentional or structural').

omega_variable(
    transmission_constraints_vs_generation_constraints,
    'Are reserve margins actually driven by generation adequacy or by transmission bottlenecks that prevent moving cheap power from one region to another?',
    'Network analysis of actual failure modes; comparison of reserve margin requirements in well-connected vs. isolated grids; simulation of requirements if transmission capacity were unlimited',
    'If transmission-constrained: the extraction vector is different (transmission operators vs consumers). If generation-constrained: current analysis is correct. The constraint identity itself may be misidentified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_constraints_vs_generation_constraints, empirical, 'Whether reserve margins are driven by generation or transmission limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grid_reserve_margin_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(grmc_tr_t0, grid_reserve_margin_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(grmc_tr_t5, grid_reserve_margin_collapse, theater_ratio, 5, 0.45).
narrative_ontology:measurement(grmc_tr_t10, grid_reserve_margin_collapse, theater_ratio, 10, 0.55).
narrative_ontology:measurement(grmc_tr_t3, grid_reserve_margin_collapse, theater_ratio, 3, 0.4).
narrative_ontology:measurement(grmc_tr_t7, grid_reserve_margin_collapse, theater_ratio, 7, 0.5).

% Extraction over time
narrative_ontology:measurement(grmc_be_t0, grid_reserve_margin_collapse, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(grmc_be_t5, grid_reserve_margin_collapse, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(grmc_be_t10, grid_reserve_margin_collapse, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(grmc_be_t2, grid_reserve_margin_collapse, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(grmc_be_t8, grid_reserve_margin_collapse, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grid_reserve_margin_collapse, resource_allocation).
narrative_ontology:boltzmann_floor_override(grid_reserve_margin_collapse, 0.18).
narrative_ontology:affects_constraint(grid_reserve_margin_collapse, renewable_integration_cost_shifting).
narrative_ontology:affects_constraint(grid_reserve_margin_collapse, frequency_stability_degradation).
narrative_ontology:affects_constraint(grid_reserve_margin_collapse, capacity_payment_rent_seeking).

% DUAL FORMULATION NOTE:
% Grid reserve margin collapse decomposes into three linked structural constraints: (1) the physics of grid stability (reserve margins are necessary — low ε), (2) the market design choice to maintain high margins (extractive — high ε), (3) the discrimination against renewable resources in capacity qualification (extractive — high ε). This story addresses the middle level (market design extraction). The upstream physics constraint and downstream renewable discrimination constraint are separate stories in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(grid_reserve_margin_collapse, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
