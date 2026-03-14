% ============================================================================
% CONSTRAINT STORY: lithium_battery_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lithium_battery_dependency, []).

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
 *   constraint_id: lithium_battery_dependency
 *   human_readable: Global Lithium Battery Supply Chain Dependency
 *   domain: economic_infrastructure/energy
 *
 * SUMMARY:
 *   Global energy transition has created a structural dependency on
 *   lithium-ion batteries for electric vehicles, grid storage, and renewable
 *   energy integration. This dependency exhibits the classic characteristics
 *   of a tangled rope constraint: it solves a genuine coordination problem
 *   (securing stable supply of the energy storage medium for global
 *   electrification) while simultaneously extracting from nations and
 *   communities that depend on energy transition but lack alternatives.
 *   Lithium's geographic concentration (60% of proven reserves in the
 *   'Lithium Triangle' of Argentina, Bolivia, and Chile) creates supply
 *   chokepoints. Water depletion from mining operations threatens agriculture
 *   and community viability in arid regions. Yet alternative battery
 *   chemistries (sodium-ion, solid-state, lithium-iron-phosphate) are
 *   mattering as genuine exit paths, giving the constraint scaffold
 *   properties on a 10-20 year horizon. The constraint's theater ratio
 *   remains moderate (0.48) because the technical coordination function is
 *   genuine — battery supply chains require real logistical integration — but
 *   standardization regimes and incumbent advantages add performative
 *   overhead. Extractiveness has risen from 0.35 to 0.58 over the decade as
 *   demand has concentrated supply bottlenecks and mining regions have faced
 *   intensifying water stress.
 *
 * KEY AGENTS:
 *   - Energy Transition Dependent Economies: Primary victim (powerless/trapped) — committed to net-zero targets; cannot exit lithium dependency without abandoning decarbonization or waiting for alternative technologies
 *   - Lithium Extracting Nations (Argentina, Bolivia, Chile): Primary beneficiary (institutional/arbitrage) — control geographic bottleneck; capture significant rents through supply coordination and pricing power
 *   - Battery Manufacturers: Secondary beneficiary (institutional/arbitrage) — benefit from stable supply coordination; have arbitrage options through vertical integration or alternative sourcing
 *   - Mining-Dependent Communities: Secondary victim (moderate/constrained) — experience genuine employment and infrastructure benefits alongside water depletion and environmental degradation
 *   - Alternative Chemistry Developers: Organized agents (organized/constrained) — see clear technological sunset pathway; building exit alternatives over generational timescale
 *   - Standardization Bodies: Institutional actors (institutional/arbitrage) — maintain battery standards optimized for lithium-ion; function partly performatively for emerging alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — can identify both genuine physical constraints and contingent political-economic structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lithium_battery_dependency, 0.58).
domain_priors:suppression_score(lithium_battery_dependency, 0.62).
domain_priors:theater_ratio(lithium_battery_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lithium_battery_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(lithium_battery_dependency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lithium_battery_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lithium_battery_dependency, tangled_rope).
narrative_ontology:human_readable(lithium_battery_dependency, "Global Lithium Battery Supply Chain Dependency").
narrative_ontology:topic_domain(lithium_battery_dependency, "economic_infrastructure/energy").

domain_priors:requires_active_enforcement(lithium_battery_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lithium_battery_dependency, lithium_extracting_nations).
narrative_ontology:constraint_beneficiary(lithium_battery_dependency, battery_manufacturers).
narrative_ontology:constraint_beneficiary(lithium_battery_dependency, electric_vehicle_producers).
narrative_ontology:constraint_victim(lithium_battery_dependency, energy_transition_dependent_economies).
narrative_ontology:constraint_victim(lithium_battery_dependency, cobalt_nickel_mining_communities).
narrative_ontology:constraint_victim(lithium_battery_dependency, competing_battery_technologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY TRANSITION DEPENDENT NATIONS (SNARE) — Nations committed to decarbonization have no structural exit from lithium dependency. Commitment to net-zero mandates requires battery electric vehicles; manufacturing capacity requires lithium-ion batteries; no alternative chemistry has achieved cost/energy density parity. Suppression is total: geological concentration of lithium reserves (60% in Argentina, Bolivia, Chile), political control by mining interests, and technical lock-in via decades of optimization around lithium-ion chemistry. Maximum experienced extraction with no exit path.
constraint_indexing:constraint_classification(lithium_battery_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINING-DEPENDENT COMMUNITIES (TANGLED ROPE) — Communities in lithium-producing regions experience genuine coordination through employment and infrastructure investment, alongside asymmetric extraction via water depletion, environmental degradation, and resource curse dynamics. They cannot easily exit (economic dependency) but also receive benefits (wages, services). Suppression is high (few alternative economic pathways) but not total (some exit through migration or economic diversification).
constraint_indexing:constraint_classification(lithium_battery_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BATTERY MANUFACTURERS (ROPE) — Experience the constraint as a pure coordination problem: securing lithium supply requires investment in processing capacity, refining relationships with mining entities, and standardizing battery specifications. They have arbitrage options (alternative suppliers, strategic reserves, vertical integration). The constraint solves a genuine collective action problem — without lithium supply coordination, battery production would collapse. Net beneficiary with real agency.
constraint_indexing:constraint_classification(lithium_battery_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LITHIUM-PRODUCING NATIONS (ROPE) — See the constraint as coordination that benefits them: organizing extraction, controlling supply, negotiating pricing power. High arbitrage options (can diversify markets, create processing capacity, invest in value-added production). The constraint creates rents they can capture. Not extraction FROM them, but a mechanism through which they extract FROM others.
constraint_indexing:constraint_classification(lithium_battery_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE BATTERY TECHNOLOGY DEVELOPERS (SCAFFOLD) — Sodium-ion, solid-state, and lithium-iron-phosphate alternatives are emerging as genuine exit paths from lithium dependency. These technologies have sunset logic: as they mature and cost curves decline, lithium's dominance weakens. Developers face significant suppression (incumbent advantages, switching costs, capital requirements) but see a clear technological pathway with a defined horizon (10-20 years). The constraint is temporary from this view.
constraint_indexing:constraint_classification(lithium_battery_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STANDARDIZATION INSTITUTIONS (PITON) — Battery standards bodies (IEC, ASTM) have optimized extensively around lithium-ion chemistry over 20+ years. The standards are now so entrenched that they function largely performatively — testing protocols, safety requirements, and compatibility measures are designed for and assume lithium-ion. Alternative chemistries must prove themselves within a testing framework built for the incumbent. The standards persist through institutional inertia despite lower actual verification function for emerging chemistries. Theater ratio is high; extractiveness modest but supported by structural lock-in.
constraint_indexing:constraint_classification(lithium_battery_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: GEOLOGICAL/ANALYTICAL VIEW (MOUNTAIN) — Energy density requirements for portable energy storage encounter hard physical limits on battery chemistry diversity. Lithium's unique combination of low density, high electrochemical potential, and stability is not easily replicated. From a sufficiently abstract view, some dependence on concentrated-source battery chemistry is inherent to portable energy density physics. However, this naturalizes what is actually a choice: we could distribute energy production and storage differently, use alternative energy carriers, or invest in genuinely diverse battery chemistry pathways. The mountain classification is a false summit — the geological constraint explains why lithium is useful, not why we must be dependent on it.
constraint_indexing:constraint_classification(lithium_battery_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lithium_battery_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lithium_battery_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lithium_battery_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lithium_battery_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lithium_battery_dependency, TR),
    TR >= 0.70.

:- end_tests(lithium_battery_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. At t=0, lithium dependency was a genuine coordination mechanism with modest extraction (0.35) — a novel energy storage technology connecting willing suppliers and buyers. As demand concentrated (electric vehicle deployment accelerated globally), supply constraints tightened and extractiveness rose. By t=10, the constraint shows clear asymmetric extraction: energy transition dependent nations must pay increasing prices, bear supply volatility, and accept environmental costs of mining, while having no structural exit. But extractiveness is not snare-level (0.66+) because: (a) alternative technologies are genuinely emerging, (b) recycling pathways exist, and (c) producers have some strategic options (reserves, efficiency improvements, demand management). Suppression (0.62): High. Multiple barriers prevent exit: technical lock-in (lithium-ion optimization is deeply embedded in automotive and grid platforms), capital requirements for alternative chemistries, political control of supply by mining-exporting nations, and the absence of rapid substitutes at comparable cost/performance. Environmental costs of mining (water depletion, toxic tailings) are borne by mining communities with limited agency. Theater ratio (0.48): Moderate. The coordination function is genuine — battery supply chains require real logistical integration, standardization, and quality control. Theater is not high because technical requirements are substantive, not performative. However, standardization for alternative chemistries adds performative overhead, and 'securing reliable supply' rhetoric sometimes masks rent-extraction dynamics.
 *
 * PERSPECTIVAL GAP:
 *   Energy-transition dependent nations experience maximum extraction (snare-level) because they have no exit from electrification commitment and face rising supply costs with concentrated suppliers. Lithium-producing nations experience rope-level coordination benefits because they control a chokepoint and can capture rents. Battery manufacturers occupy intermediate ground (tangled rope to rope) depending on their supply diversification and backward integration. Mining communities occupy tangled rope precisely because they receive genuine economic benefits from employment and infrastructure while bearing environmental and water stress costs. The gap between the powerless/trapped perspective (snare) and the institutional/arbitrage perspective (rope) is the diagnostic signature of extraction: the same structural constraint appears as pure coordination to the beneficiary and pure coercion to the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the constraint. Lithium-producing nations have arbitrage options (can choose extraction rates, invest in processing, diversify markets) and receive benefits, yielding low d (near 0.1-0.2) and negative or minimal χ. Battery manufacturers have mobile options (multiple suppliers, backward integration) and receive genuine coordination benefits, yielding moderate d (near 0.4-0.5) and moderate χ. Energy-transition dependent economies are trapped by commitment to electrification targets and face rising prices with no exit, yielding high d (near 0.85-0.90) and high χ. Mining-dependent communities are constrained (have some exit options through migration or economic diversification) but have few alternatives, yielding moderately high d (near 0.65-0.75) and moderately high χ. The piton institutional perspective (standardization bodies) receives arbitrage-level options and modest benefits, yielding low d similar to beneficiaries, but theater ratio is what drives the piton classification, not χ.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE VALIDATION: The constraint satisfies all three tangled rope gates. (1) Genuine coordination function: battery supply integration is a real collective action problem — without coordinating lithium sourcing, refining, and distribution, global battery production cannot scale. (2) Asymmetric extraction: energy-transition dependent economies bear disproportionate costs (volatile supply, rising prices, environmental externalities of mining) relative to supply-controlling nations and manufacturers. (3) Active enforcement: requires ongoing mining investment, supply contracts, trade agreements, and standardization regimes. SCAFFOLD COMPONENT: The constraint has secondary scaffold properties because alternative battery chemistries (sodium-ion, solid-state) represent genuine technological exits with sunset logic on a 10-20 year horizon. As these mature, lithium's dependency mechanism weakens. The classification is Tangled Rope at the current temporal scale, but Scaffold is embedded in the forward trajectory. PITON RISK: Standardization regimes and incumbent advantage create risk that lithium optimization becomes inertial — the constraint persists not because it solves problems but because alternatives are delayed by the friction of switching costs. The theater ratio (0.48) is moderate; if it rises above 0.70, the classification shifts toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_chemistry_timeline,
    'What is the realistic timeline for sodium-ion, solid-state, or other alternative chemistries to achieve cost/energy density parity with lithium-ion?',
    'Tracking manufacturing cost curves, energy density improvements, and production scale-up rates for alternative technologies; market penetration analysis in non-automotive and automotive segments',
    'If timeline < 5 years: scaffold perspective dominates and sunset clause is imminent, classification shifts to temporary constraint. If timeline > 20 years: lithium dependency becomes effectively permanent for multiple human lifespans.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_chemistry_timeline, empirical, 'Timeline for technological alternatives to reach parity').

omega_variable(
    supply_concentration_mechanism,
    'Is lithium supply concentration (Argentina, Bolivia, Chile) a geological accident or the result of deliberate coordination and political economy?',
    'Historical analysis of mining investment patterns, trade agreements, and supply chain consolidation; identification of points where alternative sources were deprioritized or abandoned',
    'If geological: dependency is partially natural law (mountain). If political economy: dependency is structurally constructed (snare). Most likely: combination, but the proportion determines how much extraction is inherent vs contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_concentration_mechanism, empirical, 'Whether lithium concentration is geological or political').

omega_variable(
    water_depletion_feedback,
    'At what rate of lithium extraction does water depletion in mining regions cause economic collapse of surrounding communities, triggering forced reduction in supply?',
    'Hydrological monitoring in mining regions (Atacama, Argentina salt flats); economic modeling of agriculture and community viability under water stress; historical precedent analysis from other extractive industries',
    'If feedback occurs within 10 years: suppression mechanism becomes self-limiting and constraint may shift from snare to tangled_rope. If > 20 years: extraction can continue unchecked, snare classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(water_depletion_feedback, empirical, 'Rate of water depletion feedback in mining regions').

omega_variable(
    strategic_reserve_effectiveness,
    'Can government strategic reserves (lithium stockpiles, refining capacity) actually reduce dependency, or do they merely delay extraction without changing its fundamental structure?',
    'Simulation and historical analysis of reserve depletion rates under various demand scenarios; assessment of whether reserves reduce supply concentration or merely shift when extraction occurs',
    'If effective: suppression can be reduced via policy, and tangled_rope classification is appropriate. If ineffective: structural dependency remains snare-level regardless of reserve policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_reserve_effectiveness, empirical, 'Whether strategic reserves reduce dependency or delay extraction').

omega_variable(
    recycling_viability,
    'Can lithium recycling from spent batteries achieve sufficient recovery rates and cost curves to materially reduce primary mining dependency within a 20-year horizon?',
    'Tracking recycling technology development, cost curves, and recovery rates; modeling closed-loop battery supply under high recycling penetration; assessment of scaling barriers',
    'If yes: secondary sources can reduce primary extraction pressure, shifting from snare toward tangled_rope. If no: primary mining dependency persists as structural constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recycling_viability, empirical, 'Viability of recycling-based supply alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lithium_battery_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lithium_tr_t0, lithium_battery_dependency, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lithium_tr_t5, lithium_battery_dependency, theater_ratio, 5, 0.4).
narrative_ontology:measurement(lithium_tr_t10, lithium_battery_dependency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(lithium_tr_t2, lithium_battery_dependency, theater_ratio, 2, 0.36).
narrative_ontology:measurement(lithium_tr_t7, lithium_battery_dependency, theater_ratio, 7, 0.44).

% Extraction over time
narrative_ontology:measurement(lithium_be_t0, lithium_battery_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lithium_be_t5, lithium_battery_dependency, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lithium_be_t10, lithium_battery_dependency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lithium_be_t2, lithium_battery_dependency, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(lithium_be_t7, lithium_battery_dependency, base_extractiveness, 7, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lithium_battery_dependency, resource_allocation).
narrative_ontology:affects_constraint(lithium_battery_dependency, electric_vehicle_supply_chain).
narrative_ontology:affects_constraint(lithium_battery_dependency, renewable_energy_grid_storage).
narrative_ontology:affects_constraint(lithium_battery_dependency, mining_water_depletion).
narrative_ontology:affects_constraint(lithium_battery_dependency, battery_standardization_regime).

% DUAL FORMULATION NOTE:
% Lithium battery dependency decomposes structurally into three distinct constraints with different ε values: (1) lithium_supply_bottleneck (ε=0.58, this story) addresses the asymmetric access to proven reserves; (2) battery_chemistry_lock_in (ε=0.42, separate story) addresses incumbent advantage in standardization and optimization; (3) mining_environmental_externalities (ε=0.65, separate story) addresses the transfer of extraction costs to mining communities. The upstream constraint is supply bottleneck; downstream constraints are technology lock-in and environmental debt. All three link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lithium_battery_dependency, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
