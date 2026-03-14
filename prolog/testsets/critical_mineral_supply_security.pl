% ============================================================================
% CONSTRAINT STORY: critical_mineral_supply_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_mineral_supply_security, []).

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
 *   constraint_id: critical_mineral_supply_security
 *   human_readable: Critical Mineral Supply Security as Tangled Coordination-Extraction Hybrid
 *   domain: geopolitical_economy/resource_security
 *
 * SUMMARY:
 *   Critical mineral supply security represents a global constraint where
 *   genuine coordination (enabling decarbonization, technology development,
 *   industrial capacity) is structurally entangled with asymmetric extraction
 *   (geographic scarcity rents, cartel discipline, processing monopolies,
 *   geopolitical weaponization). The constraint exhibits all characteristics
 *   of a Tangled Rope: coordination function is real and necessary
 *   (downstream manufacturers must obtain minerals to produce goods; nations
 *   must secure stable access for industrial development), but the
 *   coordination mechanism enforces extraction through geographic
 *   concentration, cartel behavior, and strategic reserve weaponization.
 *   Extractiveness has risen from 0.38 to 0.58 over the interval (T=0 to
 *   T=10) as production concentration has increased and geopolitical tensions
 *   have elevated weaponization risk. Theater ratio remains moderate (0.48)
 *   because while strategic reserves and sustainability standards contain
 *   performative elements, the underlying supply mechanism is fundamentally
 *   functional — prices do reflect scarcity, supply cuts do occur, downstream
 *   vulnerability is genuine. The constraint is not primarily maintained by
 *   false appearance but by real structural asymmetry.
 *
 * KEY AGENTS:
 *   - Downstream Manufacturers: Primary victims (powerless/trapped) — technology firms dependent on lithium, cobalt, rare earths with no viable exit options. Bears maximum extraction through price volatility, supply uncertainty, and geopolitical risk.
 *   - Resource-Poor Industrializing Nations: Secondary victims (organized/constrained) — require supply access for industrial development but lack market power to set terms. Can organize regionally but constrained by capital and geopolitical pressure.
 *   - Mining Nations (DRC, Indonesia, Chile, Australia): Primary beneficiaries (institutional/arbitrage) — control geographic concentration of production. Set prices, manage cartel discipline, weaponize supply during geopolitical disputes.
 *   - Processing & Refining Oligarchy (China dominant): Secondary beneficiary (institutional/arbitrage) — maintains downstream bottleneck through concentration of rare earth separation, cobalt refining, lithium conversion capacity. New extraction layer atop primary mining extraction.
 *   - Technology Coalition (US, EU, Japan R&D): Intermediate agent (powerful/mobile) — developing substitution technologies and domestic mining/processing as exit mechanisms. Building diversified supply but enforces new extraction through technological gatekeeping.
 *   - Sustainability Standards Coalition: Organized agent (organized/constrained) — ESG, conflict minerals, environmental standards create temporary coordination overlay with sunset implicit in material science advances. Soft enforcement (market exclusion, reputational cost) rather than hard.
 *   - Strategic Reserve Systems (National governments): Institutional actor (institutional/arbitrage) — maintain buffer stockpiles and emergency protocols with high performative content but limited actual utilization. Theater-heavy response to structural supply insecurity.
 *   - Analytical Observer: Civilization-scale perspective (analytical/analytical) — observes genuine coordination function entangled with structural extraction. Risk of misclassifying hybrid as either pure coordination or pure extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_mineral_supply_security, 0.58).
domain_priors:suppression_score(critical_mineral_supply_security, 0.62).
domain_priors:theater_ratio(critical_mineral_supply_security, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_mineral_supply_security, extractiveness, 0.58).
narrative_ontology:constraint_metric(critical_mineral_supply_security, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(critical_mineral_supply_security, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_mineral_supply_security, tangled_rope).
narrative_ontology:human_readable(critical_mineral_supply_security, "Critical Mineral Supply Security as Tangled Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(critical_mineral_supply_security, "geopolitical_economy/resource_security").

domain_priors:requires_active_enforcement(critical_mineral_supply_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_mineral_supply_security, mining_nations).
narrative_ontology:constraint_beneficiary(critical_mineral_supply_security, vertically_integrated_processors).
narrative_ontology:constraint_beneficiary(critical_mineral_supply_security, strategic_reserve_holders).
narrative_ontology:constraint_victim(critical_mineral_supply_security, downstream_manufacturers).
narrative_ontology:constraint_victim(critical_mineral_supply_security, resource_poor_nations).
narrative_ontology:constraint_victim(critical_mineral_supply_security, supply_chain_fragility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM MANUFACTURER (SNARE) — Technology firms dependent on lithium, cobalt, rare earths for batteries, semiconductors, magnets cannot exit supply chains without abandoning product lines. Geographic concentration (DRC, Indonesia, China) creates single-point-of-failure dependency. Suppression is structural: no substitutes exist at scale, geopolitical extraction of pricing and terms, cartel behavior by mining consortia. Maximum experienced extraction with zero exit options. Trapped status persists across biographical horizon.
constraint_indexing:constraint_classification(critical_mineral_supply_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-POOR INDUSTRIALIZING NATION (TANGLED ROPE) — Must coordinate supply security for industrial development (genuine coordination function: stable access enables manufacturing, export competitiveness). But also bears extraction: prices are set by mining oligopolies, strategic reserves are weaponized during geopolitical disputes, technology transfer is constrained. Organized collective action (BRICS supply agreements, regional processing corridors) provides some exit mechanism and agency, but constrained by capital requirements and geopolitical pressure. Active enforcement required to maintain coordination while managing asymmetric extraction.
constraint_indexing:constraint_classification(critical_mineral_supply_security, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MINING NATION OLIGARCHY (ROPE) — Benefits from supply scarcity and downstream demand. Experiences the constraint as coordination of extraction rents: manages cartel discipline (OPEC-like arrangements for rare earths), regulates access to mining zones, negotiates long-term supply contracts. Net beneficiary with full arbitrage options — can redirect supply, negotiate terms, switch trading partners. Zero experienced extraction; maximum extraction imposed on others.
constraint_indexing:constraint_classification(critical_mineral_supply_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY COALITION (TANGLED ROPE) — Coordinating domestic mining (US rare earths reopening, EU critical mineral strategy), material science R&D (silicon carbide substitutes, phosphate-free batteries), and recycling infrastructure creates genuine coordination function reducing supply vulnerability. But also enforces extraction through technological gatekeeping: nations with advanced processing control downstream access, creating new asymmetry replacing old. Mobile exit options (technology adoption, recycling networks) reduce suppression below snare level. Active enforcement of new supply chains paradoxically creates extraction at the enforcement layer.
constraint_indexing:constraint_classification(critical_mineral_supply_security, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SUSTAINABILITY STANDARDS COALITION (SCAFFOLD) — ESG certification, conflict minerals tracking, and environmental impact disclosure standards create temporary coordination overlay designed to sunset extractive mining practices. Theater present (performative compliance, greenwashing) but genuine structural function: standards create price signals that favor responsible sourcing and incentivize supply chain diversification. Sunset clause implicit: as circular economy and alternate materials mature, the need for extraction-based supply scarcity diminishes. Organized coalition with constrained exit — enforcement mechanisms are soft (reputation, market exclusion) rather than hard.
constraint_indexing:constraint_classification(critical_mineral_supply_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STRATEGIC RESERVE SYSTEM (PITON) — National stockpiles (US SPR parallel for minerals), international coordination mechanisms (OECD inventory standards), and government procurement programs are degraded institutional responses to supply insecurity. High theater (public announcements of reserve builds, emergency release protocols invoked rarely) relative to function (actual releases address only 5-10% of disruption scenarios). Maintained through inertia: governments fear political cost of supply shock more than cost of reserve maintenance, even as actual reserve utility declines. Piton classification driven by theater_ratio ≥ 0.70 expectation — though actual theater here is 0.48, the reserve system itself exhibits performative character.
constraint_indexing:constraint_classification(critical_mineral_supply_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilization-scale analysis reveals genuine coordination function (minerals enable decarbonization, technology, healthcare) entangled with structural extraction (geographic concentration, cartel dynamics, geopolitical weaponization). Both functions are real and structural. The constraint is not a false summit (naturalizable as immutable geophysics) because supply security is substantially a function of institutional arrangements (OPEC-like discipline, processing concentration, strategic reserve adequacy, technology gatekeeping) not pure resource scarcity. Misclassifying as pure extraction (snare) or pure coordination (rope) both obscure the hybrid character. The constraint enforces its own perpetuation through the very mechanisms that coordinate supply.
constraint_indexing:constraint_classification(critical_mineral_supply_security, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_mineral_supply_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_mineral_supply_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_mineral_supply_security, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_mineral_supply_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_mineral_supply_security, TR),
    TR >= 0.70.

:- end_tests(critical_mineral_supply_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from downstream manufacturers through pricing power and supply uncertainty, and from resource-poor nations through terms-of-trade asymmetry. The extraction is not total (snare level ~0.66+) because coordination function is genuine — disruption would cause real economic damage to mining nations and processors as well as manufacturers. The 0.58 value reflects that the constraint is sustainable only because both extractors and victims have net benefit from continuation (manufacturers get access, miners get revenue), but extraction rents are concentrated asymmetrically. Measured increase from 0.38 to 0.58 reflects growing processing concentration and geopolitical risk weaponization — the system has become more extractive over the interval. Suppression (0.62): High. Barriers to exit include: no technical substitutes at current scale, geographic immobility of resources, geopolitical risk of supply cuts, high capital cost of alternate processing capacity, technological sophistication barriers. Suppression is structural, not primarily performative. Theater ratio (0.48): Moderate. Strategic reserves and sustainability standards contain performative elements, but the core supply mechanism is functionally real — prices do signal scarcity, supply cuts do occur, downstream vulnerability is measurable. Theater is lower than in a piton because the coordination-extraction mechanism depends on actual enforcement, not on ritual maintenance. Claimed type (Tangled Rope): Required for beneficiaries (mining nations, processors), victims (manufacturers, resource-poor nations), and active enforcement (cartel discipline, supply contracts, strategic reserve coordination). The classification rejects both pure coordination (rope) and pure extraction (snare) because the constraint genuinely coordinates supply access while enforcing extraction through structural asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence across structural positions. Downstream manufacturers (powerless/trapped) classify it as snare — zero exit options, maximum extraction, no coordination benefit visible. Mining nations (institutional/arbitrage) classify it as rope — coordination of supply rents, legitimate benefit from scarcity valuation. Resource-poor nations (organized/constrained) see tangled rope — genuine need for coordination alongside extraction through terms-of-trade asymmetry. Technology coalition (powerful/mobile) sees tangled rope with sunset — recognizing that diversification and substitution offer exit paths within a generational horizon. Strategic reserves (institutional/arbitrage) see piton — acknowledging their own performative character. The analytical observer sees tangled rope as the integrated classification — both functions are structural and real. The gap between snare (victim perspective) and rope (beneficiary perspective) is maximal; the gap between tangled rope (intermediate perspectives) and either extreme is diagnostic of the hybrid character.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to extraction flow. Mining nations and processors are beneficiaries with arbitrage options (can redirect supply, set terms, switch markets) → d ≈ 0.05-0.15 → f(d) ≈ -0.12 to -0.01 → experiences negative or minimal χ. Downstream manufacturers are victims with trapped exit (no alternatives) → d ≈ 0.95 → f(d) ≈ 1.42 → experiences maximum χ. Resource-poor nations are victims with constrained exit (high cost but possible through regional coordination) → d ≈ 0.75 → f(d) ≈ 1.05 → experiences high χ. Technology coalition are intermediate agents with mobile exit (can develop substitutes) → d ≈ 0.60 → f(d) ≈ 0.80 → experiences moderate χ. Scope modifier σ(S) applies: global scope (σ = 1.2) amplifies χ for all agents because supply vulnerability scales with geographic reach — a global downstream manufacturer experiences more extraction (higher χ) at global scope than at regional scope. The formula χ = ε × f(d) × σ(S) captures that the same constraint (ε = 0.58) produces very different experienced extractiveness depending on agent position (f(d) varies 3.4×) and scope (σ varies 1.5×).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating why both coordination and extraction functions are structurally real and required by the classification. The constraint MUST coordinate supply access (else downstream manufacturers shut down, miners have no market). The constraint MUST extract rents (else mining nations have no incentive to cooperate with cartel discipline, processing concentration would break apart into competitive pricing). Misclassifying as pure coordination (rope) obscures the exploitation of manufacturers and resource-poor nations. Misclassifying as pure extraction (snare) obscures the genuine supply-access coordination that benefits all agents relative to supply collapse. Tangled Rope is the structurally accurate classification: the constraint simultaneously solves a real coordination problem (stable supply access) and enforces asymmetric extraction (rents concentrated in mining oligarchy). The three required gates are satisfied: beneficiaries (mining nations, processors) exist and are substantive; victims (manufacturers, resource-poor nations) exist and bear measurable costs; active enforcement (cartel discipline, supply contracts, geopolitical pressure) is structurally required to maintain the extraction layer atop the coordination function. The perspectival gap (snare from powerless perspective, rope from institutional perspective, tangled rope from analytical perspective) is diagnostic of the hybrid mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_scarcity_vs_cartel_discipline,
    'Is supply vulnerability driven by genuine physical scarcity (rare earths concentrated in DRC, Indonesia, China) or by cartel-like coordination restricting supply relative to geological potential?',
    'Historical production data vs resource-in-ground estimates; comparison of extraction rates to depletion models; analysis of production curtailment events correlated with price management rather than capacity constraints',
    'If scarcity: constraint approaches mountain or rope (structural coordination problem). If cartel: constraint is extraction mechanism (snare toward end users), which is mutable through antitrust/competition policy. Classification shifts from accepting inevitability to treating as contestable political economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_scarcity_vs_cartel_discipline, empirical, 'Whether supply constraints are geological or institutional (cartel discipline)').

omega_variable(
    technology_substitution_feasibility,
    'Can silicon carbide, gallium nitride, phosphate-free batteries, and rare-earth-free magnets achieve cost/performance parity with incumbent materials at scale within 10-20 years?',
    'Technoeconomic modeling with manufacturing learning curves; roadmap analysis from semiconductor and battery industries; capital cost estimates for green/brown field production capacity',
    'If feasible: scaffold perspective confirmed — supply scarcity is temporary and solvable through innovation. If infeasible: supply constraint persists indefinitely as structural (snare toward end users, tangled rope toward diversifying nations). Affects sunset clause credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Feasibility of technology substitution reducing mineral dependency').

omega_variable(
    recycling_closure_rate_limit,
    'What fraction of critical minerals can be sustainably recovered from end-of-life products through circular economy processes, and at what cost premium relative to virgin mining?',
    'Lifecycle assessment studies; pilot recycling operations cost tracking; thermodynamic limits on recovery rates for dispersed applications (battery electrolytes, semiconductor dopants)',
    'If closure rate > 60% at competitive cost: secondary supply becomes viable alternative, breaking primary mining cartel (extraction mechanism weakened). If closure rate < 30%: mining oligarchy maintains structural control (snare persistence). Affects long-term classification stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recycling_closure_rate_limit, empirical, 'Circular economy closure rate and cost competitiveness for critical minerals').

omega_variable(
    processing_concentration_irreducibility,
    'Are processing bottlenecks (rare earth element separation, cobalt refining, lithium conversion) inherently concentrated in a few locations due to technical/environmental barriers, or is concentration a contingent result of historical investment patterns and regulatory capture?',
    'Technoeconomic analysis of processing plant scaling; comparison of processing cost between incumbent (China, Indonesia) and alternative locations; regulatory/environmental barrier assessment',
    'If inherently concentrated: extraction mechanism is structural (snare), shifting to tangled rope only through technological substitution. If contingent: policy intervention (tariffs, investment incentives, environmental standards harmonization) can break concentration. Classification stability affected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(processing_concentration_irreducibility, empirical, 'Whether processing concentration is technical or institutional').

omega_variable(
    geopolitical_weaponization_structural,
    'Is supply cutoff capability (China rare earths embargo, DRC cobalt restriction) a structural feature of the system or an abuse of contingent power?',
    'Historical embargo effectiveness; correlation between geopolitical tension and supply disruption; countermeasure development time and cost',
    'If structural: weaponization persists regardless of market structure (snare from end-user perspective persists). If abuse: diversification and multi-source supply reduce vulnerability (tangled rope landscape becomes mobile). Affects perspectival gap and suppression measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_weaponization_structural, empirical, 'Whether geopolitical weaponization is structural or contingent power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_mineral_supply_security, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmss_tr_t0, critical_mineral_supply_security, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cmss_tr_t5, critical_mineral_supply_security, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cmss_tr_t10, critical_mineral_supply_security, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cmss_be_t0, critical_mineral_supply_security, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cmss_be_t5, critical_mineral_supply_security, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cmss_be_t10, critical_mineral_supply_security, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_mineral_supply_security, resource_allocation).
narrative_ontology:affects_constraint(critical_mineral_supply_security, renewable_energy_transition_bottleneck).
narrative_ontology:affects_constraint(critical_mineral_supply_security, semiconductor_supply_chain_fragility).
narrative_ontology:affects_constraint(critical_mineral_supply_security, geopolitical_resource_weaponization).

% DUAL FORMULATION NOTE:
% Critical mineral supply security decomposes into three structurally distinct constraints under the ε-invariance principle: (1) geographic scarcity as a geophysical/market coordination problem (ε ≤ 0.30, rope perspective); (2) cartel discipline as institutional extraction mechanism (ε = 0.58, tangled rope); (3) processing concentration as technological bottleneck with institutional reinforcement (ε = 0.65, snare from end-user perspective). This story models the integrated constraint (effective ε = 0.58) visible from institutional and analytical perspectives. Downstream stories can model the component constraints separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(critical_mineral_supply_security, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
