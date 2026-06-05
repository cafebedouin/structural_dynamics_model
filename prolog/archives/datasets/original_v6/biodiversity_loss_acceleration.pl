% ============================================================================
% CONSTRAINT STORY: biodiversity_loss_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biodiversity_loss_acceleration, []).

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
 *   constraint_id: biodiversity_loss_acceleration
 *   human_readable: Biodiversity Loss Acceleration
 *   domain: ecological/environmental/economic
 *
 * SUMMARY:
 *   Biodiversity loss acceleration is the structural constraint linking
 *   industrial production systems to ecosystem collapse across timescales
 *   from immediate (species loss) to civilizational (ecosystem service
 *   exhaustion). The constraint exhibits snare classification from the
 *   perspective of non-human species and future generations — maximum
 *   extraction with no exit option — while simultaneously appearing as rope
 *   to beneficiary industries, tangled rope to dependent communities,
 *   scaffold to organized conservation actors, and piton to captured
 *   regulatory institutions. The acceleration itself is the key diagnostic
 *   signature: extractiveness and theater ratio have both increased
 *   monotonically over five decades as the gap between conservation intent
 *   and ecological outcome has widened. This is not a static rent-extraction
 *   problem but a ratcheting one, where each round of extraction sets
 *   conditions for the next, foreclosing alternatives and deepening
 *   suppression. The constraint operates through three structural mechanisms:
 *   (1) institutional capture of regulatory agencies by extraction
 *   beneficiaries, (2) market price signals that treat ecosystem services as
 *   zero-cost, and (3) temporal asymmetry between human decision-making
 *   horizons and ecosystem regeneration timescales.
 *
 * KEY AGENTS:
 *   - Non-human Species and Ecosystems: Primary victims (powerless/trapped) — directly targeted for habitat conversion, chemical exposure, and trophic disruption with no alternative or exit
 *   - Future Human Generations: Temporal victims (powerless/trapped) — inherit depleted ecosystem services without participation in causative decisions; maximum suppression across deep time
 *   - Indigenous Communities and Small-Scale Producers: Secondary actors (moderate/constrained) — depend on ecosystem integrity; some coordinate biodiversity stewardship but structurally denied decision power over resource use
 *   - Industrial Agriculture, Mining, and Forestry: Primary beneficiaries (institutional/arbitrage) — capture rents from habitat conversion and resource extraction; have exit options (arbitrage to alternative jurisdictions or commodities)
 *   - Conservation and Climate Movement: Organized challengers (organized/mobile) — perceive problem as solvable through renewable energy transition and regenerative systems; have exit pathway (sunset logic) if alternatives mature
 *   - Environmental Regulation and Protected Areas: Captured institutions (institutional/constrained) — maintain performative conservation mandates with declining functional efficacy; regulatory capture prevents institutional exit
 *   - Developed-Country Consumers: Distributed beneficiaries (powerful/mobile) — benefit from low-cost commodities dependent on ecosystem conversion; have mobility (consumption change) but high private cost relative to diffuse public benefit
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional architecture as immutable ecosystem limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biodiversity_loss_acceleration, 0.68).
domain_priors:suppression_score(biodiversity_loss_acceleration, 0.72).
domain_priors:theater_ratio(biodiversity_loss_acceleration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biodiversity_loss_acceleration, extractiveness, 0.68).
narrative_ontology:constraint_metric(biodiversity_loss_acceleration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biodiversity_loss_acceleration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biodiversity_loss_acceleration, snare).
narrative_ontology:human_readable(biodiversity_loss_acceleration, "Biodiversity Loss Acceleration").
narrative_ontology:topic_domain(biodiversity_loss_acceleration, "ecological/environmental/economic").

domain_priors:requires_active_enforcement(biodiversity_loss_acceleration).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biodiversity_loss_acceleration, industrial_agriculture_sector).
narrative_ontology:constraint_beneficiary(biodiversity_loss_acceleration, extractive_resource_industries).
narrative_ontology:constraint_beneficiary(biodiversity_loss_acceleration, real_estate_development).
narrative_ontology:constraint_victim(biodiversity_loss_acceleration, non_human_species).
narrative_ontology:constraint_victim(biodiversity_loss_acceleration, ecosystem_services).
narrative_ontology:constraint_victim(biodiversity_loss_acceleration, future_human_populations).
narrative_ontology:constraint_victim(biodiversity_loss_acceleration, indigenous_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The biotic community has no exit option and bears extraction as habitat loss, chemical contamination, and trophic collapse. Maximum experienced extraction. Cannot organize or negotiate. Suppression is structural — species cannot choose alternatives; ecosystems have no reversibility option within human timescales.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Temporally trapped. Future populations inherit depleted ecosystem services without participation in decisions that caused depletion. Maximum extraction across deep time. Suppression is absolute — no options to refuse participation in ecosystem collapse they did not cause.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% Structurally dependent on ecosystem integrity for subsistence and cultural continuity. Face high costs to exit (relocation, livelihood transformation). Experience extraction through resource access denial and decision exclusion. Also coordinate some ecological management (fire regimes, rotational harvest) providing genuine value. Moderate power and constrained exit produce tangled rope: some coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Institutional beneficiaries with arbitrage options. Coordinate commodity production and resource extraction. Net beneficiaries during the acceleration window — capture value from biodiversity conversion. See constraint as coordination mechanism (solving production problems) rather than extraction. Can arbitrage to alternative extraction domains or jurisdictions.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized agents (NGOs, climate activists, scientific community) perceive biodiversity loss as a solvable coordination problem with a sunset horizon. See renewable energy transition, regenerative agriculture, and habitat restoration as alternative pathways. Have agency and exit route (transition to sustainable production). Theater ratio reflects performance in advocacy and policy negotiation, but structured sunset logic: if energy and food systems transition successfully, the extraction mechanism loses force.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Regulatory institutions (environmental ministries, conservation agencies, protected area systems) maintain performative protection mechanisms with declining functional efficacy. Paper parks, unenforceable regulations, and underfunded agencies persist through institutional inertia despite clear failure to halt acceleration. Theater ratio reflects gap between regulatory mandates and actual biodiversity outcomes. Institutions are themselves captured by extraction beneficiaries, constraining institutional exit.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Powerful agents embedded in commodity chains that depend on biodiversity extraction. Have mobility (can shift consumption) but benefit from low-cost resource access enabled by ecosystem conversion. Experience constraint as coordination of global supply chains alongside extraction of ecosystem rents. Can exit through consumption change, but high private cost relative to diffuse public benefit. Moderate extraction relative to powerless agents.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% From a civilizational/universal perspective, species extinction and ecosystem conversion appear as immutable consequences of human population and resource consumption reaching planetary boundaries. The mountain perspective risks naturalizing what is a contingent policy architecture. However, structural analysis reveals this as a false summit: the acceleration is driven by institutional choices (subsidy structures, property regimes, price signals) not by physical laws. The constraint is a snare, not a mountain.
constraint_indexing:constraint_classification(biodiversity_loss_acceleration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biodiversity_loss_acceleration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biodiversity_loss_acceleration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biodiversity_loss_acceleration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biodiversity_loss_acceleration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(biodiversity_loss_acceleration, TR),
    TR >= 0.70.

:- end_tests(biodiversity_loss_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Biodiversity conversion generates rents for industrial beneficiaries while imposing costs (ecosystem service loss, health impacts, genetic resource depletion) on non-human species, future generations, and indigenous communities. The rent extraction is not incidental to production but its core mechanism — habitat destruction IS the profit mechanism in industrial agriculture and resource extraction. The 0.68 value reflects accelerating extraction: as remaining intact ecosystems shrink, the monopoly rent on conversion intensifies. Suppression (0.72): Very high. The snare operates through: (1) ecological constraint (non-human species have no material exit), (2) temporal constraint (future generations cannot renegotiate), (3) informational suppression (ecosystem service values hidden in 'externality' language), (4) institutional suppression (regulatory agencies captured and underfunded), and (5) exit cost inflation (alternatives are expensive relative to status quo). Theater ratio (0.58): Moderate-high. Conservation policy, ESG corporate reporting, and 'sustainable development' rhetoric perform environmental commitment while material biodiversity loss accelerates. The gap between stated conservation targets and actual ecosystem outcomes has widened dramatically — this is the theater ratio increasing. However, theater is not total — some conservation spending is functionally effective, and some regulatory enforcement occurs. The accelerating trend in theater ratio (0.25→0.58 over 54 years) indicates that performative mechanisms are increasingly substituting for functional ones.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap: beneficiary institutional actors perceive coordination and benefit; victim temporal and non-human actors perceive extraction with no exit; organized conservation actors perceive a sunset path. The gap is not interpretive disagreement but structural reality — the same constraint IS coordination for one agent and extraction for another, because the extraction of rents from habitat conversion IS the coordination of industrial commodity production. The beneficiary perspective is not false; it is orthogonal to the victim perspective. Both are true descriptions of what the constraint does. The analytical observer at civilizational scope risks falsely resolving this gap by naturalizing the beneficiary's framing (extinction as inevitable limit) rather than recognizing both perspectives as legitimate descriptions of a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Non-human species: d approaches 1.0 (pure target, trapped, no options). Future generations: d=1.0 (temporal trap, zero options). Industrial beneficiaries: d approaches 0.0 (net beneficiaries, arbitrage options, extraction flows toward them). Indigenous communities: d≈0.65 (partial victims, constrained options, some coordination function but asymmetric extraction). Conservation coalitions: d≈0.45 (organized agents with exit pathway visible, constrained by current system but mobile). Regulatory institutions: d≈0.55 (captured agents constrained from exit despite notional commitment to opposite goal). The snare classification across powerless and temporal dimensions is mechanically derived from d→1.0 combined with trapped exit options. The perspectival gap between beneficiary rope and victim snare reflects d values spanning nearly the full [0.0, 1.0] range.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint classifies as snare at maximum extractiveness (0.68 > 0.46), with mandatrophy_resolved: true. The snare classification is valid at the civilizational/analytical perspective because: (1) non-human species have zero degrees of freedom (trapped), (2) ecosystem services are maximum-extraction targets (ε=0.68), (3) suppression is structural and near-total (0.72), and (4) beneficiaries have net positive directionality while victims have net negative. The mandatrophy resolution confirms that this is not a rope (pure coordination) or tangled rope (mixed but balanced) — it is a snare with genuine coordination components visible only from the beneficiary perspective. The constraint's theater ratio increase over time (0.25→0.58) reflects Goodhart drift: as material biodiversity loss has accelerated, regulatory and corporate performance metrics (protected area coverage, ESG scores, 'net positive biodiversity' commitments) have decoupled from actual outcomes, indicating metric substitution. The mandatrophy is resolved by acknowledging that the snare is partially theatrical — some conservation activity is functional, some is performative — but the net extraction mechanism is snare-type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trophic_collapse_irreversibility,
    'What fraction of observed species loss is reversible through habitat restoration versus irreversibly locked by trophic interaction collapse?',
    'Long-term ecosystem restoration data; analysis of rewilding projects showing recovery timelines and success rates; genomic analysis of genetic bottleneck severity',
    'If reversibility > 50% at current loss rates: mountain classification is false summit; constraint remains a snare (policy choice). If irreversibility > 80%: constraint approaches mountain (crosses into physical limit of ecosystem regeneration). Determines policy urgency horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trophic_collapse_irreversibility, empirical, 'Extent of reversible versus irreversible species loss from trophic collapse').

omega_variable(
    ecosystem_service_valuation_accuracy,
    'Do economic valuations of ecosystem services (pollination, water purification, carbon storage, genetic resources) capture the true substitution cost, or do they systematically underestimate replacement value?',
    'Comparison of valuations against observed costs of technological substitutes (artificial pollination labor, water treatment plants, carbon capture equipment); contingent valuation accuracy testing',
    'If underestimated: extractive industries benefit from hidden subsidy (ecosystem rents not priced), increasing effective extraction ratio chi. This is likely — ecosystem valuation typically treats non-market services as zero-value. If accurate: extraction analysis must account for full economic value of converted ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_service_valuation_accuracy, empirical, 'Whether ecosystem service valuations capture true substitution costs').

omega_variable(
    conservation_funding_sufficiency,
    'Is regulatory underperformance (piton classification) due to structural regulatory capture or to resource constraints that could be overcome by increasing conservation funding?',
    'Analysis of agencies with high-funding scenarios; comparison of enforcement outcomes across high-budget vs low-budget conservation jurisdictions; political economy of budget allocation decisions',
    'If capture-driven: more funding alone will not fix piton degradation; regulatory institutions require structural reform. If funding-driven: quadrupling conservation budgets could shift piton to scaffold (sunset through effective enforcement + alternatives maturation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_funding_sufficiency, empirical, 'Whether piton degradation is capture-driven or resource-constrained').

omega_variable(
    industrial_exit_cost_asymmetry,
    'For industrial agriculture and extraction sectors, what is the true private cost of transition away from biodiversity-destructive practices, versus the cost absorption shifted to society?',
    'Accounting for capital write-offs, profit margin reduction, and stranded asset value for firms transitioning to sustainable practices; comparison with public costs of pollution cleanup, health impacts, ecosystem restoration',
    'If private transition costs are moderate relative to captured rents: snare maintains force through institutional choice to externalize costs. If transition costs are high relative to captured rents: snare may be structurally stable (cost-benefit genuine rather than extracted). Determines whether beneficiary exit is available or apparent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industrial_exit_cost_asymmetry, empirical, 'Asymmetry between private transition costs and externalized public costs').

omega_variable(
    indigenous_biodiversity_stewardship_effectiveness,
    'Do indigenous land management practices demonstrably maintain higher biodiversity than state-protected areas or industrial systems, or is this attributed effectiveness an artifact of lower human population density rather than stewardship practice?',
    'Comparison of biodiversity metrics across indigenous territories, state parks, and industrial lands controlling for baseline habitat type and human population density; causal analysis of specific practices (fire management, species selection, rotation regimes)',
    'If practices are effective: indigenous communities have genuine coordination function and tangled rope classification is correct. If density-artifact: indigenous communities are primary victims not hybrid agents, shifting classification toward snare. Determines legitimacy of indigenous perspective as beneficiary versus victim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_biodiversity_stewardship_effectiveness, empirical, 'Whether indigenous stewardship practices causally maintain biodiversity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biodiversity_loss_acceleration, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biod_tr_t1970, biodiversity_loss_acceleration, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(biod_tr_t1990, biodiversity_loss_acceleration, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(biod_tr_t2010, biodiversity_loss_acceleration, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(biod_tr_t2024, biodiversity_loss_acceleration, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(biod_be_t1970, biodiversity_loss_acceleration, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(biod_be_t1990, biodiversity_loss_acceleration, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(biod_be_t2010, biodiversity_loss_acceleration, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(biod_be_t2024, biodiversity_loss_acceleration, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biodiversity_loss_acceleration, resource_allocation).
narrative_ontology:boltzmann_floor_override(biodiversity_loss_acceleration, 0.22).
narrative_ontology:affects_constraint(biodiversity_loss_acceleration, agricultural_land_expansion).
narrative_ontology:affects_constraint(biodiversity_loss_acceleration, climate_feedback_loops_biosphere).
narrative_ontology:affects_constraint(biodiversity_loss_acceleration, ecosystem_service_pricing_invisibility).
narrative_ontology:affects_constraint(biodiversity_loss_acceleration, regulatory_capture_environmental_agencies).

% DUAL FORMULATION NOTE:
% Biodiversity loss acceleration is downstream of multiple institutional constraints: agricultural subsidy structures, property regimes that externalize ecosystem costs, and regulatory capture. Upstream constraints (subsidy architecture, price signal distortion, institutional capture) drive the acceleration. This story focuses on the aggregate snare mechanism; decomposition into component constraints (e.g., 'agricultural commodity subsidies enable habitat conversion' as separate story) would show how multiple smaller extraction mechanisms layer to produce acceleration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biodiversity_loss_acceleration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
