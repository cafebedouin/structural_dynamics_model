% ============================================================================
% CONSTRAINT STORY: agricultural_water_pollution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agricultural_water_pollution, []).

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
 *   constraint_id: agricultural_water_pollution
 *   human_readable: Agricultural Water Pollution Constraint
 *   domain: environmental/economic/regulatory
 *
 * SUMMARY:
 *   Agricultural water pollution represents a structural constraint where the
 *   benefits of high-yield production accrue to agricultural producers and
 *   input suppliers while the costs of water contamination (degraded water
 *   quality, public health impacts, ecosystem collapse, water treatment
 *   expenses) fall on downstream communities and aquatic systems. The
 *   constraint exhibits properties of both coordination (genuine need to
 *   balance food production with water protection) and extraction (profitable
 *   externalization of pollution costs). The extractiveness has increased
 *   over the 40-year interval (0.35 to 0.58) as agricultural intensification
 *   has increased pollution volumes and discovery of long-timescale
 *   contamination (groundwater nitrate, per- and polyfluoroalkyl substances)
 *   has revealed that costs persist for decades beyond the extractive period.
 *   Theater ratio has remained moderate (0.32 to 0.48), reflecting that some
 *   genuine mitigation occurs through best management practices, while
 *   compliance metrics do not correlate strongly with actual pollution
 *   reduction — a diagnostic signature of partial theatrical performance.
 *
 * KEY AGENTS:
 *   - Downstream Communities: Primary victims (powerless/trapped) — bear health, economic, and ecological costs; no decision-making power over agricultural practices
 *   - Aquatic Ecosystems: Primary victims (powerless/trapped) — degradation is irreversible on human timescales; no capacity to organize or negotiate
 *   - Conventional Agricultural Producers (Large-Scale): Primary beneficiaries (powerful/mobile) — capture financial benefits of intensive production; can absorb or avoid mitigation costs
 *   - Chemical Manufacturers: Secondary beneficiaries (institutional/arbitrage) — profit from both high-volume commodity sales and premium-priced mitigation products
 *   - Smallholder Farmers: Mixed position (moderate/constrained) — extract from downstream communities through pollution but constrained by capital and market access; also victimized by regulatory costs they cannot afford
 *   - Environmental/Agricultural Reform Coalition: Organized reformers (organized/constrained) — building alternative systems with sunset logic; have limited capital and face political resistance
 *   - Regulatory Agencies: Institutional actors (institutional/arbitrage) — maintain compliance theater; benefit from funding tied to monitoring/enforcement; experience pressure from both agricultural and environmental constituencies
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing agricultural pollution as inevitable cost of food production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agricultural_water_pollution, 0.58).
domain_priors:suppression_score(agricultural_water_pollution, 0.65).
domain_priors:theater_ratio(agricultural_water_pollution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agricultural_water_pollution, extractiveness, 0.58).
narrative_ontology:constraint_metric(agricultural_water_pollution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(agricultural_water_pollution, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agricultural_water_pollution, tangled_rope).
narrative_ontology:human_readable(agricultural_water_pollution, "Agricultural Water Pollution Constraint").
narrative_ontology:topic_domain(agricultural_water_pollution, "environmental/economic/regulatory").

domain_priors:requires_active_enforcement(agricultural_water_pollution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agricultural_water_pollution, conventional_agricultural_producers).
narrative_ontology:constraint_beneficiary(agricultural_water_pollution, chemical_manufacturers).
narrative_ontology:constraint_beneficiary(agricultural_water_pollution, equipment_suppliers).
narrative_ontology:constraint_victim(agricultural_water_pollution, downstream_communities).
narrative_ontology:constraint_victim(agricultural_water_pollution, aquatic_ecosystems).
narrative_ontology:constraint_victim(agricultural_water_pollution, groundwater_dependent_users).
narrative_ontology:constraint_victim(agricultural_water_pollution, public_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM COMMUNITY (SNARE) — Trapped by geography and water dependency. No alternative water source; cannot exit the region without losing livelihood and community. Bears full cost of pollution (health effects, water treatment expenses, ecological degradation) with no decision-making power over agricultural practices upstream. Maximum experienced extraction.
constraint_indexing:constraint_classification(agricultural_water_pollution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AQUATIC ECOSYSTEMS (SNARE) — No agency, no exit options, no capacity to negotiate. Bear cumulative extraction through eutrophication, pesticide bioaccumulation, and habitat destruction. Cannot organize or represent interests. Pure victim status across all horizons.
constraint_indexing:constraint_classification(agricultural_water_pollution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALLHOLDER/TRANSITIONING FARMERS (TANGLED ROPE) — Constrained by capital requirements for alternative farming methods, market access barriers, and knowledge gaps. Experience both extraction (required to adopt expensive mitigation technologies, threatened by regulatory costs) and genuine coordination benefit (participatory watershed management, cooperative water quality monitoring). Cannot easily exit conventional agriculture due to debt and land lock-in.
constraint_indexing:constraint_classification(agricultural_water_pollution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHEMICAL MANUFACTURERS (ROPE) — Institutional beneficiaries with high exit optionality (can shift product lines, relocate operations, lobby for deregulation). Experience the constraint as coordination: regulatory compliance drives market segmentation (organic vs conventional), enabling product differentiation and premium pricing. Net beneficiary through arbitrage opportunities and market stabilization.
constraint_indexing:constraint_classification(agricultural_water_pollution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE-SCALE CONVENTIONAL OPERATORS (TANGLED ROPE) — Powerful agents with mobile capital (can relocate operations, diversify product mix, adopt selective best management practices). Experience mixed extraction and coordination: regulation constrains high-intensity practices but enables market differentiation. Can absorb mitigation costs better than smallholders. Both extract from downstream communities and coordinate with regulatory framework.
constraint_indexing:constraint_classification(agricultural_water_pollution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (SCAFFOLD) — Organized actors (environmental NGOs, sustainable agriculture advocates, forward-thinking regulatory agencies) see the pollution constraint as a temporary institutional failure being resolved through sunset mechanisms: regenerative agriculture certification, payment for ecosystem services, mandatory water quality monitoring, and transition funding. Exit path is real (alternative agricultural models) but constrained by capital and political resistance. Theater is lower than in pure snare because coalition has visibility and agency.
constraint_indexing:constraint_classification(agricultural_water_pollution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY COMPLIANCE THEATER (PITON) — Traditional agricultural extension, certification programs, and water quality standards are substantially performative. Compliance metrics (best management practice adoption rates, chemical application reporting) do not correlate with actual water quality improvement in many watersheds. The ritual persists through institutional inertia — agencies maintain monitoring and reporting requirements that produce the appearance of control without preventing pollution. Theater ratio is lower than verification bottleneck because some actual coordination occurs, but degradation is visible in the growing gap between reported compliance and measured contamination.
constraint_indexing:constraint_classification(agricultural_water_pollution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, water contamination from nutrient and pesticide runoff is presented as a natural consequence of intensive agriculture — an inevitable trade-off between food production and water quality. The constraint appears as a physical/hydrological law: nutrients applied to fields will migrate toward groundwater and surface water through diffusion and flow. However, the structural data contradicts the mountain classification. The pollution magnitude is contingent on agricultural practice choices, regulatory enforcement, and investment in mitigation infrastructure — not a law of nature but a contingent institutional arrangement naturalizing profit-maximizing production models.
constraint_indexing:constraint_classification(agricultural_water_pollution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agricultural_water_pollution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agricultural_water_pollution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agricultural_water_pollution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agricultural_water_pollution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agricultural_water_pollution, TR),
    TR >= 0.70.

:- end_tests(agricultural_water_pollution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Agricultural pollution imposes significant externalized costs on downstream communities and ecosystems — health care, water treatment, lost fisheries, ecological restoration — that are not priced into agricultural production. The beneficiary (conventional producers) profits by avoiding the cost of pollution prevention. The 40-year trajectory shows increasing extractiveness as (a) pollution volumes grew with intensification, (b) long-latency contaminants (nitrates, PFOA) revealed costs persisting decades beyond initial application, and (c) downstream communities discovered cumulative health impacts. Early extractiveness (0.35) reflected incomplete information about groundwater contamination; modern extractiveness reflects full recognition of long-timescale costs. Suppression (0.65): High. Downstream communities face significant barriers to exit (geographic dependency on water source, economic dependence on affected region, lack of alternative supply infrastructure). Powerless actors also face information suppression — agricultural pollution sources are dispersed and difficult to trace, creating plausible deniability. However, suppression is not total: some communities can invest in water treatment, some regions have diversified water supplies, and scientific evidence of pollution is mounting. Theater ratio (0.48): Moderate. Genuine coordination occurs: best management practices, nutrient management planning, and conservation buffers represent real mitigation efforts. But compliance reporting does not reliably predict pollution reduction — theater consists of certification metrics, adoption percentages, and compliance documentation that persist despite measured pollution remaining high. This signature (moderate theater with growing extractiveness) indicates a transition from rope toward tangled rope or snare, not toward piton.
 *
 * PERSPECTIVAL GAP:
 *   The downstream community and aquatic ecosystems see pure extraction (Snare) — they bear costs with no compensation or meaningful exit. Smallholders and transitioning farmers see mixed extraction and coordination (Tangled Rope) — they are both extractors (via water pollution) and victims (via regulatory burden). Large conventional operators see coordination (Rope, sometimes Scaffold) — for them, regulation enables market differentiation and premium pricing; sustainability compliance becomes a branding asset. Chemical manufacturers and regulators see pure coordination or beneficial arbitrage (Rope) — profitable from the production side and from the mitigation side. The reform coalition sees a resolvable problem with sunset logic (Scaffold) — alternative agriculture, regenerative practices, and payment-for-ecosystem-services are building exit pathways. The civilizational analytical observer risks seeing natural law (Mountain) — food production always pollutes water — but the structural data reveals this as naturalizing a contingent agricultural model that externalizes costs rather than a law of physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by structural position. Conventional large-scale producers have low d (beneficiaries with mobile capital and arbitrage exit options) — they experience negative or minimal effective extraction. Downstream communities have high d (trapped victims with no exit) — they experience maximum extraction chi. Smallholders have intermediate d (constrained victims with limited exit options) — they experience moderate extraction but also modest coordination benefits through agronomic learning and watershed collective action. Chemical manufacturers have very low d (institutional beneficiaries with global exit options) — they profit from both the creation of the pollution problem (commodity chemicals) and the attempted solution (specialized mitigation products). Regulatory agencies have near-zero d (institutional actors with arbitrage exit options) — they coordinate compliance theater while capturing enforcement funding. The constraint's extraction flows consistently from powerless geographic victims toward powerful agricultural producers and their supply chain beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the classification varies legitimately across power positions, not because the classification system is flawed. The snare classification from the powerless perspective is justified by high suppression, trapped exit, and maximum experienced extraction. The rope classification from institutional beneficiary perspectives is justified by their ability to profit from the constraint and exit if conditions change. The tangled rope for moderate actors is justified by their mixed position as both extractors and constrained victims. The scaffold for organized reform actors is justified by their visible exit pathways and agency. The piton risk (regulatory theater degradation) is lower than in verification bottleneck because some actual mitigation occurs, though correlation with outcomes is weak. The mountain false summit from the civilizational analytical view is diagnosed as naturalizing a contingent agricultural model, not a law of nature — the constraint would vanish if agricultural producers internalized pollution costs through pricing or regulation, proving it contingent rather than inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pollution_source_attribution,
    'What proportion of water pollution is attributable to agricultural practices vs. legacy contamination, atmospheric deposition, and urban/industrial sources?',
    'Isotopic tracing of nitrogen and phosphorus; wastewater source apportionment analysis; comparison of agricultural vs non-agricultural land use impact on water quality in matched watersheds',
    'If agriculture > 60%: extractiveness justified at 0.58+. If agriculture < 40%: constraint may be misattributed to agricultural actors; extractiveness should be lower, classification should shift toward rope (shared responsibility coordination problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pollution_source_attribution, empirical, 'Attribution of water pollution to agricultural vs. other sources').

omega_variable(
    mitigation_cost_distribution_fairness,
    'Are the costs of pollution mitigation (best management practices, precision agriculture, riparian buffers, nutrient recovery) distributed fairly across beneficiaries and victims, or do they fall disproportionately on powerless actors?',
    'Cost-benefit analysis of mitigation by farmer income level; comparison of government subsidy rates for large vs. small operators; tracking of financial burden on downstream water treatment facilities',
    'If costs fall on powerless downstream communities: snare classification confirmed, suppression ≥ 0.65 justified. If costs fairly distributed via subsidies/market mechanisms: tangled rope more accurate, suppression should be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_cost_distribution_fairness, empirical, 'Fairness of mitigation cost distribution').

omega_variable(
    alternative_agriculture_scalability,
    'Can regenerative and organic agriculture achieve current food production volumes while maintaining or reducing water contamination?',
    'Large-scale transitioning watershed studies; yield and pollution data from diversified vs. monoculture systems under identical climate/soil conditions; economic viability analysis without subsidy dependency',
    'If scalable without yield loss: scaffold perspective confirmed — real sunset pathway exists, theater should be lower. If scalable only at cost of production reduction: constraint is structural economic trade-off, not institutional arrangement — extractiveness justified, beneficiary extraction is payment for continued production.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_agriculture_scalability, empirical, 'Whether alternative agriculture can scale to current production levels').

omega_variable(
    regulatory_enforcement_correlation,
    'Does intensity of regulatory enforcement (inspection frequency, penalty magnitude, monitoring resolution) correlate with actual pollution reduction, or is compliance primarily performative theater?',
    'Regression analysis of enforcement intensity vs. measured water quality in comparable watersheds; comparison of reported best management practice adoption vs. satellite/sensor data on actual practice implementation; tracking of citation rates vs. pollution trends',
    'If strong correlation: enforcement is effective coordination, theater_ratio should be lower, classification toward rope. If weak/absent correlation: piton diagnosis confirmed — theater_ratio ≥ 0.70 justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_correlation, empirical, 'Whether regulatory enforcement actually reduces pollution').

omega_variable(
    groundwater_timescale_asymmetry,
    'What is the actual lag between contamination of shallow groundwater and appearance in drinking water wells? Does this lag create an institutional window for continued extraction despite eventual harm?',
    'Tracer studies in contaminated aquifers; modeling of contaminant migration timescales in regional hydrogeology; comparison of contamination discovery date vs. date of initial pollution source activation',
    'If lag > 20 years: institutional actors can externalize costs across generations (classification toward snare for long-horizon victims). If lag < 5 years: feedback is faster, constraint is more rapidly reversible, classification toward scaffold or rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(groundwater_timescale_asymmetry, empirical, 'Groundwater contamination lag time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agricultural_water_pollution, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agwp_tr_t0, agricultural_water_pollution, theater_ratio, 0, 0.32).
narrative_ontology:measurement(agwp_tr_t20, agricultural_water_pollution, theater_ratio, 20, 0.4).
narrative_ontology:measurement(agwp_tr_t40, agricultural_water_pollution, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(agwp_be_t0, agricultural_water_pollution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(agwp_be_t20, agricultural_water_pollution, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(agwp_be_t40, agricultural_water_pollution, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agricultural_water_pollution, resource_allocation).
narrative_ontology:boltzmann_floor_override(agricultural_water_pollution, 0.12).
narrative_ontology:affects_constraint(agricultural_water_pollution, groundwater_nitrate_depletion).
narrative_ontology:affects_constraint(agricultural_water_pollution, aquatic_eutrophication_cascade).
narrative_ontology:affects_constraint(agricultural_water_pollution, pesticide_bioaccumulation).
narrative_ontology:affects_constraint(agricultural_water_pollution, agricultural_subsidy_structure).

% DUAL FORMULATION NOTE:
% Agricultural water pollution decomposes into multiple structurally distinct constraints: nutrient runoff (high extractiveness, fast timescale), persistent organic pollutants (very high extractiveness, multi-generational timescale), and pesticide acute toxicity (moderate extractiveness, immediate timescale). Each has different ε values reflecting different victim timescales and beneficiary duration. This story represents the aggregate constraint family; see linked stories for component mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agricultural_water_pollution, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
