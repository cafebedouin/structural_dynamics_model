% ============================================================================
% CONSTRAINT STORY: global_water_bankruptcy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_water_bankruptcy, []).

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
 *   constraint_id: global_water_bankruptcy
 *   human_readable: The Global Water Bankruptcy Constraint
 *   domain: environmental/economic
 *
 * SUMMARY:
 *   The global water bankruptcy constraint describes the structural condition
 *   where humanity's aggregate water demand exceeds the renewable freshwater
 *   supply on a sustained basis. This manifests as aquifer depletion
 *   (Ogallala, North China Plain, Middle East), river collapse (Colorado,
 *   Yellow, Indus), and seasonal water scarcity affecting 2+ billion people.
 *   The constraint exhibits multiple structural dimensions: it is
 *   simultaneously a coordination problem (water allocation rules and
 *   infrastructure investment), an extraction mechanism (beneficiaries
 *   capture discounted water while victims bear depletion costs), a degraded
 *   governance system (international frameworks lack enforcement), a
 *   reversible temporary problem (technology and behavior change could
 *   restore balance), and an irreversible civilizational boundary (if
 *   critical aquifers pass recovery thresholds). The theater ratio (0.58)
 *   reflects that policy responses often emphasize performative
 *   sustainability targets (SDG 6, basin management plans) while actual
 *   behavior and infrastructure remain extraction-oriented. The
 *   extractiveness level (0.58) captures that the constraint transfers
 *   hydrological capital from future generations and powerless communities to
 *   industrial agriculture and high-consumption economies through historical
 *   allocation rules, subsidy structures, and enforcement asymmetries.
 *
 * KEY AGENTS:
 *   - Aquifer-dependent communities (powerless/trapped): farmers, herders, rural populations in arid/semi-arid regions dependent on groundwater with no exit option
 *   - Industrial agricultural sector (moderate/constrained): intensive irrigation operations dependent on subsidized water, can partially shift to less water-intensive practices but face capital barriers
 *   - High-income industrial/urban users (institutional/arbitrage): wealthy nations with technology access, water import options, and political power to secure allocation
 *   - Fossil fuel & mining industries (powerful/arbitrage): extract water for operations, externalize pollution and depletion costs, have capital for relocation or technology adaptation
 *   - International water governance institutions (institutional/constrained): river basin commissions, UNESCO, UN frameworks with weak enforcement capacity
 *   - Water-saving technology coalitions (organized/mobile): engineers, NGOs, water utilities promoting efficiency, drip irrigation, recycling as exit pathway
 *   - Future generations and aquatic ecosystems (powerless/trapped): voiceless in current allocation, bear full cost of aquifer collapse and ecological degradation
 *   - Analytical observer (analytical/analytical): sees either natural hydrological law or contingent institutional arrangement depending on false summit detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_water_bankruptcy, 0.58).
domain_priors:suppression_score(global_water_bankruptcy, 0.65).
domain_priors:theater_ratio(global_water_bankruptcy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_water_bankruptcy, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_water_bankruptcy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(global_water_bankruptcy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_water_bankruptcy, tangled_rope).
narrative_ontology:human_readable(global_water_bankruptcy, "The Global Water Bankruptcy Constraint").
narrative_ontology:topic_domain(global_water_bankruptcy, "environmental/economic").

domain_priors:requires_active_enforcement(global_water_bankruptcy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, industrial_agricultural_sector).
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, high_consumption_wealthy_nations).
narrative_ontology:constraint_beneficiary(global_water_bankruptcy, fossil_fuel_extraction_industries).
narrative_ontology:constraint_victim(global_water_bankruptcy, aquifer_dependent_communities).
narrative_ontology:constraint_victim(global_water_bankruptcy, seasonal_river_basin_populations).
narrative_ontology:constraint_victim(global_water_bankruptcy, future_generations).
narrative_ontology:constraint_victim(global_water_bankruptcy, aquatic_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AQUIFER-DEPENDENT COMMUNITY (SNARE) — Trapped by geography and water-dependent livelihoods (farming, fishing, pastoral communities). No exit option: migration is economically and culturally catastrophic. Faces full extraction as aquifers deplete and water tables collapse. Maximum suppression: no alternatives exist within the local spatial scope.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AGRICULTURAL SECTOR (TANGLED ROPE) — Moderate power at regional level. Benefits from current water allocation rules and subsidized irrigation (coordination function: ensures food production for growing populations). Constrained exit: can partially shift to less water-intensive crops or drip irrigation, but capital costs are high and regional trade barriers limit mobility. Faces extraction as water becomes scarcer and extraction costs rise. Mixed experience: both benefits and costs.
constraint_indexing:constraint_classification(global_water_bankruptcy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDUSTRIAL WATER USERS (ROPE) — Institutional power, global arbitrage options (water import embedded in trade, desalination technology access, water rights markets). Experiences constraint as solvable coordination problem: secure water supply for industrial/urban consumption requires infrastructure investment and international agreements. Net beneficiary of current allocation rules through historical precedent and economic power. Low effective extraction for this agent.
constraint_indexing:constraint_classification(global_water_bankruptcy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL WATER GOVERNANCE (PITON) — Weak institutional power constrained by sovereignty and enforcement limitations. Theater ratio elevated: river basin commissions, UNESCO water protocols, and sustainability frameworks (SDG 6) perform governance without effective enforcement mechanisms. Actual function (allocating scarce water fairly) has degraded as demand outpaces supply; governance machinery persists through institutional inertia despite limited ability to prevent overdraft. Theater reflects gap between formal agreements and on-ground realities.
constraint_indexing:constraint_classification(global_water_bankruptcy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WATER-SAVING TECHNOLOGY COALITION (SCAFFOLD) — Organized agents (agricultural technologists, water utilities, NGOs promoting drip irrigation and water recycling). See the bankruptcy constraint as a temporary coordination failure solvable through technology diffusion and behavioral change. Sunset clause embedded: as smart irrigation, water recycling, and demand-reduction technologies mature (estimated 15-20 years), the gap between supply and demand narrows. Low effective extraction because coalition has exit path and sees institutional evolution.
constraint_indexing:constraint_classification(global_water_bankruptcy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: FOSSIL FUEL & MINING INDUSTRIES (TANGLED ROPE) — Powerful actors at global scale with arbitrage options (can relocate water-intensive operations, access water markets, lobby for allocation rules favoring extraction). Both benefit and extract: benefit from subsidized water in hosting countries and lack of extraction cost internalization; extract through pollution, aquifer depletion, and environmental externalities imposed on powerless communities. High effective extraction but not maximum because they have exit options and face growing regulatory pressure.
constraint_indexing:constraint_classification(global_water_bankruptcy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: HYDROLOGICAL NATURAL LAW (FALSE SUMMIT) — Civilizational/universal analytical perspective frames water bankruptcy as immutable: global renewable freshwater is finite (~37,500 km³/year), human consumption (~4,000 km³/year) grows with population, and evapotranspiration losses are fixed by climate physics. From this view, bankruptcy is a natural consequence of exceeding planetary boundaries—unavoidable, universal, unchangeable. However, this naturalizes what is actually a contingent system: consumption is driven by agricultural subsidies, industrial inefficiency, allocation rules, and consumption patterns—all human choices. The engine's false summit detector identifies this as naturalization of a contingent institutional arrangement disguised as hydrological law.
constraint_indexing:constraint_classification(global_water_bankruptcy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: FUTURE GENERATIONS & ECOSYSTEMS (SNARE) — Powerless and temporally trapped: cannot participate in current water allocation decisions, cannot exit. Forced to inherit depleted aquifers, collapsed river systems, and ecological collapse. No agency in water governance. Maximum extraction: all costs of current overshoot imposed on agents with zero power and zero voice in the system.
constraint_indexing:constraint_classification(global_water_bankruptcy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_water_bankruptcy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_water_bankruptcy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_water_bankruptcy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_water_bankruptcy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_water_bankruptcy, TR),
    TR >= 0.70.

:- end_tests(global_water_bankruptcy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint transfers hydrological capital from aquifer reserves (accumulated over millennia) to current consumption, imposing asymmetric costs on future generations and aquifer-dependent communities. The original research suggested values between 0.32 (1975, early recognition of aquifer depletion) to 0.58 (present, widespread overdraft and ecological collapse visible). The value reflects that extraction is real and increasing—aquifers deplete irreversibly while beneficiaries externalize costs—but not maximum because some regions have adapted, some consumption can be reduced without catastrophic harm, and technology offers partial solutions. Suppression (0.65): High. Significant barriers to exit include: (1) geographic lock-in for aquifer-dependent communities; (2) capital barriers to technology adoption; (3) subsidy structures making water artificially cheap; (4) political power asymmetries favoring high-consumption nations in allocation negotiations; (5) coordination problems requiring enforcement that governance systems lack. Theater ratio (0.58): Moderate. Governance and policy responses emphasize performance metrics (SDG 6, water use efficiency targets, basin plans) that are often decoupled from actual hydrological outcomes. For example, 'water stress' indices show many regions as 'sustainable' despite aquifer collapse because metrics measure current supply vs. demand, not long-term reserve depletion. International agreements perform commitment without enforcement capacity. Technology adoption claims often exceed actual implementation. The theater has increased over the interval as visible depletion has forced policy responses but actual demand reduction has lagged targets.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between victim (Snare) and beneficiary (Rope) perspectives is maximal. An aquifer-dependent farmer in the Indus basin sees a Snare because they face complete loss of livelihood as the aquifer depletes—extraction is total and no exit exists. A water-using industrial corporation in California sees Rope because they experience the constraint as a solvable coordination problem: build desalination plants, import virtual water through trade, invest in efficiency. Both are looking at the same hydrological reality (overdrafted aquifers, river collapse), but their structural positions within the extraction mechanism produce opposite classifications. The piton perspective adds insight: international water governance performs its role (producing basin plans, international agreements) without actually preventing overdraft, suggesting institutional degradation—the machinery persists through inertia even as its primary function (sustainable allocation) fails. The mountain perspective (false summit) reveals a common naturalization error: treating hydrological limits as immutable when actual extraction is driven by contingent allocation rules, subsidies, and consumption patterns.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Aquifer-dependent communities: victims + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → high chi. Agricultural sector: split status (partial beneficiary via subsidies, partial victim via scarcity) + constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 → moderate chi. Industrial users: beneficiaries + arbitrage → d ≈ 0.15 → f(d) ≈ -0.01 → negative/minimal chi. International governance: constrained + neither pure beneficiary nor victim → d ≈ 0.50 → f(d) ≈ 0.65 → moderate chi. Technology coalition: organized + mobile exit → d ≈ 0.35 → f(d) ≈ 0.30 → low chi. Fossil fuel industries: beneficiaries + arbitrage but constrained by regulation → d ≈ 0.40 → f(d) ≈ 0.40 → low-moderate chi. Future generations: victims + trapped temporally → d ≈ 0.98 → f(d) ≈ 1.40 → maximum chi. The scope modifier σ(S) amplifies extraction at global scale (σ=1.2) relative to local scale (σ=0.8) because verification of sustainability is harder across transnational systems. This amplification is appropriate: local water overdraft can be observed and corrected; global aquifer depletion is invisible until collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in global water bankruptcy is: 'Is this Tangled Rope (hybrid coordination-extraction) or pure Snare (extraction masquerading as coordination)?' The claimed type is Tangled Rope. The resolution depends on whether the coordination function (allocating water to support agriculture and industry) is genuine or purely a cover for extraction. Evidence for Tangled Rope: water allocation rules do solve a real coordination problem—organizing distributed water access to support growing populations. Many actors (farmers, water utilities) genuinely benefit from infrastructure and rules. Evidence for Snare: the coordination benefit flows primarily to institutional/powerful actors with exit options, while victims (aquifer-dependent communities, future generations) bear full depletion cost. The beneficiaries externalize costs through subsidy structures and historical allocation rights. The suppression (0.65) is high enough that alternatives are blocked: powerless communities cannot choose desal or import options. The theater ratio (0.58) suggests governance performance without function: international agreements are produced but not enforced. Mandatrophy resolution: The constraint is genuinely Tangled Rope—real coordination function exists (water allocation does enable food production and economic activity)—but the extraction component is severe enough that many victims experience it as Snare. The perspectival gap is the answer: this IS a hybrid, but the hybrid favors extraction over coordination from the perspective of those with maximum suppression and no exit. The technology coalition's Scaffold perspective and the international governance's Piton perspective together suggest a possible exit path (demand reduction + technology adoption), which confirms the Tangled Rope classification—if it were pure Snare with no possible exit, there would be no Scaffold perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    virtual_water_trade_counterfactual,
    'If water-intensive agriculture in arid regions is eliminated and replaced with imports from water-rich regions, does the global bankruptcy constraint dissolve or relocate?',
    'Accounting of virtual water embodied in international trade; analysis of whether trade-based redistribution reduces global overdraft or merely displaces it; climate/crop-suitability modeling for alternative production regions',
    'If dissolved: the constraint is primarily a misallocation problem (Rope from stronger perspectives, Scaffold from organized view). If merely relocated: the constraint is structural to global hydrological carrying capacity (Snare/Mountain from all perspectives except beneficiaries). Classification could shift from Tangled Rope to either Rope or Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(virtual_water_trade_counterfactual, empirical, 'Whether virtual water trade solves or relocates global water bankruptcy').

omega_variable(
    aquifer_recharge_acceleration_possibility,
    'Can technological and ecological interventions (aquifer recharge wells, wetland restoration, groundwater mining reduction) meaningfully reverse aquifer depletion at a scale comparable to current overdraft rates?',
    'Comparative analysis of historical aquifer recovery rates vs. current depletion rates; pilot projects of large-scale recharge infrastructure; climate-adjusted precipitation forecasting for aquifer-dependent regions',
    'If acceleration feasible: bankruptcy is reversible within current institutional framework (Scaffold/Rope perspectives strengthened). If not: bankruptcy is unidirectional and requires demand reduction (Snare/Mountain confirmed). Theater ratio may indicate false hope if interventions are performative rather than functional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aquifer_recharge_acceleration_possibility, empirical, 'Whether aquifer recharge can reverse depletion at scale').

omega_variable(
    consumption_reduction_feasibility,
    'Is reducing global water demand by 20-30% (to restore sustainability) achievable through behavioral change, technology, and policy without catastrophic food insecurity or energy transition failure?',
    'Integrated assessment models (IAMs) linking water, food, energy, and socioeconomic systems; scenarios for diet change, irrigation efficiency, industrial conservation, and leak reduction; political economy analysis of who bears costs',
    'If feasible with distributed costs: Scaffold classification confirmed, sunset clause credible. If feasible only if high-income nations bear costs: Tangled Rope (extraction of sustainability costs onto poor nations) confirmed. If infeasible: bankruptcy is structural and extractive (Snare/Mountain), not solvable coordination problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumption_reduction_feasibility, empirical, 'Feasibility of demand reduction for water sustainability').

omega_variable(
    allocation_rule_distributional_justice,
    'Are current water allocation rules (riparian rights, prior appropriation, international river treaties) extractive by design, or are they neutral coordination mechanisms distorted by enforcement failures?',
    'Historical analysis of allocation rule origins and beneficiaries; comparison of alternative allocation frameworks (equality, need-based, sustainability-first); fairness assessment by affected communities',
    'If extractive by design: the constraint is a Snare for powerless actors—the allocation rule itself is the enforcement mechanism. If neutral but broken: the constraint is Tangled Rope with possibility of reform to Rope via new rules. Directionality of extraction hinges on this classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allocation_rule_distributional_justice, conceptual, 'Whether allocation rules are extractive by design or distorted in enforcement').

omega_variable(
    climate_change_aquifer_vulnerability,
    'Will climate change (altered precipitation, increased evapotranspiration) make aquifer recovery impossible even if demand is reduced, creating irreversible path dependence?',
    'Climate modeling with aquifer recharge scenarios; identification of critical precipitation thresholds; assessment of which aquifers are already past recovery points',
    'If yes for major aquifers: bankruptcy becomes civilizational-scale Mountain (irreversible). If no: current state is reversible Snare/Tangled Rope that can be managed. Theater ratio may mask fatalism disguised as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_change_aquifer_vulnerability, empirical, 'Whether climate change creates irreversible aquifer depletion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_water_bankruptcy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwb_tr_t0, global_water_bankruptcy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gwb_tr_t25, global_water_bankruptcy, theater_ratio, 25, 0.48).
narrative_ontology:measurement(gwb_tr_t50, global_water_bankruptcy, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(gwb_be_t0, global_water_bankruptcy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gwb_be_t25, global_water_bankruptcy, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(gwb_be_t50, global_water_bankruptcy, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_water_bankruptcy, resource_allocation).
narrative_ontology:affects_constraint(global_water_bankruptcy, agricultural_subsidy_lock_in).
narrative_ontology:affects_constraint(global_water_bankruptcy, aquifer_dependency_fragility).
narrative_ontology:affects_constraint(global_water_bankruptcy, irrigation_infrastructure_path_dependence).
narrative_ontology:affects_constraint(global_water_bankruptcy, geopolitical_water_conflict).
narrative_ontology:affects_constraint(global_water_bankruptcy, food_security_water_coupling).

% DUAL FORMULATION NOTE:
% Global water bankruptcy decomposes into regional constraint families: (1) Aquifer-specific depletion (North China Plain ε≈0.65, Ogallala ε≈0.55, Middle East ε≈0.70) with higher extractiveness and suppression due to geographic lock-in; (2) River basin over-allocation (Nile, Indus, Colorado ε≈0.52) with institutional governance failures; (3) Industrial-urban competition with agricultural users (ε≈0.45) showing lower extractiveness due to technology/arbitrage options. This story focuses on the aggregate global constraint; regional stories capture local extractiveness variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_water_bankruptcy, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
