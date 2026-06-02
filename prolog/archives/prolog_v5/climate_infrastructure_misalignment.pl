% ============================================================================
% CONSTRAINT STORY: climate_infrastructure_misalignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_infrastructure_misalignment, []).

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
 *   constraint_id: climate_infrastructure_misalignment
 *   human_readable: Climate Infrastructure Misalignment Between Decarbonization Commitments and Physical Asset Lifecycles
 *   domain: climate_policy/infrastructure/political_economy
 *
 * SUMMARY:
 *   The climate infrastructure misalignment constraint arises from a
 *   structural mismatch between political decarbonization commitments and the
 *   physical and economic lifecycles of energy, transport, and building
 *   infrastructure. Nations commit to net-zero by 2050 while maintaining
 *   operating and investment frameworks that extend fossil fuel asset
 *   lifespans to 2040-2060. This creates a dual extraction mechanism: (1)
 *   incumbent fossil fuel owners capture continued profitability from assets
 *   nominally committed to retirement, and (2) cumulative emissions lock-in
 *   is imposed on future generations and climate-vulnerable present
 *   populations. The constraint exhibits both genuine coordination functions
 *   (electricity grids, transport networks, industrial heat systems are
 *   shared infrastructure) and asymmetric extraction (benefits flow to
 *   incumbent owners and high-emitting societies; costs flow to future
 *   populations and low-emitting nations). The theatrical component (0.58)
 *   reflects extensive climate planning, renewable commitments, and climate
 *   reporting with limited de facto infrastructure replacement at the pace
 *   required by climate physics. Over a 20-year interval, extractiveness
 *   increased from 0.35 to 0.58 as the gap between committed targets and
 *   actual deployment widened, indicating accumulation of extraction rather
 *   than genuine transition. Theater ratio rose from 0.42 to 0.58, showing
 *   increasing performative content relative to functional replacement.
 *
 * KEY AGENTS:
 *   - Future Climate Stability: Primary victim (powerless/trapped) — cannot exit the cumulative emissions pathway; bears all costs of infrastructure lock-in
 *   - Developing Nations and Climate-Vulnerable Populations: Primary victim (powerless/trapped) — imposed infrastructure decisions from high-emitting nations; bear disproportionate adaptation costs
 *   - Mid-Income Industrial Nations: Secondary victim (moderate/constrained) — face transition costs and stranded asset risks; can exit but at high economic price
 *   - Fossil Fuel Infrastructure Owners: Primary beneficiary (institutional/arbitrage) — capture continued profits from assets and can diversify to avoid stranding; arbitrage options preserve value
 *   - Renewable Energy Developers: Secondary beneficiary (institutional/arbitrage) — market opportunity created by misalignment; can deploy alternatives at scale without incumbent permission
 *   - Energy Regulators and Grid Operators: Institutional actor (institutional/constrained) — maintain performative decarbonization while operationally prioritizing incumbent viability; constrained by grid stability requirements
 *   - Climate Movement and Carbon Accountability Advocates: Organized victim (organized/mobile) — can mobilize capital and policy pressure but constrained by incumbent infrastructure's political entrenchment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy pace as immutable infrastructure physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_infrastructure_misalignment, 0.58).
domain_priors:suppression_score(climate_infrastructure_misalignment, 0.62).
domain_priors:theater_ratio(climate_infrastructure_misalignment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_infrastructure_misalignment, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_infrastructure_misalignment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_infrastructure_misalignment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_infrastructure_misalignment, tangled_rope).
narrative_ontology:human_readable(climate_infrastructure_misalignment, "Climate Infrastructure Misalignment Between Decarbonization Commitments and Physical Asset Lifecycles").
narrative_ontology:topic_domain(climate_infrastructure_misalignment, "climate_policy/infrastructure/political_economy").

domain_priors:requires_active_enforcement(climate_infrastructure_misalignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_infrastructure_misalignment, incumbent_fossil_fuel_infrastructure_owners).
narrative_ontology:constraint_beneficiary(climate_infrastructure_misalignment, carbon_intensive_industry_sectors).
narrative_ontology:constraint_victim(climate_infrastructure_misalignment, future_climate_stability).
narrative_ontology:constraint_victim(climate_infrastructure_misalignment, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_infrastructure_misalignment, carbon_constrained_societies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE CLIMATE STABILITY (SNARE) — Trapped in cumulative carbon pathway. The constraint forces continued emissions lock-in through infrastructure that cannot exit without systemic coordination failure. Each year of delayed transition increases warming trajectory; no exit option until entire infrastructure fleet is retired. Bears maximum extraction cost in the form of forced climate adaptation burden.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATIONS AND CLIMATE-VULNERABLE POPULATIONS (SNARE) — Trapped by infrastructure decisions made by high-emitting nations before their political voice existed. Face maximum adaptation costs, bear disproportionate climate impacts, have no exit option and minimal voice in the infrastructure replacement timeline. Extraction runs entirely in one direction — costs imposed, benefits captured elsewhere.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-INCOME INDUSTRIAL NATIONS (TANGLED ROPE) — Constrained by simultaneous economic development and climate commitments. Benefit from infrastructure coordination (electricity systems, transport networks are genuine shared goods) but bear extraction through stranded asset risk and transition cost asymmetry. Can exit but at high economic price — constrained rather than trapped.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE ENERGY DEVELOPERS AND GREEN TECHNOLOGY FIRMS (ROPE) — Net beneficiaries of the infrastructure misalignment. The constraint creates the market opportunity for renewable infrastructure replacement. Arbitrage exit option: technology deployment bypasses incumbent infrastructure without needing its permission. Experience low extraction because they can deploy alternatives at scale — the misalignment creates profitable coordination opportunities rather than constraining them.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOSSIL FUEL INFRASTRUCTURE OWNERS (ROPE) — Primary beneficiaries with arbitrage exit option. Constraint permits continued profitability from assets nominally committed to retirement. Can exit by diversifying into gas, renewables, or financial assets — arbitrage option preserves value. Experience the constraint as pure coordination: infrastructure sharing, grid access, regulatory predictability. Negative effective extraction — the constraint subsidizes them.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENERGY REGULATORS AND GRID OPERATORS (PITON) — Theater ratio high (0.58): extensive planning, modeling, and commitment announcements with minimal de facto infrastructure replacement. Regulators perform decarbonization commitment while operationally prioritizing grid stability through incumbent infrastructure. The constraint persists through institutional inertia — transition protocols exist but are enacted at pace that preserves incumbent viability rather than maximizes climate benefit. Constrained exit: cannot remove incumbent infrastructure immediately without grid failure risk.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CLIMATE MOVEMENT AND CARBON ACCOUNTABILITY ADVOCATES (TANGLED ROPE) — Organized agents with some mobility (can deploy capital, pressure policy, coordinate markets) but constrained by incumbent infrastructure's political entrenchment and financial scale. Genuine coordination function exists: accountability frameworks, divestment campaigns, and climate commitments improve information and incentive alignment. But extraction flows through entrenchment: incumbent infrastructure can avoid rapid replacement precisely because accountability mechanisms lack enforcement power. Medium extraction derived from constrained exit relative to institutional opponents.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scope, infrastructure capital stock turnover is a mathematical constraint on transition speed: buildings last 50+ years, power plants 40+ years, transport fleets 15+ years. Replacement cannot physically occur faster than depreciation cycles permit. This perspective naturalizes the misalignment as an immutable property of infrastructure physics. However, the structural data contradicts mountain classification — the engine will flag this as false summit, revealing that the *pace* of replacement (which varies from 2% to 8% per year by sector and jurisdiction) is a contingent policy choice, not a natural law.
constraint_indexing:constraint_classification(climate_infrastructure_misalignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_infrastructure_misalignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_infrastructure_misalignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_infrastructure_misalignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_infrastructure_misalignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_infrastructure_misalignment, TR),
    TR >= 0.70.

:- end_tests(climate_infrastructure_misalignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple channels: (1) incumbent infrastructure owners capture continued revenue from assets while maintaining optionality through diversification, (2) transition costs are distributed asymmetrically (poorer nations and populations bear larger shares), (3) cumulative carbon lock-in is imposed on future generations who cannot participate in the decision. The value reflects that genuine coordination functions exist (grids, networks) alongside extraction, placing it in the tangled_rope rather than snare range. Suppression (0.62): Moderate-high. Multiple suppression mechanisms: political power of incumbent capital, massive stranded asset risk preventing rapid retirement, grid integration constraints on rapid renewable deployment, and internalization of transition timelines as inevitable. However, suppression is not total — renewable capacity is deploying at increasing rates, and some early retirement is occurring, indicating the constraint is enforced rather than natural. Theater ratio (0.58): Moderate-high. Extensive planning and commitment announcements (net-zero targets, climate investment pledges, grid modernization roadmaps) with limited de facto replacement pace. The theater has increased over the interval as gap between commitments and outcomes widened. Actual replacement rates of 3-5% annually across incumbent energy infrastructure fall well below the 8-12% annual pace required by 2050 decarbonization targets, yet this is presented as physically necessary rather than politically chosen.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives is extreme. Incumbent infrastructure owners perceive coordination and arbitrage opportunity (Rope): shared grids and networks are genuine public goods, and their exit options (diversification) are available and valuable. Future generations and vulnerable populations perceive pure extraction and entrapment (Snare): they cannot exit the carbon lock-in imposed by current infrastructure choices and will bear full adaptation costs. Mid-income nations occupy the middle ground (Tangled Rope): genuine coordination value from electricity and transport infrastructure but extraction through stranded asset risk and transition cost asymmetry. Climate advocates perceive mixed signals: they benefit from institutional attention to climate (opportunity to mobilize capital and policy) but are constrained by incumbent infrastructure's political power (Tangled Rope). Regulators perceive ritual and inertia (Piton): planning processes are extensive but operationally minimal relative to climate targets. The analytical observer's risk is false naturalization: treating the infrastructure replacement pace as physically inevitable rather than recognizing it as a policy choice that maintains incumbent profitability.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain flows from structural relationship, not from rhetorical commitment. Incumbent fossil fuel owners benefit from continued infrastructure viability and have arbitrage options (diversification into gas, renewables, financial assets, geographic relocation of capital). Their d value is low (0.10-0.15), producing negative effective extraction — the constraint subsidizes them. Renewable developers benefit from the infrastructure replacement market and have arbitrage deployment options. Their d value is similarly low (0.15-0.20). Climate advocates are organized (they can mobilize capital, pressure policy, coordinate markets) but constrained by incumbent infrastructure's political and financial scale relative to their own resources. Their d value is moderate-high (0.55-0.65), producing moderate extraction. Mid-income industrial nations benefit from infrastructure coordination (shared electricity grids, transport networks) but bear asymmetric transition costs and stranded asset risk. Their d value is moderate-high (0.60-0.70), producing moderate-high extraction. Climate-vulnerable populations and future generations face maximum extraction: they cannot exit the cumulative emissions pathway imposed by current infrastructure choices, they did not participate in infrastructure decisions, and they bear disproportionate adaptation costs. Their d values are very high (0.90-0.98), producing maximum f(d) and maximum experienced extraction. The piton classification for regulators (institutional/constrained) derives from the theater gate: their operationally performative role (extensive planning with limited de facto pace change) indicates institutional inertia despite policy commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint is not a false coordination claim; the coordination function is genuine. Electricity grids, transport networks, and industrial heat systems are authentic shared infrastructure that require coordination. The misalignment cannot be resolved by denying the coordination — it must be resolved by recognizing that coordination and extraction coexist (Tangled Rope classification) and by identifying the mechanisms through which extraction is enforced despite coordination's existence. The constraint perpetuates because: (1) incumbent infrastructure owners have genuine arbitrage options (diversification) that decouple their welfare from transition pace, so they can lobby for slowness while claiming support for climate goals, (2) grid stability requirements genuinely constrain rapid incumbent retirement, creating a real (not imagined) coordination problem, but this real problem is operationally prioritized over climate targets through policy choice, (3) theater masks the extraction: extensive climate planning and commitment announcements create the appearance of transition while operationally maintaining incumbent viability. Mandatrophy resolution requires separating the genuine coordination function (grids, networks must be managed) from the extraction mechanism (incumbent protection through policy prioritization of grid stability over climate pace). The extraction could be reduced by: accelerating renewable deployment to reduce incumbent dependence, implementing strict early retirement requirements with transition support, taxing carbon at levels that make early retirement economically rational, or decoupling grid stability from incumbent infrastructure dependence through storage and demand flexibility. Each intervention maintains coordination while reducing extraction. The false summit risk (Mountain classification from civilizational analytical view) is the most dangerous mandatrophy failure — if infrastructure replacement pace is naturalized as a physical law, policy intervention becomes unthinkable and the constraint becomes self-perpetuating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retirement_speed_policy_vs_physics,
    'Is the infrastructure retirement pace constrained by physical depreciation rates or by political economy of incumbent protection?',
    'Comparative analysis of replacement timelines across jurisdictions with different political structures; modeling of technical feasibility vs actual policy-set retirement rates; cost analysis of accelerated replacement vs observed policy choices',
    'If physics-dominated: mountain classification is correct, transition speed is naturally limited. If policy-dominated: piton/tangled_rope classification is correct, slowness is contingent institutional choice. Misidentifying the constraint''s nature prevents effective intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retirement_speed_policy_vs_physics, empirical, 'Whether infrastructure retirement pace is physics-constrained or policy-chosen').

omega_variable(
    stranded_asset_probability_calibration,
    'What is the actual probability that incumbent fossil fuel assets become stranded before end-of-life depreciation, and does this probability affect investment behavior?',
    'Historical data on asset-level retirement patterns; statistical analysis of early retirement vs scheduled retirement by technology and jurisdiction; correlation between stranded asset risk perception and capital allocation decisions',
    'If stranding probability > 30%: incumbent infrastructure owners face material risk, arbitrage exit option is genuine, and Rope classification reflects true beneficiary experience. If stranding probability < 10%: arbitrage is illusory, extraction is obscured, and classification should shift toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_probability_calibration, empirical, 'Probability and timing of asset stranding for fossil fuel infrastructure').

omega_variable(
    renewable_deployment_bottleneck_source,
    'What is the actual limiting factor on renewable energy deployment: physical manufacturing capacity, grid integration difficulty, or incumbent market power and regulatory barriers?',
    'Time-series analysis of deployment rates vs supply chain capacity; engineering studies on grid integration challenges; comparison of deployment rates in markets with different regulatory structures and incumbent power',
    'If bottleneck is manufacturing/grid: the constraint is coordination problem (Rope from renewable perspective). If bottleneck is incumbent power: the constraint is extraction mechanism (Snare/Tangled Rope). This determines whether the misalignment perpetuates due to natural limits or due to enforced institutional resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_deployment_bottleneck_source, empirical, 'Source of renewable deployment bottleneck: physical or institutional').

omega_variable(
    carbon_accountability_enforcement_threshold,
    'At what level of carbon pricing, regulatory stringency, or financial penalty do incumbent infrastructure owners actually accelerate retirement rather than lobbying for exemptions?',
    'Policy event analysis: carbon tax increases, emissions trading price spikes, regulatory tightening; correlation with capital allocation and retirement decisions; threshold identification where political resistance breaks',
    'If threshold exists at affordable price: organized advocates have genuine mobility (tangled rope classification holds, extract able value). If threshold is never crossed politically: advocates are trapped, extraction is asymmetric (Snare classification). Identifies whether constraint perpetuates due to technical transition cost or due to political economy of incumbent capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_accountability_enforcement_threshold, empirical, 'Carbon price threshold at which incumbent infrastructure accelerates retirement').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) structural (incumbent political power, capital requirements, grid integration constraints) or internalized (societies accept incumbent timelines as inevitable, climate advocates have internalized powerlessness)?',
    'Post-policy-shift analysis: if suppression is structural, rapid policy change should enable rapid deployment despite incumbent opposition. If suppression is internalized, rapid deployment may not occur even with policy permission and capital availability, because actors have accepted incumbent narratives about feasibility limits.',
    'If structural: constraint can be broken by policy and capital reallocation alone. If internalized: constraint persists through belief alignment even after institutional barriers fall — requires deliberate cognitive reframing and institutional narrative change. Identifies whether remediation is primarily policy/capital or primarily cognitive/cultural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, conceptual, 'Structural vs internalized suppression in climate infrastructure transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_infrastructure_misalignment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_infrastructure_misalignment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clim_tr_t10, climate_infrastructure_misalignment, theater_ratio, 10, 0.55).
narrative_ontology:measurement(clim_tr_t20, climate_infrastructure_misalignment, theater_ratio, 20, 0.58).
narrative_ontology:measurement(clim_tr_t5, climate_infrastructure_misalignment, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_infrastructure_misalignment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t10, climate_infrastructure_misalignment, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(clim_be_t20, climate_infrastructure_misalignment, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(clim_be_t5, climate_infrastructure_misalignment, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_infrastructure_misalignment, global_infrastructure).
narrative_ontology:affects_constraint(climate_infrastructure_misalignment, carbon_lock_in_mechanism).
narrative_ontology:affects_constraint(climate_infrastructure_misalignment, stranded_asset_trajectory).
narrative_ontology:affects_constraint(climate_infrastructure_misalignment, incumbent_regulatory_capture).
narrative_ontology:affects_constraint(climate_infrastructure_misalignment, renewable_deployment_bottleneck).

% DUAL FORMULATION NOTE:
% Climate infrastructure misalignment decomposes into four constraint stories: carbon lock-in (cumulative emissions physics, ε=0.08, Mountain), stranded asset trajectory (fossil fuel balance sheet exposure, ε=0.52, Tangled Rope), incumbent regulatory capture (political economy of grid operator entrenchment, ε=0.65, Snare), and renewable deployment bottleneck (manufacturing/integration vs incumbent market power, ε=0.48, Tangled Rope). This story models the integrated constraint; each sub-story has its own measurement trajectory and structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_infrastructure_misalignment, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
