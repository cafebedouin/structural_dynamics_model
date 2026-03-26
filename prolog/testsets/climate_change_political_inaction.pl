% ============================================================================
% CONSTRAINT STORY: climate_change_political_inaction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_change_political_inaction, []).

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
 *   constraint_id: climate_change_political_inaction
 *   human_readable: Climate Change Political Inaction Constraint
 *   domain: climate_policy/political_economy
 *
 * SUMMARY:
 *   The constraint of climate change political inaction represents a
 *   structural mechanism that extracts from future generations and
 *   climate-vulnerable populations while benefiting fossil fuel capital and
 *   existing carbon-intensive economic actors. The constraint manifests as a
 *   systematic failure to implement greenhouse gas mitigation despite
 *   scientific consensus, signed international commitments (Paris Agreement),
 *   and available technologies. The extractiveness has increased over the
 *   measurement interval (0.35 → 0.68) as the cost of continued delay
 *   accumulates in lock-in effects: carbon infrastructure investments,
 *   stranded asset protection, and political narrative maintenance. Theater
 *   ratio has similarly increased (0.40 → 0.65) as performative climate
 *   action (net-zero pledges, climate agencies, international conferences,
 *   voluntary frameworks) has proliferated without corresponding emissions
 *   reductions. This gap between pledge and policy is the signature of the
 *   piton classification for bureaucratic actors — the original function
 *   (regulate emissions) has atrophied while institutional appearance
 *   persists. The constraint exhibits all six DR types from different
 *   perspectives: snare for the climate-trapped, tangled rope for climate
 *   advocates constrained by carbon-dependent economies, rope for fossil fuel
 *   beneficiaries, piton for performative bureaucracies, scaffold for
 *   renewable-transition leaders with a clear sunset, and false mountain for
 *   civilizational observers who mistake political lock-in for thermodynamic
 *   inevitability.
 *
 * KEY AGENTS:
 *   - Future Generations & Climate-Vulnerable Populations: Primary victims (powerless/trapped) — bear full cost of delayed action and accelerating climate impacts; structurally excluded from contemporary political decisions
 *   - Fossil Fuel Industry & Carbon-Intensive Capital: Primary beneficiaries (institutional/arbitrage) — capture coordination benefits of delay (asset preservation, continued rent extraction, market share protection); can arbitrage between climate-aware and climate-skeptical jurisdictions
 *   - Climate Advocates & Organized Movements: Secondary actors (moderate/constrained) — mobilize political pressure but constrained by economic dependency on carbon systems; experience mixed coordination (externality solutions) and extraction (bearing action costs)
 *   - National Climate Agencies & Environmental Bureaucracies: Institutional custodians (institutional/constrained) — maintain performative compliance (pledges, reports, voluntary frameworks) while implementing minimal policy; trapped by climate reality but not true targets
 *   - Renewable Transition Leaders (Nordic countries, California, island nations): Organized actors (organized/mobile) — have achieved rapid decarbonization and can arbitrage to clean energy dominance; experience temporary constraint with clear sunset as alternatives mature
 *   - Analytical Observer at Civilizational Scale: Universal perspective (analytical/analytical) — risks naturalizing political lock-in as immutable physical law; false summit classification reveals this as institutional contingency rather than thermodynamic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_change_political_inaction, 0.68).
domain_priors:suppression_score(climate_change_political_inaction, 0.72).
domain_priors:theater_ratio(climate_change_political_inaction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_change_political_inaction, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_change_political_inaction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_change_political_inaction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_change_political_inaction, snare).
narrative_ontology:human_readable(climate_change_political_inaction, "Climate Change Political Inaction Constraint").
narrative_ontology:topic_domain(climate_change_political_inaction, "climate_policy/political_economy").

domain_priors:requires_active_enforcement(climate_change_political_inaction).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_change_political_inaction, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(climate_change_political_inaction, carbon_intensive_capital).
narrative_ontology:constraint_victim(climate_change_political_inaction, future_generations).
narrative_ontology:constraint_victim(climate_change_political_inaction, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_change_political_inaction, ecological_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Structurally trapped; cannot exit or arbitrage the constraint. Bears full cost of atmospheric carbon load and delayed mitigation. Suppression manifests as political exclusion (no voting power in current decisions), economic dependency (lack of capital to relocate or adapt), and epistemic closure (climate denial, delay narratives). Maximum experienced extraction.
constraint_indexing:constraint_classification(climate_change_political_inaction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE ADVOCATES (TANGLED ROPE) — Moderately powered through organizing capacity but constrained by economic dependency on carbon-intensive systems. Experience genuine coordination benefits (collective action solves externality) alongside extraction (personal cost of decarbonization, precarity of climate organizing careers). Asymmetric: beneficiaries of delay capture the coordination benefits while advocates bear disproportionate enforcement costs.
constraint_indexing:constraint_classification(climate_change_political_inaction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL INTERESTS (ROPE) — Net beneficiary (institutional/arbitrage). Experiences the constraint as coordination: delay mechanisms coordinate global capital flight from renewable transition, maintain asset value, and preserve market share. Can arbitrage between climate-aware and climate-skeptical jurisdictions. Effective extraction flows toward this agent.
constraint_indexing:constraint_classification(climate_change_political_inaction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE BUREAUCRACIES (PITON) — Maintain appearance of climate action (Paris Agreement targets, net-zero commitments, climate reports) while implementing minimal policy. Theater ratio high: meetings, reports, voluntary frameworks, and pledges accumulate without decarbonization. Original function (regulate emissions) has atrophied; maintained through institutional inertia and performative compliance. Agencies are not beneficiaries (trapped by climate reality) and not true targets (have some institutional power), but custodians of a degraded ritual.
constraint_indexing:constraint_classification(climate_change_political_inaction, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RENEWABLE TRANSITION LEADERS (SCAFFOLD) — Organized jurisdictions and communities that have mobilized capital, technology, and political will for rapid decarbonization (Nordic countries, California, Costa Rica, island nations). Experience temporary constraint with clear sunset: as renewable technologies mature and cost curves flatten, the carbon lock-in dissolves. High coordination benefits (clean energy transition), declining extraction as alternatives become dominant. Theater minimal: measured by actual emissions reductions and energy system transformation, not pledges.
constraint_indexing:constraint_classification(climate_change_political_inaction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER - NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint might appear immutable: carbon physics is fixed (CO2 greenhouse effect), energy systems take decades to transform, and the rational-actor hypothesis predicts continued carbon consumption until climate damage makes alternatives cheaper. However, this perspective mistakes contingent institutional lock-in (fossil fuel subsidies, regulatory capture, political choice) for immutable physical law. The structural data reveals this as false naturalization — the inaction is politically constructed, not thermodynamically inevitable.
constraint_indexing:constraint_classification(climate_change_political_inaction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_change_political_inaction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_change_political_inaction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_change_political_inaction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_change_political_inaction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_change_political_inaction, TR),
    TR >= 0.70.

:- end_tests(climate_change_political_inaction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts from future generations and vulnerable populations through delayed mitigation, which accelerates lock-in costs and makes adaptation harder and more expensive. The trajectory shows accumulation: early delay (0.35) had lower extraction because alternatives remained plausible; as carbon infrastructure calcifies (power plants with 40-year lifespans, urban sprawl, energy subsidies), continued inaction requires increasingly active policy choices to maintain, and the cost to the victims increases nonlinearly. Suppression (0.72): Very high. Barriers to climate action include: regulatory capture of climate policy by fossil interests; information asymmetry (coordinated climate denial despite scientific consensus); economic dependency (millions of jobs in carbon sectors, energy-intensive production); and distributed harm (impacts felt across space and time, hard to organize). Suppression manifests as institutional (regulatory frameworks favor incumbent carbon infrastructure), economic (capital lock-in in fossil assets, labor trapped in carbon jobs), epistemic (denial narratives, uncertainty manufacturing), and political (campaign finance making climate policy electorally risky). Theater ratio (0.65): High and rising. The dramatic increase from 0.40 to 0.65 reflects the proliferation of performative climate action: international climate conferences that produce unenforceable pledges, net-zero commitments with no binding mechanisms, climate agencies that publish reports without implementing policy, voluntary corporate carbon neutrality claims, carbon offset markets with minimal real reduction. The theater has become so dominant that national governments can claim climate leadership while expanding fossil fuel subsidies (Australia, Canada, Saudi Arabia all sign Paris Agreement while subsidizing coal/oil). This is the signature of piton classification — the original coordination function (regulate emissions) persists in name only while the institutional apparatus performs compliance without changing behavior. The theater gap widens as extractiveness rises: more money spent on climate PR and pledge-making while less actual decarbonization occurs.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification from the perspective of climate victims is the true structural form. Trapped agents with no exit, facing suppression through political exclusion, economic dependency, and epistemic closure. The false mountain classification from the civilizational analytical perspective reveals the naturalization trap: the observer risks treating political lock-in as thermodynamic inevitability. In reality, the physics is completely changeable (we can decarbonize) but is locked behind political and economic choices that benefit the extractors (fossil fuel capital, incumbent energy infrastructure). The piton classification for bureaucratic actors reveals that climate governance has become a self-perpetuating ritual disconnected from its original function. The rope classification for beneficiaries shows that inaction is experienced as legitimate coordination by those who profit from it. The tangled rope classification for constrained advocates shows the binding mechanism — genuine coordination could solve climate change (the externality is real and solvable through collective action), but the system enforces asymmetric extraction by making advocates bear the costs of action while beneficiaries bear costs of mitigation delay. The scaffold classification for renewable leaders shows that the constraint is not inevitable — jurisdictions that have mobilized rapidly have discovered that decarbonization produces real economic benefits (lower energy costs, cleaner air, job creation), and the sunset is becoming visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values are determined by each agent's structural position relative to the extraction flow. Fossil fuel beneficiaries (d ≈ 0.05, full beneficiary) experience low effective extraction because they are the extractors — the constraint transfers wealth and environmental harm away from them. Climate victims (d ≈ 0.95, full target) experience maximum extraction because they bear 100% of the harm (rising temperatures, lost habitat, displacement) and have no exit. Climate advocates (d ≈ 0.70) experience moderate-to-high extraction — they are neither full beneficiaries nor full targets, but constrained agents trying to solve an externality while the system extracts from them through economic precarity and organizing costs. Climate bureaucracies (d ≈ 0.50, symmetric) are neither clear beneficiaries nor clear targets — they are managers of a degraded institutional process that serves nominal coordination while enabling continued extraction elsewhere. Renewable transition leaders (d ≈ 0.40) experience lower extraction than purely trapped actors because they have genuine exit options — their constraint has a sunset and they can arbitrage to clean energy dominance. The analytical observer (d ≈ 0.72) has no stake in the extraction but can see the full structure; they are attempting to adjudicate whether the constraint is immutable or changeable.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED: The constraint simultaneously exhibits genuine coordination benefits (climate change is an externality problem that collective action could solve) AND asymmetric extraction (the system delays action in ways that concentrate benefits on fossil fuel owners and costs on future generations). This is the defining structure of a snare or tangled rope, depending on perspective. The resolution: the constraint is a SNARE from the perspective of powerless victims, because the extraction dominates any coordination benefit they experience. It is a TANGLED ROPE from the perspective of organized advocates, because genuine coordination benefits exist (solving the externality would benefit everyone) alongside asymmetric extraction (advocates bear disproportionate costs). It is a ROPE from the perspective of beneficiaries, because they genuinely experience coordination (delay mechanisms coordinate capital preservation). It is a PITON from the perspective of bureaucracies, because the coordination function has atrophied into pure ritual. The mandatrophy dissolves when we recognize that the constraint is not a single indexical claim but a presheaf over multiple perspectives. The question 'is climate inaction coordination or extraction?' has different answers from different structural positions, and all are empirically true. The binding mechanism is extraction (asymmetric distribution of costs), disguised as coordination (the narrative that 'we all need time to transition'). Beneficiaries and advocates use the coordination language to justify inaction; victims and future generations are trapped by the extraction. The resolution of mandatrophy is to abandon the search for a single type and instead map the perspectival landscape: snare dominates for the powerless, piton dominates for bureaucracies, rope dominates for beneficiaries, scaffold dominates for organized leaders, and false mountain dominates for naive observers. The constraint is all six simultaneously — the presheaf is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_activation_threshold,
    'At what level of atmospheric CO2 concentration or climate-impacts severity do political actors shift from inaction to emergency mobilization?',
    'Empirical climate feedback loops and political economy analysis; historical comparison to other environmental crises (ozone hole, acid rain) where activation occurred. Monitor bifurcation points in political willingness and capital deployment.',
    'If activation threshold is low (< 2°C warming): constraint transitions from snare to temporary scaffold as politics mobilizes. If threshold is high (> 3°C): snare persists through civilizational timescale, locking in catastrophic outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tipping_point_activation_threshold, empirical, 'Political activation threshold for climate emergency response').

omega_variable(
    technology_cost_curve_autonomy,
    'Can renewable energy costs decline sufficiently to achieve grid parity and dominant market share independent of policy intervention?',
    'Empirical cost trajectory analysis; test whether solar/wind/battery deployment accelerates or decelerates absent subsidy. Compare jurisdictions with and without climate policy. Monitor capital reallocation from fossil to renewable independent of regulation.',
    'If autonomous: scaffold perspective strengthens — decarbonization proceeds despite political inaction, and the constraint''s coercive power declines. If policy-dependent: snare persists longer — inaction locks in carbon infrastructure that becomes stranded assets only through political choice to abandon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_cost_curve_autonomy, empirical, 'Whether renewable dominance requires policy or emerges autonomously').

omega_variable(
    extraction_mechanism_identity,
    'Is the primary extraction mechanism capital preservation (fossil fuel owners extracting rents to delay transition), labor capture (workers trapped in carbon jobs), or cognitive lock-in (identity fusion with carbon-intensive lifestyles)?',
    'Political economy analysis of subsidy flows and asset protection (capital); labor transition studies and job market analysis (labor); cultural identity research and values alignment surveys (cognitive). Decompose into separate constraints if mechanisms differ by agent class.',
    'If capital mechanism dominates: snare classification holds; policy must confront ownership interests directly. If labor or cognitive: piton or tangled rope from certain perspectives; policy can succeed through reframing and job guarantee. Mechanism determines intervention lever.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_identity, conceptual, 'Primary extraction mechanism: capital rent, labor lock-in, or cognitive capture').

omega_variable(
    institutional_capture_vs_structural_incentive,
    'Is political inaction driven by fossil fuel lobby capture of regulatory institutions, or by structural economic incentives that would persist even without organized industry opposition?',
    'Counterfactual analysis: compare climate policy outcomes in jurisdictions with strong lobby presence vs weak. Study periods of industry division or regulatory independence. Test whether lobbying spending correlates with policy delay independent of underlying carbon dependency.',
    'If capture: snare can be escaped through political reform and campaign finance regulation. If structural incentive: snare persists absent macroeconomic transformation (energy transition, growth decoupling). Determines whether constraint is soluble through institutional reform or requires deeper systemic change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_structural_incentive, empirical, 'Whether inaction stems from capture or structural economic incentives').

omega_variable(
    suppression_internalization_depth,
    'How much of the suppression (barriers to climate action) is externally imposed (regulatory barriers, capital constraints, information asymmetry) vs internalized by political actors (ideological commitment to markets, identity fusion with growth, cognitive dissonance avoidance)?',
    'Political psychology research; studies of cognitive dissonance in climate-aware policymakers. Test whether providing information and removing external barriers shifts behavior, or whether internal ideological commitments persist. Examine post-office trajectories of climate-skeptical politicians in new information environments.',
    'If externalized: removing barriers (campaign finance reform, carbon pricing, renewable subsidies) solves inaction. If internalized: actors carry suppression with them; identity lock makes them unable to perceive or act on climate evidence. Determines whether constraint can be escaped through institutional change or requires agent-level cognitive reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Degree to which suppression is internalized vs externally imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_change_political_inaction, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_change_political_inaction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(clim_tr_t10, climate_change_political_inaction, theater_ratio, 10, 0.55).
narrative_ontology:measurement(clim_tr_t20, climate_change_political_inaction, theater_ratio, 20, 0.65).
narrative_ontology:measurement(clim_tr_t25, climate_change_political_inaction, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_change_political_inaction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t10, climate_change_political_inaction, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_be_t20, climate_change_political_inaction, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_change_political_inaction, base_extractiveness, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_change_political_inaction, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_change_political_inaction, carbon_lock_in_infrastructure).
narrative_ontology:affects_constraint(climate_change_political_inaction, fossil_fuel_subsidy_continuation).
narrative_ontology:affects_constraint(climate_change_political_inaction, climate_denial_epistemic_closure).
narrative_ontology:affects_constraint(climate_change_political_inaction, renewable_transition_delay).

% DUAL FORMULATION NOTE:
% Climate inaction as political constraint decomposes into multiple structurally distinct mechanisms: (1) fossil fuel industry lobbying and regulatory capture (snare), (2) labor lock-in and worker precarity in carbon jobs (tangled rope), (3) infrastructure capital stranding dynamics (piton as institutions defend investments), (4) epistemic closure and denial narratives (snare as information suppression), (5) renewable transition acceleration in some jurisdictions (scaffold). Each mechanism has different ε values and different beneficiary/victim structures. This story focuses on the aggregate political constraint; see related constraint stories for domain-specific decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_change_political_inaction, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
