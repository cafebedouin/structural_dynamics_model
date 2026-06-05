% ============================================================================
% CONSTRAINT STORY: planetary_boundaries
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_boundaries, []).

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
 *   constraint_id: planetary_boundaries
 *   human_readable: Planetary Boundaries Framework
 *   domain: environmental/economic
 *
 * SUMMARY:
 *   The Planetary Boundaries framework defines a 'safe operating space for
 *   humanity' based on nine critical Earth system processes: climate change,
 *   biosphere integrity, land-system change, freshwater use, biogeochemical
 *   flows, ocean acidification, chemical pollution, atmospheric aerosol
 *   loading, and ozone depletion. The framework creates a structural tension
 *   between scientific limits and economic structures optimized for
 *   transgression. High-consumption economies benefit from boundary
 *   transgression through access to cheap energy, agricultural inputs, and
 *   raw materials. Low-income nations and future generations bear the costs
 *   without meaningful participation in the benefits. The constraint exhibits
 *   characteristics of a pure extraction mechanism (snare) from the
 *   perspective of trapped actors, but appears as coordination (rope) from
 *   high-consumption economies and as a temporary solvable problem (scaffold)
 *   from the international climate framework. The extractiveness value (0.68)
 *   reflects that the transgression is not incidental to current economic
 *   organization but structurally embedded in growth models, externality
 *   pricing, and technology asymmetry. Theater ratio (0.58) indicates that
 *   compliance mechanisms (ESG reporting, carbon offsets, sustainability
 *   certifications) have become substantially performative — institutions
 *   signal compliance while material extraction continues.
 *
 * KEY AGENTS:
 *   - High-Consumption Economies: Primary beneficiary (institutional/arbitrage) — capture benefits of transgression through cheap resources and externalized costs; institutional capacity to absorb climate impacts
 *   - Extractive Industries: Secondary beneficiary (organized/arbitrage) — directly extract value from boundary transgression; benefit from regulatory theater without functional boundary protection
 *   - Future Generations: Primary victim (powerless/trapped) — zero agency, zero exit options, maximum cost burden from compound climate and biosphere impacts
 *   - Island States and Coastal Communities: Secondary victim (powerless/trapped) — trapped by sea-level rise and ocean acidification; cannot exit, cannot reduce global emissions driving impacts
 *   - Subsistence Agricultural Communities: Mixed victim/constrained (moderate/constrained) — face extraction through soil degradation, freshwater depletion, crop failures; limited agency through local adaptation and traditional knowledge
 *   - Green Economy Coalition: Organized beneficiary (organized/mobile) — benefit from transition finance, green premiums, and technology development; mobile exit through investment reallocation
 *   - International Climate Framework: Organized coordinator (organized/constrained) — sees boundary transgression as temporary coordination failure with technology-enabled sunset; constrained by sovereignty and enforcement limits
 *   - Regulatory System: Institutional actor (institutional/constrained) — maintains environmental compliance theater; sees own mechanisms as degraded but continues through path dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_boundaries, 0.68).
domain_priors:suppression_score(planetary_boundaries, 0.72).
domain_priors:theater_ratio(planetary_boundaries, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_boundaries, extractiveness, 0.68).
narrative_ontology:constraint_metric(planetary_boundaries, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(planetary_boundaries, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_boundaries, snare).
narrative_ontology:human_readable(planetary_boundaries, "Planetary Boundaries Framework").
narrative_ontology:topic_domain(planetary_boundaries, "environmental/economic").

domain_priors:requires_active_enforcement(planetary_boundaries).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_boundaries, high_consumption_economies).
narrative_ontology:constraint_beneficiary(planetary_boundaries, extractive_industries).
narrative_ontology:constraint_victim(planetary_boundaries, future_generations).
narrative_ontology:constraint_victim(planetary_boundaries, low_lying_island_states).
narrative_ontology:constraint_victim(planetary_boundaries, subsistence_agricultural_communities).
narrative_ontology:constraint_victim(planetary_boundaries, global_biosphere).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-LYING ISLAND STATE (SNARE) — Trapped by sea-level rise and ocean acidification driven by carbon emissions originating in high-consumption economies. No exit option: cannot reduce global emissions, cannot migrate economically or legally, cannot escape climate impacts. Experiences maximum extraction through subsidized carbon-intensive growth of wealthy nations.
constraint_indexing:constraint_classification(planetary_boundaries, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Trapped across all planetary boundaries breaches. Zero exit options, zero agency in current decision-making. Bear full cost of transgression without benefit. Cannot negotiate, cannot exit, cannot represent themselves in present institutions. Paradigmatic powerless victim.
constraint_indexing:constraint_classification(planetary_boundaries, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SUBSISTENCE AGRICULTURAL COMMUNITY (TANGLED ROPE) — Constrained by phosphorus and nitrogen cycle disruption, freshwater depletion, land system change. Limited exit options (migration requires capital and legal status) but some agency through local adaptation, seed preservation, crop diversification. Experiences both extraction (forced land degradation, water scarcity) and coordination benefit (participation in agricultural commons, traditional knowledge systems). Mixed constraint.
constraint_indexing:constraint_classification(planetary_boundaries, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GREEN ECONOMY COALITION (ROPE) — Organized agents (renewable energy firms, sustainable agriculture networks, carbon markets) see the planetary boundaries as a coordination problem solvable through technology, certification, and market signals. Mobile exit options through investment reallocation. Net beneficiary through transition finance and green premium positioning. Experiences constraint as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(planetary_boundaries, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-CONSUMPTION ECONOMIES (ROPE) — Institutional actors with arbitrage capacity. Can externalize costs to other nations and future time periods. Currently benefit from transgression of boundaries (cheap energy, agricultural inputs, raw materials). Experience the framework as informational coordination mechanism: receive scientific findings, adjust policies at margin, maintain economic growth trajectory. Arbitrage options preserve beneficiary status.
constraint_indexing:constraint_classification(planetary_boundaries, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL CLIMATE FRAMEWORK (SCAFFOLD) — Paris Agreement, SDGs, and biodiversity protocols represent temporary coordinating structures with explicit sunset logic: net-zero by 2050, 1.5C stabilization goals, biodiversity recovery targets. Organized but with enforcement constraints. Theater ratio declining as implementation mechanisms mature. Sees boundary transgression as temporary coordination failure with built-in exit: technology transition, circular economy, nature-based solutions create pathways to compliance within specified timeframe.
constraint_indexing:constraint_classification(planetary_boundaries, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EXTRACTIVE INDUSTRY REGULATORY SYSTEM (PITON) — Environmental impact assessments, carbon pricing schemes, sustainability reporting standards, and ESG frameworks have become substantially performative. Companies comply with theater (publish sustainability reports, offset calculations, greenwashing narratives) while core extraction continues. Regulatory mechanisms persist through institutional inertia despite limited functional boundary protection. Theater ratio high — compliance signals without emissions or biodiversity outcomes. System sees its own process as degraded but continues through regulatory path dependency.
constraint_indexing:constraint_classification(planetary_boundaries, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, some planetary boundary transgression follows from the second law of thermodynamics: dissipative economic structures produce entropy and heat. The boundary framework appears as a natural limit imposed by physics itself — no human policy can repeal it. However, the structural data contradicts the mountain classification: the transgression is driven by contingent institutional arrangements (growth imperative, externalitized pricing, technology asymmetry), not by immutable physical law. Engine flags as false summit.
constraint_indexing:constraint_classification(planetary_boundaries, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_boundaries_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_boundaries, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_boundaries, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planetary_boundaries, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(planetary_boundaries, TR),
    TR >= 0.70.

:- end_tests(planetary_boundaries_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The planetary boundaries framework reveals that current economic organization systematically transgresses nine critical boundaries. This is not accidental but structural: the profit extraction mechanisms of high-consumption economies depend on externalizing environmental costs. The value reflects that boundary transgression generates direct monetary benefits to specific actors (fossil fuel firms, industrial agriculture, mining companies) while costs are distributed to future time periods and lower-income populations unable to participate in benefit capture. The extractiveness has increased from 0.35 to 0.68 over the measurement interval as transgression has accelerated and alternative institutional arrangements have been proposed but not implemented. Suppression (0.72): High. Barriers to boundary compliance include: technology lock-in (infrastructure optimized for fossil fuels and industrial agriculture), path dependency in investment, regulatory capture by extractive industries, information asymmetry (externalized costs hidden in supply chains), and institutional incentives (GDP growth metrics reward transgression). Suppression is not total — scientific evidence is available, alternative technologies exist, policy tools exist — but overcoming suppression requires simultaneous disruption of multiple institutional layers. Theater ratio (0.58): Moderate-high. Compliance mechanisms (ESG reporting, carbon offset markets, sustainability certifications, SDG commitments) have become substantially performative. Corporations publish net-zero pledges while expanding extraction; governments commit to Paris targets while approving fossil fuel infrastructure; financial institutions tout green investing while maintaining holdings in boundary-transgressing firms. Theater has increased over the interval as the gap between commitment and implementation has widened. However, theater is not total — some genuine decoupling has occurred in renewable energy penetration, energy efficiency, and land restoration.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival gap from a single structural reality. High-consumption economies see the framework as informational coordination — they receive the scientific data and adjust policies at margins while maintaining growth trajectories. The extractive industry sees it as theater — comply with reporting and certification while continuing extraction. Future generations and island states see it as snare — trapped in a system designed to extract value to their cost. The international climate framework sees it as a temporary coordination problem with sunset — technology transition, circular economy, and nature-based solutions enable boundary compliance within specified timeframes. Subsistence communities see mixed extraction and coordination — the framework's boundary recognition validates their ecological knowledge but implementation mechanisms often impose external solutions rather than supporting local agency. The thermodynamic observer risks seeing immutable physical law where the structural data reveals contingent institutional arrangements. The gap widens as extractiveness increases: actors with arbitrage capacity maintain rope or scaffold classifications while trapped actors experience snare increasingly severely.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value follows from their structural position relative to boundary transgression. High-consumption economies with arbitrage capacity (ability to shift investments, access technology, absorb impacts) derive low d values reflecting that they are beneficiaries of the current structure and experience the framework as informational coordination rather than constraint. Trapped actors (island states, future generations, subsistence communities) with no exit options derive high d values reflecting that they bear extraction costs without benefit capture. Organized intermediate actors (green economy coalition, climate frameworks) derive moderate d values reflecting mixed positions: constrained by current institutions but with agency to develop alternatives. The institutional regulatory system derives moderate d reflecting its capture by extractive interests while maintaining formal compliance mandates. The false summit (thermodynamic mountain) derives analytical d reflecting that the observer risks naturalizing contingent institutional arrangements as immutable physical law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The high extractiveness (0.68) creates risk of mislabeling as pure coordination (rope) when the structural data reveals pure extraction (snare) for trapped actors. Mandatrophy resolution requires distinguishing the beneficiary's perspective (rope/coordination) from the victim's perspective (snare/extraction). High-consumption economies genuinely experience the framework as coordination — they are solving the information problem of how to maintain growth while managing environmental constraints. But this experience reflects their position as extractors, not the objective structure of the constraint. The snare classification for trapped actors is the structural reality: they have no exit option, no participation in benefits, and bear accumulating costs. The mandate failure would be claiming the framework is 'fundamentally a coordination mechanism' (rope for all) when the actual structure involves asymmetric extraction. The framework is simultaneously genuine coordination (for high-income nations seeking growth-plus-environment management) and genuine extraction (for island states and future generations seeking survival). The mandatrophy is resolved by the indexical classification system itself: it produces different types for different structural positions, revealing that the framework name-elides what is actually an extractive mechanism for some and a coordination mechanism for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_safe_margin_calibration,
    'What is the true epistemic confidence in the ''safe operating space'' quantification for each boundary, and how much is safety margin versus measured crossing?',
    'Meta-analysis of Earth system modeling uncertainty ranges; comparison of boundary thresholds across independent research groups; identification of hidden parameter sensitivity in boundary calculations',
    'If boundaries are conservative (large unknown safety margins): transgression is less severe than framework suggests, extraction narrative weakens, classification shifts toward Rope. If boundaries are calibrated to tipping points with low margins: transgression is near-critical, snare classification strengthens, extraction narrative hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_safe_margin_calibration, empirical, 'Epistemic confidence in safe operating space quantification').

omega_variable(
    technology_transition_feasibility,
    'Can renewable energy, circular economy, and regenerative agriculture systems actually scale to maintain current consumption levels or must absolute reduction occur?',
    'Net energy return analysis on renewable infrastructure; materials requirements for green transition versus available reserves; biophysical capacity modeling for regenerative agriculture; comparison against decoupling claims in IPCC/IEA scenarios',
    'If scaling feasible: scaffold and rope perspectives are structurally sound, boundary transgression is solvable through technology coordination, extraction window has real sunset. If scaling infeasible: technology transition is aspirational theater, extraction is permanent structural feature, classification hardens to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transition_feasibility, empirical, 'Whether technology transition can maintain consumption without absolute reduction').

omega_variable(
    institutional_sovereignty_enforcement_mechanism,
    'What enforcement mechanism could compel high-consumption economies to internalize boundary costs without creating equivalent extraction burden on lower-income nations?',
    'Analysis of climate reparations frameworks, technology transfer mechanisms, and trade enforcement; historical precedent in other environmental agreements (Montreal Protocol, CITES); modeling of incentive compatibility without forced transfer',
    'If mechanism exists: tangled rope classification is stable, extraction can be rebalanced through enforcement. If mechanism absent: snare extraction is structurally necessary — no way to compel compliance without violating national sovereignty or creating new extraction forms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_sovereignty_enforcement_mechanism, conceptual, 'Enforcement mechanism for boundary compliance without secondary extraction').

omega_variable(
    consumption_level_necessity,
    'Is current consumption level in high-income nations necessary for human flourishing or is it a contingent cultural/institutional norm propagated by extraction narratives?',
    'Cross-cultural wellbeing metrics at different consumption levels; analysis of consumption-wellbeing correlation controlling for inequality; study of post-materialist transition in wealthy nations; qualitative research on sufficiency economics',
    'If necessary: boundary transgression reflects genuine human need, extraction is partly coordination for survival, classification softens. If contingent: transgression is pure rent-seeking, extraction narrative hardens to snare, alternative institutional arrangements become viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumption_level_necessity, preference, 'Whether current consumption levels are necessary for human flourishing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_boundaries, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pb_tr_t0, planetary_boundaries, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pb_tr_t30, planetary_boundaries, theater_ratio, 30, 0.5).
narrative_ontology:measurement(pb_tr_t60, planetary_boundaries, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(pb_be_t0, planetary_boundaries, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pb_be_t30, planetary_boundaries, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(pb_be_t60, planetary_boundaries, base_extractiveness, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_boundaries, global_infrastructure).
narrative_ontology:affects_constraint(planetary_boundaries, climate_change_mitigation).
narrative_ontology:affects_constraint(planetary_boundaries, biodiversity_loss_acceleration).
narrative_ontology:affects_constraint(planetary_boundaries, nitrogen_phosphorus_cycle_disruption).
narrative_ontology:affects_constraint(planetary_boundaries, freshwater_depletion).
narrative_ontology:affects_constraint(planetary_boundaries, ocean_acidification).

% DUAL FORMULATION NOTE:
% The Planetary Boundaries framework itself is an overarching constraint that decomposes into distinct boundary-specific constraints. Each boundary (climate, biosphere, nutrient cycles, freshwater, ocean chemistry, etc.) has its own extractiveness profile and institutional dynamics. This story models the framework as a unified constraint capturing the meta-structural arrangement where transgression of individual boundaries is systematically encouraged by high-income institutional actors. The affects_constraints list identifies downstream constraint families where specific boundaries manifest as distinct extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(planetary_boundaries, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
