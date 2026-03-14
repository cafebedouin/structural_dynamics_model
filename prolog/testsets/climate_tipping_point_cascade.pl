% ============================================================================
% CONSTRAINT STORY: climate_tipping_point_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_tipping_point_cascade, []).

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
 *   constraint_id: climate_tipping_point_cascade
 *   human_readable: Climate Tipping Point Cascade Lock-in
 *   domain: climate_systems/geopolitical_economy
 *
 * SUMMARY:
 *   The climate tipping point cascade represents a structural constraint
 *   where the interdependencies of Earth system processes create a lock-in
 *   mechanism that concentrates extraction on actors with lowest exit
 *   capacity and highest vulnerability. The constraint operates across
 *   multiple timescales and spatial scales, creating asymmetric impacts:
 *   wealthy mid-latitude nations retain adaptive capacity and technology
 *   options; low-latitude populations and small island states face
 *   inescapable climate impacts from cumulative historical and current
 *   emissions. The fossil fuel extraction industry benefits from continued
 *   carbon release up to tipping thresholds; decarbonization coalitions see
 *   the transition as winnable but time-constrained; climate governance
 *   apparatus maintains performative legitimacy despite structural
 *   inefficacy. The extractiveness value (0.68) reflects that the constraint
 *   systematically transfers climate risks from high-emission historical
 *   actors to low-emission contemporary populations, with suppression (0.72)
 *   enforced through infrastructure lock-in, financial incentives, and
 *   unequal adaptive capacity. Theater ratio (0.58) reflects significant
 *   performativity in climate pledges and governance mechanisms that maintain
 *   institutional legitimacy while inadequate mitigation persists. This
 *   constraint exemplifies how natural system dynamics (tipping points in
 *   climate physics) become extractive institutional arrangements through
 *   unequal exposure and unequal capacity to respond.
 *
 * KEY AGENTS:
 *   - Low-Latitude Populations and Small Island States: Primary victims (powerless/trapped) — bear full climate impacts despite minimal historical responsibility for carbon debt. No exit options; face forced migration, economic collapse, ecosystem degradation.
 *   - Future Generations: Victim (powerless/analytical) — inherit climate debt and constrained options. Cannot participate in decisions creating the constraint.
 *   - Climate System Integrity: Victim (analytical/trapped) — abstract collective good; cannot organize or advocate. Degraded by feedback cascades.
 *   - Fossil Fuel Extraction Industry: Primary beneficiary (institutional/arbitrage) — captures rents during carbon overshoot period. Net beneficiary from delayed climate action.
 *   - Carbon-Intensive Industries: Secondary beneficiary (institutional/arbitrage) — benefit from carbon externalities and regulatory arbitrage.
 *   - Mid-Latitude Developed Economies: Mixed (moderate/constrained) — bear transition costs but retain wealth and technology for adaptation; benefit from historical emissions advantage.
 *   - Decarbonization Coalition: Organized agents (organized/mobile) — renewable firms, climate networks, subnational governments building alternative energy pathways.
 *   - International Climate Governance: Institutional custodian (institutional/arbitrage) — maintains Paris framework, performs commitment cycles, benefits from legitimacy despite inefficacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_tipping_point_cascade, 0.68).
domain_priors:suppression_score(climate_tipping_point_cascade, 0.72).
domain_priors:theater_ratio(climate_tipping_point_cascade, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_tipping_point_cascade, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_tipping_point_cascade, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_tipping_point_cascade, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_tipping_point_cascade, snare).
narrative_ontology:human_readable(climate_tipping_point_cascade, "Climate Tipping Point Cascade Lock-in").
narrative_ontology:topic_domain(climate_tipping_point_cascade, "climate_systems/geopolitical_economy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_tipping_point_cascade, fossil_fuel_extractors).
narrative_ontology:constraint_beneficiary(climate_tipping_point_cascade, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_tipping_point_cascade, climate_system_integrity).
narrative_ontology:constraint_victim(climate_tipping_point_cascade, low_latitude_populations).
narrative_ontology:constraint_victim(climate_tipping_point_cascade, future_generations).
narrative_ontology:constraint_victim(climate_tipping_point_cascade, small_island_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — No exit option from escalating climate impacts. Trapped by geographic vulnerability and carbon debt not of their making. Bear extraction in the form of forced displacement, economic collapse, ecosystem degradation. Cannot exit the constraint; cannot organize sufficient political power to reverse mechanisms. Maximum experienced extraction.
constraint_indexing:constraint_classification(climate_tipping_point_cascade, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-LATITUDE DEVELOPED ECONOMIES (TANGLED ROPE) — Constrained by carbon lock-in in infrastructure and finance, but also benefit from historical emissions advantage (technology, wealth accumulation, adaptive capacity). Face high transition costs but retain agency and resources. Mixed extraction and coordination: must transition away from fossil fuels while managing economic disruption.
constraint_indexing:constraint_classification(climate_tipping_point_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL EXTRACTORS (ROPE) — Primary beneficiary. Experiences the tipping point cascade as enabling their continued high-margin operations. Climate threshold dynamics create coordination function: stranded asset risk is managed through regulatory arbitrage, carbon markets, and narrative control. Net extraction flow toward this agent.
constraint_indexing:constraint_classification(climate_tipping_point_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECARBONIZATION COALITION (SCAFFOLD) — Organized agents (renewable energy firms, climate networks, subnational governments) see the tipping point cascade as a coordination problem with a sunset: renewable energy cost curves and decarbonization technologies are creating alternative energy pathways. High suppression from fossil fuel incumbency but genuine exit path visible. Theater ratio moderately high due to climate policy performativity.
constraint_indexing:constraint_classification(climate_tipping_point_cascade, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE GOVERNANCE APPARATUS (PITON) — Paris Agreement and UNFCCC frameworks persist through institutional inertia despite structural inefficacy. Theater_ratio elevated by performative national pledges, NDCs revised without binding enforcement, and climate finance pledges unmet. The governance apparatus maintains legitimacy through ritual (COP conferences, commitment cycles) while the underlying constraint (tipping point cascade) continues to tighten. Sees its own process as degraded but maintains it through lack of credible alternatives.
constraint_indexing:constraint_classification(climate_tipping_point_cascade, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a deep time / thermodynamic perspective, carbon accumulation in the atmosphere is an immutable constraint on planetary energy balance. Tipping points at specific thresholds (1.5°C, 2.0°C, Amazon dieback, permafrost collapse, ice sheet destabilization) are embedded in climate physics. Once crossed, feedbacks become self-reinforcing and exit options vanish entirely. This perspective naturalizes the constraint as physical law. However, structural analysis reveals this mountain classification obscures the contingent institutional choices (energy systems, economic structure, governance) that determine whether thresholds are crossed. The 'inevitability' framing may be a false summit masking policy failures.
constraint_indexing:constraint_classification(climate_tipping_point_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_tipping_point_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_tipping_point_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_tipping_point_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_tipping_point_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_tipping_point_cascade, TR),
    TR >= 0.70.

:- end_tests(climate_tipping_point_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): High. The constraint operates as a systematic transfer of climate risks from wealthy high-emission actors to poor low-emission actors. The mechanism: historical and current carbon emissions create cumulative atmospheric concentrations; threshold effects create tipping points; once crossed, self-reinforcing feedbacks create semi-irreversible climate changes that disproportionately impact low-latitude regions and small island states. The extraction flow is not intentional institutional design but emerges from unequal contribution to and unequal vulnerability to climate change. Over the 20-year measurement interval, extractiveness has increased from 0.35 to 0.68 as atmospheric CO2 has approached tipping thresholds and impacts have accelerated. Suppression (0.72): High. Barriers to exit for trapped agents are severe: geographic location (cannot move tropical regions to higher latitudes); economic dependency on climatic stability (agriculture, fisheries, water); lack of capital for rapid adaptation; dependence on global systems for food security that are themselves destabilized by climate change. For mid-latitude developed nations, suppression is lower (0.40-0.50) because they retain adaptive capacity, technology, migration options, and financial resources. Theater ratio (0.58): Moderate-high. Climate governance institutions (UNFCCC, Paris Agreement, NDCs) maintain high performative content: nations pledge emissions reductions but international enforcement mechanisms are weak; climate finance pledges are unmet; corporate net-zero commitments often rely on unverified carbon credits; carbon markets show signs of credit inflation and additionality gaming. The theater has increased from 0.42 to 0.58 over the interval as gap between pledges and actual emissions reductions has widened and performance legitimacy has become more important for institutional survival.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Trapped populations perceive the tipping point cascade as a Snare with no exit (maximum extraction experienced, no alternative pathways visible). Fossil fuel extractors perceive Rope (coordination function: their operations generate profits while global coordination mechanisms remain inefficient enough to tolerate continued emissions). Mid-latitude developed nations perceive Tangled Rope (mixed coordination and extraction: transition costs are real but adaptive capacity and technology options remain available; they are neither beneficiaries nor fully victimized). The decarbonization coalition perceives Scaffold (organized agents see a sunset: renewable cost curves and decarbonization technologies are creating alternative energy pathways with visible timelines, though high suppression from incumbent resistance remains). The climate governance apparatus perceives itself as Piton (performative ritual maintaining institutional legitimacy despite structural inefficacy; sees its own process as degraded). The analytical civilizational observer risks perceiving Mountain (tipping points are embedded in climate physics, once crossed become irreversible, therefore constraint is immutable natural law) — but this risks naturalizing contingent institutional choices (energy systems, economic structure, governance responsiveness) that determine whether thresholds are approached. The perspectival gap is not about disagreement on facts but about differential exposure to the extraction mechanism and differential capacity to respond.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position: power level relative to the constraint, exit capacity, and relationship to extraction flow. Trapped populations with no exit options (d ≈ 0.95) experience maximum extraction through f(d) ≈ 1.42. Institutional fossil fuel extractors with arbitrage options (d ≈ 0.10) experience low or negative effective extraction — the constraint coordinates their high-margin operations. Institutional governance actors with arbitrage options (d ≈ 0.15) experience negative extraction (they benefit from institutional legitimacy of climate frameworks). Moderate mid-latitude nations with constrained exit (d ≈ 0.60) experience moderate extraction (f(d) ≈ 0.85) — transition costs are real but not binding. Organized decarbonization agents with mobile options (d ≈ 0.45) experience lower extraction (f(d) ≈ 0.55) because they have visible exit paths. Analytical observers see the structure across all positions and recognize the asymmetry as the core constraint mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH MULTI-PERSPECTIVAL CLARITY: The mandatrophy question is 'Is this a natural law (mountain) or an extractive institutional arrangement (snare)?' The analytical observer classification addresses this directly: while tipping points in climate physics ARE natural laws (irreversible thresholds embedded in Earth system dynamics), the extraction mechanism is NOT. The extraction emerges from unequal distribution of: (1) historical responsibility for carbon accumulation, (2) current emission levels, (3) wealth and technology for adaptation, (4) political power to shape climate governance. A different set of institutional choices would create different distributions: carbon tax that penalizes historical emitters, technology transfer that equalizes adaptive capacity, governance structures that give vulnerable nations decision power, rapid decarbonization that prevents crossing tipping thresholds. The constraint is therefore not a mountain but a Snare: it is an institutional arrangement that uses natural system dynamics (tipping points) as the mechanism, but the extraction mechanism itself is contingent and changeable. The piton perspective reveals that climate governance maintains legitimacy through performative commitment cycles despite structural inefficacy, enabling the extraction to persist. Classification of the tipping point cascade as Snare, not Mountain, is confirmed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_threshold_ambiguity,
    'Are climate tipping points best modeled as sharp phase transitions or as smooth nonlinear accelerations? Does this distinction change the classification from Snare to Tangled Rope?',
    'High-resolution paleoclimate data analysis, Earth system model ensemble outputs, empirical forcing-response curves for major tipping candidates (Amazon, AMOC, West Antarctic Ice Sheet)',
    'If thresholds are sharp and irreversible: extraction mechanism is binding and classification remains Snare (no exit beyond threshold). If thresholds are smooth with hysteresis but reversible: some agents retain agency through carbon removal / geoengineering and classification shifts to Tangled Rope (constrained but with alternative pathways).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_threshold_ambiguity, empirical, 'Threshold sharpness and reversibility of climate tipping points').

omega_variable(
    carbon_lock_in_irreversibility,
    'Is carbon lock-in in fossil fuel infrastructure and financial systems an irreversible constraint or a high-cost but surmountable barrier?',
    'Cost-benefit analysis of stranded assets vs renewable transition capital; historical precedent from other infrastructure lock-in breaks (automotive shift, electrification); modeling of economic disruption vs climate damage avoidance',
    'If irreversible: low-latitude nations are permanently trapped (powerless/trapped classification confirmed, Snare stands). If high-cost but surmountable: wealthy nations retain constrained exit and classification shifts to Tangled Rope; equity dimensions emerge (who pays for transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_lock_in_irreversibility, empirical, 'Whether carbon lock-in is irreversible or costly-but-surmountable').

omega_variable(
    geoengineering_as_exit_option,
    'Does stratospheric aerosol injection or direct air capture constitute a genuine exit option from the tipping point cascade, or do these technologies themselves create new extractive constraints (deployment power asymmetry, side effects, moral hazard)?',
    'Technology readiness assessments; modeling of effectiveness and side effects; analysis of deployment power (who controls geoengineering infrastructure); monitoring of moral hazard effects on emissions reduction incentives',
    'If viable without new constraints: wealthy agents gain exit option and reclassify to mobile or arbitrage (Tangled Rope or Rope from their perspective). If creates new extraction: geoengineering becomes a parallel Snare controlled by technological elites. Shapes whether scaffold perspective''s decarbonization sunset is real or illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geoengineering_as_exit_option, empirical, 'Whether geoengineering provides genuine exit or creates parallel extraction').

omega_variable(
    adaptive_capacity_substitutability,
    'Can climate adaptation (migration, agricultural innovation, infrastructure hardening) substitute for mitigation in protecting vulnerable populations, or are adaptation and mitigation complementary with hard limits on what adaptation alone can achieve?',
    'Comparative analysis of adaptation costs vs mitigation costs; modeling of adaptation effectiveness at different warming scenarios (1.5°C, 2.0°C, 3.0°C, 4.0°C); historical study of climate-driven migration and economic collapse',
    'If substitutable: low-latitude nations gain constrained exit through adaptation technology and reclassify from trapped to constrained (Snare shifts toward Tangled Rope). If complementary with hard limits: trapped classification persists and extraction asymmetry deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_capacity_substitutability, empirical, 'Whether adaptation can substitute for mitigation or are complementary').

omega_variable(
    institutional_capacity_for_rapid_decarbonization,
    'Can institutional and economic systems deliver the scale and speed of decarbonization required to avoid major tipping points (net-zero by 2050, 50% reductions by 2035), or are these targets structurally infeasible given existing infrastructure, financial incentives, and political constraints?',
    'Historical rates of infrastructure transitions; sectoral modeling of decarbonization pathways; political economy analysis of fossil fuel incumbent resistance; monitoring of actual vs pledged emissions reductions',
    'If feasible: scaffold perspective is real and sunset is plausible (decarbonization coalition has genuine exit path). If infeasible: tipping points become increasingly probable, extraction mechanisms tighten, and trapped agents'' situation worsens (Snare deepens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capacity_for_rapid_decarbonization, empirical, 'Whether rapid decarbonization targets are institutionally feasible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_tipping_point_cascade, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tip_tr_t0, climate_tipping_point_cascade, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clim_tip_tr_t10, climate_tipping_point_cascade, theater_ratio, 10, 0.5).
narrative_ontology:measurement(clim_tip_tr_t20, climate_tipping_point_cascade, theater_ratio, 20, 0.58).
narrative_ontology:measurement(clim_tip_tr_t5, climate_tipping_point_cascade, theater_ratio, 5, 0.46).

% Extraction over time
narrative_ontology:measurement(clim_tip_be_t0, climate_tipping_point_cascade, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_tip_be_t10, climate_tipping_point_cascade, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_tip_be_t20, climate_tipping_point_cascade, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_tip_be_t5, climate_tipping_point_cascade, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_tipping_point_cascade, global_infrastructure).
narrative_ontology:affects_constraint(climate_tipping_point_cascade, arctic_amplification_feedback).
narrative_ontology:affects_constraint(climate_tipping_point_cascade, ocean_acidification_cascade).
narrative_ontology:affects_constraint(climate_tipping_point_cascade, amazon_dieback_threshold).
narrative_ontology:affects_constraint(climate_tipping_point_cascade, financial_stranded_assets).
narrative_ontology:affects_constraint(climate_tipping_point_cascade, geopolitical_climate_migration).

% DUAL FORMULATION NOTE:
% The climate tipping point cascade decomposes into at least five distinct constraint stories: (1) Arctic amplification feedback (irreversible ice-albedo feedback), (2) Ocean acidification cascade (carbonate chemistry threshold), (3) Amazon dieback (vegetation tipping point), (4) Financial stranded asset lock-in (capital stock constraint), (5) Geopolitical climate-driven migration (forced displacement). Each has different ε values reflecting different empirical confidence in threshold proximity and reversibility. The aggregate 'climate tipping point cascade' is the upstream constraint that creates dependencies for all downstream constraints. Extractiveness increases as thresholds approach because exit options narrow for trapped populations while beneficiary actors race to extract final rents before stranded asset write-downs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_tipping_point_cascade, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
