% ============================================================================
% CONSTRAINT STORY: post_soviet_naftogaz_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_post_soviet_naftogaz_monopoly, []).

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
 *   constraint_id: post_soviet_naftogaz_monopoly
 *   human_readable: Post-Soviet Naftogaz Natural Gas Monopoly
 *   domain: energy/political_economy
 *
 * SUMMARY:
 *   The post-Soviet Naftogaz monopoly represents a structurally evolved
 *   constraint originating in Soviet-era centralized energy infrastructure
 *   and degraded through oligarchic capture into a pure extraction mechanism.
 *   Ukraine inherited from the USSR a centralized natural gas distribution
 *   network designed for Soviet administrative coordination. After 1991, this
 *   inherited infrastructure became a tool for political patronage,
 *   oligarchic rent extraction, and geopolitical leverage. Naftogaz retained
 *   its monopoly over gas import, distribution, and pricing despite multiple
 *   reform initiatives. The constraint exhibits a temporal trajectory:
 *   initial legitimate coordination (1992-2000, managing post-Soviet supply
 *   disruptions), escalating extraction (2000-2015, oligarchic capture and
 *   pricing manipulation), partial reform pressure (2015-2020, EU integration
 *   and IMF conditionality), and current unstable equilibrium. The
 *   extractiveness trajectory shows accumulation from 0.45 to 0.72,
 *   indicating increasing rent-seeking layered onto coordination. Theater
 *   ratio climbing from 0.35 to 0.62 reflects bureaucratic activity
 *   substituting for real market function — regulatory committees, pricing
 *   boards, and supply allocation decisions multiplying without improving
 *   actual coordination outcomes. The slight decline in both metrics
 *   post-2015 reflects initial EU-mandated tariff increases and market
 *   liberalization pressure, but both remain at high levels, indicating
 *   suppression of reform implementation.
 *
 * KEY AGENTS:
 *   - Ukrainian Households: Primary victims (powerless/trapped) — trapped by geographic monopoly and heating necessity; bear extraction cost through above-market pricing and state subsidies shifting costs to taxation
 *   - Ukrainian Industrial Base: Primary victims (powerless/trapped) — competitiveness eroded by monopoly gas prices; unable to relocate; loss of market share to regional competitors with market-priced feedstock
 *   - State Budget: Mixed (moderate/constrained) — nominal owner of Naftogaz; receives dividends but must subsidize household prices to maintain political stability; net victim despite nominal ownership
 *   - Naftogaz Management and Connected Oligarchs: Primary beneficiaries (institutional/arbitrage) — capture monopoly rents, redirect supply flows to highest-bidding consumers, leverage energy control for political influence; maintain arbitrage options through state connections
 *   - EU Integration Framework: Organized external pressure (organized/constrained) — IMF, EU directives, Third Energy Package requirements impose market liberalization conditions; constrained by limited enforcement leverage over Ukrainian political system
 *   - Soviet Infrastructure Apparatus: Institutional legacy (institutional/arbitrage) — central planning systems persist through inertia; bureaucratic structures maintain monopoly control despite obsolescence; theater ratio high (administrative activity divorced from real coordination function)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_soviet_naftogaz_monopoly, 0.68).
domain_priors:suppression_score(post_soviet_naftogaz_monopoly, 0.72).
domain_priors:theater_ratio(post_soviet_naftogaz_monopoly, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_soviet_naftogaz_monopoly, extractiveness, 0.68).
narrative_ontology:constraint_metric(post_soviet_naftogaz_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(post_soviet_naftogaz_monopoly, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_soviet_naftogaz_monopoly, snare).
narrative_ontology:human_readable(post_soviet_naftogaz_monopoly, "Post-Soviet Naftogaz Natural Gas Monopoly").
narrative_ontology:topic_domain(post_soviet_naftogaz_monopoly, "energy/political_economy").

domain_priors:requires_active_enforcement(post_soviet_naftogaz_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_soviet_naftogaz_monopoly, naftogaz_management).
narrative_ontology:constraint_beneficiary(post_soviet_naftogaz_monopoly, connected_oligarchs).
narrative_ontology:constraint_victim(post_soviet_naftogaz_monopoly, ukrainian_households).
narrative_ontology:constraint_victim(post_soviet_naftogaz_monopoly, industrial_consumers).
narrative_ontology:constraint_victim(post_soviet_naftogaz_monopoly, state_budget).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN HOUSEHOLD CONSUMER (SNARE) — Trapped by geographic necessity (no alternative gas suppliers), structural dependency (heating fuel for survival in winter), and state-enforced monopoly. Cannot exit; bears full extraction cost through above-market pricing. No coordination benefit perceived — constraint experienced as pure coercion.
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UKRAINIAN INDUSTRIAL BASE (SNARE) — Locked into monopoly pricing for gas feedstock; cannot relocate operations or source alternative supply without massive capital cost. Extraction continues across generational time — industrial competitiveness systematically degraded relative to regional competitors with market-priced gas. No exit; full victim status.
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINIAN STATE (TANGLED ROPE) — Mixed extraction and coordination. State owns Naftogaz, receives nominal dividends and production revenue, but loses far more through subsidies required to keep household prices below cost. Political pressure to maintain cheap heating creates revenue shortfall. Constrained by energy security dependency and lack of capital for alternative infrastructure. Experiences coordination (energy delivery) alongside extraction (monopoly rents diverted from state accounts).
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: NAFTOGAZ MANAGEMENT & OLIGARCHS (ROPE) — Primary beneficiaries experiencing the constraint as coordination: control over gas supply enables capture of rents, control over supply channels, political influence through energy security threats. Arbitrage options exist (can redirect gas flows to highest-bidding consumers, can shift production allocation geographically). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: EU INTEGRATION & REFORM (SCAFFOLD) — External organized agents (EU energy market directives, IMF structural adjustment programs, Third Energy Package requirements) impose temporary coordination problems with sunset: market liberalization, regulated tariffs, third-party access requirements, and interconnection with European gas networks reduce monopoly extraction over generational time. Suppression remains high (enforcement of monopoly against reform) but is constrained by external conditionality. Scaffolding logic visible — organized external pressure creates sunset path.
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOVIET INSTITUTIONAL INERTIA (PITON) — The Naftogaz monopoly represents degraded Soviet central planning apparatus. The constraint persists through institutional inertia — Naftogaz maintains monopoly control despite technological obsolescence and structural misfit with market conditions. Theater ratio reflects this: extensive bureaucratic activity (regulation, pricing committees, supply routing decisions) with declining actual coordination function. The apparatus 'survives' through theatrical enforcement rather than legitimate functional necessity.
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, the Naftogaz monopoly is a hybrid constraint: genuine coordination of Soviet-era infrastructure (centralized pipeline routing, integrated supply network) combined with asymmetric rent extraction that has intensified in post-Soviet period. The coordination function remains real (alternative distributed supply would be inefficient); the extraction is amplified by political capture and lack of market discipline. Civilizational scope reveals the constraint as path-dependent: Soviet infrastructure created genuine need for centralized coordination, which oligarchic capture leveraged into pure extraction mechanism. The constraint is neither purely natural (mountain) nor purely political (snare) — it is a degraded but still functional coordination mechanism wrapped in extractive institutional superstructure.
constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_soviet_naftogaz_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_soviet_naftogaz_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(post_soviet_naftogaz_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(post_soviet_naftogaz_monopoly, TR),
    TR >= 0.70.

:- end_tests(post_soviet_naftogaz_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximum. The constraint extracts substantial rents from trapped consumers and industrial users through above-market pricing. However, the extraction is not total (0.85-0.95) because: (1) Some portion of above-market pricing reflects legitimate energy security costs given vulnerability to Russian supply disruptions; (2) The state retains nominal ownership and receives some revenue, creating partial interest alignment; (3) EU integration and alternative supplier development are beginning to create competitive pressure, reducing monopoly pricing power at margins. The 2015-2020 slight decline reflects these forces. Suppression (0.72): High. Trapped consumers have no exit options (heating necessity, geographic monopoly, no alternative suppliers). Industrial users face massive relocation costs. State political pressure to maintain cheap heating prevents market liberalization despite economic logic. Legal/regulatory barriers (Naftogaz monopoly charter) enforce the constraint actively. Theater ratio (0.58): Moderate-high. The constraint maintains real coordination function (centralized pipeline network, integrated supply), but increasingly substitutes bureaucratic theater for actual coordination: pricing committees whose decisions track oligarchic interests rather than cost or demand; regulatory bodies that perform legitimacy rather than enforcement; supply routing allocation that favors connected consumers. The theater has increased over time as the genuine coordination function (managing supply disruptions in 1990s) has become less urgent (more stable supply) while extraction mechanisms have become more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals how identical structural data (monopoly pricing, trapped consumers, oligarchic control) classifies as snare from victim perspective (powerless/trapped), rope from beneficiary perspective (institutional/arbitrage), tangled rope from state perspective (moderate/constrained), scaffold from EU perspective (organized/constrained, sunset), and piton from Soviet apparatus perspective (institutional/arbitrage, high theater). The gap is largest between snare and rope perspectives — same pricing mechanism, opposite experiential classification. This gap is not an analytical mistake; it reflects genuine power asymmetry and structural differentiation. The constraint IS a snare for households and IS a rope for oligarchs. The unified classification must be determined by power-weighted prevalence: powerless/trapped agents (majority) experience snare; institutional/arbitrage agents (minority) experience rope. The engine's computed constraint_claim will weight perspectives by agent power and prevalence, likely producing snare as primary classification with tangled_rope as secondary (state perspective weight).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to extraction flow: (1) Households and industry: victims with no exit (d ≈ 0.92-0.95); (2) State: nominal owner but net victim after subsidies (d ≈ 0.60); (3) Naftogaz/oligarchs: beneficiaries with arbitrage exit (d ≈ 0.08-0.15); (4) EU/reform pressure: organized external agents with constrained leverage (d ≈ 0.55). The high d values for trapped consumers and industrial users map to maximum experienced extraction chi through the sigmoid function. The beneficiaries' low d values produce negative or near-zero chi — they experience the constraint as coordination benefit. The state's moderate d reflects mixed position: owns Naftogaz (beneficiary position d-pushing) but subsidizes consumers (victim position d-pushing) — net position is victim with some nominal benefits. Overrides not needed — the structural data produces internally consistent d values.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The 'Naftogaz monopoly' concept decomposes into three structurally distinct constraints with different ε values and omegas: (1) ENERGY_COORDINATION_INFRASTRUCTURE (ε ≈ 0.15-0.25): Genuine coordination problem of centralized gas pipeline routing and integrated supply management inherited from Soviet era. This constraint is partially mountain (irreducible efficiency of centralization) and partially rope (coordinating shared infrastructure). (2) OLIGARCHIC_RENT_EXTRACTION (ε ≈ 0.72-0.80): Snare capturing monopoly rents through above-market pricing, regulatory capture, and connected-party transfers. This is pure extraction layered onto the coordination infrastructure. (3) RUSSIAN_GEOPOLITICAL_LEVERAGE (ε ≈ 0.55-0.65): Mixed constraint where monopoly structure increases vulnerability to Russian supply manipulation (suppression mechanism) but is justified by energy security argument. This creates tangled rope dynamics. The current JSON story conflates all three into a single snare, which is diagnostically valid but analytically incomplete. A full corpus treatment would separate these three stories and link them via network.affects_constraints. The mandatrophy is resolved by recognizing that genuine infrastructure coordination (constraint 1, ε low) provides political cover for oligarchic extraction (constraint 2, ε high) which is justified through geopolitical security framing (constraint 3, ε medium). The constraint family reveals how false natural law (mountain framing of coordination necessity) enables snare classification. When decomposed, the legitimacy question clarifies: is the monopoly structure justified by coordination efficiency, or is coordination efficiency being used as cover for extraction?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    energy_security_vs_extraction_boundary,
    'What portion of above-market pricing represents legitimate energy security cost vs. oligarchic rent extraction?',
    'Comparative analysis with peer Central European gas monopolies; cost breakdown distinguishing infrastructure maintenance, supply acquisition, and administrative overhead from profit margins and connected-party transfers',
    'If security cost > 40%: snare classification weakens toward tangled_rope. If security cost < 20%: snare classification solidifies. Distribution of extracted rents (to state vs oligarchs vs reinvestment) determines whether extraction is public or private.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_security_vs_extraction_boundary, empirical, 'Energy security costs versus oligarchic rent extraction proportion').

omega_variable(
    alternative_infrastructure_feasibility,
    'Could decentralized gas distribution (LNG terminals, alternative suppliers, renewable integration) replicate current infrastructure coordination function within 10-15 years?',
    'Engineering feasibility study; cost comparison of alternative topologies vs centralized Naftogaz system; institutional analysis of market fragmentation risks',
    'If feasible: scaffold perspective confirmed — technological sunset is real and approaching. If infeasible: institutional monopoly is partly justified by network efficiency (coordination function remains genuine); reclassify toward robust tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_infrastructure_feasibility, empirical, 'Technical feasibility of alternative gas distribution infrastructure').

omega_variable(
    oligarchic_capture_depth,
    'Are Naftogaz rents captured by external oligarchs (pure extraction) or are they embedded in state budgeting (public extraction for political redistribution)?',
    'Forensic analysis of Naftogaz transfer pricing, connected-party contracts, and subsidiary ownership; budget impact analysis distinguishing state revenue vs oligarchic siphoning',
    'If external: snare classification confirmed — extraction flows to private beneficiaries with no state benefit. If embedded: reclassify toward state-capture tangled_rope — state coordinates supply but extracts from households for budget stabilization and oligarchic side-deals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligarchic_capture_depth, empirical, 'Oligarchic capture depth and redistribution of extracted rents').

omega_variable(
    eu_integration_timeline,
    'Will EU energy market integration and Third Energy Package implementation actually force Naftogaz market liberalization, or will Ukrainian political capture prevent enforcement?',
    'Institutional analysis of EU leverage mechanisms; historical precedent of EU conditionality effectiveness in Ukraine; timeline projection of market opening milestones',
    'If EU enforces: scaffold sunset is real — extractiveness should decline generationally toward 0.35-0.45 (tangled_rope or rope). If capture prevents: scaffold is aspirational — extractiveness persists at 0.65+ (snare). Measurement trajectory over next 5-10 years will disambiguate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_integration_timeline, empirical, 'EU enforcement of energy market integration and sunset timeline credibility').

omega_variable(
    supply_disruption_vulnerability,
    'Does Naftogaz monopoly structure reduce or increase Ukraine''s vulnerability to Russian gas cutoffs and supply manipulation?',
    'Historical analysis of 2006, 2009, 2014-2022 gas disputes; game-theoretic model of monopoly buyer vs monopsony seller; alternative scenario modeling with market-diversified supply',
    'If vulnerability increases: suppression and extraction justified by security argument — reclassify toward military-necessitated tangled_rope. If vulnerability decreases: suppression appears as pure extraction mechanism (snare confirmed). The constraint may be partially justified (energy security) or purely extractive (oligarchic rents) depending on this empirical resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_disruption_vulnerability, empirical, 'Impact of monopoly structure on Russian gas supply vulnerability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_soviet_naftogaz_monopoly, 1992, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1992, post_soviet_naftogaz_monopoly, theater_ratio, 1992, 0.35).
narrative_ontology:measurement(naft_tr_t2000, post_soviet_naftogaz_monopoly, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(naft_tr_t2008, post_soviet_naftogaz_monopoly, theater_ratio, 2008, 0.55).
narrative_ontology:measurement(naft_tr_t2015, post_soviet_naftogaz_monopoly, theater_ratio, 2015, 0.62).
narrative_ontology:measurement(naft_tr_t2020, post_soviet_naftogaz_monopoly, theater_ratio, 2020, 0.58).

% Extraction over time
narrative_ontology:measurement(naft_be_t1992, post_soviet_naftogaz_monopoly, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(naft_be_t2000, post_soviet_naftogaz_monopoly, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(naft_be_t2008, post_soviet_naftogaz_monopoly, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement(naft_be_t2015, post_soviet_naftogaz_monopoly, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(naft_be_t2020, post_soviet_naftogaz_monopoly, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_soviet_naftogaz_monopoly, global_infrastructure).
narrative_ontology:affects_constraint(post_soviet_naftogaz_monopoly, russian_gas_supply_vulnerability).
narrative_ontology:affects_constraint(post_soviet_naftogaz_monopoly, ukrainian_household_energy_poverty).
narrative_ontology:affects_constraint(post_soviet_naftogaz_monopoly, european_energy_security).

% DUAL FORMULATION NOTE:
% The Naftogaz monopoly decomposes into constraint family: energy_coordination_infrastructure (ε≈0.20, rope/mountain), oligarchic_rent_extraction (ε≈0.75, snare), and russian_geopolitical_leverage (ε≈0.60, tangled_rope). This story represents the unified view. Downstream constraints (household energy poverty, European energy security implications) are affected by all three component constraints at different magnitudes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(post_soviet_naftogaz_monopoly, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
