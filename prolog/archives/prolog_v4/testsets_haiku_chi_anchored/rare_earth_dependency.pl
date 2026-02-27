% ============================================================================
% CONSTRAINT STORY: rare_earth_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_dependency, []).

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
 *   constraint_id: rare_earth_dependency
 *   human_readable: Strategic Rare Earth Element Dependency
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Strategic rare earth element (REE) dependency creates a structural
 *   extraction mechanism whereby nations reliant on imports from concentrated
 *   suppliers (historically 60-80% from China and Myanmar) experience both
 *   economic vulnerability and security blackmail risk. The constraint is not
 *   inherent to geology—uneven geographic distribution is real—but the
 *   extraction mechanism is institutional: high switching costs, long
 *   supply-chain lead times, concentrated refining capacity, and geopolitical
 *   leverage create a situation where dependent nations cannot credibly
 *   threaten exit. Unlike commodity markets with substitutes (oil/gas/coal),
 *   REE dependency lacks viable alternatives for many advanced applications,
 *   making it uniquely extractive. The theater ratio (0.38) indicates that
 *   institutional mechanisms—long-term contracts, strategic reserves, trade
 *   negotiations—still retain some functional content; they are not yet fully
 *   performative. However, the rising extractiveness (0.42 → 0.68 over 30
 *   years) reflects that geopolitical instrumentalization of REE supply has
 *   intensified, particularly after 2020 supply restrictions. The constraint
 *   demonstrates all six DR types from different perspectives: a snare for
 *   dependent manufacturers and defense sectors; rope for the dominant
 *   exporter; tangled rope for alternative-sourcing coalitions; piton for
 *   trade regimes that claim to manage it; and a false-summit mountain for
 *   civilizational observers who naturalize geopolitical dependence as
 *   geological law.
 *
 * KEY AGENTS:
 *   - Dependent Nation Manufacturing Sector: Primary victim (powerless/trapped) — industrial producers cannot exit without capital-intensive restructuring; faces suppression via switching costs and long lead times
 *   - Defense and Technology Supply Chain: Primary victim (moderate/constrained) — military and semiconductor sectors strategically trapped; cannot compromise on supply reliability without national security risk
 *   - REE Exporting Monopolist (China, Myanmar, Russia): Primary beneficiary (institutional/arbitrage) — exercises monopoly pricing and supply-control leverage; benefits from extraction with managed relationship stability
 *   - Alternative Sourcing Coalition: Secondary actor (organized/mobile) — allied nations, mining consortia, recycling R&D invest in alternatives; constrained by capital requirements and geopolitical alignment barriers
 *   - Trade Regime and Strategic Doctrine: Institutional actor (institutional/constrained) — WTO, bilateral trade agreements, strategic reserves attempt to manage dependency; faces theater degradation as mechanisms prove ineffective against weaponization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional geopolitics as immutable geological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_dependency, 0.68).
domain_priors:suppression_score(rare_earth_dependency, 0.72).
domain_priors:theater_ratio(rare_earth_dependency, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_dependency, extractiveness, 0.68).
narrative_ontology:constraint_metric(rare_earth_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_dependency, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_dependency, snare).
narrative_ontology:human_readable(rare_earth_dependency, "Strategic Rare Earth Element Dependency").
narrative_ontology:topic_domain(rare_earth_dependency, "economic/geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_victim(rare_earth_dependency, dependent_nation_manufacturing_sector).
narrative_ontology:constraint_victim(rare_earth_dependency, dependent_nation_defense_capabilities).
narrative_ontology:constraint_victim(rare_earth_dependency, downstream_technology_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT NATION MANUFACTURING (SNARE) — Industrial producers in import-dependent nations cannot exit the supply chain without massive capital reallocation and technological reconstruction. Trapped by sunk infrastructure, long lead times for alternative sourcing, and no domestic REE reserves. Suppression is structural: switching costs exceed switching benefits. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.94.
constraint_indexing:constraint_classification(rare_earth_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEFENSE AND TECHNOLOGY SUPPLY CHAIN (SNARE) — Military procurement, semiconductor fabrication, and advanced electronics all require REE-heavy components. Nations cannot substitute or source alternatives without compromising defense posture or surrendering technological leadership. Exit options are constrained by strategic necessity and geopolitical risk. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(rare_earth_dependency, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REE EXPORTING MONOPOLIST (ROPE) — For the dominant exporter (historically China with ~70% global production), the supply constraint is experienced as coordination: managing extraction requires stable supply relationships, pricing mechanisms, and predictable demand. The exporter benefits from dependency but must maintain it through reliable supply; too much coercion causes switching investment. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Net beneficiary through informational arbitrage.
constraint_indexing:constraint_classification(rare_earth_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE SOURCING COALITION (TANGLED ROPE) — Organized actors (allied nations, investment consortia, mining companies in non-monopolist regions) see REE dependency as both a coordination problem (building alternative supply chains) and an extraction problem (supply-side cartel effects). They have higher exit options (can invest in new deposits, recycling, substitution research) but face capital and geopolitical barriers. d≈0.48, f(d)≈0.58, σ=1.1 → χ≈0.43. Mixed coordination and asymmetric extraction.
constraint_indexing:constraint_classification(rare_earth_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: TRADE REGIME & STRATEGIC SUPPLY DOCTRINE (PITON) — International institutions (WTO, trade agreements) and strategic supply doctrines treat REE dependency as a temporary feature to be managed through market mechanisms and alliance structures, but the actual function (fair pricing, diversified supply, risk management) has degraded. Theater persists: long-term contracts, strategic reserves, and trade negotiations are performed but fail to prevent supply shocks or weaponization. theater_ratio=0.38 is on the piton boundary; suggests some functional content remains. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(rare_earth_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOLOGICAL CONSTRAINT VIEW (MOUNTAIN) — From a 500-year geological and technological perspective, REE dependency appears as a natural resource constraint: certain elements (dysprosium, neodymium, terbium) have uneven geographic distribution; no nation has all deposits; thermodynamic extraction costs favor concentration. This reads as immutable natural law. However, the structural data (ε=0.68, suppression=0.72) contradicts the mountain classification — the engine will compute false summit, revealing that dependency is a contingent geopolitical arrangement, not a law of geology.
constraint_indexing:constraint_classification(rare_earth_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_dependency, TR),
    TR >= 0.70.

:- end_tests(rare_earth_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The dominant exporter captures monopoly rents by controlling 70-80% of global refining capacity, not primary mining (supply is more dispersed). The extraction mechanism operates through: (1) supply restrictions (2010 export quota episode, 2023 export controls), (2) pricing power (REE prices track exporter preferences), (3) long procurement lead times (18-36 months for specialized materials), (4) geopolitical leverage (conditioning supply on political alignment). The rising trajectory (0.42 → 0.68) reflects increased weaponization post-2020. Suppression (0.72): Very high. Dependent nations face: (a) switching costs — $50-500M per refinery or manufacturing retooling, (b) tacit knowledge concentration — refining processes developed over decades, difficult to relocate, (c) lead times — new supply cannot be created in < 5 years, (d) strategic necessity — no substitutes for many defense applications, (e) geopolitical alignment costs — alternative suppliers are often geopolitically costly allies. Theater ratio (0.38): Moderate, declining. Strategic reserves, long-term contracts, and trade negotiations still perform functional work (they do sometimes stabilize supply), but their effectiveness has declined as geopolitical conflict has intensified. The declining trajectory (0.52 → 0.38) suggests that institutions are shedding theater and becoming more nakedly extractive.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The dependent manufacturing sector experiences pure extraction (Snare): no coordination benefit, only cost absorption. The defense sector experiences strategic trap (Snare with civilizational time horizon): cannot exit even at extreme cost without surrendering security. The exporter experiences profitable coordination (Rope): managing a stable supply relationship is valuable, and some reliability is needed to maintain the relationship. The alternative-sourcing coalition experiences hybrid (Tangled Rope): building new supply chains coordinates around shared interest in breaking monopoly dependence, but asymmetric extraction persists in the form of capital requirements and geopolitical access barriers. The trade regime sees itself as managing risk (Piton on the boundary): strategic reserves and long-term contracts are institutions that claim functional necessity but increasingly perform theater. The civilizational observer risks seeing immutable geology (Mountain) when actually observing contingent geopolitics — the false summit detector flags this. The perspectival gap reveals that 'dependency' is not a neutral technical fact but a structure with winners (exporter, trade regime) and losers (dependent nations), and the classification divergence is the key diagnostic.
 *
 * DIRECTIONALITY LOGIC:
 *   Dependent Nation Manufacturing: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Cannot switch, cannot negotiate, cannot exit. Trapped is the operative constraint — even powerful nations become powerless once trapped in REE dependency. Defense Sector: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction with slightly more agency than manufacturing (can invest in strategic reserves, substitution R&D), but strategic necessity prevents true exit. REE Monopolist: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; experiences constraint as profitable coordination. Alternative Sourcing Coalition: Mixed + mobile → d≈0.48, f(d)≈0.58. Moderate extraction because coalition has capital and can invest, but geopolitical barriers and capital concentration create asymmetry. Trade Regime: Institutional + constrained → d≈0.65, f(d)≈1.00. Moderate-high extraction; institutions that claim to manage dependency are themselves partly captured by exporter preferences. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain-seeking perspective that naturalizes dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the extraction-vs-coordination ambiguity by demonstrating that REE dependency combines both: (1) genuine coordination problem (uneven geology requires international trade, supply integration), (2) genuine extraction mechanism (monopoly refining, supply weaponization, captured institutions). The mandatrophy is resolved by showing that the ratio shifts over time. In the 1990s-2010 period (ε≈0.35-0.42), the constraint was more coordination-heavy: REE supply was integrated into a global value chain with multiple participants and pricing discovery. Post-2020 (ε≈0.65-0.68), it shifted toward extraction-heavy: supply weaponization, trade blocs, geopolitical conditioning. The rising extractiveness trajectory (0.42 → 0.68) shows the transition from coordination to extraction. Neither snare nor rope alone captures this; the snare classification reflects the current state (ε > 0.66, suppression > 0.60), but the historical rope classification would be accurate for the earlier period. The alternative-sourcing coalition's tangled-rope perspective captures the future state if investments succeed: a genuinely mixed mechanism with reduced monopoly power. The mandatrophy is thus resolved by indexing to time horizon and observational depth — the constraint is extractive from the immediate/biographical perspective of dependent nations, but mixed from the generational perspective of alternative sourcing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recycling_technology_threshold,
    'At what recycling efficiency percentage does REE dependency transition from extractive cartel to fair-priced coordination mechanism?',
    'Longitudinal tracking of closed-loop REE recycling adoption rates; correlation between recycling availability and supply-chain pricing power; simulation of supply elasticity under different recycling scenarios',
    'If threshold ≤ 40% current tech: many dependent nations can achieve autonomy within 15-20 years. If threshold > 70%: recycling alone insufficient; new mining and substitution essential. Classification changes from Snare to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_technology_threshold, empirical, 'Recycling efficiency threshold for supply chain independence').

omega_variable(
    substitution_feasibility_scope,
    'For which critical applications (semiconductors, permanent magnets, battery technology, laser systems) can REE substitution or synthetic alternatives be achieved within the next 20 years without unacceptable performance loss?',
    'Materials science R&D tracking; prototype performance data; cost-benefit analysis of substitutes vs. REE-dependent designs; patent landscape analysis',
    'If > 60% of current applications: dependency becomes partial and negotiated (Tangled Rope). If < 30%: fundamental REE dependence locked in for generation (persistent Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_feasibility_scope, empirical, 'Scope of feasible REE substitution in critical applications').

omega_variable(
    monopolist_supply_credibility,
    'Will the dominant REE exporter maintain reliable supply relationships, or will geopolitical conflict force supply weaponization?',
    'Historical analysis of past supply interruptions and their triggers; structural analysis of exporter''s domestic demand and export revenue dependence; game-theoretic modeling of incentive alignment',
    'If credibility remains high: dependency becomes stable coordination (Rope perspective strengthens). If credibility collapses: dependency becomes weaponized extraction and trigger for security conflict (Snare/Tangled Rope intensify).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monopolist_supply_credibility, preference, 'Whether monopolist maintains supply reliability or weaponizes dependency').

omega_variable(
    geopolitical_bifurcation_permanence,
    'Is the current bifurcation into supply-aligned blocs (China-Russia-Iran vs. US-EU-Japan) a temporary geopolitical state or a structural shift toward competing REE supply ecosystems?',
    'Tracking investment flows in mining and processing capacity across aligned vs. non-aligned states; long-term trade agreement structures; diplomatic statements regarding strategic autonomy and industrial policy',
    'If temporary: dependency may ease as competition increases (multiple suppliers). If structural: each bloc faces isolated dependency within its own ecosystem, and inter-bloc access becomes weaponizable leverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_bifurcation_permanence, conceptual, 'Whether bifurcation of supply into geopolitical blocs is permanent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ree_tr_t0, rare_earth_dependency, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ree_tr_t15, rare_earth_dependency, theater_ratio, 15, 0.45).
narrative_ontology:measurement(ree_tr_t30, rare_earth_dependency, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(ree_be_t0, rare_earth_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ree_be_t15, rare_earth_dependency, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ree_be_t30, rare_earth_dependency, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_dependency, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_dependency, semiconductor_supply_chain).
narrative_ontology:affects_constraint(rare_earth_dependency, renewable_energy_manufacturing).
narrative_ontology:affects_constraint(rare_earth_dependency, defense_industrial_base).
narrative_ontology:affects_constraint(rare_earth_dependency, technological_sovereignty).

% DUAL FORMULATION NOTE:
% REE dependency decomposes into three distinct constraint stories by observable: (1) geological constraint (ε≈0.08, Mountain) — uneven element distribution is immutable, (2) refining bottleneck (ε≈0.42, Tangled Rope) — concentrated refining capacity is contingent and addressable, (3) geopolitical weaponization (ε≈0.68, Snare) — supply restriction as political leverage. This story addresses the third decomposition. The first (geology) is the upstream constraint; the second (refining) is intermediate; the third (weaponization) is downstream and most extractive. All three must be linked for complete constraint family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_dependency, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
