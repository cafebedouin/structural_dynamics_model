% ============================================================================
% CONSTRAINT STORY: geopolitical_mineral_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_mineral_dependency, []).

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
 *   constraint_id: geopolitical_mineral_dependency
 *   human_readable: Geopolitical Mineral Dependency Constraint
 *   domain: geopolitical_economic
 *
 * SUMMARY:
 *   Geopolitical mineral dependency constrains import-dependent states
 *   through structural vulnerability to supply disruption, price volatility,
 *   and coercive leverage. The constraint exhibits tangled rope mechanics: a
 *   genuine coordination problem (connecting geographically dispersed mineral
 *   deposits to globally distributed demand for clean energy and defense
 *   technology) is coupled with asymmetric extraction (supply-controlling
 *   actors benefit from maintaining artificial scarcity and coercive
 *   leverage). The constraint's base extractiveness (0.58) reflects moderate
 *   extraction mechanisms — not maximal because supply-controlling states
 *   benefit from maintaining stable, profitable relationships rather than
 *   optimizing coercion; constrained because import-dependent states have
 *   partial agency through diversification and substitution strategies.
 *   Suppression (0.72) is high: technological constraints on substitution,
 *   long deployment timelines for alternative supply, and political barriers
 *   to domestic mining create significant barriers to exit. Theater ratio
 *   (0.48) is moderate-low: while some supply-security rhetoric is
 *   performative, the underlying constraint is functionally structural —
 *   supply concentration creates genuine vulnerability regardless of how it
 *   is framed. The measurements show escalating extractiveness and
 *   suppression over a 20-year horizon, driven by accelerating clean energy
 *   transition demand (increasing mineral dependency just as substitution
 *   timelines remain distant) and increasing political weaponization of
 *   supply chains.
 *
 * KEY AGENTS:
 *   - Import-Dependent States (powerless/trapped): Nations requiring rare earth elements, cobalt, lithium for grid infrastructure and defense. Minimal substitution options on relevant timescales. Bear full extraction cost during transition.
 *   - Supply-Controlling States (institutional/arbitrage): China (rare earths, processing), DRC (cobalt), Australia (lithium), Indonesia (nickel). Benefit from concentrated supply via pricing power and geopolitical leverage.
 *   - Vertically Integrated Mining Companies (institutional/arbitrage): Multinational extractors and processors that capture rents from supply concentration and maintain concession monopolies.
 *   - Clean Energy Transition Coalition (organized/constrained): IEA, multinational renewable energy firms, developed states investing in grid decarbonization. Genuine coordination function (deploying clean tech) coupled with extraction (higher mineral prices, supply vulnerability).
 *   - Diversification-Seeking States (moderate/constrained): States investing in domestic supply, stockpiles, and technology substitution. Face high costs and long timelines; experience both coordination benefits (reduced dependency) and extraction (opportunity costs, supply vulnerability during transition).
 *   - Substitution and Recycling Innovation Programs (organized/mobile): Battery chemistry research, closed-loop manufacturing, alternative processing. Represent temporary support pathways with sunset clauses.
 *   - Colonial-Era Mining Regime (institutional/arbitrage): Legacy extraction monopolies and concession agreements. Persists through institutional inertia despite degraded coordination function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_mineral_dependency, 0.58).
domain_priors:suppression_score(geopolitical_mineral_dependency, 0.72).
domain_priors:theater_ratio(geopolitical_mineral_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_mineral_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(geopolitical_mineral_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geopolitical_mineral_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_mineral_dependency, tangled_rope).
narrative_ontology:human_readable(geopolitical_mineral_dependency, "Geopolitical Mineral Dependency Constraint").
narrative_ontology:topic_domain(geopolitical_mineral_dependency, "geopolitical_economic").

domain_priors:requires_active_enforcement(geopolitical_mineral_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_mineral_dependency, supply_chain_controlling_states).
narrative_ontology:constraint_beneficiary(geopolitical_mineral_dependency, vertically_integrated_mining_companies).
narrative_ontology:constraint_victim(geopolitical_mineral_dependency, import_dependent_states).
narrative_ontology:constraint_victim(geopolitical_mineral_dependency, clean_energy_transition_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPORT-DEPENDENT STATE (SNARE) — Nations requiring rare earth elements, cobalt, or lithium for grid infrastructure or defense systems have minimal exit options. Domestic substitution is technologically infeasible on relevant timescales. Vulnerability is structural and enforced through supply concentration. Suppression is high: disruption threatens energy security, industrial capacity, and military capability. The constraint extracts through coercive leverage (price volatility, supply denial threats) with minimal coordination benefit.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DIVERSIFICATION-SEEKING STATE (TANGLED ROPE) — States pursuing supply diversification (developing domestic deposits, building stockpiles, investing in substitution) experience mixed coordination and extraction. The constraint coordinates genuine collective action: multiple actors investing in redundant supply paths does increase systemic resilience. But extraction runs upward during the transition: higher mineral prices during the buildout phase, vulnerability to supply denial while diversification remains incomplete, and opportunity costs of foregone other investments. High suppression: the transition is technically and politically difficult, with years or decades of implementation lag.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPPLY-CONTROLLING STATE (ROPE) — Nations or actors controlling major supply chains (China with rare earths, DRC with cobalt, Australia with lithium) experience the constraint as pure coordination: managing export flows to meet global demand while maintaining stable pricing and supply relationships. They benefit from the dependency structure via arbitrage pricing and geopolitical leverage, but the coordination function is real — stable supply is valuable to all parties and the controlling state has strong incentives to maintain it. Low effective extraction because the controller has agency and faces counter-incentive pressures (reputational damage, diversification investment by others, technological substitution).
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLEAN ENERGY TRANSITION COALITION (TANGLED ROPE) — Coordinated international actors (IEA, multinational firms, developed states investing in renewable infrastructure) experience the constraint as a hybrid: genuine coordination on technology standards, supply forecasting, and investment sequencing alongside extraction mechanisms. The coalition coordinates global deployment of clean tech (a coordination good) while simultaneously being extracted from through mineral prices inflated by concentrated supply. High suppression because transition timelines are compressed — technology deployment cannot wait for supply diversification. Active enforcement: international standards for mineral sourcing, trade negotiations, tariff regimes all actively manage the constraint.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SUBSTITUTION AND RECYCLING PATHWAY (SCAFFOLD) — Organized innovation programs (battery chemistry transitions, circular economy mandates, materials science R&D) represent temporary support structures for supply diversification. Effective extraction is low because these pathways have concrete sunset clauses: as sodium-ion batteries mature, cobalt dependency falls; as closed-loop manufacturing scales, recycling reduces virgin ore demand; as alternative rare-earth processing becomes cost-competitive, supply chokepoints ease. Theater is moderate because some programs are performative (symbolic recycling commitments without infrastructure) but core innovation trajectories are functional.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLONIAL-ERA MINING REGIME (PITON) — Traditional extraction monopolies and concession agreements (established under colonialism or Cold War client relationships) persist through institutional inertia despite obsolescence. Extraction companies maintain legacy arrangements through lobbying and regulatory capture, but the actual coordination function (connecting deposits to industrial users) has degraded as supply chains have globalized and new entrants have arrived. Theater ratio is high: legacy agreements invoke property rights and stability language while actual value capture has shifted to processing and downstream integration.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PHYSICAL SCARCITY VIEW (NATURAL LAW CANDIDATE) — From a civilizational/universal perspective, rare earth and critical mineral concentration in geology is an immutable physical fact: deposits are geographically concentrated, some require complex processing, and substitution is physically constrained by technological requirements. This perspective frames dependency as natural law — unavoidable given physical constraints. However, structural data contradicts this: supply concentration is partially political (export restrictions, concession monopolies, processing concentration in specific states), and the extraction mechanisms are institutional choices, not laws of physics. False summit candidate.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SYSTEMIC VIEW) — Cross-positional analysis reveals that geopolitical mineral dependency is a hybrid coordination-extraction mechanism: genuine coordination problems exist (connecting dispersed deposits to global demand, maintaining supply chain stability, managing price volatility), but extraction mechanisms are layered on top of this coordination function. The constraint persists because it solves real coordination problems AND because it delivers asymmetric benefits. This is the canonical tangled rope signature.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_mineral_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_mineral_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_mineral_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_mineral_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_mineral_dependency, TR),
    TR >= 0.70.

:- end_tests(geopolitical_mineral_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts substantially but not maximally from victims because supply-controlling states face counter-incentives to absolute coercion. Stable supply generates more long-term profit than disruption; excessive extraction triggers diversification investment and substitution R&D. The value reflects that extraction runs upward during energy transition (increasing mineral demand while substitutes remain distant) but stabilizes if diversification succeeds. Suppression (0.72): High. Victims face technological barriers to substitution (rare earths have no perfect substitutes for many defense and grid applications on 10-20 year timescales), geological facts that cannot be wished away, and political barriers to domestic mining (environmental opposition, regulatory complexity, capital requirements). But suppression is not total — diversification pathways exist, substitution is technologically feasible at longer timescales, and recycling can reduce virgin demand. Theater ratio (0.48): Moderate. Much supply-security discourse is performative (symbolic investments in substitution, high-level trade negotiations that achieve modest results), but the underlying constraint is functionally structural — supply concentration creates real vulnerability. The measurements show theater increasing from 0.35 to 0.62 over 20 years, reflecting that solutions remain aspirational while supply vulnerability deepens.
 *
 * PERSPECTIVAL GAP:
 *   The most severe gap is between the supply-controlling state (Rope perspective: experiences coordination as the primary function, extraction as incidental) and the import-dependent state (Snare perspective: experiences only extraction and vulnerability). From the controlling state's view, managing global mineral supply is a coordination problem that they solve well; from the dependent state's view, they are held hostage to another actor's preferences. The analytical observer sees both functions simultaneously: there IS a coordination problem (managing the connection between dispersed deposits and global demand), AND there IS extraction (captured rents flow upward to concentrated controllers and away from dispersed consumers). The substitution pathway sees a temporary constraint with a concrete sunset (Scaffold), while the colonial-era mining regime sees a perpetual arrangement (Piton). The clean energy coalition sees mixed coordination and extraction simultaneously (Tangled Rope) — they are coordinating global deployment of clean tech while being extracted from through mineral prices. The diversification-seeking state sees a transition problem with partial agency (Tangled Rope) — they can reduce dependency through investment, but face high suppression during the transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Supply-controlling states derive d from beneficiary status + arbitrage exit options → low d → negative f(d) → they experience the constraint as low extraction. Import-dependent states derive d from victim status + trapped exit options → high d → high f(d) → they experience maximum extraction. Organized diversification-seeking states derive d from victim status + constrained exit options → moderate-high d → moderate f(d) → they experience moderate extraction partially mitigated by agency. The analytical observer uses canonical d for the analytical power atom, producing a balanced perception that captures both coordination and extraction functions. The scope modifier σ(S) applies at global scale (σ=1.2), amplifying effective extraction for all actors — the constraint operates across state borders and cannot be locally evaded.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that both coordination and extraction are structurally real. The coordination function is genuine: connecting geographically dispersed mineral deposits to globally distributed technology demand is a hard coordination problem; supply-controlling states solve part of this problem by maintaining supply chains. The extraction function is equally genuine: beneficiaries capture asymmetric rents from controlling the connection points; victims pay above-competitive prices and face vulnerability to coercion. The tangled rope classification captures this duality. The constraint persists because it solves the coordination problem while simultaneously delivering asymmetric benefits — dismantling the extraction would break the coordination unless replaced by alternative mechanisms (diversification, substitution, recycling, stockpiling). The mandatrophy is resolved by recognizing that the coordination and extraction functions are NOT competing interpretations of the same facts — they are two real mechanisms operating simultaneously in a single institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_concentration_origin,
    'Is mineral supply concentration primarily a result of geological distribution (natural) or institutional arrangements (political)?',
    'Comparative analysis of proven reserves vs. production capacity; examination of export restrictions, concession agreements, and processing monopolies; modeling of cost curves for secondary sources and substitutes',
    'If primarily geological: mountain classification strengthens, natural law framing is justified. If primarily institutional: false summit confirmed, constraint is contingent on political choices. Mixed case: actual structure is that modest geological concentration (real) has been amplified into extreme supply chokepoints through institutional choices (export restrictions, processing monopolies, concession exclusivity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_concentration_origin, empirical, 'Whether mineral concentration is natural or politically amplified').

omega_variable(
    substitution_feasibility_timeline,
    'What is the actual technology timeline for viability of sodium-ion batteries, alternative rare-earth processing, and synthetic substitutes for critical minerals?',
    'Technology readiness level (TRL) assessment; cost curve projection to grid parity; pilot-scale production capacity tracking; materials science literature on fundamental constraints',
    'If feasible within 10 years at scale: scaffold perspective is correct, sunset is real, constraint is temporary. If 30+ years: scaffold is aspirational, constraint persists across multiple human lifespans, extraction mechanisms harden. If infeasible: mountain perspective gains credence, but only if technological limits are shown to be hard rather than economic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_feasibility_timeline, empirical, 'Timeline for technological substitution and supply alternatives').

omega_variable(
    recycling_economics_closure,
    'Can closed-loop mineral recycling achieve economic viability without permanent subsidies or mandates, and what percentage of supply can it realistically cover?',
    'Cost analysis of closed-loop battery manufacturing vs. virgin mining; tracking of regulatory mandates (EU battery directive, US IRA mineral provisions) to identify whether economics or policy drives adoption; pilot program outcomes',
    'If viable at 40%+ coverage without subsidy: suppression drops substantially, dependency constraint shifts from snare toward rope. If subsidy-dependent or limited to <20%: recycling is theater, suppression remains high, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_economics_closure, empirical, 'Economic viability of closed-loop mineral recycling at scale').

omega_variable(
    geopolitical_coercion_frequency,
    'How often do supply-controlling states actually exercise coercive leverage (supply denial, price manipulation, political conditionality) vs. maintaining stable supply for economic benefit?',
    'Historical analysis of supply disruptions (1970s oil embargo as analog; rare earth export restrictions 2010-2015; recent lithium price manipulation); frequency and duration of disruption events; correlation with political demands',
    'If coercion is frequent and effective: snare classification strengthens for victims. If coercion is rare and countermeasured: extraction is more moderate, victims have some agency. Baseline: coercion threat is the suppression mechanism even if actual disruption is rare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_coercion_frequency, empirical, 'Actual vs. threatened use of mineral supply coercion').

omega_variable(
    diversification_investment_sufficiency,
    'Is the level of global investment in diversification (domestic mining, stockpiles, substitution R&D, processing capacity in non-controlling states) sufficient to reduce dependency within the generational timescale?',
    'Aggregate spending on mineral security initiatives (US IRA, EU CRMA, Japanese and European sovereign wealth funds); capacity growth in alternative sources; timeline comparison to demand growth in clean energy transition',
    'If investment is insufficient (likely): diversification-seeking states remain trapped, tangled rope or snare classification persists. If investment accelerates to match transition timelines: scaffold sunset becomes real, constraint weakens across next 20 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversification_investment_sufficiency, empirical, 'Adequacy of global investment in supply diversification').

omega_variable(
    identity_lock_in_dependency_framing,
    'Have import-dependent states internalized mineral dependency as an immutable feature of their identity or development pathway, preventing cognitive escape even if material barriers eased?',
    'Policy analysis: presence of ''strategic dependency'' language in national security doctrine; lobbying by incumbent supply-chain actors against disruptive alternatives; institutional resistance to domestic mining or substitution despite profitability; comparative analysis of states that have escaped similar dependencies',
    'If identity lock is present: states remain trapped even if exits emerge, because they cannot see alternatives as legitimate. If absent: states will rapidly shift supply strategies when economically feasible. This determines whether the constraint is perpetually snare (for trapped victims) or transitional tangled rope (for agents with escape paths).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_dependency_framing, conceptual, 'Identity lock-in preventing escape from dependency framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_mineral_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geomin_tr_t0, geopolitical_mineral_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(geomin_tr_t10, geopolitical_mineral_dependency, theater_ratio, 10, 0.48).
narrative_ontology:measurement(geomin_tr_t20, geopolitical_mineral_dependency, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(geomin_be_t0, geopolitical_mineral_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(geomin_be_t10, geopolitical_mineral_dependency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(geomin_be_t20, geopolitical_mineral_dependency, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(geomin_su_t0, geopolitical_mineral_dependency, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(geomin_su_t10, geopolitical_mineral_dependency, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(geomin_su_t20, geopolitical_mineral_dependency, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_mineral_dependency, resource_allocation).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, clean_energy_transition_supply_chain).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, semiconductor_supply_security).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, defense_technology_dependence).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, economic_coercion_via_trade).

% DUAL FORMULATION NOTE:
% Geopolitical mineral dependency is downstream of geological facts (concentration of deposits) but distinct from them. The geological constraint would be a mountain (immutable physical scarcity of specific minerals). The geopolitical constraint is a tangled rope (institutional choices about supply chain organization, processing concentration, and export control). These are separate constraint stories. The geological facts CREATE OPPORTUNITY for the geopolitical extraction, but the extraction mechanism is political, not physical.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_mineral_dependency, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
