% ============================================================================
% CONSTRAINT STORY: rare_earth_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Strategic rare earth element dependency represents a structural asymmetry
 *   where one nation (or small bloc of allied producers) controls 85-95% of
 *   global processing capacity for materials essential to defense systems,
 *   renewable energy infrastructure, and advanced electronics manufacturing.
 *   Dependent nations face a snare: supply disruption would cripple military
 *   and technological capacity, but developing independent capacity requires
 *   15-25 years and billions in capital investment. The constraint exhibits
 *   the full range of Deferential Realism types from different structural
 *   positions. The dependent nation experiences maximum extraction with no
 *   exit (snare). The monopoly producer experiences pure coordination — they
 *   benefit from the arrangement with full optionality to arbitrage supply.
 *   Organized coalitions (technology sectors, allied nations) experience
 *   mixed coordination and extraction (tangled rope). Supply diversification
 *   initiatives see a real sunset path (scaffold). Cold War strategic
 *   dependency framing persists as institutional theater (piton). A
 *   civilizational observer might naturalize the constraint as inherent
 *   scarcity (false summit). The constraint's extractiveness has increased
 *   over two decades (0.42→0.58) as geopolitical concentration has tightened
 *   and technology dependence has grown. Theater ratio has remained low
 *   (0.28-0.38) because the extraction is functional, not performative —
 *   disruption is a genuine threat, not a symbolic ritual.
 *
 * KEY AGENTS:
 *   - Dependent Nation (US, EU, Japan, India, Australia): Primary victim (powerless/trapped) — no immediate exit from supply dependency; 15-25 year development timeline for alternatives
 *   - Monopoly Producer (China): Primary beneficiary (institutional/arbitrage) — controls 85-95% of processing; captures economic rent and geopolitical leverage
 *   - Technology Sector Coalition: Secondary victim/organized agent (organized/constrained) — benefits from supply coordination advocacy but constrained by pricing and supply throttling
 *   - Supply Diversification Initiative: Organized agent (organized/mobile) — government programs building alternative capacity with explicit sunset targets (30-40% domestic/allied capacity)
 *   - Recycling Infrastructure Programs: Organized agent (organized/mobile) — technical pathway to reduce primary demand; nascent industry with 10-15 year maturation curve
 *   - Cold War Strategic Dependency Institutional Framework: Institutional actor maintaining performative logic (institutional/constrained) — SPR protocols, annual reviews, national security framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_dependency, 0.58).
domain_priors:suppression_score(rare_earth_dependency, 0.72).
domain_priors:theater_ratio(rare_earth_dependency, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_dependency, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_dependency, snare).
narrative_ontology:human_readable(rare_earth_dependency, "Strategic Rare Earth Element Dependency").
narrative_ontology:topic_domain(rare_earth_dependency, "economic/geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_dependency, monopoly_producer).
narrative_ontology:constraint_victim(rare_earth_dependency, dependent_nation).
narrative_ontology:constraint_victim(rare_earth_dependency, technology_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT NATION (SNARE) — Locked into REE imports with no immediate exit; cannot develop domestic alternatives within decisive timeframes (15-25 years for new mining/processing infrastructure). Supply disruption threatens military capability, technology manufacturing, and energy transition. Suppression is maximum: alternative suppliers are geographically unavailable, monopolist controls 85-95% of processing capacity. Extraction runs at maximum force.
constraint_indexing:constraint_classification(rare_earth_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MONOPOLY PRODUCER (ROPE) — Experiences constraint as pure coordination: setting export prices and managing global supply achieves their material objectives. Can arbitrage supply against demand. Net beneficiary with full exit optionality — can reduce exports, shift to other buyers, or invest in downstream processing. Low effective extraction because extraction runs toward them.
constraint_indexing:constraint_classification(rare_earth_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: TECHNOLOGY SECTOR COALITION (TANGLED ROPE) — Organized enough to lobby for strategic reserves and subsidy programs (coordination benefit), but constrained by supply dependencies and price manipulation. Experiences both genuine coordination (sharing reserve stockpiles, developing substitutes collectively) and asymmetric extraction (monopolist price-setting, supply throttling during geopolitical tensions). Mixed experience with partial exit via diversification.
constraint_indexing:constraint_classification(rare_earth_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SUPPLY DIVERSIFICATION INITIATIVE (SCAFFOLD) — Government-backed programs to develop alternative suppliers (Vietnam, Indonesia), recycling infrastructure, and substitution technologies. Structured as temporary support with explicit sunset: once domestic/allied capacity reaches 30-40% of demand, the dependency constraint loses force. Theater is low (functional infrastructure development) compared to performative strategic-reserves rhetoric. Exit path is real and measurable.
constraint_indexing:constraint_classification(rare_earth_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COLD WAR STRATEGIC DEPENDENCY MODEL (PITON) — Historical framework ('critical materials,' 'strategic reserves') persists as institutional theater long after its functional basis has shifted. During Cold War, physical scarcity and technological lock-in were real; post-2000, geopolitical concentration became the dominant factor. Institutions (SPR protocols, annual reviews, 'national security' framing) maintain the old dependency logic performatively, even as substitution and recycling make physical scarcity less binding. Theater ratio is high because much institutional activity (reserve rotations, threat assessments) is now largely symbolic.
constraint_indexing:constraint_classification(rare_earth_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a universal timescale, REE dependency might appear as an immutable constraint: REEs are geographically concentrated, processing requires specialized chemistry, recycling loops take decades to mature. This perspective risks naturalizing what is actually a contingent combination of geology (concentration), policy (tariffs/quotas), and technology (substitution alternatives exist but are not deployed). The engine will identify this as a false summit revealing that 'scarcity' language naturalizes a geopolitical extraction mechanism.
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
 *   Extractiveness (0.58): Moderate-high. The monopoly producer captures significant economic rent through price premium (10-20% above competitive pricing) and geopolitical leverage. However, extraction is not at snare maximum (0.66+) because substitution alternatives exist and recycling is maturing — the monopoly is contingent on technological lock-in, not absolute physical scarcity. The trend over 20 years (0.42→0.58) reflects tightening geopolitical concentration and increased strategic importance of REEs for energy transition (wind turbines, EV motors). Suppression (0.72): High. Barriers include geographic monopoly (processing capacity concentrated in single nation), specialized chemistry requiring years to replicate, high capital barriers ($2-3B per refinery), permitting delays (10+ years in Western democracies), and trade policy restrictions (tariffs, quotas). But suppression is not absolute (0.90+) because technological substitution is advancing and recycling pathways exist. Theater ratio (0.38): Low. The extraction is functional and threat-based, not performative. Supply disruptions have occurred (2010 embargo, 2022 restricted shipments) with real economic consequences. Institutional activity (strategic reserves, diversification funding) addresses actual constraints, not ritual. The low theater ratio distinguishes this from piton-degraded constraints.
 *
 * PERSPECTIVAL GAP:
 *   The dependent nation sees a snare: locked into imports, threatened by weaponization, maximum extraction force. The monopoly producer sees a rope: coordination of supply and pricing achieves their objectives, full exit optionality, net beneficiary. The technology sector sees tangled rope: benefits from advocacy coordination (subsidies, reserve programs) but bears extraction through supply volatility and price premiums. The diversification initiative sees scaffold: real exit path (30-40% capacity target) with measurable sunset. The institutional cold war framework sees piton: the logic of strategic dependency persists performatively through reserve rotations and threat assessments, even as technological alternatives mature. The civilizational observer risks seeing a mountain (inherent scarcity) until structural analysis reveals the geopolitical concentration as contingent, not natural. This perspectival gap is the core diagnostic value of the constraint story.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: beneficiary status, victim status, and exit capacity. The monopoly producer (beneficiary + arbitrage exit) has low d → low/negative χ. The dependent nation (victim + trapped exit) has high d → high χ. Organized coalitions (mixed beneficiary/victim + constrained exit) have medium d. The diversification initiative (organized + mobile exit) has lower d because exit optionality reduces experienced extraction. The piton perspective (institutional + constrained) has moderate d because the institutional actor benefits from maintaining the old logic but is constrained by the alternative pathways. The analytical observer (analytical exit options) occupies a special position where d is computed from the presheaf of all perspectives rather than from a single structural location.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy by showing that snare classification for the dependent nation is legitimate and not confusion with tangled rope or piton. The snare differs from tangled rope because: (1) there is no significant coordination function benefiting the victim (the dependent nation gains no organizational benefit from the dependency, only losses), (2) suppression is high and not balanced by countervailing power (no symmetry), and (3) exit options are genuinely trapped (development timelines are 15-25 years, not negotiable). The snare differs from piton because: (1) the extraction is functional and threat-based, not performative (supply disruptions have real consequences), (2) theater ratio is low, indicating actual constraint rather than institutional inertia, and (3) the monopoly producer actively maintains the mechanism, not accidentally through degraded ritual. The mandatrophy is resolved by the perspectival gap: snare is the dependent nation's genuine experience; rope is the monopoly producer's genuine experience. Both are accurate from their respective structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_feasibility_threshold,
    'What substitution rate (% of REE functions replaced by alternative materials or processes) is required to break monopoly pricing power?',
    'Technology roadmap analysis: identify which REE applications have viable substitutes and estimate cost parity timelines; track adoption rates of substitutes in actual manufacturing',
    'If threshold < 20%: monopolist loses leverage quickly, snare weakens to rope. If threshold > 60%: dependency persists through transition, snare remains high-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_feasibility_threshold, empirical, 'Substitution rate required to break monopoly pricing').

omega_variable(
    processing_capacity_replication_timeline,
    'How many years are required to build independent processing capacity at 30% of current demand in a dependent nation or allied bloc?',
    'Capital cost estimates, permitting timelines, technical training requirements; case studies of Vietnam and Indonesian refineries; comparison to Chinese capacity-building timelines',
    'If < 10 years: scaffold sunset is credible, exit path is real. If > 25 years: scaffold is aspirational theater, victim remains trapped indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(processing_capacity_replication_timeline, empirical, 'Timeline for independent processing capacity replication').

omega_variable(
    geopolitical_weaponization_probability,
    'What is the likelihood that a monopoly producer will use supply disruption as an explicit coercive tool against a dependent nation within a 10-year window?',
    'Historical precedent analysis (2010 rare earth embargo, lithium restrictions); institutional signaling (public statements, policy documents); structural incentive analysis (geopolitical tensions, trade disputes)',
    'If probability > 60%: snare classification is conservative. If probability < 20%: snare may degrade to tangled rope if beneficiary exploits dependency more through price mechanisms than explicit coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_weaponization_probability, empirical, 'Probability of supply weaponization within 10 years').

omega_variable(
    recycling_loop_maturity_rate,
    'What percentage of REE demand can be met by closed-loop recycling (from electronic waste and end-of-life products) at industrial scale within 15 years?',
    'Current recycling rates by element; technology maturation curves; infrastructure investment commitments; trace data on rare earth recovery from WEEE streams',
    'If > 40%: scaffold exit path accelerates, snare weakens toward rope or tangled rope. If < 15%: victim remains dependent on primary sources, snare suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_loop_maturity_rate, empirical, 'Closed-loop recycling capacity growth within 15 years').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ree_tr_t0, rare_earth_dependency, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ree_tr_t10, rare_earth_dependency, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ree_tr_t20, rare_earth_dependency, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(ree_be_t0, rare_earth_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ree_be_t10, rare_earth_dependency, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ree_be_t20, rare_earth_dependency, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_dependency, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_dependency, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(rare_earth_dependency, lithium_battery_dependency).
narrative_ontology:affects_constraint(rare_earth_dependency, energy_transition_mineral_constraints).

% DUAL FORMULATION NOTE:
% Rare earth element dependency is the primary constraint in a family of mineral/resource concentration constraints. It is upstream of semiconductor supply and battery constraints in that REE availability is a precondition for manufacturing both; however, it is downstream of broader geopolitical concentration dynamics. The family structure reflects how technological transitions (renewable energy, electrification) have created new strategic dependencies on mineral concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_dependency, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
