% ============================================================================
% CONSTRAINT STORY: rare_earth_supply_chain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_supply_chain, []).

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
 *   constraint_id: rare_earth_supply_chain
 *   human_readable: Rare Earth Element Supply Chain Constraint
 *   domain: economic/geopolitical/industrial
 *
 * SUMMARY:
 *   The rare earth element supply chain constraint represents a hybrid
 *   coordination-extraction mechanism that emerged from geographic
 *   concentration of processing capacity (>70% in China by 2010) combined
 *   with rapidly increasing demand from renewable energy, electronics, and
 *   defense applications. The constraint exhibits genuine coordination
 *   functions (global supply optimization, demand management, process
 *   efficiency) alongside severe extraction (pricing power during
 *   disruptions, supplier lock-in, geopolitical coercion risk). The supply
 *   chain is not immutable — alternative mining sources exist, substitution
 *   technologies are maturing, and recycling infrastructure is developing —
 *   but all exit pathways face substantial barriers (capital intensity,
 *   technology development risk, environmental remediation costs,
 *   decades-long deployment horizons). Theater ratio remains moderate because
 *   the underlying supply coordination is functionally necessary, but
 *   strategic reserve announcements and emergency supply mobilizations create
 *   theater that masks persistent structural dependency. The measurement
 *   trajectory shows extractiveness rising from 0.35 to 0.58 over the 15-year
 *   interval, reflecting accumulating dependency despite diversification
 *   efforts, while theater ratio rises more gradually from 0.32 to 0.48,
 *   indicating that performative crisis management is increasing but has not
 *   yet become the constraint's primary mechanism.
 *
 * KEY AGENTS:
 *   - China's Processing Establishment: Primary beneficiary (institutional/arbitrage) — controls >70% of global rare earth processing; net benefits from geographic concentration through pricing power and supply allocation control
 *   - Dependent Manufacturing Nations: Primary victims (powerless/trapped) — cannot substitute or delay; face extraction through pricing shocks and supply disruptions; no credible exit within 10-20 year horizon
 *   - Alternative Supply Developers: Secondary victims (moderate/constrained) — invest in mining/processing but face stranded asset risk when Chinese processing dominance reasserts during price collapses
 *   - Technology Transition Coalition: Organized agents (organized/mobile) — OECD initiatives, standard bodies, renewable consortia building rare-earth-free substitutes; see constraint as temporary with 15-25 year sunset
 *   - Environmental Remediation Communities: Affected populations (individual/constrained) — experience both employment coordination benefits and asymmetric environmental extraction (pollution, health impacts, water contamination)
 *   - Legacy Demand Systems: Institutional inertia (institutional/arbitrage) — 2000s-era demand forecasting models still driving emergency stockpiling despite substitution reducing long-term demand
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent geographic/political concentration as thermodynamic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_supply_chain, 0.58).
domain_priors:suppression_score(rare_earth_supply_chain, 0.72).
domain_priors:theater_ratio(rare_earth_supply_chain, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_supply_chain, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_supply_chain, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_supply_chain, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_supply_chain, tangled_rope).
narrative_ontology:human_readable(rare_earth_supply_chain, "Rare Earth Element Supply Chain Constraint").
narrative_ontology:topic_domain(rare_earth_supply_chain, "economic/geopolitical/industrial").

domain_priors:requires_active_enforcement(rare_earth_supply_chain).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_supply_chain, china_processing_monopoly).
narrative_ontology:constraint_beneficiary(rare_earth_supply_chain, incumbent_technology_firms).
narrative_ontology:constraint_victim(rare_earth_supply_chain, dependent_manufacturing_nations).
narrative_ontology:constraint_victim(rare_earth_supply_chain, technology_diversification_efforts).
narrative_ontology:constraint_victim(rare_earth_supply_chain, supply_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT MANUFACTURERS (SNARE) — Nations and firms dependent on rare earth elements cannot substitute or delay. Exit is structurally impossible without decades of mining/processing infrastructure development. Suppression operates through geographic concentration: China controls >70% of global processing capacity. Manufacturers face extraction through pricing power during supply disruptions, with no credible exit threat.
constraint_indexing:constraint_classification(rare_earth_supply_chain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE SUPPLY DEVELOPERS (TANGLED ROPE) — Countries investing in alternative rare earth mining and processing (US, Australia, Japan) face high capital costs and environmental remediation burdens. They benefit from supply security coordination through diversification but bear massive extraction in the form of stranded assets when prices collapse and Chinese processing dominance reasserts. Constrained exit: switching back to Chinese supply after domestic investment losses.
constraint_indexing:constraint_classification(rare_earth_supply_chain, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHINA PROCESSING ESTABLISHMENT (ROPE) — Chinese processors experience the constraint as pure coordination: managing global supply according to demand signals, optimizing export logistics, and maintaining market share through efficiency. Net beneficiary position with arbitrage options (can shift supply to allied nations or reduce processing if prices fall). Experiences the constraint as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(rare_earth_supply_chain, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY TRANSITION COALITION (SCAFFOLD) — Organized groups (OECD supply chain initiatives, technology standard bodies, renewable energy consortia) recognize rare earth dependency as a temporary bottleneck being overcome through technology substitution: permanent magnets using less dysprosium/terbium, rare-earth-free electric motors, critical mineral recycling infrastructure. Has sunset clause: estimated 15-25 year horizon as substitution technologies mature and circular economy infrastructure scales. Mobile exit: coalition members can accelerate transition investments.
constraint_indexing:constraint_classification(rare_earth_supply_chain, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY DEMAND SYSTEMS (PITON) — Industrial planning models developed in the 2000s-2010s still treat rare earth demand as continuously rising, driving emergency stockpiling and strategic reserves despite evidence that substitution and recycling are reducing long-term demand. Theater ratio high: theatrical supply crisis management (emergency declarations, export restrictions, buying panics) persists despite structural fundamentals shifting. Maintained through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(rare_earth_supply_chain, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL REMEDIATION COMMUNITIES (TANGLED ROPE) — Local populations hosting rare earth mining/processing operations experience both coordination benefits (employment, infrastructure, economic activity) and asymmetric extraction through environmental contamination, water pollution, and health impacts. Constrained exit: economic dependency on mining operations; cannot leave community without material cost. Suppression high: environmental data suppression, occupational health underreporting, political barriers to regulatory enforcement.
constraint_indexing:constraint_classification(rare_earth_supply_chain, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, rare earth element supply is constrained by absolute thermodynamic limits on energy-efficient separation: rare earths occur in nature as dilute mixtures and separation requires energy-intensive processing. This perspective sees the bottleneck as immutable law of chemistry/physics. However, structural data contradicts mountain classification — the constraint's suppression and extraction are contingent on political/economic organization, not thermodynamic fundamentals. Engine will flag this as false summit.
constraint_indexing:constraint_classification(rare_earth_supply_chain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_supply_chain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_supply_chain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_supply_chain, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_supply_chain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_supply_chain, TR),
    TR >= 0.70.

:- end_tests(rare_earth_supply_chain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts through pricing power during disruptions, supply sequencing favoring allied nations, and forcing dependent manufacturers to either accept higher costs or invest in costly substitutes. The extraction is real but not maximal because: (1) alternative supply sources exist and are developing; (2) substitution technologies are maturing; (3) recycling infrastructure is scaling; (4) no single manufacturer is completely dependent — diversification is possible at cost. The trajectory rising from 0.35 to 0.58 reflects accumulating dependency despite diversification efforts, as demand growth outpaces alternative supply scaling. Suppression (0.72): High. Structural barriers to exit include: geographic concentration of processing capacity (requires 10-15 year capital cycle to establish new mines), technology lock-in (rare earth specifications embedded in 20-year-old equipment designs), scale advantages (Chinese processing achieves 30-40% cost advantage through volume), geopolitical barriers (China restricts export licensing during tensions), and information asymmetries (Chinese firms control supply data). Theater ratio (0.48): Moderate. The constraint's coordination function (global supply management, demand signaling, logistics optimization) is functionally necessary and substantial — theater is not the primary mechanism. However, theatrical elements exist: emergency declarations overstating disruption severity, strategic reserve announcements that provide psychological comfort without structural resilience, and public stockpiling that doesn't address underlying dependency.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is stark. China's processing establishment experiences the constraint as pure coordination (rope) — managing global supply according to legitimate demand signals. Dependent manufacturers experience it as near-immovable extraction (snare) — trapped by thermodynamic and economic limits on short-term substitution. The analytical observer risks misclassifying this as a mountain (natural law of chemistry) when it is actually a tangled rope (genuine coordination plus extractive geographic monopoly). The technology transition coalition sees the constraint as temporary (scaffold) with a sunset tied to substitution maturation — their perspective is forward-looking and assumes access to capital and technical expertise to execute transition. Environmental remediation communities experience a different extraction mechanism entirely (environmental externalities) overlaid on the supply dependency constraint. The piton perspective on legacy demand systems reveals how institutional inertia maintains performative crisis management even as structural fundamentals shift.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from the agent's structural position relative to the supply constraint. China's processing establishment (institutional/arbitrage) has d ≈ 0.10 (net beneficiary) — they control the choke point and can arbitrage supply allocation. Dependent manufacturers (powerless/trapped) have d ≈ 0.95 (full target) — they cannot exit and face maximum extraction. Alternative supply developers (moderate/constrained) have d ≈ 0.65 (partial victim) — they face stranded asset risk and high capital barriers but have some agency through technology development. Technology coalition (organized/mobile) has d ≈ 0.40 (partial victim with agency) — constrained by technology maturation timelines but have exit paths through substitution. Environmental communities (individual/constrained) have d ≈ 0.80 (strong victim) — trapped by economic dependency and suppressed voice in regulatory processes. These directionality values feed into the sigmoid f(d) function to produce experienced extractiveness χ for each perspective, differentiating snare (d ≈ 0.95) from tangled rope (d ≈ 0.65) from rope (d ≈ 0.10).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (supply management, demand optimization, processing efficiency) from extractive lock-in (geographic monopoly, pricing power, supply sequencing). The claimed tangled_rope type is justified: beneficiaries (China's processors) derive genuine coordination function (cannot operate without global demand signals, price feedback, and logistical optimization). Victims (dependent manufacturers, alternative developers) experience both coordination benefits (access to processed rare earths) and asymmetric extraction (pricing vulnerability, supply sequencing, forced substitution investment). The constraint does not collapse into pure extraction (snare) because the coordination function is real and necessary; nor does it collapse into pure coordination (rope) because extraction is asymmetric and benefits privileged actors. The analytical observer perspective risks false summit (mountain as 'thermodynamic law') because the constraint's severity is political/economic (geographic concentration, processing monopoly) not thermodynamic. Substitution and recycling technologies are developing, which indicates the constraint is not immutable — alternative extraction mechanisms exist and are being deployed. The piton perspective on legacy demand systems shows how institutional theater maintains supply anxiety even as structural drivers (demand growth, substitution, recycling) are evolving toward lower dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_technology_pace,
    'Will technology substitution (rare-earth-free motors, alternative permanent magnets, ceramic capacitors) develop fast enough to reduce dependency before geopolitical coercion causes permanent supply disruptions?',
    'Timeline comparison: maturation rates of substitution technologies vs deployment windows for dependency reduction; correlation between R&D investment and substitution progress',
    'If substitution accelerates: scaffold classification confirmed, snare transitions to rope. If substitution stalls: snare deepens, extraction can persist 30+ years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technology_pace, empirical, 'Pace of rare-earth-free technology substitution').

omega_variable(
    circular_economy_closure_feasibility,
    'Can recycling infrastructure scale to recover 40-60% of rare earth demand from e-waste and scrap without China''s processing monopoly, or is China''s infrastructure advantage permanent?',
    'Cost-competitive analysis of distributed recycling vs centralized processing; recovery rates and purity thresholds for technology deployment',
    'If recycling closes the loop: snare classification shifts toward rope/scaffold. If China retains processing advantage: extraction mechanism persists through secondary supply channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circular_economy_closure_feasibility, empirical, 'Whether recycling can break China''s processing monopoly').

omega_variable(
    geopolitical_coercion_trigger,
    'What level of rare earth supply disruption (% reduction, duration) triggers permanent diversification of supply chains vs temporary demand adjustment? Is there a threshold past which manufacturers accept substitution costs?',
    'Historical analysis of previous supply shocks (2010-2011, 2020-2021); manufacturer behavior during disruptions; capital allocation to substitution R&D as function of disruption magnitude',
    'If threshold < 20% supply reduction: snare extraction relatively contained. If threshold > 40%: extraction can be severe before triggering alternative-seeking behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_coercion_trigger, empirical, 'Geopolitical coercion trigger threshold for supply chain diversification').

omega_variable(
    environmental_extraction_overlap,
    'Does suppression of environmental impacts (water pollution, radiation exposure, remediation costs) operate as a separate extraction mechanism from supply control, or is it derivative of supply monopoly leverage?',
    'Jurisdictional comparison: environmental enforcement vigor in rare earth regions with vs without monopoly leverage; cost accounting for environmental externalities in supply pricing',
    'If separate: environmental remediation communities experience dual extraction (supply monopoly + environmental suppression). If derivative: reducing supply monopoly would also reduce environmental extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_extraction_overlap, empirical, 'Whether environmental suppression is independent extraction mechanism').

omega_variable(
    strategic_reserve_efficacy,
    'Do government strategic reserves (US, Japan, EU) reduce actual vulnerability to supply disruption, or do they create theater of preparedness while leaving structural dependency intact?',
    'Stress-test analysis: reserve coverage duration under various disruption scenarios; correlation between reserve levels and actual manufacturing resilience during past supply shocks',
    'If reserves effective: piton theater is actually functional scaffold. If ineffective: piton classification confirmed, high theater_ratio justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_reserve_efficacy, empirical, 'Efficacy of government strategic reserves for supply disruption resilience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_supply_chain, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rees_tr_t0, rare_earth_supply_chain, theater_ratio, 0, 0.32).
narrative_ontology:measurement(rees_tr_t5, rare_earth_supply_chain, theater_ratio, 5, 0.4).
narrative_ontology:measurement(rees_tr_t10, rare_earth_supply_chain, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rees_tr_t15, rare_earth_supply_chain, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(rees_be_t0, rare_earth_supply_chain, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rees_be_t5, rare_earth_supply_chain, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rees_be_t10, rare_earth_supply_chain, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rees_be_t15, rare_earth_supply_chain, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_supply_chain, resource_allocation).
narrative_ontology:boltzmann_floor_override(rare_earth_supply_chain, 0.18).
narrative_ontology:affects_constraint(rare_earth_supply_chain, renewable_energy_transition_cost).
narrative_ontology:affects_constraint(rare_earth_supply_chain, electric_vehicle_supply_chain).
narrative_ontology:affects_constraint(rare_earth_supply_chain, defense_industrial_base_dependency).
narrative_ontology:affects_constraint(rare_earth_supply_chain, semiconductor_supply_resilience).

% DUAL FORMULATION NOTE:
% Rare earth supply chain constraint decomposes into two structurally distinct claims: (1) geographic processing monopoly (ε ≈ 0.58, tangled rope) — China controls >70% of processing and benefits from pricing power. (2) Technology substitution pathway (ε ≈ 0.25, scaffold) — rare-earth-free motors, permanent magnet alternatives, recycling infrastructure are reducing long-term dependency. The two stories are linked because substitution technologies address the fundamental constraint, but at different timeframes and cost structures. The processing monopoly story is immediate/biographical (extraction happening now); the substitution story is generational/civilizational (exit pathway developing over 15-25 years). Both link to downstream supply chain constraints (renewable energy, EV supply chains) because those sectors depend on rare earth availability and pricing stability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_supply_chain, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
