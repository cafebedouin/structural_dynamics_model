% ============================================================================
% CONSTRAINT STORY: rare_earth_export_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_export_restrictions, []).

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
 *   constraint_id: rare_earth_export_restrictions
 *   human_readable: Rare Earth Export Restrictions
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   China's dominance of rare earth elements supply (approximately 85% of
 *   global processing capacity) combined with its strategic use of export
 *   restrictions creates a constraint on global manufacturing and
 *   technological development. Rare earth elements are essential inputs for
 *   permanent magnets (electric vehicles, wind turbines), catalytic
 *   converters, phosphors (displays, lighting), and defense electronics. The
 *   constraint operates through export licensing, production quotas, and
 *   strategic supply manipulation. Extractiveness has increased over the
 *   measurement interval (2010-2024) as Chinese control has consolidated and
 *   geopolitical tensions have made the supply bottleneck more weaponizable.
 *   The low theater ratio (0.38) indicates that the extraction is primarily
 *   structural and economic rather than performative — Chinese state control
 *   operates through direct supply chain domination, not through
 *   institutional ritual. This distinguishes it from degraded piton
 *   constraints that maintain extraction through theater.
 *
 * KEY AGENTS:
 *   - Chinese State Apparatus: Primary beneficiary (institutional/arbitrage) — captures geopolitical leverage, industrial rent, and supply control; can shift away from rare earth dependency over time
 *   - Integrated Chinese Manufacturers: Secondary beneficiary (institutional/arbitrage) — receive preferential supply access, technological integration, and protection from global competition
 *   - Downstream Manufacturers (Electronics, Defense, Energy): Primary victim (powerless/trapped) — dependent on Chinese supply; face extraction through supply volatility, price increases, and potential cutoffs; exit costs prohibitive
 *   - Alternative Rare Earth Producers: Secondary victim (moderate/constrained) — face predatory pricing and strategic supply flooding; constrained exit (15+ year development timelines for new production)
 *   - Global Supply Chain Reliability: Tertiary victim (powerless/trapped) — abstract good that cannot organize or exit; bears structural vulnerability to Chinese supply decisions
 *   - Alternative Supply Coalition: Organized actors (organized/constrained) — US rare earth initiatives, Australian producers, recycling technology developers building parallel supply chains with sunset logic
 *   - WTO Trade Regime: Institutional observer (institutional/arbitrage) — formally prohibits export restrictions but enforcement mechanisms fail; piton classification reflects rule degradation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees rare earth dependency as permanent structural vulnerability constraining global technology development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_export_restrictions, 0.58).
domain_priors:suppression_score(rare_earth_export_restrictions, 0.72).
domain_priors:theater_ratio(rare_earth_export_restrictions, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_export_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_export_restrictions, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rare_earth_export_restrictions, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_export_restrictions, snare).
narrative_ontology:human_readable(rare_earth_export_restrictions, "Rare Earth Export Restrictions").
narrative_ontology:topic_domain(rare_earth_export_restrictions, "economic/geopolitical").

domain_priors:requires_active_enforcement(rare_earth_export_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_export_restrictions, chinese_state_apparatus).
narrative_ontology:constraint_beneficiary(rare_earth_export_restrictions, integrated_chinese_manufacturers).
narrative_ontology:constraint_victim(rare_earth_export_restrictions, downstream_manufacturers).
narrative_ontology:constraint_victim(rare_earth_export_restrictions, global_supply_chain_reliability).
narrative_ontology:constraint_victim(rare_earth_export_restrictions, technology_diversification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT MANUFACTURER (SNARE) — Global manufacturers of electronics, defense systems, and renewable energy infrastructure are trapped in dependency. Exit costs (building alternative supply chains, developing substitutes, relocating production) are prohibitive. No leverage to negotiate. Faces extraction through supply disruption threats, price volatility, and forced technology transfer conditions. Maximum structural extraction from powerless, trapped agent.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING RARE EARTH PRODUCER (SNARE) — Non-Chinese producers (Myanmar, Vietnam, USA, Australia operations) face extraction through predatory pricing, supply flooding during license periods, and strategic underinvestment in alternative production. Exit options are constrained by capital intensity and long development timelines. Even organized producers cannot exit without 10-15 year R&D and infrastructure investment. Experiences structural extraction through market manipulation.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE INDUSTRIAL POLICY APPARATUS (TANGLED ROPE) — The state apparatus uses rare earth control for genuine coordination of industrial development (coordination benefit: vertical integration, downstream manufacturing clusters, technological advancement) AND for extraction (monopoly pricing, supply control, geopolitical leverage). The constraint serves both functions. Mobile exit option because China can shift away from rare earth dependency over time; powerful position enables flexible use of the restriction. Not pure extraction because coordination function is genuine.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INTEGRATED CHINESE MANUFACTURER (ROPE) — Domestic manufacturers integrated into the restricted supply chain experience the constraint as coordination with benefits. Preferential access to controlled supply, lower effective prices through subsidies and coordination mechanisms, technological knowledge transfer, and long-term security. Institutional power with arbitrage exit (can shift to other materials or efficiency gains). Net beneficiary experiencing the constraint as functional coordination.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE SUPPLY COALITION (SCAFFOLD) — Organized actors (rare earth producers in USA, Australia, Myanmar; recycling initiatives; material substitution R&D funded by allied governments; supply chain diversification mandates) see rare earth restriction as a temporary problem with a sunset. Alternative supply chains, improved recycling technology, and synthetic substitutes are maturing. Exit mechanism is constrained but time-bounded — estimated 15-20 years for alternative supply to reduce Chinese dominance from ~85% to ~50% of global capacity. Theater is low because alternative supply is tangible infrastructure, not performative ritual.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WTO TRADE REGIME (PITON) — The multilateral trading system's formal prohibition on export restrictions (GATT Article XI) is substantially degraded. China's restriction violates the rule but persists because enforcement mechanisms are weak and political will to sanction China is limited. The WTO dispute settlement process (2010-2015, Chinese rare earth case) produced a formal ruling but no effective remedy — China complied superficially while maintaining de facto control through refined export licensing. The trade rule is inert institutional theater; real coordination happens through bilateral state arrangements and supply contracts, not through the multilateral regime. Degradation is high (piton theater ≥ 0.70) but not total.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, rare earth dependency is a snare on global technological capability. The supply bottleneck constrains energy transition (electric vehicles, wind turbines, solar grid modernization), military modernization, and technological autonomy for all states except China. The constraint is structurally extractive: the target (global manufacturing) bears transition costs, supply uncertainty, and technological vulnerability; the beneficiary (Chinese state) captures geopolitical leverage and industrial rent. Suppression is high (export licensing bureaucracy, strategic opacity, supply volatility). The constraint has no coordinating function from this civilizational view — it is pure extraction.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_export_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_export_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_export_restrictions, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_export_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_export_restrictions, TR),
    TR >= 0.70.

:- end_tests(rare_earth_export_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple mechanisms: (1) price premium from supply scarcity (estimated 15-40% above competitive baseline), (2) forced technology transfer conditions embedded in supply contracts, (3) strategic undersupply to competitors during geopolitical disputes, (4) supply uncertainty costs (manufacturers must hold 6-12 month strategic reserves). The extraction is not total (0.90+) because alternative supply pathways exist and diversification is technically feasible over long timelines. Suppression (0.72): High. Significant barriers to exiting the constraint include: high capital intensity of new rare earth production (billions of dollars), long development timelines (10-15 years from exploration to production), environmental regulation complexity in Western countries (enabling China's cost advantage through externality dumping), and tacit knowledge concentration in Chinese supply chains. Victims cannot easily organize (geographic dispersion, competing interests) or relocate production (sunk capital in supply relationships). Theater ratio (0.38): Low. The constraint operates through structural supply dominance rather than performative ritual. Chinese export control is direct and material — actual supply quotas, licensing bureaucracy that affects physical flows, not institutional theater. This low theater distinguishes it from piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the beneficiary's Rope/Tangled Rope classification and the victim's Snare classification. From the Chinese state's view, rare earth control is a legitimate industrial policy tool that coordinates domestic technological development (coordination benefit is real) while providing geopolitical leverage (extraction benefit). From the dependent manufacturer's view, the same arrangement is pure extraction — they have no coordination benefit, only supply risk and cost increases. The scaffold perspective (alternative supply coalition) bridges this gap: it acknowledges real extraction but argues for a sunset. The piton perspective reveals institutional failure: the WTO rule against export restrictions is formally binding but effectively inert, degraded by weak enforcement. The analytical observer sees the constraint as having no sunset at civilizational scale — rare earth dependency will constrain global technology development indefinitely unless substitutes or alternative supply mature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Chinese state apparatus, integrated manufacturers) are assigned low d values (0.05-0.25 range) based on their arbitrage exit options and beneficiary status — they experience negative effective extraction from the constraint, or coordination benefits. Victims (dependent manufacturers) are assigned high d values (0.85-0.95 range) based on trapped exit options and victim status — they experience high effective extraction. Alternative producers (moderate power, constrained exits) receive moderate d values (0.60-0.75 range). The state apparatus receives a mid-range override despite institutional power because its constraint-relative directionality reflects partial capture: it benefits from extraction but also invests in the coordination function (technological development). No overrides are necessary here — structural derivation produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint initially appears to face a classification ambiguity — is it a Snare (pure extraction) or a Tangled Rope (mixed coordination and extraction)? The resolution depends on which agent we prioritize. From the dependent manufacturer's perspective (the most powerless and most victimized agent), it is clearly a Snare: no coordination benefit, only extraction. From the Chinese state apparatus's perspective, it is clearly Tangled Rope: both coordination (vertical integration, technological advancement) and extraction (geopolitical leverage, pricing power) are genuine structural functions. The mandatrophy is resolved by recognizing that both classifications are correct from their respective perspectives, and the presheaf of all perspectives reveals the true structure: the constraint serves coordination for beneficiaries and pure extraction for victims. The analytical perspective at civilizational scale sees a Snare because it asks: 'What does this constraint do for humanity?', and the answer is: it extracts resources from global manufacturing to concentrate power in one state. No coordination benefit accrues to the global system. This resolves the ambiguity in favor of Snare at civilizational scale, with the caveat that local perspectives (beneficiary, state apparatus) experience genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_supply_timeline,
    'What is the realistic timeline for non-Chinese rare earth production to reduce Chinese market share from 85% to below 60%?',
    'Tracking progress in US Rare Element Extraction and Purification (REX) initiatives, Australia''s rare earth projects, Myanmar geology and political stability, recycling technology maturity curves, and substitution R&D funding',
    'If timeline < 10 years: scaffold perspective is validated (sunset is near). If timeline > 25 years: scaffold is aspirational, and the constraint persists as snare for entire generation. If timeline cannot be met: rare earth dependency becomes a permanent structural vulnerability for non-Chinese actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_timeline, empirical, 'Timeline for alternative rare earth supply maturation').

omega_variable(
    technology_substitution_feasibility,
    'Are permanent material substitutes (non-rare-earth permanent magnets, alternative phosphors, synthetic alternatives) technically feasible at scale for the majority of current rare earth applications, or is the rare earth dependency structural?',
    'Comparison of physical properties (energy density, thermal stability, cost) of substitutes vs rare earth elements for key applications. Analysis of whether substitutes enable functional equivalence or only partial replacement.',
    'If substitutes feasible for 70%+ of applications: Chinese control of rare earths is tactically severe but strategically temporary. If substitutes exist only for <30%: rare earth dependency is permanent structural vulnerability, and the snare classification persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_substitution_feasibility, empirical, 'Technical feasibility of material substitutes for rare earths').

omega_variable(
    geopolitical_escalation_mechanism,
    'Under what conditions does China use rare earth export restrictions as explicit coercion (supply cutoff) versus implicit rent-extraction (pricing control), and how does the distinction affect the classification between extraction and political leverage?',
    'Historical analysis of Chinese rare earth restrictions 2010-2015 (explicit quota controls), 2020-2024 (implicit pricing and licensing), and 2024+ (potential future escalation). Correlation with US-China geopolitical tensions and specific technology disputes.',
    'If primarily rent-extraction: snare classification is accurate but emphasizes economic rather than military extraction. If escalation to explicit cutoffs becomes baseline: the constraint becomes a coercive political tool with snare classification reinforced. If China negotiates binding supply agreements with allies: constraint degrades to tangled_rope through agreed-upon rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_escalation_mechanism, empirical, 'Mechanism of rare earth control: rent-extraction vs explicit coercion').

omega_variable(
    recycling_economic_viability,
    'At what rare earth price point does recycling from end-of-life electronics become economically viable without subsidy, and when will that price point be reached?',
    'Analysis of recycling economics curves; tracking development of recycling technology cost reductions; monitoring rare earth spot prices and long-term forecasts',
    'If recycling viability reached within 5 years: alternative supply emerges faster, scaffold sunset is real, extraction window narrows. If viability requires sustained price premium (>$200/kg rare earths): recycling remains subsidy-dependent, scaffold sunset is delayed, snare classification persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_economic_viability, empirical, 'Economic viability threshold for rare earth recycling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_export_restrictions, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ree_tr_t0, rare_earth_export_restrictions, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ree_tr_t7, rare_earth_export_restrictions, theater_ratio, 7, 0.32).
narrative_ontology:measurement(ree_tr_t14, rare_earth_export_restrictions, theater_ratio, 14, 0.38).

% Extraction over time
narrative_ontology:measurement(ree_be_t0, rare_earth_export_restrictions, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ree_be_t7, rare_earth_export_restrictions, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(ree_be_t14, rare_earth_export_restrictions, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_export_restrictions, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, semiconductor_supply_chain_vulnerability).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, electric_vehicle_battery_supply).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, defense_technology_autonomy).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, wind_energy_scaling).

% DUAL FORMULATION NOTE:
% Rare earth export restrictions is the upstream constraint that affects multiple downstream supply chain vulnerabilities. Each downstream constraint (semiconductor, EV battery, defense tech, wind energy) has its own extractiveness value reflecting the specific material dependencies and market conditions, but all are causally dependent on the rare earth bottleneck. This is a constraint family where the upstream rare earth control creates structural vulnerability that manifests as extraction at multiple points in the global supply chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_export_restrictions, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
