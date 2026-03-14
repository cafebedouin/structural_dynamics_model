% ============================================================================
% CONSTRAINT STORY: renewable_cost_parity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_renewable_cost_parity, []).

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
 *   constraint_id: renewable_cost_parity
 *   human_readable: Renewable Energy Cost Parity Constraint
 *   domain: energy/economics/technology
 *
 * SUMMARY:
 *   The renewable energy cost parity constraint operates at the intersection
 *   of technology learning curves, economic incentives, and climate urgency.
 *   It manifests as a requirement that renewable energy reach price parity
 *   with incumbent fossil fuels before accelerated deployment can be
 *   justified politically and economically. This framing creates a structural
 *   tension: communities facing climate impacts experience the parity
 *   requirement as an extractive delay mechanism, while equipment
 *   manufacturers and utility operators experience it as a coordination
 *   framework that enables long-term planning and margin protection. The
 *   constraint exhibits hybrid characteristics: it contains genuine
 *   coordination functions (stable investment horizons, technical integration
 *   standards) alongside asymmetric extraction (delayed transition costs
 *   borne by vulnerable populations, premium margins sustained during
 *   transition). The theater ratio has increased from 0.42 to 0.55 over the
 *   interval as 'parity' claims have become increasingly contested and
 *   scope-dependent, with different accounting methodologies producing widely
 *   varying timelines. The constraint's extractiveness has grown from 0.22 to
 *   0.38 as climate impacts have accelerated, making delay costs more
 *   visible, while technology learning curves have continued to narrow the
 *   actual cost gap—creating a widening gap between the authentic cost
 *   convergence and the parity narrative used to justify continued fossil
 *   infrastructure investment.
 *
 * KEY AGENTS:
 *   - Incumbent Fossil Fuel Producers: Primary beneficiary (institutional/arbitrage) — parity requirement delays market displacement and sustains stranded asset value during transition window
 *   - Renewable Equipment Manufacturers: Secondary beneficiary (institutional/arbitrage) — parity framing enables premium pricing and sustainable margins as production scales
 *   - Grid Infrastructure Operators: Mixed (moderate/constrained) — benefit from predictable transition timelines; bear asymmetric costs of grid management during technology transition
 *   - Climate-Vulnerable Communities: Primary victim (powerless/trapped) — experience delay costs through climate damage, health impacts, and stranded local infrastructure; no exit from parity timeline requirements
 *   - Climate Policy Coalition: Organized advocates (organized/constrained) — view parity as temporary scaffold; perceive sunset mechanism through market forces and policy learning
 *   - Utility Regulatory Agencies: Institutional theater (institutional/arbitrage) — maintain depreciation and cost-recovery rules designed for fossil fuel era; persist through inertia despite functional atrophy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(renewable_cost_parity, 0.38).
domain_priors:suppression_score(renewable_cost_parity, 0.42).
domain_priors:theater_ratio(renewable_cost_parity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(renewable_cost_parity, extractiveness, 0.38).
narrative_ontology:constraint_metric(renewable_cost_parity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(renewable_cost_parity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(renewable_cost_parity, tangled_rope).
narrative_ontology:human_readable(renewable_cost_parity, "Renewable Energy Cost Parity Constraint").
narrative_ontology:topic_domain(renewable_cost_parity, "energy/economics/technology").

domain_priors:requires_active_enforcement(renewable_cost_parity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(renewable_cost_parity, incumbent_fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(renewable_cost_parity, grid_infrastructure_operators).
narrative_ontology:constraint_beneficiary(renewable_cost_parity, energy_equipment_manufacturers).
narrative_ontology:constraint_victim(renewable_cost_parity, renewable_deployment_communities).
narrative_ontology:constraint_victim(renewable_cost_parity, climate_mitigation_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE COMMUNITIES (SNARE) — Communities dependent on rapid decarbonization face maximum extraction. They bear the costs of delayed transition (climate damage, health impacts, stranded infrastructure) while parity requirements lock them into slow adoption timelines. No exit: they cannot implement transition faster than cost curves permit. The constraint extracts their future welfare while they remain powerless to accelerate adoption.
constraint_indexing:constraint_classification(renewable_cost_parity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GRID OPERATORS (TANGLED ROPE) — Grid operators benefit from stable long-term planning enabled by parity requirements (coordination function: predictable investment horizons, manageable technical integration). But they also bear asymmetric costs: maintaining backup capacity for intermittency, managing network topology changes, absorbing stranded fossil asset risk. High suppression through regulatory requirement to maintain grid reliability regardless of technology transition speed. Both genuine coordination and extractive asymmetry present.
constraint_indexing:constraint_classification(renewable_cost_parity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RENEWABLE EQUIPMENT MANUFACTURERS (ROPE) — Primary beneficiaries. The parity requirement creates a coordination function (standardized procurement, predictable demand curves) while enabling extraction through sustained premium margins during the transition. Cost curves favor manufacturers: they can delay aggressive cost reduction by framing 'parity' as already achieved (LCOE accounting tricks, scope narrowing). Arbitrage: manufacturers can shift operations and supply chains globally; exit is low-cost.
constraint_indexing:constraint_classification(renewable_cost_parity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE POLICY COALITION (SCAFFOLD) — Organized agents (climate scientists, environmental NGOs, renewable energy startups) view parity as a transitional scaffolding: it enables legal legitimacy for renewable deployment while market mechanisms and policy learning gradually reduce fossil lock-in. Low effective extraction because the coalition has agency and sees a sunset: as genuine parity emerges, the artificial parity requirements become redundant and dissolve. Sunset mechanism: technology cost curves and climate pressure eventually make fossil fuels uncompetitive regardless of parity framing.
constraint_indexing:constraint_classification(renewable_cost_parity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UTILITY REGULATORY FRAMEWORK (PITON) — Traditional utility regulation assumes long-term asset depreciation and stable fuel cost curves. The parity constraint persists as regulatory theater: utilities maintain the language of 'full cost accounting' and 'market integration' while operating under depreciation rules designed for fossil fuels. The framework's original function (managing natural monopolies, ensuring cost recovery) has atrophied as grid technology and market structure change. Maintained through institutional inertia, not because it functions as designed. Theater ratio high: 'integrated resource planning' and 'least-cost procurement' processes often reach predetermined conclusions.
constraint_indexing:constraint_classification(renewable_cost_parity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a purely physical perspective, cost parity between renewable and fossil energy reflects fundamental thermodynamic and geological limits: fossil fuels are highly energy-dense but depletable; renewables are dilute but abundant. The 'parity problem' appears as an invariant feature of energy physics — you cannot escape thermodynamic costs of resource conversion regardless of technology choice. However, the structural data contradicts this mountain framing: the measured extractiveness and suppression reflect institutional choices (accounting standards, subsidy structures, grid topology, policy timelines), not physical laws. The engine's false summit detector will flag this as naturalization of political economy as thermodynamics.
constraint_indexing:constraint_classification(renewable_cost_parity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(renewable_cost_parity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(renewable_cost_parity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(renewable_cost_parity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(renewable_cost_parity, TR),
    TR >= 0.70.

:- end_tests(renewable_cost_parity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The parity constraint extracts from climate-vulnerable populations through delayed transition, imposing health and climate damage costs. However, the extraction is not total—renewable deployment is accelerating in many jurisdictions despite parity framing, and genuine technology learning curves are converging costs independently. The measurement reflects that parity functions as a political brake, not an absolute barrier. Suppression (0.42): Moderate. Barriers to faster transition include financing structures, grid integration requirements, supply-chain capacity, and regulatory timelines. But suppression is not insurmountable—some countries have deployed >50% renewable grids, and no fundamental physical law prevents acceleration. Theater ratio (0.55): Moderate-high and rising. The parity narrative has become increasingly performative as actual cost convergence proceeds faster than parity claims acknowledge. LCOE calculations claim parity or near-parity in many regions; yet deployment continues to face 'parity not yet sufficient' barriers in policy and financing. The gap between calculated parity and required parity for deployment reveals the constraint's performative function—it justifies maintenance of fossil-compatible infrastructure and utility rate structures designed for the pre-parity era.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival separation. The beneficiary (fossil/renewable producers) sees rope—their genuine coordination function is enabled by stable parity framing. The victim (climate-vulnerable communities) sees snare—they cannot exit the temporal requirement and bear all delay costs. The grid operator sees tangled rope—they coordinate integration while absorbing transition costs. The policy coalition sees scaffold—they perceive an exit mechanism through technology learning curves and political pressure. The utility regulator sees piton—the regulatory framework persists despite functional obsolescence. The analytical observer risks seeing mountain—but structural analysis reveals institutional contingency, not thermodynamic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows the extraction flow: fossil producers and renewable manufacturers derive arbitrage benefits (low d) from sustained price premiums during the transition window. Grid operators face constrained exit (medium d)—they can invest in renewable integration but at mandated timelines and cost structures. Vulnerable communities face trapped exit (high d)—they bear climate impacts regardless of transition speed and cannot exit the constraint's temporal requirements. The analytical observer at civilizational timescale risks d=0.5 (false symmetry), seeing 'inevitable thermodynamic tradeoff' instead of 'contingent political choice.' The climate policy coalition's d derives from their power to shape sunset mechanisms—constrained exit but with strategic agency reduces their experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by separating genuine coordination functions from extractive delay mechanisms. The coordination function is real: standardized cost accounting, investment horizons, technical integration planning enable large-scale renewable deployment. The extraction is also real: the delay imposed by parity framing costs vulnerable populations more than the coordination benefit accrues to them. The tangled rope classification is stable across structural variations—the two functions coexist in the same constraint with asymmetric distribution of costs and benefits. The false summit mountain classification (thermodynamic inevitability) is detectible through structural analysis: the constraints are political and institutional, not physical. Overcoming the mandatrophy requires acknowledging that parity is simultaneously a real economic phenomenon (cost curves are converging) and a performative narrative (parity framing is sustained beyond its original analytical purpose, masking continued extraction during transition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parity_accounting_definition,
    'What expenditure categories should be included in ''cost parity'' calculations? Grid balancing? Transmission infrastructure? Decommissioning? System integration costs?',
    'Standardized lifecycle cost accounting framework; comparison of parity claims across multiple accounting methodologies',
    'If narrow LCOE (levelized cost of electricity) used: renewables appear at parity sooner, reducing measured extractiveness and suppression. If full system costs included: parity pushed further into future, increasing extraction. Current variation: 0.15–0.35 ε swing depending on scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parity_accounting_definition, conceptual, 'Variation in ''cost parity'' definition across accounting frameworks').

omega_variable(
    grid_integration_feasibility_threshold,
    'At what renewable penetration level (grid inertia, frequency stability, storage requirements) does the technical constraint (not cost) become binding?',
    'Grid simulation and empirical data from high-renewable grids (Denmark, Uruguay, South Australia); identification of technical barriers independent of cost',
    'If threshold low (20–30% penetration): cost parity is sufficient for rapid transition; constraint is purely economic (Rope or Tangled Rope). If threshold high (60–80%): grid integration costs and technical requirements exceed cost parity savings; constraint becomes structural (more Snare-like for communities dependent on rapid transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_integration_feasibility_threshold, empirical, 'Technical threshold for grid integration feasibility').

omega_variable(
    fossil_subsidy_counterfactual,
    'If fossil fuel subsidies (direct and externalized: healthcare costs, climate damage, military expenditure on resource security) were fully internalized, what would actual cost parity be and when?',
    'Meta-analysis of subsidy quantification studies; sensitivity analysis on externality valuations; comparison of actual deployment rates with full-cost LCOE',
    'If true parity already achieved at current externality valuations: parity is performative theater masking political choice, not economic constraint. Extractiveness would increase (Snare classification more appropriate). If true parity still 5–10 years away: parity remains real constraint but measurement reveals political cost-shifting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_subsidy_counterfactual, empirical, 'Impact of fossil fuel subsidy internalization on parity timeline').

omega_variable(
    technology_learning_rate_variance,
    'Are renewable cost curves following stable exponential learning patterns, or are they subject to supply-chain shocks, manufacturing bottlenecks, and geopolitical disruptions that could flatten trajectories?',
    'Historical cost trajectory data with shock identification; forecasting model validation; supply-chain resilience analysis',
    'If stable learning curves: parity timeline is predictable, extractiveness remains moderate. If volatile with flattening risk: trajectory is hostage to geopolitical contingencies, extractiveness and suppression increase unpredictably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_learning_rate_variance, empirical, 'Stability and predictability of renewable cost learning curves').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(renewable_cost_parity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(renew_tr_t0, renewable_cost_parity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(renew_tr_t5, renewable_cost_parity, theater_ratio, 5, 0.48).
narrative_ontology:measurement(renew_tr_t10, renewable_cost_parity, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(renew_be_t0, renewable_cost_parity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(renew_be_t5, renewable_cost_parity, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(renew_be_t10, renewable_cost_parity, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(renewable_cost_parity, resource_allocation).
narrative_ontology:affects_constraint(renewable_cost_parity, grid_stability_infrastructure).
narrative_ontology:affects_constraint(renewable_cost_parity, fossil_stranded_asset_write_down).
narrative_ontology:affects_constraint(renewable_cost_parity, renewable_supply_chain_bottleneck).

% DUAL FORMULATION NOTE:
% Cost parity decomposes into two structurally distinct constraints: (1) direct cost convergence (technology learning, manufacturing scale), which has ε ~0.08 (mountain-like empirically verifiable);  (2) parity-as-deployment-timeline (political/regulatory requirement), which has ε ~0.38 (tangled rope). The JSON story covers the second constraint (the one that matters for transition speed). The first constraint (pure technology learning curve) is downstream and should be modeled separately if fine-grained analysis is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(renewable_cost_parity, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
