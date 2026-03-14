% ============================================================================
% CONSTRAINT STORY: uk_net_zero_transition_pathway
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_net_zero_transition_pathway, []).

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
 *   constraint_id: uk_net_zero_transition_pathway
 *   human_readable: UK Net Zero Transition Pathway
 *   domain: energy_policy/environmental_governance
 *
 * SUMMARY:
 *   The UK net-zero transition pathway represents a hybrid constraint
 *   combining genuine coordination (the need to decarbonize energy systems,
 *   reduce emissions, and manage infrastructure transition) with significant
 *   asymmetric extraction (concentrated costs on low-income households,
 *   fossil fuel workers, and regional economies, while benefits accrue to
 *   renewable energy investors, technology manufacturers, and green finance
 *   sectors). The constraint exhibits all ten DR types from different
 *   structural positions, making it a diagnostic exemplar for how energy
 *   policy enacts asymmetric burden-sharing within climate action. The same
 *   structural phenomenon — the legal and financial commitment to net-zero by
 *   2050 (with intermediate targets: 78% by 2035, 90% electricity by 2035) —
 *   appears as pure extraction (snare) to fossil fuel workers and low-income
 *   households, as mixed coordination and extraction (tangled rope) to
 *   regional economies and labor unions, as pure coordination (rope) to
 *   renewable energy investors, as temporary coordination (scaffold) to
 *   climate justice advocates, as degraded theater (piton) to the regulatory
 *   framework itself, or as natural law (mountain) to the analytical
 *   observer. The theater ratio has risen from 0.52 to 0.68 over the first
 *   six years of the measurement interval, reflecting increasing gap between
 *   regulatory commitments (detailed carbon budgets, sectoral targets) and
 *   actual decarbonization mechanisms (which depend on market forces, capital
 *   availability, and consumer behavior). Extractiveness has risen from 0.42
 *   to 0.58, indicating that costs are concentrating faster than promised
 *   redistributive mechanisms materialize.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Workers: Primary victim (powerless/trapped, regional scope) — face unemployment, geographic immobility, and pension vulnerability as coal and gas plants close without credible reskilling
 *   - Low-Income Households: Primary victim (powerless/trapped, national scope) — bear energy bill increases, cannot afford heat pump retrofits or EV conversion, experience regressive cost burden
 *   - Regional Economies: Secondary victim (moderate/constrained, regional scope) — coal-dependent regions lose tax base and employment concentration; transition costs exceed available capital for diversification
 *   - Renewable Energy Investors: Primary beneficiary (institutional/arbitrage, global scope) — benefit from guaranteed demand, subsidy mechanisms, contracts for difference, and profitable asset classes
 *   - Technology Manufacturers: Primary beneficiary (institutional/arbitrage, global scope) — global supply chains benefit from UK mandatory adoption of heat pumps, EVs, batteries; can shift production to other net-zero markets
 *   - Green Finance Sector: Secondary beneficiary (institutional/arbitrage, global scope) — green bonds, sustainability-linked financing, carbon credit markets generate new asset classes
 *   - Labor Unions / Community Coalitions: Organized agents (organized/constrained, national scope) — advocate for just transition but face constrained negotiating power; genuine coordination function undermined by externalization of costs
 *   - Local Authorities / Regional Governments: Institutional actors (institutional/constrained, regional scope) — coordinate local transition but constrained by reduced central funding and unfunded mandates
 *   - Climate Justice Coalition: Organized agents (organized/mobile, national scope) — frame transition as scaffold with sunset logic; monitor compliance and publicize shortfalls; exit path is visible but dependent on policy fidelity
 *   - Regulatory Framework (Climate Change Act): Institutional system (institutional/arbitrage, national scope) — maintains performative reporting while enforcement mechanisms are weak; original binding function has atrophied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_net_zero_transition_pathway, 0.58).
domain_priors:suppression_score(uk_net_zero_transition_pathway, 0.62).
domain_priors:theater_ratio(uk_net_zero_transition_pathway, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_net_zero_transition_pathway, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_net_zero_transition_pathway, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(uk_net_zero_transition_pathway, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_net_zero_transition_pathway, tangled_rope).
narrative_ontology:human_readable(uk_net_zero_transition_pathway, "UK Net Zero Transition Pathway").
narrative_ontology:topic_domain(uk_net_zero_transition_pathway, "energy_policy/environmental_governance").

domain_priors:requires_active_enforcement(uk_net_zero_transition_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_net_zero_transition_pathway, renewable_energy_investors).
narrative_ontology:constraint_beneficiary(uk_net_zero_transition_pathway, technology_manufacturers).
narrative_ontology:constraint_beneficiary(uk_net_zero_transition_pathway, green_finance_sector).
narrative_ontology:constraint_victim(uk_net_zero_transition_pathway, fossil_fuel_workers).
narrative_ontology:constraint_victim(uk_net_zero_transition_pathway, low_income_households).
narrative_ontology:constraint_victim(uk_net_zero_transition_pathway, regional_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL FUEL WORKER (SNARE) — Trapped by geographic location, skill specialization, and pension dependencies. The transition pathway mandates phase-out without credible alternative employment routes in coal-dependent regions. Cannot exit the constraint without material loss. Bears full extraction cost of decarbonization.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOW-INCOME HOUSEHOLD (SNARE) — Trapped by inability to afford energy retrofitting, heat pump installation, or EV conversion. Energy policy transitions costs to consumers while benefiting are concentrated in capital-intensive investments. High suppression from energy cost volatility and inability to exit housing market. Extraction is material and immediate.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ECONOMY (TANGLED ROPE) — Regions dependent on fossil fuel infrastructure face genuine coordination problems (how to transition energy systems) alongside asymmetric extraction (transition costs fall on regions, benefits accrue to national and international capital). Constrained by infrastructure lock-in and capital availability. Experiences both necessary coordination and disproportionate burden.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE ENERGY INVESTOR (ROPE) — Benefits from policy certainty, subsidies, contracts for difference, and mandatory renewable targets. Constraint is experienced as pure coordination: policy creates predictable demand for renewable capacity. Arbitrage available: capital can shift to other net-zero markets if UK conditions change. Net beneficiary.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGY MANUFACTURER (ROPE) — Globally integrated supply chains benefit from UK mandatory transition (heat pumps, EVs, batteries). Policy creates predictable demand. Experiences constraint as market coordination. Can arbitrage: shift production to other net-zero markets if UK becomes uncompetitive. Net beneficiary.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR UNION / COMMUNITY COALITION (TANGLED ROPE) — Organized agents nominally see the transition as necessary coordination (climate goals are genuine) but face constrained exit: political pressure limits negotiating power, and the transition is treated as inevitable rather than negotiable. Genuine need for just-transition coordination sits alongside extraction mechanism where labor costs are externalized and upfront investment is borne by workers. Extraction is enforceable because alternatives are framed as illegitimate.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LOCAL AUTHORITY / REGIONAL GOVERNMENT (TANGLED ROPE) — Coordinate local infrastructure transition (building heating, transport) but constrained by reduced central funding, unfunded mandates, and loss of revenue from energy company tax contributions. Genuine coordination function (managing transition) alongside extraction (bearing costs without corresponding fiscal transfer). Experiences active enforcement through inspection regimes and statutory requirements.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: CLIMATE JUSTICE COALITION (SCAFFOLD) — Organized groups advocating for just transition see the pathway as a temporary coordination mechanism with sunset logic: reskilling programs, regional reinvestment, and just-transition funds are framed as the transition phase's support structure. Exit path is visible (transition complete when regional economies are diversified and workers are retrained). Theater is moderate because the coalition actively monitors compliance and publicizes shortfalls. Suppression is not total because public accountability mechanisms exist.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 9: REGULATORY FRAMEWORK (PITON) — The Climate Change Act (2008) and subsequent Carbon Budget framework are largely performative: targets are set, but enforcement mechanisms are weak (missed budgets trigger reviews rather than sanctions), and loopholes allow offsetting and international credit purchases. Theater ratio high: regulatory apparatus maintains detailed reporting while actual trajectory is determined by market forces and capital investment patterns. Original function (binding decarbonization commitment) has atrophied; maintained through institutional inertia.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/physical perspective, some energy transition is inevitable (fossil fuels are finite; carbon constraints are thermodynamic reality). The observer risks seeing net-zero as a natural law rather than a contingent political-economic arrangement. However, the structural data contradicts mountain classification: the timing, pace, and distribution of transition costs are entirely contingent on policy choices, funding mechanisms, and worker protections. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_net_zero_transition_pathway_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_net_zero_transition_pathway, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_net_zero_transition_pathway, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_net_zero_transition_pathway, TR),
    TR >= 0.70.

:- end_tests(uk_net_zero_transition_pathway_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The transition pathway extracts from fossil workers (lost employment, pension vulnerability), low-income households (energy cost burden, unaffordable retrofits), and regional economies (concentrated disinvestment). The extraction is not maximal (0.72+) because: (1) climate necessity is genuine (not manufactured scarcity), (2) redistributive mechanisms exist on paper (just-transition funds, household retrofit grants, regional investment pledges), and (3) organized agents retain some negotiating power. However, extractiveness is rising (0.42→0.58) as the gap between promised support and actual implementation becomes visible. Suppression (0.62): High. Significant barriers prevent exit: fossil workers cannot relocate without material loss; low-income households cannot afford transition technologies; regional economies are locked into existing infrastructure; alternative energy policies are delegitimized as climate denial. Suppression is not total (≤1.0) because: (1) international capital can still exit (renewable investors and manufacturers can serve other markets), (2) public accountability mechanisms exist (regulatory framework is public, targets are legislated), (3) organized labor retains voice if not exit. Theater ratio (0.68): High. The regulatory apparatus is substantially performative: detailed carbon budgets are set, sectoral targets are specified, reporting requirements are comprehensive, but enforcement is weak (missed budgets trigger reviews, not sanctions), and loopholes allow offsetting and international credits. The gap between regulatory presentation (binding decarbonization) and actual mechanism (capital investment in renewable capacity, consumer behavior change) has widened as renewable deployment has depended increasingly on market confidence rather than regulatory mandate. The theater has increased from 0.52 to 0.68 as political consensus has solidified (reducing need for detailed justification) while actual decarbonization has required aggressive cost pass-through to consumers (reducing room for regulatory persuasion).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is acute. Renewable energy investors experience zero suppression and high arbitrage capacity — they see rope. Fossil fuel workers experience maximum suppression and zero exit capacity — they see snare. The gap reflects fundamental asymmetry in bargaining power: global capital can arbitrage; localized workers cannot. The gap is not merely disagreement; it is structural: the constraint simultaneously enables one group (investors/manufacturers) to exit and traps another (workers/low-income households). This asymmetry is the defining feature of tangled rope classification at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is computed from agent power, exit options, and beneficiary/victim status. Fossil fuel workers: powerless power atom + trapped exit + victim status → d ≈ 0.95, f(d) ≈ 1.42 → high experienced extraction (snare classification confirmed). Low-income households: powerless power atom + trapped exit + victim status → d ≈ 0.95, f(d) ≈ 1.42 → high experienced extraction (snare classification confirmed). Regional economies: moderate power atom + constrained exit + victim status → d ≈ 0.72, f(d) ≈ 1.15, but beneficiary coordination function exists (transition solves infrastructure problem) → tangled rope classification confirmed. Renewable investors: institutional power atom + arbitrage exit + beneficiary status → d ≈ 0.05, f(d) ≈ -0.12 → negative experienced extraction, pure coordination (rope classification confirmed). Local authorities: institutional power atom + constrained exit (reduced central funding, unfunded mandates) + both beneficiary (coordinate necessary transition) and victim (bear costs) status → d ≈ 0.50, f(d) ≈ 0.65 → moderate experienced extraction with coordination function (tangled rope classification confirmed). Labor unions: organized power atom + constrained exit (political pressure limits negotiating power) + both beneficiary (transition is genuinely necessary, union has stake in workers' future) and victim (short-term cost externalization) status → d ≈ 0.55, f(d) ≈ 0.75 → moderate-high experienced extraction with coordination function (tangled rope classification confirmed). Climate justice advocates: organized power atom + mobile exit (can shift advocacy focus, support alternative decarbonization pathways) + beneficiary status (their position is strengthened by transition commitment) → d ≈ 0.40, f(d) ≈ 0.40 → low-moderate experienced extraction, genuine coordination with exit path visible (scaffold classification confirmed). Regulatory framework: institutional power atom + arbitrage exit (can be replaced with alternative enforcement mechanisms) + beneficiary status (maintains institutional power) and victim status (must report failures) → d ≈ 0.50 but theater_ratio >> 0.5 → piton classification confirmed by theater gate, not by extraction level.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution is emergent from the perspectival structure. The constraint is legitimately tangled rope at the system level (genuine coordination of infrastructure transition + asymmetric extraction concentrated on powerless agents), but appears as snare from the victim perspectives (powerless/trapped) and as rope from beneficiary perspectives (institutional/arbitrage). The mandatrophy asks: Is this constraint falsely labeled as coordination (when it is actually extraction)? The answer is no — the constraint genuinely coordinates energy system transition (renewable capacity is needed, fossil phase-out is physical necessity, infrastructure adaptation is real coordination problem) AND genuinely extracts (costs are concentrated, exit is unavailable for powerless agents, distribution is asymmetric). The tension between these is not a labeling error; it is the defining structure of tangled rope. The constraint resolves mandatrophy by remaining tangled rope at the analytical level while exhibiting the full perspectival range (snare, rope, scaffold, piton, mountain) from different structural positions. The classification is not 'which type is correct?' but 'all types are correct from their respective positions, and the system-level classification (tangled rope) reflects the hybrid coordination-extraction structure that produces this perspectival range.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_implementation_fidelity,
    'Are just-transition mechanisms (reskilling funds, regional investment) actually implemented with sufficient pace and adequacy to prevent worker extraction?',
    'Longitudinal tracking of reskilling program completion rates, regional investment timelines, wage replacement levels, and employment outcomes for displaced workers vs. baseline expectations',
    'If high fidelity: constraint moves toward scaffold (organized exit path). If low fidelity: constraint moves toward snare (promised exit not materialized, extraction persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_implementation_fidelity, empirical, 'Whether just-transition programs achieve promised worker protection').

omega_variable(
    energy_cost_burden_distribution,
    'Are the costs of net-zero transition distributed proportionally or reggressively across income deciles?',
    'Cost-benefit analysis by income quintile: energy bill increases, retrofit costs, transport costs vs. income; tracking of subsidy incidence (who receives heat pump grants, who absorbs costs)',
    'If proportional: extraction is moderate, constraint approaches tangled rope. If regressive: extraction of low-income households is severe, constraint approaches snare for this agent. Potential oscillation as policy adjusts redistributive mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(energy_cost_burden_distribution, empirical, 'Whether transition costs fall disproportionately on low-income households').

omega_variable(
    renewable_capacity_sufficiency,
    'Can the UK achieve net-zero electricity generation by 2035 with domestic renewable capacity, or will the target require imported renewable energy credits or reliance on fragile supply chains?',
    'Engineering analysis: renewable potential (wind, solar, tidal) vs. demand projections; supply chain resilience for critical components (rare earth magnets, silicon for solar, batteries); comparison to analogous economies',
    'If capacity sufficient: net-zero is structurally feasible and constraint is genuine coordination problem. If capacity insufficient: net-zero requires either reliance on volatile energy markets or carbon offsetting, shifting extraction to developing nations or making UK targets unachievable without demand reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_capacity_sufficiency, empirical, 'Whether UK can achieve net-zero targets with domestic renewable capacity').

omega_variable(
    capital_reallocation_mechanism,
    'Does the transition pathway constitute genuine capital reallocation (from fossil to renewable) or merely capital addition (green investment added without fossil divestment)?',
    'Financial flow analysis: total capital deployed, percentage from new funding vs. reallocation from fossil sector, timeline of fossil asset write-downs vs. renewable capacity additions',
    'If genuine reallocation: extraction is concentrated, constraint is snare for fossil workers and tangled rope for regional economies. If capital addition: extraction is diffused through inflation and fiscal burden, affecting all taxpayers and low-income households more severely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_reallocation_mechanism, empirical, 'Whether net-zero is capital reallocation or capital addition').

omega_variable(
    policy_credibility_and_reversal_risk,
    'What is the risk that net-zero policy is reversed or significantly weakened if a subsequent government deprioritizes climate action?',
    'Institutional analysis of policy lock-in (statutory vs. discretionary commitments, international treaty obligations, cross-party consensus), historical precedent in UK and comparable democracies',
    'If credibility high: investors'' arbitrage exit is real; constraint is stable rope/tangled rope. If credibility low: investors face hold-up risk, and workers face uncertainty about reskilling timelines; constraint shifts toward snare (extraction with unstable exit promises).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_credibility_and_reversal_risk, conceptual, 'Risk of net-zero policy reversal or significant weakening').

omega_variable(
    international_competitiveness_loss,
    'Does unilateral UK net-zero commitment reduce industrial competitiveness relative to nations with slower transitions, and does this reduce future ability to fund just-transition programs?',
    'Comparative analysis of energy costs, manufacturing competitiveness indexes, capital investment flows, and fiscal capacity for regional support programs in net-zero leader nations vs. counterparts',
    'If competitiveness loss is severe: UK regional economies face extraction from both transition costs AND industrial decline, constraint approaches pure snare for affected regions. If loss is manageable: constraint remains tangled rope with moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_competitiveness_loss, empirical, 'Whether unilateral net-zero reduces UK industrial competitiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_net_zero_transition_pathway, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uknet_tr_t0, uk_net_zero_transition_pathway, theater_ratio, 0, 0.52).
narrative_ontology:measurement(uknet_tr_t3, uk_net_zero_transition_pathway, theater_ratio, 3, 0.6).
narrative_ontology:measurement(uknet_tr_t6, uk_net_zero_transition_pathway, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(uknet_be_t0, uk_net_zero_transition_pathway, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uknet_be_t3, uk_net_zero_transition_pathway, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(uknet_be_t6, uk_net_zero_transition_pathway, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_net_zero_transition_pathway, resource_allocation).
narrative_ontology:boltzmann_floor_override(uk_net_zero_transition_pathway, 0.18).
narrative_ontology:affects_constraint(uk_net_zero_transition_pathway, fossil_fuel_pension_liabilities).
narrative_ontology:affects_constraint(uk_net_zero_transition_pathway, grid_infrastructure_lock_in).
narrative_ontology:affects_constraint(uk_net_zero_transition_pathway, renewable_supply_chain_fragility).

% DUAL FORMULATION NOTE:
% The net-zero pathway constraint family decomposes into three structurally distinct constraints: (1) transition_infrastructure_coordination (ε ≈ 0.25, Rope) — genuine coordination of renewable capacity deployment and grid modernization; (2) worker_displacement_extraction (ε ≈ 0.72, Snare) — asymmetric cost concentration on fossil workers; (3) energy_cost_burden (ε ≈ 0.65, Tangled Rope) — mixed coordination of household energy efficiency with regressive cost distribution. This story synthesizes all three; decomposition into family would create three separate files linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_net_zero_transition_pathway, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
