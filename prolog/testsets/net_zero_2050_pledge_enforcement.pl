% ============================================================================
% CONSTRAINT STORY: net_zero_2050_pledge_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_net_zero_2050_pledge_enforcement, []).

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
 *   constraint_id: net_zero_2050_pledge_enforcement
 *   human_readable: Net Zero 2050 Pledge Enforcement
 *   domain: environmental_policy/climate_governance
 *
 * SUMMARY:
 *   The Net Zero 2050 pledge represents a global institutional commitment to
 *   eliminate greenhouse gas emissions by mid-century, formalized through
 *   national pledges, corporate commitments, and financial sector net-zero
 *   initiatives. The constraint operates across three structural dimensions:
 *   (1) the coordination problem it solves (mobilizing capital and political
 *   will for energy transition), (2) the asymmetric extraction it enables
 *   (concentrating costs on powerless actors while benefiting incumbent
 *   industries and developed economies), and (3) the enforcement gap between
 *   pledges and behavior (theater_ratio rising from 0.42 to 0.72 reflects
 *   growing divergence between rhetorical commitment and actual emissions
 *   reductions). The constraint classifies as Tangled Rope because it
 *   genuinely coordinates global climate action while simultaneously
 *   extracting through enforcement mechanisms that shift transition costs
 *   onto vulnerable populations and developing economies. The extractiveness
 *   trajectory (0.35 → 0.58) reflects accumulating extraction as developed
 *   economies use carbon accounting loopholes, offset arbitrage, and
 *   production relocation to maintain compliance while continuing fossil
 *   expansion. The theater ratio trajectory (0.42 → 0.68) reveals
 *   institutional degradation: climate diplomacy institutions proliferate
 *   agreements while global emissions rise, and compliance metrics become
 *   increasingly performative. This constraint exhibits all six
 *   classification types across perspectives, demonstrating how indexical
 *   positions shape perception of the same structural phenomenon.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Dependent Workers: Primary victims (powerless/trapped) — Coal miners, oil refinery workers, power plant operators facing job loss with minimal transition support or geographic alternatives
 *   - Developing Economies: Primary victims (moderate/constrained) — Countries with limited capital for energy transition, facing debt conditionality and technology licensing costs attached to climate finance
 *   - Energy Transition Industries: Primary beneficiaries (institutional/arbitrage) — Solar, wind, battery, and EV manufacturers capturing massive capital flows and policy support mobilized by pledges
 *   - Carbon Credit Traders: Secondary beneficiary (institutional/arbitrage) — Financial institutions and offset project developers extracting rents through carbon accounting arbitrage and offset certification
 *   - National Governments: Mixed role (institutional/arbitrage) — Pledge nominally as climate leadership while using accounting loopholes to avoid actual emissions reductions; extract from populations through carbon taxes and industrial policy favoring capital-intensive sectors
 *   - Sub-National Climate Coalitions: Organized agents (organized/mobile) — Cities and regions with genuine agency and exit options, building alternative coordination pathways (community choice aggregation, municipal bonds, regional grids)
 *   - Traditional Climate Diplomacy Institutions: Institutional inertia (institutional/arbitrage) — UNFCCC, IPCC, intergovernmental negotiation structures maintaining performative authority while enforcement capacity approaches zero
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(net_zero_2050_pledge_enforcement, 0.58).
domain_priors:suppression_score(net_zero_2050_pledge_enforcement, 0.65).
domain_priors:theater_ratio(net_zero_2050_pledge_enforcement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(net_zero_2050_pledge_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(net_zero_2050_pledge_enforcement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(net_zero_2050_pledge_enforcement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(net_zero_2050_pledge_enforcement, tangled_rope).
narrative_ontology:human_readable(net_zero_2050_pledge_enforcement, "Net Zero 2050 Pledge Enforcement").
narrative_ontology:topic_domain(net_zero_2050_pledge_enforcement, "environmental_policy/climate_governance").

domain_priors:requires_active_enforcement(net_zero_2050_pledge_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(net_zero_2050_pledge_enforcement, energy_transition_industries).
narrative_ontology:constraint_beneficiary(net_zero_2050_pledge_enforcement, carbon_credit_traders).
narrative_ontology:constraint_beneficiary(net_zero_2050_pledge_enforcement, national_governments).
narrative_ontology:constraint_victim(net_zero_2050_pledge_enforcement, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(net_zero_2050_pledge_enforcement, developing_economies).
narrative_ontology:constraint_victim(net_zero_2050_pledge_enforcement, future_climate_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL FUEL DEPENDENT WORKERS (SNARE) — Trapped by geographic dependence on coal and oil infrastructure with no alternative employment pathways. Coal mining regions, oil refineries, and petrochemical plants offer single dominant employer status. Exit requires relocation, retraining, and abandonment of community ties. Suppression mechanisms include disinvestment from transition support, political capture preventing retraining funding, and narrative erasure of worker constituencies from climate discourse. Maximum extraction — bears full cost of the transition while experiencing zero agency.
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVELOPING ECONOMIES (TANGLED ROPE) — Constrained by capital scarcity and debt vulnerability, yet also coordinating via the Net Zero pledge framework to access climate finance and technology transfer mechanisms. The constraint has genuine coordination function (global emissions reduction requires coordinated action); simultaneously, the enforcement mechanism extracts through debt conditionality, technology licensing restrictions, and agricultural land grabs for carbon offset plantations. Asymmetric extraction: developed economies can transition using accumulated capital and existing industrial capacity; developing economies must coordinate transition while in poverty.
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ENERGY TRANSITION INDUSTRIES (ROPE) — Primary beneficiaries. Solar, wind, battery, and electric vehicle manufacturers experience the Net Zero pledge as pure coordination: the constraint mobilizes capital, policy support, and consumer demand for their products. Exit options abundant — these industries can arbitrage across regulatory regimes, source capital globally, and relocate operations. Low effective extraction because the power dynamic runs toward the beneficiary. The constraint solves their coordination problem (converting global climate concern into market demand) with minimal coercive overhead.
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKER COALITION ADVOCATES (TANGLED ROPE) — Organized agents (unions, climate-justice NGOs) see both coordination and asymmetric extraction. The constraint provides a framework for demanding 'just transition' funds and worker protections; simultaneously, the enforcement mechanisms often weaken union power through gig economy proliferation in green sectors (solar installers, EV charging networks) and busting of traditional energy-sector unions. Constrained by limited political leverage and funding dependence on philanthropic sources. Mixed classification: genuine coordination function (matching worker retraining to transition timeline) alongside extraction (weakening worker bargaining power).
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL CLIMATE DIPLOMACY INSTITUTIONS (PITON) — The UNFCCC, IPCC, and intergovernmental negotiation structures are now primarily performative. These institutions proliferate agreements (Paris Accord, Glasgow Climate Pact, Dubai Consensus) while global emissions continue rising. The institutions persist through inertia — they command diplomatic attention and convene actors — but their enforcement capacity is near-zero. Theater ratio measures the gap between rhetoric and implementation. These structures see their own process as degraded but cannot reinvent because doing so would require admitting that international agreements without enforcement teeth are unfit for purpose. Piton classification derives from high theater and low effective enforcement.
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SUB-NATIONAL CLIMATE COALITIONS (SCAFFOLD) — Cities, states, and regional coalitions (C40 Cities, Regional Greenhouse Gas Initiative) experience Net Zero pledges as a temporary scaffolding structure with built-in sunset. These actors have agency and exit options — they can transition at their own pace, use carbon pricing to fund adaptation, and build parallel institutions (city bonds, regional grids). The constraint has genuine coordination function for local climate action; extraction is moderate because these actors retain capacity to negotiate terms. The scaffold is real: as distributed renewable grids mature and battery storage becomes commodity, the centralized enforcement mechanism becomes unnecessary and naturally sunsets.
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - PHYSICAL LIMITS VIEW (MOUNTAIN) — From a civilizational perspective on planetary energy budgets, the Net Zero constraint appears as a natural law: the carbon budget for 1.5°C warming is finite and fixed by thermodynamics. Any observed enforcement failure appears to reflect an immutable constraint — you cannot negotiate with physics. However, this mountain classification is diagnostically suspect. The actual constraint operating is the institutional enforcement mechanism, not the physical budget. The physical budget is real; the enforcement gap is contingent institutional arrangement. The false summit detector will identify this as naturalization of contingent policy.
constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(net_zero_2050_pledge_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(net_zero_2050_pledge_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(net_zero_2050_pledge_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(net_zero_2050_pledge_enforcement, TR),
    TR >= 0.70.

:- end_tests(net_zero_2050_pledge_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over interval. The constraint begins with genuine coordination (0.35) — mobilizing capital and political will is necessary. However, extractiveness rises as enforcement mechanisms become active. Developed economies use scope 3 emissions accounting loopholes, carbon offset arbitrage, and production relocation to maintain compliance while continuing fossil expansion in consumption terms. The constraint extracts from fossil fuel workers through job loss with inadequate transition support, from developing economies through debt conditionality and technology licensing costs, and from the climate system through delayed real emissions reductions. Suppression (0.65): High. Mechanisms include: (1) geographic lock-in of fossil fuel workers without alternative employment or relocation subsidies, (2) capital scarcity constraints preventing developing economies from accessing clean energy capital, (3) intellectual property protections on renewable technology driving up transition costs, (4) political economy of fossil fuel subsidies and regulatory capture preventing rapid disinvestment, (5) narrative capture of climate discourse by techno-optimist solutions (carbon capture, offset certificates) that avoid addressing consumption patterns. Theater ratio (0.68): High and rising. Climate diplomacy institutions commit to targets while global emissions continue rising. Carbon accounting diverges from physical reality through scope 3 loopholes, offset permanence assumptions, and accounting arbitrage. Just transition rhetoric increases while funding remains symbolic. The gap between pledge rhetoric and emissions trajectory grows over the interval, indicating increasing performative content.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is stark. Fossil fuel workers perceive the Net Zero pledge as a Snare: unavoidable extraction with no agency or alternatives. Energy transition industries perceive it as Rope: coordination mechanism solving their market access problem with minimal coercive overhead. Energy transition industries and workers experience entirely different χ values from the same constraint because their directionality values differ by nearly an order of magnitude. Developing economies perceive Tangled Rope: real coordination function (global emissions reduction requires their participation) alongside asymmetric extraction (they bear transition costs while developed economies shift production and use accounting loopholes). The traditional climate diplomacy institutions perceive a Piton: their own institutional procedures have become performative, but they cannot reinvent without admitting that their enforcement mechanisms are unfit for purpose. Sub-national coalitions perceive a Scaffold: temporary coordination structure with real sunset as distributed renewables mature and battery storage becomes commodity. The analytical observer risks perceiving a Mountain (Net Zero as immutable physical law), but this is diagnostically exposed as a false summit — the physical carbon budget is real, but the institutional enforcement mechanism is contingent and contains extractive loopholes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives due to structural differences in power and exit options. Fossil fuel workers (d ≈ 0.95) experience maximum extraction: trapped by geography, skill specificity, and community ties with no arbitrage options. Developing economies (d ≈ 0.75) face high extraction through debt conditionality and technology licensing despite constrained exit options. Energy transition industries (d ≈ 0.10) are beneficiaries with arbitrage options — they can source capital globally, relocate freely, and exit if regulatory support changes. National governments (d ≈ 0.55) occupy a hybrid position: they benefit from capital flows to transition industries while bearing political costs of worker displacement and bearing extraction from international climate finance conditionality. Sub-national coalitions (d ≈ 0.45) have more agency through distributed renewable grids and municipal finance mechanisms. Worker coalition advocates (d ≈ 0.65) face constrained exit through union membership and funding dependence but have organizing capacity and coalition leverage. The directionality spread (0.10 to 0.95) produces correspondingly different experienced extractiveness values through the sigmoid function, explaining why beneficiaries see pure coordination (Rope) while victims see extraction (Snare) from identical structural constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy through the coupling of coordination and extraction functions. The Tangled Rope classification is correct because: (1) genuine coordination function exists — global emissions reduction requires coordinated capital deployment and policy alignment, which the pledge framework provides; (2) asymmetric extraction occurs through enforcement mechanisms that concentrate costs on powerless agents (fossil fuel workers, developing economies) while benefits concentrate on already-powerful actors (energy transition industries, developed economy governments); (3) active enforcement exists through capital flows, regulatory policy, and international finance conditionality. The mandatrophy prevents naive classification as pure Rope (which would miss the extraction) or pure Snare (which would miss the coordination). The perspectival diversity resolves mandatrophy by showing that the same constraint legitimately appears as Rope to beneficiaries, Snare to victims, Scaffold to organized sub-national actors with exit paths, and Piton to institutional actors whose enforcement capacity has degraded. The constraint is not inherently one type — its type depends on structural position. The analytical observer's false mountain classification reveals how physical limits can naturalize contingent institutional arrangements. The rising theater ratio indicates Goodhart drift: as pledge compliance becomes easier to fake through accounting arbitrage, the constraint's function shifts from coordination toward performative extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_funding_sufficiency,
    'Is the committed capital for worker retraining and community economic transition sufficient to prevent accumulating harm to fossil fuel dependent workers, or is it a symbolic gesture?',
    'Longitudinal tracking of retraining completion rates, wage replacement trajectories, and community economic indicators (unemployment, median income, life expectancy) in transition regions over 10-year cohorts',
    'If sufficient: constraint is Tangled Rope with real coordination function balancing transition equity. If symbolic: constraint is Snare for workers and exploitation mechanism justified by green rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_funding_sufficiency, empirical, 'Whether just transition funding is sufficient or purely symbolic').

omega_variable(
    enforcement_capacity_vs_rhetorical_commitment,
    'What is the actual causal mechanism by which Net Zero pledges drive behavioral change in fossil fuel infrastructure investment and retirement?',
    'Comparative analysis of capital flows pre- and post-pledge; correlation analysis between pledge stringency and actual divestment rates; examination of loopholes in carbon accounting (scope 3 emissions, offset mechanisms, etc.)',
    'If pledges drive real divestment: constraint has moderate extractiveness and genuine enforcement. If pledges decouple from behavior: constraint is primarily performative (Piton classification), with extraction hidden in carbon credit arbitrage and offset accounting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_rhetorical_commitment, empirical, 'Causal mechanism linking pledges to actual behavior change').

omega_variable(
    developing_economy_debt_conditionality_coupling,
    'To what degree does IMF/World Bank climate conditionality on transition loans replicate or worsen extractive patterns from structural adjustment programs?',
    'Comparative policy analysis of climate finance conditions vs traditional development loans; tracking of privatization and deregulation requirements attached to Net Zero transition funds; measurement of terms-of-trade effects on climate-compliant developing economies',
    'If structurally similar to adjustment: constraint is explicitly extractive (Snare) masquerading as coordination through climate framing. If genuinely reformed: constraint is Tangled Rope with coordination function and asymmetric extraction but reduced historical predation patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_economy_debt_conditionality_coupling, empirical, 'Whether climate conditionality replicates extractive structural adjustment patterns').

omega_variable(
    carbon_offset_accounting_integrity,
    'What proportion of carbon offsets (forests, soil carbon, methane capture) used for Net Zero accounting are actually displacing emissions versus enabling accounting arbitrage?',
    'Verification audits of offset permanence and additionality; satellite monitoring of forest offset projects for actual land use change; comparison of offset prices to marginal abatement costs in renewable energy sectors',
    'If mostly legitimate: Net Zero pledges reflect genuine global emissions reductions. If mostly accounting arbitrage: pledges mask continued fossil expansion in developed economies while appearing compliant, making constraint primarily extractive (Snare with smoke-and-mirrors accounting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_offset_accounting_integrity, empirical, 'Whether carbon offsets achieve real emissions reduction or enable accounting arbitrage').

omega_variable(
    coordinated_regulatory_arbitrage,
    'Are developed economies using Net Zero pledges to shift high-emission production to unregulated jurisdictions while claiming compliance through scope 3 emissions accounting loopholes?',
    'Tracking of manufacturing relocation to unregulated zones post-pledge; analysis of implicit tariffs on carbon-intensive imports vs actual climate policy stringency; measurement of consumption-based vs production-based emissions accounting discrepancies',
    'If widespread: constraint is extractive mechanism hidden in accounting frameworks, enabling rich-economy free-riding. If controlled: constraint has real enforcement and represents genuine global coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinated_regulatory_arbitrage, empirical, 'Whether pledges enable regulatory arbitrage and production shifting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(net_zero_2050_pledge_enforcement, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nz2050_tr_t0, net_zero_2050_pledge_enforcement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nz2050_tr_t5, net_zero_2050_pledge_enforcement, theater_ratio, 5, 0.58).
narrative_ontology:measurement(nz2050_tr_t10, net_zero_2050_pledge_enforcement, theater_ratio, 10, 0.68).
narrative_ontology:measurement(nz2050_tr_t15, net_zero_2050_pledge_enforcement, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(nz2050_be_t0, net_zero_2050_pledge_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nz2050_be_t5, net_zero_2050_pledge_enforcement, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(nz2050_be_t10, net_zero_2050_pledge_enforcement, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(nz2050_be_t15, net_zero_2050_pledge_enforcement, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(net_zero_2050_pledge_enforcement, resource_allocation).
narrative_ontology:boltzmann_floor_override(net_zero_2050_pledge_enforcement, 0.18).
narrative_ontology:affects_constraint(net_zero_2050_pledge_enforcement, carbon_offset_integrity).
narrative_ontology:affects_constraint(net_zero_2050_pledge_enforcement, just_transition_financing).
narrative_ontology:affects_constraint(net_zero_2050_pledge_enforcement, regulatory_arbitrage_mechanisms).
narrative_ontology:affects_constraint(net_zero_2050_pledge_enforcement, fossil_fuel_subsidy_persistence).

% DUAL FORMULATION NOTE:
% Net Zero 2050 pledge enforcement decomposes into several structurally distinct constraints: (1) the coordination problem of mobilizing global capital for energy transition (this story, ε≈0.58); (2) the verification problem of carbon accounting integrity (downstream, ε≈0.70+, likely Snare); (3) the financing problem of just transition for affected workers (downstream, ε≈0.65, Tangled Rope). These are linked via network edges because the coordination function in this story depends on assumptions (honest accounting, adequate worker transition) that are contested in downstream stories. The present story models the aggregate constraint; decomposed stories would examine specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(net_zero_2050_pledge_enforcement, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
