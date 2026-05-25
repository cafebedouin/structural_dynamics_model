% ============================================================================
% CONSTRAINT STORY: carbon_pricing_distributional_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carbon_pricing_distributional_asymmetry, []).

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
 *   constraint_id: carbon_pricing_distributional_asymmetry
 *   human_readable: Carbon Pricing Distributional Asymmetry
 *   domain: environmental_policy/economic_inequality
 *
 * SUMMARY:
 *   Carbon pricing mechanisms (carbon taxes, cap-and-trade systems, emissions
 *   trading schemes) represent a coordination attempt to internalize climate
 *   externalities by pricing carbon. However, they create a fundamental
 *   distributional asymmetry: the costs of carbon pricing fall
 *   disproportionately on low-income households, fossil fuel dependent
 *   workers, and rural communities who lack capital for technology
 *   transitions, while benefits concentrate among capital-intensive
 *   industries, high-income households with resources for efficiency
 *   improvements, and early technology adopters. The constraint exhibits
 *   Tangled Rope structure: genuine coordination function (aligning
 *   incentives with climate goals) exists alongside asymmetric extraction
 *   (cost burden distributed regressively). Theater ratio increases over time
 *   as transition support programs become more elaborate performatively
 *   (carbon dividend announcements, retraining program establishment) while
 *   actual resource transfer and effectiveness remain contested. The
 *   constraint demonstrates how a legitimate coordination mechanism can embed
 *   structural injustice through contingent policy design choices — choices
 *   that could be different but persist through political economy dynamics
 *   and narrative naturalization ('carbon pricing is the most economically
 *   efficient approach, therefore its distributional impacts are necessary').
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victim (powerless/trapped) — bear costs without capacity for technology transition or arbitrage
 *   - Fossil Fuel Dependent Workers: Primary victim (powerless/constrained) — face job security and economic viability threats; trapped by regional economic structure
 *   - Rural Communities: Secondary victim (moderate/constrained) — benefit from climate coordination incentives but bear disproportionate transition costs due to infrastructure requirements
 *   - Capital-Intensive Industries: Primary beneficiary (institutional/arbitrage) — absorb costs and capture market share through competitive advantage in green technology adoption
 *   - High-Income Households: Primary beneficiary (institutional/arbitrage) — can afford efficiency improvements and technology transitions; experience carbon pricing as market signal for profitable investments
 *   - Environmental Justice Coalition: Organized secondary actor (organized/mobile) — advocates for revenue recycling and just transition; has leverage to reshape constraint terms
 *   - Transition Support Programs: Policy mechanism (organized/mobile) — designed to sunset the asymmetry through retraining, community investment, and dividend returns
 *   - Carbon Market Institutions: Institutional actor (institutional/arbitrage) — maintain cap-and-trade and offset mechanisms through inertia; benefit from market participation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent distributional choices as inherent to climate action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carbon_pricing_distributional_asymmetry, 0.58).
domain_priors:suppression_score(carbon_pricing_distributional_asymmetry, 0.62).
domain_priors:theater_ratio(carbon_pricing_distributional_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carbon_pricing_distributional_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(carbon_pricing_distributional_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(carbon_pricing_distributional_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carbon_pricing_distributional_asymmetry, tangled_rope).
narrative_ontology:human_readable(carbon_pricing_distributional_asymmetry, "Carbon Pricing Distributional Asymmetry").
narrative_ontology:topic_domain(carbon_pricing_distributional_asymmetry, "environmental_policy/economic_inequality").

domain_priors:requires_active_enforcement(carbon_pricing_distributional_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carbon_pricing_distributional_asymmetry, capital_intensive_industries).
narrative_ontology:constraint_beneficiary(carbon_pricing_distributional_asymmetry, high_income_households).
narrative_ontology:constraint_beneficiary(carbon_pricing_distributional_asymmetry, early_technology_adopters).
narrative_ontology:constraint_victim(carbon_pricing_distributional_asymmetry, low_income_households).
narrative_ontology:constraint_victim(carbon_pricing_distributional_asymmetry, fossil_fuel_dependent_workers).
narrative_ontology:constraint_victim(carbon_pricing_distributional_asymmetry, rural_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLD (SNARE) — Trapped by material dependency on affordable energy and transportation. Carbon pricing increases heating, fuel, and food costs without alternative mobility or heating options. No arbitrage available; exit from the constraint requires relocating or changing economic status, both infeasible at biographical time horizon. Maximum suppression and extraction experienced.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FOSSIL FUEL DEPENDENT WORKER (SNARE) — Coal, oil, and gas sector employment concentrated in specific regions with limited alternative economic opportunities. Carbon pricing threatens job security and regional economic viability. Suppression operates through economic dependency and geographic immobility. Can theoretically retrain or relocate, but costs are prohibitive and identity/community attachment deepens the lock.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RURAL COMMUNITY (TANGLED ROPE) — Benefits from genuine climate coordination: carbon pricing creates incentives for renewable energy infrastructure that rural areas need for economic diversification. But simultaneously bears asymmetric costs: transportation and heating are more expensive in dispersed settlements; retrofitting is capital-intensive; electric vehicle adoption slower due to infrastructure gaps and lower incomes. Constrained exit via cost-prohibitive transitions or relocation.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL-INTENSIVE INDUSTRY (ROPE) — Benefits from carbon pricing through competitive advantage: can absorb transition costs, invest in low-carbon technology, and capture market share from smaller competitors. Experiences carbon pricing as a coordination mechanism that aligns incentives with climate goals while enabling profitable technology deployment. Arbitrage available through product innovation, offshoring, or carbon credit strategies.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-INCOME HOUSEHOLD (ROPE) — Can absorb carbon price increases through higher discretionary spending. Arbitrage available: electric vehicles, home retrofits, renewable energy adoption, efficiency improvements all become cost-effective investments. Experiences carbon pricing as coordinating mechanism that provides market signals for profitable green technology adoption. Absolute exposure to price increases is low relative to income.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL JUSTICE COALITION (TANGLED ROPE) — Organized agents (environmental justice nonprofits, labor unions, community organizations) recognize carbon pricing as both coordinating toward necessary climate action AND creating distributional injustice. Mobile agency available through policy advocacy for revenue recycling, worker transition support, and community resilience investment. Effective extraction is moderate because the coalition has leverage to reshape the constraint's terms, though structural power remains asymmetric.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: TRANSITION SUPPORT PROGRAM (SCAFFOLD) — Policy mechanisms designed to sunset the distributional asymmetry: carbon dividend returns, worker retraining, just transition funding, community development grants. These are structural components with explicit sunset logic — the programs are temporary supports designed to phase out as low-carbon infrastructure matures and new employment opportunities develop. Theater ratio increases if programs become performative without real resource transfer.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: CARBON MARKET MECHANISM (PITON) — International carbon markets, cap-and-trade systems, and offset mechanisms persist largely through institutional inertia despite well-documented limitations: leakage, additionality failures, fraud, and effectiveness gaps. The theater ratio is high — compliance and certification rituals dominate without clear emissions reduction outcomes. The mechanism is maintained because comprehensive alternatives haven't fully replaced it, not because it effectively coordinates global carbon reduction.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / ECONOMIC THERMODYNAMIC VIEW (MOUNTAIN) — From a civilizational/universal perspective, carbon pricing creates unavoidable distributional asymmetry: the physics of carbon concentration requires rapid, large-scale reduction; the economics of transition impose costs; the distribution of those costs cannot be made perfectly symmetric because the current economic system already embeds asymmetries in income, assets, and opportunity. This perspective risks naturalizing the distributional asymmetry as inherent to climate action, when the asymmetry actually reflects contingent policy design choices.
constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carbon_pricing_distributional_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carbon_pricing_distributional_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carbon_pricing_distributional_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carbon_pricing_distributional_asymmetry, TR),
    TR >= 0.70.

:- end_tests(carbon_pricing_distributional_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial implementation (t=0) at 0.32 reflects relative novelty and limited scope. As carbon pricing expands and deepens (t=5 to t=15), extractiveness increases as costs compound for trapped agents while beneficiaries accumulate advantage through capital investments and market position. Suppression (0.62): High. Material barriers include: (1) limited mobility options (low-income households cannot easily switch heating fuels, transportation modes, or residence); (2) capital constraints (retrofitting requires upfront investment beyond low-income household capacity); (3) geographic dependency (fossil fuel workers concentrated in regions with limited alternative employment); (4) information and infrastructure gaps (rural communities lack renewable technology access and installation services). Theater ratio (0.58): Moderate-high, increasing over interval. Carbon pricing mechanisms increasingly rely on performative elements: announcement of just transition programs without adequate funding; carbon dividend proposals without implementation; carbon market compliance rituals without verified emissions reduction outcomes. The theater increases as policy design attempts to address distributional criticism through rhetorical and programmatic elaboration while fundamental asymmetries persist.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival polarization. Low-income households and fossil fuel workers (powerless/trapped perspectives) classify as pure Snare: maximum extraction with no coordination benefit perceived. Capital-intensive industries and high-income households (institutional/arbitrage perspectives) classify as pure Rope: coordination mechanism that enables profitable transitions without experienced extraction. Organized environmental justice coalition sees Tangled Rope: genuine climate coordination alongside asymmetric costs, with agency to reshape terms through advocacy. Transition support programs see Scaffold: temporary supports with explicit sunset logic designed to bridge the gap. Carbon market institutions see Piton: their own mechanisms degraded and increasingly performative. Analytical observer risks false summit (Mountain): naturalizing the distributional asymmetry as inherent to climate action. The perspectival gap is not reconcilable by better communication — it reflects genuine structural differences in who bears costs and who gains benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from beneficiary/victim status and exit capacity. Low-income households and fossil fuel workers are victims with minimal exit options (trapped or constrained), producing high d (0.85-0.95) → high f(d) (1.15-1.42) → high effective extraction chi. Capital-intensive industries and high-income households are beneficiaries with arbitrage options, producing low d (0.05-0.20) → negative/low f(d) (-0.12 to 0.02) → low/negative effective extraction chi — they experience the constraint as coordination. Organized agents (environmental justice coalition) have constrained exit but organized status and mobile options, producing moderate d (0.40-0.55) → moderate f(d) (0.40-0.75) → moderate chi. The perspectival gap reflects these directionality differences: powerless victims see snare; institutional beneficiaries see rope; organized agents with leverage see tangled rope with potential reshaping. National scope modifier σ = 1.0 applies base multiplier; global leakage dynamics could justify universal scope (σ = 1.0 or 1.2 depending on formulation).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by revealing how carbon pricing's distributional asymmetry is NOT an inherent property of climate action but a contingent policy design that concentrates extraction through (1) pricing mechanism (cost borne by consumption-based households, not wealth/asset holders); (2) regressive revenue use (if not returned as dividend); (3) insufficient just transition funding (political economy constraint, not technical constraint); (4) global leakage (carbon pricing in high-income jurisdictions drives production to low-regulation jurisdictions, shifting burden rather than reducing total emissions). Alternative policy designs exist: (a) wealth tax + carbon price (targets asset holders, not consumption); (b) mandatory dividend return (makes revenue recycling binding); (c) front-loaded transition investment (reduces suppression before pricing begins); (d) border carbon adjustment (prevents leakage). The constraint's Tangled Rope classification is correct — genuine coordination function exists (climate price signal) alongside genuine extraction (regressive cost burden) — AND the extraction component is NOT inevitable. The mandatrophy dissolves by specifying which policy parameters determine the relative weight of coordination vs extraction. Empirically testable: jurisdictions with strong dividend returns, robust retraining, and border adjustments should show lower extractiveness and faster transition to rope-only classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_dividend_sufficiency,
    'Does revenue recycling through carbon dividends actually compensate low-income households for increased costs, or does the distributional asymmetry persist even with dividend programs?',
    'Comparative analysis of household budgets before/after carbon pricing plus dividend across income deciles; longitudinal tracking of consumption patterns and cost burden in jurisdictions with carbon pricing (EU ETS, Canada, Scandinavia)',
    'If dividends fully compensate: distributional asymmetry is policy choice, not inherent. Snare classification becomes rope with just revenue return. If dividends insufficient: asymmetry persists even with best-faith redistribution, indicating fundamental mismatch between climate requirements and income distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_dividend_sufficiency, empirical, 'Whether carbon dividends adequately compensate low-income households').

omega_variable(
    alternative_decarbonization_regressive_burden,
    'Would alternative decarbonization approaches (direct regulation, subsidies for renewables, government-led transition) distribute costs more equitably than carbon pricing?',
    'Comparative policy analysis across decarbonization pathways; modeling of distributional impacts under carbon tax vs regulatory mandate vs subsidies; historical analysis of prior energy transitions and their distributional patterns',
    'If alternatives are equally or more regressive: carbon pricing''s distributional asymmetry is not a policy design flaw but an inherent feature of rapid decarbonization under current economic structure. If alternatives are less regressive: carbon pricing is not the least-extractive approach — tantamount_rope is maintained through policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_decarbonization_regressive_burden, conceptual, 'Whether alternative decarbonization approaches are more equitable').

omega_variable(
    fossil_fuel_worker_retraining_effectiveness,
    'Can displaced fossil fuel workers successfully transition to renewable energy sector employment at comparable wages and with reasonable geographic mobility?',
    'Longitudinal wage tracking of retrained workers; cost-benefit analysis of transition support programs; comparison of job availability, skill match, and wages in solar/wind/grid modernization vs coal/oil/gas sectors by region',
    'If retraining effective: suppression can decline and exit options improve to mobile; snare classification softens to tangled rope. If retraining unsuccessful: trapped classification is confirmed; structural economic dependency remains even with support programs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_worker_retraining_effectiveness, empirical, 'Effectiveness of fossil fuel worker retraining programs').

omega_variable(
    renewable_infrastructure_rural_deployment,
    'Can distributed renewable energy infrastructure (rooftop solar, small wind, community solar) actually serve rural communities cost-effectively, or does rural decarbonization require expensive centralized grid upgrades?',
    'Cost analysis of rural grid upgrades vs distributed renewable deployment; case studies of rural renewable deployment in low-income regions (Appalachia, Great Plains); comparison of transition economics in dispersed vs dense settlements',
    'If distributed renewables viable: rural communities gain exit options through local energy production and cost reduction. Tangled rope classification is accurate — genuine coordination benefit exists. If centralized upgrades required: rural households remain dependent on high-cost infrastructure; asymmetry persists regardless of carbon pricing design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_infrastructure_rural_deployment, empirical, 'Feasibility of rural distributed renewable energy deployment').

omega_variable(
    global_carbon_leakage_pattern,
    'Does carbon pricing in high-income jurisdictions drive emissions-intensive production to low-regulation jurisdictions, shifting the burden globally rather than reducing it?',
    'Trade flow analysis and embedded carbon accounting; tracking of manufacturing relocation to low-carbon-price jurisdictions; comparison of territorial vs consumption-based emissions accounting',
    'If leakage high: carbon pricing reduces high-income household costs while increasing emissions and costs in developing economies. Global distributional asymmetry is worse than domestic asymmetry. Snare classification expands globally. If leakage controlled: carbon pricing maintains coordination function globally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_carbon_leakage_pattern, empirical, 'Magnitude of carbon leakage to low-regulation jurisdictions').

omega_variable(
    political_economy_of_just_transition,
    'Why do just transition programs consistently underfund retraining and community support relative to climate policy targets, if such programs are politically justified?',
    'Budget analysis of carbon revenue allocation across jurisdictions; political economy of fossil fuel industry influence on transition program design; comparison of authorized vs appropriated funding for transition support',
    'If systemic underfunding: just transition is performative theater rather than structural mitigation of asymmetry. Piton classification applies to transition programs themselves. If adequate funding achieved: scaffold classification is vindicated; sunset mechanism is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_of_just_transition, empirical, 'Why just transition programs are underfunded relative to climate targets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carbon_pricing_distributional_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpda_tr_t0, carbon_pricing_distributional_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cpda_tr_t5, carbon_pricing_distributional_asymmetry, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cpda_tr_t10, carbon_pricing_distributional_asymmetry, theater_ratio, 10, 0.58).
narrative_ontology:measurement(cpda_tr_t15, carbon_pricing_distributional_asymmetry, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(cpda_be_t0, carbon_pricing_distributional_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cpda_be_t5, carbon_pricing_distributional_asymmetry, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cpda_be_t10, carbon_pricing_distributional_asymmetry, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cpda_be_t15, carbon_pricing_distributional_asymmetry, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carbon_pricing_distributional_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(carbon_pricing_distributional_asymmetry, 0.2).
narrative_ontology:affects_constraint(carbon_pricing_distributional_asymmetry, fossil_fuel_supply_chain_dependency).
narrative_ontology:affects_constraint(carbon_pricing_distributional_asymmetry, energy_poverty_lock_in).
narrative_ontology:affects_constraint(carbon_pricing_distributional_asymmetry, green_technology_access_inequality).
narrative_ontology:affects_constraint(carbon_pricing_distributional_asymmetry, labor_market_transition_friction).

% DUAL FORMULATION NOTE:
% Carbon pricing distributional asymmetry is downstream of the climate coordination constraint (global emissions reduction target) but represents a structurally distinct constraint in its own right. The upstream climate constraint has its own extractiveness reflecting scientific and economic necessity of rapid decarbonization; this downstream constraint has its own extractiveness reflecting policy design choices in how costs are distributed. Constraint families should include: (1) carbon_pricing_distributional_asymmetry (base story), (2) carbon_dividend_revenue_recycling (ε lower if strong dividend implementation), (3) fossil_fuel_worker_transition_support (ε lower if well-funded), (4) carbon_border_adjustment_mechanism (ε lower if leakage prevented).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carbon_pricing_distributional_asymmetry, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
