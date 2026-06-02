% ============================================================================
% CONSTRAINT STORY: rural_land_externalities
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rural_land_externalities, []).

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
 *   constraint_id: rural_land_externalities
 *   human_readable: Rural Land Use Externalities and Subsidy Coordination
 *   domain: agricultural_economics/environmental_policy
 *
 * SUMMARY:
 *   Rural land use generates structural externalities where agricultural
 *   producers benefit from subsidized inputs and commodity price supports
 *   while environmental costs (groundwater contamination, soil degradation,
 *   watershed damage) accumulate on small-scale farmers, rural water users,
 *   and downstream communities. The constraint combines genuine coordination
 *   functions (price stability for farmers, reliable commodity supply for
 *   consumers) with systematic extraction (externality costs borne by
 *   powerless agents, subsidy benefits concentrated among large producers).
 *   The subsidy-externality system has degraded over 40 years as: (1)
 *   extractiveness increased (ε: 0.35→0.58) as fertilizer inputs intensified
 *   and subsidy programs expanded; (2) theater ratio increased (0.45→0.68) as
 *   subsidy programs shifted from supporting struggling farmers to
 *   maintaining consolidated industrial agriculture while performing rural
 *   stewardship. The constraint exhibits all six DR types depending on
 *   observer position: immutable ecological limits (mountain), temporary
 *   policy problem with reform pathway (scaffold), degraded institutional
 *   ritual (piton), pure extraction for trapped agents (snare), mixed
 *   coordination-extraction for moderate agents (tangled rope), and
 *   coordinating function for beneficiaries (rope).
 *
 * KEY AGENTS:
 *   - Large Agricultural Producers: Primary beneficiary (institutional/arbitrage) — capture commodity price supports, fertilizer subsidies, and scale economies; externalize environmental costs
 *   - Small-Scale Farmers: Primary victim (powerless/trapped) — bear environmental degradation costs and debt burdens while subsidy structure incentivizes industrial consolidation
 *   - Rural Groundwater Users: Secondary victim (moderate/constrained) — face contamination from agricultural runoff; barriers to exit via alternative water sources or relocation
 *   - Downstream Watershed Communities: Secondary victim (moderate/constrained) — bear nutrient runoff and water quality degradation costs; constrained by infrastructure dependency
 *   - Environmental Regulation Coalition: Organized agents (organized/constrained) — propose policy reforms and regenerative agriculture incentives with generational sunset logic
 *   - Agricultural Support System: Institutional actor (institutional/arbitrage) — maintains subsidy and price support programs; sees own mechanism as degraded but persists through political inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choice as ecological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rural_land_externalities, 0.58).
domain_priors:suppression_score(rural_land_externalities, 0.62).
domain_priors:theater_ratio(rural_land_externalities, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rural_land_externalities, extractiveness, 0.58).
narrative_ontology:constraint_metric(rural_land_externalities, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(rural_land_externalities, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rural_land_externalities, tangled_rope).
narrative_ontology:human_readable(rural_land_externalities, "Rural Land Use Externalities and Subsidy Coordination").
narrative_ontology:topic_domain(rural_land_externalities, "agricultural_economics/environmental_policy").

domain_priors:requires_active_enforcement(rural_land_externalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rural_land_externalities, large_agricultural_producers).
narrative_ontology:constraint_beneficiary(rural_land_externalities, chemical_manufacturers).
narrative_ontology:constraint_beneficiary(rural_land_externalities, commodity_exporters).
narrative_ontology:constraint_victim(rural_land_externalities, small_scale_farmers).
narrative_ontology:constraint_victim(rural_land_externalities, rural_groundwater_users).
narrative_ontology:constraint_victim(rural_land_externalities, downstream_watershed_communities).
narrative_ontology:constraint_victim(rural_land_externalities, wildlife_habitat_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-SCALE FARMER (SNARE) — Trapped by land debt, commodity price volatility, and subsidy structure that incentivizes industrial monoculture. Cannot exit without abandoning generational holdings or accepting bankruptcy. Externality costs (soil degradation, water contamination) accumulate on their land while commodity price floors and fertilizer subsidies flow to large producers. Maximum extraction from this position.
constraint_indexing:constraint_classification(rural_land_externalities, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL GROUNDWATER USERS (TANGLED ROPE) — Constrained by infrastructure dependency (wells, irrigation systems tied to current land use pattern) and regulatory complexity. Genuine coordination exists: fertilizer runoff prevention benefits all users. But asymmetric extraction runs through: industrial agriculture externalizes water contamination costs onto downstream users who bear cleanup expense and risk. Some exit via relocation or remediation, but at high cost.
constraint_indexing:constraint_classification(rural_land_externalities, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE AGRICULTURAL PRODUCER (ROPE) — Experiences the constraint as coordination: commodity standards, fertilizer supply chains, market infrastructure. Net beneficiary through price supports and input subsidies. Can arbitrage between regulatory jurisdictions (relocate production or source inputs from more permissive regions). The constraint enables profitable scaling while externality costs remain off-balance-sheet.
constraint_indexing:constraint_classification(rural_land_externalities, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENVIRONMENTAL REGULATION COALITION (SCAFFOLD) — Organized agents (watershed districts, conservation nonprofits, regulatory agencies) see the current subsidy-externality structure as temporary and solvable through policy reform. Active enforcement exists (water quality standards, nutrient management plans) with explicit sunset logic: payments-for-ecosystem-services and regenerative agriculture incentive programs are building alternative pathways. Constraint is structural but declining with policy sunset clause: estimated 15-25 year transition to internalized costs.
constraint_indexing:constraint_classification(rural_land_externalities, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AGRICULTURAL SUPPORT SYSTEM (PITON) — Price support systems, crop insurance, and fertilizer subsidies persist largely through institutional inertia. Original coordination function (stabilizing rural incomes post-Depression) has been substantially replaced by rent-seeking. Theater ratio (0.68) reflects that much program activity is performative: subsidy payments maintain rural political constituency more than they support viable farming. System sees its own mechanism as degraded but maintains it for political stability rather than functional necessity.
constraint_indexing:constraint_classification(rural_land_externalities, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONTINENTAL AGRICULTURAL MARKET (TANGLED ROPE) — At market scale, externality subsidy structure coordinates international commodity trading (standardized prices, stable supply) while extracting through environmental degradation that accumulates across borders. Fertilizer-intensive monoculture spreads because inputs are subsidized; watershed contamination crosses jurisdictional boundaries; soil erosion accelerates regionally. Market participants experience coordination benefits (price stability, input availability) alongside extraction costs (environmental liability, regulatory uncertainty). Organized agents can exit by relocating production or sourcing.
constraint_indexing:constraint_classification(rural_land_externalities, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECOLOGICAL LIMITS VIEW (MOUNTAIN) — From a civilizational perspective, some agricultural externality is inherent to large-scale food production: monoculture always increases pest pressure, chemical inputs always run off into water systems, soil organic matter always declines under intensive use. This perspective sees the externality constraint as a natural law of agronomic systems. However, the structural data contradicts the mountain classification — the engine will identify this as false naturalization of what is actually a contingent policy choice (whether to internalize costs or externalize them).
constraint_indexing:constraint_classification(rural_land_externalities, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rural_land_externalities_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rural_land_externalities, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rural_land_externalities, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rural_land_externalities, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rural_land_externalities, TR),
    TR >= 0.70.

:- end_tests(rural_land_externalities_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The subsidy-externality structure transfers wealth to large producers while imposing environmental remediation costs on powerless agents. The extractiveness is not maximal (0.75+) because some coordination value is genuine (price stability does support some viable farming, commodity supply chains do function). The 0.58 value reflects that extraction is substantial but embedded in mixed coordination. Suppression (0.62): Moderate-high. Significant barriers to exit include: land debt and illiquidity (small farmers cannot sell without economic destruction), infrastructure lock-in (water users cannot redirect irrigation systems), regulatory complexity (policy reform requires political consensus), and path-dependent commodity systems (switching production patterns requires multi-year transition). But suppression is not total (0.80+) — some actors can exit via relocation, policy change is possible, alternative farming models exist. Theater ratio (0.68): Moderate-high. Subsidy programs perform rural support while primary function has shifted to rent extraction and political constituency maintenance. Program administration (crop insurance, price floor enforcement) is substantial but ritualistic — much effort goes to maintaining the appearance of farmer support rather than enabling viable farming. Environmental regulations create performative compliance (nutrient management plans) without necessarily reducing runoff. Theater has increased over the interval as programs shifted from stabilizing marginal farms to subsidizing consolidation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the beneficiary (large producer) sees rope (coordination), the victim (small farmer) sees snare (extraction), the organized coalition sees scaffold (temporary problem with sunset), the degraded system sees piton (ritual without function), moderate agents see tangled rope (mixed coordination and extraction), and the analytical observer risks seeing mountain (false naturalization). The perspectival gaps reflect real structural differences: large producers genuinely experience the subsidy system as enabling stable commodity production (their rope experience is accurate); small farmers genuinely experience land degradation and debt accumulation without subsidy access (their snare experience is accurate); the environmental coalition genuinely sees policy levers for reform (their scaffold experience reflects real political possibility); and the support system genuinely operates through institutional momentum rather than functional necessity (the piton observation is diagnostically sound). The false mountain perspective would claim agricultural externalities are inherent to food production — but the empirical variation in externality intensity across farming methods reveals this is contingent institutional choice, not ecological law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the externality flow. Large producers benefit from subsidies and externalize costs → low d (full beneficiary). Small farmers are trapped with debt and land degradation → high d (full target). Groundwater users and downstream communities bear contamination costs while having some exit options (relocation, remediation) → moderate-high d. Environmental coalition members are organized and can exit via policy change → moderate d. Agricultural support system maintains subsidies despite degraded function → beneficiary position (low d) but increasingly theatrical. At civilizational analytical scope, the mountain perspective is tempting but structurally incorrect — ecological limits exist, but the constraint's extractiveness derives from institutional choice to externalize costs, not from inherent agricultural necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by showing that large-scale agricultural subsidy-externality systems are neither pure coordination (rope) nor pure extraction (snare) but rather hybrid tangled ropes where: (1) genuine coordination functions exist (price stability enables some farming, commodity systems feed populations); (2) asymmetric extraction is systematic (externality costs are borne by powerless agents while subsidy benefits concentrate among large producers); (3) active enforcement is required (regulatory compliance, subsidy administration, commodity market management); (4) the coordination function is real but degraded (subsidy programs shifted from marginal-farm support to rent extraction over 40 years). The mandatrophy-resolving insight: the subsidy-externality structure coordinates commodity markets while extracting from powerless agents precisely BECAUSE it externalizes costs. The extraction mechanism IS the coordination mechanism — subsidy extraction subsidizes commodity production, which coordinates global food markets. Reducing suppression (internalizing externality costs) would require either: (a) massive subsidy expansion to compensate for cost internalization, or (b) accepting commodity system restructuring and price volatility. The constraint is tangled rope at institutional scale but appears as snare from the powerless victim's perspective and rope from the beneficiary's perspective — the mandatrophy is resolved by recognizing that all three perspectives are structurally accurate for their positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_necessity_ambiguity,
    'Are agricultural price supports necessary to prevent rural economic collapse, or do they maintain artificially concentrated land ownership that would otherwise consolidate further?',
    'Historical analysis of rural income and land ownership patterns; comparison of regions with vs without subsidy structures; modeling of transition to cost-internalized pricing',
    'If necessary: subsidy structure is legitimate coordination (Rope from more perspectives). If artificial: subsidies are pure rent extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_necessity_ambiguity, empirical, 'Whether subsidies prevent collapse or artificially concentrate ownership').

omega_variable(
    externality_localization,
    'Do externality costs (groundwater contamination, soil degradation, nutrient runoff) remain localized to agricultural regions or distribute globally through food trade and atmospheric/water transport?',
    'Isotope tracing of nutrient pathways; mapping of contaminated watershed extent; lifecycle analysis of food system externalities across supply chain',
    'If localized: small-farmer perspective sees regional Snare. If distributed: global commodity market perspective amplifies extraction to institutional scale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_localization, empirical, 'Geographic scope of externality distribution').

omega_variable(
    regenerative_agriculture_scalability,
    'Can regenerative agriculture (cover crops, crop rotation, reduced tillage) achieve commodity-scale yields without subsidized chemical inputs, or is it fundamentally constrained to smaller farms?',
    'Yield comparisons at scale (>500 acres); input cost analysis over 10-year rotation; carbon/soil quality metrics vs conventional farming',
    'If scalable: scaffold sunset is real and timeline is predictable. If not: externality constraint persists regardless of policy reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regenerative_agriculture_scalability, empirical, 'Whether regenerative agriculture can replace industrial monoculture at scale').

omega_variable(
    institutional_lock_in_mechanism,
    'Is the persistence of subsidy-externality structure driven by vested financial interest (agribusiness political power), genuine rural economic necessity, or path-dependent institutional inertia?',
    'Political contribution tracking; rural income sensitivity analysis; comparative policy shifts in peer democracies; institutional history of subsidy programs',
    'If financial vested interest dominates: piton perspective is correct — constraint persists despite dysfunction. If economic necessity: reform timeline must account for transition costs. If inertia: rapid policy change is feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in_mechanism, preference, 'Primary driver of constraint persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rural_land_externalities, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rle_tr_t0, rural_land_externalities, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rle_tr_t20, rural_land_externalities, theater_ratio, 20, 0.58).
narrative_ontology:measurement(rle_tr_t40, rural_land_externalities, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(rle_be_t0, rural_land_externalities, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rle_be_t20, rural_land_externalities, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(rle_be_t40, rural_land_externalities, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rural_land_externalities, resource_allocation).
narrative_ontology:affects_constraint(rural_land_externalities, agricultural_commodity_markets).
narrative_ontology:affects_constraint(rural_land_externalities, groundwater_depletion).
narrative_ontology:affects_constraint(rural_land_externalities, soil_carbon_accumulation).
narrative_ontology:affects_constraint(rural_land_externalities, rural_population_decline).

% DUAL FORMULATION NOTE:
% Rural land externalities decompose into three structurally distinct constraints: (1) commodity market coordination (low ε, rope) addressing price stability; (2) subsidy distribution (high ε, snare) addressing wealth transfer; (3) environmental remediation (moderate ε, tangled rope) addressing cost absorption. This story models all three as coupled components of a single constraint family. Upstream stories include agricultural commodity market price support (ε≈0.25) and downstream stories include groundwater contamination (ε≈0.72) and rural population collapse (ε≈0.68).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rural_land_externalities, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
