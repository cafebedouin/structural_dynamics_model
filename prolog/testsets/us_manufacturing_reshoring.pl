% ============================================================================
% CONSTRAINT STORY: us_manufacturing_reshoring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_manufacturing_reshoring, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_manufacturing_reshoring
 *   human_readable: US Manufacturing Reshoring Policy Constraint
 *   domain: economic_policy/industrial_production
 *
 * SUMMARY:
 *   US manufacturing reshoring policy represents a structural constraint
 *   combining genuine coordination needs (supply chain resilience, regional
 *   employment stability, critical capacity security) with asymmetric
 *   extraction (price increases for consumers, barriers for non-connected
 *   firms, externalized costs to overseas workers). The constraint exhibits
 *   tangled_rope structure: active enforcement (tariffs, subsidies, local
 *   content requirements, investment reviews) maintains a coordination
 *   function (domestic production capacity, labor market support) while
 *   simultaneously extracting from multiple victim groups. The theater_ratio
 *   (0.68) reflects substantial performative activity: extensive policy
 *   administration, subsidy bureaucracy, and reshoring announcements that
 *   produce limited actual manufacturing capacity relative to policy
 *   intensity. Extractiveness has risen from 0.42 to 0.58 over the interval
 *   as policies have accumulated and enforcement has intensified without
 *   corresponding manufacturing growth, suggesting accumulation of
 *   rent-seeking layered onto the original coordination rationale.
 *
 * KEY AGENTS:
 *   - Overseas Production Workers: Primary victim (powerless/trapped) — face employment collapse and wage suppression from policy-driven capital exit
 *   - US Consumers: Primary victim (powerless/constrained) — bear extraction through tariff-driven price increases and reduced product variety
 *   - Politically Connected Corporations: Primary beneficiary (institutional/arbitrage) — capture subsidies, tax incentives, and protected market access; can arbitrage across policy regimes
 *   - Domestic Labor Unions: Organized beneficiary (organized/mobile) — win wage floor agreements and employment restoration in unionized sectors; mobile in political alliance
 *   - Non-Connected Domestic Manufacturers: Secondary victim (moderate/constrained) — face input cost inflation and regulatory burden despite lack of subsidy access
 *   - Multinational Supply Chain Operators: Complex actor (powerful/mobile) — experience mixed coordination (domestic clustering benefits) and extraction (forced geographic concentration, logistics cost increases)
 *   - Green Technology Coalition: Organized reformer (organized/constrained) — frame reshoring as temporary scaffold supporting climate transition; see sunset in global carbon pricing regime
 *   - Industrial Policy Bureaucracy: Institutional actor (institutional/arbitrage) — maintains reshoring administration as performative ritual; benefits from continued policy complexity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice (reshoring) as economic law (comparative advantage immutability)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_manufacturing_reshoring, 0.58).
domain_priors:suppression_score(us_manufacturing_reshoring, 0.65).
domain_priors:theater_ratio(us_manufacturing_reshoring, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_manufacturing_reshoring, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_manufacturing_reshoring, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_manufacturing_reshoring, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_manufacturing_reshoring, tangled_rope).
narrative_ontology:human_readable(us_manufacturing_reshoring, "US Manufacturing Reshoring Policy Constraint").
narrative_ontology:topic_domain(us_manufacturing_reshoring, "economic_policy/industrial_production").

domain_priors:requires_active_enforcement(us_manufacturing_reshoring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_manufacturing_reshoring, incumbent_domestic_manufacturers).
narrative_ontology:constraint_beneficiary(us_manufacturing_reshoring, politically_connected_corporations).
narrative_ontology:constraint_beneficiary(us_manufacturing_reshoring, domestic_labor_unions).
narrative_ontology:constraint_victim(us_manufacturing_reshoring, consumers_via_price_increases).
narrative_ontology:constraint_victim(us_manufacturing_reshoring, overseas_production_workers).
narrative_ontology:constraint_victim(us_manufacturing_reshoring, non_connected_manufacturing_firms).
narrative_ontology:constraint_victim(us_manufacturing_reshoring, global_supply_chain_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVERSEAS PRODUCTION WORKERS (SNARE) — Trapped by capital flight and policy-driven supply chain disruption. Cannot organize across borders or exit the constraint. Bears full extraction cost through unemployment and wage suppression as manufacturing exits.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: US CONSUMERS (SNARE) — Constrained by market structures and tariff regimes. Experiences extraction through higher prices for consumer goods, reduced product variety, and slower innovation. Exit would require international relocation or accepting supply-side scarcity.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NON-CONNECTED DOMESTIC MANUFACTURERS (TANGLED ROPE) — Constrained by policy barriers (tariffs, local content requirements). Experience mixed coordination (access to domestic supply chains, labor availability improvements) and extraction (forced higher input costs, regulatory burden). Cannot easily relocate or exit US regulatory framework.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICALLY CONNECTED CORPORATIONS (ROPE) — Experience the constraint as pure coordination benefit. Benefit from protectionist policies, subsidies, tax incentives, and preferential access to reshoring funds. High exit options (can lobby for alternative policies, relocate subsidiaries across jurisdictions). Extraction runs toward this agent.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOMESTIC LABOR UNIONS (ROPE) — Organized actors experiencing coordination benefits through policy-driven employment restoration and wage floor agreements. Mobile in exit options (can negotiate with management or shift political support). Primary beneficiary of reshoring rhetoric, though benefits are often concentrated in unionized sectors and geographies.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: MULTINATIONAL SUPPLY CHAIN OPERATORS (TANGLED ROPE) — Experience mixed coordination (access to reshored production, domestic supply clustering) and extraction (forced geographic concentration, higher logistics costs, reduced flexibility). Mobile enough to arbitrage across policies, but constrained by sunk capital in existing networks.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: GREEN TECHNOLOGY COALITION (SCAFFOLD) — See reshoring as temporary support mechanism for climate-oriented manufacturing transition. Frame domestic production as coordination (environmental monitoring, labor standards, supply chain transparency) with built-in sunset as global carbon pricing and trade rules mature. Extraction is tolerated as transitional cost, not permanent structure.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: INDUSTRIAL POLICY BUREAUCRACY (PITON) — Maintains reshoring administration (CHIPS Act implementation, investment reviews, subsidy distribution) as performative ritual despite declining actual reshoring impact. Theater ratio high: extensive planning and monitoring activity produces limited manufacturing movement. Persists through institutional inertia and career structures within the policy apparatus.
constraint_indexing:constraint_classification(us_manufacturing_reshoring, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / COMPARATIVE ADVANTAGE VIEW (MOUNTAIN) — From a civilizational economic view, factor-cost arbitrage and capital mobility are immutable features of markets: manufacturing will flow to lowest-cost, highest-skill locations regardless of policy. Reshoring efforts appear to violate fundamental economic laws. However, structural data contradicts the mountain classification — this naturalizes what are contingent institutional choices (trade rules, capital controls, labor mobility regimes).
constraint_indexing:constraint_classification(us_manufacturing_reshoring, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_manufacturing_reshoring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_manufacturing_reshoring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_manufacturing_reshoring, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_manufacturing_reshoring, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_manufacturing_reshoring, TR),
    TR >= 0.70.

:- end_tests(us_manufacturing_reshoring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reshoring regime creates genuine coordination benefits (supply chain diversification, critical capacity maintenance, labor market support in specific regions) but these are substantially outweighed by extraction mechanisms: consumer price increases from tariffs, barriers facing non-politically-connected firms, inefficiency from forced geographic concentration, and externalized costs to overseas workers. The rising trajectory (0.42→0.58) indicates that accumulating policies and enforcement intensity are increasing extraction faster than coordination benefits are materializing. Suppression (0.65): High. Multiple barriers limit agent exit: tariffs and quotas constrain consumer choice and firm input access; investment reviews restrict capital allocation; overseas workers have no voice in reshoring decisions; non-connected firms face regulatory barriers. Suppression is structural rather than temporary. Theater ratio (0.68): Moderately high. Extensive policy administration (CHIPS Act implementation, Committee on Foreign Investment reviews, subsidy distribution machinery) produces disproportionate planning activity relative to actual manufacturing capacity gains. Policy announcements of reshoring often precede actual investment by years or prove ephemeral.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap appears between politically connected corporations (Rope: benefits flow to them) and overseas production workers (Snare: pure extraction). Both face the same policy constraint, but their structural relationships are opposite. Consumers see snare; beneficiaries see rope. The tangled_rope consensus (from moderate and some organizational perspectives) represents the true structural mixed function: genuine coordination (supply chain resilience, labor market transition support) layered with asymmetric extraction (corporate subsidy capture, consumer cost increases, overseas worker displacement). The piton perspective (institutional policy bureaucracy maintaining performative review processes) captures the degradation pathway — as subsidy capture concentrates and actual manufacturing gains stagnate, the policy apparatus persists through institutional inertia rather than functional necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural data for directionality derivation: (1) Primary beneficiaries are politically connected corporations and unions — these declare benefits flowing toward them, constraining d toward 0.0–0.25 range for institutional/organized agents with arbitrage/mobile exit options. (2) Primary victims are consumers, non-connected firms, and overseas workers — these declare costs flowing toward them, expanding d toward 0.75–0.95 range depending on exit mobility. (3) Secondary ambiguity: are unions beneficiaries or manipulated victims? Analysis shows they are genuine beneficiaries in unionized sectors with negotiating power, though non-unionized workers bear more of the extraction burden. Union directionality: institutional/organized + arbitrage/mobile + genuine benefit flow → d ≈ 0.45–0.55. No overrides needed — the natural derivation captures the structural asymmetry. The high variance in d across agent groups (0.15 to 0.95) confirms mixed extraction mechanisms operating simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves mandatrophy by demonstrating that tangled_rope is structurally accurate and stable. The tension is real: genuine coordination function (supply chain diversification, critical capacity) coexists with asymmetric extraction (corporate subsidy concentration, consumer cost increases, worker displacement). The classification avoids false naturalization (the mountain perspective) by showing that reshoring is policy-contingent, not economic-law-contingent. It avoids false symmetry (rope or pure coordination) by documenting the victim groups and asymmetric benefit distribution. The rising extractiveness trajectory (0.42→0.58) indicates a degradation pathway where extraction mechanisms are accumulating faster than coordination benefits materialize — this suggests a potential reclassification toward snare if the trajectory continues, but current structure supports tangled_rope with active enforcement and documented beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reshoring_authenticity_threshold,
    'What constitutes genuine reshoring versus policy theater masquerading as manufacturing return?',
    'Tracking actual jobs created vs capital invested; distinguishing assembly operations (low value-add, vulnerable to subsequent offshoring) from design and production capacity (higher value-add); measuring job quality and duration stability',
    'If reshoring is primarily assembly and temporary: extractiveness rises above 0.58 (pure extraction with minimal genuine coordination benefit). If reshoring establishes durable production capacity: extractiveness falls below 0.50 (genuine coordination function validated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reshoring_authenticity_threshold, empirical, 'Authenticity of reshoring versus policy theater').

omega_variable(
    subsidy_incidence_distribution,
    'Do reshoring subsidies actually reach manufacturing workers and small suppliers, or do they concentrate in politically connected corporate hands?',
    'Audit of subsidy distribution patterns; tracking job creation per dollar of subsidy; mapping capital investment to wage growth in beneficiary regions',
    'If concentrated in corporate/political hands: suppression and extraction metrics rise; constraint degrades toward pure snare. If distributed to workers and small suppliers: coordination function validated; constraint remains tangled_rope or improves toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_incidence_distribution, empirical, 'Subsidy distribution between corporate and worker beneficiaries').

omega_variable(
    global_supply_chain_efficiency_cost,
    'How much total economic value is lost due to forced geographic concentration and supply chain inefficiency in the reshoring regime?',
    'Input-output analysis comparing optimal global supply chains to policy-constrained domestic chains; measurement of logistics costs, inventory carrying costs, and innovation lag',
    'If efficiency loss exceeds 3% of manufacturing GDP: cost to consumers and non-connected firms is severe; snare perspective dominates. If efficiency loss < 1%: coordination benefits more plausibly outweigh extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_supply_chain_efficiency_cost, empirical, 'Economic efficiency losses from geographic concentration').

omega_variable(
    alternative_industrial_policy_pathways,
    'Could the coordination goals (stable manufacturing capacity, supply chain resilience, worker income) be achieved through less extractive mechanisms (R&D investment, education, infrastructure) rather than protectionist reshoring?',
    'Comparison with peer economies using different industrial policy instruments; historical analysis of manufacturing regions that built durable capacity without protectionism',
    'If alternatives exist and are comparable/superior: reshoring constraint is choice-contingent (could be replaced), not structural necessity. Reclassifies toward snare (exposure mechanism shifts from market failure to policy choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_industrial_policy_pathways, conceptual, 'Whether less extractive industrial policy alternatives exist').

omega_variable(
    identity_lock_in_manufacturing_politics,
    'How much of reshoring support is driven by genuine economic analysis versus identity fusion with ''American manufacturing'' as national identity?',
    'Political rhetoric analysis; voter preference studies separating cost-benefit reasoning from identity markers; behavioral experiments on reshoring support with/without national framing',
    'If identity-driven: organized beneficiaries (unions, politicians) experience constraint differently (see rope/scaffold) than materially-motivated agents; identity_locked exit option becomes relevant for policy-capture analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_manufacturing_politics, conceptual, 'Identity-driven versus materially-driven support for reshoring').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_manufacturing_reshoring, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usm_tr_t0, us_manufacturing_reshoring, theater_ratio, 0, 0.55).
narrative_ontology:measurement(usm_tr_t5, us_manufacturing_reshoring, theater_ratio, 5, 0.62).
narrative_ontology:measurement(usm_tr_t10, us_manufacturing_reshoring, theater_ratio, 10, 0.68).
narrative_ontology:measurement(usm_tr_t15, us_manufacturing_reshoring, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(usm_be_t0, us_manufacturing_reshoring, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(usm_be_t5, us_manufacturing_reshoring, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(usm_be_t10, us_manufacturing_reshoring, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(usm_be_t15, us_manufacturing_reshoring, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_manufacturing_reshoring, resource_allocation).
narrative_ontology:affects_constraint(us_manufacturing_reshoring, global_supply_chain_vulnerability).
narrative_ontology:affects_constraint(us_manufacturing_reshoring, semiconductor_production_capacity).
narrative_ontology:affects_constraint(us_manufacturing_reshoring, labor_market_geographic_concentration).

% DUAL FORMULATION NOTE:
% Reshoring policy is downstream of global trade regime and comparative advantage dynamics, but represents a distinct structural constraint with its own extractiveness and beneficiary/victim structure. The upstream constraints (semiconductor supply, supply chain vulnerability) have their own ε values reflecting empirical scarcity; reshoring has its own ε reflecting policy-induced extraction. Decomposition note: if empirical analysis reveals that actual reshoring is primarily assembly (low value-add, theater) rather than durable production capacity, a separate story would model the distinction with higher theater_ratio and potentially reclassify toward piton.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
