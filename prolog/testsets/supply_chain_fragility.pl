% ============================================================================
% CONSTRAINT STORY: supply_chain_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supply_chain_fragility, []).

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
 *   constraint_id: supply_chain_fragility
 *   human_readable: Supply Chain Fragility and Systemic Extraction
 *   domain: economic/logistics/infrastructure
 *
 * SUMMARY:
 *   Supply chain fragility represents a structural constraint where the
 *   pursuit of operational efficiency through just-in-time logistics,
 *   inventory minimization, and supplier consolidation has created a system
 *   with minimal slack and high vulnerability to disruption. This constraint
 *   exhibits dual nature: it is both a genuine coordination mechanism solving
 *   real problems of inventory cost and demand matching, and an extraction
 *   mechanism that transfers risk asymmetrically from large retailers and
 *   financial intermediaries to small suppliers and dependent nations. The
 *   constraint classification varies dramatically across perspectives because
 *   different actors experience different mixes of benefit and cost. Large
 *   retailers experience it as pure coordination; small suppliers experience
 *   it as a snare with no exit; consumer bases experience it as mixed
 *   coordination with periodic extraction through shortages; export-dependent
 *   nations experience it as generational lock-in with both genuine market
 *   access and structural dependency. The extractiveness value has increased
 *   over the interval as firms have progressively eliminated redundancy,
 *   pushed inventory costs upstream to suppliers, and concentrated logistics
 *   infrastructure. The theater ratio remains moderate because the constraint
 *   has genuine functional content (actual demand coordination) alongside
 *   degrading redundancy protocols.
 *
 * KEY AGENTS:
 *   - Large Retailers: Primary beneficiary (institutional/arbitrage) — capture efficiency gains through just-in-time, shift inventory and shortage costs to suppliers, maintain exit options through multi-sourcing at scale
 *   - Small Suppliers: Primary victim (powerless/trapped) — locked into retailer demand, bear inventory risk and payment delays, have no alternative distribution channels
 *   - Consumers: Secondary victim (moderate/constrained) — benefit from lower retail prices but bear cost of periodic shortages and supply disruptions; constrained by limited retail alternatives in concentrated markets
 *   - Export-Dependent Nations: Organized victim (organized/constrained) — integrated into global supply chains with genuine market access but structural vulnerability to upstream disruptions and wage suppression
 *   - Financial Intermediaries: Beneficiary (institutional/arbitrage) — profit from supply chain financing, inventory factoring, and risk instruments that monetize supplier vulnerability
 *   - Reshoring Coalition: Organized actor (organized/mobile) — policy makers, domestic manufacturers, labor unions building alternative supply structures with sunset logic
 *   - Redundancy Theater: Institutional mechanism (institutional/arbitrage) — insurance and regulatory compliance requirements maintain fiction of supply chain resilience despite operational pressure to eliminate actual redundancy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supply_chain_fragility, 0.58).
domain_priors:suppression_score(supply_chain_fragility, 0.65).
domain_priors:theater_ratio(supply_chain_fragility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supply_chain_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(supply_chain_fragility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supply_chain_fragility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supply_chain_fragility, tangled_rope).
narrative_ontology:human_readable(supply_chain_fragility, "Supply Chain Fragility and Systemic Extraction").
narrative_ontology:topic_domain(supply_chain_fragility, "economic/logistics/infrastructure").

domain_priors:requires_active_enforcement(supply_chain_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supply_chain_fragility, consolidating_logistics_firms).
narrative_ontology:constraint_beneficiary(supply_chain_fragility, large_retailers).
narrative_ontology:constraint_beneficiary(supply_chain_fragility, financial_intermediaries).
narrative_ontology:constraint_victim(supply_chain_fragility, small_suppliers).
narrative_ontology:constraint_victim(supply_chain_fragility, consumers).
narrative_ontology:constraint_victim(supply_chain_fragility, dependent_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL SUPPLIER (SNARE) — Trapped in dependency on major retailers' ordering systems with no alternative distribution channels. Bears full cost of demand volatility, payment delays, and inventory risk. No exit option exists; supplier is locked into asymmetric contractual terms.
constraint_indexing:constraint_classification(supply_chain_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL CONSUMER BASE (TANGLED ROPE) — Benefits from just-in-time efficiency (lower retail prices) but bears cost of supply disruptions. Constrained by limited alternative retail options in concentrated markets. Moderate power through collective consumer action, but coordinating withdrawal is costly.
constraint_indexing:constraint_classification(supply_chain_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE RETAILER (ROPE) — Experiences the constraint as pure coordination: inventory optimization, demand signaling, supplier diversification create mutual benefit with suppliers. Has exit options (alternative suppliers, vertical integration); constraint is a negotiated settlement beneficial to both parties at macro level, though asymmetric at micro level.
constraint_indexing:constraint_classification(supply_chain_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXPORT-DEPENDENT NATION (TANGLED ROPE) — Organized actor constrained by integration into global supply chains. Genuine coordination function exists (access to markets, employment, technology transfer) but paired with structural extraction: dependency on external demand, vulnerability to upstream disruptions, wage suppression relative to productivity. Real but asymmetric benefit.
constraint_indexing:constraint_classification(supply_chain_fragility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE REDUNDANCY THEATER (PITON) — Supply chain resilience protocols (safety stock policies, dual sourcing, contingency plans) are increasingly performative. Firms maintain these on paper while operational pressure incentivizes just-in-time elimination in practice. Theater ratio reflects the gap between stated redundancy strategy and actual implementation; the institutional mechanism persists through regulatory compliance and insurance requirements despite degraded function.
constraint_indexing:constraint_classification(supply_chain_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE RESHORING MOVEMENT (SCAFFOLD) — Organized actors (policy makers, domestic manufacturers, labor coalitions) are building alternative supply chain structures through reshoring incentives, regional manufacturing hubs, and supplier diversification mandates. This represents a temporary coordination mechanism with a sunset: as domestic capacity matures and regional supply chains stabilize, the global fragility constraint loses force. High suppression now but declining trajectory as alternatives mature.
constraint_indexing:constraint_classification(supply_chain_fragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a systems perspective, supply chain fragility appears as an immutable law: any sufficiently optimized complex system trades robustness for efficiency, and perfect resilience is economically impossible. Specialization and comparative advantage necessarily create dependencies. However, this naturalizes what is actually a policy choice: the fragility emerges from specific institutional arrangements (just-in-time optimization mandates, financial incentives for asset-light models, lack of regulatory resilience requirements), not from physics.
constraint_indexing:constraint_classification(supply_chain_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supply_chain_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supply_chain_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supply_chain_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supply_chain_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supply_chain_fragility, TR),
    TR >= 0.70.

:- end_tests(supply_chain_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The baseline extractiveness has increased from 0.35 to 0.58 over the 20-year interval as firms systematized the elimination of redundancy and shifted operational risk upstream. The metric reflects the magnitude of costs borne by small suppliers (payment delays averaging 30-60 days, inventory risk absorption, demand volatility absorption) relative to benefits captured by large retailers (inventory turnover advantages, working capital optimization). Suppression (0.65): High. Small suppliers face severe barriers to exit: switching costs are substantial (retailer-specific systems, sunk relationship investment), alternative distribution channels are limited in consolidated retail markets, and the threat of delisting creates strong compliance incentives. Consumers face information asymmetries and collective action problems in coordinating retail switching. Export-dependent nations face lock-in through infrastructure and capital investment. Theater ratio (0.48): Moderate. The constraint has genuine functional content (actual demand coordination) but is paired with increasing performative redundancy — firms maintain safety stock policies on paper while operational metrics incentivize just-in-time elimination in practice. Insurance and regulatory compliance drive some residual redundancy, but it is increasingly theatrical as actual capacity is stripped away.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence between beneficiary and victim perspectives. The large retailer (institutional/arbitrage, immediate/global) sees a coordination mechanism with net-positive benefits to both parties. The small supplier (powerless/trapped, biographical/global) sees pure extraction with no exit. This gap indicates that the constraint's structure is genuinely hybrid: the coordination function is real and benefits the retailer, but the implementation distributes costs asymmetrically. The scope modifier amplifies this gap: at global scope, large retailers' arbitrage options compound while small suppliers' trapped status is reinforced by inability to shift to alternative regional chains. The reshoring perspective (scaffold) suggests the gap could narrow over time as alternative supply structures mature, giving small suppliers mobile exit options. The piton perspective indicates that the gap is widening in the short term as redundancy theater declines and actual operational pressure increases toward just-in-time elimination.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declaration drives directionality: large retailers are primary beneficiaries of efficiency gains (payment term benefits, inventory turnover, working capital optimization); small suppliers, consumers, and dependent nations are victims of risk-shifting. Financial intermediaries are secondary beneficiaries (profiting through supply chain financing and risk instruments). The exit options differentiate beneficiaries from victims: large retailers have arbitrage options (multiple suppliers, vertical integration, automation) while small suppliers have trapped options (switching costs, delisting threat, limited alternatives). This differentiation produces the directional gap: beneficiaries with arbitrage have d ≈ 0.05-0.15, victims with trapped/constrained options have d ≈ 0.60-0.92. The scope modifier σ(S) = 1.2 at global scope amplifies the effective extraction chi, because larger-scope disruptions (global pandemic affecting container shipping, geopolitical conflict disrupting semiconductor supply) create higher-stakes fragility and greater extraction asymmetry. Overrides are not needed — the derivation chain captures the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The supply chain fragility constraint resolves mandatrophy by separating the coordination function (genuine demand matching) from the extraction mechanism (risk shifting and redundancy elimination). The constraint is a tangled rope at the analytical level because both functions are structurally necessary: the large retailer genuinely needs supply coordination, AND the small supplier is genuinely being extracted from through asymmetric risk allocation. The mandatrophy is not 'which is correct?' but 'how do we decompose the real coordination from the extractive overhead?' The large retailer's rope perspective is structurally valid for their experience — they do experience the constraint as coordination. But this does not invalidate the small supplier's snare perspective or the analytical observer's tangled rope assessment. The constraint's extractiveness (0.58) reflects that a significant portion of the coordination could be achieved with lower asymmetric cost-shifting — the extraction is not inherent to coordination but embedded in how the coordination is structured (payment terms, inventory responsibility allocation, contract terms). This is precisely the structure that the tangled rope type is designed to detect: genuine coordination function plus asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_resilience_tradeoff_bound,
    'Is the efficiency-resilience tradeoff fundamental to supply networks or is it an artifact of current financial incentive structures?',
    'Comparative analysis of supply chain design in different regulatory regimes; examination of systems with explicit resilience constraints (military procurement, critical infrastructure) versus cost-optimized systems',
    'If fundamental: many constraints should reclassify toward mountain. If incentive-driven: the fragility is extractive institutional choice, not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_resilience_tradeoff_bound, empirical, 'Whether efficiency-resilience tradeoff is fundamental or policy-driven').

omega_variable(
    power_asymmetry_enforcement_mechanism,
    'Does the large retailer''s power over suppliers derive from their market position or from their ability to shift risk through contractual terms and payment delays?',
    'Analysis of supplier profit margins, payment cycles, and contract terms across market structures; comparison of firms with and without market power but similar operational needs',
    'If structural market position: extraction is endogenous to scale. If enforcement through risk-shifting: extraction is a policy choice and could be reduced through contract regulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_enforcement_mechanism, empirical, 'Whether asymmetric power derives from market position or contractual enforcement').

omega_variable(
    redundancy_collapse_threshold,
    'At what point does elimination of redundancy (safety stock, backup suppliers, excess capacity) become systemically dangerous rather than merely operationally risky?',
    'Network simulation and historical failure analysis; correlation between redundancy levels and cascade failure rates across industries',
    'If threshold is well-defined and currently breached: the constraint should classify toward snare for dependent actors. If threshold is fuzzy: actors can claim system is still resilient and maintain fragility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redundancy_collapse_threshold, empirical, 'Threshold at which redundancy elimination becomes systemically dangerous').

omega_variable(
    alternative_supply_chain_viability,
    'Can reshoring and regional supply chains provide comparable cost and availability to global just-in-time networks, or is the fragility the price of accessing the global cost advantage?',
    'Cost analysis of regional supply chains under mature development; tracking of reshoring initiatives'' actual cost-competitiveness versus imported goods; assessment of whether resilience gains offset lost comparative advantage',
    'If regionally viable: scaffold sunset is real and extractive fragility can be escaped. If cost penalty is substantial: fragility is an ongoing extraction of resilience from dependent actors for global consumer benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_chain_viability, empirical, 'Whether regional supply chains can match cost-competitiveness of global just-in-time').

omega_variable(
    small_supplier_coalition_capacity,
    'Can small suppliers organize collective bargaining power sufficient to negotiate away the asymmetric terms that create their trapped exit status?',
    'Analysis of supplier associations, purchasing cooperatives, and collective contracting mechanisms; comparison of organized versus unorganized supplier power in different industries and geographies',
    'If capacity exists: powerless classification may upgrade to organized under coalition conditions, changing the snare perspective. If organizing is prevented by law or contract: suppression is structural and snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_supplier_coalition_capacity, empirical, 'Whether small suppliers can achieve collective bargaining power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supply_chain_fragility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scf_tr_t0, supply_chain_fragility, theater_ratio, 0, 0.3).
narrative_ontology:measurement(scf_tr_t10, supply_chain_fragility, theater_ratio, 10, 0.4).
narrative_ontology:measurement(scf_tr_t20, supply_chain_fragility, theater_ratio, 20, 0.48).
narrative_ontology:measurement(scf_tr_t5, supply_chain_fragility, theater_ratio, 5, 0.35).

% Extraction over time
narrative_ontology:measurement(scf_be_t0, supply_chain_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scf_be_t10, supply_chain_fragility, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(scf_be_t20, supply_chain_fragility, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(scf_be_t5, supply_chain_fragility, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supply_chain_fragility, resource_allocation).
narrative_ontology:affects_constraint(supply_chain_fragility, semiconductor_supply_concentration).
narrative_ontology:affects_constraint(supply_chain_fragility, just_in_time_redundancy_elimination).
narrative_ontology:affects_constraint(supply_chain_fragility, port_infrastructure_consolidation).

% DUAL FORMULATION NOTE:
% Supply chain fragility decomposes into three structurally distinct constraints: (1) resource allocation coordination (genuine demand matching), (2) redundancy elimination (policy choice about acceptable risk), and (3) supplier power asymmetry (contractual extraction mechanism). This story captures the bundle; individual stories could decompose each element separately with distinct epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supply_chain_fragility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
