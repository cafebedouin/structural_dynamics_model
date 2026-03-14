% ============================================================================
% CONSTRAINT STORY: cross_border_manufacturing_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_border_manufacturing_coupling, []).

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
 *   constraint_id: cross_border_manufacturing_coupling
 *   human_readable: Cross-Border Manufacturing Supply Chain Coupling
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Cross-border manufacturing supply chains represent one of the most
 *   complex global coordination mechanisms: they distribute production across
 *   multiple countries, align just-in-time logistics, coordinate quality
 *   standards, and synchronize complex product assembly. Yet they
 *   simultaneously create structural extraction through dependency locks,
 *   wage suppression, regulatory arbitrage, and power asymmetries between
 *   lead manufacturers and peripheral suppliers. The constraint exhibits a
 *   dual nature: genuine coordination value coexists with genuine extraction.
 *   Lead manufacturing hubs (institutional beneficiaries with arbitrage
 *   options) experience it as pure Rope — a coordination mechanism that
 *   enables specialization and lowers consumer prices. Peripheral suppliers
 *   and labor forces (powerless agents with trapped exit) experience it as
 *   Snare — dependency that extracts through wage suppression, technology
 *   lock-in, and coercive contracting. Intermediate suppliers experience
 *   Tangled Rope — real knowledge transfer and value-addition alongside
 *   enforced standards and extraction. The constraint's extractiveness has
 *   increased from 0.38 to 0.52 over the interval as manufacturing complexity
 *   has grown, wage gaps have widened despite globalization, and peripheral
 *   regions have deepened their dependency. Theater ratio has also risen,
 *   reflecting increasing performative compliance (labor audits,
 *   sustainability certifications) that persist regardless of actual
 *   conditions. Trade governance institutions and labor standards coalitions
 *   are creating a sunset pathway, but its realism depends on whether
 *   automation benefits peripheral regions or further concentrates gains.
 *
 * KEY AGENTS:
 *   - Lead Manufacturing Hubs: Primary beneficiaries (institutional/arbitrage) — orchestrate supply chains, set specifications, control final products, capture largest margin. Can relocate, shift suppliers, or reshape chains at will.
 *   - Peripheral Suppliers: Primary victims (powerless/trapped) — locked into specifications, pricing, technology requirements set by lead hubs. Capital investments are sunk; alternative markets are unavailable or substantially lower-margin.
 *   - Labor Forces in Dependent Regions: Primary victims (powerless/trapped) — dependent on manufacturing employment with no alternative income sources. Wage suppression, unsafe conditions, lack of regulatory protection. Extraction through geographic and economic constraints.
 *   - Intermediate Tier Suppliers: Secondary victims (organized/constrained) — benefit from supply chain access and technical knowledge but face enforced standardization, IP constraints, and vertical integration pressure. Higher power than peripheral suppliers but still constrained.
 *   - Consumer Markets: Beneficiaries (institutional/arbitrage) — benefit from low prices enabled by extraction elsewhere in chain. Have switching options and no suppression.
 *   - Trade Governance Institutions: Organized actors (organized/constrained) — labor standards enforcement, transparency initiatives, and trade bloc labor requirements creating alternative pathways. See the constraint as having a sunset.
 *   - National Trade Barriers & Regulatory Apparatus: Institutional actors maintaining performative enforcement (institutional/arbitrage) — tariffs and regulations persist through inertia even as their functional effect degrades and supply chains route around them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_border_manufacturing_coupling, 0.52).
domain_priors:suppression_score(cross_border_manufacturing_coupling, 0.48).
domain_priors:theater_ratio(cross_border_manufacturing_coupling, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_border_manufacturing_coupling, extractiveness, 0.52).
narrative_ontology:constraint_metric(cross_border_manufacturing_coupling, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cross_border_manufacturing_coupling, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_border_manufacturing_coupling, tangled_rope).
narrative_ontology:human_readable(cross_border_manufacturing_coupling, "Cross-Border Manufacturing Supply Chain Coupling").
narrative_ontology:topic_domain(cross_border_manufacturing_coupling, "economic/geopolitical").

domain_priors:requires_active_enforcement(cross_border_manufacturing_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_border_manufacturing_coupling, lead_manufacturing_hubs).
narrative_ontology:constraint_beneficiary(cross_border_manufacturing_coupling, consumer_markets).
narrative_ontology:constraint_victim(cross_border_manufacturing_coupling, peripheral_suppliers).
narrative_ontology:constraint_victim(cross_border_manufacturing_coupling, labor_forces_in_dependent_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL SUPPLIERS (SNARE) — Locked into dependency through capital investment requirements, technology specifications, and contractual obligations. Cannot exit without catastrophic loss. Extraction is maximal: low wages, limited negotiating power, vulnerability to sudden contract termination, coercive quality demands, and price pressure that leaves no margin.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR FORCES (SNARE) — Trapped by geographic and economic constraints; manufacturing jobs are the primary income source with no alternative employment. Extraction through wage suppression, unsafe working conditions, and lack of regulatory protection. Suppression mechanisms include geographic isolation, poverty traps, and absence of enforcement.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERMEDIATE SUPPLIERS (TANGLED ROPE) — Mid-tier manufacturers benefit from access to supply chains and technical knowledge transfer; coordinated value-addition is genuine. But extraction occurs through enforced standardization, intellectual property constraints, technology lock-in, and forced vertical integration. High suppression but real coordination function.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEAD MANUFACTURING HUBS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: assembling components across borders, managing logistics, synchronizing production. Can arbitrage between suppliers, relocate operations, or reshape supply chains. Low experienced extraction — constraint subsidizes their operations.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER MARKETS (ROPE) — Benefit from low prices enabled by extraction elsewhere in the chain. Have arbitrage options (alternative suppliers, shifting demand). No suppression from their perspective; they experience only coordination benefits.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADE GOVERNANCE & LABOR STANDARDS COALITIONS (SCAFFOLD) — International labor standards enforcement, supply chain transparency initiatives, and trade bloc labor requirements create an exit pathway from pure extraction. The constraint has a sunset: as labor standards harmonization, nearshoring, and automation reduce peripheral labor dependency, the extraction mechanism loses force. High suppression now but declining over generational horizon as norms mature.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: NATIONAL TRADE BARRIERS (PITON) — Tariffs, quotas, and regulatory requirements that enforce the coupling are increasingly performative. Global supply chains route around them; regulations become theatrical compliance (impact audits, certifications) while extraction continues. Theater ratio high because enforcement cannot keep pace with supply chain complexity. Barrier structures persist through institutional inertia even as their functional effect degrades.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global/civilizational vantage, cross-border manufacturing coupling genuinely coordinates value-chain efficiency and enables specialization across comparative advantage. But it simultaneously extracts through wage suppression, labor arbitrage, and regulatory capture that concentrate gains upstream. Both functions are structural, not contingent. The constraint cannot collapse to pure Rope without losing its coordination efficiency; cannot collapse to pure Snare without losing the supply-chain logic that justifies it.
constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_border_manufacturing_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_border_manufacturing_coupling, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_border_manufacturing_coupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_border_manufacturing_coupling, TR),
    TR >= 0.70.

:- end_tests(cross_border_manufacturing_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting real value extraction through wage suppression, dependency lock-in, and regulatory arbitrage. The value is elevated (vs. 0.38 historically) because peripheral regions have deepened manufacturing specialization, increasing exit costs. The increase from 0.38 to 0.52 over the interval tracks rising wage gaps and supply-chain complexity that magnify asymmetries. Suppression (0.48): Moderate, reflecting significant but not total barriers to exit. Peripheral suppliers face high costs (sunk capital, technical dependence, contract lock-in) but some alternatives exist (unionization, regionalization, supplier defection, domestic supply chains). Labor forces face higher suppression (geographic isolation, poverty traps, lack of alternatives) but some mobility exists. Theater ratio (0.55): Moderate, reflecting performative compliance mechanisms (labor audits, sustainability certifications, impact reporting) that proliferate while actual extraction continues. The increase from 0.35 to 0.55 reflects growth in theatrical certification requirements alongside persistent wage suppression and poor conditions. Theater is not dominant (0.55 < 0.70) because the constraint does have real coordination content — supply chains genuinely function, logistics genuinely synchronize, quality standards genuinely enforce.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The lead manufacturing hub sees Rope (pure coordination enabling specialization and efficiency). The peripheral supplier sees Snare (pure extraction through locked dependency). The intermediate supplier sees Tangled Rope (genuine value-addition alongside enforced asymmetry). The labor force sees Snare (trapped extraction with minimal coordination benefit). The consumer market sees Rope (low prices through coordination). The trade governance coalition sees Scaffold (sunset via labor standards and nearshoring). The national trade barrier apparatus sees Piton (performative enforcement of rules that routes around enforcement). The analytical observer sees Tangled Rope (both functions genuinely structural). All perspectives derive from the same base metrics but produce divergent classifications because their exit options and structural relationships differ fundamentally. No perspective is 'wrong' — each is a legitimate reading from its position.
 *
 * DIRECTIONALITY LOGIC:
 *   Lead manufacturing hubs (institutional/arbitrage) derive low d values: they benefit from the constraint and can exit at will. The beneficiary status + arbitrage exit option yields d ≈ 0.10, producing negative χ values (constraint subsidizes them). Peripheral suppliers (powerless/trapped) derive high d values: they bear costs and cannot exit. Victim status + trapped exit option yields d ≈ 0.95, producing high f(d) ≈ 1.42 (maximum experienced extraction). Intermediate suppliers (organized/constrained) occupy middle ground: they have some coordination benefits and some agency, but face real costs and constrained exit. Victim status + constrained exit yields d ≈ 0.55, producing moderate f(d) ≈ 0.75. The analytical observer at civilizational scope derives d ≈ 0.72 (balanced perception of coordination and extraction). Scope amplification (global σ = 1.2) means the constraint's extractiveness is magnified at larger scales: local supplier networks might show χ = 0.30 (tangled rope), but global coupling amplifies this to χ ≈ 0.62 (snare threshold).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED BY PERSPECTIVAL PLURALISM: The constraint cannot be collapsed to a single type without erasure. A pure Rope classification would require ignoring the powerless agents' actual experience of extraction and trap. A pure Snare classification would require denying the genuine coordination value that enables global specialization and efficiency. The Tangled Rope classification from the analytical perspective captures the structural dual nature: the constraint genuinely coordinates value-chain efficiency AND genuinely extracts through asymmetric dependencies. The perspectives collectively resolve the mandatrophy by showing that all six types are legitimate readings from different structural positions. The key mandatrophy insight: the constraint does not collapse to a single type because its functions are genuinely dual — the coordination and extraction are not a ratio but two simultaneous structural effects. The sunset mechanism (labor standards, nearshoring, automation) is real but slow — decades timescale — and its outcome depends on whether automation in peripheral regions reduces extraction (supporting Scaffold) or concentrates gains (supporting persistent Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_proportionality,
    'What fraction of the measured extractiveness (0.52) is necessary coordination cost vs. pure rental extraction?',
    'Counter-factual analysis: compare labor costs in coupled supply chains vs. labor costs in equivalent manufacturing where suppliers had negotiating parity. Identify extractiveness that persists even under fair-wage scenarios.',
    'If necessary coordination > 0.40: constraint may downgrade to Rope. If necessary coordination < 0.20: constraint may upgrade to Snare. Current 0.52 implies ~0.25 necessary coordination cost, ~0.27 pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_proportionality, empirical, 'Proportion of extractiveness attributable to coordination necessity vs. pure extraction').

omega_variable(
    exit_possibility_for_periphery,
    'Are peripheral suppliers genuinely trapped, or do constrained alternatives exist (domestic supply chains, unionization, geographic relocation)?',
    'Case studies of supplier defection; analysis of successful supplier transitions to independent status or new supply chains; labor mobility data for regions with manufacturing dominance.',
    'If genuine exit options exist: reclassify powerless victims as moderate agents (constrained rather than trapped). Snare perspectives degrade toward Tangled Rope. If exit requires organized coalition action: reclassify to organized power + constrained exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_possibility_for_periphery, empirical, 'Whether peripheral suppliers have genuine exit options beyond the coupling').

omega_variable(
    labor_standards_sunset_realism,
    'Are trade-linked labor standards and nearshoring genuinely creating a sunset mechanism, or are they performative cover for persistent coupling?',
    'Tracking of wage convergence between developed and developing manufacturing hubs; measurement of actual nearshoring investment vs. rhetorical commitment; compliance enforcement rates for labor standards provisions in trade agreements.',
    'If sunset real: Scaffold classification holds. If performative: scaffold is false hope, and constraint is more durable Snare/Tangled Rope than horizonal optimism suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_standards_sunset_realism, empirical, 'Whether labor standards harmonization and nearshoring represent genuine sunset mechanism').

omega_variable(
    automation_impact_trajectory,
    'Does automation in peripheral manufacturing regions reduce extraction (by eliminating low-wage labor arbitrage) or intensify it (by concentrating gains further upstream)?',
    'Historical analysis of automation deployment in prior dependent-region manufacturing; measurement of wage/employment effects; tracking of automation investment sources (lead hubs vs. peripheral regions).',
    'If automation benefits peripheral regions: extraction mechanism decays over time, supporting Scaffold sunset logic. If automation concentrates gains: extraction intensifies even as labor dependency shrinks — constraint becomes more Snare-like despite lower suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(automation_impact_trajectory, empirical, 'Whether automation reduces or intensifies extraction in dependent manufacturing regions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_border_manufacturing_coupling, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbmc_tr_t0, cross_border_manufacturing_coupling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cbmc_tr_t10, cross_border_manufacturing_coupling, theater_ratio, 10, 0.48).
narrative_ontology:measurement(cbmc_tr_t20, cross_border_manufacturing_coupling, theater_ratio, 20, 0.55).
narrative_ontology:measurement(cbmc_tr_t30, cross_border_manufacturing_coupling, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(cbmc_be_t0, cross_border_manufacturing_coupling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cbmc_be_t10, cross_border_manufacturing_coupling, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cbmc_be_t20, cross_border_manufacturing_coupling, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cbmc_be_t30, cross_border_manufacturing_coupling, base_extractiveness, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_border_manufacturing_coupling, resource_allocation).
narrative_ontology:affects_constraint(cross_border_manufacturing_coupling, supply_chain_vulnerability).
narrative_ontology:affects_constraint(cross_border_manufacturing_coupling, labor_standard_harmonization).
narrative_ontology:affects_constraint(cross_border_manufacturing_coupling, regional_manufacturing_dependence).

% DUAL FORMULATION NOTE:
% Cross-border manufacturing coupling decomposes into three related constraints: (1) supply chain vulnerability (geopolitical risk), (2) labor standard harmonization (extraction mechanism), and (3) regional manufacturing dependence (structural entrenchment). This story captures the coupling constraint itself — the structural lock-in mechanism. The three downstream constraints reflect different perspectives on how the coupling manifests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cross_border_manufacturing_coupling, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
