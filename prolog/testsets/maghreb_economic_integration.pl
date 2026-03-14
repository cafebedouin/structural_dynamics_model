% ============================================================================
% CONSTRAINT STORY: maghreb_economic_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maghreb_economic_integration, []).

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
 *   constraint_id: maghreb_economic_integration
 *   human_readable: Maghreb Economic Integration Constraint
 *   domain: economic_policy/regional_integration
 *
 * SUMMARY:
 *   Maghreb economic integration represents a constraint on cross-border
 *   economic activity nominally designed to coordinate regional growth but
 *   functionally operating as an asymmetric extraction mechanism protecting
 *   incumbent domestic firms and state revenue collection from regional
 *   competition. The constraint exhibits characteristics of all six DR types
 *   depending on perspective: for cross-border workers and SMEs it is a
 *   snare; for incumbent firms it is rope coordination; for integrationist
 *   coalitions it is a temporary scaffold with planned sunset; for state
 *   revenue systems it is a degraded piton; for integrationist governments it
 *   is a tangled rope requiring active political management; for the
 *   analytical observer it risks naturalizing institutional barriers as
 *   geographic immutability. The extractiveness has increased over the
 *   measurement interval (0.42 → 0.58) as informal trade has grown to 40-60%
 *   of cross-border commerce while formal tariff systems persist, suggesting
 *   the constraint's primary function has shifted from coordination to
 *   revenue protection and domestic firm shielding. Theater ratio (0.65)
 *   reflects that formal integration agreements (UEMOA protocols, Arab
 *   Maghreb Union commitments) exist and are repeatedly invoked rhetorically
 *   while actual liberalization lags substantially behind announced timelines
 *   and informal trade networks handle most regional commerce.
 *
 * KEY AGENTS:
 *   - Cross-Border Workers: Primary victim (powerless/trapped) — face visa restrictions and work permit barriers fragmenting the regional labor market
 *   - SMEs in Cross-Border Trade: Secondary victim (moderate/constrained) — bear tariff and customs costs while having some market access through integration agreements
 *   - Incumbent Domestic Firms in Protected Sectors: Primary beneficiary (institutional/arbitrage) — protected from regional competition and benefit from tariff barriers that raise input costs for export-oriented competitors
 *   - State Revenue Systems: Institutional beneficiary (institutional/arbitrage) — maintain tariff and customs revenue; also structurally dependent on these revenue streams for government financing
 *   - Regional Integration Coalition: Organized actors (organized/mobile) — UEMOA secretariat, Arab Maghreb Union, civil society organizations, NGOs pushing toward deeper integration with planned timelines
 *   - Integrationist National Governments: Multi-level agents (organized/constrained) — nominally committed to integration but constrained by domestic protectionist lobbies and revenue dependency
 *   - Informal Trade Networks: Unmodeled beneficiary — operate outside formal constraint structure; handle 40-60% of cross-border commerce but lack representation in formal integration negotiations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maghreb_economic_integration, 0.58).
domain_priors:suppression_score(maghreb_economic_integration, 0.68).
domain_priors:theater_ratio(maghreb_economic_integration, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maghreb_economic_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(maghreb_economic_integration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(maghreb_economic_integration, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maghreb_economic_integration, tangled_rope).
narrative_ontology:human_readable(maghreb_economic_integration, "Maghreb Economic Integration Constraint").
narrative_ontology:topic_domain(maghreb_economic_integration, "economic_policy/regional_integration").

domain_priors:requires_active_enforcement(maghreb_economic_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maghreb_economic_integration, import_competing_domestic_firms).
narrative_ontology:constraint_beneficiary(maghreb_economic_integration, state_revenue_protectionists).
narrative_ontology:constraint_beneficiary(maghreb_economic_integration, incumbent_industrial_sectors).
narrative_ontology:constraint_victim(maghreb_economic_integration, regional_labor_mobility).
narrative_ontology:constraint_victim(maghreb_economic_integration, cross_border_trade_actors).
narrative_ontology:constraint_victim(maghreb_economic_integration, consumer_access_efficiency).
narrative_ontology:constraint_victim(maghreb_economic_integration, smes_regional_competitiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROSS-BORDER WORKER (SNARE) — Trapped by national immigration restrictions, visa requirements, and work permit barriers that fragment the labor market. No exit options; bears full cost of wage suppression and employment scarcity within single national labor market. Maximum extraction without coordination benefit.
constraint_indexing:constraint_classification(maghreb_economic_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SME IN CROSS-BORDER TRADE (TANGLED ROPE) — Constrained by tariff barriers, customs procedures, and regulatory compliance costs, but also benefits from preferential access under partial integration agreements and market expansion opportunities. High extraction relative to benefits; constrained exit due to capital sunk in regional operations.
constraint_indexing:constraint_classification(maghreb_economic_integration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT FIRM IN PROTECTED SECTOR (ROPE) — Institutional beneficiary with arbitrage options. Benefits from tariff protection and non-tariff barriers that exclude regional competitors. Experiences constraint as coordination mechanism for maintaining market stability and predictable profit flows. Net positive extraction flow.
constraint_indexing:constraint_classification(maghreb_economic_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL INTEGRATION COALITION (SCAFFOLD) — Organized actors (UEMOA framework, AU protocols, civil society organizations) pushing toward deeper integration with sunset logic: tariff schedules have planned reduction pathways, visa protocols are incrementally liberalizing, and trade harmonization is building toward full integration over 10-15 years. Agents see the protectionist barriers as temporary obstacles to be overcome through graduated phasing.
constraint_indexing:constraint_classification(maghreb_economic_integration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: STATE REVENUE COLLECTION (PITON) — National governments maintain tariff barriers and customs procedures that generate revenue and employment in customs administration, but the primary function has degraded over time as informal trade routes have grown to 40-60% of cross-border commerce. The formal tariff system is maintained through institutional inertia — governments continue collecting formal duties on formal trade while informal networks bypass the constraint entirely. Theater ratio high because the formal system persists despite losing functional significance.
constraint_indexing:constraint_classification(maghreb_economic_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NATIONAL GOVERNMENT COMMITTED TO INTEGRATION (TANGLED ROPE) — State actors nominally committed to regional integration (signatories to UEMOA, Arab Maghreb Union protocols) face constraints from domestic protectionist lobbies and revenue dependency, but also benefit from integration through diversified export markets and GDP growth correlations. Active enforcement of integration requires managing domestic political opposition; constrained by political economy of reform.
constraint_indexing:constraint_classification(maghreb_economic_integration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, geographic distance, transportation costs, and linguistic/cultural variation create inherent friction to economic integration. This perspective naturalizes the barrier as immutable geography. However, structural data contradicts the mountain classification: the constraint is primarily institutional (tariffs, visas, regulatory divergence) not geographic — the engine will flag this as a false summit.
constraint_indexing:constraint_classification(maghreb_economic_integration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maghreb_economic_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maghreb_economic_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maghreb_economic_integration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maghreb_economic_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maghreb_economic_integration, TR),
    TR >= 0.70.

:- end_tests(maghreb_economic_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially from cross-border workers and SMEs through tariff barriers, visa restrictions, and customs procedures that enforce national market segmentation. However, it is not as severe as pure extraction (snare) because integration agreements provide partial market access for some actors and create growth opportunities for integrating firms. The measurement trajectory (0.42 → 0.58) reflects increasing extraction as informal trade has filled the gap between formal integration commitments and actual liberalization, leaving formal trade with higher effective barriers. Suppression (0.68): High. Barriers to regional trade and labor mobility include tariff structures, non-tariff barriers (standards divergence, certification requirements), visa and work permit restrictions, bureaucratic delays in customs procedures, and currency controls. Exit costs are substantial: firms must invest in separate supply chains for each national market; workers face legal barriers rather than just economic costs. Theater ratio (0.65): Moderate-high. Formal integration agreements and protocols are invoked repeatedly in diplomatic contexts and development rhetoric, but actual implementation lags significantly. Tariff reduction schedules are repeatedly delayed; visa liberalization protocols are signed but enforcement remains restrictive; customs harmonization commitments exist in legal documents but face institutional implementation gaps.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full range of DR classification from a single set of base properties. Incumbent firms protected by the constraint see rope — a coordination mechanism for stable markets and predictable returns. Integration coalitions see scaffold — temporary barriers being systematically reduced through planned timelines. State revenue systems see piton — a degraded ritual maintained by inertia despite loss of primary function. Cross-border workers see snare — no exit, full cost bearing, no coordination benefit. SMEs see tangled rope — mixed benefits (market access) and costs (barriers). Integrationist governments see tangled rope — genuine coordination function (market expansion, growth) alongside extraction (political costs of managing domestic opposition). The analytical observer risks seeing a mountain — naturalizing institutional barriers as geographic immutability — but the structural data reveals this as a false summit: the barriers are contingent institutional arrangements (tariff schedules, visa requirements, customs procedures) that could be rapidly dismantled through political will.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural position relative to the constraint. Cross-border workers face trapped exit (no legal alternative to national market fragmentation) and are victims → high d → high f(d) → maximum experienced extraction. SMEs have constrained exit (can operate within single market or invest heavily to multiply operations) and are partial victims → moderate-high d → high f(d) → significant extraction. Incumbent firms are beneficiaries with arbitrage options (can exit to other markets if regional integration threatens) → low d → negative f(d) → negative extraction (benefit). State revenue systems are institutional beneficiaries with arbitrage (can maintain barriers indefinitely or seek alternative revenue) → very low d → most negative f(d). Integrationist governments are constrained by political economy but have some mobility → moderate d. The gap between beneficiary and victim directionalities reveals the asymmetric extraction structure: those who benefit can exit the constraint easily; those who suffer from it cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   PARTIALLY RESOLVED: This constraint resolves some mandatrophy through its tangled rope primary classification, which acknowledges both the genuine coordination function (market expansion, growth generation for integrating economies) and the asymmetric extraction (protection of incumbents, suppression of cross-border mobility). However, unresolved tension remains: is the constraint primarily failing as a coordination mechanism (in which case reform should focus on implementation), or primarily functioning as an extraction mechanism (in which case reform should focus on redistribution)? The measurement trajectory (extractiveness increasing over time despite formal integration deepening) suggests the primary function is shifting from coordination to extraction: formal integration agreements are being signed and invoked rhetorically while informal trade networks absorb the actual cross-border flows, leaving formal trade with enforced barriers. This would make the constraint increasingly snare-like from the perspective of formal trade actors and increasingly piton-like from the state perspective (formal system persists despite losing function). Resolution requires empirical determination of whether informal trade is absorbing or replacing formal flows, and whether integrationist governments can sustain liberalization against cyclical protectionist pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informal_trade_displacement,
    'Does increased formal integration reduce informal trade or merely redirect it without efficiency gain?',
    'Empirical measurement of total trade flows (formal + estimated informal) pre- and post-integration phases; tracking of cross-border informal employment relative to formal integration depth',
    'If informal trade absorbs displaced formal actors: integration may be zero-sum redistribution, not growth-generating. If informal trade declines with formal integration: constraint truly coordinates access expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_trade_displacement, empirical, 'Whether formal integration reduces or redirects informal cross-border trade').

omega_variable(
    domestic_political_sustainability,
    'Can integrationist governments sustain liberalization commitment against cyclical protectionist pressure during downturns?',
    'Historical pattern analysis of integration rollback during recessions; correlation between GDP contraction and tariff increases; timeline of policy reversal relative to external shocks',
    'If governments repeatedly reverse: scaffold sunset clause is not credible, and agents will not invest in integration-dependent activities. Constraint remains tangled rope with high uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_political_sustainability, empirical, 'Political sustainability of integrationist commitments during economic downturns').

omega_variable(
    labor_mobility_wage_convergence,
    'Does liberalized labor mobility drive wage convergence across the region or merely capture low-wage labor flows into high-wage economies?',
    'Wage level tracking in source and destination countries for comparable occupational categories; analysis of whether remittance flows and return migration generate local wage pressure in origin economies',
    'If convergence occurs: labor mobility is genuine coordination mechanism. If divergence amplifies: constraint becomes extraction mechanism (snare from analytical perspective), not rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_mobility_wage_convergence, empirical, 'Whether labor mobility drives wage convergence or labor extraction').

omega_variable(
    institutional_capacity_mismatch,
    'Do national regulatory and customs institutions have capacity to implement harmonized standards, or will implementation gaps create new extraction opportunities?',
    'Assessment of bureaucratic capacity, training infrastructure, and equipment modernization in customs authorities; measurement of implementation compliance rates for harmonized protocols',
    'If capacity gaps persist: formal integration rules become theater, and informal workarounds continue. Piton classification expands to cover formal integration machinery itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_mismatch, empirical, 'Institutional capacity to implement harmonized integration standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maghreb_economic_integration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maghreb_tr_t0, maghreb_economic_integration, theater_ratio, 0, 0.55).
narrative_ontology:measurement(maghreb_tr_t5, maghreb_economic_integration, theater_ratio, 5, 0.62).
narrative_ontology:measurement(maghreb_tr_t10, maghreb_economic_integration, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(maghreb_be_t0, maghreb_economic_integration, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(maghreb_be_t5, maghreb_economic_integration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(maghreb_be_t10, maghreb_economic_integration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maghreb_economic_integration, resource_allocation).
narrative_ontology:affects_constraint(maghreb_economic_integration, maghreb_customs_harmonization).
narrative_ontology:affects_constraint(maghreb_economic_integration, maghreb_labor_mobility_restrictions).
narrative_ontology:affects_constraint(maghreb_economic_integration, maghreb_currency_alignment).

% DUAL FORMULATION NOTE:
% Economic integration constraint operates as three structurally distinct mechanisms: tariff barriers (trade extraction), labor mobility restrictions (labor market segmentation), and customs/regulatory divergence (administrative extraction). This story captures the aggregate constraint; decomposition into three separate stories would isolate each mechanism with distinct ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maghreb_economic_integration, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
