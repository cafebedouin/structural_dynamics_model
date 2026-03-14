% ============================================================================
% CONSTRAINT STORY: taiwan_semiconductor_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_semiconductor_concentration, []).

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
 *   constraint_id: taiwan_semiconductor_concentration
 *   human_readable: Taiwan Semiconductor Concentration and Global Supply Chain Dependency
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Taiwan's concentration of advanced semiconductor manufacturing —
 *   particularly TSMC's dominance of sub-5nm chip production — represents a
 *   critical global supply chain vulnerability with characteristics of a pure
 *   extraction constraint (Snare). The constraint emerges from a combination
 *   of historical path dependence, technological superiority, geographic
 *   clustering, and capital concentration that creates structural dependency
 *   for global technology industries. Over the 2003-2023 interval (t=0 to
 *   t=20), extractiveness has increased from 0.35 to 0.68 as global demand
 *   for advanced chips has grown exponentially while production capacity
 *   remains concentrated. Theater ratio (0.35) reflects that the
 *   concentration is fundamentally structural rather than performative — it
 *   results from genuine technological and economic factors, not from ritual
 *   enforcement or institutional inertia. However, theater has been rising as
 *   geopolitical actors have begun using supply chain framing and
 *   'resilience' narratives to justify subsidies and trade restrictions,
 *   introducing performative elements atop the structural reality. The
 *   constraint exhibits all seven perspectives simultaneously, creating a
 *   diagnostic exemplar of how a single structural phenomenon can appear as
 *   pure extraction (Snare) from one position, coordination (Rope) from
 *   another, and natural law (Mountain-adjacent) from a civilizational view.
 *
 * KEY AGENTS:
 *   - Taiwan Semiconductor Manufacturers (TSMC, MediaTek, others): Primary beneficiaries (institutional/arbitrage) — capture geopolitical leverage, pricing power, foreign investment, and technology leadership
 *   - Global Technology-Dependent Industries (automotive, defense, healthcare, consumer electronics): Primary victims (powerless/trapped) — dependent on Taiwan supply; no viable alternatives at competitive scale
 *   - Advanced Chip Designers and Fabless Companies (Qualcomm, Apple, NVIDIA, AMD): Secondary victims (moderate/constrained) — structurally dependent on TSMC; high switching costs but some negotiating power
 *   - US/EU Government Industrial Policy Actors: Organized agents (organized/constrained) — funding alternative fabs (CHIPS Act, EU Chips Act) while simultaneously extracting knowledge transfer and market access from Taiwan manufacturers
 *   - China's Semiconductor Strategy: Powerful actor (powerful/mobile) — seeking to reduce Taiwan dependency while leveraging concentration for geopolitical advantage; mobile enough to pursue sanctions evasion and internal ecosystem development
 *   - Post-WWII Liberal Trade Order Institutions: Degraded institutional framing (institutional/arbitrage) — market efficiency narratives increasingly performative as technology nationalism and subsidy competition replace comparative advantage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_semiconductor_concentration, 0.68).
domain_priors:suppression_score(taiwan_semiconductor_concentration, 0.72).
domain_priors:theater_ratio(taiwan_semiconductor_concentration, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_semiconductor_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(taiwan_semiconductor_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_semiconductor_concentration, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_semiconductor_concentration, snare).
narrative_ontology:human_readable(taiwan_semiconductor_concentration, "Taiwan Semiconductor Concentration and Global Supply Chain Dependency").
narrative_ontology:topic_domain(taiwan_semiconductor_concentration, "economic/geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_semiconductor_concentration, taiwan_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(taiwan_semiconductor_concentration, advanced_chip_designers).
narrative_ontology:constraint_victim(taiwan_semiconductor_concentration, global_economies).
narrative_ontology:constraint_victim(taiwan_semiconductor_concentration, technology_dependent_industries).
narrative_ontology:constraint_victim(taiwan_semiconductor_concentration, chip_consumers_worldwide).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL TECH-DEPENDENT INDUSTRIES (SNARE) — Cannot exit the constraint; trapped by structural dependency on Taiwan-manufactured chips. Automotive, defense, healthcare, and consumer electronics sectors have no alternative sources for advanced node fabrication. Maximum suppression: no viable substitute suppliers exist at competitive scale. Complete extraction: price volatility, supply prioritization, geopolitical leverage all flow to Taiwan and TSMC.
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHIP DESIGNERS AND FABLESS COMPANIES (SNARE) — Structurally constrained by high switching costs and lack of alternative foundries with comparable process nodes. TSMC manufactures >90% of world's most advanced chips (sub-5nm). Exit is possible at prohibitive cost: multi-year fab partnerships, qualification cycles, yield learning, capital investment in non-TSMC fabs. High extraction: TSMC controls pricing, capacity allocation, and access prioritization. Moderate experienced power due to some negotiating leverage and ability to shift portions of production.
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TAIWAN SEMICONDUCTOR MANUFACTURERS (ROPE) — Benefits from concentration; experiences the constraint as pure coordination. TSMC's dominance creates coordination function: global supply chain synchronizes on one reliable supplier. Taiwan gains geopolitical leverage, foreign investment, export revenue, and technology leadership. Arbitrage options abound: can diversify clients, shift capacity between applications, negotiate with governments for subsidies and protection. For Taiwan manufacturers, the constraint is a coordination mechanism they dominate.
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: US/EU GOVERNMENT INDUSTRIAL POLICY (TANGLED ROPE) — Organized actors with significant but constrained exit. US CHIPS Act (2022), EU Chips Act (2023) fund alternative foundries (Intel, Samsung, TSMC US plants) to reduce concentration. Genuine coordination function exists: ensuring supply chain resilience benefits all parties. But asymmetric extraction persists: massive subsidies to TSMC (US fab incentives), regulatory pressure on Taiwan (export controls), and Taiwan's technological superiority still dominates. Governments coordinate supply diversification while simultaneously extracting TSMC's production and knowledge transfer as condition of subsidies and market access. High suppression via export controls and technology restrictions.
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-WWII LIBERAL TRADE ORDER (PITON) — The constraint's institutional framing as 'free market efficiency' (concentration happened because TSMC is superior, not because of policy lock-in) is increasingly performative. Theater ratio of 0.35 reflects that market efficiency narratives now obscure geopolitical vulnerability recognition. The liberal trade framework that produced this outcome is degraded: de facto technology nationalism (export controls, subsidy wars, supply chain localization mandates) is replacing liberal principles. The piton persists through institutional inertia: governments still invoke comparative advantage while simultaneously funding re-shoring and technological autarky. The constraint's theater has been low (market-driven) but rising as contradictions emerge.
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINA'S SEMICONDICTOR STRATEGY (TANGLED ROPE) — Powerful actor with mobile options seeking to reduce Taiwan dependency while leveraging concentration for political advantage. China coordinates internal semiconductor ecosystem (SMIC, HiSilicon, SOC integration) while extracting through espionage, sanctions evasion, and supply chain integration. Benefits from Taiwan concentration (can pressure Taiwan geopolitically through supply threats) while bearing costs (export controls on advanced equipment, design tool restrictions). High suppression: US/EU export controls create barriers to accessing advanced nodes. But not trapped — can produce lower-node chips and is investing massively in indigenous design and manufacturing. Mixed coordination (needs global supply chain access) and extraction (seeks to circumvent restrictions and dominate internal markets).
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, the constraint is structurally irreversible in the short-term (20+ year timescale). Taiwan's geographic position, accumulated expertise, capital stock, and talent density create path dependence that no amount of subsidy or effort can quickly overcome. This perspective sees the snare structure as deeply embedded in the global economy's infrastructure. Exit for dependent economies requires multi-decade alternative fab development or geographic diversification of production — feasible but costly and slow. The analytical view reveals the snare structure: global extraction through concentrated capacity, persistent suppression via high switching costs and technological barriers, and systematic beneficiary (Taiwan/TSMC) dominance.
constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_semiconductor_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_semiconductor_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_semiconductor_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_semiconductor_concentration, TR),
    TR >= 0.70.

:- end_tests(taiwan_semiconductor_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing over the interval. TSMC controls >90% of world's most advanced chips (sub-5nm). This dominance enables systematic extraction: pricing power (customers have no alternatives), capacity allocation (TSMC prioritizes clients based on geopolitical alignment and profitability), and supply withholding leverage (demonstrated during pandemic supply shocks). The increase from 0.35 to 0.68 reflects growing dependency as AI/advanced computing demand surges while TSMC's market share has only consolidated. Suppression (0.72): Very high. Structural barriers to exit: (1) Capital intensity — building competing fabs costs $20-50B; (2) Expertise clustering — Taiwan has accumulated 40+ years of semiconductor expertise and talent density; (3) Time lag — constructing and qualifying alternative fabs requires 5-10 years minimum; (4) Technological barriers — advanced process nodes require proprietary equipment and know-how; (5) Policy lock-in — export controls on advanced equipment restrict alternative suppliers' access. Theater ratio (0.35): Moderate and rising. The constraint is fundamentally structural (technological and economic), not primarily performative. However, theater has increased as governments invoke 'supply chain resilience' and 'national security' narratives to justify subsidies, protectionist policies, and Taiwan military support — these are increasingly performative framings atop the core structural extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. TSMC sees Rope — a coordination mechanism enabling global supply chain synchronization that benefits Taiwan. Global technology industries see Snare — pure extraction with no coordination benefit. US/EU governments see Tangled Rope — genuine coordination function (supply security) with embedded extraction (taxpayer subsidy of competitor fabs, knowledge transfer demands). China sees both a target (dependency on Taiwan) and leverage (can threaten supply). The liberal trade order sees a market efficiency outcome (Rope or Mountain-adjacent) but this framing is increasingly performative — the constraint is clearly extractive (Snare) from most positions. The analytical observer sees the snare structure from civilizational scope: the extraction is deep, the suppression is structural, and exit is generationally expensive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from structural position and exit options. Global technology industries (powerless/trapped): d ≈ 0.95 (full target). TSMC and Taiwan manufacturers (institutional/arbitrage): d ≈ 0.05 (full beneficiary). Chip designers (moderate/constrained): d ≈ 0.70 (primarily victim with limited agency). US/EU governments (organized/constrained): d ≈ 0.60 (bearing costs of subsidy and vulnerability, benefiting from supply security). China (powerful/mobile): d ≈ 0.65 (bearing costs of export controls, benefiting from leverage over Taiwan and domestic production potential). The beneficiary/victim declaration is stark: Taiwan semiconductor manufacturers benefit; everyone else bears costs. The engine derives d from these structural relationships plus exit options, producing high chi for victims and low/negative chi for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA STRUCTURAL CLARITY: This constraint resolves mandatrophy through explicit agent identification and beneficiary/victim declaration. The core claim — that Taiwan semiconductor concentration creates global supply chain vulnerability — is robust across perspectives but manifests as different types based on structural position. The classification as Snare from global perspectives (powerless/trapped, moderate/constrained, analytical) reflects the extractive structure: concentrated capacity enables pricing power, supply leverage, and geopolitical coercion. The Rope classification from TSMC's perspective is not a competing claim but a perspectival truth: for the beneficiary, the constraint is pure coordination. The Tangled Rope from government perspectives reflects genuine dual functions: supply resilience (coordination) with embedded subsidies and knowledge extraction (extraction). Mandatrophy is resolved because each perspective's type reflects its structural relationship to the extraction flow, not disagreement about what is actually happening. The constraint IS a Snare from global scope, a Rope from Taiwan's scope, and a Tangled Rope from government scope — simultaneously. The extractiveness metric (0.68) applies to the victims; the beneficiaries experience negative extractiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geopolitical_weaponization_escalation,
    'Will Taiwan semiconductor concentration become a direct military flashpoint, or will it remain a constraint within economic/political coercion?',
    'Monitoring of military posturing around Taiwan Strait; correlation between supply disruption threats and military deployments; game-theoretic analysis of incentive structures for armed conflict vs economic pressure',
    'If weaponized militarily: classification may shift from Snare toward Scaffold (constraint becomes temporary via warfare/territorial resolution). If constrained to economic pressure: Snare classification persists. If used as leverage for political integration: classification remains Snare but extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_weaponization_escalation, empirical, 'Whether semiconductor concentration becomes military flashpoint').

omega_variable(
    alternative_fab_viability_timeline,
    'Can US/EU subsidized fabs (Intel, TSMC US, Samsung) achieve cost-competitive advanced node production within 10-15 years, or is Taiwan''s structural advantage insurmountable?',
    'Tracking of fab yields, manufacturing costs, time-to-competitiveness for alternative suppliers; economic modeling of subsidy requirements vs private returns; technology transfer success metrics',
    'If viable within 10 years: Scaffold classification becomes appropriate (extraction is temporary, sunset via alternative supply). If non-viable or requires permanent subsidy: Snare persists. If alternative fabs achieve parity but remain geographically concentrated (e.g., only US): concentration problem shifts but structure remains similar (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fab_viability_timeline, empirical, 'Timeline for alternative fab viability and cost competitiveness').

omega_variable(
    supply_chain_fragmentation_vs_concentration,
    'Is semiconductor manufacturing fundamentally subject to concentration (minimum viable scale, expertise clustering, capital requirements), or is observed concentration contingent on policy/investment choices?',
    'Historical analysis of manufacturing technology adoption curves; comparison with other high-tech industries (pharma, aerospace); modeling of theoretical minimum efficient scale for advanced nodes',
    'If fundamental: Taiwan concentration approaches Mountain (natural law of economics and physics, not extractive constraint). If contingent: Snare classification holds and concentration is revealed as policy-driven extractive structure. If partially both: Tangled Rope (genuine coordination function + asymmetric extraction built on path dependence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_fragmentation_vs_concentration, conceptual, 'Whether semiconductor concentration is natural or policy-contingent').

omega_variable(
    taiwan_political_independence_assumption,
    'Does the Snare classification assume Taiwan remains politically independent and able to leverage its position, or does it hold under Chinese political integration?',
    'Scenario analysis under alternative political configurations; modeling of extraction mechanisms under different governance structures; historical comparison with Hong Kong''s manufacturing role pre- and post-1997',
    'Under independence: Snare persists with Taiwan/TSMC as beneficiary. Under political integration with China: structure inverts — Taiwan becomes victim (domestic profits extracted to Beijing), China becomes beneficiary (controls supply lever), and global dependency deepens (geopolitically unified extraction). Classification would shift to more severe Snare from global perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taiwan_political_independence_assumption, conceptual, 'Taiwan political status assumption and impact on constraint structure').

omega_variable(
    chip_design_decoupling_feasibility,
    'Can software abstraction, chiplet architectures, or open-source design tools decouple advanced chip design from Taiwan/TSMC manufacturing, enabling alternative suppliers to be viable even at lower process nodes?',
    'Technology roadmap analysis for chiplet standardization, RISC-V adoption, and open-source EDA tools; feasibility studies for older-node advanced-function designs; market demand analysis for non-TSMC-fabbed alternatives',
    'If feasible: Snare classification becomes Tangled Rope (genuine design-manufacturing decoupling coordination emerges alongside residual extraction). If infeasible: Snare persists. If achieved but only by powerful actors (China, US government): concentration shifts rather than dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chip_design_decoupling_feasibility, empirical, 'Feasibility of chip design/manufacturing decoupling via open architectures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_semiconductor_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsem_tr_t0, taiwan_semiconductor_concentration, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tsem_tr_t10, taiwan_semiconductor_concentration, theater_ratio, 10, 0.26).
narrative_ontology:measurement(tsem_tr_t20, taiwan_semiconductor_concentration, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(tsem_be_t0, taiwan_semiconductor_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tsem_be_t10, taiwan_semiconductor_concentration, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tsem_be_t20, taiwan_semiconductor_concentration, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_semiconductor_concentration, global_infrastructure).
narrative_ontology:affects_constraint(taiwan_semiconductor_concentration, us_china_technology_decoupling).
narrative_ontology:affects_constraint(taiwan_semiconductor_concentration, export_control_semiconductor_supply).
narrative_ontology:affects_constraint(taiwan_semiconductor_concentration, taiwan_strait_military_dependency).

% DUAL FORMULATION NOTE:
% Taiwan semiconductor concentration is upstream of technology competition constraints (US-China decoupling, export controls). The concentration structure enables both markets and geopolitical leverage. Alternative formulations could decompose into: (1) manufacturing capacity concentration (ε=0.68, Snare), (2) design-manufacturing coupling (ε=0.55, Tangled Rope), and (3) technology nationalism as response (ε=0.72, Snare). This story captures the integrated structure; decomposition would require separate ε values for supply-side vs technology-competition perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_semiconductor_concentration, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
