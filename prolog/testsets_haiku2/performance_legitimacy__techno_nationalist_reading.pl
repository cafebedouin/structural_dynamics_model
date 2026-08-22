% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy Regime
 *   domain: political_economy/state_capitalism/development_strategy
 *
 * SUMMARY:
 *   The techno-nationalist reading of the performance-legitimacy kernel
 *   asserts that a state regime's primary legitimacy claim rests on achieving
 *   technological self-sufficiency and global competitive leadership in
 *   strategic industries critical to national security and great-power
 *   status. This reading instantiates a specific constraint: capital and
 *   talent are redirected from consumer and service sectors toward
 *   state-designated strategic champions through direct subsidy, preferential
 *   allocation, and supply-chain protection, regardless of market signals or
 *   profitability. The regime measures its success against technological
 *   milestones and geopolitical parity indicators, not against improvements
 *   in daily life or economic efficiency. This reading differs structurally
 *   from the sibling 'livelihood security' reading (which grounds legitimacy
 *   in direct welfare improvement) and the 'qualitative development' reading
 *   (which emphasizes efficiency and sustainability). The constraint is
 *   CLAIMED as tangled rope (genuine coordination problem in achieving
 *   technological parity, plus asymmetric extraction from consumer sectors)
 *   while the measurement trajectory shows extractiveness rising steeply in
 *   the early interval (0.42 to 0.65) then plateauing, theater ratio climbing
 *   throughout (marking increasing divergence between the regime's strategic
 *   narrative and the actual extraction mechanism), and suppression
 *   requirement rising then stabilizing as the enforcement apparatus hardens
 *   (early investment redirection requires coercion; later stages routinize
 *   the suppression). The claim/metric independence is deliberate—the
 *   engine's per-seat computation will reveal whether beneficiary seats
 *   experience this as genuine coordination and payer seats experience it as
 *   extraction.
 *
 * KEY AGENTS:
 *   - Defense-tech champions (state-owned or state-controlled): institutional beneficiaries, receive directed capital and preferential allocation
 *   - State enterprise conglomerates: institutional beneficiaries, integrated into strategic supply chains
 *   - Strategic research institutes: organized beneficiaries, identity-locked to state strategic vision
 *   - Consumer-goods sectors: powerful payers, lose capital access and market protection
 *   - Market-driven allocators (private finance, competitive firms): powerful payers, coerced into state-directed lending and supply-chain compliance
 *   - Domestic migrant workers: powerless payers, identity-locked (hukou-style systems), lose non-strategic employment
 *   - Rural agricultural regions: powerless payers, trapped by resource extraction and investment starvation
 *   - Rival great powers: excluded structural actors, prevented from participating in supply chains
 *   - Regime legitimacy apparatus: agenda-setter, administers allocation, measures legitimacy against technological benchmarks
 *   - International competitiveness analysts: observer seat, measures whether technological parity is achieved or subsidized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.72).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy Regime").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political_economy/state_capitalism/development_strategy").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, '658f26b8-4117-4c03-84ff-20559816583b').
narrative_ontology:cs_kernel_codification('658f26b8-4117-4c03-84ff-20559816583b', formalized).
narrative_ontology:cs_authority_grounding('658f26b8-4117-4c03-84ff-20559816583b', extraction).
narrative_ontology:cs_interpretation_layer_present('658f26b8-4117-4c03-84ff-20559816583b').
narrative_ontology:cs_reading_relation('658f26b8-4117-4c03-84ff-20559816583b', performance_legitimacy__livelihood_security_reading, forecloses).
narrative_ontology:cs_reading_relation('658f26b8-4117-4c03-84ff-20559816583b', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('658f26b8-4117-4c03-84ff-20559816583b', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_axiom('658f26b8-4117-4c03-84ff-20559816583b', foundational, technological_dominance_grounds_legitimacy).
narrative_ontology:cs_axiom_status(technological_dominance_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('658f26b8-4117-4c03-84ff-20559816583b', technological_dominance_grounds_legitimacy, deontological).
narrative_ontology:cs_axiom('658f26b8-4117-4c03-84ff-20559816583b', foundational, strategic_sector_state_direction_necessary).
narrative_ontology:cs_axiom_status(strategic_sector_state_direction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('658f26b8-4117-4c03-84ff-20559816583b', strategic_sector_state_direction_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('658f26b8-4117-4c03-84ff-20559816583b', technological_vulnerability_paradigm).
narrative_ontology:cs_drift_state('658f26b8-4117-4c03-84ff-20559816583b', contemporary_post_parity_competitive_stasis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('658f26b8-4117-4c03-84ff-20559816583b', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_tech_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, state_enterprise_conglomerates).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, strategic_research_institutes).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, market_driven_allocators).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, domestic_migrant_workers).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, rural_agricultural_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State-designated strategic industries (semiconductors, aerospace, quantum computing, AI infrastructure) receive guaranteed capital allocation, preferential licensing, access to talent pools, and export subsidies. They operate under directed innovation mandates that prioritize technological parity with rivals regardless of profitability. Their success is measured against global benchmarks, not market returns. Exit is impossible: the state owns or deeply controls them; they are the reading's primary beneficiary.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_tech_champions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Large mixed-ownership enterprises integrated into supply chains for strategic industries receive preferential access to financing, raw materials, and state contracts. They are insulated from competitive pressure in protected domestic markets while gaining first-mover advantage in supply-chain segments the state deems critical. Their role is coordinating national champions' supply ecosystems.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, state_enterprise_conglomerates, beneficiary,
    institutional, civilizational, trapped, national).

% State-funded laboratories, university departments, and think tanks aligned with strategic industry missions receive sustained budget increases, recruitment authority, and freedom from publication constraints when working on classified research. Their legitimacy is tied to demonstrating scientific achievements that position the nation as a technological leader. Their identity is fused with the state's strategic vision.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, strategic_research_institutes, beneficiary,
    organized, civilizational, identity_locked, national).

% Industries serving consumer demand (textiles, appliances, light manufacturing, commercial services) lose capital access as investment flows to strategic sectors. They compete for scarce labor and materials at state-controlled prices while facing tariff barriers protecting strategic-industry supply chains. They cannot exit because the domestic market is closed and foreign competitors are restricted; they shrink rather than relocate.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors, payer,
    powerful, biographical, constrained, national).

% Private firms that historically allocated capital based on return signals face state direction to redirect investment toward strategic industries regardless of profitability. They are coerced to participate in state-identified supply chains and accept price controls on materials fed into strategic production. Financial institutions are directed to favor strategic-sector lending. Exit means losing domestic market access and being cut off from foreign-exchange allocations.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, market_driven_allocators, payer,
    powerful, biographical, constrained, global).

% Rural-to-urban workers drawn to manufacturing and services lose employment as those sectors shrink. They are redirected to lower-wage jobs in logistics supporting strategic supply chains or absorbed into state-sector support roles. Their hukou (household registration) or equivalent identity mechanism keeps them trapped in labor-surplus regions, preventing free migration to higher-wage opportunities. The state presents this as protecting 'proper' economic structure; workers experience it as blocked mobility and falling real wages.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, domestic_migrant_workers, payer,
    powerless, biographical, identity_locked, national).

% Agricultural and resource-extraction regions are subject to state procurement at below-market prices to supply raw materials for strategic industries. Environmental costs of resource extraction are externalized locally. Investment in local infrastructure, education, and services is starved as capital flows to strategic-technology clusters. Rural outmigration is treated as inevitable rather than a policy failure; the regions become geographically and economically dependent on state subsidies for survival.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, rural_agricultural_regions, payer,
    powerless, generational, trapped, regional).

% Geopolitical competitors are explicitly excluded from strategic supply chains through export controls, technology transfer restrictions, and preferential trade blocs favoring allies. They would argue for open technological competition and integration; instead they are treated as security threats whose participation in the constraint's supply network is prevented by design. This exclusion is the enforcement mechanism itself.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, rival_great_powers, excluded,
    institutional, civilizational, trapped, global).

% The state governance structure sets the strategic industry mandate, allocates capital, directs research priorities, and enforces supply-chain compliance through regulation and subsidy. It measures its own legitimacy against technological leadership benchmarks and great-power parity goals, not against consumer welfare or economic efficiency. The apparatus maintains this constraint by continuously identifying new 'strategic' sectors and redirecting resources into them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, regime_legitimacy_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Academic economists, think-tank researchers, and policy analysts from outside the benefiting parties measure whether the constraint's strategic industry investments generate sustainable competitive advantage or create protected monopolies dependent on perpetual state subsidy. They provide external assessment of whether the founding problem (geopolitical vulnerability) remains live or has been resolved, and whether the constraint's persistence is justified by actual technological parity or by institutional inertia.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, international_competitiveness_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, defense_tech_champions).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools national scientific and manufacturing capacity toward achieving technological parity with rival great powers in sectors deemed existentially critical to military capability and economic sovereignty (semiconductors, advanced materials, quantum systems, AI infrastructure). Solves the coordination problem of decentralized R&D producing fragmented efforts that lose global races; centralized direction enables sustained, focused investment that accumulates over decades.
% TRANSFER_FUNCTION: Transfers capital, talent, and raw materials from consumer-sector industries, rural regions, and market-driven allocators toward state-designated strategic champions and research institutes. The mechanism: state capital allocation, preferential licensing, tariff protection for supply chains, below-market procurement from resource sectors, and wage suppression in non-strategic labor markets. The extraction moves resources from where citizens experience direct welfare loss (shrinking consumer goods production, stagnant rural incomes, reduced service availability) to where the regime measures legitimacy (technological milestone achievement, great-power parity, export-control resilience).
% ABSENT_VOICES: Rival great powers and their allies are structurally excluded; they would argue for open technology markets and competitive supply chains but are kept out by the same export-control and state-ownership apparatus that the constraint rides on. Domestic consumer advocates and efficiency-focused economists are present but overruled; labor unions representing displaced workers are either coopted into state structures or suppressed. Small- and medium-sized private enterprises have no seat in strategic-industry governance despite bearing supply-chain costs.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, capital would flow back toward consumer and service sectors; consumer prices would fall; rural regions would receive investment in local infrastructure and services; migrant workers would find market-wage opportunities in competitive labor markets rather than state-directed redistribution. The geopolitical posture would shift from technological independence toward competitive engagement with rivals. Great-power status measured by technological dominance would give way to measurement by economic efficiency and living standards. The entire resource-allocation hierarchy would reorganize around market signals rather than state strategic mandates.
% FOUNDING_PROBLEM: Vulnerability to technological dependency on geopolitical rivals: an adversary could embargo critical semiconductor supply, cut off access to advanced materials, or achieve decisive military advantage through unmatched AI or quantum capabilities. Historical cases: Cold War Soviet technical surprises (Sputnik), modern semiconductor bottlenecks exposing supply-chain fragility, and current AI capability races where trailing nations fear technological subjugation. The founding problem asserts that market-driven allocation cannot sustain the focused, decades-long investments required to achieve technological parity in existentially critical domains.
% FOUNDING_PROBLEM_CORROBORATION: The regime attests the founding problem is live and mounting; defense strategists outside the benefiting parties acknowledge genuine geopolitical risk in critical supply-chain concentration. However, independent economic analysis and international competitiveness research increasingly show that: (1) many 'strategic' industries are protected monopolies dependent on perpetual subsidy rather than globally competitive at market prices; (2) the opportunity cost of foregone consumer-sector growth and rural development is severe enough to undermine legitimacy through channels the regime does not measure; (3) technological leadership itself persists through market competition and international collaboration rather than state direction (semiconductor design achieved by competitive firms, AI advances by decentralized research). The corroboration is mixed: the founding problem is real, but whether the constraint solves it versus merely consuming resources to maintain the appearance of solving it remains unresolved.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.42 at t=0, reflecting genuine but nascent technological vulnerability and early-stage capital redirection) and rises steeply through the first two decades (t=10: 0.54; t=15: 0.60; t=20: 0.65) as the regime identifies more sectors as 'strategic' and extends supply-chain protection. It plateaus at 0.68 after t=25, suggesting the regime has stabilized its resource-reallocation boundaries and further growth requires new geopolitical triggers rather than continued voluntary redirection. Theater ratio climbs throughout (0.18 to 0.41), indicating that an increasing share of suppression activity defends the constraint's legitimacy narrative (technological leadership, great-power parity) rather than the underlying coordination function. At t=25 onward, the regime's investment in technological showcase projects and publicity about strategic milestones rises even as actual competitive advantage in several sectors stagnates—the constraint becomes more theatrical as its real function faces diminishing returns. Suppression requirement mirrors extractiveness but remains consistently higher (0.72 at interval end vs 0.68 for extractiveness), indicating that maintaining the constraint against resistance and market-signal contradictions requires continuous active enforcement: labor controls, export restrictions, price controls, and capital-flow directives must all be hardened and deepened. The one-grid alignment means every metric is authored at every time point examined; this enables reliable measurement of the divergence between the regime's claim (Tangled Rope: genuine coordination for security) and the payer-seat experience (extraction that requires mounting suppression to sustain).
 *
 * PERSPECTIVAL GAP:
 *   The regime legitimacy apparatus experiences this constraint as essential coordination: pooling national capacity to avoid technological subjugation is a genuine coordination problem, and the constraint solves it by directing resources where dispersed markets would not. Beneficiary institutional seats (defense-tech champions, state enterprises, research institutes) experience it as enabling—they receive guaranteed capital, talent, and protection that allow long-term strategic investment without profitability pressure. Payer seats experience the same constraint very differently. Consumer-goods sectors experience capital starvation and enforced supply-chain integration as extraction. Market-driven allocators experience state direction and financial coercion as pure constraint violation. Domestic migrant workers experience labor-market suppression and reduced non-strategic-sector wages as extraction of their mobility and earnings. Rural regions experience procurement-price suppression and investment diversion as extraction of their natural resources and future development. The engine computes per-seat types from the structural data: the regime seat and beneficiary seats should classify the constraint as Rope or Tangled Rope (genuine coordination value, moderate extraction); payer seats should classify it as Tangled Rope or Snare (low coordination value, high extraction and suppression). This divergence is the measurement the corpus takes—where the divergence is widest (regime vs. migrant worker) is where the constraint's claim is most contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit-option asymmetries. Defense-tech champions and strategic research institutes are structural beneficiaries (d near 0.0): they receive capital, talent, and protection; their exit is impossible (institutional ownership, identity fusion). State enterprise conglomerates are beneficiaries (d ≈ 0.15): they gain preferential access but are also constrained by state ownership and supply-chain mandates. Consumer-goods sectors are structural targets (d ≈ 0.85): they bear capital starvation, loss of tariff protection, and diverted raw materials; their exit is constrained (domestic market is closed, foreign competitors restricted). Market-driven allocators are targets (d ≈ 0.75): they are coerced into state-directed lending and supply chains; constrained exit (loss of market access). Migrant workers are deep targets (d ≈ 0.95): they are identity-locked (hukou systems), trapped in labor-surplus regions, and lose non-strategic employment; their exit is nearly impossible. Rural regions are targets (d ≈ 0.90): they are trapped by resource-extraction dependency and investment starvation. Rival powers are excluded (d = 1.0 treated as structurally irrelevant to effective extraction computation, since they are not governed by the constraint). The regime apparatus is the agenda-setter (d not computed for institutional governance seats). The directionality asymmetry—high d for powerless workers and rural regions, low d for institutional beneficiaries—is the structural foundation of the claim/metric gap: effective extraction is amplified for identity-locked, trapped targets and dampened for beneficiaries with arbitrage exit, which is exactly the distributional pattern the regime's strategic narrative obscures.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's claimed type (Tangled Rope) rests on two assertions: (1) genuine coordination solves a real problem (technological vulnerability in strategic sectors), and (2) asymmetric extraction from non-strategic sectors is the price of that coordination. The measurement trajectory reveals a challenge to the second assertion. From t=0 to t=20, extractiveness and suppression rise together with theater ratio, suggesting the regime is still arguing about the NEED for the constraint—the theater activity is justifying technological race urgency and great-power competition. But from t=20 to t=35, extractiveness plateaus while theater climbs. This disjunction indicates that the constraint's real coordination function has stabilized (technological investment levels off, strategic sectors reach competitive parity in key domains, or the regime stops being able to absorb more resources into strategic industries), but the suppression and narrative machinery keep intensifying. This is the mandatrophy signature: the constraint persists not because it solves the founding problem but because the apparatus administering it (regime, strategic-research institutes, state enterprises) has become fused with the state's legitimacy narrative and cannot acknowledge achievement without delegitimizing itself. If the founding problem is 'dead' (technological vulnerability resolved, strategic parity achieved, or rivals' capabilities stabilized and defensible), the constraint's persistence as an extraction mechanism becomes visible. The international competitiveness analysts seat attests that several 'strategic' sectors have indeed achieved or surpassed rival parity but remain protected and subsidized—evidence that the constraint has outlived its stated function. The theater-ratio rise from t=25 onward (milestone announcements, capability showcases, competitive-standing claims) without corresponding extractiveness gains suggests the constraint is now maintained by institutional inertia and narrative investment rather than by solving an unsolved problem. This is not a definitive mandatrophy verdict—the regime and beneficiary seats will dispute whether the founding problem remains live—but the structural data flags the risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_parity_achievement,
    'Has the constraint solved its founding problem—achieved technological parity and reduced vulnerability to strategic supply-chain embargo—or does the plateau in extractiveness after t=25 indicate that the constraint persists by inertia despite solving or overshooting the original problem?',
    'Independent assessment of competitive standing in designated strategic sectors (semiconductors, AI, quantum, advanced materials) against key rivals, controlling for subsidy and protection; comparison of historical vulnerability (t=0 supply-chain exposure) to current vulnerability; measurement of whether removal of state support would result in immediate competitive collapse or continued dominance.',
    'If parity is achieved, the constraint''s persistence is mandatrophy: it extracts from non-strategic sectors to maintain an obsolete apparatus. Classification shifts from Tangled Rope (real coordination + extraction) toward Piton (institutional inertia + theater). If parity is not achieved despite plateau, the constraint has hit efficiency limits—further resource reallocation faces diminishing returns, and the regime must either accept vulnerability or escalate extraction further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_parity_achievement, empirical, 'Whether the founding problem (technological vulnerability) is solved, live, or transcended by subsidy-dependent artificial dominance.').

omega_variable(
    extraction_distributional_justice,
    'Is the extraction distributed across society (a broad sacrifice for national security) or concentrated on powerless agents (migrant workers, rural regions) whose lack of political voice prevents their bearing costs from destabilizing legitimacy?',
    'Time-series comparison of wage trajectories for workers in strategic vs. non-strategic sectors; measurement of educational and healthcare investment in strategic-tech clusters vs. rural regions; political voice measurement (representation in decision-making bodies for strategic-sector policy); analysis of whether powerful sectors (consumer goods, finance) can mount effective resistance vs. powerless sectors'' inability to resist.',
    'If extraction is broadly distributed, the constraint operates as Tangled Rope (all bear costs, some benefit, coordination story is plausible). If extraction concentrates on powerless seats, the constraint operates as Snare with a narrative overlay (Tangled Rope framing is false summary; beneficiary seats exploit the powerless''s inability to articulate grievance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_distributional_justice, empirical, 'Whether the constraint extracts broadly or targets powerless beneficiaries selectively.').

omega_variable(
    supply_chain_resilience_vs_market_efficiency,
    'Does state-directed strategic supply-chain integration genuinely improve resilience to embargo and competitive disruption, or does the same resilience emerge from market-driven diversification and alliance networks, making the state-directed approach redundant extraction?',
    'Empirical comparison of supply-chain robustness (time-to-substitute-if-embargo) in state-directed sectors vs. market-integrated sectors in similar industries; historical evidence from sanctions episodes and trade-war scenarios; analysis of whether protection-and-subsidy actually reduces vulnerability or creates dependency on state apparatus that becomes itself a vulnerability (if regime changes, subsidy stops).',
    'If market-driven diversification achieves same resilience at lower cost, the constraint is pure extraction masquerading as coordination. If state direction genuinely improves resilience beyond market capacity, the Tangled Rope classification holds. This omega addresses the fundamental claim/metric gap: is the constraint solving the founding problem or is the founding problem an excuse for institutional rent extraction?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_resilience_vs_market_efficiency, empirical, 'Whether state-directed supply-chain integration improves resilience beyond market alternatives or merely shifts costs.').

omega_variable(
    regime_identity_fusion_in_tech_narrative,
    'How much of the constraint''s persistence depends on the regime''s identity becoming fused with the technological-leadership narrative such that admitting the founding problem is solved (or unsolvable) would delegitimize the regime itself?',
    'Historical analysis of regime legitimacy claims over time—do they shift when technological parity is achieved, or do they intensify technological-race framing to prevent acknowledgment of achievement? Elite rhetoric analysis: do regime officials and beneficiary-seat voices admit success and propose reallocation, or do they construct new ''strategic'' sectors to justify continued extraction?',
    'If regime identity is heavily fused with technological-dominance narrative, the constraint is locked into persistence by institutional-identity mechanisms even if the founding problem is solved—a classic piton-signature condition (inertia + theater + no party willing to exit despite mounting costs). This omega captures the identity-lock dynamic at the institutional level, distinct from individual-worker identity-lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_identity_fusion_in_tech_narrative, conceptual, 'Whether regime legitimacy is fused with technological-dominance narrative such that constraint cannot be revised without regime delegitimization.').

omega_variable(
    kernel_reading_foreclosure_vs_coexistence,
    'Is the techno-nationalist reading''s core axiom (strategic technological dominance grounds regime legitimacy) logically incompatible with the livelihood-security reading''s core axiom (direct tangible improvements in daily life ground regime legitimacy) within the same governance framework, or do they coexist as different rhetorical strategies the regime can emphasize depending on political context?',
    'Institutional analysis: can a single regime claim both ''legitimacy from technological parity'' AND ''legitimacy from welfare improvement'' simultaneously, or must it choose? Historical case evidence: when technological investment and livelihood investment compete for same resources, does the regime resolve the tradeoff openly or obscure it? Political economy: does the regime present the constraint as temporary sacrifice for future welfare gains (coexistence narrative) or as permanent techno-nationalist refocusing (foreclosure)?',
    'If readings foreclose each other within a single framework, this constraint (techno-nationalist) cannot coexist with livelihood-security reading in the same regime justification—the regime must actively suppress livelihood-security claims. If they coexist, the regime can rhetoricize both simultaneously, using technological-dominance claims to justify to elites and international audiences, livelihood-security claims to justify to domestic constituencies. This determines whether the suppression measured in the constraint is suppression-of-alternatives (foreclosure) or rhetorical-compartmentalization (coexistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_vs_coexistence, conceptual, 'Whether techno-nationalist and livelihood-security readings are logically foreclosed or can coexist as simultaneous regime claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__techno_nationalist_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__techno_nationalist_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__techno_nationalist_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__techno_nationalist_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__techno_nationalist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__techno_nationalist_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__techno_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% The performance-legitimacy kernel admits four structurally distinct readings, each claiming authority for the same regime's right to rule but grounding that authority in different outcomes. This constraint instantiates the techno-nationalist reading: legitimacy from strategic technological self-sufficiency and great-power industrial leadership. The sibling readings reframe the same institutional regime around different legitimacy criteria (growth rates, structural development quality, livelihood security). Each reading has its own ε (extractiveness from the constraint's standpoint), its own stakeholder structure, and its own beneficiary/victim assignments. They are not perspectives on one constraint; they are separate constraints that share a kernel (the contested legitimacy commitment). Decomposition is required by the ε-invariance principle: measuring 'performance legitimacy' as a single constraint would require choosing which outcome to measure, which pre-adjudicates the reading. Each reading is authored independently with its own structural data and metrics. Network links establish that they are family members addressing the same kernel from different reading positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__techno_nationalist_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
