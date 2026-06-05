% ============================================================================
% CONSTRAINT STORY: us_tariffs_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_tariffs_2025, []).

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
 *   constraint_id: us_tariffs_2025
 *   human_readable: 2025 United States Tariff Policy on Imported Goods
 *   domain: economic/political
 *
 * SUMMARY:
 *   The 2025 U.S. tariff policy represents a broad protective mechanism
 *   imposed on imported goods across multiple sectors, justified by national
 *   security, trade deficit reduction, and domestic manufacturing protection.
 *   The constraint exhibits characteristics of a tangled rope: it provides
 *   genuine coordination benefits to protected industries (solving the
 *   collective action problem of competing against lower-wage producers)
 *   while simultaneously extracting rents from consumers, supply-chain
 *   integrated firms, and trading partner economies. The theater_ratio (0.65)
 *   reflects that tariff policy operates partly through genuine economic
 *   protection (coordination function) and partly through political theater
 *   and threat-making in trade negotiations (performative dimension). The
 *   constraint's extractiveness has increased over the 12-month window from
 *   0.35 to 0.58, indicating both the implementation of broader tariff
 *   coverage and the accumulation of deadweight losses as supply chains
 *   adjust. The suppression level (0.72) reflects significant barriers to
 *   consumer and firm exit: consumers cannot easily access foreign markets;
 *   firms cannot quickly relocate production; import-dependent industries
 *   face double-squeeze (tariffs on inputs and tariffs on final goods). The
 *   constraint is neither a pure coordination mechanism (rope) nor pure
 *   extraction (snare), but a hybrid that combines legitimate industry
 *   protection with significant distributional asymmetries.
 *
 * KEY AGENTS:
 *   - Domestic Manufacturing Sectors (Steel, Automotive, Semiconductors): Primary beneficiary (organized/constrained) — gain protected market access and price support; incentivized to maintain political coalition supporting tariffs
 *   - Domestic Consumers: Primary victim (powerless/trapped) — bear direct cost of import price increases with minimal exit options; geographically dispersed and politically unorganized
 *   - Supply-Chain Integrated Firms (Appliances, Electronics, Automotive Parts): Secondary victim (moderate/constrained) — face tariff cascade on both inputs and outputs; cannot easily relocate supply chains
 *   - Federal Government: Institutional beneficiary (institutional/arbitrage) — captures tariff revenue (~$70B annually) and political credit for protecting jobs; can arbitrage by removing tariffs
 *   - Trading Partner Economies (Vietnam, Mexico, India, China): Victim (powerful/arbitrage at global level) — face demand destruction for labor-intensive exports; high global arbitrage but politically constrained by U.S. market concentration
 *   - Global Supply Chain Ecosystem: Organized agent (organized/constrained) — disrupted coordination mechanisms (just-in-time, specialization); constrained exit due to sunk integration
 *   - Analytical Observer: Sees natural law of comparative advantage being violated; risks naturalizing tariff policy as necessary rather than contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_tariffs_2025, 0.58).
domain_priors:suppression_score(us_tariffs_2025, 0.72).
domain_priors:theater_ratio(us_tariffs_2025, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_tariffs_2025, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_tariffs_2025, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_tariffs_2025, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_tariffs_2025, tangled_rope).
narrative_ontology:human_readable(us_tariffs_2025, "2025 United States Tariff Policy on Imported Goods").
narrative_ontology:topic_domain(us_tariffs_2025, "economic/political").

domain_priors:requires_active_enforcement(us_tariffs_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_tariffs_2025, domestic_manufacturing_protected_sectors).
narrative_ontology:constraint_beneficiary(us_tariffs_2025, import_competing_industries).
narrative_ontology:constraint_beneficiary(us_tariffs_2025, federal_government_revenue).
narrative_ontology:constraint_victim(us_tariffs_2025, domestic_consumers).
narrative_ontology:constraint_victim(us_tariffs_2025, export_dependent_industries).
narrative_ontology:constraint_victim(us_tariffs_2025, supply_chain_integrated_firms).
narrative_ontology:constraint_victim(us_tariffs_2025, trading_partner_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC CONSUMER (SNARE) — Bears direct cost of import price increases with minimal exit option. Cannot easily arbitrage to foreign suppliers; constrained to domestic market. No political voice concentrated enough to oppose tariff coalition. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(us_tariffs_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUPPLY-CHAIN INTEGRATED FIRM (SNARE) — Firms relying on imported intermediate goods face tariff cascade (tariffs on inputs, tariffs on final products). Cannot easily relocate supply chains; constrained by sunk capital in current structure. Bears extraction while competitors in tariff-protected sectors benefit. d≈0.80, f(d)≈1.18, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(us_tariffs_2025, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTECTED MANUFACTURING SECTOR (TANGLED ROPE) — Primary beneficiary with coordination function (tariff barrier solves collective action problem for domestic producers competing against low-wage imports). Also benefits from supply-chain integration (captive domestic suppliers). Constrained exit because tariff removal triggers immediate competitive pressure. d≈0.25, f(d)≈0.10, σ=1.0 → χ≈0.06.
constraint_indexing:constraint_classification(us_tariffs_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT REVENUE (ROPE) — Tariffs generate direct fiscal revenue (~$70B annually at proposed rates) and enable political credit-claiming for 'protecting jobs.' Can exit by removing tariffs; arbitrage is available. Sees tariff as pure coordination mechanism for generating revenue and political capital. d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(us_tariffs_2025, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADING PARTNER ECONOMIES (SNARE) — High-extractiveness from external perspective. Developing economies and specialized exporters (Vietnam, Mexico, India) face demand destruction for labor-intensive exports. Arbitrage available at global level (relocate to other markets) but politically constrained by U.S. market dominance. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(us_tariffs_2025, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL SUPPLY CHAIN ECOSYSTEM (TANGLED ROPE) — Tariffs disrupt coordination mechanisms (just-in-time supply, labor specialization, input sourcing) that have enabled efficiency gains. At the same time, tariffs create extraction through double-margin pricing and comparative disadvantage for non-protected sectors. The ecosystem has constrained exit: dismantling U.S. supply chain integration takes years. d≈0.65, f(d)≈0.92, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(us_tariffs_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADE NEGOTIATION RITUAL (PITON) — Tariff policy represents degraded coordination mechanism. Historical GATT/WTO framework established reciprocal tariff reduction as the coordination problem; tariff reversion is institutional inertia. Modern trade is supply-chain integrated (tariffs on components hurt final producers). The negotiation theater (reciprocal tariff threats, retaliation cycles, bilateral deal-making) persists despite low functional value. theater_ratio≈0.75 reflects that much of the activity is performative threat-making rather than genuine equilibrium-seeking. d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(us_tariffs_2025, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the perspective of comparative advantage theory (Ricardian economics), tariffs represent a constraint against the natural law of specialization. The pressure to produce according to comparative advantage is immutable — tariffs cannot permanently prevent more efficient producers from eventually supplying goods, only raise friction costs. However, the structural data (ε=0.58, suppression=0.72, theater=0.65) contradicts the mountain classification. The engine will detect this as a false summit: comparative advantage is an analytical law, but tariff policy is a contingent institutional extraction mechanism that naturalizes the economic logic without interrogating the distributional asymmetries.
constraint_indexing:constraint_classification(us_tariffs_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_tariffs_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_tariffs_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_tariffs_2025, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_tariffs_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_tariffs_2025, TR),
    TR >= 0.70.

:- end_tests(us_tariffs_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The tariff policy extracts from consumers and supply-chain firms through price increases and resource reallocation costs. The extraction is not total (0.70+) because protected industries genuinely solve a coordination problem (competing against lower-wage countries) and provide measurable domestic production capacity gains. The extractiveness trajectory (0.35→0.48→0.58 over 12 months) reflects both deepening tariff coverage and the realization of deadweight losses as the initial impact (price changes) accumulates into broader supply-chain disruption. Suppression (0.72): High. Multiple barriers prevent exit: consumers cannot arbitrage to foreign suppliers; firms face geographic and capital constraints in relocating supply chains; trading partners face U.S. market dominance constraints. However, suppression is not maximal (0.95+) because some exit routes exist (tariff avoidance via transshipment, supply-chain relocation). Theater ratio (0.65): Moderate-high. The tariff policy contains genuine economic substance (price protection, revenue generation) but also significant performative content (threat-making in negotiations, reciprocal tariff cycles, political credit-claiming). The theater has increased from 0.50 to 0.65, suggesting that much of the policy implementation involves public posturing about trade 'wins' rather than optimal economic optimization. The ritual of bilateral negotiation and reciprocal tariff threats is largely performative — the underlying supply-chain integration makes symmetrical tariff escalation economically damaging to all parties, yet the negotiation theater persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival divide between protected beneficiaries and dispersed victims. Protected manufacturers see pure coordination (Rope from their perspective) — tariffs solve the collective action problem of competing against lower-wage imports without requiring explicit cartelization. The federal government sees institutional coordination (Rope) — tariffs generate revenue and enable political credit-claiming. Consumers and supply-chain firms see extraction (Snare) — they bear costs with no offsetting benefits. Trading partner economies see extraction (Snare) from their perspective — demand destruction without corresponding market access gains. The global supply-chain ecosystem sees a hybrid (Tangled Rope) — disruption of coordination mechanisms (just-in-time) combined with extraction (rent transfers). The trade negotiation system sees institutional degradation (Piton) — the WTO-era coordination framework is being replaced by bilateral threat-making with low functional value. The analytical observer risks seeing natural law (Mountain) — 'tariffs are inevitable national defense' — but the structural data reveals this as a false summit: tariffs are a contingent institutional extraction mechanism, not a law of economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic manufacturers: Beneficiary + constrained → d≈0.25, f(d)≈0.10. Net beneficiary with constrained exit (tariff removal triggers competitive pressure). Consumers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no geographic arbitrage available. Supply-chain firms: Victim + constrained → d≈0.80, f(d)≈1.18. High extraction with some exit (tariff avoidance routes, relocation) but slow and costly. Federal government: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary with immediate exit available (tariff removal). Trading partners: Victim + arbitrage (global level) → d≈0.88, f(d)≈1.32. High extraction with slow exit (market relocation takes years). Global supply chain: Organized victim + constrained → d≈0.65, f(d)≈0.92. Moderate-high extraction with constrained exit (integration is capital-intensive). Trade negotiation system: Institutional actor + constrained → d≈0.35, f(d)≈0.28. Low effective extraction; institutional perspective sees degradation rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is unresolved because the tangled_rope classification requires simultaneous coordination benefits (to protected industries) and extraction (from consumers/supply chains), which the data supports. However, the empirical status of the coordination benefits is contested: omega_1 (tariff efficacy on employment) is the critical uncertainty. If tariffs fail to increase employment — if they concentrate rents without job creation — then the 'coordination' function disappears and the constraint becomes a pure snare (extraction without coordination). The current claimed_type (tangled_rope) is justified by the assumption that tariffs provide real protection to domestic industries, but this assumption is the primary empirical wager. The mandatrophy_resolved flag is false because the theoretical uncertainty (does the coordination component outweigh the extraction component?) remains unresolved until employment data from 2025-2026 is available. Alternative resolution paths: (1) Tariffs increase employment → tangled_rope confirmed. (2) Tariffs concentrate rents without employment growth → snare or piton (institutional theater masking extraction). (3) Retaliation escalates symmetrically → tangled_rope confirmed (symmetric extraction). (4) Retaliation is minimal → snare confirmed (asymmetric extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tariff_efficacy_on_domestic_production,
    'Do broad tariffs actually increase domestic manufacturing capacity and employment, or do they primarily transfer rents to protected producers without net job creation?',
    'Longitudinal employment data in protected sectors (steel, automotive, semiconductors) post-tariff; comparison to counterfactual baseline (industry trends absent tariffs); analysis of wage/employment growth vs. profit concentration',
    'If tariffs increase employment: constraint functions as coordination (Rope gains evidence). If rents concentrate without employment growth: constraint is pure extraction (Snare gains evidence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tariff_efficacy_on_domestic_production, empirical, 'Whether tariffs create employment or concentrate rents').

omega_variable(
    supply_chain_relocation_speed,
    'Can tariff-evading supply chains relocate out of targeted countries fast enough to maintain imports, or is the supply-chain lock-in real?',
    'Monitoring of tariff avoidance routes (transshipment, rule-of-origin arbitrage, relocation announcements); timeline analysis of actual supply-chain shifts vs. tariff implementation dates',
    'If relocation is fast (6-12 months): tariffs are temporary friction, snare classification weakens. If relocation is slow (2+ years) or requires capital investment: lock-in is real, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_relocation_speed, empirical, 'Speed of supply-chain adaptation to tariffs').

omega_variable(
    retaliation_escalation_threshold,
    'At what tariff level do trading partners implement counter-tariffs that offset domestic manufacturing gains?',
    'Modeling of tariff-retaliation cycles; tracking of announced retaliatory tariffs from EU, China, Mexico, Canada; measurement of export-sector job losses vs. protected-sector gains',
    'If retaliation is immediate and symmetric: tangled_rope classification confirmed (coordination + extraction balanced). If retaliation is delayed or asymmetric: snare classification strengthened (extraction without symmetrical coordination failure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retaliation_escalation_threshold, empirical, 'Trading partner retaliation timing and magnitude').

omega_variable(
    consumer_price_pass_through,
    'How much of tariff cost is passed through to consumer prices vs. absorbed by importers/retailers?',
    'Price tracking for tariffed goods (appliances, textiles, electronics); econometric estimation of elasticity of pass-through; comparison of retail prices to tariff rates',
    'If 100% pass-through: consumer snare classification confirmed. If <50% pass-through: importers bear more extraction, tangled_rope classification gains evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_price_pass_through, empirical, 'Price pass-through rate for tariffed goods to consumers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_tariffs_2025, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ustariff_tr_t0, us_tariffs_2025, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ustariff_tr_t6, us_tariffs_2025, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ustariff_tr_t12, us_tariffs_2025, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(ustariff_be_t0, us_tariffs_2025, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ustariff_be_t6, us_tariffs_2025, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ustariff_be_t12, us_tariffs_2025, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_tariffs_2025, resource_allocation).
narrative_ontology:affects_constraint(us_tariffs_2025, global_supply_chain_fragility).
narrative_ontology:affects_constraint(us_tariffs_2025, semiconductor_supply_security).
narrative_ontology:affects_constraint(us_tariffs_2025, inflation_targeting_credibility).

% DUAL FORMULATION NOTE:
% The 2025 tariff policy is downstream of broader trade deficit concerns and domestic manufacturing decline (upstream constraints: manufacturing_competitiveness_gap, trade_deficit_political_salience). The tariff policy itself affects downstream constraints in supply-chain resilience, inflation dynamics, and geopolitical fragmentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_tariffs_2025, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
