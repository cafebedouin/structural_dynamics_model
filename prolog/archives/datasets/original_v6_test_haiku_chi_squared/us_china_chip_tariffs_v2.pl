% ============================================================================
% CONSTRAINT STORY: us_china_chip_tariffs_v2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_chip_tariffs_v2, []).

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
 *   constraint_id: us_china_chip_tariffs_v2
 *   human_readable: US Tariffs on Chinese High-Tech Goods (2024)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The 2024 US tariffs on Chinese high-tech goods (including a 50% tariff on
 *   semiconductors by 2025) represent a strategic industrial policy
 *   intervention that simultaneously coordinates supply-chain decoupling and
 *   extracts from US consumers and importers. The constraint exhibits a high
 *   perspectival gap: domestic chipmakers see protection (Rope), the US
 *   government sees strategic coordination (Rope), consumers see pure
 *   extraction (Snare), tech companies see constrained victimhood
 *   (Snare/Tangled Rope), Chinese makers see market loss with compensatory
 *   protected growth (Tangled Rope), and the global supply chain faces forced
 *   geographic reorganization (Tangled Rope). The theater ratio (0.58)
 *   reflects that tariff justifications invoke strategic security necessity,
 *   but the policy also serves incumbent industry rent-seeking and political
 *   signaling. The extractiveness (0.52) captures the genuine cost transfer:
 *   tariffs shift producer surplus from Chinese and US importers to US
 *   domestic semiconductor producers and government revenue. Mandatrophy is
 *   resolved by recognizing that the tariff is a legitimate Tangled Rope
 *   (coordination + extraction) with a real sunset horizon (as US capacity
 *   scales, protection necessity declines), not a pure Snare masquerading as
 *   strategy.
 *
 * KEY AGENTS:
 *   - US Domestic Semiconductor Manufacturers (Intel, Micron): Primary beneficiary (institutional/arbitrage) — gain market protection, access to CHIPS Act subsidies, and reduced competition from Chinese imports
 *   - US Consumers: Primary victim (powerless/trapped) — absorb tariff pass-through on electronics, computing, and downstream goods; no exit mechanism
 *   - US Tech Companies (Apple, Google, Meta, etc.): Secondary victim (moderate/constrained) — face tariff costs on inputs; constrained exit options (reshoring is slow, offshoring risks US market access)
 *   - US Government: Coordinating beneficiary (powerful/arbitrage) — pursues strategic industrial policy (decoupling, reshoring, supply-chain security); captures tariff revenue
 *   - Chinese Chipmakers: Asymmetric victim-beneficiary (moderate/constrained) — lose US market access but gain protected Asian/emerging market growth as trade redirects
 *   - Global Chip Supply Chain (TSMC, Samsung): Organized actors (organized/mobile) — face efficiency costs from forced decoupling but can leverage supply-chain diversification for new markets
 *   - Tariff Bureaucracy: Institutional maintainer (institutional/constrained) — enforces HTS classification and exemption waivers; maintains performative flexibility
 *   - US Domestic Fab Reshoring Initiative: Organized coalition (organized/constrained) — builds new capacity with sunset logic; expects tariff necessity to decline as capacity matures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_chip_tariffs_v2, 0.52).
domain_priors:suppression_score(us_china_chip_tariffs_v2, 0.68).
domain_priors:theater_ratio(us_china_chip_tariffs_v2, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_chip_tariffs_v2, tangled_rope).
narrative_ontology:human_readable(us_china_chip_tariffs_v2, "US Tariffs on Chinese High-Tech Goods (2024)").
narrative_ontology:topic_domain(us_china_chip_tariffs_v2, "economic/political").

domain_priors:requires_active_enforcement(us_china_chip_tariffs_v2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_domestic_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_government_revenue).
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_labor_in_chipmaking).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, us_consumers).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, us_tech_companies_importing_chips).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, chinese_chipmakers).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, global_chip_supply_chain).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: US CONSUMER (SNARE) — Trapped by tariff pass-through; cannot exit domestic market for cheaper alternatives. Absorbs price increases on electronics, computing hardware, and downstream consumer goods. No effective arbitrage. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: US TECH COMPANY IMPORTING CHIPS (SNARE) — Bears direct tariff costs; options constrained (reshoring is capital-intensive and slow; offshoring risks US market access; lobbying is ineffective against tariff momentum). Strategic vulnerability creates extraction: companies must either absorb costs, raise prices, or reduce margins. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.68.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US DOMESTIC CHIPMAKER (ROPE) — Primary beneficiary. Tariffs reduce competition, support reshoring subsidies (CHIPS Act), and create protected market share. Experiences tariff regime as coordination: government policy coordinates market access for domestic producers. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US GOVERNMENT STRATEGIC INTENT (ROPE) — Tariffs coordinate industrial policy: decoupling supply chains from China, building domestic capacity, and strengthening national security. Government views tariff as solving collective action problem (individual firms lack incentive to build expensive fabs domestically without protection). d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.02. Net beneficiary.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINESE CHIPMAKER (TANGLED ROPE) — Suffers direct extraction (blocked market access); but also benefits from protected domestic market as US tariffs redirect trade to China-friendly suppliers and create incentive for Chinese companies to capture non-US markets. Mixed: loses US market, gains protected Asian/emerging market share. d≈0.60, f(d)≈0.80, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL CHIP SUPPLY CHAIN (TANGLED ROPE) — Experiences both extraction (forced decoupling, inefficiency from geography-driven design) and coordination benefit (tariff creates incentive for supply-chain diversification and risk mitigation). Organized actors (TSMC, Samsung, Intel) can shift production; smaller companies are trapped. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: TARIFF BUREAUCRACY (PITON) — Maintains tariff classification and enforcement system (HTS codes, country-of-origin rules, waiver processes). Theater_ratio=0.58 reflects that enforcement is substantial but also contains performative elements: exemption waiver processes create appearance of flexibility while actual flow remains blocked. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.18.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: US DOMESTIC CHIP RESHORING INITIATIVE (SCAFFOLD) — Temporary coordination with sunset logic: tariffs + CHIPS Act subsidies drive fab construction, but as US capacity matures and supply-chain redundancy is achieved, tariff necessity decreases. Estimated sunset: 10-15 years (by ~2035-2040 when US domestic capacity reaches 25-30% of consumption). d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / COMPARATIVE ADVANTAGE VIEW (MOUNTAIN) — From a civilizational economic perspective, tariffs on high-tech goods violate comparative advantage logic, creating irreversible efficiency losses (deadweight loss, resource misallocation). This perspective sees tariffs as fighting immutable law. However, the structural data (ε=0.52, suppression=0.68) contradicts pure mountainness. The engine identifies this as a false summit: the 'law' is a descriptive economic principle, not a natural law; tariffs demonstrate that political power can override comparative advantage deliberately.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_chip_tariffs_v2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_chip_tariffs_v2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_chip_tariffs_v2, TR),
    TR >= 0.70.

:- end_tests(us_china_chip_tariffs_v2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The tariff transfers substantial surplus from importers and consumers to domestic producers and government. The extraction is real but not total — the policy has genuine strategic rationale (supply-chain security, technological sovereignty), not pure rent-seeking. Over the interval (0-4 years), extractiveness rose from 0.28 to 0.52 as tariff rates escalated and implementation became comprehensive. Suppression (0.68): High. Alternatives are suppressed: negotiated deals with Taiwan/South Korea are politically infeasible (bipartisan protectionism); market-driven reshoring is too slow; consumer exit is blocked by national borders. Tariff enforcement is robust, with limited exemption pathways. Theater ratio (0.58): Moderate. Policy rhetoric emphasizes strategic security (genuine component), but also contains performative elements: tariff exclusion processes create appearance of flexibility while blocking most trade; 'friend-shoring' language masks protectionism; CHIPS Act branding inflates reshoring expectations. Theater has increased over interval as implementation has revealed performative elements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a large perspectival gap driven by directionality and exit options. Domestic chipmakers experience protection (Rope/beneficiary perspective). Consumers experience extraction with no exit (Snare/victim perspective). Tech importers experience mixed effects: they lose market efficiency but some gain from protected downstream markets (Tangled Rope/moderate perspective). Chinese makers experience asymmetric loss-gain (Tangled Rope/constrained perspective). The global supply chain sees efficiency loss but also supply-chain risk mitigation (Tangled Rope/organized perspective). The comparative advantage mountain perspective (tariffs violate economic law) is revealed as a false summit by the actual structural data: tariffs are a deliberate political choice that overrides efficiency, not an inevitable outcome of natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   US Consumers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit options. US Tech Importers: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction; reshoring and offshoring are both blocked. US Domestic Chipmakers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; high exit capacity (can leverage protection or invest globally). US Government: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary; policy is deliberately chosen. Chinese Makers: Mixed (asymmetric victim-beneficiary) + constrained → d≈0.60, f(d)≈0.80. Lose US market but gain protected alternatives. Global Supply Chain: Victim (efficiency) + mobile (organized actors can shift) → d≈0.55, f(d)≈0.75. Moderate extraction for smaller players, lower for multinational corporations.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED by temporal measurement and omega variables. The key omega is 'actual_reshoring_velocity': if US fab capacity scales to cost-competitive maturity within 10-15 years (by ~2035-2040), the tariff has a real sunset and is a legitimate Scaffold. The measurement data shows extractiveness rising from 0.28 to 0.52 over 4 years, but the Tangled Rope classification (not Snare) is justified because (1) the constraint has demonstrable coordination function (supply-chain security, industrial policy), (2) beneficiaries are named (domestic chipmakers, government), (3) victims are identified (consumers, importers), and (4) enforcement is active. If reshoring fails and tariff persists after 2035 with extractiveness ≥0.66, the constraint should be reclassified as Snare. Currently, mandatrophy is resolved by treating the tariff as a high-extraction Tangled Rope with a generational sunset, not as a masked Snare with indefinite extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_reshoring_velocity,
    'Will US domestic fab capacity actually scale to offset Chinese competitive advantage before tariff sunset?',
    'Tracking fab construction timelines (fabs take 5-7 years to build), yield maturation curves, cost-per-unit convergence with Taiwan/South Korea production. Measurement of actual CapEx deployed under CHIPS Act.',
    'If reshoring succeeds: tariff has real sunset and is legitimate scaffold (temporary). If reshoring fails: tariff becomes permanent extraction (Snare), and mandatrophy is unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(actual_reshoring_velocity, empirical, 'Whether US fab reshoring reaches cost-competitive maturity within 15 years').

omega_variable(
    china_retaliatory_capacity,
    'Can China effectively retaliate against US tariffs through counter-tariffs, export controls on rare earths, or supply-chain weaponization?',
    'Monitoring Chinese retaliatory tariffs on US agricultural/industrial goods; tracking rare earth export restrictions; analyzing supply-chain vulnerability indices for critical US industries dependent on China.',
    'If retaliation is effective: US consumers and tech companies face escalating extraction (χ rises). If retaliation is limited: US can maintain tariff regime with lower cost to US importers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(china_retaliatory_capacity, empirical, 'Effectiveness of Chinese counter-tariffs and supply-chain retaliation').

omega_variable(
    allied_decoupling_momentum,
    'Do US allies (EU, Japan, South Korea, Taiwan) voluntarily decouple from Chinese chips, or do they maintain dual-sourcing to preserve cost advantages?',
    'Trade flow analysis showing whether allies increase US chip purchases or maintain Chinese sourcing; interviews with procurement officers; analysis of diversification strategies in ally supply chains.',
    'If allies decouple: tariff benefits extend globally, offsetting extraction on US importers. If allies keep Chinese options: US tariff extracts from US companies while allies capture cost savings, making US consumers/companies bear disproportionate cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_decoupling_momentum, empirical, 'Whether US allies voluntarily decouple or maintain dual-sourcing').

omega_variable(
    strategic_purity_vs_extraction_hybrid,
    'Is the tariff regime genuinely a strategic industrial policy (Rope/Scaffold coordination), or has it become a rent-seeking extraction mechanism?',
    'Analysis of tariff revenue allocation: if revenues fund broad reshoring/R&D, it''s coordination. If revenues accrue to incumbent chipmakers without spurring new capacity, it''s extraction. Measurement of policy goal articulation vs. actual implementation.',
    'If strategic: classification as Tangled Rope with real sunset is correct. If extraction: classification should shift toward Snare from more perspectives; mandatrophy would require resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_purity_vs_extraction_hybrid, conceptual, 'Whether tariff is strategic policy or rent-seeking extraction hybrid').

omega_variable(
    consumer_price_passthrough_rate,
    'What proportion of tariff costs are passed through to US consumers vs. absorbed by companies or suppliers?',
    'Price index tracking for consumer electronics; regression analysis of price changes against tariff schedules; survey of corporate margin compression.',
    'If passthrough is high (>75%): consumers bear full extraction cost (Snare classification from consumer perspective is correct). If passthrough is low (<50%): companies absorb costs and extraction is mitigated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_price_passthrough_rate, empirical, 'Tariff cost passthrough to consumer prices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_chip_tariffs_v2, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_tr_t0, us_china_chip_tariffs_v2, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tariff_tr_t2, us_china_chip_tariffs_v2, theater_ratio, 2, 0.5).
narrative_ontology:measurement(tariff_tr_t4, us_china_chip_tariffs_v2, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(tariff_be_t0, us_china_chip_tariffs_v2, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tariff_be_t2, us_china_chip_tariffs_v2, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(tariff_be_t4, us_china_chip_tariffs_v2, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_chip_tariffs_v2, resource_allocation).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, semiconductor_supply_chain_fragmentation).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, us_china_geopolitical_decoupling).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, chips_act_subsidy_effectiveness).

% DUAL FORMULATION NOTE:
% The US tariff regime is downstream of broader US-China geopolitical decoupling (higher-level constraint) and affects semiconductor supply-chain fragmentation (sister constraint with shared victims/beneficiaries). The tariff also shares institutional overlap with CHIPS Act subsidies (dual implementation of same strategic intent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_chip_tariffs_v2, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
