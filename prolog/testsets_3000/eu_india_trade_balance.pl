% ============================================================================
% CONSTRAINT STORY: eu_india_trade_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_india_trade_balance, []).

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
 *   constraint_id: eu_india_trade_balance
 *   human_readable: EU-India Trade Balance Asymmetry and Structural Extraction
 *   domain: international_trade/political_economy
 *
 * SUMMARY:
 *   The EU-India trade relationship exhibits structural extraction of Indian
 *   manufacturing and agricultural sectors by EU competitors, masked by the
 *   rhetoric of 'mutually beneficial trade' and 'comparative advantage.' The
 *   constraint combines genuine coordination (both parties benefit from
 *   market access, technology transfer, FDI) with asymmetric extraction (EU
 *   capital advantages, colonial-era institutional path-dependence,
 *   suppression of Indian manufacturing through competitive pressure and
 *   non-tariff barriers). The extractiveness has increased over the
 *   measurement period (0.42 → 0.58) as Indian manufacturing has developed
 *   capacity that threatens EU incumbents, triggering increased NTB
 *   enforcement and regulatory tightening. The theater ratio (0.48, low for a
 *   Tangled Rope) reflects that the extractive mechanism is mostly structural
 *   rather than performative — the imbalance requires active enforcement
 *   through tariff schedules and dispute mechanisms rather than theatrical
 *   maintenance.
 *
 * KEY AGENTS:
 *   - EU Manufacturing Sector: Primary beneficiary (institutional/arbitrage) — captures market access, supply chain integration, protected home market. High exit options (alternative markets, capital mobility). Low experienced suppression.
 *   - EU Agricultural Exporters: Secondary beneficiary (organized/arbitrage) — benefit from CAP subsidies and preferential market access. Exit options abundant. Low suppression.
 *   - Indian Manufacturing SMEs: Primary victim (powerless/trapped) — face overwhelming EU competition, tariff barriers, compliance costs. No meaningful exit options. High suppression, concentrated extraction.
 *   - Indian Government & Domestic Market Protection: Secondary victim (organized/constrained) — must balance trade benefits against domestic industry protection. Constrained by trade agreement obligations. Moderate-high suppression.
 *   - Indian Agricultural Sector: Mixed role (moderate/constrained) — benefits from EU market access but faces CAP subsidy competition. Constrained exit due to EU dominance.
 *   - Analytical Observer: System-level view (analytical/analytical) — sees genuine coordination function alongside asymmetric extraction enabled by capital advantage and institutional path-dependence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_india_trade_balance, 0.58).
domain_priors:suppression_score(eu_india_trade_balance, 0.52).
domain_priors:theater_ratio(eu_india_trade_balance, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_india_trade_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_india_trade_balance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(eu_india_trade_balance, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_india_trade_balance, tangled_rope).
narrative_ontology:human_readable(eu_india_trade_balance, "EU-India Trade Balance Asymmetry and Structural Extraction").
narrative_ontology:topic_domain(eu_india_trade_balance, "international_trade/political_economy").

domain_priors:requires_active_enforcement(eu_india_trade_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_india_trade_balance, eu_manufacturing_sector).
narrative_ontology:constraint_beneficiary(eu_india_trade_balance, eu_agricultural_exporters).
narrative_ontology:constraint_victim(eu_india_trade_balance, indian_manufacturing_smes).
narrative_ontology:constraint_victim(eu_india_trade_balance, indian_domestic_market_protection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN MANUFACTURING SMEs (SNARE) — Face overwhelming competition from EU manufacturers with established supply chains, tariff advantages, and capital. Exit requires either competing on terms set by EU trade agreements or relocating production. Suppression is high: tariff structures, NTBs (non-tariff barriers), and compliance costs create insurmountable barriers. Extraction concentrated on this powerless agent with no meaningful alternatives.
constraint_indexing:constraint_classification(eu_india_trade_balance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIAN GOVERNMENT / DOMESTIC MARKET PROTECTION (TANGLED ROPE) — Must coordinate trade benefits (access to EU market, technology transfer, FDI attraction) while protecting domestic industries. Genuine coordination function exists (mutual market access theoretically benefits both). But asymmetric extraction embedded: EU sector competitiveness advantage, capital concentration, and regulatory capture of trade dispute mechanisms favor EU interests. High suppression through dispute resolution structures and conditional aid tied to trade liberalization.
constraint_indexing:constraint_classification(eu_india_trade_balance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EU MANUFACTURING & AGRICULTURAL SECTORS (ROPE) — Primary beneficiary. Experience the constraint as pure coordination: market access, tariff certainty, supply chain integration, regulatory harmonization. Net extraction flow toward this actor. Exit options abundant (alternative markets, capital mobility, diversified supply chains). Low experienced suppression — integration is voluntary and beneficial for these actors.
constraint_indexing:constraint_classification(eu_india_trade_balance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIAN AGRICULTURAL & EXPORT-ORIENTED SECTORS (TANGLED ROPE) — Moderate power actors with mixed experience. Benefit from EU market access (exports of textiles, agricultural products, IT services). But face significant extraction: EU Common Agricultural Policy subsidies undercut Indian agricultural exports; EU industrial goods flood Indian market. Constrained exit: shifting to other markets faces tariff barriers; domestic production faces EU competition.
constraint_indexing:constraint_classification(eu_india_trade_balance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: EMERGING MULTILATERAL ALTERNATIVES (SCAFFOLD) — Regional trade blocs (RCEP, African Continental Free Trade Area), bilateral agreements with other economies, and digital trade pathways provide alternative coordination mechanisms with lower extraction. Theater low compared to traditional bilateral trade regimes. This perspective sees the EU-India bilateral trade constraint as temporary (5-15 year sunset) as India diversifies trade partnerships and develops manufacturing capacity. Exit path visible: supply chain regionalization, ASEAN integration, direct EU competition from other suppliers.
constraint_indexing:constraint_classification(eu_india_trade_balance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLONIAL-ERA INSTITUTIONAL INERTIA (PITON) — The EU-India trade asymmetry perpetuates structural patterns from the colonial period: India as raw materials/agricultural supplier, EU as manufacturing/industrial exporter. This institutional arrangement persists largely through inertia and regulatory capture despite India's emergence as a manufacturing power. The theatrical maintenance involves invoking 'comparative advantage' and 'development stage' to justify asymmetries that are actually path-dependent institutional design. Theater ratio reflects the performative framing of asymmetric extraction as mutually beneficial specialization.
constraint_indexing:constraint_classification(eu_india_trade_balance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as a hybrid of genuine coordination (mutual market access, economic integration) and asymmetric extraction (capital asymmetry, institutional design favoring EU, regulatory capture of dispute mechanisms, suppression of Indian manufacturing through competitive pressure). Base extractiveness (0.58) reflects moderate-high extraction with genuine coordination benefits. The constraint is neither pure extraction nor pure coordination — it requires active enforcement through trade agreements and dispute mechanisms.
constraint_indexing:constraint_classification(eu_india_trade_balance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_india_trade_balance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_india_trade_balance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_india_trade_balance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_india_trade_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_india_trade_balance, TR),
    TR >= 0.70.

:- end_tests(eu_india_trade_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from Indian manufacturing and agricultural sectors through competitive pressure, tariff structures, and non-tariff barriers. But extraction is not maximal (0.70+) because genuine market access and technology transfer benefits exist for India, and exit pathways are emerging through RCEP and other trade blocs. The escalation from 0.42 to 0.58 reflects increased regulatory tightening as Indian manufacturing capacity has developed and threatened EU incumbents. Suppression (0.52): Moderate-high. Barriers include tariff schedules, NTBs (safety standards, certification), compliance costs, capital requirements, and path-dependence in supply chains. But suppression is not total — Indian firms can and do penetrate EU markets in textiles, agribusiness, and IT services. Theater ratio (0.48): Low-moderate. The extractive mechanism is largely structural (tariff schedules, regulatory standards, capital requirements) rather than performative. Narrative justification ('comparative advantage,' 'development stage') provides some cover, but the primary enforcement is through institutional design rather than theatrical maintenance. Theater has increased slightly over the interval as regulatory narrative emphasis has increased in response to Indian manufacturing challenge.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (EU manufacturing) sees pure coordination (Rope) — market access benefits, supply chain integration, regulatory certainty. The trapped victim (Indian manufacturing SMEs) sees pure extraction (Snare) — overwhelming competition with no exit. The constrained victim (Indian government) sees mixed extraction-coordination (Tangled Rope) — trade benefits alongside suppression of domestic industry. The organized agents (EU agriculture, Indian export sectors) see constrained extraction (Tangled Rope) — benefits with significant barriers. The emerging trade alternatives perspective sees this as temporary (Scaffold) — RCEP and supply chain diversification provide sunset. The civilizational perspective sees institutional inertia (Piton) — colonial-era patterns maintained through regulatory capture and historical path-dependence. The perspectival gap arises from differential exit options and structural positions: beneficiaries perceive coordination because they can arbitrage alternatives; trapped victims perceive extraction because they cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   EU manufacturing beneficiaries with arbitrage exit options experience low or negative effective extraction (d ≈ 0.05-0.15, f(d) ≈ -0.12 to -0.01). Indian manufacturing victims with trapped exit experience high effective extraction (d ≈ 0.90, f(d) ≈ 1.35). Indian government with constrained exit and mixed beneficiary/victim status experiences moderate extraction (d ≈ 0.55, f(d) ≈ 0.75). The chi formula χ = ε × f(d) × σ(S) applies scope modifier σ(global) = 1.2, so effective extractiveness for powerless agents reaches 0.58 × 1.35 × 1.2 ≈ 0.94, confirming Snare classification from their perspective. For institutional beneficiaries with arbitrage exit, chi becomes 0.58 × (-0.12) × 1.2 ≈ -0.08, confirming Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by maintaining distinct classification legitimacy across perspectives. EU manufacturers genuinely experience coordination (Rope) — trade agreements enable their exports and market access. Indian SMEs genuinely experience extraction (Snare) — they face overwhelming competitive pressure with suppressed alternatives. The Indian government genuinely experiences hybrid extraction-coordination (Tangled Rope) — real benefits from trade alongside real suppression of domestic industry. The constraint is neither mislabeled extraction-as-coordination nor coordination-as-extraction — it IS both, distributed across different power positions. The resolution prevents false universalization: if we used only the beneficiary's perspective, we would naturalize Rope as the 'correct' classification and ignore the Snare experienced by Indian manufacturing. If we used only the victim's perspective, we would erase the real coordination benefits India gains from EU market access and technology transfer. Tangled Rope at the analytical level captures that both are true simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    comparative_advantage_vs_structural_asymmetry,
    'Does the EU-India trade imbalance reflect genuine comparative advantage (mutually beneficial specialization) or structural asymmetry requiring extraction enforcement?',
    'Counterfactual analysis: comparison of trade flows if regulatory barriers were equalized, capital access were equivalent, and historical path-dependence were removed. Decomposition of trade deficits by sector and profitability.',
    'If comparative advantage: constraint reclassifies toward Rope (lower extractiveness, lower suppression). If structural asymmetry: remains Tangled Rope or escalates toward Snare (higher extractiveness, higher suppression required to maintain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_advantage_vs_structural_asymmetry, empirical, 'Distinguishing comparative advantage from structural extraction').

omega_variable(
    manufacturing_capacity_trajectory,
    'Will Indian manufacturing capacity and technology acquisition from EU FDI eventually equilibrate the trade relationship, or does the constraint itself suppress that equilibration?',
    'Longitudinal tracking of Indian manufacturing export sophistication, capital intensity, and technology adoption rates relative to EU competition. Analysis of whether technology transfer is occurring or being restricted through IPR enforcement.',
    'If equilibration occurs: constraint is genuinely temporary (Scaffold perspective validated). If constrained: escalation toward Snare indicates structural lock-in preventing Indian development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_capacity_trajectory, empirical, 'Whether the constraint permits or suppresses Indian manufacturing development').

omega_variable(
    regulatory_capture_in_dispute_resolution,
    'Do trade dispute mechanisms (WTO, bilateral arbitration) exhibit regulatory capture favoring EU interests, or do they provide neutral enforcement?',
    'Analysis of dispute outcomes by sector, win rates by complainant power asymmetry, and correlation between regulatory complexity and dispute likelihood.',
    'If neutral: suppression (0.52) is overstated; constraint reclassifies toward lower-extraction forms. If captured: suppression is understated; constraint escalates toward higher-extraction forms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_in_dispute_resolution, empirical, 'Whether dispute mechanisms are regulatory-captured').

omega_variable(
    nontariff_barrier_functional_role,
    'Are NTBs (regulatory standards, certification requirements, safety protocols) legitimate coordination costs or instruments of trade suppression?',
    'Comparative analysis of NTB stringency for EU exporters vs Indian exporters; examination of whether standards are applied asymmetrically; cost-benefit analysis of compliance.',
    'If legitimate: suppression reflects genuine coordination cost (lower omega impact). If asymmetric: suppression is functional extraction mechanism (higher omega impact, escalates constraint type).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nontariff_barrier_functional_role, empirical, 'Whether NTBs are legitimate standards or extraction instruments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_india_trade_balance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eutib_tr_t0, eu_india_trade_balance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eutib_tr_t5, eu_india_trade_balance, theater_ratio, 5, 0.42).
narrative_ontology:measurement(eutib_tr_t10, eu_india_trade_balance, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(eutib_be_t0, eu_india_trade_balance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eutib_be_t5, eu_india_trade_balance, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(eutib_be_t10, eu_india_trade_balance, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_india_trade_balance, resource_allocation).
narrative_ontology:affects_constraint(eu_india_trade_balance, eu_agricultural_subsidy_regime).
narrative_ontology:affects_constraint(eu_india_trade_balance, global_supply_chain_asymmetry).
narrative_ontology:affects_constraint(eu_india_trade_balance, developing_economy_regulatory_capture).

% DUAL FORMULATION NOTE:
% EU-India trade balance decomposes into three structurally distinct constraints: (1) agricultural subsidy extraction (CAP → affects_constraints), (2) manufacturing tariff/NTB barriers (this story), and (3) technology transfer and IP enforcement (affects downstream). Each has different ε and omega variables. This story focuses on manufacturing sector extraction as the dominant mechanism. Network links show upstream agricultural subsidy constraint and downstream supply chain integration feedback.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_india_trade_balance, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
