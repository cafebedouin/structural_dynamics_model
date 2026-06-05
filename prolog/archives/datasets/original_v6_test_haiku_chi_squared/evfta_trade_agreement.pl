% ============================================================================
% CONSTRAINT STORY: evfta_trade_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evfta_trade_agreement, []).

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
 *   constraint_id: evfta_trade_agreement
 *   human_readable: EU-Vietnam Free Trade Agreement (EVFTA)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU-Vietnam Free Trade Agreement (EVFTA), ratified in 2020 after a
 *   decade of negotiation, eliminates nearly all tariffs between the EU and
 *   Vietnam, creating reciprocal but asymmetrically distributed benefits. On
 *   its surface, EVFTA is presented as a win-win coordination mechanism:
 *   Vietnam gains market access for labor-intensive manufactures; the EU
 *   gains access to lower-cost supply chains and investment opportunities.
 *   But the constraint embeds structural extraction mechanisms that operate
 *   across multiple institutional and worker populations. Vietnamese
 *   manufacturing workers face intensified cost-cutting pressure from
 *   multinational buyers exploiting tariff-free supply chains. Vietnamese
 *   small agricultural producers cannot compete with subsidized EU
 *   agribusiness in tariff-free markets. EU manufacturing workers face wage
 *   pressure and deindustrialization as production relocates to lower-cost
 *   Vietnam. EU agricultural producers lose price protection in commodity
 *   markets. The agreement includes labor-standards clauses and environmental
 *   commitments, but these function partly as performative compliance theater
 *   (theater_ratio=0.58) rather than hard enforcement mechanisms, enabling a
 *   scaffold perspective where institutional capacity building could
 *   eventually reduce the need for external oversight. The constraint also
 *   exhibits piton characteristics: the agreement performs elaborate
 *   ceremonial negotiation and national sovereignty rituals while delivering
 *   regulatory harmonization that WTO rules already mandated, indicating
 *   institutional degradation. The analytical observer risks naturalizing
 *   EVFTA as an inevitable consequence of comparative advantage doctrine
 *   (mountain perspective), but the structural data reveals this as a false
 *   summit: the underlying factor endowments (labor cost, technology access)
 *   are products of historical inequality and path dependence, not natural
 *   law.
 *
 * KEY AGENTS:
 *   - Vietnamese Manufacturing Workers: Primary victim (powerless/trapped) — face intensified cost-cutting pressure; cannot exit garment, electronics, footwear sectors
 *   - Vietnamese Small Agricultural Producers: Primary victim (powerless/trapped) — tariff-free EU competition eliminates price protection; cannot scale to industrial production
 *   - EU Manufacturing Workers: Primary victim (powerless/trapped) — wage pressure and deindustrialization from labor-cost competition; cannot relocate without losing social infrastructure
 *   - EU Agricultural Producers: Secondary victim (moderate/constrained) — lose price protection; can exit through consolidation/automation but land-based communities are structurally trapped
 *   - Vietnamese Export Manufacturers: Primary beneficiary (institutional/arbitrage) — gain tariff-free EU market access; can arbitrage labor-cost differentials
 *   - EU Financial Services and Capital: Primary beneficiary (institutional/arbitrage) — benefit from supply-chain investment opportunities; can reallocate capital globally
 *   - Multinational Supply Chain Integrators: Mixed actor (organized/constrained) — benefit from tariff reduction; also extract from workers via competitive bidding
 *   - Vietnamese State Apparatus: Hybrid actor (institutional/constrained) — benefits from FDI and manufacturing growth; constrained by tariff commitments and labor-rights conditionality
 *   - EU Trade Commission: Enforcer (institutional/constrained) — coordinates EU trade policy; suppresses partner policy autonomy through enforcement mechanisms
 *   - Labor Rights and Environmental NGO Coalition: Organized agents (organized/mobile) — building external scaffolding for labor/environmental standards; sunset logic if institutions mature
 *   - WTO/Multilateral Trade System: Institutional actor (institutional/constrained) — piton perspective: bilateral framework performs sovereignty rituals while delivering predetermined WTO harmonization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent inequality as natural law of comparative advantage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evfta_trade_agreement, 0.52).
domain_priors:suppression_score(evfta_trade_agreement, 0.68).
domain_priors:theater_ratio(evfta_trade_agreement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evfta_trade_agreement, extractiveness, 0.52).
narrative_ontology:constraint_metric(evfta_trade_agreement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(evfta_trade_agreement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evfta_trade_agreement, tangled_rope).
narrative_ontology:human_readable(evfta_trade_agreement, "EU-Vietnam Free Trade Agreement (EVFTA)").
narrative_ontology:topic_domain(evfta_trade_agreement, "economic/political").

domain_priors:requires_active_enforcement(evfta_trade_agreement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evfta_trade_agreement, vietnamese_export_manufacturers).
narrative_ontology:constraint_beneficiary(evfta_trade_agreement, eu_financial_services).
narrative_ontology:constraint_beneficiary(evfta_trade_agreement, multinational_supply_chain_integrators).
narrative_ontology:constraint_victim(evfta_trade_agreement, vietnamese_domestic_labor).
narrative_ontology:constraint_victim(evfta_trade_agreement, vietnamese_small_agricultural_producers).
narrative_ontology:constraint_victim(evfta_trade_agreement, eu_manufacturing_workers).
narrative_ontology:constraint_victim(evfta_trade_agreement, eu_agricultural_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VIETNAMESE MANUFACTURING WORKERS (SNARE) — Trapped by tariff elimination and labor cost competition. Cannot exit garment, electronics, and footwear sectors without catastrophic loss of livelihood. EVFTA amplifies extraction through intensified cost-cutting by multinational buyers. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(evfta_trade_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VIETNAMESE SMALL AGRICULTURAL PRODUCERS (SNARE) — Tariff-free EU competition eliminates price protection; cannot scale to industrial production; trapped in low-margin commodity cultivation. Labor-intensive rice and coffee face flood from mechanized EU agribusiness. d≈0.90, f(d)≈1.35, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(evfta_trade_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: EU MANUFACTURING WORKERS (SNARE) — Tariff elimination opens manufacturing sectors to Vietnamese competition with 5-10x wage differential. Cannot relocate without abandoning social infrastructure. Deindustrialization forces downward wage pressure or displacement. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.69.
constraint_indexing:constraint_classification(evfta_trade_agreement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: EU AGRICULTURAL SECTOR (SNARE) — Tariff elimination on Vietnamese coffee, rice, and fruit threatens CAP-subsidized producers. Some exit through consolidation/automation possible, but regional farm communities are structurally trapped (land cannot be relocated). d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(evfta_trade_agreement, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VIETNAMESE EXPORT MANUFACTURERS (ROPE) — Multinational garment, electronics, and footwear firms benefit from tariff-free EU market access and EU supply chain integration. Export growth, profit repatriation, capital inflow. Can arbitrage labor cost differential via production relocation. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Net beneficiary; coordination function: tariff elimination solves collective market access problem for exporters.
constraint_indexing:constraint_classification(evfta_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EU FINANCIAL SERVICES AND CAPITAL (ROPE) — EU banks, investment funds, and institutional capital benefit from tariff elimination creating supply-chain investment opportunities in Vietnam. Portfolio diversification, capital gains in Vietnamese manufacturing growth. Can exit via capital reallocation. d≈0.10, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; coordination function: tariff reduction solves market access and capital flow harmonization.
constraint_indexing:constraint_classification(evfta_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: MULTINATIONAL SUPPLY CHAIN INTEGRATORS (TANGLED ROPE) — Organizations like Uniqlo, Nike, Samsung that operate integrated supply chains across EU-Vietnam. Benefit from tariff elimination (coordination function: reduces supply chain friction). Also extract from workers via competitive bidding between Vietnam and EU suppliers; suppress wage-rising coalitions. d≈0.48, f(d)≈0.62, σ=1.2 → χ≈0.39. Mixed: both coordination benefit and extraction mechanism.
constraint_indexing:constraint_classification(evfta_trade_agreement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: VIETNAMESE STATE APPARATUS (TANGLED ROPE) — Benefits from FDI inflow, manufacturing export growth, and geopolitical integration with EU. Also constrained by commitment to tariff elimination (limits future infant-industry protection). Suppression via labor-rights conditionality clauses (EVFTA labor standards). d≈0.52, f(d)≈0.67, σ=1.0 → χ≈0.35. Both coordination (access to EU markets) and constraint (limited policy autonomy).
constraint_indexing:constraint_classification(evfta_trade_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: EU TRADE COMMISSION (TANGLED ROPE) — Coordinates EU trade policy and enforces EVFTA rules. Coordination function: tariff harmonization solves collective EU trade negotiation problem. Extraction/suppression: enforcement mechanisms (dispute resolution, labor inspections) can suppress policy autonomy of trading partners. d≈0.48, f(d)≈0.62, σ=1.1 → χ≈0.35. Enforcer role; hybrid beneficiary/victim status.
constraint_indexing:constraint_classification(evfta_trade_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 10: LABOR RIGHTS AND ENVIRONMENTAL NGO COALITION (SCAFFOLD) — EVFTA includes labor-standards clauses and environmental commitments with sunset logic: as Vietnamese labor regulations strengthen and environmental enforcement improves, the agreement's corrective mechanisms become less necessary. Current theater_ratio=0.58 reflects partially performative compliance monitoring; as institutions mature, the external scaffolding can be removed. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.22. Sunset: institutional capacity building reduces need for external oversight within 15-20 years.
constraint_indexing:constraint_classification(evfta_trade_agreement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 11: WTO/MULTILATERAL TRADE SYSTEM (PITON) — EVFTA represents institutional ceremony around tariff elimination that was largely inevitable under WTO rules. The bilateral framework performs national sovereignty (detailed negotiation, separate signing) while delivering regulatory harmonization that WTO already mandated. theater_ratio=0.58 reflects rituals of negotiation and sovereignty assertion that mask predetermined outcomes. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.19. Degraded: agreement maintains ritual autonomy while delivering predetermined technical harmonization.
constraint_indexing:constraint_classification(evfta_trade_agreement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 12: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From comparative advantage doctrine, tariff elimination is a natural law of optimal trade: Vietnam has labor cost advantage, EU has capital/technology advantage; free trade maximizes total surplus and delivers welfare gains to all. This perspective sees EVFTA as inevitable consequence of economic logic, not contingent institutional choice. However, structural data (ε=0.52, suppression=0.68) contradicts mountain classification. The engine's false summit detector will flag this: the 'natural' distribution of comparative advantage is contingent on pre-existing factor endowments, which are products of colonialism, inequality, and path dependence — not natural law. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.71.
constraint_indexing:constraint_classification(evfta_trade_agreement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evfta_trade_agreement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evfta_trade_agreement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evfta_trade_agreement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evfta_trade_agreement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evfta_trade_agreement, TR),
    TR >= 0.70.

:- end_tests(evfta_trade_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The agreement provides genuine coordination benefits (market access, supply-chain integration) valued at ~0.38 baseline, but overlaid with extraction mechanisms: wage-suppression via labor-cost competition, capital repatriation, policy autonomy constraints. The trajectory shows extractiveness rising from 0.38 to 0.52 over 6 years as multinational cost-cutting intensifies and worker displacement accumulates. Suppression (0.68): High. Multiple barriers prevent exit or resistance: (1) workers trapped in sectors with no alternative employment; (2) agricultural producers cannot relocate land-based production; (3) Vietnamese state constrained by legal EVFTA commitments and geopolitical dependence on EU markets; (4) labor organizing suppressed by multinational capital mobility (relocation threat); (5) WTO rules make unilateral tariff reimposition illegal. Theater ratio (0.58): Moderate-high. The agreement performs elaborate ceremonial negotiation, sustainability impact assessments, and labor-standards clauses, but enforcement is weak (theater increases from 0.42 to 0.58 as compliance monitoring reveals limited actual constraint on wage suppression). Claimed type: Tangled Rope. The agreement simultaneously provides coordination benefits (market access, supply-chain efficiency) and asymmetric extraction (wage suppression, policy autonomy loss). Both are structural, not incidental.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full range of perspectival disagreement. Vietnamese workers see pure snare (d≈0.92, χ≈0.72) — trapped with no coordination benefit. EU workers see pure snare (d≈0.88, χ≈0.69) — wage pressure with no offsetting gain. Multinational exporters see pure rope (d≈0.08, χ≈-0.07) — coordination benefit, net beneficiary. Multinational supply-chain integrators see tangled rope (d≈0.48, χ≈0.39) — both coordination and extraction. The Vietnamese state sees tangled rope (d≈0.52, χ≈0.35) — FDI benefit offset by commitment constraint. The labor-rights coalition sees scaffold (d≈0.55, χ≈0.22) — temporary problem with sunset via institutional maturation. The WTO system sees piton (d≈0.50, χ≈0.19) — degraded: ceremonial sovereign negotiation masking predetermined technical harmonization. The analytical observer risks seeing mountain (natural law of comparative advantage) but the false summit detector catches this: the underlying inequality is contingent, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Vietnamese workers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. EU workers: Victim + trapped → d≈0.88, f(d)≈1.32. High extraction. Vietnamese agricultural producers: Victim + trapped → d≈0.90, f(d)≈1.35. High extraction. EU agricultural producers: Victim + constrained (can consolidate, but land cannot relocate) → d≈0.78, f(d)≈1.18. Significant extraction. Vietnamese exporters: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. EU capital: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.10. Net beneficiary. Multinational integrators: Mixed + constrained (trapped by supply-chain dependencies) → d≈0.48, f(d)≈0.62. Tangled. Vietnamese state: Mixed + constrained (benefits from FDI, constrained by legal commitments) → d≈0.52, f(d)≈0.67. Tangled. Labor coalition: Organized + mobile (can shift focus to different campaigns) → d≈0.55, f(d)≈0.75. Low effective extraction with exit option. WTO system: Institutional + constrained (bound by previous commitments) → d≈0.50, f(d)≈0.65. Degraded performer. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. False summit risk.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: EVFTA satisfies all three gates for tangled rope classification: (1) Genuine coordination function (tariff elimination solves collective market-access problem for exporters; enables supply-chain integration). (2) Asymmetric extraction (wage suppression, capital repatriation, policy autonomy loss disproportionately affect workers and developing-country state). (3) Active enforcement (tariff commitments are legally binding; labor-standards clauses require active monitoring; dispute-resolution mechanisms enforce compliance). The mandatrophy arises from the temptation to classify EVFTA as either pure coordination (rope: 'trade creates mutual benefit') or pure extraction (snare: 'developed-world capital exploits developing workers'). The correct classification is BOTH, simultaneously and structurally. The agreement delivers genuine gains for exporting manufacturers and capital (rope dynamics from their perspective) while imposing genuine costs on workers and small producers (snare dynamics from their perspective). This is not a measurement ambiguity — it is a perspectival reality. Workers and exporters are in genuinely different structural positions relative to the same constraint. The engine's multi-perspective architecture resolves this by computing different χ values for different (P,T,E,S) tuples: Vietnamese workers experience high effective extraction (χ≈0.72), exporters experience negative extraction (χ≈-0.07), the state experiences moderate extraction (χ≈0.35). All perspectives are correct; the constraint is structurally hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_standard_enforcement_effectiveness,
    'Do EVFTA labor-standards clauses actually constrain wage-suppression extraction, or do they function as performative compliance theater?',
    'Longitudinal monitoring of Vietnamese wage growth, union formation rates, and strike frequency pre- vs post-EVFTA; audit frequency and penalty severity for labor violations',
    'If effective (penalties bite, wages rise): scaffold perspective confirmed, suppression declines over time. If theater (penalties rare, wages stagnant): snare perspective confirmed, suppression persists and may increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_standard_enforcement_effectiveness, empirical, 'Effectiveness of EVFTA labor standards enforcement').

omega_variable(
    manufacturing_job_displacement_threshold,
    'What rate of EU manufacturing job displacement constitutes acceptable structural adjustment vs extractive deindustrialization?',
    'Historical comparison of wage/employment trends in prior EU FTAs (CETA, Korea, etc); correlation between job loss and worker retraining success rates',
    'If displacement < 5%/year and retraining successful: tangled rope (mixed coordination/extraction). If displacement > 8%/year and retraining fails: snare (pure extraction from workers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_job_displacement_threshold, preference, 'Threshold for job displacement acceptability').

omega_variable(
    vietnamese_infant_industry_protection_forgone,
    'What productive industrial capacity could Vietnam have developed if it had retained tariff protection for key sectors (e.g., machinery, electronics assembly)?',
    'Counterfactual industrial development analysis; comparison with Bangladesh, India tariff-protected growth trajectories; simulation of domestic value-added growth under phased vs immediate liberalization',
    'If significant forgone capacity (10-20% lower manufacturing complexity): suggests long-term extraction via technology lock-in. If minimal difference: EVFTA accelerates development path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vietnamese_infant_industry_protection_forgone, empirical, 'Counterfactual analysis of forgone infant-industry protection benefits').

omega_variable(
    eu_agricultural_consolidation_sustainability,
    'Can EU agricultural sector absorb Vietnamese competition without regional collapse of small-farm communities?',
    'Demographic analysis of farm operator age, succession rates, land consolidation trends; comparison with prior trade shocks (CAP reform, MERCOSUR access)',
    'If consolidation sustainable (10-15 year transition): tangled rope (mixed coordination/extraction). If unsustainable (30%+ farm closure, regional depopulation): snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_agricultural_consolidation_sustainability, empirical, 'Sustainability of EU agricultural sector consolidation').

omega_variable(
    capital_flight_revenue_extraction,
    'What fraction of Vietnamese manufacturing profits are repatriated to foreign parent companies vs retained for domestic investment?',
    'Foreign Direct Investment flow analysis; comparison of profit repatriation rates for EU capital in Vietnam vs domestic reinvestment; tax authority data on transfer pricing',
    'If repatriation > 60%: EVFTA operates as extraction mechanism (profit drain). If repatriation < 30%: EVFTA functions as coordination (capital retention supports domestic growth).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_flight_revenue_extraction, empirical, 'Profit repatriation vs domestic reinvestment rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evfta_trade_agreement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evfta_tr_t0, evfta_trade_agreement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(evfta_tr_t3, evfta_trade_agreement, theater_ratio, 3, 0.5).
narrative_ontology:measurement(evfta_tr_t6, evfta_trade_agreement, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(evfta_be_t0, evfta_trade_agreement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(evfta_be_t3, evfta_trade_agreement, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(evfta_be_t6, evfta_trade_agreement, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evfta_trade_agreement, resource_allocation).
narrative_ontology:affects_constraint(evfta_trade_agreement, wto_most_favored_nation).
narrative_ontology:affects_constraint(evfta_trade_agreement, global_supply_chain_dependency).
narrative_ontology:affects_constraint(evfta_trade_agreement, labor_arbitrage_extraction).
narrative_ontology:affects_constraint(evfta_trade_agreement, developing_world_policy_autonomy).

% DUAL FORMULATION NOTE:
% EVFTA is downstream of WTO most-favored-nation rules (which mandated non-discriminatory tariff treatment) and upstream of labor-arbitrage extraction mechanisms that operate at the individual-firm level. The constraint family decomposes as: (1) WTO MFN rule (ε≈0.15, Mountain) — logical/legal requirement for non-discrimination; (2) EVFTA tariff elimination (ε≈0.52, Tangled Rope) — bilateral implementation of MFN logic with asymmetric enforcement; (3) Labor-cost arbitrage in supply chains (ε≈0.68, Snare) — firm-level extraction enabled by tariff-free competition. These are linked by institutional causality: MFN mandates tariff reduction, tariff reduction enables labor arbitrage. EVFTA represents the middle layer where coordination and extraction are inseparable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evfta_trade_agreement, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
