% ============================================================================
% CONSTRAINT STORY: shock_propagation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shock_propagation_asymmetry, []).

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
 *   constraint_id: shock_propagation_asymmetry
 *   human_readable: The One-Way Crisis Valve
 *   domain: economic/logistical
 *
 * SUMMARY:
 *   The one-way crisis valve is a structural mechanism embedded in
 *   contemporary global supply chains and financial integration that
 *   systematically concentrates positive shocks (growth, efficiency gains,
 *   technological spillovers) at the center (advanced economies,
 *   multinational corporations, central financial hubs) while funneling
 *   negative shocks (inflation, supply failures, currency crises,
 *   environmental externalities, labor immiseration) exclusively to the
 *   periphery (commodity-dependent economies, global supply chain nodes,
 *   environmental commons, peripheral labor forces). This constraint operates
 *   through multiple reinforcing mechanisms: (1) financial asymmetry — debt
 *   obligations denominated in central currencies force peripheral economies
 *   to accept unfavorable terms; (2) supply chain control — central actors
 *   control critical nodes and can offload inventory and demand shocks; (3)
 *   information asymmetry — central actors have superior market signals and
 *   can hedge; (4) environmental dumping — peripheral locations absorb
 *   pollution, deforestation, and resource depletion; (5) labor asymmetry —
 *   wage differentials allow capital to shed labor costs during downturns.
 *   The constraint is maintained through formal institutions (IMF
 *   conditionality, trade rules, capital controls) and informal norms
 *   (efficiency rhetoric, development narratives). The extractiveness has
 *   increased over the 20-year interval as supply chain integration has
 *   deepened and financial markets have become more complex, while theater
 *   has risen as development rhetoric has proliferated without reducing the
 *   underlying asymmetry.
 *
 * KEY AGENTS:
 *   - Peripheral Economies: Primary victims (powerless/trapped) — bear disproportionate impact of demand shocks, currency crises, commodity price collapses; have no exit from supply chain dependence
 *   - Central Financial Hub: Primary beneficiary (institutional/arbitrage) — captures gains from efficient capital allocation, diversification, hedging; externalizes losses through financial instruments and contract terms
 *   - Multinational Corporations: Primary beneficiary (organized/arbitrage) — realize gains from wage arbitrage, regulatory arbitrage, cost externalization; shed losses through supply chain shifting and labor reduction
 *   - Environmental Commons (Periphery): Primary victim (powerless/trapped) — absorbs extraction of environmental debt (deforestation, mining, pollution) with zero compensation; cannot exit or organize
 *   - Supply Chain Intermediaries: Secondary victims (moderate/constrained) — depend on central financing and coordination; bear shock absorption during demand collapses; cannot fully exit due to debt obligations
 *   - Development Policy Advocates: Analytical observers (moderate/mobile) — see genuine gains from trade alongside clear extraction; some peripheral economies have escaped through diversification, showing mobility is possible but rare
 *   - Analytical Observer (Civilizational): Systemic perspective (analytical/analytical) — reveals the structural asymmetry as a fundamental feature of market integration without redistribution mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shock_propagation_asymmetry, 0.68).
domain_priors:suppression_score(shock_propagation_asymmetry, 0.72).
domain_priors:theater_ratio(shock_propagation_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shock_propagation_asymmetry, extractiveness, 0.68).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shock_propagation_asymmetry, snare).
narrative_ontology:human_readable(shock_propagation_asymmetry, "The One-Way Crisis Valve").
narrative_ontology:topic_domain(shock_propagation_asymmetry, "economic/logistical").

domain_priors:requires_active_enforcement(shock_propagation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, central_financial_hub).
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, multinational_corporations).
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, advanced_economies).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, peripheral_economies).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, supply_chain_nodes).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, environmental_commons).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, labor_periphery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL ECONOMY (SNARE) — Structurally trapped in commodity export dependency. Absorbs demand shocks (price collapses), currency crises, and supply chain disruptions. Has no exit: alternative supply chains are controlled by central actors. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.65.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENVIRONMENTAL COMMONS (SNARE) — Bears extraction of environmental debt (deforestation, mining, pollutant dispersal) with zero compensation. Cannot exit or organize. Suppression is total — alternative land uses are not permitted by capital-importing agreements. d≈0.98, f(d)≈1.45, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SUPPLY CHAIN INTERMEDIARY (TANGLED ROPE) — Constrained by debt obligations and dependence on central financing. Benefits from coordination (just-in-time logistics access, market information). But bears disproportionate shock: when central hub faces demand shock, periphery absorbs inventory collapse. d≈0.70, f(d)≈1.08, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CENTRAL FINANCIAL HUB (ROPE) — Experiences the framework as pure coordination: efficient capital allocation, diversified exposure to shocks, hedging mechanisms. Gains from growth are retained; losses are externalized via financial instruments and supply contracts. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTINATIONAL CORPORATIONS (ROPE) — See the supply chain framework as efficient coordination: risk diversification across regions, wage arbitrage, tax arbitrage, regulatory arbitrage. Positive gains (margin improvement) are captured; negative shocks (labor unrest, commodity price crashes) are externalized to suppliers and labor. d≈0.12, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPMENT POLICY ADVOCATE (TANGLED ROPE) — Sees genuine coordination gains from global trade but also clear extraction mechanism. The framework functions AND extracts. Exit is mobile: some peripheral economies have successfully diversified (Vietnam, Rwanda, Botswana), but majority remain trapped. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the asymmetric shock propagation is a fundamental structural feature of market integration without redistribution mechanisms. The framework captures gains at the center and funnels losses to the periphery as a systemic pattern, not a bug. χ≈0.68 indicates this is a genuine snare, not a false summit.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shock_propagation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shock_propagation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shock_propagation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, and increasing. The constraint systematically extracts value from peripheral economies through multiple channels: commodity prices are set by central traders (not producers), currency crises force fire-sales of assets, supply shocks create cascading defaults, wage pressures from globalization are absorbed locally while efficiency gains flow to center. The initial value (0.35) reflects an early period when integration was partial and alternatives existed; the final value (0.68) reflects mature supply chain integration where alternatives have been eliminated. Suppression (0.72): Very high. Peripheral economies face multiple barriers to exit: (1) debt obligations denominated in foreign currency create compulsory participation; (2) supply chain control by central actors prevents independent sourcing; (3) technology gaps require dependence on central innovation; (4) capital controls formally restrict exit options; (5) ideological hegemony (development narrative) frames extraction as beneficial. Theater ratio (0.58): Moderate-high. The constraint's performative components include: (1) development finance that conditions on accepting unfavorable terms; (2) free trade rhetoric masking asymmetric gains; (3) poverty reduction metrics that mask absolute extraction; (4) ESG commitments that displace but do not eliminate environmental externalities; (5) diversity narratives in multinational hiring that obscure wage differentials. The theater is lower than in some institutional pitons (peer review, corporate governance) because the extraction mechanism is explicit and material — the performative layer is secondary. Theater has risen over time as development language has proliferated (sustainability, inclusion, resilience) while the underlying asymmetry has intensified.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically divergent classifications across structural positions. Peripheral economies see a pure snare: trapped, absorbing all negative shocks, experiencing high effective extraction (χ≈0.65). Central financial actors see a pure rope: efficient coordination, diversification, risk management, experiencing negative effective extraction (χ≈-0.05) because they are the beneficiaries. Supply chain intermediaries and development advocates see a tangled rope: genuine coordination benefits (logistics, market access) combined with extraction (shock absorption, debt obligations). The analytical observer sees a snare at the civilizational scale: the pattern is systemic and recursive. This perspectival gap is NOT a measurement ambiguity — it reflects the actual structural reality that the constraint extracts from some and benefits others. The gap itself is the diagnostic signature of the constraint's function.
 *
 * DIRECTIONALITY LOGIC:
 *   Peripheral economies: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction because they have no alternatives and bear all shock costs. Environmental commons: Victims + trapped → d≈0.98, f(d)≈1.45. Even higher because abstract collective cannot organize and extraction is total (all environmental debt flows to periphery). Supply chain intermediaries: Victims + constrained → d≈0.70, f(d)≈1.08. High extraction but not total because some intermediaries can switch hubs or negotiate terms. Development advocates: Mixed (mobile) → d≈0.55, f(d)≈0.75. Moderate extraction because the constraint offers genuine coordination benefits and some actors have proven exit capacity (Vietnam, Rwanda). Central hub: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary with negative effective extraction because gains flow to them. Multinationals: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.01. Net beneficiary because they control extraction mechanisms and can shift costs. The directionality chain is consistent: beneficiaries get low d → negative χ, victims get high d → high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint satisfies the snare classification (ε=0.68 > 0.46, suppression=0.72 > 0.60, χ≈0.68 > 0.66) and has been verified as a genuine snare rather than a false extraction claim. The mandatrophy analysis shows that: (1) the coordination benefits (logistics efficiency, market access) are real but captured entirely by central actors; (2) peripheral actors receive no coordination benefit proportional to their cost; (3) exit is genuinely suppressed (debt obligations, supply chain control, capital constraints); (4) the extraction increases over time as integration deepens (0.35 → 0.68); (5) the constraint serves no function for peripheral actors beyond forced participation. A false mandatrophy would claim pure extraction without any coordination function — but the constraint genuinely solves logistics and market-access problems, it simply concentrates all solutions at the center and all costs at the periphery. The classification is neither tangled rope (which would imply peripheral actors benefit proportionally) nor pure coordination rope (which would imply symmetric gains). It is a snare that functions through institutional mechanisms (debt, supply control, capital restrictions) rather than through coercive force (violence, imprisonment), making it less visible but structurally identical to historical snares (slavery, debt peonage, colonial extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_participation_fiction,
    'To what extent do peripheral economies genuinely choose participation in the supply chain framework versus face coercive pressure from debt obligations and lack of alternatives?',
    'Comparative analysis of exit costs for peripheral economies attempting diversification; historical case studies of countries that successfully exited vs. those that failed; credit rating impacts on exit attempts',
    'If genuine choice: framework is Rope (coordination with asymmetric benefits). If coercive: framework is Snare (pure extraction). Current evidence suggests coercive (constraints on currency, capital controls, debt service priorities), supporting Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_fiction, empirical, 'Whether peripheral participation is voluntary or coercively enforced').

omega_variable(
    shock_absorption_capacity_divergence,
    'What is the actual threshold at which peripheral economies cannot absorb additional shocks without structural collapse versus central economies'' demonstrated shock resilience?',
    'Time-series analysis of GDP volatility, unemployment spikes, currency crises in peripheral vs. central economies following global demand shocks; measurement of foreign exchange reserve depletion rates',
    'If periphery capacity < center by factor of 2x: snare. If < 1.5x: tangled rope. If comparable: rope. Current data shows periphery 3-4x more volatile, confirming snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shock_absorption_capacity_divergence, empirical, 'Differential shock absorption capacity between peripheral and central economies').

omega_variable(
    alternative_supply_chain_feasibility,
    'Can peripheral economies establish redundant, self-contained supply chains that reduce dependence on the central hub framework without accepting catastrophic productivity losses?',
    'Cost-benefit analysis of autarky vs. integration models; case studies of regional trade agreements (ASEAN, MERCOSUR, AfCFTA); technology transfer requirements for local manufacturing',
    'If feasible: suppression gate fails (0.72 → lower), classification downgrades to Tangled Rope. If infeasible: suppression confirmed, snare stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_chain_feasibility, empirical, 'Feasibility of peripheral economies establishing independent supply chains').

omega_variable(
    policy_corrective_mechanism_existence,
    'Do international policy mechanisms (IMF conditionality, WTO rules, development finance) actually redistribute shock burdens or primarily enforce the extraction asymmetry?',
    'Historical analysis of conditionality impacts on peripheral economy resilience; comparison of shock outcomes pre/post IMF intervention; audit of development finance effectiveness at reducing periphery volatility',
    'If redistributive: theater ratio rises (performative redistribution without effect) but extraction persists. If enforcement: snare confirmed. Current evidence suggests enforcement, supporting high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_corrective_mechanism_existence, empirical, 'Whether policy mechanisms redistribute shocks or enforce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shock_propagation_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shock_tr_t0, shock_propagation_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(shock_tr_t10, shock_propagation_asymmetry, theater_ratio, 10, 0.5).
narrative_ontology:measurement(shock_tr_t20, shock_propagation_asymmetry, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(shock_be_t0, shock_propagation_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shock_be_t10, shock_propagation_asymmetry, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(shock_be_t20, shock_propagation_asymmetry, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shock_propagation_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, commodity_price_volatility).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, currency_crisis_cascade).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, peripheral_debt_trap).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, environmental_externality_dumping).
narrative_ontology:affects_constraint(shock_propagation_asymmetry, labor_arbitrage_extraction).

% DUAL FORMULATION NOTE:
% The one-way crisis valve decomposes into five distinct constraints: commodity price volatility (peripheral producers lack pricing power), currency crises (peripheral central banks lack reserves), peripheral debt (structural dependence on foreign borrowing), environmental dumping (externalities flow to periphery), and labor arbitrage (wage differentials create extraction). Each has its own ε, but all are manifestations of the same underlying asymmetry. The parent constraint (shock_propagation_asymmetry, ε=0.68) aggregates the structural pattern; the child constraints reveal specific mechanisms. All five are downstream of the fundamental asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shock_propagation_asymmetry, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
