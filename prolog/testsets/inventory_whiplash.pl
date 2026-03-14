% ============================================================================
% CONSTRAINT STORY: inventory_whiplash
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inventory_whiplash, []).

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
 *   constraint_id: inventory_whiplash
 *   human_readable: Inventory Whiplash in Supply Chain Coordination
 *   domain: logistics/operations/economics
 *
 * SUMMARY:
 *   Inventory whiplash (also called the bullwhip effect) describes the
 *   phenomenon where small fluctuations in downstream consumer demand
 *   generate progressively larger order fluctuations at upstream supply chain
 *   stages. A 10% change in retail demand can trigger 20-30% variance in
 *   wholesale orders, which can trigger 50%+ variance in manufacturer
 *   production. This structural constraint exhibits tangled_rope
 *   characteristics: it creates genuine coordination functions (retailers
 *   must signal demand, manufacturers must schedule production) alongside
 *   asymmetric extraction (upstream manufacturers bear disproportionate
 *   volatility costs while downstream actors benefit from flexibility and
 *   logistics arbitrage). The constraint's extractiveness has increased over
 *   the measurement interval (0.28 → 0.48) as supply chains have become more
 *   complex and information systems more sophisticated without necessarily
 *   improving transparency. Theater ratio (0.58) reflects that traditional
 *   demand forecasting rituals consume significant resources without reducing
 *   actual whiplash — the constraint is partially maintained through
 *   institutional theater (forecast committees, consensus meetings) that
 *   creates the appearance of control without delivering coordination.
 *
 * KEY AGENTS:
 *   - Upstream Manufacturers: Primary victims (powerless/trapped) — bear full production volatility and inventory carrying costs; cannot exit supply chain relationships without business failure
 *   - Mid-Chain Distributors: Secondary agents (organized/constrained) — coordinate essential logistics while exploiting information gaps; experience mixed coordination and extraction
 *   - Downstream Retailers: Primary beneficiaries (institutional/arbitrage) — control demand signal timing; can shift whiplash costs backward; experience constraint as manageable coordination
 *   - Logistics Providers: Secondary beneficiaries (moderate/constrained) — coordinate transportation while extracting through surcharges and demurrage fees triggered by whiplash
 *   - Demand Forecasting Systems: Institutional degradation (institutional/arbitrage) — maintain theatrical forecasting rituals despite documented ineffectiveness; persist through inertia rather than function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating information lag as immutable natural law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inventory_whiplash, 0.48).
domain_priors:suppression_score(inventory_whiplash, 0.65).
domain_priors:theater_ratio(inventory_whiplash, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inventory_whiplash, extractiveness, 0.48).
narrative_ontology:constraint_metric(inventory_whiplash, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(inventory_whiplash, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inventory_whiplash, tangled_rope).
narrative_ontology:human_readable(inventory_whiplash, "Inventory Whiplash in Supply Chain Coordination").
narrative_ontology:topic_domain(inventory_whiplash, "logistics/operations/economics").

domain_priors:requires_active_enforcement(inventory_whiplash).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(inventory_whiplash, downstream_retailers).
narrative_ontology:constraint_beneficiary(inventory_whiplash, logistics_providers).
narrative_ontology:constraint_victim(inventory_whiplash, upstream_manufacturers).
narrative_ontology:constraint_victim(inventory_whiplash, supply_chain_stability).
narrative_ontology:constraint_victim(inventory_whiplash, working_capital_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UPSTREAM MANUFACTURER (SNARE) — Trapped in demand amplification cycle with no exit. Bears full volatility cost through production fluctuations, inventory holdings, and capacity planning failures. Cannot exit supply chain; constrained to respond to downstream demand signals that distort their own production economics. Maximum experienced extraction.
constraint_indexing:constraint_classification(inventory_whiplash, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CHAIN DISTRIBUTOR (TANGLED ROPE) — Constrained by both upstream demand volatility and downstream ordering patterns, but also coordinates essential logistics functions. Benefits from information asymmetry (can time orders to exploit discounts) while bearing coordination costs. Mixed experience of coordination necessity and extractive oscillation.
constraint_indexing:constraint_classification(inventory_whiplash, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DOWNSTREAM RETAILER (ROPE) — Primary beneficiary experiencing constraint as coordination mechanism for demand signaling and inventory management. Can arbitrage between suppliers; experiences whiplash as controllable via ordering timing. Net beneficiary — orders flow to strategic advantage.
constraint_indexing:constraint_classification(inventory_whiplash, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LOGISTICS PROVIDER (TANGLED ROPE) — Coordinates essential transportation and warehousing functions while extracting through demurrage, expedited fees, and storage surcharges triggered by whiplash volatility. Genuine coordination function (goods must move) alongside asymmetric extraction (surcharges applied to distressed orders).
constraint_indexing:constraint_classification(inventory_whiplash, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / INFORMATION LAG VIEW (MOUNTAIN) — From civilizational timescale, demand amplification appears as an immutable consequence of information lag in supply chains: finite communication speed and processing time create unavoidable oscillations. This perspective risks naturalizing what is actually an institutional choice (transparency, information sharing architecture) as physical law. Engine will flag this as false summit.
constraint_indexing:constraint_classification(inventory_whiplash, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DEMAND FORECASTING BUREAU (PITON) — Traditional forecast-driven ordering persists despite documented failure to reduce whiplash. Rituals of forecast committees, consensus meetings, and scenario planning consume resources without improving predictions. Theater ratio high because the institution has become the activity rather than serving its coordination function. Persists through inertia; alternatives (collaborative planning, CPFR) exist but encounter institutional resistance.
constraint_indexing:constraint_classification(inventory_whiplash, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inventory_whiplash_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(inventory_whiplash, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inventory_whiplash, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(inventory_whiplash, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(inventory_whiplash, TR),
    TR >= 0.70.

:- end_tests(inventory_whiplash_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Upstream manufacturers face documented volatility costs: safety stock increases exponentially with demand variance, production capacity utilization drops during low-demand phases creating sunk-cost waste, and capital tied up in inventory increases with cycle length. The constraint extracts real economic value from manufacturers to retailers and logistics providers. Suppression (0.65): High. Manufacturers face significant barriers to exit: switching suppliers is costly, supply contracts lock them in for years, information asymmetry prevents visibility into downstream demand, and collective action problems prevent coordinated transparency improvements. Behavioral barriers are also high — forecasting culture and competitive secrecy create psychological suppression. Theater ratio (0.58): Moderate-high. Demand forecasting consumes 10-15% of supply chain labor costs but demonstrably fails to reduce whiplash in most industries. The constraint is partially maintained through ritualistic forecasting activity that creates institutional legitimacy without delivering functional improvement. Increasing theater ratio over the interval reflects growing gap between forecasting sophistication and actual whiplash reduction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates acute perspectival divergence. Retailers see whiplash as a manageable coordination problem (Rope) — they perceive demand signaling as functional and see their ordering flexibility as efficient. Manufacturers see it as a predatory extraction mechanism (Snare) — demand volatility is imposed on them without their input and cannot be escaped. Distributors see mixed coordination and extraction (Tangled Rope) — they genuinely move goods (coordination) while exploiting timing (extraction). Logistics providers see similar mixing — transportation is necessary (coordination) while surcharges are discretionary (extraction). The demand forecasting system sees itself as solving a problem (coordination narrative) while actually maintaining ritual theater that generates consultant fees, software vendor revenue, and institutional budget justification. The civilizational analyst risks seeing immutable information-lag physics where the constraint is actually maintained through institutional choices (opacity, competitive secrecy, forecasting culture).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position in the extraction flow. Retailers (institutional/arbitrage) experience low d — they benefit from demand signal control and can time orders strategically; d ≈ 0.15. Manufacturers (powerless/trapped) experience high d — they bear demand volatility costs and cannot exit; d ≈ 0.92. Distributors (organized/constrained) experience moderate d — they coordinate logistics (benefit) while facing demand volatility (cost); d ≈ 0.45. Logistics providers (moderate/constrained) experience mixed directionality — they extract through surcharges (low d on the surcharge flow) while also genuinely coordinating (high d on the coordination function); net d ≈ 0.35 reflecting their mixed position as both coordinator and extractor. The analytical observer at civilizational scope risks naturalizing institutional choices as immutable, deriving d ≈ 0.72 from the false summit bias.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint avoids mandatrophy mislabeling by explicitly separating the genuine coordination function (information signaling about demand) from the asymmetric extraction mechanism (cost concentration on manufacturers, surcharge extraction by logistics providers). The constraint is fundamentally tangled_rope: retailers genuinely need to communicate demand (coordination), but they do so in ways that maximize their own flexibility while minimizing upstream visibility (asymmetric extraction). If the constraint were pure coordination (Rope), all parties would see mutual benefit and collusion would be unnecessary. If it were pure extraction (Snare), retailers would gain nothing and the system would collapse. The tangled_rope classification is stable because both functions are empirically verifiable: demand communication is necessary, and cost concentration on manufacturers is observable. The false summit (analytical mountain perspective) reveals the risk that natural law framing (information lag is inevitable) can obscure institutional choices (transparency and CPFR are optional, not technologically impossible).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_lag_vs_behavioral_herding,
    'Is demand amplification driven by genuine information lag in signal propagation or by rational behavioral herding under uncertainty?',
    'Empirical test: Compare whiplash magnitude in systems with identical information lag but different behavioral incentives (CPFR with shared data vs traditional ordering). If whiplash persists despite information transparency, herding is dominant mechanism.',
    'If information lag dominant: constraint approaches mountain (immutable). If behavioral herding dominant: constraint is tangled_rope/snare classification confirmed — extractive institutional choices are the binding mechanism, not physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_lag_vs_behavioral_herding, empirical, 'Whether whiplash is information-lag driven or behavioral-herding driven').

omega_variable(
    extraction_intentionality,
    'Are logistics surcharges and redistribution profits intentional extraction mechanisms or incidental byproducts of operational necessity?',
    'Structural analysis: Do surcharges correlate with whiplash volatility (extraction) or with actual cost changes (operation)? Do logistics firms lobby against transparency/CPFR initiatives that would reduce surcharge opportunities?',
    'If incidental: suppliers absorb whiplash as coordination cost (higher ε threshold for Tangled Rope gate). If intentional: whiplash becomes deliberately maintained extraction mechanism (lowers ε, confirms Snare classification from manufacturer perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intentionality, empirical, 'Whether logistics surcharges are intentional extraction or operational necessity').

omega_variable(
    collaborative_planning_adoption_barrier,
    'Why do CPFR and VMI systems (demand visibility, collaborative forecasting) remain niche despite 30+ years of documentation showing they reduce whiplash?',
    'Investigation of adoption barriers: transaction costs of systems, data-sharing trust barriers, power dynamics (who controls information controls supply chain), incumbent forecasting vendor lock-in, institutional culture resistance.',
    'If barrier is technical/cost: constraint could sunset through investment (Scaffold logic). If barrier is power/incentive: the whiplash is maintained because opacity serves extractive interests (Snare/Tangled Rope confirmed as irreducible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collaborative_planning_adoption_barrier, empirical, 'Why collaborative planning adoption remains limited despite evidence of effectiveness').

omega_variable(
    amplification_factor_measurement,
    'What is the true demand amplification factor (ratio of upstream order variance to downstream demand variance) and does it remain constant across industries and supply chain types?',
    'Empirical metrology: Track end-customer demand volatility vs manufacturer orders across diverse supply chains. Isolate information lag contribution from behavioral contribution using experimental variation (introduce transparency, measure effect).',
    'If amplification factor > 2.0 universally: suggests mountain-level invariance. If factor < 2.0 or highly variable: suggests institutional/behavioral origin (Tangled Rope/Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amplification_factor_measurement, empirical, 'Quantification of demand amplification factor across supply chains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inventory_whiplash, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(invwh_tr_t0, inventory_whiplash, theater_ratio, 0, 0.42).
narrative_ontology:measurement(invwh_tr_t5, inventory_whiplash, theater_ratio, 5, 0.5).
narrative_ontology:measurement(invwh_tr_t10, inventory_whiplash, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(invwh_be_t0, inventory_whiplash, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(invwh_be_t5, inventory_whiplash, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(invwh_be_t10, inventory_whiplash, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(inventory_whiplash, resource_allocation).
narrative_ontology:affects_constraint(inventory_whiplash, demand_forecasting_accuracy).
narrative_ontology:affects_constraint(inventory_whiplash, working_capital_financing).
narrative_ontology:affects_constraint(inventory_whiplash, production_capacity_utilization).

% DUAL FORMULATION NOTE:
% Inventory whiplash decomposes into distinct coordination and extraction mechanisms. The coordination story (demand signaling) operates at ε ≈ 0.15 (Rope). The extraction story (cost concentration) operates at ε ≈ 0.48 (Snare/Tangled Rope). This JSON unifies them as tangled_rope because they are structurally inseparable — retailers cannot signal demand without creating whiplash, and whiplash cannot occur without information asymmetry. However, alternative architectures (CPFR, VMI, real-time demand visibility) could decompose the constraint by reducing information lag while maintaining coordination. Such alternatives are currently suppressed by institutional incentives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(inventory_whiplash, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
