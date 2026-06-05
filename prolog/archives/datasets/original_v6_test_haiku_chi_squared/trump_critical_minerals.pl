% ============================================================================
% CONSTRAINT STORY: trump_critical_minerals
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_critical_minerals, []).

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
 *   constraint_id: trump_critical_minerals
 *   human_readable: Trump Critical Minerals Stockpile Project
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Trump Critical Minerals Stockpile Project represents a structural
 *   constraint that simultaneously performs coordination and extraction
 *   functions. Ostensibly, the policy addresses genuine supply vulnerability
 *   to Chinese monopolies in rare earth elements and other minerals essential
 *   to defense and clean energy infrastructure. However, the implementation
 *   creates a hybrid constraint where coordination benefits (supply security
 *   for the defense industrial base, long-term supply guarantees for domestic
 *   mining) are coupled with asymmetric extraction: developing nation mineral
 *   exporters face demand destruction, domestic consumers face price
 *   inflation, and renewable energy deployment faces feedstock cost barriers.
 *   The constraint exhibits rising theater (0.42→0.65) as national security
 *   framing increasingly justifies what economically functions as domestic
 *   mining subsidy and protectionist supply restriction. The extractiveness
 *   gradient (0.18→0.38) reflects the policy's intensifying effects on global
 *   supply chains. This is a canonical case of how coordination mechanisms
 *   can be captured by organized beneficiaries (domestic mining interests,
 *   defense procurement) to enable extraction from dispersed victims
 *   (consumers, foreign exporters, renewable energy deployment).
 *
 * KEY AGENTS:
 *   - Trump Administration / National Security Apparatus: Institutional beneficiary (institutional/arbitrage) — initiates and enforces stockpiling mandate; controls supply restructuring
 *   - Domestic Mining Companies: Organized beneficiary (organized/constrained) — capture guaranteed procurement and subsidy support; constrained by environmental regulation and permitting
 *   - Defense Industrial Base: Institutional beneficiary (institutional/arbitrage) — primary coordination beneficiary; receives reliable supply guarantees; possesses exit via alternative sourcing
 *   - Developing Nation Mineral Exporters: Powerless victim (powerless/trapped) — face demand destruction and price volatility; cannot exit commodity supply chains
 *   - Domestic Consumers: Powerless victim (powerless/trapped) — face higher prices for electronics, vehicles, renewable energy; cannot opt out of price increases
 *   - Renewable Energy Sector: Moderate victim (moderate/constrained) — needs minerals but faces supply restrictions and price inflation; constrained by input availability
 *   - International Trade Coalition: Organized observer (organized/constrained) — sees policy as temporary deviation from free trade norms with implicit sunset; constrained by geopolitical competition
 *   - Global Liberal Trade Regime: Institutional observer (institutional/arbitrage) — maintains free-trade rhetoric while implementing protectionist mechanisms (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_critical_minerals, 0.38).
domain_priors:suppression_score(trump_critical_minerals, 0.48).
domain_priors:theater_ratio(trump_critical_minerals, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_critical_minerals, extractiveness, 0.38).
narrative_ontology:constraint_metric(trump_critical_minerals, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(trump_critical_minerals, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_critical_minerals, tangled_rope).
narrative_ontology:human_readable(trump_critical_minerals, "Trump Critical Minerals Stockpile Project").
narrative_ontology:topic_domain(trump_critical_minerals, "economic/political").

domain_priors:requires_active_enforcement(trump_critical_minerals).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_critical_minerals, domestic_mining_companies).
narrative_ontology:constraint_beneficiary(trump_critical_minerals, defense_industrial_base).
narrative_ontology:constraint_beneficiary(trump_critical_minerals, national_security_apparatus).
narrative_ontology:constraint_victim(trump_critical_minerals, global_supply_chain_efficiency).
narrative_ontology:constraint_victim(trump_critical_minerals, consumer_prices).
narrative_ontology:constraint_victim(trump_critical_minerals, developing_nation_mineral_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATION MINERAL EXPORTERS (SNARE) — Cannot exit global supply chain restructuring; face demand destruction and price volatility. Trapped in commodity dependency with no alternative markets. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.64.
constraint_indexing:constraint_classification(trump_critical_minerals, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC CONSUMERS (SNARE) — Face higher prices for electronics, vehicles, and renewable energy technologies. Cannot exit markets or opt out of price increases. Suppression via consumer choice illusion. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(trump_critical_minerals, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC MINING COMPANIES (TANGLED ROPE) — Primary beneficiaries with guaranteed procurement and subsidy support (coordination function). Also constrained by environmental regulations, permitting delays, and long development timelines (extraction mechanism). d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.14.
constraint_indexing:constraint_classification(trump_critical_minerals, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE INDUSTRIAL BASE (ROPE) — Primary beneficiary with reliable supply guarantees. Benefits from coordination mechanism (strategic reserve reduces sourcing uncertainty). Arbitrage exit available through alternative sourcing. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(trump_critical_minerals, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RENEWABLE ENERGY SECTOR (TANGLED ROPE) — Needs critical minerals (rare earths for turbines/panels) but faces supply restrictions and higher prices from stockpiling diversion. Both benefits (long-term supply security) and costs (short-term price inflation). d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(trump_critical_minerals, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL TRADE COALITION (SCAFFOLD) — Sees stockpiling as temporary deviation from free trade norms with implicit sunset as supply chain resilience improves. Constrained by geopolitical competition but possesses organizing capacity. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.20.
constraint_indexing:constraint_classification(trump_critical_minerals, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: GLOBAL LIBERAL TRADE REGIME (PITON) — Stockpiling narrative invokes free-trade language while implementing protectionist mechanisms (theater ratio 0.65). The regime's commitment to open markets is largely performative when national security exceptions proliferate. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.03.
constraint_indexing:constraint_classification(trump_critical_minerals, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the constraint exhibits genuine coordination function (supply security) coupled with asymmetric extraction (price increases, developing nation exclusion). ε=0.38 and suppression=0.48 sustain hybrid classification. d≈0.68, f(d)≈1.12, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(trump_critical_minerals, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_critical_minerals_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_critical_minerals, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_critical_minerals, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_critical_minerals, TR),
    TR >= 0.70.

:- end_tests(trump_critical_minerals_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The policy clearly benefits some actors (domestic miners, defense) while imposing costs on others (developing exporters, consumers). The extraction is not maximal because genuine supply security concerns are non-trivial, and the defense industrial base does experience real coordination benefits. However, the benefits are concentrated (organized domestic actors) while costs are dispersed (millions of consumers, many developing nations). This concentration asymmetry justifies 0.38 rather than lower. Suppression (0.48): Moderate. Developing exporters cannot easily escape supply chain restructuring; consumers cannot exit markets or avoid price increases. However, suppression is not total — some alternative sourcing routes exist, and domestic mining development (though slow) does provide an exit path over longer horizons. Theater ratio (0.65): Moderate-high. The policy is justified through national security framing, which is not entirely performative (China does hold significant market share), but the theatrical element is substantial. Strategic reserve rhetoric emphasizes security while actual implementation prioritizes domestic mining subsidies. The ratio increased over the interval (0.42→0.65) as the subsidy mechanism became more explicit relative to security justifications.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival divide. Domestic beneficiaries (mining companies, defense) see Rope or low-extraction Tangled Rope — they experience genuine coordination benefits and supportive policy. Developing nation exporters see pure Snare — they face demand destruction with no alternatives. Consumers see Snare — they face price increases with no exit. The renewable energy sector sees Tangled Rope — the policy both enables long-term supply security and imposes near-term cost barriers. The international trade system sees Piton — stockpiling invokes free-trade language while implementing protectionist mechanisms, with rising theater as the subsidy mechanism becomes explicit. The analytical observer sees the full Tangled Rope structure: genuine coordination coupled with extraction. The perspectival gap reveals how national security framing naturalizes what is economically a subsidy and protection mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing mineral exporters: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — no exit options; demand destruction is compulsory. Consumers: Victim + trapped → d≈0.85, f(d)≈1.15. High extraction — price increases are non-negotiable for essential goods. Domestic mining companies: Beneficiary + constrained → d≈0.38, f(d)≈0.38. Moderate directionality — net beneficiary but constrained by regulation and long development timelines. Defense industrial base: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with exit options (can source alternatives, though less efficiently). Renewable energy: Victim + constrained → d≈0.58, f(d)≈0.75. Moderate extraction; constrained by input availability but has agency to source alternatives over time. International trade coalition: Observer + constrained → d≈0.42, f(d)≈0.42. Sees policy as temporary deviation; constrained by geopolitical competition but organized. Global trade regime: Institutional + arbitrage → d≈0.15, f(d)≈0.05. Piton classification derives from theater gate (0.65), not from high chi. Analytical observer: analytical → d≈0.68, f(d)≈1.12. Balanced perspective across beneficiaries and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The stockpiling policy legitimately exhibits both coordination and extraction functions, resolving potential mandatrophy misclassification. Coordination is genuine: the defense industrial base benefits from supply security, and domestic mining receives guaranteed markets that reduce entrepreneurial risk in high-capital-intensive extraction. Extraction is also genuine: developing exporters face demand destruction, consumers face price increases, and renewable energy deployment faces feedstock barriers — all imposed without their consent or compensation. The policy is not mislabeled as coordination when it is extraction, nor vice versa. It is accurately Tangled Rope: a hybrid mechanism where coordination benefits for organized insiders are coupled with asymmetric costs imposed on dispersed outsiders. The rising theater ratio (0.42→0.65) indicates increasing performative content as national security justifications intensify relative to supply-security improvements. The constraint is unstable in the direction of increasing extraction relative to coordination — as subsidy mechanisms become more explicit and security rhetoric recedes, the tangled rope risks degrading toward pure Snare from the exporters' perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_security_vs_extraction,
    'Is the stockpiling mechanism primarily a coordination solution to genuine supply vulnerability or a vehicle for domestic mining subsidies disguised as national security?',
    'Historical analysis of China supply disruption frequency vs perceived threat inflation; cost-benefit analysis comparing stockpile maintenance vs actual prevented supply shocks; lobbying expenditure tracking by domestic mining interests',
    'If supply vulnerability is genuine: Rope classification likely. If primarily subsidy mechanism: Snare classification for developing exporters likely. Shifts the coordination function from necessary to performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_security_vs_extraction, empirical, 'Whether stockpiling addresses genuine supply risk or serves as subsidy vehicle').

omega_variable(
    consumer_cost_incidence,
    'What fraction of consumer price increases for electronics and renewable energy derive from critical minerals scarcity vs from government stockpiling policies that restrict market supply?',
    'Price decomposition analysis; comparison of minerals-constrained vs unrestricted market scenarios; econometric isolation of policy effect from supply chain disruption effects',
    'If policy-induced costs exceed supply-risk costs: extraction mechanism is substantial. If supply risk dominates: prices reflect coordination necessity, not extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_cost_incidence, empirical, 'Attribution of consumer price increases to policy vs supply risk').

omega_variable(
    developing_nation_exit_capacity,
    'Do developing nation mineral exporters possess viable exit options (alternative markets, supply diversification, processing integration) or are they genuinely trapped by restructured supply chains?',
    'Analysis of mineral market structure pre/post stockpiling policy; tracking of alternative buyer networks and processing investment; assessment of technical barriers to supply chain reorientation',
    'If trapped: Snare classification confirmed for exporters. If viable exits exist: Tangled Rope classification more accurate; extraction is high but not absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_nation_exit_capacity, empirical, 'Whether developing exporters possess exit options from restructured supply chains').

omega_variable(
    sunset_credibility,
    'Is the stockpiling policy credibly temporary with identifiable trigger conditions for unwinding, or is it institutionally durable (sticky) regardless of supply security improvements?',
    'Analysis of policy exit criteria specificity; historical precedent for strategic reserve drawdowns or policy reversal; institutional commitment mechanisms and political durability across administrations',
    'If credibly temporary: Scaffold classification sustained. If institutionally sticky: reclassify as Snare or Piton (sustained extraction mechanism). Determines whether the constraint has genuine sunset or is performing sunsets while remaining.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_credibility, conceptual, 'Credibility of policy sunset and institutional durability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_critical_minerals, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcm_tr_t0, trump_critical_minerals, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tcm_tr_t2, trump_critical_minerals, theater_ratio, 2, 0.55).
narrative_ontology:measurement(tcm_tr_t4, trump_critical_minerals, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(tcm_be_t0, trump_critical_minerals, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tcm_be_t2, trump_critical_minerals, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(tcm_be_t4, trump_critical_minerals, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_critical_minerals, resource_allocation).
narrative_ontology:affects_constraint(trump_critical_minerals, china_rare_earth_monopoly).
narrative_ontology:affects_constraint(trump_critical_minerals, renewable_energy_supply_chain).
narrative_ontology:affects_constraint(trump_critical_minerals, semiconductor_manufacturing_dependence).

% DUAL FORMULATION NOTE:
% The critical minerals stockpile is downstream of China's dominant market position in rare earth extraction and processing (upstream constraint: china_rare_earth_monopoly, ε≈0.55, Snare). The stockpile is an attempted coordination response to that monopoly but itself becomes a mixed constraint because implementation prioritizes domestic subsidy over pure supply security. The renewable energy sector constraint is downstream because critical minerals scarcity directly affects deployment feasibility; the stockpile's resource allocation decisions determine renewable sector input costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trump_critical_minerals, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
