% ============================================================================
% CONSTRAINT STORY: supply_chain_brittleness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supply_chain_brittleness, []).

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
 *   constraint_id: supply_chain_brittleness
 *   human_readable: Supply Chain Brittleness and Systemic Vulnerability
 *   domain: economic/logistical/systems
 *
 * SUMMARY:
 *   Supply chain brittleness represents a structural constraint emerging from
 *   decades of optimization toward efficiency at the expense of resilience.
 *   Global manufacturing has progressively compressed inventory buffers,
 *   eliminated redundant suppliers, concentrated production in
 *   cost-minimizing locations, and financialized supply chain operations.
 *   This creates a coordination mechanism (just-in-time logistics works
 *   elegantly) layered with systematic extraction: the benefits of efficiency
 *   accrue to optimizers and financial actors, while the costs of disruption
 *   (shortages, price spikes, unavailability) distribute to consumers and
 *   small suppliers who have no exit. The constraint exhibits all eight DR
 *   types from different structural positions. Extractiveness has increased
 *   from 0.35 (year 0) to 0.58 (year 30) as supply chains have become more
 *   optimized and therefore more brittle; theater ratio has increased from
 *   0.35 to 0.48 as risk management processes have become more elaborate
 *   while actual resilience has declined (Goodhart drift). The COVID-19
 *   pandemic and subsequent geopolitical disruptions revealed that
 *   brittleness is not an abstract risk but a concrete structural
 *   vulnerability affecting billions of actors.
 *
 * KEY AGENTS:
 *   - End Consumers: Primary victims (powerless/trapped) — no individual exit from dependency; bear all costs of disruption through shortages and price inflation
 *   - Small Suppliers: Primary victims (powerless/trapped) — locked into asymmetric contracts; absorb input volatility and inventory risk without corresponding pricing power
 *   - Mid-Tier Logistics Operators: Secondary victims (moderate/constrained) — benefit from high-volume throughput but bear operational and inventory risk; constrained by capital requirements
 *   - Supply Chain Optimizers: Primary beneficiaries (institutional/arbitrage) — capture licensing fees, performance bonuses, and data access; zero vulnerability to disruption outcomes
 *   - Financial Extractors: Primary beneficiaries (institutional/arbitrage) — harvest margin differentials through efficiency gains; arbitrage across sectors without supply chain exposure
 *   - Resilience Coalition: Organized agents (organized/constrained) — government, industry groups, NGOs pushing regulatory mandates for dual-sourcing and strategic reserves with sunset logic
 *   - Risk Management Systems: Institutional theater (institutional/arbitrage) — maintains performative stress testing and insurance contracts; persists despite declining real explanatory power
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing optimization-driven brittleness as inherent economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supply_chain_brittleness, 0.58).
domain_priors:suppression_score(supply_chain_brittleness, 0.65).
domain_priors:theater_ratio(supply_chain_brittleness, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supply_chain_brittleness, extractiveness, 0.58).
narrative_ontology:constraint_metric(supply_chain_brittleness, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supply_chain_brittleness, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supply_chain_brittleness, tangled_rope).
narrative_ontology:human_readable(supply_chain_brittleness, "Supply Chain Brittleness and Systemic Vulnerability").
narrative_ontology:topic_domain(supply_chain_brittleness, "economic/logistical/systems").

domain_priors:requires_active_enforcement(supply_chain_brittleness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supply_chain_brittleness, logistical_optimizers).
narrative_ontology:constraint_beneficiary(supply_chain_brittleness, cost_reduction_consultants).
narrative_ontology:constraint_beneficiary(supply_chain_brittleness, financial_extractors).
narrative_ontology:constraint_victim(supply_chain_brittleness, end_consumers).
narrative_ontology:constraint_victim(supply_chain_brittleness, small_suppliers).
narrative_ontology:constraint_victim(supply_chain_brittleness, systemic_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END CONSUMER (SNARE) — Trapped in dependency on globally optimized supply chains with no practical exit. Individual consumers cannot switch to locally resilient alternatives at scale; cost structures and product availability are entirely determined by the fragile system. Maximum extraction: bear all costs of disruption (shortages, price spikes, product unavailability) with zero agency.
constraint_indexing:constraint_classification(supply_chain_brittleness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL SUPPLIER (SNARE) — Trapped by contract lock-in, dependency on single or few buyers, and no capacity to service alternative markets. Extractive relationship: absorb input cost volatility while prices to customers are fixed; absorb inventory risk; absorb lead-time vulnerability. Cannot exit without business failure.
constraint_indexing:constraint_classification(supply_chain_brittleness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER LOGISTICS OPERATOR (TANGLED ROPE) — Constrained by capital requirements for fleet modernization and facility expansion, but benefits from high throughput volumes and premium pricing during scarcity events. Experiences genuine coordination (just-in-time efficiency) alongside asymmetric extraction (bears operational risk, inventory risk, and margin compression). Can exit at significant cost.
constraint_indexing:constraint_classification(supply_chain_brittleness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUPPLY CHAIN OPTIMIZER (ROPE) — Experiences the constraint as pure coordination: algorithms that reduce slack, eliminate redundancy, and compress lead times. Benefits from increased efficiency gains, licensing fees, performance-based contracts, and data access. Net beneficiary. Operates at arbitrage layer — can migrate expertise across sectors without disruption.
constraint_indexing:constraint_classification(supply_chain_brittleness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL EXTRACTOR (ROPE) — Captures value through supply chain optimization strategies: purchase supplier firms, apply efficiency improvements, harvest margin differential, exit via IPO or strategic sale. Zero vulnerability to supply disruption — profits flow from optimization regardless of brittleness outcomes. Benefits from information asymmetry about systemic risk.
constraint_indexing:constraint_classification(supply_chain_brittleness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RESILIENCE COALITION (SCAFFOLD) — Government actors, industry groups, and NGOs pushing for supply chain resilience standards, dual-sourcing mandates, and strategic reserve requirements. Organized agents see the brittleness as a temporary coordination failure with a sunset: regulatory frameworks (critical infrastructure protection, supply chain transparency, reshoring incentives) are building redundancy into the system. Constraint will degrade as legal mandates force slack back into supply chains.
constraint_indexing:constraint_classification(supply_chain_brittleness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: RISK MANAGEMENT THEATER (PITON) — Traditional supply chain risk management (scenario planning, stress testing, insurance contracts) is largely performative: models assume risk distributions from stable historical periods; insurance is structured around insurable events, not systemic cascade failures; stress tests don't account for correlated shocks (pandemic + geopolitical + climate simultaneously). The risk apparatus persists through institutional inertia despite low explanatory power. Theater ratio high because the rituals (quarterly risk reviews, vendor audits, business continuity plans) continue without fundamental addressing of brittleness.
constraint_indexing:constraint_classification(supply_chain_brittleness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, supply chain brittleness appears to be an immutable consequence of economic scaling: optimization always trades robustness for efficiency; global complexity always creates systemic vulnerability; information asymmetries always enable extraction. This perspective sees brittleness as a law of nature in scaled economies. However, the structural data reveals this as a false summit — the brittleness is contingent on design choices (single-source sourcing, minimal buffers, financial optimization over systemic resilience) that could be different with different incentive structures.
constraint_indexing:constraint_classification(supply_chain_brittleness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supply_chain_brittleness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supply_chain_brittleness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supply_chain_brittleness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supply_chain_brittleness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supply_chain_brittleness, TR),
    TR >= 0.70.

:- end_tests(supply_chain_brittleness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint embeds significant extraction — beneficiaries (optimizers, financial actors) capture efficiency gains while victims (consumers, small suppliers) bear disruption costs. However, extractiveness is not at snare levels (0.66+) because substantial genuine coordination value exists: just-in-time logistics does reduce costs, eliminate waste, and enable complex manufacturing. The measurement trajectory (0.35 → 0.58) reflects 30 years of progressive efficiency optimization that has compressed slack without corresponding resilience investment. Suppression (0.65): High. Barriers to exit are substantial: consumers cannot switch to resilient alternatives at competitive cost; small suppliers are locked into contracts with few alternatives; mid-tier operators face high capital barriers to restructuring. Information asymmetry suppresses resistance — most actors do not have visibility into systemic cascade risk. Regulatory and financial lock-in further suppress alternatives. Theater ratio (0.48): Moderate. Risk management processes are performative (scenario planning assumes stable distributions; insurance assumes insurable events; stress tests don't model correlated shocks), but they are not as thoroughly theatrical as institutional review in some domains. The ratio's increase over time (0.35 → 0.48) reflects Goodhart drift: as actual resilience declined, risk management rituals expanded to compensate, creating theater without functionality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Supply chain optimizers perceive pure coordination (Rope) — they are solving a genuine problem (how to move goods efficiently). End consumers perceive pure extraction (Snare) — they have no agency and bear all disruption costs. Financial extractors perceive pure coordination (Rope) — they capture efficiency gains with zero exposure. Small suppliers perceive extraction (Snare) — they are locked into asymmetric terms with no alternatives. The resilience coalition perceives a temporary problem with sunset (Scaffold) — regulatory mandates are building redundancy and exit paths. Risk management professionals perceive their own degraded ritual (Piton) — they conduct stress tests and insurance reviews that do not address actual systemic vulnerability. The civilizational analytical observer risks perceiving immutable law (Mountain) — efficiency always trades robustness, complexity always creates vulnerability — but the structural data reveals these as design choices, not natural laws. The gap between Mountain and Snare perspectives is maximal: one sees inevitability, the other sees a changeable extractive arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives based on structural position. End consumers and small suppliers are victims with trapped exit options (d ≈ 0.95) — they experience maximum effective extraction chi despite moderate base extractiveness. Supply chain optimizers are beneficiaries with arbitrage exit (d ≈ 0.05) — they experience negative chi, pure coordination benefit. Financial extractors are beneficiaries with arbitrage exit (d ≈ 0.02) — maximum coordination benefit with zero vulnerability. Mid-tier operators are moderate victims with constrained exit (d ≈ 0.58) — they experience moderate chi alongside some coordination benefit. Resilience coalition actors are organized with constrained exit (d ≈ 0.45) — they have agency and can drive sunset mechanisms, reducing their experienced extraction. The perspectival gaps reflect these directionality differences: the beneficiary sees rope (pure coordination); the victim sees snare (pure extraction); the organized agent sees scaffold (temporary problem with sunset); the analytical observer risks seeing mountain (naturalizing optimization as law).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: This constraint resolves mandatrophy through the scaffold mechanism. The brittleness is NOT inherent to global supply chains but contingent on specific optimization choices (single-source, minimal buffers, financial incentive alignment). Regulatory mandates (resilience requirements, transparency standards, strategic reserves, reshoring incentives) are building structural exits — the scaffold sunset clause is real. As legal mandates force slack back into supply chains, base extractiveness will decline (from 0.58 toward 0.35-0.40), snare perspectives transition toward tangled rope, and organized victim resistance gains agency. The false summit (mountain from the analytical observer) is revealed: brittleness appears inevitable only from within the optimization paradigm. Escape requires design changes, not acceptance of law. RISK: Regulatory capture could transform resilience mandates into new extraction mechanisms (beneficiaries capture the contracts for dual-sourcing, strategic reserves become protected monopolies, reshoring mandates favor incumbent suppliers). If captured, the scaffold becomes tangled rope — mandates redistribute extraction without reducing it. Continuous re-examination of whether resilience improvements genuinely reduce extraction or merely relocate it is required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_resilience_tradeoff_threshold,
    'What efficiency loss is acceptable to maintain systemic resilience, and who decides this threshold?',
    'Comparative analysis of supply chain cost structures with varying redundancy levels; measurement of actual resilience gains vs theoretical efficiency losses; post-disruption audit of which systems with built-in slack performed better',
    'If threshold < 5% cost increase: resilience can be built with minimal burden redistribution. If threshold > 15%: resilience mandate requires major cost transfer to consumers or winners, triggering redistribution conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_resilience_tradeoff_threshold, empirical, 'Efficiency-resilience tradeoff threshold and decision authority').

omega_variable(
    cascading_failure_inevitability,
    'Are supply chain cascade failures inherent to global complexity or artifacts of specific architectural choices (single-source, minimal buffers, financial optimization)?',
    'Network analysis of supply chain failure modes; comparison of failure rates in diversified vs centralized supply networks; historical analysis of systems that maintained resilience through redundancy',
    'If inherent: mountain classification justified, brittleness is unavoidable. If artifacts of design: brittleness is contingent, false summit confirmed, resilience is achievable through structural redesign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cascading_failure_inevitability, empirical, 'Whether cascade failures are structural or design-contingent').

omega_variable(
    information_asymmetry_persistence,
    'Can supply chain transparency mandates reduce information asymmetry about systemic risk, or does complexity always outrun visibility?',
    'Assessment of real-time supply chain visibility under transparency mandates; measurement of actor behavior changes when systemic risk information is disclosed; comparison of cascading failure frequency before/after transparency implementation',
    'If transparency effective: financial extractors lose arbitrage advantage, extraction mechanism weakens, snare becomes tangled rope with organized victim resistance. If transparency ineffective: information asymmetry persists, extraction continues unabated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether transparency mandates can address systemic risk information asymmetry').

omega_variable(
    regulatory_capture_risk_in_resilience,
    'Will resilience mandates (dual-sourcing, strategic reserves, reshoring) be captured by incumbent extractors, transformed into new rent-seeking mechanisms, or remain genuine resilience improvements?',
    'Post-mandate analysis of how firms respond to resilience requirements; tracking whether mandates increase costs to small suppliers or large coordinators; comparison of resilience metrics before/after mandate in captured vs non-captured jurisdictions',
    'If captured: scaffold becomes tangled rope — mandates redistribute extraction rather than reduce it. If effective: scaffold genuinely builds exit paths, brittleness declines, snare becomes tangled rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_risk_in_resilience, conceptual, 'Whether resilience mandates will be captured or remain genuine improvements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supply_chain_brittleness, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scb_tr_t0, supply_chain_brittleness, theater_ratio, 0, 0.35).
narrative_ontology:measurement(scb_tr_t15, supply_chain_brittleness, theater_ratio, 15, 0.42).
narrative_ontology:measurement(scb_tr_t30, supply_chain_brittleness, theater_ratio, 30, 0.48).
narrative_ontology:measurement(scb_tr_t5, supply_chain_brittleness, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(scb_be_t0, supply_chain_brittleness, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scb_be_t15, supply_chain_brittleness, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(scb_be_t30, supply_chain_brittleness, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(scb_be_t5, supply_chain_brittleness, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supply_chain_brittleness, resource_allocation).
narrative_ontology:affects_constraint(supply_chain_brittleness, semiconductor_supply_vulnerability).
narrative_ontology:affects_constraint(supply_chain_brittleness, labor_concentration_manufacturing).
narrative_ontology:affects_constraint(supply_chain_brittleness, geopolitical_economic_coupling).

% DUAL FORMULATION NOTE:
% Supply chain brittleness is upstream of specific commodity dependencies (semiconductors, rare earths, food) but represents a distinct structural constraint operating at the systemic level. The architecture of global supply chains creates vulnerability for all downstream commodities simultaneously. Decomposed constraint family: supply_chain_brittleness (systemic architecture, ε=0.58), semiconductor_supply_vulnerability (specific embodiment, ε=0.65), geopolitical_economic_coupling (political weaponization of dependency, ε=0.72).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supply_chain_brittleness, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
