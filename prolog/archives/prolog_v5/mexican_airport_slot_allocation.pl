% ============================================================================
% CONSTRAINT STORY: mexican_airport_slot_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mexican_airport_slot_allocation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mexican_airport_slot_allocation
 *   human_readable: Mexican Airport Slot Allocation System
 *   domain: economic_policy/infrastructure/regulation
 *
 * SUMMARY:
 *   Mexico's airport slot allocation system, concentrated at Mexico City
 *   International (MEX) and Cancun (CUN), exhibits a classic tangled rope
 *   structure: genuine coordination function (safe, efficient operations at
 *   capacity-constrained hubs) layered with asymmetric extraction (incumbent
 *   carriers protected from competition, new entrants and regional carriers
 *   excluded, passengers pay higher fares). The system uses formal allocation
 *   criteria (historical use, service to regional routes, carrier size) that
 *   are substantially decorative relative to the actual mechanism: political
 *   negotiation and incumbent rent capture. Theater has increased over the
 *   observation interval (0.42 to 0.58) as the gap between formal rules and
 *   political practice has widened. The constraint appears as an immutable
 *   scarcity problem (mountain) from the civilizational analytical view, but
 *   this naturalizes a contingent allocation regime that could be
 *   restructured toward competitive mechanisms (auctions, slot trading,
 *   use-it-or-lose-it enforcement) as demonstrated by the EU and US. The
 *   primary extraction mechanism is grandfathering: historical slot holders
 *   (primarily Aeromexico and a few other incumbents) retain dominant market
 *   position despite growth in competing demand from budget carriers,
 *   regional operators, and new entrants. The suppression (0.65) reflects
 *   barriers to entry that combine structural (airport congestion) and
 *   institutional (allocation opacity, discretionary authority). The
 *   beneficiaries are incumbent carriers and government revenue (implicit
 *   rents from allocation authority). The victims are regional carriers,
 *   budget carriers, and fare-paying passengers who face higher prices and
 *   reduced route choice.
 *
 * KEY AGENTS:
 *   - Incumbent Carriers (Aeromexico, Interjet historically): Institutional/arbitrage beneficiaries — capture network effects, pricing power, and route dominance through grandfathered slots
 *   - Regional Carriers (Volotea, regional Mexicana franchises): Powerless/trapped victims — excluded from major hubs, cannot scale operations or offer competitive fares
 *   - Budget Carriers (Viva Aerobus, Frontier expansion): Moderate/constrained victims — operate at secondary airports or high-cost secondary slots; bear asymmetric coordination burden
 *   - Fare-Paying Passengers: Powerless/trapped victims — face higher fares and reduced route competition due to slot scarcity artificially constrained by allocation opacity
 *   - Government Aviation Authority (SCT): Institutional/arbitrage beneficiary — captures implicit revenue and maintains political discretion over allocation
 *   - Regulatory Reform Coalition (competition advocates, consumer groups, international pressure): Organized/constrained agents — pushing for slot trading markets, use-it-or-lose-it enforcement, competitive auctions
 *   - Analytical Observer: Civilizational/analytical — risks naturalizing scarcity response as inevitable structural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mexican_airport_slot_allocation, 0.58).
domain_priors:suppression_score(mexican_airport_slot_allocation, 0.65).
domain_priors:theater_ratio(mexican_airport_slot_allocation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mexican_airport_slot_allocation, extractiveness, 0.58).
narrative_ontology:constraint_metric(mexican_airport_slot_allocation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mexican_airport_slot_allocation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mexican_airport_slot_allocation, tangled_rope).
narrative_ontology:human_readable(mexican_airport_slot_allocation, "Mexican Airport Slot Allocation System").
narrative_ontology:topic_domain(mexican_airport_slot_allocation, "economic_policy/infrastructure/regulation").

domain_priors:requires_active_enforcement(mexican_airport_slot_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mexican_airport_slot_allocation, incumbent_carriers).
narrative_ontology:constraint_beneficiary(mexican_airport_slot_allocation, mexican_government_revenue).
narrative_ontology:constraint_victim(mexican_airport_slot_allocation, new_market_entrants).
narrative_ontology:constraint_victim(mexican_airport_slot_allocation, regional_carriers).
narrative_ontology:constraint_victim(mexican_airport_slot_allocation, fare_paying_passengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL CARRIER (SNARE) — Trapped by inability to access Mexico City and Cancun slots; cannot expand route network or scale operations without slots. Faces full extraction: incumbent carriers maintain pricing power and route dominance. Exit requires abandoning growth strategy or relocating operations.
constraint_indexing:constraint_classification(mexican_airport_slot_allocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDGET CARRIER (TANGLED ROPE) — Constrained by slot scarcity and allocation grandfathering rules. Benefits from coordination function (predictable operations, efficient scheduling) but bears asymmetric extraction through slot pricing and limited allocation. Can operate at secondary airports but faces high cost of airport congestion and passenger inconvenience.
constraint_indexing:constraint_classification(mexican_airport_slot_allocation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT CARRIER (ROPE) — Benefits from grandfathered slot allocation and maintains competitive advantage through network effects. Experiences constraint as coordination: slot stability enables long-term planning and network optimization. Net beneficiary with arbitrage options (can trade slots, adjust capacity, optimize routes).
constraint_indexing:constraint_classification(mexican_airport_slot_allocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized pressure for slot trading markets, capacity auctions, and transparent allocation. Views the bottleneck as a temporary coordination failure with a sunset: if competitive allocation mechanisms (slot auctions, use-it-or-lose-it rules) are implemented, the extractive grandfathering mechanism loses force. Constrained by incumbent political power but sees structural path to regime change.
constraint_indexing:constraint_classification(mexican_airport_slot_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNMENT AVIATION AUTHORITY (PITON) — Maintains performative allocation rules (historical use, carrier equity) while the actual mechanism is political patronage and incumbent rent extraction. Theater persists through institutional inertia: the authority's formal allocation criteria (equity, frequency levels, carrier size) are largely decorative relative to the actual slot distribution, which is negotiated through political channels. The authority benefits from arbitrage revenue but sees its own process as degraded.
constraint_indexing:constraint_classification(mexican_airport_slot_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a civilizational scale, airport capacity is genuinely scarce at Mexico City and Cancun hubs. Slot allocation to SOMEBODY is a structural necessity. This perspective risks naturalizing the current allocation mechanism (grandfathering, political negotiation) as an inevitable response to scarcity. However, the structural data reveals this as a false summit: many nations (EU, US auctions) have proven slot scarcity is compatible with competitive allocation mechanisms. Naturalization masks contingent institutional design.
constraint_indexing:constraint_classification(mexican_airport_slot_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mexican_airport_slot_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mexican_airport_slot_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mexican_airport_slot_allocation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mexican_airport_slot_allocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mexican_airport_slot_allocation, TR),
    TR >= 0.70.

:- end_tests(mexican_airport_slot_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The incumbent carriers capture substantial rents through grandfathered slots at capacity-constrained hubs. The extraction is not maximal (0.72+) because: (1) the system does serve a genuine coordination function (safe operations at congested airports), (2) some new entry does occur through secondary slots or secondary airports, and (3) the formal allocation criteria (service to regional routes, carrier equity) create some binding constraints on pure rent extraction. The trajectory from 0.38 to 0.58 reflects the progressive tightening of slot scarcity and the widening gap between formal allocation rules and political practice. Suppression (0.65): Moderate-high. Barriers to exit include both structural (airport congestion is real) and institutional (allocation opacity, discretionary authority, lack of transparent trading mechanisms). Regional carriers face multiple exit costs: they cannot access major hubs without slots, cannot operate profitably at secondary airports, and cannot cheaply relegate from their current markets. The suppression combines hard constraints (physical airport capacity) with soft constraints (political/institutional barriers to transparent allocation and secondary markets). Theater Ratio (0.58): Moderate-high and increasing. The formal allocation criteria (historical use, regional service, carrier equity) are substantially decorative. Actual allocation is negotiated through political channels; the authority's formal rules predict allocation outcomes poorly. The theater has increased because the gap between formal rules and political practice has widened — early in the sample period (t=0), the formal criteria had more predictive power (theater=0.42); by t=16, allocation is almost entirely political negotiation (theater=0.58). This Goodhart drift (observed metrics become targets of manipulation) indicates that the authority is deliberately maintaining performative procedures while the actual allocation mechanism has shifted to political negotiation. Claimed Type (Tangled Rope): The system combines a genuine coordination function (managing capacity constraints at congested hubs) with active enforcement of asymmetric extraction (grandfathering, political discretion). This satisfies the tangled rope requirements: beneficiaries (incumbents), victims (entrants and passengers), active enforcement (government authority), and genuinely mixed coordination and extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of institutional classification perspectives. The incumbent carrier (institutional/arbitrage) sees coordination and benefits — slot stability enables planning. The government authority (institutional/arbitrage) also sees coordination benefits plus revenue capture. The budget carrier (moderate/constrained) sees mixed coordination and extraction — can operate but at high cost. The regional carrier (powerless/trapped) sees pure extraction — grandfathering blocks their market access. The reform coalition (organized/constrained) sees a temporary problem with a sunset — competitive mechanisms could restructure allocation. The analytical observer (analytical/analytical) risks seeing immutable scarcity — but this naturalizes contingent institutional design. The perspectival gap reveals that beneficiaries and authority experience rope (coordination), victims experience snare (extraction), organized agents experience scaffold (temporary), and the system's own process (formal rules vs. political reality) experiences piton (degraded). The mountain classification is a false summit — scarcity is real but allocation mechanism is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from agent power, exit options, and beneficiary/victim status. Incumbent carriers are beneficiaries with arbitrage options (can trade slots, adjust capacity, switch routes): d ≈ 0.15 → f(d) ≈ -0.01 → negative or near-zero effective extraction experienced by beneficiary. Regional carriers are victims with trapped exit options (cannot access major hubs, cannot scale): d ≈ 0.92 → f(d) ≈ 1.38 → high experienced extraction. Budget carriers are victims with constrained exit (can operate at secondary airports but at high cost): d ≈ 0.70 → f(d) ≈ 0.95 → moderate-high experienced extraction. Passengers are powerless victims with trapped exit (must accept whatever routes/fares result from slot allocation): d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. The scope modifier σ(S) = 1.0 (national scope) does not amplify or dampen chi relative to the canonical case. The composition χ = ε × f(d) × σ(S) produces moderate effective extractiveness for the incumbent beneficiary and high effective extractiveness for trapped victims, sustaining the tangled rope classification rather than pure rope (which would require more balanced directionality) or snare (which would require ε ≥ 0.66).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates how the tangled rope classification prevents misidentification as pure coordination (rope) or pure extraction (snare). If the constraint were classified as Rope (coordination only), the analysis would miss the systematic asymmetry: incumbent carriers benefit, new entrants are blocked, and passengers pay higher fares. This misclassification would wrongly suggest that slot allocation is solving a collective action problem for everyone's benefit. The rope classification would be accurate ONLY from the beneficiary's perspective (incumbent carrier) and the authority's perspective (both see coordination and benefit). But the regional carrier's snare perspective and the passenger's snare perspective are equally structurally valid. The tangled rope classification (0.40 ≤ χ ≤ 0.90) unifies these: the system has a genuine coordination function that benefits incumbents and the authority, AND it has an extractive function that harms entrants and passengers. Both are real and structural. If the constraint were classified as Snare (pure extraction), the analysis would miss that some agents actually benefit from coordination and that the system does solve legitimate problems (safe operations at congested hubs). The snare classification would wrongly suggest that the slot allocation could be eliminated without replacing its coordination function. The tangled rope classification insists that reform must preserve coordination capacity while redirecting extraction: auctions, trading, and use-it-or-lose-it mechanisms are structurally viable because they maintain coordination (predictable, efficient operations) while removing the grandfathering extraction. This is exactly what the reform coalition (scaffold perspective) proposes: competitive mechanisms with a sunset from current extractive regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slot_trading_market_viability,
    'Could secondary markets in slot trading reduce extraction while maintaining capacity coordination?',
    'Comparative analysis of EU slot trading results (post-2008), US slot performance at constrained airports (LGA, ORD, DCA), Brazil''s slot auction pilot outcomes',
    'If viable: scaffold sunset is real and achievable through policy reform. Extraction could drop below Snare threshold (0.46). If not viable: extraction mechanism is structural to scarcity, constraining policy options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slot_trading_market_viability, empirical, 'Whether slot trading markets can reduce extraction while maintaining coordination').

omega_variable(
    governmental_revenue_dependency,
    'How much of government aviation authority budget depends on implicit rents from slot allocation opacity?',
    'Budget audit; correlation analysis between slot allocation decisions and authority revenue sources; interviews with decision-makers on revenue pressure',
    'If significant dependency: government is co-beneficiary (not neutral allocator), altering the directed extraction flow and possibly changing classification toward Snare. If minimal: extraction is primarily incumbent-captured, supporting current Tangled Rope analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governmental_revenue_dependency, empirical, 'Government revenue dependency on implicit slot allocation rents').

omega_variable(
    regional_carrier_coalition_threshold,
    'At what number of excluded regional carriers does coalition power emerge to challenge incumbent dominance?',
    'Historical analysis of airline coalitions in Mexico and regional markets; threshold analysis from network game theory applied to Mexican carrier structure',
    'If threshold exceeded: powerless agent power atom could upgrade to organized, changing snare classification trajectory and enabling credible exit threats. If threshold not met: powerless agents remain atomized, snare persists unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_carrier_coalition_threshold, empirical, 'Coalition formation threshold for regional carriers').

omega_variable(
    international_passenger_welfare_accounting,
    'What proportion of passenger fare impact (higher fares, reduced route choice) is attributable to slot allocation extraction vs. legitimate congestion costs?',
    'Econometric decomposition: compare fares on competitive (unconstrained) routes vs. slot-constrained routes, controlling for distance and demand; international benchmarking of Mexico City slot utilization efficiency',
    'If extraction dominates congestion costs: victim classification (passengers) strengthens, supporting snare perspective. If costs are primarily congestion-driven: extraction is legitimate scarcity coordination cost, shifting some classification weight toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_passenger_welfare_accounting, empirical, 'Fare impact attribution: extraction vs. legitimate congestion costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mexican_airport_slot_allocation, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mxslot_tr_t0, mexican_airport_slot_allocation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mxslot_tr_t8, mexican_airport_slot_allocation, theater_ratio, 8, 0.5).
narrative_ontology:measurement(mxslot_tr_t16, mexican_airport_slot_allocation, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(mxslot_be_t0, mexican_airport_slot_allocation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mxslot_be_t8, mexican_airport_slot_allocation, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(mxslot_be_t16, mexican_airport_slot_allocation, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mexican_airport_slot_allocation, resource_allocation).
narrative_ontology:boltzmann_floor_override(mexican_airport_slot_allocation, 0.18).
narrative_ontology:affects_constraint(mexican_airport_slot_allocation, mexican_airline_market_concentration).
narrative_ontology:affects_constraint(mexican_airport_slot_allocation, international_connectivity_mexico).

% DUAL FORMULATION NOTE:
% The slot allocation system is upstream of airline market concentration and international connectivity. The concentration constraint (ε ≈ 0.55) and connectivity constraint (ε ≈ 0.48) are structurally dependent on slot allocation decisions. Changes to slot allocation mechanism would propagate directly to these downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mexican_airport_slot_allocation, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
