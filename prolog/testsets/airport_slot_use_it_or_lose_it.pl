% ============================================================================
% CONSTRAINT STORY: airport_slot_use_it_or_lose_it
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_airport_slot_use_it_or_lose_it, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: airport_slot_use_it_or_lose_it
 *   human_readable: "Use-it-or-lose-it" rule for airport landing slots
 *   domain: economic/regulatory
 *
 * SUMMARY:
 *   The 'use-it-or-lose-it' rule (typically 80/20) governs the allocation of
 *   scarce landing and take-off slots at congested airports. While its stated
 *   purpose is to ensure efficient capacity utilization, its primary
 *   structural effect is the creation of 'grandfather rights,' allowing
 *   incumbent airlines to retain valuable slots indefinitely. This creates a
 *   significant barrier to entry for new competitors, extracts economic
 *   opportunity, and incentivizes perverse behaviors like 'ghost flights'
 *   (flying empty planes to meet usage quotas), especially during demand
 *   shocks.
 *
 * KEY AGENTS:
 *   - Incumbent Airlines: Primary beneficiaries (institutional/arbitrage) - Gain protected assets (slots) and reduced competition.
 *   - New Entrant Airlines: Primary victims (organized/constrained) - Face a significant, artificial barrier to entering key markets.
 *   - Passengers: Secondary victims (powerless/trapped) - Experience higher fares and fewer choices due to suppressed competition.
 *   - Airport Coordinators/Regulators: Institutional enforcers (institutional/constrained) - Use the rule as a clear, albeit flawed, resource allocation mechanism.
 *   - Environmental Groups: Organized observers (organized/constrained) - View the rule through the lens of its wasteful byproduct, ghost flights.
 *   - Market Reform Advocates: Organized agents (organized/mobile) - Push for replacing the rule with market-based auctions, viewing it as a temporary scaffold.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(airport_slot_use_it_or_lose_it, 0.55).
domain_priors:suppression_score(airport_slot_use_it_or_lose_it, 0.75).
domain_priors:theater_ratio(airport_slot_use_it_or_lose_it, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, extractiveness, 0.55).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, theater_ratio, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(airport_slot_use_it_or_lose_it, tangled_rope).
narrative_ontology:human_readable(airport_slot_use_it_or_lose_it, "\"Use-it-or-lose-it\" rule for airport landing slots").
narrative_ontology:topic_domain(airport_slot_use_it_or_lose_it, "economic/regulatory").

domain_priors:requires_active_enforcement(airport_slot_use_it_or_lose_it).
narrative_ontology:has_sunset_clause(airport_slot_use_it_or_lose_it).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, incumbent_airlines).
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, airport_coordinators).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, new_entrant_airlines).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, passengers).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, the_environment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW ENTRANT (SNARE) — Experiences the rule as a coercive barrier to entry. Grandfather rights held by incumbents create an artificial scarcity that suppresses competition, trapping new airlines out of key markets. d≈0.8, f(d)≈1.2, σ=1.0 → χ≈0.66.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT (ROPE) — Experiences the rule as a pure coordination mechanism that provides stability and protects valuable assets (grandfathered slots). They can trade slots, lobby for waivers, and plan networks with certainty. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. Negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PASSENGER (MOUNTAIN) — The effects of reduced competition (higher fares, fewer choices) are perceived as an unchangeable feature of the market at major airports. The underlying regulatory mechanism is invisible, making the outcome feel like a natural law of supply and demand. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: ENVIRONMENTAL ADVOCATE (PITON) — Focuses on 'ghost flights'—empty or near-empty planes flown solely to meet the usage quota. From this view, the rule's original function is degraded, and its primary output is a performative, wasteful ritual. The high theater_ratio (0.71) satisfies the piton gate.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MARKET REFORMER (SCAFFOLD) — Views the rule as a temporary, flawed system to be replaced by more efficient market-based mechanisms like slot auctions. The ongoing debate and potential for reform act as an implicit sunset clause. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41. The classification gate for scaffold (χ ≤ 0.30) is not met, indicating the reform effort is not yet structurally effective, but the perspective remains.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL (TANGLED ROPE) — Recognizes both the genuine coordination function (managing scarce capacity) and the asymmetric extraction (creating barriers to entry that benefit incumbents). This matches the claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(airport_slot_use_it_or_lose_it_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(airport_slot_use_it_or_lose_it, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(airport_slot_use_it_or_lose_it, TR),
    TR >= 0.70.

:- end_tests(airport_slot_use_it_or_lose_it_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Represents the significant economic opportunity extracted from potential competitors and transferred to incumbents as a durable asset (grandfathered slots). Suppression (0.75): High, as the rule makes it extremely difficult for new entrants to acquire commercially viable slots at key airports, effectively locking them out. Theater Ratio (0.71): High, primarily due to the phenomenon of 'ghost flights,' a purely performative action to satisfy the rule with no economic or transport function. This ratio has increased over time as demand volatility has risen. The `has_sunset_clause` is set to true to reflect the persistent, organized political efforts to replace the system with auctions, which functions as a structural pressure for reform, enabling the Scaffold perspective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is a diagnostic exemplar. Incumbents see a stable coordination mechanism (Rope). New entrants see a coercive barrier to entry (Snare). Passengers see an unchangeable market reality (Mountain). Environmentalists see a wasteful, degraded ritual (Piton). Reformers see a temporary, flawed policy to be replaced (Scaffold). The analytical view synthesizes these, identifying a system with both coordination and extraction functions (Tangled Rope). The type is determined by the observer's structural relationship to the flow of value and opportunity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent airlines) with arbitrage exit options have a very low directionality (d), resulting in negative effective extraction (χ), classifying the constraint as a Rope. Victims (new entrants) with constrained exit options have a high d, resulting in high χ that crosses the Snare threshold. Powerless victims (passengers) with trapped exit options experience the highest d, perceiving the constraint's effects as an inescapable Mountain. The different structural positions mathematically generate the different classifications from the same set of base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a single regulation, officially framed as pure coordination (Rope), is structurally a Tangled Rope. The label 'efficient resource allocation' masks the significant extractive function of creating barriers to entry. Only by adopting multiple perspectives—especially that of the agent from whom opportunity is extracted (the new entrant)—can the full nature of the constraint be understood, preventing the misclassification of a hybrid system as a purely beneficial one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_efficiency_vs_stability,
    'Is the network stability provided by grandfather rights more valuable to the overall system than the potential efficiency and competition gains from a pure market/auction system?',
    'Comparative economic modeling of auction-based vs. grandfathered systems, analyzing impacts on network connectivity, fare volatility, and airline solvency.',
    'If stability is paramount, the rule is closer to a Rope. If efficiency gains are large, it''s closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_vs_stability, conceptual, 'Trade-off between stability from grandfather rights and efficiency from market auctions').

omega_variable(
    ghost_flight_prevalence,
    'What is the true scale of economically and environmentally wasteful flights operated solely to retain slots, especially outside of major, publicly-acknowledged crises?',
    'Analysis of flight load factor data for routes at slot-constrained airports, correlated with slot retention deadlines.',
    'Higher prevalence confirms the Piton perspective and increases the theater_ratio. Lower prevalence suggests the rule is more functional than performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ghost_flight_prevalence, empirical, 'The true scale of ''ghost flights'' operated to satisfy the rule').

omega_variable(
    waiver_politicization,
    'To what extent are regulatory waivers of the rule driven by objective systemic crises versus the lobbying power of incumbent airlines?',
    'Political economy analysis correlating lobbying expenditures by airline alliances with the timing and scope of slot rule waivers.',
    'High correlation suggests the rule is an actively managed Snare whose enforcement is negotiable for powerful actors. Low correlation suggests it operates more like a rigid, if flawed, Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(waiver_politicization, empirical, 'Extent to which waivers are driven by crisis vs. incumbent lobbying').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(airport_slot_use_it_or_lose_it, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airp_tr_t2005, airport_slot_use_it_or_lose_it, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(airp_tr_t2015, airport_slot_use_it_or_lose_it, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(airp_tr_t2025, airport_slot_use_it_or_lose_it, theater_ratio, 2025, 0.71).

% Extraction over time
narrative_ontology:measurement(airp_be_t2005, airport_slot_use_it_or_lose_it, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(airp_be_t2015, airport_slot_use_it_or_lose_it, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(airp_be_t2025, airport_slot_use_it_or_lose_it, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(airport_slot_use_it_or_lose_it, resource_allocation).
narrative_ontology:affects_constraint(airport_slot_use_it_or_lose_it, airline_ticket_pricing_algorithms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
