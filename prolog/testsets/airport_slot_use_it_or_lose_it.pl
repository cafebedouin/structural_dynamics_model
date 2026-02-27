% ============================================================================
% CONSTRAINT STORY: airport_slot_use_it_or_lose_it
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: airport_slot_use_it_or_lose_it
 *   human_readable: Use-it-or-lose-it Rule for Airport Landing Slots
 *   domain: economic/aviation_regulation
 *
 * SUMMARY:
 *   The use-it-or-lose-it (80% utilization) rule for airport landing slots at
 *   congested hubs operates as a mechanism to prevent slot hoarding and
 *   speculation. Ostensibly a coordination mechanism ensuring slots go to
 *   airlines that actually fly them, the rule functions as a barrier to entry
 *   protecting incumbent carriers. The constraint reveals itself differently
 *   from each structural position: legacy carriers see coordination and
 *   protection; new entrants see a catch-22 lock-out; the allocation
 *   authority sees procedural routine; competition regulators see a temporary
 *   measure that has become permanent; and civilizational observers risk
 *   naturalizing the rule as an immutable consequence of airport physics. The
 *   extractiveness value (0.52) reflects moderate-to-high asymmetric rent
 *   extraction: legacy carriers capture protected slot rents while new
 *   entrants bear the full cost of exclusion. Suppression is high (0.58)
 *   because the rule is backed by regulatory authority and there is no legal
 *   pathway for new entrants to challenge or bypass it without proving they
 *   can already operate at scale. Theater has risen over 30 years (0.25 →
 *   0.41) because enforcement has become increasingly procedural and
 *   performative as the original coordination rationale has been superseded
 *   by rent-protection logic.
 *
 * KEY AGENTS:
 *   - Legacy Carriers (e.g., BA, Lufthansa, Air France): Primary beneficiaries (institutional/arbitrage) — capture protection rent from slot scarcity; design and control the rule; benefit from suppression of new competition
 *   - New Entrant Airlines (e.g., Ryanair, easyJet early phase): Primary victims (powerless/trapped) — catch-22 exclusion: cannot access slots without proving utilization they cannot achieve without slots; no regulatory pathway to challenge
 *   - Regional and Secondary Carriers: Mixed beneficiary/victim (moderate/constrained) — benefit from protection against low-cost entrants but constrained by forced operations to maintain slot holdings; experience tangled rope coordination-extraction mix
 *   - Airport Authority / Slot Coordinator: Institutional implementer (institutional/arbitrage) — operates the allocation mechanism; maintains regulatory appearance of neutrality; protected from carrier pressure by administrative procedures
 *   - Competition Authority (EU, FAA, equivalent): Regulator (organized/constrained) — theoretically can repeal or modify the rule but constrained by incumbent carrier lobbying and path dependency; sees the rule as a temporary measure with no political capital to sunset it
 *   - Market Efficiency: Victim abstraction (powerless/trapped) — slot misallocation, forced unprofitable operations, suppressed competition, and reduced consumer choice; abstract collective good with no advocate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(airport_slot_use_it_or_lose_it, 0.52).
domain_priors:suppression_score(airport_slot_use_it_or_lose_it, 0.58).
domain_priors:theater_ratio(airport_slot_use_it_or_lose_it, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, extractiveness, 0.52).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(airport_slot_use_it_or_lose_it, tangled_rope).
narrative_ontology:human_readable(airport_slot_use_it_or_lose_it, "Use-it-or-lose-it Rule for Airport Landing Slots").
narrative_ontology:topic_domain(airport_slot_use_it_or_lose_it, "economic/aviation_regulation").

domain_priors:requires_active_enforcement(airport_slot_use_it_or_lose_it).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, incumbent_legacy_carriers).
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, slot_allocation_authority).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, new_entrant_airlines).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, market_efficiency).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, slot_underutilization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW ENTRANT AIRLINE (SNARE) — Powerless to acquire premium slots at congested airports without proving 80% utilization they cannot achieve without the slots. Trapped in a catch-22: cannot grow without slots, cannot access slots without existing scale. Bears full extraction cost through perpetual exclusion. Maximum suppression: no regulatory pathway to challenge the barrier, no arbitrage opportunity, no exit.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL CARRIER (TANGLED ROPE) — Constrained by utilization thresholds but also benefits from the barrier to entry that protects their existing slot portfolio from new competition. Experiences both coordination (regular slot schedules enable interline partnerships) and extraction (forced to operate unprofitable routes to maintain slot holdings). Some agency through seasonal variation and route adjustment, but ultimate exit option is exit from the market entirely.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGACY CARRIER (ROPE) — Institutional beneficiary with arbitrage exit. The constraint solves a coordination problem: it locks their dominant slot position, enabling long-term network planning and slot swapping among legacy carriers through informal agreements. Low experienced extraction because they designed and control the rule. Benefits from suppression of new entrants without regulatory overhead costs.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPETITION AUTHORITY (SCAFFOLD) — Organized agent (EU aviation authority, FAA, similar) implementing a transitional mechanism: the 80% rule was intended as a temporary response to slot scarcity, but has calcified into a permanent barrier. The authority sees the constraint as having a sunset clause in principle (when capacity constraints ease, the rule becomes unnecessary), but enforcement inertia has prevented sunsetting. Low effective extraction because the authority retains theoretical power to repeal; constrained in practice by incumbent carrier lobbying.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SLOT ALLOCATION AUTHORITY (PITON) — The administrative body managing slot allocation has become a theater: it maintains an air of neutral governance while systematically protecting incumbent interests. The rule's original function (preventing speculation) has been replaced by performative metrics (80% utilization reports, annual reviews) that change nothing. The authority has arbitrage (could theoretically reallocate or repeal), but institutional inertia means it doesn't. High theater ratio (0.41 is relatively high for slot administration) reflects that much administrative activity is procedural performance rather than substantive reallocation.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational/universal perspective, airport runway capacity is a genuine physical limit: a congested airport has a maximum throughput (movements per hour), and allocating scarce slots requires some rule. This perspective sees the use-it-or-lose-it rule as an immutable feature of managing scarcity. However, the structural data contradicts the mountain classification: the rule is contingent and contestable (slot trading markets, dynamic pricing, congestion-pricing mechanisms all exist as alternatives). The engine's false summit detector identifies this as naturalization of a policy choice.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, mountain,
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
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): The rule creates asymmetric extraction favoring incumbents. Legacy carriers capture two rents: (1) protection against new competition (entry barrier), and (2) ability to operate marginal/unprofitable routes profitably because the slots themselves have value. New entrants and market efficiency bear costs. The value is moderate-to-high because the extraction is substantial but not total — secondary trading markets (where permitted) and route flexibility allow some mitigation. Suppression (0.58): High coercive content. The rule is backed by regulatory authority with no legal exit pathway. New entrants cannot challenge it through competition law (it is explicitly authorized regulation), cannot arbitrage around it (slots are location-specific and non-tradeable at many hubs), and cannot exit without abandoning the market. However, suppression is not total because enforcement varies by jurisdiction (EU vs US vs elsewhere) and some lobbying pressure exists. Theater ratio (0.41): Moderate. The slot allocation process has become increasingly procedural over time: annual utilization reviews, administrative hearings, exemption requests. These are performative — they change nothing about the fundamental barrier structure. The original coordination function (preventing speculation) has been replaced by routine administration that maintains the status quo. Theater has risen over the 30-year interval as the rule has ossified from temporary measure to permanent feature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival gap driven by power asymmetry and exit options. The legacy carrier sees rope (coordination + arbitrage), experiencing low effective extraction because they control the rule. The new entrant sees snare (pure extraction, no exit), experiencing maximum extraction because they are powerless and trapped. The regional carrier sees tangled rope (mixed coordination and extraction), experiencing moderate extraction because they have some agency through route adjustment but cannot escape the utilization requirement. The allocation authority sees itself as a neutral administrator (procedural view), but the competition authority sees it as a captured regulator (institutional inertia view). The gap between the beneficiary's rope and the victim's snare is maximal: the same rule appears as beneficial coordination to one and catastrophic exclusion to the other. This gap reveals the extraction mechanism: the constraint's entire function is to protect incumbents by preventing entry.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's effective extractiveness (χ) derives from their structural position relative to the constraint. Legacy carriers have institutional power and arbitrage exit (can trade slots informally, adjust routes, influence policy), so their derived directionality is low — they experience the constraint as coordination. New entrants are powerless relative to the rule (no regulatory standing, no exit from the constraint, full cost bearing), so their derived directionality is high — they experience snare extraction. Regional carriers are moderate and constrained (they benefit from protection but pay compliance costs through forced operations), so their derived directionality is middle-range — they experience tangled rope. The allocation authority has institutional power but constrained exit (theoretically could repeal but faces political barriers), producing moderate directionality. The analytical observer's mountain perspective is rejected by the structural data: the rule is contingent (slot trading markets exist; dynamic pricing alternatives exist) and contestable (regulatory authority could repeal it), so naturalizing it as immutable physics is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the tangled rope classification correctly captures the constraint's dual nature: it has a genuine coordination function (preventing slot speculation and enabling long-term network planning) AND asymmetric extraction (protection of incumbents, entry barrier for new competitors). The critical mandatrophy question is: 'Is the coordination function the primary purpose, or merely a rationalization for extraction?' Historical evidence and the perspectival gap suggest extraction is primary: (1) secondary trading markets (e.g., proposed London slot trading) achieve better coordination without the extraction, (2) the utilization threshold has been static despite changing market conditions, suggesting inertia rather than dynamic coordination, (3) lobbying intensity correlates with incumbent rent protection rather than coordination benefit. The theater ratio's rise (0.25 → 0.41) further indicates that performative routine has replaced functional coordination. The constraint resolves as tangled rope with dominant extraction component: the coordination function is real but subordinate to rent extraction. A pure rope or pure snare classification would fail — rope ignores the entry barrier, snare ignores the legitimate coordination of schedule certainty among incumbents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_capacity_utilization,
    'What would actual runway utilization rates be under a slot trading market (London ITA-2008 counterfactual) versus current use-it-or-lose-it enforcement?',
    'Comparative analysis of slot utilization and revenue metrics in airports with vs without secondary trading; econometric models of counterfactual capacity utilization under dynamic pricing',
    'If trading markets achieve higher utilization with lower forced operations: the constraint is revealed as pure extraction masquerading as coordination. If trading creates different inefficiencies: the coordination function becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_capacity_utilization, empirical, 'Capacity utilization under alternative allocation mechanisms').

omega_variable(
    incumbent_advantage_magnitude,
    'How much of legacy carrier profitability derives from slot scarcity protection versus operational efficiency?',
    'Decomposition of carrier profitability by route segment, slot ownership cohort, and capacity utilization; cost accounting for forced-operation losses vs protection rent; competitive analysis of carriers with vs without premium slots',
    'If protection rent > 30% of profitability: the constraint is primarily extractive. If < 10%: the constraint may be legitimately coordinating efficient service. If 10-30%: tangled rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_advantage_magnitude, empirical, 'Incumbent profitability attributable to slot protection versus efficiency').

omega_variable(
    new_entrant_viability_threshold,
    'Is the utilization threshold (80%) technically achievable for a new entrant with access to 5-10 slots, or does it require pre-existing network scale?',
    'Feasibility analysis of new entrant entry scenarios with varying slot allocations; historical data on entrant utilization trajectories at competing less-congested airports; simulation of break-even network size for various utilization thresholds',
    'If achievable: the snare classification is disputed; the rule allows entry and the barrier is surmountable (rope or scaffold). If impossible without scale: the snare is confirmed; no regulatory pathway exists for competitive entry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(new_entrant_viability_threshold, empirical, 'Technical feasibility of 80% utilization for new entrant airlines').

omega_variable(
    sunset_clause_removal_mechanism,
    'What political and institutional conditions would be required to repeal the use-it-or-lose-it rule or replace it with secondary trading?',
    'Institutional analysis of regulatory capture in aviation; comparative study of regulatory changes in EU, US, Australia aviation slot systems; identification of coalition dynamics (competition authority vs carriers vs consumer advocates) necessary for rule change',
    'If sunset is technically possible but politically impossible: the constraint is revealed as resilient extraction (snare with institutional protection). If sunset is feasible: the scaffold classification is strengthened; the rule is contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_removal_mechanism, conceptual, 'Political feasibility of rule change or sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(airport_slot_use_it_or_lose_it, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slot_tr_t0, airport_slot_use_it_or_lose_it, theater_ratio, 0, 0.25).
narrative_ontology:measurement(slot_tr_t15, airport_slot_use_it_or_lose_it, theater_ratio, 15, 0.35).
narrative_ontology:measurement(slot_tr_t30, airport_slot_use_it_or_lose_it, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(slot_be_t0, airport_slot_use_it_or_lose_it, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(slot_be_t15, airport_slot_use_it_or_lose_it, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(slot_be_t30, airport_slot_use_it_or_lose_it, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(airport_slot_use_it_or_lose_it, resource_allocation).
narrative_ontology:affects_constraint(airport_slot_use_it_or_lose_it, airline_market_concentration).
narrative_ontology:affects_constraint(airport_slot_use_it_or_lose_it, airport_capacity_constraint).
narrative_ontology:affects_constraint(airport_slot_use_it_or_lose_it, slot_trading_secondary_market).

% DUAL FORMULATION NOTE:
% The use-it-or-lose-it rule is downstream of underlying airport capacity constraint (physical runway limit) and upstream of market concentration effects. The upstream constraint (airport_capacity_constraint) has lower extractiveness (0.15-0.25 range: the physical limit is genuine). The use-it-or-lose-it rule transforms that natural bottleneck into an institutional barrier with high extractiveness (0.52) through regulatory mechanism design. Secondary market decomposition: slot trading and use-it-or-lose-it are related but distinct constraints — trading rules solve different coordination problems than utilization rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(airport_slot_use_it_or_lose_it, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
