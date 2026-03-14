% ============================================================================
% CONSTRAINT STORY: slot_trading_secondary_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_slot_trading_secondary_market, []).

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
 *   constraint_id: slot_trading_secondary_market
 *   human_readable: Slot Trading Secondary Market Coordination with Extraction
 *   domain: economic/market_structure
 *
 * SUMMARY:
 *   Slot trading secondary markets create a structurally hybrid constraint
 *   combining genuine coordination function with asymmetric extraction. The
 *   secondary market solves real coordination problems: operators with excess
 *   capacity can relinquish slots; operators needing additional slots can
 *   acquire them without waiting for new primary allocation; prices provide
 *   signals for capacity optimization. However, the constraint simultaneously
 *   creates and maintains artificial scarcity that benefits early slot
 *   recipients (who received slots at original allocation cost, often zero or
 *   minimal) at the expense of late entrants who must purchase at secondary
 *   market prices significantly above original cost. The allocator benefits
 *   from both coordination (efficient resource use) and extraction (fee
 *   revenue and administrative control). Brokers facilitate price discovery
 *   while extracting bid-ask spreads. The original allocation framework, now
 *   degraded to ritual status, theoretically governed slot distribution, but
 *   actual allocation has shifted entirely to secondary trading. This
 *   constraint demonstrates how coordination and extraction can coexist
 *   within a single mechanism, with different agents experiencing
 *   fundamentally different classification outcomes depending on their
 *   structural position in the allocation hierarchy.
 *
 * KEY AGENTS:
 *   - Slot Allocators: Institutional beneficiary (institutional/arbitrage) — controls primary allocation and captures secondary market coordination fees; can reallocate slots at will
 *   - Early Slot Holders: Powerful beneficiary (powerful/arbitrage) — received initial allocation at minimal cost; benefit from secondary market price appreciation
 *   - Late Entrants: Powerless victim (powerless/trapped) — cannot access primary allocation after initial window; forced to purchase at secondary market prices often 2-10x original cost
 *   - Small Operators: Moderate victim (moderate/constrained) — can purchase slots but at substantial cost; benefit from some market coordination but bear asymmetric fees
 *   - Broker Ecosystem: Organized actor (organized/constrained) — coordinate matching and price discovery; extract bid-ask spreads; face switching costs preventing alternative platforms
 *   - End Users: Collective victim (powerless/trapped) — ultimately bear secondary market markups in final service delivery costs; no direct participation in trading
 *   - Analytical Observer: System view (analytical/analytical) — identifies hybrid structure as genuinely coordinating AND extracting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(slot_trading_secondary_market, 0.58).
domain_priors:suppression_score(slot_trading_secondary_market, 0.52).
domain_priors:theater_ratio(slot_trading_secondary_market, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(slot_trading_secondary_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(slot_trading_secondary_market, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(slot_trading_secondary_market, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(slot_trading_secondary_market, tangled_rope).
narrative_ontology:human_readable(slot_trading_secondary_market, "Slot Trading Secondary Market Coordination with Extraction").
narrative_ontology:topic_domain(slot_trading_secondary_market, "economic/market_structure").

domain_priors:requires_active_enforcement(slot_trading_secondary_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(slot_trading_secondary_market, slot_allocators).
narrative_ontology:constraint_beneficiary(slot_trading_secondary_market, early_slot_holders).
narrative_ontology:constraint_beneficiary(slot_trading_secondary_market, brokers).
narrative_ontology:constraint_victim(slot_trading_secondary_market, late_entrants).
narrative_ontology:constraint_victim(slot_trading_secondary_market, small_operators).
narrative_ontology:constraint_victim(slot_trading_secondary_market, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ENTRANT (SNARE) — New operators entering after slots have been allocated face no viable alternative to the secondary market. Purchase prices far exceed original allocation cost. No exit: must buy into the system or abandon business. High suppression through artificial scarcity and price barriers.
constraint_indexing:constraint_classification(slot_trading_secondary_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL OPERATOR (TANGLED ROPE) — Benefits from some market coordination (slot liquidity, price discovery) but bears asymmetric extraction through broker fees and price markups. Can technically exit by relocating or changing business model, but costs are substantial. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(slot_trading_secondary_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SLOT ALLOCATOR (ROPE) — Experiences the secondary market as coordination enabling asset redeployment and continuous optimization. Captures administrative benefit from pricing signals and liquidity. Net beneficiary with high exit optionality — controls the allocation mechanism itself.
constraint_indexing:constraint_classification(slot_trading_secondary_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY SLOT HOLDER (ROPE) — Received slots at original allocation (often zero or minimal cost). Secondary market enables profitable exit or lease arrangements. Significant arbitrage opportunity between original cost and secondary market price. Pure beneficiary.
constraint_indexing:constraint_classification(slot_trading_secondary_market, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BROKER ECOSYSTEM (TANGLED ROPE) — Brokers coordinate matching and price discovery (legitimate function) while extracting fees and spreads. Have some agency to develop alternative platforms but face switching costs and liquidity network effects. See constraint as both coordinating mechanism and extraction vehicle.
constraint_indexing:constraint_classification(slot_trading_secondary_market, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORIGINAL ALLOCATION FRAMEWORK (PITON) — The original allocation logic (first-come, lottery, merit-based) has been largely replaced by price-based secondary market as the de facto allocation mechanism. The original framework persists in ritual form but all real allocation now occurs through trading. Theater ratio high: regulatory oversight, transparency mandates, but actual function degraded as allocation has shifted entirely to secondary trading.
constraint_indexing:constraint_classification(slot_trading_secondary_market, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a system view, slot secondary markets solve genuine coordination problems (matching supply and demand, enabling Coasean reallocation, price discovery) while simultaneously creating and maintaining artificial scarcity that benefits early holders at the expense of new entrants. The constraint is genuinely hybrid: coordination function is real; extraction mechanism is also real and asymmetrically distributed.
constraint_indexing:constraint_classification(slot_trading_secondary_market, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slot_trading_secondary_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(slot_trading_secondary_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slot_trading_secondary_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(slot_trading_secondary_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(slot_trading_secondary_market, TR),
    TR >= 0.70.

:- end_tests(slot_trading_secondary_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Reflects asymmetric price markups for late entrants alongside genuine coordination benefits for the overall system. The base value increased from 0.28 to 0.58 over the measurement interval as secondary market premiums accumulated and became the de facto allocation mechanism. Suppression (0.52): Moderate. Barriers include artificial slot scarcity (by design of primary allocation), network effects concentrating brokers, switching costs for alternative platforms, and regulatory barriers to new allocation mechanisms. However, suppression is not total — some operators do exit, some alternative platforms exist (though with reduced liquidity), and regulatory oversight provides some transparency. Theater ratio (0.38): Moderate. The original allocation framework maintains regulatory compliance and transparency requirements but performs minimal actual allocation function — all real distribution occurs via secondary trading. Theater has increased from 0.15 to 0.38 as regulatory ritual has grown while coordination function has shifted entirely to secondary markets.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between beneficiaries and victims of the original allocation. Allocators and early holders see pure coordination (Rope) — the market enables efficiency and price discovery. Late entrants see pure extraction (Snare) — the secondary market forces them to subsidize early recipients. Both are structurally correct from their own positions. Small operators and brokers occupy hybrid perspectives (Tangled Rope) reflecting genuine coordination benefits bundled with asymmetric extraction. The piton classification of the original allocation framework emerges from the gap between formal function (distributing slots) and actual function (price-based reallocation through secondary trading). The theater increases as regulatory requirements proliferate while substantive allocation has moved entirely to trading mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status plus exit options. Allocators and early holders are beneficiaries with arbitrage exit options (d ≈ 0.10-0.20), experiencing negative or low χ — the constraint subsidizes them. Late entrants are victims with trapped exit (d ≈ 0.95), experiencing maximum χ — the constraint extracts from them. Small operators occupy middle position: moderate power, constrained exit, mixed beneficiary/victim status (they benefit from some coordination but bear fees and price markups) produce d ≈ 0.65-0.70, yielding moderate χ. Brokers as organized institutional actors with constrained exit (switching costs) derive d ≈ 0.55-0.60. The analytical observer (d ≈ 0.72) recognizes the system-level hybrid structure without being captured by any single agent's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that Tangled Rope is the accurate analytical classification when all perspectives are integrated. Early holders' Rope classification is NOT wrong — they genuinely experience coordination without extraction. Late entrants' Snare classification is NOT wrong — they genuinely experience pure extraction. The analytical observer recognizes that both experiences are produced by the same constraint structure: the secondary market coordinates (real function) while extracting through artificial scarcity (real mechanism). Mandatrophy is resolved by acknowledging that hybrid classification does not dissolve the perspectival gap — it clarifies that the gap is a structural feature, not an observational ambiguity. The question 'is this coordination or extraction?' has answer: yes, both, and the asymmetry of who coordinates and who extracts is the constraint's defining property.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allocation_fairness_threshold,
    'At what secondary market price markup does coordination benefit become dominated by extraction cost?',
    'Comparative analysis of operators'' profitability pre- and post-secondary market adoption; measurement of markup ratio relative to original allocation cost over time',
    'If markup < 50%: coordination benefit dominates; reclassify toward Rope. If markup > 200%: extraction dominates; reclassify toward Snare for new entrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_fairness_threshold, empirical, 'Threshold between coordination value and extractive cost').

omega_variable(
    original_allocation_design_intentionality,
    'Was the original allocation intentionally designed to create secondary market scarcity, or is secondary market extraction an unintended consequence?',
    'Historical documentation of allocation design decisions; analysis of allocator incentives; comparison with alternative allocation mechanisms that could have supported secondary trading without scarcity-creation',
    'If intentional: increases perceived extraction; mandatrophy shifts toward Snare. If unintended: maintains Tangled Rope classification; opens policy intervention pathways.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_allocation_design_intentionality, conceptual, 'Whether scarcity-creating aspects of allocation were intentional').

omega_variable(
    alternative_platform_viability,
    'Could operators successfully migrate to alternative slot trading platforms without significant loss of liquidity and price discovery?',
    'Network analysis of broker ecosystems; empirical testing of liquidity fragmentation effects; comparison with multi-platform trading environments in other markets',
    'If viable: broker suppression is lower than measured; exit_options upgrade from constrained to mobile for some agents. If not viable: lock-in is structural; exit genuinely constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative trading platforms could function at scale').

omega_variable(
    slot_necessity_versus_privilege,
    'Are slots a genuine operational necessity or a privilege bundled with market access?',
    'Comparative analysis of operators who secure slots versus those who operate without; cost-benefit analysis of slot acquisition versus alternative service delivery models',
    'If necessity: trapped exit is accurate; Snare classification holds. If privilege: exit is more mobile than measured; reclassify constrained agents to mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slot_necessity_versus_privilege, empirical, 'Whether slots are operational necessity or market privilege').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(slot_trading_secondary_market, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slot_tr_t0, slot_trading_secondary_market, theater_ratio, 0, 0.15).
narrative_ontology:measurement(slot_tr_t3, slot_trading_secondary_market, theater_ratio, 3, 0.28).
narrative_ontology:measurement(slot_tr_t6, slot_trading_secondary_market, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(slot_be_t0, slot_trading_secondary_market, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(slot_be_t3, slot_trading_secondary_market, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(slot_be_t6, slot_trading_secondary_market, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(slot_trading_secondary_market, resource_allocation).
narrative_ontology:affects_constraint(slot_trading_secondary_market, access_to_market_services).
narrative_ontology:affects_constraint(slot_trading_secondary_market, pricing_power_asymmetry).

% DUAL FORMULATION NOTE:
% Slot trading secondary markets decompose into two distinct constraints: (1) Market Coordination Function (ε≈0.15, Rope) — genuine price discovery and liquidity provision; (2) Scarcity-Based Extraction (ε≈0.72, Snare) — the artificial scarcity created by original allocation design that benefits early holders. These are presented here as a single unified Tangled Rope story to capture the simultaneity of both functions in the operational constraint. Alternative decomposition: separate stories for coordination mechanics vs. extraction mechanism linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(slot_trading_secondary_market, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
