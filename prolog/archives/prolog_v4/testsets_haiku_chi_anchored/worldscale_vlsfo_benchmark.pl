% ============================================================================
% CONSTRAINT STORY: worldscale_vlsfo_benchmark
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_worldscale_vlsfo_benchmark, []).

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
 *   constraint_id: worldscale_vlsfo_benchmark
 *   human_readable: Worldscale Flat Rate Benchmark Based on VLSFO
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Worldscale flat rate benchmark, a century-old standardized pricing
 *   mechanism for oil tanker freight, was updated to use Very Low Sulfur Fuel
 *   Oil (VLSFO) as the reference fuel in response to IMO 2020 environmental
 *   regulations. This constraint exhibits the classic Tangled Rope structure:
 *   it solves a genuine market coordination problem (standardized fuel
 *   specifications reduce transaction costs across global shipping) while
 *   simultaneously enabling extraction through information asymmetry and
 *   institutional lock-in. Small independent tanker operators and high-sulfur
 *   fuel producers experience the benchmark as a Snare — they are trapped by
 *   a global pricing mechanism that embeds fuel cost premiums without
 *   transparent passthrough mechanisms. Major oil majors and fuel suppliers
 *   experience it as a Rope — they benefit from the coordination function
 *   while capturing rents through fuel sourcing monopolies. Environmental
 *   regulators and the sustainability coalition see it as a Scaffold — a
 *   temporary enforcement mechanism that will be displaced by alternative
 *   fuels within 15-30 years. The legacy Worldscale committee experiences its
 *   own institutional authority as degraded (Piton) — real pricing power has
 *   migrated to electronic trading platforms, yet the committee's rate
 *   publications maintain nominal governance authority. The analytical
 *   observer risks naturalizing the benchmark as a necessary market
 *   coordination tool, missing the contingent institutional choices (why
 *   VLSFO specifically? why Worldscale as opposed to decentralized pricing?)
 *   that enable the extraction.
 *
 * KEY AGENTS:
 *   - Small Independent Tanker Operators: Primary victims (powerless/trapped) — constrained by global benchmark with no exit option; forced to absorb fuel cost asymmetries
 *   - High Sulfur Fuel Oil Producers: Secondary victims (moderate/constrained) — face demand collapse from VLSFO mandate; benefit from fuel blending and infrastructure services but net extraction is severe
 *   - Major Oil Majors and Fuel Suppliers: Primary beneficiaries (institutional/arbitrage) — capture rents through fuel sourcing control and compliance infrastructure monopoly; experience benchmark as enabling coordination
 *   - Environmental Compliance Coalition: Organized agents (organized/mobile) — regulators, IMO, sustainability advocates building alternative fuel pathways; see benchmark as temporary enforcement mechanism with defined sunset
 *   - Major Shipping Lines and Charterers: Secondary institutional actors (organized/constrained) — have countervailing power through scale but constrained by regulatory mandate; see mixed coordination and extraction
 *   - Worldscale Committee: Legacy institutional actor (institutional/arbitrage) — maintains nominal governance authority through convention; real pricing power has migrated to electronic platforms (Piton degradation)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as necessary market coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(worldscale_vlsfo_benchmark, 0.38).
domain_priors:suppression_score(worldscale_vlsfo_benchmark, 0.42).
domain_priors:theater_ratio(worldscale_vlsfo_benchmark, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, extractiveness, 0.38).
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(worldscale_vlsfo_benchmark, tangled_rope).
narrative_ontology:human_readable(worldscale_vlsfo_benchmark, "Worldscale Flat Rate Benchmark Based on VLSFO").
narrative_ontology:topic_domain(worldscale_vlsfo_benchmark, "economic/technological").

domain_priors:requires_active_enforcement(worldscale_vlsfo_benchmark).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, fuel_cost_standardization_advocates).
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, environmental_compliance_regulators).
narrative_ontology:constraint_victim(worldscale_vlsfo_benchmark, high_sulfur_fuel_operators).
narrative_ontology:constraint_victim(worldscale_vlsfo_benchmark, small_tanker_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL INDEPENDENT TANKER OPERATORS (SNARE) — Trapped by global Worldscale standard with no alternative pricing mechanism. Cannot exit fuel cost asymmetry created by VLSFO benchmark. Forced to invest in compliance infrastructure (fuel quality certification, bunker management) without corresponding rate adjustment. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH SULFUR FUEL OIL PRODUCERS (TANGLED ROPE) — Constrained by environmental regulations phase-in and fuel demand collapse, but also benefit from compliance infrastructure demand and fuel blending services. The benchmark extraction is real (market contraction) but the coordination function is also real (standardized pricing enables market transparency). d≈0.68, f(d)≈1.00, σ=1.2 → χ≈0.46.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MAJOR OIL MAJORS AND FUEL SUPPLIERS (ROPE) — Benefit from standardized VLSFO benchmark through cost predictability, compliance infrastructure monopoly, and ability to arbitrage fuel sourcing globally. Experience the constraint as coordination: the benchmark solves a collective action problem (fuel quality standardization) while enabling extraction through information asymmetry. d≈0.12, f(d)≈0.08, σ=1.2 → χ≈0.04. Near-zero effective extraction because beneficiary position is so strong.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENVIRONMENTAL COMPLIANCE COALITION (SCAFFOLD) — Organized agents (environmental regulators, IMO, sustainability advocates) see the VLSFO benchmark as a temporary enforcement mechanism with a sunset: technology development in low-carbon fuels (biofuels, synthetic fuels, ammonia) will eventually displace the fossil-fuel-based benchmark entirely. The scaffold has both enforcement (regulatory mandate) and coordination (fuel quality standard) functions. Sunset timeline: 15-30 years as alternative fuels scale. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.16.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY WORLDSCALE COMMITTEE (PITON) — The Worldscale committee's institutional authority persists through inertia and convention, even as the benchmark itself (VLSFO) becomes outdated. The committee's governance function is largely performative: real pricing power has migrated to electronic trading platforms and bilateral negotiations. Theater_ratio=0.55 captures the partial degradation — the committee still publishes rates, but market pricing often differs materially. d≈0.08, f(d)≈-0.05, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MAJOR SHIPPING LINES AND CHARTERERS (TANGLED ROPE) — Organized institutional actors that see the VLSFO benchmark as both a coordination mechanism (industry-standard pricing) and an extraction mechanism (hidden fuel cost pass-through to cargo owners and smaller operators). Constrained by regulatory mandate but have countervailing power through scale and negotiation. d≈0.50, f(d)≈0.64, σ=1.2 → χ≈0.30.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MARKET COORDINATION VIEW (ROPE) — From a civilizational perspective, the VLSFO benchmark solves a genuine market coordination problem: standardized fuel specifications reduce transaction costs and information asymmetry across global shipping markets. The extraction observed by smaller actors may be a byproduct of market structure (concentration among majors) rather than an intrinsic feature of the benchmark itself. This perspective risks naturalizing what is actually a contingent institutional arrangement (why VLSFO and not an open-source fuel specification?). d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.49.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(worldscale_vlsfo_benchmark_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(worldscale_vlsfo_benchmark, TR),
    TR >= 0.70.

:- end_tests(worldscale_vlsfo_benchmark_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The VLSFO benchmark creates real cost asymmetries for small operators and high-sulfur fuel producers, but the extraction is not total — the benchmark does solve a genuine coordination problem (fuel quality standardization) that reduces broader transaction costs. The moderate value reflects that ~40% of the benchmark's function is coordination (legitimate) and ~60% is extraction (unfair distribution of compliance costs). Suppression (0.42): Moderate. Significant barriers include: lock-in to Worldscale flat rates (no decentralized alternative has achieved scale), regulatory mandate (IMO 2020 creates compliance necessity), and fuel sourcing concentration among majors. However, suppression is not total — small operators can source alternative fuels, electronic trading platforms provide partial exit to Worldscale rates, and some regions have fuel supplier diversity. Theater ratio (0.55): Moderate. The Worldscale committee's rate publications maintain performative authority (markets still reference them, trades are still denominated in Worldscale terms) but real pricing increasingly happens on electronic exchanges. The performance is substantial but declining as electronic platforms gain market share.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the classic Tangled Rope structure: small operators see extraction (Snare) because they lack countervailing power, while majors see coordination (Rope) because they benefit from the standardization while controlling fuel sources. The environmental coalition sees a temporary tool with a sunset (Scaffold) that will be displaced by alternative fuels. The Worldscale committee sees its own institutional degradation (Piton) — it still publishes rates but has lost real pricing authority. The analytical observer risks the false summit of naturalizing the benchmark as 'how markets coordinate fuel standards' when it is actually 'how concentrated institutional power distributes compliance costs' — a choice, not a law.
 *
 * DIRECTIONALITY LOGIC:
 *   Small tanker operators: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. High-sulfur fuel producers: Victims + constrained → d≈0.68, f(d)≈1.00. Significant extraction but not maximal; some benefit from compliance services. Major oil majors: Beneficiaries + arbitrage → d≈0.12, f(d)≈0.08. Near-zero effective extraction; net beneficiary. Environmental coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; coalition has agency and visible exit path. Worldscale committee: Institutional + arbitrage → d≈0.08, f(d)≈-0.05. Piton classification comes from theater gate, not extraction structure. Shipping lines: Organized + constrained → d≈0.50, f(d)≈0.64. Mixed position — have countervailing power but constrained by regulation. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Medium-high directionality — observer sees the coordination function but risks naturalizing the institutional lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends entirely on the observer's structural position relative to fuel sourcing control and regulatory compliance costs. The small operator sees pure extraction (Snare) because they cannot exit and must absorb costs. The major sees coordination (Rope) because they control the fuel source and benefit from standardization. The environmental actor sees temporary coordination with a sunset (Scaffold) because alternative fuels will displace the benchmark. No single type is 'correct' — the presheaf of perspectives IS the complete picture. The false summit risk is real: the analytical observer might naturalize the benchmark as 'how markets solve fuel standardization problems,' missing that the institutional choices (VLSFO specification, Worldscale lock-in, fuel sourcing concentration) are contingent and could be different. The mandatrophy is resolved by recognizing that all six types are valid perspectival readings of a structure that is simultaneously coordination (real fuel quality problem) and extraction (unequal cost distribution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fuel_cost_passthrough_mechanism,
    'What portion of VLSFO fuel cost premium is structurally embedded in the Worldscale benchmark versus passed through via spot price adjustment?',
    'Historical regression analysis of Worldscale flat rates vs VLSFO spot prices vs HSFO prices; comparison with alternative fuel pricing mechanisms (biofuel benchmarks, synthetic fuel indices)',
    'If passthrough is automatic (embedded): the benchmark is primarily coordination (Rope). If passthrough is delayed or incomplete: the benchmark is primarily extraction (Snare/Tangled Rope from small operators'' perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fuel_cost_passthrough_mechanism, empirical, 'Fuel cost passthrough structure in Worldscale benchmarks').

omega_variable(
    alternative_pricing_mechanism_viability,
    'Are decentralized or alternative benchmarks (blockchain-based pricing, transparent fuel spot markets, forward contracts) technically and operationally viable replacements for Worldscale flat rates?',
    'Technical feasibility analysis of alternative mechanisms; pilot tests of decentralized pricing in shipping; cost comparison of transaction overhead',
    'If viable alternatives exist: small operators'' exit option is ''mobile'' not ''trapped'', reducing d and χ. If alternatives fail at scale: Worldscale lock-in is structural, supporting Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pricing_mechanism_viability, empirical, 'Viability of alternative pricing mechanisms for tanker freight').

omega_variable(
    fuel_specification_lock_in,
    'Is the choice of VLSFO as the benchmark fuel a technical necessity or a contingent institutional choice that could be replaced with a more transparent or inclusive specification?',
    'Analysis of fuel quality variance across VLSFO suppliers; comparison with alternative specifications (blended fuels, synthetic fuels, open-source fuel standards); cost of specification change',
    'If VLSFO is technically optimal: benchmark choice is legitimate coordination (Rope). If choice is arbitrary or path-dependent: the benchmark naturalizes institutional power imbalance (false summit risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fuel_specification_lock_in, conceptual, 'Whether VLSFO specification choice is technically necessary or institutionally contingent').

omega_variable(
    environmental_compliance_equity,
    'Does the VLSFO mandate distribute environmental compliance costs equitably across operators by vessel size, operator scale, and geographic region?',
    'Analysis of compliance cost burden by operator type (major vs independent, large vs small vessels, regional differences); survey of fuel sourcing accessibility across port regions',
    'If inequitable: scaffold''s equity sunset is at risk — environmental benefit may come with unintended regressive distributional consequences. If equitable: scaffold classification is confirmed — the temporary measure achieves its coordination and environmental function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_compliance_equity, empirical, 'Equity of VLSFO compliance cost distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(worldscale_vlsfo_benchmark, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsvlsfo_tr_t0, worldscale_vlsfo_benchmark, theater_ratio, 0, 0.32).
narrative_ontology:measurement(wsvlsfo_tr_t5, worldscale_vlsfo_benchmark, theater_ratio, 5, 0.44).
narrative_ontology:measurement(wsvlsfo_tr_t10, worldscale_vlsfo_benchmark, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(wsvlsfo_be_t0, worldscale_vlsfo_benchmark, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(wsvlsfo_be_t5, worldscale_vlsfo_benchmark, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(wsvlsfo_be_t10, worldscale_vlsfo_benchmark, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(worldscale_vlsfo_benchmark, resource_allocation).
narrative_ontology:affects_constraint(worldscale_vlsfo_benchmark, imo_2020_sulfur_regulations).
narrative_ontology:affects_constraint(worldscale_vlsfo_benchmark, shipping_market_concentration).
narrative_ontology:affects_constraint(worldscale_vlsfo_benchmark, fuel_sourcing_monopolies).

% DUAL FORMULATION NOTE:
% The VLSFO benchmark is downstream of IMO 2020 sulfur reduction regulations and upstream of fuel sourcing monopolies and shipping market concentration. The benchmark itself (ε=0.38) represents the institutional choice to standardize around a specific fuel specification; the environmental regulation (IMO 2020) has its own ε reflecting the policy choice; fuel sourcing concentration has its own ε reflecting market structure. These form a constraint family where the benchmark is the central mechanism that translates regulatory intent into distributional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(worldscale_vlsfo_benchmark, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
