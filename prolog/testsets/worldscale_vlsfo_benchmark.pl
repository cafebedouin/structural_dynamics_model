% ============================================================================
% CONSTRAINT STORY: worldscale_vlsfo_benchmark
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The Worldscale flat rate system is an industry-wide benchmark for pricing
 *   oil tanker freight globally. In 2020, following International Maritime
 *   Organization (IMO) regulations requiring Very Low Sulfur Fuel Oil (VLSFO)
 *   or equivalent emissions abatement, the Worldscale benchmark was updated
 *   to use VLSFO as the reference fuel for rate calculations. This constraint
 *   presents a structural tension between legitimate coordination —
 *   standardized fuel specification supports environmental compliance and
 *   reduces transaction costs — and extraction through oligopolistic pricing
 *   control. The benchmark simultaneously achieves environmental regulation
 *   goals AND locks independent shipowners into fuel cost structures they
 *   cannot negotiate independently. The extraction mechanism depends on two
 *   dynamics: (1) the VLSFO specification was standardized by regulatory fiat
 *   (IMO), not by market consensus, making exit costly; (2) major shipping
 *   operators and fuel suppliers can arbitrage the gap between benchmark and
 *   operational costs, while independents cannot. The benchmark exhibits
 *   increasing extractiveness over its interval (0.38 → 0.52) as VLSFO supply
 *   tightens and price premiums accumulate.
 *
 * KEY AGENTS:
 *   - Major Shipping Operators: Primary beneficiary (institutional/arbitrage) — capture margin arbitrage through scale, operational flexibility, and futures hedging capabilities
 *   - Fuel Suppliers: Primary beneficiary (institutional/arbitrage) — benefit from standardized specification creating guaranteed demand; control pricing through supply-side oligopoly
 *   - Independent Shipowners: Primary victim (powerless/trapped) — locked into benchmark with no exit option; must absorb VLSFO cost premium without operational hedging capacity
 *   - Developing Nation Traders: Secondary victim (moderate/constrained) — lack capital, information, and negotiating power to exit; bear asymmetric extraction through information opacity
 *   - Environmental Regulation Coalition (IMO, EU, national regulators): Organized enforcer (organized/constrained) — imposed VLSFO requirement for coordination (sulfur emissions reduction) but inadvertently enabled extraction through pricing centralization
 *   - Analytical Observer: System-level perspective (analytical/analytical) — recognizes dual structure: genuine coordination function coupled with extractive lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(worldscale_vlsfo_benchmark, 0.52).
domain_priors:suppression_score(worldscale_vlsfo_benchmark, 0.48).
domain_priors:theater_ratio(worldscale_vlsfo_benchmark, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, extractiveness, 0.52).
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(worldscale_vlsfo_benchmark, tangled_rope).
narrative_ontology:human_readable(worldscale_vlsfo_benchmark, "Worldscale Flat Rate Benchmark Based on VLSFO").
narrative_ontology:topic_domain(worldscale_vlsfo_benchmark, "economic/technological").

domain_priors:requires_active_enforcement(worldscale_vlsfo_benchmark).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, major_shipping_operators).
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, fuel_suppliers).
narrative_ontology:constraint_victim(worldscale_vlsfo_benchmark, independent_shipowners).
narrative_ontology:constraint_victim(worldscale_vlsfo_benchmark, developing_nation_traders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT SHIPOWNER (SNARE) — Locked into Worldscale pricing mechanism with no exit. Cannot negotiate outside benchmark; fuel specification locked to VLSFO benchmark even if alternative fuels are operationally cheaper. Bears full cost of benchmark changes while receiving no coordination benefit. Suppression enforced by industry convention and contract standardization.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION TRADER (TANGLED ROPE) — Constrained by lack of market information, capital to hedge fuel price volatility, and limited negotiating power. Derives some benefit from standardized pricing (reduces transaction costs) but also bears extraction through inability to adjust to local fuel availability or cheaper alternatives. Exit constrained by market structure, not absolute.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR SHIPPING OPERATOR (ROPE) — Experiences Worldscale as pure coordination mechanism. Standardized benchmark reduces transaction costs, enables rapid freight negotiation, and allows hedging via fuel futures markets. Can arbitrage between benchmark and actual fuel costs through scale and operational flexibility. Net beneficiary — has exit options (can shift routes, fuel suppliers, contract terms).
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUEL SUPPLIER (ROPE) — Benefits from standardized VLSFO benchmark as it creates guaranteed market demand and pricing transparency. Can forecast supply requirements and manage margins through the specification. Institutional actor with arbitrage options — can source from multiple regions or adjust blend to maintain VLSFO compliance.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL REGULATION COALITION (TANGLED ROPE) — Organized agents (IMO, EU, national regulators) embedded the VLSFO requirement as environmental coordination mechanism (sulfur cap regulation). Derives genuine coordination function: standardized fuel specification reduces monitoring complexity and ensures compliance. But also extracts: the VLSFO benchmark locks in higher fuel costs, transfers environmental compliance expense to shipowners rather than fuel producers, and centralizes pricing power. Enforcement is active and continuous.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Views the constraint as a hybrid structure coupling environmental regulation (coordination) with oligopolistic pricing control (extraction). The VLSFO benchmark simultaneously achieves the legitimate goal of sulfur emission reduction AND enables major operators and fuel suppliers to lock in margins that independent shipowners cannot escape. Requires active enforcement by regulators and market convention. The beneficiaries (major operators, fuel suppliers) have genuine coordination benefit; the victims (independents, developing traders) bear extraction without equivalent coordination gain.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(worldscale_vlsfo_benchmark_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(worldscale_vlsfo_benchmark, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(worldscale_vlsfo_benchmark_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The constraint extracts measurable economic value from trapped agents (independent shipowners). The VLSFO premium over alternative fuels or heavy fuel oil can range 8-15% of operating costs, concentrated among those without exit options. However, extraction is not maximal (snare threshold ≥0.46) because: (1) the coordination benefit (sulfur compliance) is genuine and reduces external costs; (2) some hedging and operational adjustment is possible; (3) regulatory environment remains contestable (alternative fuels are emerging). Suppression (0.48): Moderate-high. Barriers to exit include: standardization enforced by IMO and major shipping companies; limited alternative fuel supply; capital constraints for independents to adopt alternative-compliant vessels; and market convention making Worldscale the default. But suppression is not total — some alternative benchmarking has been attempted, and regulatory change can unlock new fuel specifications. Theater ratio (0.35): Low. The benchmark is functionally transparent and operationally necessary — fuel prices feed directly into freight rates through published formulas. Theater comes not from obscurity but from the legitimacy theater of environmental regulation masking the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a perspectival split aligned with market structure. Major operators (institutional/arbitrage) experience pure coordination — the benchmark solves the real problem of standardizing fuel specs across global routes. Independents (powerless/trapped) experience snare — they are locked into cost structures they cannot exit or negotiate. The analytical observer recognizes that both perspectives are structurally true: the constraint simultaneously solves a coordination problem (fuel standardization) AND enables extraction (pricing lock-in). The gap widens as VLSFO supply tightens and price premiums accumulate over the interval. The tangled rope classification for the analytical perspective reflects that beneficiaries' coordination benefit and victims' extraction mechanism are structurally coupled — you cannot have the standardization without the lock-in, and you cannot exit the lock-in without losing the coordination benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from market position and exit capacity. Major operators and fuel suppliers have arbitrage options — they can shift fuel sourcing, route optimization, and hedging strategies to capture upside from the benchmark. Their d values are low (~0.05-0.15), producing low effective extraction chi. Independent shipowners have no arbitrage options — they must accept Worldscale rates set by others and absorb fuel costs without hedging capacity. Their d values are high (~0.85-0.95), producing high effective extraction chi. Developing nation traders occupy an intermediate position: they have constrained exit options (can switch routes or delay voyages) but face capital and information barriers. Their d value is moderate (~0.55-0.65), producing moderate chi. The environmental regulators occupy an organized position with constrained exit (they can adjust specifications but face political pressure to maintain stability) — their d is moderate (~0.40-0.50). The engine derives these d values from beneficiary/victim declarations and exit options; the commentary reflects the structural relationships that justify the classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the tangled rope classification at the analytical level is structurally justified: the constraint has genuine coordination function (fuel standardization for environmental compliance) AND asymmetric extraction (pricing lock-in for independents). The snare classification from the powerless perspective is not a failure to recognize coordination — the powerless agent genuinely does not benefit from the coordination, they only experience the extraction. The rope classification from the major operator perspective is also correct — they genuinely do see only coordination benefits because they have exit optionality. The mandatrophy resolution is: the constraint is BOTH tangled rope AND snare AND rope, depending on where you sit. The false natural law would be classifying it as 'just a market mechanism' (mountain) or 'pure coordination' (rope). The real structure is hybrid coordination-extraction, with distribution of costs and benefits aligned with market power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_fuel_recognition,
    'Will alternative low-sulfur fuels (biofuels, ammonia, hydrogen) achieve sufficient standardization and supply to break the VLSFO benchmark lock?',
    'Tracking of IMO MEPC decisions on alternative fuel specifications; supply curve expansion for non-VLSFO compliant fuels; shipowner adoption rates of alternative fuel vessels',
    'If yes: benchmark loses extractive force (becomes rope/scaffold). If no: VLSFO lock persists 20+ years, extraction accumulates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fuel_recognition, empirical, 'Whether alternative fuels will break VLSFO benchmark dominance').

omega_variable(
    fuel_cost_divergence_mechanism,
    'How much of the VLSFO price premium reflects genuine scarcity/refinery constraint vs. collective pricing control by fuel suppliers?',
    'Econometric analysis of VLSFO price convergence to production cost; investigation of supply capacity utilization; comparison to sulfur removal cost models',
    'If genuine scarcity: benchmark is coordination response (rope). If pricing control: benchmark enables extraction (snare). Affects whether suppression reflects market structure or deliberate cartelization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fuel_cost_divergence_mechanism, empirical, 'Scarcity vs. pricing control in VLSFO cost structure').

omega_variable(
    negotiation_exit_optionality,
    'Can independent shipowners realistically exit Worldscale through private contracts, regional alternatives (e.g., Baltic Exchange precedent), or consortium arrangements?',
    'Market data on non-Worldscale freight rates; survey of independent shipowner contract terms; analysis of failed alternative benchmarking attempts',
    'If yes: constraints are mobile (not trapped). If no: confirms trap gate. Directly affects powerless perspective classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negotiation_exit_optionality, empirical, 'Whether exit from Worldscale benchmark is materially available').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(worldscale_vlsfo_benchmark, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_vlsfo_tr_t0, worldscale_vlsfo_benchmark, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ws_vlsfo_tr_t5, worldscale_vlsfo_benchmark, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ws_vlsfo_tr_t10, worldscale_vlsfo_benchmark, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ws_vlsfo_be_t0, worldscale_vlsfo_benchmark, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ws_vlsfo_be_t5, worldscale_vlsfo_benchmark, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ws_vlsfo_be_t10, worldscale_vlsfo_benchmark, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(worldscale_vlsfo_benchmark, information_standard).
narrative_ontology:affects_constraint(worldscale_vlsfo_benchmark, imf_fuel_specification_harmonization).
narrative_ontology:affects_constraint(worldscale_vlsfo_benchmark, shipping_emissions_cap_allocation).

% DUAL FORMULATION NOTE:
% The VLSFO benchmark decomposes into two related constraints: (1) fuel specification standardization (pure coordination, low ε), and (2) pricing lock-in mechanism (pure extraction, high ε). This story focuses on the joint constraint as experienced by actors in the shipping market. The upstream constraint is IMO fuel specification harmonization; the downstream constraint is emissions cap allocation and alternative fuel adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(worldscale_vlsfo_benchmark, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
