% ============================================================================
% CONSTRAINT STORY: eu_renewable_energy_mandate
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_renewable_energy_mandate, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_renewable_energy_mandate
 *   human_readable: EU Renewable Energy Directive and Support Schemes
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint represents the complex web of EU directives and national
 *   support schemes (e.g., feed-in tariffs, contracts for difference) designed
 *   to accelerate the transition to renewable energy sources like wind and
 *   solar. While it serves a clear coordination function—solving the
 *   collective action problem of decarbonization—it does so through a mechanism
 *   that imposes costs broadly (on consumers via levies, taxpayers via subsidies)
 *   while concentrating benefits narrowly (on renewable energy developers and
 *   equipment manufacturers). This creates a significant perspectival gap between
 *   those who bear the costs and those who reap the rewards.
 *
 * KEY AGENTS (by structural relationship):
 *   - eu_energy_consumers: Primary target (powerless/trapped) — bear extraction via energy bill levies.
 *   - fossil_fuel_sector: Secondary target (powerful/constrained) — suppressed by regulation and subsidized competition.
 *   - renewable_energy_sector: Primary beneficiary (institutional/arbitrage) — benefits from subsidies and guaranteed markets.
 *   - eu_climate_policymakers: Secondary beneficiary (institutional/arbitrage) — architects of the policy, benefit politically.
 *   - national_governments: Inter-institutional actor (institutional/constrained) — tasked with implementation, balancing EU targets against domestic political and economic pressures.
 *   - analytical_observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_renewable_energy_mandate, 0.48).
domain_priors:suppression_score(eu_renewable_energy_mandate, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_renewable_energy_mandate, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, extractiveness, 0.48).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_renewable_energy_mandate, tangled_rope).
narrative_ontology:human_readable(eu_renewable_energy_mandate, "EU Renewable Energy Directive and Support Schemes").
narrative_ontology:topic_domain(eu_renewable_energy_mandate, "economic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_renewable_energy_mandate). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, eu_climate_policymakers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, eu_energy_consumers).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, fossil_fuel_sector).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE ENERGY CONSUMER (SNARE)
% Experiences the constraint as a non-negotiable surcharge on energy bills.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.1 (continental scope) ≈ 0.75. This is well into Snare territory (χ ≥ 0.66).
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE RENEWABLE ENERGY DEVELOPER (ROPE)
% Benefits from subsidies, grants, and favorable market access.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.1 ≈ -0.06. A negative effective extraction.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% Engine derives canonical d for analytical ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66. This lands at the boundary of
% Tangled Rope and Snare, correctly identifying the high level of extraction.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: The Fossil Fuel Sector (SNARE)
% Experiences the constraint as an existential threat designed to suppress its business.
% Engine derives d from: victim membership + constrained exit → d ≈ 0.85 → f(d) ≈ 1.35
% χ = 0.48 * 1.35 * 1.1 ≈ 0.71. Clearly a Snare.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% Perspective 4B: The National Government Implementer (TANGLED ROPE)
% Must manage the trade-offs between EU goals, domestic industry, and voter satisfaction.
% Not a full beneficiary or victim; exit is constrained by EU law.
% This mixed role derives a d value closer to 0.5. Let's estimate d ≈ 0.55 → f(d) ≈ 0.75.
% χ = 0.48 * 0.75 * 1.1 ≈ 0.40. This is a perfect Tangled Rope classification.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_renewable_energy_mandate_tests).

test(perspectival_gap_consumer_vs_developer) :-
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope, context(agent_power(analytical), _, _, _)),
    true.

test(inter_institutional_gap) :-
    % Verify that two institutional actors with different exit options see different things.
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    true.

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, _),
    narrative_ontology:constraint_victim(eu_renewable_energy_mandate, _),
    domain_priors:requires_active_enforcement(eu_renewable_energy_mandate).

:- end_tests(eu_renewable_energy_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value is high, reflecting the significant wealth transfer from the general public
 *     to a concentrated group of energy producers. It is not higher because the policy has a genuine, large-scale
 *     coordination function (decarbonization) which provides a non-extractive public good.
 *   - Suppression (0.75): The policy framework is explicitly designed to suppress fossil fuels through regulation,
 *     carbon pricing, and subsidized competition. This high score reflects its coercive nature against incumbents.
 *   - This combination of high extraction, high suppression, and a legitimate coordination function is the hallmark of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - The gap is profound. For an energy consumer (powerless, trapped), the constraint is a Snare: a pure cost added to a
 *     necessary utility, with no direct, tangible benefit. The climate benefits are diffuse and generational.
 *   - For a renewable energy developer (institutional, arbitrage), the constraint is a Rope: a coordination mechanism that
 *     de-risks investment and creates a stable market, resulting in negative effective extraction (subsidy).
 *   - The analytical observer sees both sides, classifying it as a Tangled Rope—a tool that achieves coordination
 *     through asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the flow of costs and benefits.
 *   - Beneficiaries (`renewable_energy_sector`): Receive direct financial subsidies and market access. Their `arbitrage`
 *     exit option (they can invest elsewhere) gives them immense leverage, driving their derived `d` value close to 0.
 *   - Victims (`eu_energy_consumers`): Pay for the subsidies through levies. Their `trapped` status (they cannot easily opt
 *     out of the energy grid) makes them price-takers, driving their derived `d` value close to 1.0.
 *   This clear structural asymmetry is why the classification diverges so sharply across perspectives.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the different experiences of two institutional actors. The renewable energy sector has `arbitrage` exit
 *   and is a pure beneficiary, seeing a Rope. National governments have `constrained` exit (bound by EU law) and a mixed
 *   beneficiary/victim role (balancing goals vs costs), leading them to see a Tangled Rope. This highlights how exit options
 *   and structural roles, not just power level, determine an agent's experience of a constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not label the policy as a pure Rope, ignoring the huge
 *   extractive costs borne by consumers. It also does not label it as a pure Snare, which would ignore the genuine and necessary
 *   coordination function of accelerating the energy transition. The Tangled Rope classification acknowledges this dual nature,
 *   which is critical for sound policy analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_eu_renewable_energy_mandate,
    'Will the long-term public good (climate stability, energy independence) generated by this policy ultimately outweigh the near-term extractive costs imposed on consumers?',
    'Analysis of levelized cost of energy (LCOE) for mature renewable installations post-subsidy vs. fossil fuels over a 50-year horizon, plus quantified models of climate change mitigation benefits.',
    'If benefits outweigh costs, the policy is a successful, albeit extractive, coordination mechanism (Tangled Rope). If costs outweigh benefits, it is a value-destructive Snare that has been mislabeled as coordination.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_renewable_energy_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint. Models the intensification
% of the policy from early, smaller-scale programs to the current EU-wide mandate.

% Theater ratio over time (slight increase as bureaucracy grows):
narrative_ontology:measurement(eu_rem_tr_t0, eu_renewable_energy_mandate, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eu_rem_tr_t5, eu_renewable_energy_mandate, theater_ratio, 5, 0.20).
narrative_ontology:measurement(eu_rem_tr_t10, eu_renewable_energy_mandate, theater_ratio, 10, 0.30).

% Extraction over time (increases as subsidies and levies became widespread):
narrative_ontology:measurement(eu_rem_ex_t0, eu_renewable_energy_mandate, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(eu_rem_ex_t5, eu_renewable_energy_mandate, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(eu_rem_ex_t10, eu_renewable_energy_mandate, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This policy primarily functions by redirecting capital and grid resources.
narrative_ontology:coordination_type(eu_renewable_energy_mandate, resource_allocation).

% Network relationships (structural influence edges)
% This mandate is structurally coupled with grid stability and carbon pricing mechanisms.
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, national_energy_grid_stability).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, eu_carbon_trading_scheme).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are used in this file. The standard derivation chain, which
% uses beneficiary/victim declarations in combination with agent exit options,
% is sufficient to correctly differentiate the directionality for all key
% agents, including the two distinct institutional perspectives (developer vs.
% national government).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */