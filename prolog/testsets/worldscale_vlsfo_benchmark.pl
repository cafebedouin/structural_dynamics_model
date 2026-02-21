% ============================================================================
% CONSTRAINT STORY: worldscale_vlsfo_benchmark
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_worldscale_vlsfo_benchmark, []).

:- use_module(constraint_indexing).
:- use_moudle(domain_priors).
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
    domain_priors:emerges_naturally/1.

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
 *   The Worldscale flat rates, an industry standard for pricing oil tanker
 *   freight, were updated to use Very Low Sulfur Fuel Oil (VLSFO) as the
 *   benchmark fuel. This creates a structural subsidy for ships equipped with
 *   "scrubbers," allowing them to burn cheaper High Sulfur Fuel Oil (HSFO)
 *   while being compensated at the higher VLSFO rate. This rule facilitates
 *   a massive arbitrage opportunity, extracting value from charterers and
 *   transferring it to scrubber-equipped ship owners and the traders who
 *   charter them.
 *
 * KEY AGENTS (by structural relationship):
 *   - Oil Traders & Refiners (Charterers): Primary target (powerless/trapped) — bear the cost of the inflated benchmark rate.
 *   - Scrubber-Equipped Ship Owners & a Major Trading House: Primary beneficiary (institutional/arbitrage) — profit from the spread between the VLSFO benchmark and their actual HSFO fuel costs.
 *   - The Worldscale Associations: Secondary Actor/Architect (institutional/constrained) — sets and maintains the standard, enabling the system.
 *   - Analytical Observer: The Deferential Realism system — sees the full structure as a Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(worldscale_vlsfo_benchmark, 0.65).
domain_priors:suppression_score(worldscale_vlsfo_benchmark, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(worldscale_vlsfo_benchmark, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, extractiveness, 0.65).
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(worldscale_vlsfo_benchmark, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(worldscale_vlsfo_benchmark, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(worldscale_vlsfo_benchmark). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, scrubber_equipped_ship_owners).
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, arbitrage_trading_houses).
narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, worldscale_associations).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(worldscale_vlsfo_benchmark, oil_traders_and_refiners).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Oil traders and refiners who must charter these ships. The market for VLCCs
% has been cornered, leaving them trapped and forced to pay the inflated rate.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The trading house that cornered the market and owners of scrubber-equipped ships.
% They exploit the rule for arbitrage profit. For them, it is a highly beneficial
% coordination mechanism.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function (a standardized contract rate) and
% the massive, asymmetric extraction it enables. This is the canonical view.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The rule-setting body itself. As an industry association, its purpose is to
% provide coordination, making it a beneficiary. However, it is constrained by
% its powerful members and does not directly capture the arbitrage profits.
% Its 'constrained' exit option results in a different derived directionality
% than the arbitrage-based beneficiary.
constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(worldscale_vlsfo_benchmark_tests).

test(perspectival_gap, [nondet]) :-
    % Verify the core Rope/Snare gap.
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(worldscale_vlsfo_benchmark, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_compliance) :-
    % Verify that all conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(worldscale_vlsfo_benchmark, _),
    narrative_ontology:constraint_victim(worldscale_vlsfo_benchmark, _),
    domain_priors:requires_active_enforcement(worldscale_vlsfo_benchmark).

test(threshold_validation) :-
    % Verify the metrics align with a high-extraction Tangled Rope/Snare.
    domain_priors:base_extractiveness(worldscale_vlsfo_benchmark, E),
    domain_priors:suppression_score(worldscale_vlsfo_benchmark, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(worldscale_vlsfo_benchmark_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. Represents the large, structurally
 *     guaranteed profit margin (the "scrubber spread") created by the
 *     benchmark mismatch. This value is captured directly by beneficiaries
 *     from victims.
 *   - Suppression Score (S=0.80): High. The Worldscale rates are the de facto
 *     global standard for tanker freight. For major players, there is no
 *     viable alternative for benchmarking contracts, making the system highly
 *     coercive.
 *   - The combination of a legitimate coordination function (standardizing
 *     rates) with severe, asymmetric extraction makes this a canonical
 *     Tangled Rope from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   - The gap is stark. For charterers (victims), the rule is a Snare. They
 *     are trapped by a cornered market and a coercive standard, forced to pay
 *     a price disconnected from the seller's actual costs.
 *   - For the trading house and scrubber-equipped owners (beneficiaries), the
 *     rule is a perfect Rope. It's a coordination mechanism that generates
 *     massive, risk-free profit through arbitrage. The effective extraction (χ)
 *     is negative for them, as the system subsidizes their operations.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `scrubber_equipped_ship_owners` and the `arbitrage_trading_houses`
 *     that charter them. They directly profit from the VLSFO-HSFO price spread.
 *     The `worldscale_associations` are also beneficiaries, as their relevance
 *     depends on providing such coordination tools.
 *   - Victims: `oil_traders_and_refiners` who need to charter the ships. They
 *     pay the inflated price, directly funding the beneficiaries' profits.
 *   - The engine correctly derives a low `d` for beneficiaries (especially with
 *     `arbitrage` exit) and a high `d` for victims (with `trapped` exit),
 *     driving the perspectival split in χ and classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case exemplifies how a seemingly neutral technical standard (a Rope)
 *   can be weaponized into a powerful extractive tool (a Snare). Classifying
 *   it as a pure Snare would miss its genuine coordination function, while
 *   classifying it as a Rope would ignore the enormous asymmetric extraction.
 *   The Tangled Rope classification correctly identifies this duality, preventing
 *   the mislabeling of coordinated extraction as either pure coordination or
 *   pure coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_worldscale_vlsfo_benchmark,
    'Was the VLSFO benchmark rule intentionally designed to subsidize scrubber-equipped ships, or was it a naive technical update that was later exploited?',
    'Internal documents or board minutes from the Worldscale Associations detailing the rationale for the benchmark change.',
    'Intentional design implies a deeply embedded, stable rent-seeking mechanism (Snare/Tangled Rope). An unforeseen loophole implies a flawed coordination system that might be corrected, making it more unstable.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(worldscale_vlsfo_benchmark, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the introduction of the new rule. Before the change, the
% benchmark was based on HSFO, so no scrubber spread existed (low ε).
% After the change (T=5), the extraction became possible and was realized.
% Base extractiveness is high (>0.46), so temporal data is required.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(wvb_tr_t0, worldscale_vlsfo_benchmark, theater_ratio, 0, 0.10).
narrative_ontology:measurement(wvb_tr_t5, worldscale_vlsfo_benchmark, theater_ratio, 5, 0.10).
narrative_ontology:measurement(wvb_tr_t10, worldscale_vlsfo_benchmark, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(wvb_ex_t0, worldscale_vlsfo_benchmark, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(wvb_ex_t5, worldscale_vlsfo_benchmark, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(wvb_ex_t10, worldscale_vlsfo_benchmark, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's primary coordination function is to provide a pricing standard.
narrative_ontology:coordination_type(worldscale_vlsfo_benchmark, information_standard).

% Network relationships (structural influence edges)
% This rule is a direct consequence of the IMO 2020 regulations that created the
% two tiers of fuel (VLSFO and HSFO). It structurally depends on that prior constraint.
narrative_ontology:affects_constraint(imo_2020_emissions_regulation, worldscale_vlsfo_benchmark).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation based
% on beneficiary/victim declarations and exit options (trapped, arbitrage,
* constrained) accurately models the power dynamics and perspectival gaps.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */