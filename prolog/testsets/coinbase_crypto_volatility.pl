% ============================================================================
% CONSTRAINT STORY: coinbase_crypto_volatility
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_coinbase_crypto_volatility, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coinbase_crypto_volatility
 *   human_readable: Centralized Exchange Market Structure on Volatile Assets
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint models the market structure of a centralized crypto
 *   exchange like Coinbase, which is built upon the inherent price volatility
 *   of crypto assets. While raw price volatility may be a Mountain-like
 *   phenomenon, the exchange's rules, fee structures, and liquidation
 *   mechanisms constitute a man-made system. This system provides a genuine
 *   coordination function (liquidity, market access) while simultaneously
 *   enabling asymmetric extraction from less sophisticated participants,
 *   especially during periods of high volatility.
 *
 * KEY AGENTS (by structural relationship):
 *   - Retail Investors: Primary target (powerless/trapped) — bears the cost of volatility through fees, liquidations, and inability to exit during crashes.
 *   - Coinbase & Arbitrageurs: Primary beneficiary (institutional/arbitrage) — benefits directly from trading volume driven by volatility.
 *   - Institutional Investors: Secondary beneficiary (organized/mobile) — uses the platform as a regulated on-ramp, benefiting from the coordination function.
 *   - Analytical Observer: Sees the full hybrid structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coinbase_crypto_volatility, 0.42).
domain_priors:suppression_score(coinbase_crypto_volatility, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(coinbase_crypto_volatility, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coinbase_crypto_volatility, extractiveness, 0.42).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(coinbase_crypto_volatility, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(coinbase_crypto_volatility, tangled_rope).
narrative_ontology:human_readable(coinbase_crypto_volatility, "Centralized Exchange Market Structure on Volatile Assets").
narrative_ontology:topic_domain(coinbase_crypto_volatility, "economic/technological").

% --- Binary flags ---
domain_priors:requires_active_enforcement(coinbase_crypto_volatility). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, coinbase_and_arbitrageurs).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(coinbase_crypto_volatility, retail_investors_without_hedging).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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
% For the retail trader during a flash crash, volatility combined with the
% exchange's structure is a Snare. They are liquidated or unable to exit
% positions due to platform congestion, resulting in asymmetric capital loss.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For Coinbase as an entity, volatility is a Rope. It is the engine of their
% business. Without price movement, there is no trading volume and no revenue.
% They use this structure to pull capital into their ecosystem.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees the complete system: a genuine coordination service
% (liquidity provision) that is structurally coupled with an extractive
% mechanism that profits from that same volatility, often at the expense of
% the least sophisticated participants.
constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coinbase_crypto_volatility_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(coinbase_crypto_volatility, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(coinbase_crypto_volatility, _),
    narrative_ontology:constraint_victim(coinbase_crypto_volatility, _),
    domain_priors:requires_active_enforcement(coinbase_crypto_volatility).

:- end_tests(coinbase_crypto_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file misclassified this as a Mountain by conflating raw
 *   market volatility with the man-made market structure built to harness it.
 *   The revised scores reflect the properties of the exchange system itself.
 *   - Base Extractiveness (0.42): Represents the portion of value captured through fees, spreads, and liquidations that exceeds the pure cost of providing the service.
 *   - Suppression (0.85): High, as retail users have few liquid, regulated, and user-friendly alternatives. Centralized exchanges are a chokepoint.
 *   - Enforcement: The matching engine, liquidation protocols, and fee collection mechanisms are all actively enforced by the platform operator.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For Coinbase (beneficiary), the system is a Rope; volatility is the source of revenue and the core of their business model. For a retail investor (target), especially one caught in a downturn, the same system is a Snare that extracts capital through forced liquidations and an inability to exit positions. The analytical view must be Tangled Rope to capture both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `coinbase_and_arbitrageurs` directly profit from the trading volume that volatility generates. Their relationship is subsidized by the system's structure.
 *   - Victim: `retail_investors_without_hedging` bear the direct costs of this volatility through fees and catastrophic losses during crashes, from which the platform still profits.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly prevents two errors. It avoids the "natural law" fallacy of calling it a Mountain, which would ignore the designed, extractive elements of the exchange. It also avoids calling it a pure Snare, which would ignore the genuine and valuable coordination function (liquidity, price discovery, access) that the exchange provides. The Tangled Rope classification acknowledges that it is both a service and a trap, depending on the market conditions and the agent's position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_coinbase_crypto_volatility,
    'Can Coinbase survive a multi-year period of low volatility and low prices without its secondary revenue streams (e.g., staking) becoming dominant?',
    'Track the ratio of non-transaction revenue to transaction revenue over a 24-month crypto winter.',
    'If non-transaction revenue fails to grow, the business model remains a Tangled Rope dependent on volatility. If it succeeds, the company may be transitioning to a more stable Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(coinbase_crypto_volatility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the cyclical nature of crypto markets. Extraction
% spikes during peak volatility and crashes.
%
% Theater ratio over time:
narrative_ontology:measurement(coinbase_crypto_volatility_tr_t0, coinbase_crypto_volatility, theater_ratio, 0, 0.10).
narrative_ontology:measurement(coinbase_crypto_volatility_tr_t5, coinbase_crypto_volatility, theater_ratio, 5, 0.10).
narrative_ontology:measurement(coinbase_crypto_volatility_tr_t10, coinbase_crypto_volatility, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(coinbase_crypto_volatility_ex_t0, coinbase_crypto_volatility, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(coinbase_crypto_volatility_ex_t5, coinbase_crypto_volatility, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(coinbase_crypto_volatility_ex_t10, coinbase_crypto_volatility, base_extractiveness, 10, 0.42).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(coinbase_crypto_volatility, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships in this system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */