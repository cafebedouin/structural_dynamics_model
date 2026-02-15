% ============================================================================
% CONSTRAINT STORY: dexy_gold_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_dexy_gold_protocol, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dexy_gold_protocol
 *   human_readable: DexyGold Seigniorage Mechanism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   DexyGold is a seigniorage-based algorithmic stablecoin on the Ergo
 *   blockchain, pegged to the price of gold (XAU). It maintains its peg via
 *   an on-chain algorithmic "Bank" that mints tokens at oracle rates and an
 *   Automated Market Maker (AMM) for trading. Stability is enforced through
 *   "top-up swaps" where the Bank intervenes if the market price deviates
 *   from the oracle price. A key feature is a redemption lock on liquidity
 *   pools during significant de-pegs to prevent reserve draining.
 *
 * KEY AGENTS (by structural relationship):
 *   - Liquidity Providers (LPs): Primary target (moderate/constrained) — bear extraction via redemption locks during crises.
 *   - Arbitrageurs: Primary beneficiary (organized/arbitrage) — profit by enforcing the peg between the Bank and the AMM.
 *   - Retail Traders: Secondary beneficiary (powerless/mobile) — use the protocol as a tool to gain exposure to gold.
 *   - Analytical Observer: Sees the full structure of the coordination mechanism and its failure modes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dexy_gold_protocol, 0.20).
domain_priors:suppression_score(dexy_gold_protocol, 0.20).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(dexy_gold_protocol, 0.11).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dexy_gold_protocol, extractiveness, 0.20).
narrative_ontology:constraint_metric(dexy_gold_protocol, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(dexy_gold_protocol, theater_ratio, 0.11).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(dexy_gold_protocol, tangled_rope).

% --- Binary flags ---
% The protocol's rules (e.g., redemption locks) are enforced by the smart
% contract code itself. This is a form of active, automated enforcement.
% This declaration is critical to prevent misclassification as a Scaffold.
domain_priors:requires_active_enforcement(dexy_gold_protocol). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(dexy_gold_protocol, arbitrageurs).
%
% Who bears disproportionate cost?
% LPs are the victims of the redemption lock mechanism during de-pegs.
narrative_ontology:constraint_victim(dexy_gold_protocol, lp_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (LIQUIDITY PROVIDER)
% LPs face a Snare during de-pegs. The protocol explicitly prevents LP token
% redemptions if the oracle rate is significantly below the LP rate to prevent
% draining reserves. This "locks" the agent into the constraint during crisis.
constraint_indexing:constraint_classification(dexy_gold_protocol, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ARBITRAGEUR)
% For an arbitrageur, the protocol is a pure coordination mechanism (Rope)
% that provides profit opportunities for enforcing the peg. They have full
% exit optionality and benefit directly from the protocol's existence.
constraint_indexing:constraint_classification(dexy_gold_protocol, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (peg maintenance)
% and the asymmetric extraction (LP locks). This dual nature, combined with
% active code-based enforcement, defines a Tangled Rope.
constraint_indexing:constraint_classification(dexy_gold_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE RETAIL TRADER
% To a user simply looking to hedge into gold, Dexy is a tool (Rope).
% Unlike a trapped LP, a retail trader can usually exit via the AMM,
% provided there is liquidity.
constraint_indexing:constraint_classification(dexy_gold_protocol, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE PROTOCOL DEVELOPER (INSTITUTIONAL)
% From the perspective of the developers or the foundation supporting the
% protocol, it is a public good and coordination mechanism (Rope) for the
% ecosystem.
constraint_indexing:constraint_classification(dexy_gold_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dexy_gold_protocol_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the gap between the LP (target) and Arbitrageur (beneficiary).
    constraint_indexing:constraint_classification(dexy_gold_protocol, snare, context(agent_power(moderate), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(dexy_gold_protocol, rope, context(agent_power(organized), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical view must resolve the conflict into Tangled Rope.
    constraint_indexing:constraint_classification(dexy_gold_protocol, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify all three required properties for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(dexy_gold_protocol, _),
    narrative_ontology:constraint_victim(dexy_gold_protocol, _),
    domain_priors:requires_active_enforcement(dexy_gold_protocol).

:- end_tests(dexy_gold_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.20) is low because the protocol's primary
 *   function is coordination (peg maintenance) rather than value extraction.
 *   The key feature is the *conditional* extraction mechanism: the redemption
 *   lock that activates only during crises. This creates a sharp perspectival
 *   gap. The suppression score (0.20) is low as it's an open-source protocol
 *   in a competitive ecosystem of stablecoins.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for arbitrageurs and retail traders, it's a Rope that
 *   provides a useful service. For Liquidity Providers, it becomes a Snare
 *   at the worst possible moment, when they are trapped by redemption locks
 *   while their assets devalue. The analytical perspective must classify this
 *   as a Tangled Rope, as it possesses both a genuine coordination function
 *   (beneficiary exists) and an asymmetric extraction mechanism (victim exists)
 *   that is actively enforced by the code.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `arbitrageurs` directly profit from the price discrepancies
 *     the protocol is designed to correct. Their actions serve the protocol's
 *     goal.
 *   - Victim: `lp_providers` bear the cost of stability during crises. Their
 *     capital is locked to prevent a bank run, making them involuntary lenders
 *     of last resort to the protocol.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates how a system can be both a coordination tool and
 *   an extractive instrument depending on market conditions and one's role.
 *   Classifying it as a pure Rope would ignore the coercive lock-in mechanism.
 *   Classifying it as a pure Snare would ignore its legitimate and effective
 *   coordination function during normal operation. The Tangled Rope
 *   classification correctly captures this duality. The addition of
 *   `requires_active_enforcement` is crucial to distinguish it from a Scaffold,
 *   as the enforcement (the lock) is permanent, not temporary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_dexy_gold_protocol,
    'Can the anti-draining measures (redemption locks) effectively stop cyclic arbitrage-top-up attacks under sustained, high-volatility market conditions?',
    'Mainnet performance monitoring during a major market crash, comparing reserve depletion rates to similar protocols without locks.',
    'If the locks fail or are insufficient, the protocol collapses, turning the Tangled Rope into a Snare for all remaining participants, not just LPs.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_dexy_gold_protocol, empirical, 'Resilience of redemption locks against sophisticated economic attacks.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dexy_gold_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.20) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(dexy_gold_protocol, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */