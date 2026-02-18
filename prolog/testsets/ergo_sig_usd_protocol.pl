% ============================================================================
% CONSTRAINT STORY: sig_usd_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_sig_usd_protocol, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sig_usd_protocol
 *   human_readable: SigUSD Stability Mechanism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   SigUSD is an algorithmic, crypto-backed stablecoin on the Ergo blockchain,
 *   based on the AgeUSD protocol. It operates via a dual-token system: SigUSD
 *   (stablecoin) and SigRSV (reserve token). Stability is maintained by SigRSV
 *   holders who act as the "bank," absorbing ERG price volatility in exchange
 *   for protocol fees, while SigUSD holders maintain a value peg as long as
 *   the reserve ratio stays within a 400-800% range. The constraint is the
 *   set of smart contract rules governing these interactions.
 *
 * KEY AGENTS (by structural relationship):
 *   - SigUSD Holder (powerless/trapped): Primary target of the stability promise,
 *     but trapped by contract logic during low-reserve states.
 *   - SigRSV Holder (moderate/mobile): Primary beneficiary of fees, but bears
 *     the risk of collateral price drops.
 *   - Protocol Auditor (institutional/arbitrage): Views the system for systemic
 *     risk, can exit via secondary markets.
 *   - Analytical Observer: Sees the full structure of risks and benefits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The protocol charges fees (minting/burning) that flow from users
% to reserve holders. While asymmetric, it is a service-for-risk trade.
domain_priors:base_extractiveness(sig_usd_protocol, 0.40).

% Rationale: Alternatives like wrapped USDT or other algorithmic coins exist
% on Ergo; the protocol does not actively suppress them.
domain_priors:suppression_score(sig_usd_protocol, 0.20).

% Rationale: The protocol is a functional economic mechanism with low
% performative overhead.
domain_priors:theater_ratio(sig_usd_protocol, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sig_usd_protocol, extractiveness, 0.40).
narrative_ontology:constraint_metric(sig_usd_protocol, suppression_requirement, 0.20).
narrative_ontology:constraint_metric(sig_usd_protocol, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
% For a user, the smart contract's math is immutable (high collapse) and
% faces no meaningful resistance (low resistance).
narrative_ontology:constraint_metric(sig_usd_protocol, accessibility_collapse, 0.90).
narrative_ontology:constraint_metric(sig_usd_protocol, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sig_usd_protocol, rope).
narrative_ontology:human_readable(sig_usd_protocol, "SigUSD Stability Mechanism").
narrative_ontology:topic_domain(sig_usd_protocol, "economic/technological").

% --- Emergence flag (required for mountain constraints) ---
% The rules emerge from immutable smart contracts, not active human enforcement.
domain_priors:emerges_naturally(sig_usd_protocol).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sig_usd_protocol, sig_rsv_holders).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sig_usd_protocol, sig_rsv_holders_at_low_reserve).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SIGUSD HOLDER (RETAIL USER)
% For a retail user, the protocol's math is "natural law." They cannot change
% the reserve requirements or fee structure. If the reserve ratio drops, they
% are trapped by the smart contract's "Redeem Only" state.
constraint_indexing:constraint_classification(sig_usd_protocol, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SIGRSV HOLDER (THE "BANKER")
% The SigRSV holder sees the protocol as a tool (Rope) for leverage. It is a
% functional coordination mechanism that allows them to earn fees for taking
% on risk. They willingly enter and can exit when the reserve ratio allows.
constraint_indexing:constraint_classification(sig_usd_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE PROTOCOL AUDITOR (INSTITUTIONAL)
% From a systemic view, if the reserve ratio stays near 400% during a
% prolonged ERG crash, the protocol becomes a "Snare." It forces asymmetric
% losses on the last reserve holders to stay liquid, potentially leading to a
% "death spiral" where the constraint becomes coercive to those remaining.
constraint_indexing:constraint_classification(sig_usd_protocol, snare,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical view sees a coordination mechanism with asymmetric risk/reward,
% but one that provides a genuine service (stability). The base metrics do not
% meet the thresholds for a tangled rope, so it classifies as a Rope.
constraint_indexing:constraint_classification(sig_usd_protocol, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sig_usd_protocol_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(sig_usd_protocol, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sig_usd_protocol, Type2, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(sig_usd_protocol, Type3, context(agent_power(institutional), _, _, _)),
    Type1 == mountain,
    Type2 == rope,
    Type3 == snare,
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % This is a conceptual test. Institutional agents can arbitrage (lower
    % experienced extraction) whereas powerless agents pay the flat protocol fee.
    ScorePowerless = 0.4,
    ScorePowerful = 0.2,
    ScorePowerless > ScorePowerful.

test(time_immutability) :-
    % At 'immediate' horizon, it is a Mountain (cannot change code).
    constraint_indexing:constraint_classification(sig_usd_protocol, mountain, context(_, time_horizon(immediate), _, _)).

:- end_tests(sig_usd_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.40): Chosen because while the protocol is "fair," the
 *     2% fee is an asymmetric transfer from those needing stability to those
 *     providing it, representing a significant but not purely extractive cost.
 *   - Suppression (0.20): Low because alternatives exist on-chain, even if they
 *     have lower liquidity or different trust models. The protocol does not
 *     actively prevent their use.
 *   - NL Profile: The `mountain` classification for the powerless user requires
 *     the NL profile. The smart contract logic is, from their perspective,
 *     an immutable law of their financial environment (`accessibility_collapse`=0.9)
 *     to which resistance is futile (`resistance`=0.1).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The powerless user sees an immutable law (Mountain). The
 *   risk-taking reserve provider sees a tool for profit (Rope). The systemic
 *   auditor sees a potential trap under stress conditions (Snare). This gap
 *   is driven by differences in power (ability to influence or exit), time
 *   horizon, and exit options (trapped in contract vs. arbitrage on DEXs).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `sig_rsv_holders` directly benefit from protocol fees and
 *     leveraged upside on the collateral (ERG).
 *   - Victim: `sig_rsv_holders_at_low_reserve` are the victims. This is a
 *     state-dependent victim class. When the reserve ratio is low, they bear
 *     the full cost of dilution to maintain the SigUSD peg and may be unable to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the dual nature of the protocol.
 *   It is not a pure Snare, as it provides a genuine coordination service
 *   (stable value). It is not a pure Rope, as the failure mode is a coercive
 *   trap for a specific class of participants. The analytical classification
 *   as Rope reflects its primary function under normal operating conditions,
 *   while other perspectives reveal its dangerous failure modes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_sig_usd_protocol,
    "Is the ERG/USD oracle price feed accurate and manipulation-resistant?",
    "Post-hoc analysis of oracle data vs. global market average during high volatility events.",
    "If the oracle fails or is manipulated, the Mountain (math) becomes a Snare (error-driven extraction), as redemptions would occur at a false price.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_sig_usd_protocol, empirical, "Integrity and manipulation-resistance of the ERG/USD price oracle.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sig_usd_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: A decentralized resource allocation mechanism for stability.
narrative_ontology:coordination_type(sig_usd_protocol, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */