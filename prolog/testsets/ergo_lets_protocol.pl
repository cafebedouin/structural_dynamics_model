% ============================================================================
% CONSTRAINT STORY: ergo_lets_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-17
% ============================================================================

:- module(constraint_ergo_lets_protocol, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_lets_protocol
 *   human_readable: Ergo Local Exchange Trading System (LETS)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   LETS on the Ergo blockchain is a trustless mutual credit system where the
 *   sum of all participant balances is always zero. It allows communities
 *   to trade goods and services using a local currency created through
 *   "IOUs" backed by collateral or reputation, enforced via smart contracts.
 *   The protocol transforms currency from a commodity to be extracted into a
 *   public utility for coordination.
 *
 * KEY AGENTS (by structural relationship):
 *   - New_Entrant: Primary target (powerless/trapped) — faces a collateral requirement that acts as an unchangeable barrier.
 *   - Community_Member: Primary beneficiary (moderate/mobile) — uses the system to facilitate local trade without needing external fiat.
 *   - Protocol_Auditor: Analytical observer — sees the mathematical integrity of the zero-sum invariant as a logical necessity.
 *   - Over-leveraged_Defaulter: Secondary target (powerless/constrained) — faces coercive collateral seizure upon default.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The protocol is designed to be non-extractive; fees are
% typically minimal and used only to prevent spam.
domain_priors:base_extractiveness(ergo_lets_protocol, 0.15).
% Rationale: It is a voluntary, opt-in coordination layer. It does not
% suppress other economic models.
domain_priors:suppression_score(ergo_lets_protocol, 0.1).
% Rationale: The protocol's function is direct and computational; there is
% very little performative or theatrical activity.
domain_priors:theater_ratio(ergo_lets_protocol, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_lets_protocol, extractiveness, 0.15).
narrative_ontology:constraint_metric(ergo_lets_protocol, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ergo_lets_protocol, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% The "Sum = 0" invariant is a logical property of the system. Alternatives
% are mathematically incoherent within the protocol's design.
narrative_ontology:constraint_metric(ergo_lets_protocol, accessibility_collapse, 0.95).
% Resistance to the zero-sum rule is impossible without breaking the protocol,
% which is what the collateral/enforcement system prevents.
narrative_ontology:constraint_metric(ergo_lets_protocol, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
% While it has Mountain-like properties, its primary function is coordination.
narrative_ontology:constraint_claim(ergo_lets_protocol, rope).
narrative_ontology:human_readable(ergo_lets_protocol, "Ergo Local Exchange Trading System (LETS)").
narrative_ontology:topic_domain(ergo_lets_protocol, "economic/technological").

% --- Emergence flag (required for mountain constraints) ---
% The "Sum = 0" rule is a mathematical property that emerges from the
% protocol's axiomatic design, akin to a logical theorem, even though the
% overall system is human-constructed. This flag enables the Mountain gate.
domain_priors:emerges_naturally(ergo_lets_protocol).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, local_communities).
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, unbanked_users).
% Who bears disproportionate cost?
% Victims are arguably "bad actors" whose collateral is seized upon default,
% though this is a self-imposed risk.
narrative_ontology:constraint_victim(ergo_lets_protocol, defaulters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNDER-COLLATERALIZED NEW ENTRANT (MOUNTAIN)
% For a user without the required ERG to post as collateral, the protocol's
% entry requirement is a Mountain. It is an unchangeable law of the system
% that prevents them from accessing the "Rope" of credit.
constraint_indexing:constraint_classification(ergo_lets_protocol, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ACTIVE COMMUNITY MEMBER (ROPE)
% For the established user, LETS is a Rope. It is a pure coordination tool
% they use to transact when liquidity in ERG or fiat is low. They have the
% agency to issue credit and settle debts within the system's rules.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE PROTOCOL ARCHITECT / INSTITUTIONAL OBSERVER (MOUNTAIN)
% From an institutional or designer's view, the "Sum = 0" rule is a
% Mountain. It is the core mathematical constraint that ensures systemic
% solvency and cannot be relaxed without destroying the protocol.
constraint_indexing:constraint_classification(ergo_lets_protocol, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE OVER-LEVERAGED DEFAULTER (SNARE)
% When a user defaults, the collateral seizure mechanism becomes a Snare.
% The smart contract "strangles" their assets to repay the pool's debt,
% acting coercively to maintain the zero-sum balance.
constraint_indexing:constraint_classification(ergo_lets_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_lets_protocol_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(ergo_lets_protocol, TypeRope, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(ergo_lets_protocol, TypeMountain, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(ergo_lets_protocol, TypeSnare, context(agent_power(powerless), _, constrained, _)),
    TypeRope == rope,
    TypeMountain == mountain,
    TypeSnare == snare,
    TypeRope \= TypeMountain.

test(zero_sum_immutability) :-
    % Analytical/Institutional perspective correctly identifies the math as Mountain.
    constraint_indexing:constraint_classification(ergo_lets_protocol, mountain, context(agent_power(institutional), _, _, _)).

:- end_tests(ergo_lets_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low base extraction (0.15) and suppression (0.1) reflect that this is
 *   a voluntary, non-rent-seeking financial tool. The perspectival variance
 *   is key: the same protocol is a Rope (coordination tool), a Mountain
 *   (impassable barrier), and a Snare (coercive enforcement) depending on
 *   the agent's structural relationship to its rules (membership, entry, default).
 *
 * PERSPECTIVAL GAP:
 *   An active member sees a Rope because they meet the entry conditions and
 *   benefit from the coordination. A prospective member without collateral
 *   sees a Mountain because the entry rule is an absolute, unchangeable
 *   barrier to them. An institutional auditor sees the core "Sum=0" logic
 *   as a Mountain of mathematics. A defaulter experiences the enforcement
 *   mechanism as a Snare that seizes their assets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly distinguishes the protocol's coordination
 *   function (Rope) from its rigid entry requirements (Mountain) and its
 *   coercive enforcement mechanism (Snare). It avoids mislabeling the entire
 *   system as a Snare just because it has a punitive default condition, or
 *   as a pure Rope while ignoring the exclusionary barrier to entry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_reputation_portability,
    "Can LETS reputation be ported between communities without collateral?",
    "Development of cross-LETS reputation standards or zero-knowledge proofs of credit history.",
    "If yes, the 'Mountain' of collateral drops for reputable users, making it a more universal Rope. If no, each community remains a silo with high entry barriers.",
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_reputation_portability, empirical, "Whether a technical standard for cross-community reputation can be developed and adopted.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ergo_lets_protocol, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */