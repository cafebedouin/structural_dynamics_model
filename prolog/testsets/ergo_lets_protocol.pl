% ============================================================================
% CONSTRAINT STORY: ergo_lets_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_lets_protocol, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_lets_protocol
 *   human_readable: Ergo Local Exchange Trading System (LETS) Protocol
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Ergo LETS is a trustless mutual credit system implemented as a protocol
 *   on the Ergo blockchain where the sum of all participant balances must
 *   always equal zero. This mathematical constraint — that credit issued by
 *   one participant is credit owed by another, with no external subsidy or
 *   fractional reserve — creates a pure coordination mechanism. The system
 *   solves the collective action problem of enabling exchange without
 *   intermediary institutions (banks, clearinghouses) while maintaining
 *   cryptographic accountability. The zero-sum rule is enforced by the
 *   protocol logic itself, not by governance discretion, making it
 *   technically immutable. The constraint classifies as Rope from all
 *   meaningful perspectives because it enables coordination without
 *   extraction: participants voluntarily join, can exit costlessly, and
 *   benefit equally from network liquidity. The analytical perspective
 *   reveals that the zero-sum property is not contingent but a logical
 *   necessity of double-entry accounting applied to a closed system.
 *
 * KEY AGENTS:
 *   - Protocol Participants: Individual traders (moderate/mobile) — primary agents executing mutual credit exchanges; experience constraint as coordination enabling lower-friction trade
 *   - LETS Community Stewards: Regional operators (powerful/mobile) — manage network nodes and participant onboarding; benefit from network effects without extraction rents
 *   - Ergo Protocol Maintainers: Core developers (institutional/arbitrage) — maintain protocol infrastructure; experience constraint as enabling trustless coordination
 *   - Inactive Participants: Network members with minimal activity (moderate/constrained) — presence creates theatrical records without functional exchange; constrained exit due to social embedding
 *   - Analytical Observer: Civilizational auditor (analytical/analytical) — recognizes zero-sum balance as accounting identity, not institutional policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_lets_protocol, 0.18).
domain_priors:suppression_score(ergo_lets_protocol, 0.12).
domain_priors:theater_ratio(ergo_lets_protocol, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_lets_protocol, extractiveness, 0.18).
narrative_ontology:constraint_metric(ergo_lets_protocol, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(ergo_lets_protocol, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_lets_protocol, rope).
narrative_ontology:human_readable(ergo_lets_protocol, "Ergo Local Exchange Trading System (LETS) Protocol").
narrative_ontology:topic_domain(ergo_lets_protocol, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, protocol_participants).
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, local_exchange_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL PARTICIPANT (ROPE) — Individual traders in a LETS community experience the constraint as pure coordination. The zero-sum balance rule creates mutual credit without intermediary extraction. Participants can exit by stopping trades or joining other networks. The constraint solves the collective action problem of trust and credit availability in local exchange without coercive overhead.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL LETS OPERATOR (ROPE) — Operators managing LETS nodes or communities experience the constraint as a coordination mechanism enabling network effects. The protocol's zero-sum property prevents their extraction while guaranteeing network stability. Operators benefit from network growth without rent-extraction — their power derives from stewardship, not dominance. Mobile exit (choosing which networks to operate) prevents lock-in.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROTOCOL MAINTAINERS (ROPE) — The Ergo development community and protocol maintainers experience LETS as a coordination infrastructure. The zero-sum balance constraint ensures the system remains trustless and verifiable. Maintainers benefit from adoption and ecosystem growth without the constraint creating extraction rents. They have arbitrage options: maintaining alternative protocols, pivoting to other projects.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INACTIVE PARTICIPANT (PITON) — Participants who join but trade infrequently experience LETS as theater — the coordination structure persists (they remain in the network record) but with minimal functional activity. Their balance may remain static, theater ratio is high (presence without function), but they can exit by stopping participation. Constrained exit (some social cost to leaving) maintains the appearance of participation.
constraint_indexing:constraint_classification(ergo_lets_protocol, piton,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the zero-sum balance constraint is a mathematical/accounting identity, not a social policy choice. For any mutual credit system without external subsidy, the sum of all participant balances must equal zero. This is an immutable property of double-entry accounting, not a contingent institutional arrangement. The constraint emerges naturally from the logical structure of accounting itself.
constraint_indexing:constraint_classification(ergo_lets_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_lets_protocol_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_lets_protocol, TR),
    TR >= 0.70.

:- end_tests(ergo_lets_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The zero-sum balance constraint prevents any participant or operator from extracting surplus value. Every unit of credit extended by one agent is a liability for another; no netting, subsidies, or external creation of value is possible. The modest non-zero extractiveness reflects only minor asymmetries: protocol maintainers derive modest network effects value, stewards gain reputation benefits, early adopters capture initial liquidity advantage. But these are coordination benefits, not extraction. Suppression (0.12): Low. Participants face minimal coercive barriers. Exit costs are low (stopping participation costs little), alternatives are available (other exchange systems, barter, fiat currency), and entry is open (no gatekeeping by design). Suppression reflects only minor friction: learning curve, initial lack of trading partners, psychological adjustment to no-interest credit. Theater ratio (0.25): Low. The protocol maintains high functional content: balances are tracked with cryptographic verification, every transaction affects the network state, inactive records accumulate theater but remain minority. Theater increases modestly over the interval as some participants join communities without sustained trading, creating statistical presence without active coordination.
 *
 * PERSPECTIVAL GAP:
 *   LETS presents unusual perspectival alignment: most agents experience the same constraint as pure coordination (Rope). The gap is not between beneficiary and victim, but between active and inactive participants. Active traders see coordination; inactive members see theater (their accounts persist but their economic function vanishes). The analytical perspective reveals that the zero-sum property is a mathematical fact, not a contingent institutional choice — this classification as Mountain at the universal scale is not a false summit but a genuine recognition that the constraint is immutable by definition. The small gap reflects that LETS solves a genuine coordination problem (credit availability, trust establishment) without introducing the extraction hierarchies that plague banking and fiat systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in LETS is nearly symmetric (d ≈ 0.50 for most participants) because the zero-sum structure prevents sustained advantage accumulation. A participant cannot be a pure beneficiary (extracting permanently) without counterparties accepting permanent deficit status — which they would not do voluntarily. Similarly, victims cannot be permanently victimized because the protocol ensures balanced obligation. Active traders experience d ≈ 0.45 (slight beneficiary status from network effects), operators experience d ≈ 0.40 (institutional benefit from stewardship), inactive participants experience d ≈ 0.55 (slight victim status from theatrical presence without function). These are all near-symmetric because the structural mechanism prevents the asymmetries that would produce extreme d values. The derived f(d) values therefore produce moderate χ across all perspectives, consistent with the Rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credit_fungibility_assumption,
    'Does the zero-sum balance rule presume that all credit units within a LETS community are equally fungible and acceptable, or do differential trust levels create de facto hierarchies?',
    'Empirical observation of LETS transaction patterns; analysis of whether participants segregate balances by counterparty reputation or accept all credit units equally',
    'If credit is truly fungible: pure coordination rope. If trust hierarchies emerge: the system becomes tangled rope with asymmetric credit availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_fungibility_assumption, empirical, 'Whether credit units are treated as fungible or hierarchically valued by trust').

omega_variable(
    network_critical_mass,
    'What population density and trade frequency threshold is required for a LETS community to sustain coordination benefits versus collapsing into theater (inactive records)?',
    'Comparative analysis of successful vs failed LETS communities; correlation between network size, transaction volume, and perceived value',
    'If critical mass is low: LETS can operate at small scale as rope. If critical mass is high: many communities degrade to piton (theatrical presence). System extractiveness may vary by context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_critical_mass, empirical, 'Minimum viable population and activity threshold for sustained LETS function').

omega_variable(
    protocol_vs_governance_extraction,
    'Does the technical protocol enforce zero-sum balances trustlessly, or do community governance decisions create extraction opportunities (e.g., write-offs, credit resets, exclusion)?',
    'Analysis of governance structures in operational LETS systems; comparison of protocol-enforced constraints vs community-adjudicated exceptions',
    'If protocol enforces trustlessly: pure rope. If governance can override: system becomes tangled rope with potential for extraction by governing body.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protocol_vs_governance_extraction, conceptual, 'Whether zero-sum is enforced by code or by governance discretion').

omega_variable(
    scalability_versus_trust,
    'As a LETS network grows beyond local trust circles, does the zero-sum constraint create inefficiencies (hoarding, credit unavailability) that extract value from growth-seeking participants?',
    'Analysis of LETS performance metrics (velocity, coverage, failed trades) across different network sizes; identification of scaling thresholds where extraction dynamics emerge',
    'If scaling preserves rope: system coordinates efficiently at all scales. If scaling introduces friction: system may become snare for growth-stage participants (high demand, constrained credit supply).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_versus_trust, empirical, 'Whether zero-sum creates inefficiencies as network grows beyond local trust').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_lets_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_lets_tr_t0, ergo_lets_protocol, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ergo_lets_tr_t5, ergo_lets_protocol, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ergo_lets_tr_t10, ergo_lets_protocol, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(ergo_lets_be_t0, ergo_lets_protocol, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ergo_lets_be_t5, ergo_lets_protocol, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(ergo_lets_be_t10, ergo_lets_protocol, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_lets_protocol, resource_allocation).
narrative_ontology:affects_constraint(ergo_lets_protocol, blockchain_settlement_finality).
narrative_ontology:affects_constraint(ergo_lets_protocol, mutual_credit_trust_bootstrap).

% DUAL FORMULATION NOTE:
% Ergo LETS is downstream of blockchain scalability and settlement constraints (upstream) but represents a distinct layer: the protocol enforces the zero-sum rule, but the practical emergence of trust and liquidity depends on network effects. The present story focuses on the mathematical/protocol constraint; related stories analyze trust bootstrapping and settlement dynamics separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
