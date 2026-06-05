% ============================================================================
% CONSTRAINT STORY: ergo_lets_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Ergo Local Exchange Trading System (LETS) is a blockchain-based
 *   implementation of mutual credit protocols where all transactions are
 *   peer-to-peer and the mathematical constraint that the sum of all
 *   participant balances equals zero is enforced at the protocol level. This
 *   constraint is a canonical Rope: pure coordination mechanism with minimal
 *   coercion, no central authority, and genuine mutual benefit. Unlike
 *   fiat-based LETS (which depend on community enforcement and social trust),
 *   Ergo LETS achieves trustlessness through cryptographic proof. The
 *   zero-sum balance constraint is not extractive but rather a conservation
 *   law that prevents systemic debt accumulation. Participants experience
 *   LETS as a solution to the double coincidence of wants problem — in a pure
 *   barter economy, you need what I have and I need what you have. LETS
 *   unbundles this by allowing me to accumulate credit in exchange for
 *   goods/services, which I then spend with any other member, not just the
 *   original counterparty. This is coordination, not extraction. Theater
 *   ratio is low (0.25) because blockchain verification is objective — no
 *   performative review or ritual maintains LETS; it is enforced by code.
 *
 * KEY AGENTS:
 *   - Mutual Credit Participants: Primary beneficiary (moderate/mobile) — earn and spend credits; experience LETS as coordination solution
 *   - Protocol Maintainer (Ergo team): Secondary beneficiary (institutional/arbitrage) — benefits from network adoption and protocol viability
 *   - Excluded Non-Participants: Neutral observer (powerless/mobile) — not suppressed from joining; free to participate or ignore
 *   - Analytical Observer: Sees LETS as pure coordination mechanism (analytical/analytical) — zero-sum constraint is conservation law, not extraction
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
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, mutual_credit_participants).
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, participants_with_network_effects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTICIPATING MEMBER (ROPE) — Member can exit by settling balance or ceasing transactions. Experiences LETS as pure coordination: earning and spending credits within network. No suppression of alternatives — traditional currency remains available. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.09. Low effective extraction; genuine mutual benefit.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: PROTOCOL MAINTAINER (ROPE) — Ergo blockchain developers and LETS implementation team. Benefit from network effects and adoption. Can arbitrage between protocol design and external coordination services. Exit options abundant (move to other blockchain projects). d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.002. Net beneficiary but not extractive.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EXCLUDED NON-PARTICIPANT (ROPE) — Community members who cannot afford Ergo participation or lack technical access. LETS appears as coordination mechanism they are absent from, not as extraction. They experience it as neutral coordination among others. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.09. Low friction; network is open to join.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — Sees LETS as a pure coordination solution to the double coincidence of wants problem. The zero-sum balance constraint (sum of all balances = 0) is a natural law of conservation, not an extractive mechanism. Trustless mutual credit is coordination without central authority. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.25. This is the canonical classification; LETS is a Rope from all perspectives.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_lets_protocol_tests).
:- end_tests(ergo_lets_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The zero-sum balance constraint prevents any participant from extracting net value across the system — extracting from others requires an equivalent injection elsewhere. Any asymmetry in benefit is temporary and market-corrected: if I consistently gain more value than I contribute, other participants will reduce trading with me. The constraint actually prevents extraction by design. Suppression (0.12): Very low. No coercion maintains LETS membership. Participants retain traditional currency, can exit by settling balance, and face no penalty for non-participation. Technical friction exists (blockchain interaction, wallet management) but is symmetric and transparent. Theater ratio (0.25): Low. LETS verification is cryptographic, not performative. Transactions are immediately settled and verifiable on-chain. No ritual or social performance maintains the system — code enforcement is sufficient. Claimed type (Rope): Justified. This is pure coordination: solves double coincidence problem without central intermediary, achieves mutual benefit for all participants, and requires minimal overhead. No extraction mechanism exists.
 *
 * PERSPECTIVAL GAP:
 *   Unusually low perspectival gap. All four perspectives classify LETS as Rope because the structural data genuinely represents pure coordination. The participating member experiences it as Rope (mutual benefit, mobile exit). The protocol maintainer experiences it as Rope (institutional beneficiary from network effects, arbitrage available). The excluded non-participant sees it as Rope (coordination among others; neutral). The analytical observer sees it as Rope (conservation law, not extraction). This lack of gap is a signature of well-designed coordination mechanisms. When all perspectives agree, the constraint is not generating perspectival conflict — it is serving its function transparently.
 *
 * DIRECTIONALITY LOGIC:
 *   Mutual credit participants: Beneficiary + mobile → d≈0.50, f(d)≈0.65. Symmetric position relative to constraint; both benefit and bear costs of participation. Protocol maintainer: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary with external options; not trapped by LETS adoption. Excluded non-participants: Neither beneficiary nor victim + mobile → d≈0.50, f(d)≈0.65. Neutral observer position; constraint does not affect them. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Sees the conservation law structure; no hidden extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy trivially: there is no confusion between pure coordination and hidden extraction. The zero-sum balance property is a mathematical truth, not a rhetorical frame. Every perspective, measured honestly, produces Rope. The constraint exhibits textbook coordination: (1) solves collective action problem (double coincidence), (2) requires no central authority, (3) achieves mutual benefit, (4) permits exit, (5) operates transparently. No extraction mechanism exists because the system is designed to prevent it. This is what a healthy coordination mechanism looks like in the framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_bootstrap_critical_mass,
    'What participant threshold triggers sufficient network effects for LETS viability, and what happens if critical mass is not achieved?',
    'Longitudinal tracking of LETS adoption in Ergo-based communities; correlation between participant count and transaction volume; comparison with failed LETS implementations',
    'If critical mass < 50 participants: LETS remains marginal utility (coordination failure masquerading as supply issue). If critical mass > 500: coordination function is robust. Below threshold, constraint appears as rope but with minimal actual benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_bootstrap_critical_mass, empirical, 'Critical mass threshold for LETS network viability').

omega_variable(
    credit_cycling_stability,
    'In the absence of external settlement (goods/services), can mutual credit cycle indefinitely without systematic balance buildup for any participant cohort?',
    'Analysis of transaction patterns in mature LETS systems; examination of whether repeated subsets of participants accumulate uncovered credit vs. achieve equilibrium',
    'If cycles can be fully balanced: Rope classification holds (pure coordination). If certain subgroups systematically accumulate credit: hidden extraction mechanism emerges (Tangled Rope from subgroup perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credit_cycling_stability, empirical, 'Whether mutual credit cycles achieve stable balance distribution').

omega_variable(
    technical_barrier_to_exit,
    'Does technical friction in blockchain settlement (gas fees, wallet management, Ergo-to-fiat conversion) function as covert suppression of exit?',
    'Measurement of actual exit transaction costs; comparison with traditional currency settlement costs; survey of participants citing technical barriers as obstacle to leaving',
    'If friction is symmetric: Rope remains. If friction systematically increases exit cost for some participants: suppression arises (potential Tangled Rope signature).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_barrier_to_exit, empirical, 'Technical friction as potential exit suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_lets_protocol, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_lets_tr_t0, ergo_lets_protocol, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ergo_lets_tr_t6, ergo_lets_protocol, theater_ratio, 6, 0.24).
narrative_ontology:measurement(ergo_lets_tr_t12, ergo_lets_protocol, theater_ratio, 12, 0.25).

% Extraction over time
narrative_ontology:measurement(ergo_lets_be_t0, ergo_lets_protocol, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(ergo_lets_be_t6, ergo_lets_protocol, base_extractiveness, 6, 0.17).
narrative_ontology:measurement(ergo_lets_be_t12, ergo_lets_protocol, base_extractiveness, 12, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_lets_protocol, resource_allocation).
narrative_ontology:affects_constraint(ergo_lets_protocol, blockchain_consensus_overhead).
narrative_ontology:affects_constraint(ergo_lets_protocol, ergo_network_security).

% DUAL FORMULATION NOTE:
% LETS is a pure coordination mechanism; it does not decompose into separate constraints with different ε values. If a LETS implementation were to introduce extraction (e.g., through protocol fees, unequal computational overhead, or cartel-enforced credit rationing), a separate story (ergo_lets_rent_extraction) would be warranted. Currently, the protocol operates as designed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
