% ============================================================================
% CONSTRAINT STORY: asymmetric_computational_difficulty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_computational_difficulty, []).

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
 *   constraint_id: asymmetric_computational_difficulty
 *   human_readable: Proof-of-Work Zero-Day
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models a Proof-of-Work (PoW) based coordination system
 *   (e.g., a cryptocurrency) that has been compromised by a secret zero-day
 *   exploit. The exploit breaks the foundational 'asymmetric computational
 *   difficulty,' allowing a single advanced adversary to solve the
 *   computational puzzle with trivial effort. While the system was designed
 *   as a Rope—a fair mechanism for achieving decentralized consensus—the
 *   secret compromise has transformed it into a pure Snare. Legitimate
 *   participants continue to expend vast resources on 'mining,' but this
 *   activity is now pure theater, serving only to maintain the illusion of
 *   security while the adversary extracts the system's entire value.
 *
 * KEY AGENTS:
 *   - Advanced Adversary: Primary beneficiary (institutional/arbitrage) — Possesses the exploit and can generate valid proofs with near-zero cost.
 *   - Legitimate Miners: Primary victims (powerless/trapped) — Invested heavily in specialized hardware and are locked into a competition they cannot win.
 *   - Network Users: Secondary victims (moderate/mobile) — Rely on the network's security guarantees, which are now void.
 *   - Unaware Protocol Foundation: Institutional actor (institutional/constrained) — Maintains the protocol under the false belief that it is secure.
 *   - Analytical Observer: Security researcher/cryptographer (analytical/analytical) — Can model and identify the structural failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_computational_difficulty, 0.85).
domain_priors:suppression_score(asymmetric_computational_difficulty, 0.9).
domain_priors:theater_ratio(asymmetric_computational_difficulty, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, extractiveness, 0.85).
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(asymmetric_computational_difficulty, theater_ratio, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_computational_difficulty, snare).
narrative_ontology:human_readable(asymmetric_computational_difficulty, "Proof-of-Work Zero-Day").
narrative_ontology:topic_domain(asymmetric_computational_difficulty, "technological").

domain_priors:requires_active_enforcement(asymmetric_computational_difficulty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_computational_difficulty, advanced_adversary).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, legitimate_miners).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, network_users).
narrative_ontology:constraint_victim(asymmetric_computational_difficulty, network_security_guarantees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGITIMATE MINER (SNARE) — Trapped by massive capital investment in specialized hardware (ASICs) and energy contracts. Their computational work is rendered meaningless by the adversary's exploit, turning their effort into pure cost extracted by the adversary. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.45 (capped at 1.0).
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADVANCED ADVERSARY (ROPE) — The adversary possesses a zero-day exploit that breaks the computational asymmetry. For them, the network is a perfect coordination mechanism that subsidizes their activity. They expend minimal effort to solve blocks, extracting the entire security budget. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.12. Negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: UNAWARE PROTOCOL FOUNDATION (ROPE) — The institution maintaining the protocol is unaware of the compromise. They perceive the system as functioning according to its design specification: a fair, decentralized coordination mechanism (Rope). Their classification is based on the intended, not the actual, structure.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: CASUAL USER (PITON) — From the user's perspective, transactions are still being processed and blocks are being added. The core function *appears* to work. However, the underlying security guarantee is completely gone. The massive expenditure of energy by legitimate miners is pure theater. With theater_ratio=0.95, this classifies as a Piton: a system maintained by inertia and performance, whose primary function has failed.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SNARE) — The analyst sees the complete structure: a system designed as a Rope has been structurally transformed into a Snare by a secret asymmetry. The high base extractiveness (0.85) and suppression (0.90) confirm the classification. The analytical view pierces the veil of the system's claims.
constraint_indexing:constraint_classification(asymmetric_computational_difficulty, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_computational_difficulty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_computational_difficulty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_computational_difficulty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_computational_difficulty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asymmetric_computational_difficulty, TR),
    TR >= 0.70.

:- end_tests(asymmetric_computational_difficulty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.85): Extremely high. The adversary captures the entire security budget (block rewards and transaction fees) that is supposed to incentivize honest participation from a distributed network of miners. Suppression (0.90): Extremely high. The secrecy of the exploit prevents any counter-strategy. Legitimate miners are suppressed not by overt force, but by information asymmetry; they cannot opt-out of the rigged game without abandoning their sunk costs. Theater Ratio (0.95): Near total. All the energy and computation expended by legitimate miners is functionally useless for securing the network. It is a massive, performative ritual that serves only to conceal the underlying extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. The adversary experiences a perfect subsidy (Rope), while the miners experience a perfect trap (Snare). The unaware maintainers see the system's blueprint (Rope), while casual users see a degraded but seemingly functional utility (Piton). This gap between the claimed function and the operational reality for different agents is the core pathology the constraint reveals.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the structural positions. The 'advanced_adversary' is the sole beneficiary with arbitrage exit, yielding a low 'd' value and negative effective extraction (a subsidy). The 'legitimate_miners' are victims with trapped exit, yielding a maximal 'd' value and extreme effective extraction. The unaware foundation, while institutional, is constrained and perceives itself as a symmetric participant, leading to a Rope classification from its flawed viewpoint.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic resolution of the Mandatrophy. The system's public claim is that of a Rope (fair coordination). An analysis that only considered this claim would be dangerously wrong. By indexing to the powerless, trapped victim (the legitimate miner), the framework correctly classifies the operational reality as a Snare. The system is not a coordination mechanism with a flaw; it is an extraction mechanism disguised as a coordination mechanism. The high 'mandatrophy_resolved' flag is required because the extractiveness (0.85) is so severe that this distinction is critical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adversary_identity_and_motive,
    'Is the adversary a state actor seeking control, a rational economic actor maximizing profit, or a nihilistic actor seeking to destroy the network?',
    'Attribution of the exploit through forensic analysis or intelligence gathering.',
    'A state actor might maintain the illusion for surveillance, while an economic actor might cash out, revealing the flaw. This determines the system''s lifespan and failure mode.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adversary_identity_and_motive, empirical, 'Determining the identity and goals of the compromising adversary.').

omega_variable(
    exploit_discoverability,
    'Is the exploit statistically invisible, or does it leave subtle traces on the blockchain (e.g., non-uniform nonce distribution) that could be detected?',
    'Advanced statistical analysis of the blockchain history by independent security researchers.',
    'If discoverable, the Snare has a finite lifespan. If not, it could persist indefinitely, making it a ''perfect'' crime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploit_discoverability, empirical, 'Whether the zero-day exploit is theoretically detectable post-facto.').

omega_variable(
    community_fork_coordination,
    'If the exploit is revealed, can the community successfully coordinate a hard fork to a new, secure algorithm, or will the revelation cause a catastrophic collapse in trust and value?',
    'Observing the community''s response to the revelation, including developer consensus, miner signaling, and market reaction.',
    'Successful coordination would transform the Snare into a Scaffold (the fork is a temporary measure). Failure means the entire ecosystem collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_fork_coordination, preference, 'The capacity of the decentralized community to respond to a fundamental security breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_computational_difficulty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_tr_t0, asymmetric_computational_difficulty, theater_ratio, 0, 0.05).
narrative_ontology:measurement(asym_tr_t5, asymmetric_computational_difficulty, theater_ratio, 5, 0.6).
narrative_ontology:measurement(asym_tr_t10, asymmetric_computational_difficulty, theater_ratio, 10, 0.95).

% Extraction over time
narrative_ontology:measurement(asym_be_t0, asymmetric_computational_difficulty, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(asym_be_t5, asymmetric_computational_difficulty, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(asym_be_t10, asymmetric_computational_difficulty, base_extractiveness, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_computational_difficulty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
