% ============================================================================
% CONSTRAINT STORY: quantum_decryption_risk_2026
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: "End-to-end encryption: Best ideas of the century" by Matthew Sparkes
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(quantum_decryption_risk_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: quantum_decryption_risk_2026
 * human_readable: Quantum Decryption & The RSA Vulnerability
 * domain: technological/security
 * temporal_scope: 2026 - 2040 (The "Q-Day" Horizon)
 * spatial_scope: Global (Digital Infrastructure)
 * * SUMMARY:
 * Current end-to-end encryption (ETEE) relies on the "immutable mathematics" of 
 * algorithms like RSA, which hinges on the extreme difficulty of factoring 
 * large prime numbers. Quantum decryption risk represents the 
 * possibility that quantum computers will utilize Shor's algorithm to solve 
 * these problems near-instantaneously, potentially collapsing the "wall" 
 * that keeps digital secrets safe.
 * * KEY AGENTS:
 * - The Privacy Defender: Relies on encryption to "save their lives" and 
 * protect democracy.
 * - The State Intelligence Agency: An actor that may currently store 
 * encrypted data to "harvest now, decrypt later" using future quantum tech.
 * - The Quantum Engineer: The analytical agent building the physical systems 
 * that challenge mathematical constants.
 * * NARRATIVE ARC:
 * Since 1977, RSA has provided peace of mind. However, as we 
 * move through the 21st century, the "immutable" nature of this math is 
 * threatened by a new physical paradigm—quantum processing.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(quantum_decryption_risk_2026, 0, 10).
narrative_ontology:constraint_claim([quantum_decryption_risk_2026], [existential_technological_risk]).

% Base extractiveness score (0.8 = High)
% Rationale: The ability to decrypt global communications is the ultimate 
% asymmetric extraction tool, transferring the "secrets" of the powerless 
% to the possessors of the technology.
domain_priors:base_extractiveness(quantum_decryption_risk_2026, 0.8).

% Suppression score (0.3 = Low)
% Rationale: The risk is widely publicized and debated by privacy groups 
% like Big Brother Watch.
domain_priors:suppression_score(quantum_decryption_risk_2026, 0.3).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(quantum_decryption_risk_2026, extractiveness, 0.8).
narrative_ontology:constraint_metric(quantum_decryption_risk_2026, suppression_requirement, 0.3).

% Enforcement: Emerges naturally through the advancement of physics.
domain_priors:emerges_naturally(quantum_decryption_risk_2026).

% Metrics required for Section 1 of the Executive Summary
% Status set to MANDATROPHY due to high extractiveness (0.8).
% BENEFICIARIES & VICTIMS
constraint_beneficiary(quantum_decryption_risk_2026, quantum_capable_states).
constraint_victim(quantum_decryption_risk_2026, private_citizens).
constraint_victim(quantum_decryption_risk_2026, human_rights_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PRIVACY DEFENDER - Snare
   --------------------------------------------------------------------------
   
   WHO: powerless (Vulnerable users relying on ETEE for life/safety)
   WHEN: immediate (Real-time communications)
   WHERE: trapped (No conceptual exit if the math fails)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For those who "literally rely on encryption to save their lives," the threat 
   of quantum decryption is a "Snare". It turns their current 
   mathematical "wall" into a future trap where their past secrets can be 
   extracted at will.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    quantum_decryption_risk_2026,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: STATE INTELLIGENCE - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional (State power with long-term strategic goals)
   WHEN: historical (Century-scale intelligence gathering)
   WHERE: arbitrage (Can wait for technology to catch up to the data)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   For the state actor, quantum decryption is a "Rope"—a functional 
   coordination mechanism for total information awareness. It allows them 
   to bypass the "impenetrable gibberish" that currently prevents them 
   from demanding or threatening their way into secrets.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    quantum_decryption_risk_2026,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE QUANTUM PHYSICIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (Observer of the limits of computation)
   WHEN: civilizational (Fundamental change in species' power)
   WHERE: analytical (Logic and physical law)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, the ability of a quantum system to solve the factoring 
   problem is a "Mountain"—a fixed physical reality. It is not an 
   ideological choice but a consequence of Shor's algorithm and the 
   nature of qubits.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    quantum_decryption_risk_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(quantum_decryption_risk_tests).

test(multi_perspective_asymmetry) :-
    % Beneficiary (Institutional) sees a Tool (Rope)
    constraint_indexing:constraint_classification(quantum_decryption_risk_2026, rope, context(institutional, historical, arbitrage, global)),
    % Victim (Powerless) sees a Trap (Snare)
    constraint_indexing:constraint_classification(quantum_decryption_risk_2026, snare, context(powerless, immediate, trapped, global)).

test(math_vulnerability_scaling) :-
    % High extractiveness (0.8) indicates the Mandatrophy status
    domain_priors:base_extractiveness(quantum_decryption_risk_2026, E),
    E > 0.7.

:- end_tests(quantum_decryption_risk_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.8):
 * The risk is fundamentally asymmetric. The source notes that ETEE 
 * allows people in "very dangerous parts of the world" to save 
 * their lives. If this is broken, the power dynamic 
 * shifts entirely to those who possess quantum hardware.
 * * 2. PERSPECTIVE SELECTION:
 * The model chose to contrast the "Privacy Defender" (who sees 
 * current math as a lifeline) with "State Intelligence" 
 * (who sees quantum as a tool to break that lifeline).
 * * 3. MANDATROPHY STATUS:
 * Resolved as a Mandatrophy Risk because the "Rope" for the 
 * institutional agent is built specifically upon the "Snare" 
 * of the powerless agent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    q_day_arrival_date,
    "When will a quantum computer achieve the necessary error-corrected qubits to factor RSA-2048?",
    resolution_mechanism("Physical demonstration of Shor's algorithm on a 2048-bit equivalent system"),
    impact("If soon: Immediate Snare for all current digital legacy data. If far: Rope for transition to PQC."),
    confidence_without_resolution(low)
).

omega_variable(
    quantum_decryption_risk_extraction_intent,
    "Is the development of quantum decryption a strategic pursuit of state dominance (Snare) or a byproduct of scientific progress (Mountain)?",
    resolution_mechanism("Audit of state quantum budgets vs. open scientific publishing rates"),
    impact("If strategic dominance: Mandatrophy Snare. If byproduct: Physical Mountain."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Post-Quantum Cryptography (PQC)
 * Viability: Algorithms based on lattices or other math not solvable 
 * by Shor's algorithm.
 * Suppression: Currently "sluggish" in adoption compared to the 
 * velocity of quantum hardware research.
 * * CONCLUSION:
 * The existence of PQC acts as a potential new "Rope" for privacy, but 
 * until it is standard, the "Quantum Decryption" Snare remains tightened 
 * around legacy ETEE systems.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [quantum_decryption_risk_2026].
% Report: ?- multi_index_report(quantum_decryption_risk_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
