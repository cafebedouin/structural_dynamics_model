% ============================================================================
% CONSTRAINT STORY: e2ee_digital_privacy_2026
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "End-to-end encryption: Best ideas of the century" by Matthew Sparkes
% ============================================================================

:- module(constraint_e2ee_digital_privacy_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: e2ee_digital_privacy_2026
 * human_readable: End-to-End Encryption (E2EE)
 * domain: technological/political/social
 * temporal_scope: 1977-2026
 * spatial_scope: Global (Digital)
 * 
 * SUMMARY:
 * End-to-end encryption (E2EE) is a digital "wall" that ensures messages are only 
 * readable by the sender and recipient, rendering them "meaningless gibberish" 
 * during transmission. It provides a technological safeguard for privacy by
 * relying on "immutable mathematics" rather than the promises of service providers.
 * 
 * KEY AGENTS:
 * - The Vulnerable Individual (Individual Powerless): Relies on E2EE for physical safety.
 * - The Surveillance State (Institutional): A spy agency or police force unable to access communications.
 * - The Mathematician (Analytical): Observer of the underlying mathematical laws.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(e2ee_digital_privacy_2026, 0, 10).
narrative_ontology:constraint_claim(e2ee_digital_privacy_2026, rope).

% Base extractiveness: 0.05 (Very Low)
% Rationale: E2EE is fundamentally protective rather than extractive; it prevents 
% the extraction of information by unauthorized parties.
domain_priors:base_extractiveness(e2ee_digital_privacy_2026, 0.05).

% Suppression score: 0.5 (Moderate)
% Rationale: It suppresses the visibility of intermediaries and states over 
% private communication, effectively "hiding" the alternative of surveillance.
domain_priors:suppression_score(e2ee_digital_privacy_2026, 0.5).

% Enforcement: Requires active software implementation.
domain_priors:requires_active_enforcement(e2ee_digital_privacy_2026).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(e2ee_digital_privacy_2026, individual_users).
constraint_victim(e2ee_digital_privacy_2026, institutional_surveillance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE VULNERABLE INDIVIDUAL - Rope
   --------------------------------------------------------------------------
   WHO: individual_powerless (e.g., a dissident or journalist in a hostile regime)
   WHEN: immediate (Protection from imminent harm)
   WHERE: trapped (Physically located in a dangerous region)
   
   WHY THIS CLASSIFICATION:
   For this user, E2EE is a life-saving 'Rope'. It is the essential tool that
   coordinates their private communication, protecting them from a state that
   wishes to harm them. Their physical survival depends on it.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    e2ee_digital_privacy_2026,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE SURVEILLANCE STATE - Snare
   --------------------------------------------------------------------------
   WHO: institutional (State power/Police/Spy agencies)
   WHEN: immediate (Short-term investigations/surveillance)
   WHERE: constrained (Blocked by the technological "wall")
   
   WHY THIS CLASSIFICATION:
   For the state seeking access, E2EE is a 'Snare' that constrains their 
   power. It renders their traditional tools—demands, blackmail, or 
   threats—ineffective against "impenetrable gibberish".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    e2ee_digital_privacy_2026,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE MATHEMATICIAN - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observer of cryptographic constants)
   WHEN: historical (From RSA 1977 foundations onward)
   WHERE: analytical (Observing universal laws of logic/prime numbers)
   
   WHY THIS CLASSIFICATION:
   To the mathematician, the cryptographic foundation of E2EE is a 'Mountain'—an 
   immutable reality grounded in the fundamental difficulty of factoring large 
   numbers. It is a zero-degree-of-freedom constraint of logic.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    e2ee_digital_privacy_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(e2ee_digital_privacy_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(e2ee_digital_privacy_2026, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

:- end_tests(e2ee_digital_privacy_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. PERSPECTIVE SELECTION: Added the 'Vulnerable Individual' as the required
 *    'individual_powerless' agent. This highlights the most critical use case
 *    for E2EE, where it acts as a 'Rope' for survival, not just convenience.
 * 
 * 2. CLASSIFICATION RATIONALE:
 *    - Vulnerable User (Rope): A tool for survival.
 *    - Surveillance State (Snare): A barrier to their power.
 *    - Mathematician (Mountain): An expression of immutable mathematical truth.
 * 
 * 3. EXTRACTIVENESS (0.05): Kept extremely low. E2EE is a shield against extraction,
 *    not a tool of it. The primary conflict is over control and visibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Core uncertainties about the long-term viability of the mathematical 'Mountain'.
 */

omega_variable(
    quantum_decryption_risk,
    "Will the rise of quantum computing render current 'hard-to-crack' mathematics (the Mountain) solvable?",
    resolution_mechanism("Experimental verification of Shor's algorithm at a scale capable of breaking current cryptographic standards."),
    impact("If yes: The protective 'Mountain' of current cryptography collapses into a temporary 'Scaffold', and the Rope breaks."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * ALTERNATIVE 1: Custodial Encryption (Managed Keys by a third party)
 *    Viability: Was the default for most early internet services.
 *    Suppression: Actively rejected by the E2EE model, which removes the trusted (or untrusted) third party from the key management process.
 *    Evidence: "Providers... cannot see your communications".
 * 
 * CONCLUSION:
 * The explicit rejection of custodial models demonstrates that E2EE is an intentional
 * design choice to shift power. It turns the "Managed Secret" (a 'Rope' for the state) 
 * into a "Private Secret" (a 'Rope' for the individual), creating a 'Snare' for the surveillant.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/e2ee_digital_privacy_2026].
 * 2. Multi-perspective: ?- multi_index_report(e2ee_digital_privacy_2026).
 * 3. Run tests: ?- run_tests(e2ee_digital_privacy_2026_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */